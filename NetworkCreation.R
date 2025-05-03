library(lubridate)
library(igraph)
library(Matrix)
library(network)
library(readxl)
#install.packages("openxlsx")
library(openxlsx)
# apro i dataframe
df_joint_venture = read_excel("SDC Platinum Alliances_session.xlsx")
df_joint_venture <- df_joint_venture[,c(2,9)]# accedo solo alle colonne che mi interessano(date e partecipanti)
df_MA <- read_excel("SDC Platinum M&A.xlsx")
df_MA <- df_MA[,c(2,4,7)]# accedo solo alle colonne che mi interessano (data, target e acquirer)
targets <- unique(df_MA$`Target Short Name`)
acquires <- unique(df_MA$`Acquiror Short Name`)
split_names <- lapply(df_joint_venture$Participants, function(x) unlist(strsplit(x, split = "\r\n")))
list_of_aziende <- unlist(split_names, recursive = TRUE)
unique_aziende<- unique(list_of_aziende)
length(intersect(targets,unique_aziende))# aziende in comune tra targets e Joint vent
length(intersect(acquires,unique_aziende)) # aziende in comune tra acquires e joint vent
# filtro il df delle M&A in modo tale che io abbia informazioni solo di aziende contenute nel df delle joint venture
# perche l idea è avere ego network del target e dell'acquirer, non sono interessato a M&A di cui non
# ho info sulla joint venture
filtered_MA <- df_MA[df_MA$`Target Short Name` %in% unique_aziende & df_MA$`Acquiror Short Name` %in% unique_aziende, ]
filtered_MA<-filtered_MA[!duplicated(filtered_MA), ]
dim(filtered_MA)
r <- dim(filtered_MA)[1]
# ho 297 M&A
# liste vuote in cui appenderò i risultati ottenuti
degree_a <- list()
betw_a <- list()
close_a <- list()
burt_a <- list()
eigen_a <- list()
degree_t <- list()
betw_t <- list()
close_t <- list()
burt_t <- list()
eigen_t <- list()
degree_m <- list()
betw_m <- list()
close_m <- list()
burt_m <- list()
eigen_m <- list()
direct_connection <- list()
shared_nodes <- list()
pres_in_same_net <- list()
dates<- list()
l<-list()

# Function to get all connected participants
get_all_connections <- function(participant, df) {
  connections <- participant
  new_connections <- participant
  repeat {
    # Adjusting the matching pattern to include \r\n handling within the Participants
    rows_with_connections <- grepl(paste(new_connections, collapse = "|"), df$Participants)
    
    # Clean up the participants on-the-fly by replacing \r\n with ", "
    cleaned_participants <- gsub("\r\n", ", ", df$Participants[rows_with_connections])
    
    # Split the cleaned participants string into individual participants
    connected_participants <- unique(unlist(strsplit(cleaned_participants, ", ")))
    
    # Identify new connections that haven't been seen before
    new_connections <- setdiff(connected_participants, connections)
    
    # If no new connections are found, break the loop
    if (length(new_connections) == 0) {
      break
    }
    
    # Update the connections list with newly found connections
    connections <- unique(c(connections, new_connections))
  }
  
  return(connections)
}
start_exec = Sys.time()
# ora itero su ciascuna riga del dataframe del M&A
for (i in 1:r) {
  print(i)
  date <- filtered_MA$`Date Announced`[i]
  date <- as.POSIXct(date)
  date_2 <- date - 86400*365*6
  target <- filtered_MA$`Target Short Name`[i]
  acquirer <-filtered_MA$`Acquiror Short Name`[i]
  # accedo al dataframe delle Joint Venture considerando solo quelle comprese tra le due date
  df_jv_filt <- df_joint_venture[df_joint_venture$`Date Announced` > date_2 & df_joint_venture$`Date Announced` <= date,]
  target_connections <- get_all_connections(target, df_jv_filt)
  df_jv_filt_target <- df_jv_filt[grepl(paste(target_connections, collapse = "|"), df_jv_filt$Participants), ]
# contiene tutte le connessioni dirette e indirette di target  
  # Get all connections for acquirer
  if (dim(df_jv_filt_target)[1]==0){
    next
  } # se il dataframe è vuoto (target non coinvolto in nessuna JV prima di quella data, vado alla prossima riga)
  # non ho interesse nel M&A se le aziende non sono coinvolte in JV
  # stessa cosa per l acquirer
  acquirer_connections <- get_all_connections(acquirer, df_jv_filt)
  df_jv_filt_acquirer <- df_jv_filt[grepl(paste(acquirer_connections, collapse = "|"), df_jv_filt$Participants), ]
  # contiene tutte le connessioni dirette e indirette di acquirer 
   # da questo df costruirò il grafo del acquirer 
  if (dim(df_jv_filt_acquirer)[1]==0){
    next
  }# se il dataframe è vuoto (target non coinvolto in nessuna JV prima di quella data, vado alla prossima riga)
  # costruzione del grafo acquirer
  # creo la cartella dove salvare i plots
  subdir<-paste0(acquirer,"-",target)
  if (!dir.exists(subdir)) {
    dir.create(subdir)
  }
  # ora dal dataframe che contiene tutte le righe con connessioni dirette e indirette di acquirer
  # devo creare una edge list sotto forma di dataframe per creare l'oggetto grafo
  aziende_a = df_jv_filt_acquirer$Participants
  split_names_a <- lapply(aziende_a, function(x) unlist(strsplit(x, split = "\r\n")))
  max_length_a <- max(sapply(split_names_a, length))
  padded_names_a <- lapply(split_names_a, function(x) {
    length(x) <- max_length_a
    x
  })
  names_matrix_a <- do.call(rbind, padded_names_a)
  
  extract_pairs <- function(row) {
    row <- row[!is.na(row)]  # Remove NAs
    if (length(row) < 2) return(NULL)  # Return NULL if less than 2 elements
    pairs <- t(combn(row, 2))  # Get all combinations of pairs
    return(pairs)
  }
  edges_list_a <- do.call(rbind, lapply(1:nrow(names_matrix_a), function(i) extract_pairs(names_matrix_a[i, ])))
  edges_list_a <- unique(edges_list_a)
  edge_list_df_a <- as.data.frame(edges_list_a, stringsAsFactors = FALSE)
  colnames(edge_list_df_a) <- c("Source", "Target")
  graph_a <- graph_from_data_frame(d = edge_list_df_a, directed = FALSE)
  components_info_a <- components(graph_a)
  acquirer_node_index <- which(V(graph_a)$name == acquirer)
  # Find the component that contains the 'acquirer' node
  acquirer_component <- components_info_a$membership[acquirer_node_index]
  # Extract the subgraph that contains all indirect connections of the 'acquirer' node
  graph_a <- induced_subgraph(graph_a, V(graph_a)[components_info_a$membership == acquirer_component])
  edge_list_df_a <-  igraph::as_data_frame(graph_a, what = "edges")
  colnames(edge_list_df_a) <- c("Source", "Target")
  # lets check if target is in graph_a, if so they are in the same network
  filtered_edges <- edge_list_df_a[
    (edge_list_df_a$Source == target | edge_list_df_a$Target == target) & 
      (edge_list_df_a$Source != acquirer & edge_list_df_a$Target != acquirer),]
  if (dim(filtered_edges)[1]>0){
    pres_in_same_net <- append(pres_in_same_net,1)
  } else {
    pres_in_same_net <- append(pres_in_same_net,0)
  }

  degree_centrality_a <- degree(graph_a)
  acquirer_node_index <- which(V(graph_a)$name == acquirer)
  # qui accedo ai nomi dei direct neighbours di acquirer
  acquirer_neighbors <- V(graph_a)[neighbors(graph_a, V(graph_a)[V(graph_a)$name == acquirer])]
  acquirer_neighbors <-acquirer_neighbors$name
  if (target %in% acquirer_neighbors) {
    direct_connection <- append(direct_connection, 1)
  } else {
    direct_connection <- append(direct_connection, 0)# Code to execute if the condition is FALSE
  }
  #Plotting command
  # Create a vector of labels where only the target node's label is visible
  node_labels_a <- rep(NA, vcount(graph_a))  # Initialize all labels to NA
  node_labels_a[acquirer_node_index] <- V(graph_a)$name[acquirer_node_index]
  node_colors_a <- rep("lightblue", vcount(graph_a))  # Default color for all nodes
  node_colors_a[acquirer_node_index] <- "red"
  dates<- append(dates,list(date))
  # Plot the graph
  #x11()
  filepath<-file.path(subdir, paste0(acquirer, ".pdf"))
  pdf(filepath)
  plot(graph_a, 
       vertex.label = node_labels_a,  # Only the central node will have a label
       vertex.size = 3,            # Size of the nodes
       vertex.label.cex = 1, 
       vertex.color = node_colors_a,
       main = "Network Graph with Central Node Label Only (Acquirer)")
  dev.off()
  
  # here we can compute the centrality measures for the central node
  central_degree_a <- degree(graph_a, v = acquirer_node_index)
  central_betweenness_a <- betweenness(graph_a, v = acquirer_node_index)
  central_closeness_a <- closeness(graph_a, v = acquirer_node_index)
  central_burt_constraint_a <- constraint(graph_a, nodes = acquirer_node_index, weights = NULL)
  central_eigenvector_a <- eigen_centrality(graph_a,directed = FALSE,scale = FALSE,weights = NULL)$vector[acquirer_node_index]
  degree_a <- append(degree_a,central_degree_a)
  betw_a <- append(betw_a,central_betweenness_a)
  close_a <- append(close_a,central_closeness_a)
  burt_a <- append(burt_a,central_burt_constraint_a)
  eigen_a <- append(eigen_a,central_eigenvector_a)
  # lets repeat the operation for the target
  aziende_t = df_jv_filt_target$Participants
  split_names_t <- lapply(aziende_t, function(x) unlist(strsplit(x, split = "\r\n")))
  max_length_t <- max(sapply(split_names_t, length))
  padded_names_t <- lapply(split_names_t, function(x) {
    length(x) <- max_length_t
    x
  })
  names_matrix_t <- do.call(rbind, padded_names_t)
  edges_list_t <- do.call(rbind, lapply(1:nrow(names_matrix_t), function(i) extract_pairs(names_matrix_t[i, ])))
  edges_list_t <- unique(edges_list_t)
  edge_list_df_t <- as.data.frame(edges_list_t, stringsAsFactors = FALSE)
  colnames(edge_list_df_t) <- c("Source", "Target")
  graph_t <- graph_from_data_frame(d = edge_list_df_t, directed = FALSE)
  components_info_t <- components(graph_t)
  target_node_index <- which(V(graph_t)$name == target)
  # Find the component that contains the 'acquirer' node
  target_component <- components_info_t$membership[target_node_index]
  # Extract the subgraph that contains all indirect connections of the 'acquirer' node
  graph_t <- induced_subgraph(graph_t, V(graph_t)[components_info_t$membership == target_component])
  edge_list_df_t <-  igraph::as_data_frame(graph_t, what = "edges")
  colnames(edge_list_df_t) <- c("Source", "Target")
  degree_centrality_t <- degree(graph_t)
  target_node_index <- which(V(graph_t)$name == target)
  # nomi dei direct neighbours del target node
  #print(target)
  target_neighbors <- V(graph_t)[neighbors(graph_t, V(graph_t)[V(graph_t)$name == target])]
  target_neighbors<-target_neighbors$name
  # interseco le due liste per vedere i nodi in comune
  shared_nodes_m <- intersect(acquirer_neighbors, target_neighbors)
  # Count the number of shared nodes
  number_of_shared_nodes <- length(shared_nodes_m)
  shared_nodes <- append(shared_nodes,number_of_shared_nodes)
  
  #Plotting commands
  # Create a vector of labels where only the target node's label is visible
  node_labels_t <- rep(NA, vcount(graph_t))  # Initialize all labels to NA
  node_labels_t[target_node_index] <- V(graph_t)$name[target_node_index]
  node_colors_t <- rep("lightblue", vcount(graph_t))  # Default color for all nodes
  node_colors_t[target_node_index] <- "red"
  
  # Plot the graph
  #x11()
  filepath<-file.path(subdir, paste0(target, ".pdf"))
  pdf(filepath)
  plot(graph_t, 
       vertex.label = node_labels_t,  # Only the central node will have a label
       vertex.size = 3,            # Size of the nodes
       vertex.label.cex = 1, 
       vertex.color = node_colors_t,
       main = "Network Graph with Central Node Label Only (Target)")
  dev.off()
 
   # here we can compute the centrality measures for the central node
  central_degree_t <- degree(graph_t, v = target_node_index)
  degree_t<- append(degree_t,central_degree_t)
  central_betweenness_t <- betweenness(graph_t, v = target_node_index)
  betw_t <- append(betw_t,central_betweenness_t)
  central_closeness_t <- closeness(graph_t, v = target_node_index)
  close_t <- append(close_t,central_closeness_t)
  central_burt_constraint_t <- constraint(graph_t, nodes = target_node_index, weights = NULL)
  burt_t <- append(burt_t,central_burt_constraint_t)
  central_eigenvector_t <- eigen_centrality(graph_t,directed = FALSE,scale = FALSE,weights = NULL)$vector[target_node_index]
  eigen_t <- append(eigen_t,central_eigenvector_t)
  # controlliamo se il target è gia nel joint venture net dell acquire
  # se cosi fosse, rimuovo dall edge list dell'acquirer la riga che contiene il target(nodo collassa)
  if (target %in% V(graph_a)$name) {
     #direct_connection<- append(direct_connection,1)
    edge_list_df_a <- edge_list_df_a[!apply(edge_list_df_a, 1, function(row) any(row == target)), ]
  }
  # ora mergiamo i due grafi
  # nel edge list del target, sostituisco il nome del target con quello del acquirer
  edge_list_df_t[] <- lapply(edge_list_df_t, function(x) {
    replace(x, x == target, acquirer)
  })
  # dopo aver sostituito il nome del target con quello del acquirer, rimuovo eventuali self loops
  # dove Source e target rappresentano lo stesso nodo
  edge_list_df_t <- edge_list_df_t[edge_list_df_t$Source !=edge_list_df_t$Target, ]
  # solo se l edge list dell'acquirer non è vuoto (vuol dire che acquirer ha almeno un altra connessione
  # che non sia con il target, posso unire i due dataframe)
  label<-paste0(acquirer,"-",target,".pdf")
  lab<-sub("\\.pdf$", "", label)
  l<-append(l,lab)
  if (dim(edge_list_df_a)[1]>0){
    merged_df <- rbind(edge_list_df_a,edge_list_df_t)
    merged_df<-merged_df[!duplicated(merged_df), ]
    graph_m <- graph_from_data_frame(d = merged_df, directed = FALSE)
    components_info_m <- components(graph_m)
    merg_acquirer_node_index <- which(V(graph_m)$name == acquirer)
    # Find the component that contains the 'acquirer' node
    merged_acquire_component <- components_info_m$membership[merg_acquirer_node_index]
    # Extract the subgraph that contains all indirect connections of the 'acquirer' node
    graph_m <- induced_subgraph(graph_m, V(graph_m)[components_info_m$membership == merged_acquire_component])
    merg_acquirer_node_index <- which(V(graph_m)$name == acquirer)
    degree_centrality_m <- degree(graph_m)
    # Create a vector of labels where only the central node's label is visible
    
    acquirer_node_index_m <- which(V(graph_m)$name == acquirer)
    
    # Create a vector of labels where only the target and acquirer nodes' labels are visible
    #node_labels_m <- rep(NA, vcount(graph_m))  # Initialize all labels to NA
    #node_labels_m[target_node_index_m] <- V(graph_m)$name[target_node_index_m]  # Label the target node
    #node_labels_m[acquirer_node_index_m] <- V(graph_m)$name[acquirer_node_index_m]
    #node_colors_m <- rep("lightblue", vcount(graph_m))  # Default color for all nodes
    #node_colors_m[acquirer_node_index_m] <- "red"
    
    # Plot the graph
    #x11()
    
    filepath<-file.path(subdir, paste0(acquirer,"-",target,".pdf"))
    pdf(filepath)
    #pdf(label)
    plot(graph_m, 
         vertex.label = NA,  # Only the central node will have a label
         vertex.size = 2,            # Size of the nodes
         vertex.label.cex = 1,  
         #vertex.color = node_colors_m,# Size of the labels
         main = "Network Graph with Central Node Label Only (After acquisition)")
    dev.off()
    # here we can compute the centrality measures for the central node
    #print(i)
    central_degree_m <- degree(graph_m, v = acquirer_node_index_m)
    central_betweenness_m <- betweenness(graph_m, v = acquirer_node_index_m)
    central_closeness_m <- closeness(graph_m, v = acquirer_node_index_m)
    central_burt_constraint_m <- constraint(graph_m, nodes = acquirer_node_index_m, weights = NULL)
    central_eigenvector_m <- eigen_centrality(graph_m,directed = FALSE,scale = FALSE,weights = NULL)$vector[acquirer_node_index_m]
    degree_m <- append(degree_m,central_degree_m)
    betw_m <- append(betw_m,central_betweenness_m)
    close_m <- append(close_m,central_closeness_m)
    burt_m <- append(burt_m,central_burt_constraint_m)
    eigen_m <- append(eigen_m,central_eigenvector_m)

} else {
      degree_m <- append(degree_m,NA)
      betw_m <- append(betw_m,NA)
      close_m <- append(close_m,NA)
      burt_m <- append(burt_m,NA)
      eigen_m <- append(eigen_m,NA)
  }
}
end_exec = Sys.time()
run_time <- end_exec - start_exec
print(run_time)
# df dove salvo tutte le informazioni
dates<-as.POSIXct(unlist(dates))
dates <- as.Date(dates)
dates<-as.character(dates)
non_zero_direct_connection <- sum(direct_connection!=0)
direct_connection<-as.character(direct_connection)
non_zero_shared_nodes <-sum(shared_nodes!=0)
shared_nodes<- as.character(shared_nodes)
non_zero_pres_in_same_net <- sum(unlist(pres_in_same_net)!=0)
pres_in_same_net<-as.character(unlist(pres_in_same_net))
degree_matrix<-cbind(dates,l,degree_a,degree_t,degree_m,direct_connection,pres_in_same_net,shared_nodes)
degree_df <- as.data.frame(degree_matrix)
colnames(degree_df)<-c("Date","ID","Acquirer","Target","Merged","Direct Connection","Same net","N of shared nodes")
x<-degree_df[,c(6,7)]
x$`Same net`[x$`Direct Connection` == 1 & x$`Same net` == 0] <- 1
degree_df$`Same net`<- x$`Same net`

#degree_df$Date <- as.character(degree_df$Date)
write.xlsx(degree_df, file = "Degree.xlsx",na.string = "NA")

betw_matrix<-cbind(dates,l,betw_a,betw_t,betw_m,direct_connection,pres_in_same_net,shared_nodes)
betw_df <- as.data.frame(betw_matrix)
colnames(betw_df)<-c("Date","ID","Acquirer","Target","Merged","Direct Connection","Same net","N of shared nodes")
betw_df$`Same net`<-x$`Same net`
write.xlsx(betw_df, file = "Betweenness.xlsx",na.string = "NA")

close_matrix<-cbind(dates,l,close_a,close_t,close_m,direct_connection,pres_in_same_net,shared_nodes)
close_df <- as.data.frame(close_matrix)
colnames(close_df)<-c("Date","ID","Acquirer","Target","Merged","Direct Connection","Same net",
                      "N of shared nodes")
close_df$`Same net`<-x$`Same net`
write.xlsx(close_df, file = "Closeness.xlsx",na.string = "NA")

burt_matrix<-cbind(dates,l,burt_a,burt_t,burt_m,direct_connection,pres_in_same_net,shared_nodes)
burt_df <- as.data.frame(burt_matrix)
colnames(burt_df)<-c("Date","ID","Acquirer","Target","Merged","Direct Connection",
                     "Same net","N of shared nodes")
burt_df$`Same net`<- x$`Same net`
burt_df$delta <- as.numeric(burt_df$Acquirer)-as.numeric(burt_df$Merged)

write.xlsx(burt_df, file = "Burt-Constraint.xlsx",na.string = "NA")

eigen_matrix<-cbind(dates,l,eigen_a,eigen_t,eigen_m,direct_connection,pres_in_same_net,shared_nodes)
eigen_df <- as.data.frame(eigen_matrix)
colnames(eigen_df)<-c("Date","ID","Acquirer","Target","Merged","Direct Connection","Same net","N of shared nodes")
eigen_df$`Same net`<-x$`Same net`
eigen_df$edelta <-as.numeric(eigen_df$Acquirer)-as.numeric(eigen_df$Merged)
write.xlsx(eigen_df, file = "Eigen-Vector.xlsx",na.string = "NA")
sum(eigen_df$`Same net`==1)
