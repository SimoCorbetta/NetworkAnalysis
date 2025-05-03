# Regressione
library(lubridate)
library(igraph)
library(Matrix)
library(network)
library(readxl)
library(openxlsx)
library(rrr)
full_R2 <- {}
full_adjR2<-{}
loc_adjR2 <- {}
glob_adjR2 <-{}
ex_adjR2 <-{}
full_car<- {}
local_car <- {}
global_car <- {}
exchange_car <- {}
any_zeros <- {}
target <- {}
acquirer <- {}
dates<- {}
df_markets = read_excel("Datastream - Market returns_V.xlsx", na = "NA")
df_markets <- as.data.frame(df_markets)
df_global <- read_excel("Datastream - Global returns_V.xlsx", na = "NA")
df_global <- as.data.frame(df_global)
df_stock <- read_excel("Datastream - Stock returns_V.xlsx", na = "NA")
df_stock <- as.data.frame(df_stock)
df_exchange <- read_excel("Datastream - Exchange rate_V.xlsx", na = "NA")
df_exchange <- as.data.frame(df_exchange)

market_na<-rowSums(is.na(df_markets))
stock_na <- rowSums(is.na(df_stock))
glob_na <- rowSums(is.na(df_global))
ex_na <- rowSums(is.na(df_exchange))
# estimation window da - 11 a -110
# event è la colonna 373
r<- dim(df_markets)[1]

for (i in 1:r) {
  print(i)
  e1<-371
  e<-373
  e2<-375
  p<-df_markets[i,]
  dates <- append(dates,p$`Date Announced`)
  target <- append(target,p$`Target Short Name`)
  acquirer <- append(acquirer,p$`Acquiror Short Name`)
  est_m<-as.numeric(p[,263:362])
  
  if (p$WD==1){
    meno_2<- e-4
    meno_1 <- e-3
    piu_1<- e+1
    piu_2 <- e+2
    
    even_m<-p[,c(meno_2,meno_1,e,piu_1,piu_1)]
  }
  if (p$WD==2){
    meno_2<- e-4
    meno_1 <- e-1
    piu_1<- e+1
    piu_2 <- e+2
    
    even_m<-p[,c(meno_2,meno_1,e,piu_1,piu_1)]
  }
  if (p$WD==3){
    meno_2<- e-2
    meno_1 <- e-1
    piu_1<- e+1
    piu_2 <- e+2
    
    even_m<-p[,c(meno_2,meno_1,e,piu_1,piu_2)]
  }
  
  if (p$WD==4){
    meno_2<- e-2
    meno_1 <- e-1
    piu_1<- e+1
    piu_2 <- e+3
    
    even_m<-p[,c(meno_2,meno_1,e,piu_1,piu_2)]
  }
  if (p$WD==5){
    meno_2<- e-2
    meno_1 <- e-1
    piu_1<- e+3
    piu_2 <- e+4
    
    even_m<-p[,c(meno_2,meno_1,e,piu_1,piu_1)]
  }
  if (p$WD==6){
    meno_2<- e-2
    meno_1 <- e-1
    ed <- e+2 
    piu_1<- e+3
    piu_2 <- e+4
    even_m<-p[,c(meno_2,meno_1,ed,piu_1,piu_1)]
  }
  if (p$WD==7){
    meno_2<- e-3
    meno_1 <- e-2
    ed <- e+1 
    piu_1<- e+2
    piu_2 <- e+3
    even_m<-p[,c(meno_2,meno_1,ed,piu_1,piu_1)]
  }
  
  
  # event è nellaa colonna 373
  globe<-df_global[i, ]
  g1<-371
  g<-373
  g2<-375
  est_g<-as.numeric(globe[,263:362])
  if (globe$WD==1){
    meno_2<- g-4
    meno_1 <- g-3
    piu_1<- g+1
    piu_2 <- g+2
    
    even_g<-globe[,c(meno_2,meno_1,g,piu_1,piu_1)]
  }
  if (globe$WD==2){
    meno_2<- g-4
    meno_1 <- g-1
    piu_1<- g+1
    piu_2 <- g+2
    
    even_g<-globe[,c(meno_2,meno_1,g,piu_1,piu_1)]
  }
  if (globe$WD==3){
    meno_2<- g-2
    meno_1 <- g-1
    piu_1<- g+1
    piu_2 <- g+2
    
    even_g<-globe[,c(meno_2,meno_1,g,piu_1,piu_2)]
  }
  
  if (globe$WD==4){
    meno_2<- g-2
    meno_1 <- g-1
    piu_1<- g+1
    piu_2 <- g+3
    
    even_g<-globe[,c(meno_2,meno_1,g,piu_1,piu_2)]
  }
  if (globe$WD==5){
    meno_2<- g-2
    meno_1 <- g-1
    piu_1<- g+3
    piu_2 <- g+4
    
    even_g<-globe[,c(meno_2,meno_1,g,piu_1,piu_1)]
  }
  if (globe$WD==6){
    meno_2<- g-2
    meno_1 <- g-1
    ed <- g+2 
    piu_1<- g+3
    piu_2 <- g+4
    even_g<-globe[,c(meno_2,meno_1,ed,piu_1,piu_1)]
  }
  if (globe$WD==7){
    meno_2<- g-3
    meno_1 <- g-2
    ed <- g+1 
    piu_1<- g+2
    piu_2 <- g+3
    even_g<-globe[,c(meno_2,meno_1,ed,piu_1,piu_1)]
  }
  
  # la colonna event è quella 372
  stock<-df_stock[i,]
  s1<-370
  s<- 372
  s2<-374
  est_s<-as.numeric(stock[,262:361])
  if (stock$WD==1){
    meno_2<- s-4
    meno_1 <- s-3
    piu_1<- s+1
    piu_2 <- s+2
    
    even_s<-stock[,c(meno_2,meno_1,s,piu_1,piu_1)]
  }
  if (stock$WD==2){
    meno_2<- s-4
    meno_1 <- s-1
    piu_1<- s+1
    piu_2 <- s+2
    
    even_s<-stock[,c(meno_2,meno_1,s,piu_1,piu_1)]
  }
  if (stock$WD==3){
    meno_2<- s-2
    meno_1 <- s-1
    piu_1<- s+1
    piu_2 <- s+2
    
    even_s<-stock[,c(meno_2,meno_1,s,piu_1,piu_2)]
  }
  
  if (stock$WD==4){
    meno_2<- s-2
    meno_1 <- s-1
    piu_1<- s+1
    piu_2 <- s+3
    
    even_s<-stock[,c(meno_2,meno_1,s,piu_1,piu_2)]
  }
  if (stock$WD==5){
    meno_2<- s-2
    meno_1 <- s-1
    piu_1<- s+3
    piu_2 <- s+4
    
    even_s<-stock[,c(meno_2,meno_1,s,piu_1,piu_1)]
  }
  if (stock$WD==6){
    meno_2<- s-2
    meno_1 <- s-1
    ed <- s+2 
    piu_1<- s+3
    piu_2 <- s+4
    even_s<-stock[,c(meno_2,meno_1,ed,piu_1,piu_1)]
  }
  if (stock$WD==7){
    meno_2<- s-3
    meno_1 <- s-2
    ed <- s+1 
    piu_1<- s+2
    piu_2 <- s+3
    even_s<-stock[,c(meno_2,meno_1,ed,piu_1,piu_1)]
  }
  
  # la colonna event è la 373
  e<-df_exchange[i, ]
  ex1<-371
  ex<- 373
  ex2<-375
  est_e<-as.numeric(e[,263:362])
  if (e$WD==1){
    meno_2<- ex-4
    meno_1 <- ex-3
    piu_1<- ex+1
    piu_2 <- ex+2
    
    even_e<-e[,c(meno_2,meno_1,ex,piu_1,piu_1)]
  }
  if (e$WD==2){
    meno_2<- ex-4
    meno_1 <- ex-1
    piu_1<- ex+1
    piu_2 <- ex+2
    
    even_e<-e[,c(meno_2,meno_1,ex,piu_1,piu_1)]
  }
  if (e$WD==3){
    meno_2<- ex-2
    meno_1 <- ex-1
    piu_1<- ex+1
    piu_2 <- ex+2
    
    even_e<-e[,c(meno_2,meno_1,ex,piu_1,piu_2)]
  }
  
  if (e$WD==4){
    meno_2<- ex-2
    meno_1 <- ex-1
    piu_1<- ex+1
    piu_2 <- ex+3
    
    even_e<-e[,c(meno_2,meno_1,ex,piu_1,piu_2)]
  }
  if (e$WD==5){
    meno_2<- ex-2
    meno_1 <- ex-1
    piu_1<- ex+3
    piu_2 <- ex+4
    
    even_e<-e[,c(meno_2,meno_1,ex,piu_1,piu_1)]
  }
  if (e$WD==6){
    meno_2<- ex-2
    meno_1 <- ex-1
    ed <- ex+2 
    piu_1<- ex+3
    piu_2 <- ex+4
    even_e<-e[,c(meno_2,meno_1,ed,piu_1,piu_1)]
  }
  if (e$WD==7){
    meno_2<- ex-3
    meno_1 <- ex-2
    ed <- ex+1 
    piu_1<- ex+2
    piu_2 <- ex+3
    even_s<-e[,c(meno_2,meno_1,ed,piu_1,piu_1)]
  }
  mat_0 <- cbind(est_s,est_g,est_m,est_e)
  d_0 <- dim(mat_0)[1]
  #print(d_0)
  colnames(mat_0)<-c("stock","global","local","exchange")
  cleaned_mat_0 <- mat_0[!(mat_0[, "stock"] == 0 | mat_0[, "global"] == 0 | mat_0[, "local"] == 0), ]
  cd_0 <- dim(cleaned_mat_0)[1]
 # print(cd_0)
  if (cd_0==0 || is.null(cd_0)){
    full_car <- append(full_car,NA)
    full_R2<-append(full_R2,NA)
    full_adjR2<-append(full_adjR2,NA)
    local_car <- append(local_car,NA)
    loc_adjR2 <- append(loc_adjR2,NA)
    glob_adjR2 <- append(glob_adjR2,NA)
    ex_adjR2 <- append(ex_adjR2,NA)
    global_car <- append(global_car,NA)
    exchange_car <- append(exchange_car,NA)
    any_zeros <- append(any_zeros,"all")
    next
  }
  zeros_0 <- d_0 - cd_0
  any_zeros <- append(any_zeros,zeros_0)
  
  if  (any(is.na(even_s))){
    full_car <- append(full_car,NA)
    full_R2<-append(full_R2,NA)
    full_adjR2<-append(full_adjR2,NA)
    local_car <- append(local_car,NA)
    loc_adjR2 <- append(loc_adjR2,NA)
    glob_adjR2 <- append(glob_adjR2,NA)
    ex_adjR2 <- append(ex_adjR2,NA)
    global_car <- append(global_car,NA)
    exchange_car <- append(exchange_car,NA)
    next
  }
  y<- est_s
  if  (any(is.na(y))){
    full_car <- append(full_car,NA)
    full_R2<-append(full_R2,NA)
    full_adjR2<-append(full_adjR2,NA)
    local_car <- append(local_car,NA)
    loc_adjR2 <- append(loc_adjR2,NA)
    glob_adjR2 <- append(glob_adjR2,NA)
    ex_adjR2 <- append(ex_adjR2,NA)
    global_car <- append(global_car,NA)
    exchange_car <- append(exchange_car,NA)
    next
  }
  xg<-est_g
  if  (any(is.na(xg))){
    full_car <- append(full_car,NA)
    full_R2<-append(full_R2,NA)
    full_adjR2<-append(full_adjR2,NA)
    global_car <- append(global_car,NA)
    next
  }
  
  xm<-est_m
  if  (any(is.na(xm))){
    full_car <- append(full_car,NA)
    full_R2<-append(full_R2,NA)
    full_adjR2<-append(full_adjR2,NA)
    local_car <- append(local_car,NA)
    loc_adjR2 <- append(loc_adjR2,NA)
    glob_adjR2 <- append(glob_adjR2,NA)
    ex_adjR2 <- append(ex_adjR2,NA)
    global_car<- append(global_car,NA)
    exchange_car <- append(exchange_car,NA)
    next
  }
  
  xe<-est_e
  if  (any(is.na(xe))){
    full_car <- append(full_car,NA)
    full_R2<-append(full_R2,NA)
    loc_adjR2 <- append(loc_adjR2,NA)
    glob_adjR2 <- append(glob_adjR2,NA)
    ex_adjR2 <- append(ex_adjR2,NA)
    full_adjR2<-append(full_adjR2,NA)
    exchange_car <- append(exchange_car,NA)
    next
  }
  
  mat <- cbind(y,xg,xm,xe)
  colnames(mat)<-c("stock","global","local","exchange")
  cleaned_mat_0 <- mat[!(mat[, "stock"] == 0 | mat[, "global"] == 0 | mat[, "local"] == 0), ]
  colnames(cleaned_mat_0)<-c("stock","global","local","exchange")
  df<-as.data.frame(cleaned_mat_0)
  
  model <- lm(stock ~ global + local + exchange, data = df)
  model_l <- lm(stock~local, data=df)
  model_g <- lm(stock~global, data=df)
  model_e <- lm(stock~exchange, data=df)
  
  # Summary of the model
  #summary(model)
  R2 <- summary(model)$r.squared
  adjusted_R2 <- summary(model)$adj.r.squared
  full_R2 <- append(full_R2,R2)
  full_adjR2 <- append(full_adjR2,adjusted_R2)
  tab <-  summary(model)$coefficients
  tab <- tab[,1]
  inter<- tab[1]
  coeff_g <- tab[2]
  coeff_l <- tab[3]
  coeff_e <- tab[4]
  if (is.na(coeff_e)) {
    coeff_e <- 0
  }
  if (is.na(coeff_g)) {
    coeff_g <- 0
  }
  if (is.na(coeff_l)) {
    coeff_l <- 0
  }
  sum_g <- coeff_g * as.numeric(even_g) 
  sum_l <- coeff_l * as.numeric(even_m)
  sum_e <- coeff_e * as.numeric(even_e)
  int<- c(as.numeric(inter),as.numeric(inter),as.numeric(inter),
          as.numeric(inter),as.numeric(inter))
  res <- rbind(sum_g,sum_l,sum_e,int)
  expected_returns <- colSums(res)
  abnormal <- as.numeric(even_s) - expected_returns
  cumulative_abnormal <- sum(abnormal)
  full_car<-append(full_car,cumulative_abnormal)
  # let s evaluate local market model
  adjr2_local<-summary(model_l)$adj.r.squared
  loc_adjR2<- append( loc_adjR2,adjr2_local)
  tab_l <-  summary(model_l)$coefficients
  tab_l <- tab_l[,1]
  inter_l<- tab_l[1]
  coeff_loc <- tab_l[2]
  if (is.na(coeff_loc)) {
    coeff_loc <- 0
  }
  sum_loc <- coeff_loc * as.numeric(even_m)
  int_l<- c(as.numeric(inter_l),as.numeric(inter_l),as.numeric(inter_l),
            as.numeric(inter_l),as.numeric(inter_l))
  res_l <- rbind(sum_loc,int_l)
  expected_returns_l <- colSums(res_l)
  abnormal_l <- as.numeric(even_s) - expected_returns_l
  cumulative_abnormal_l <- sum(abnormal_l)
  local_car<-append(local_car,cumulative_abnormal_l)
  # global market model
  adjR2_glob <- summary(model_g)$adj.r.squared
  glob_adjR2<- append(glob_adjR2,adjR2_glob)
  tab_g <-  summary(model_g)$coefficients
  tab_g <- tab_g[,1]
  inter_g<- tab_g[1]
  coeff_glob <- tab_g[2]
  if (is.na(coeff_glob)) {
    coeff_glob <- 0
  }
  sum_glob <- coeff_glob * as.numeric(even_g)
  int_g<- c(as.numeric(inter_g),as.numeric(inter_g),as.numeric(inter_g),
            as.numeric(inter_g),as.numeric(inter_g))
  res_g <- rbind(sum_glob,int_g)
  expected_returns_g <- colSums(res_g)
  abnormal_g <- as.numeric(even_s) - expected_returns_g
  cumulative_abnormal_g <- sum(abnormal_g)
  global_car<-append(global_car,cumulative_abnormal_g)
  # exchange rate model
  adjR2_exchange <- summary(model_e)$adj.r.squared
  ex_adjR2<-append(ex_adjR2,adjR2_exchange)
  tab_e <-  summary(model_e)$coefficients
  tab_e <- tab_e[,1]
  inter_e<- tab_e[1]
  coeff_exchange <- tab_e[2]
  if (is.na(coeff_exchange)) {
    coeff_exchange <- 0
  }
  sum_e <- coeff_exchange * as.numeric(even_e)
  int_e<- c(as.numeric(inter_e),as.numeric(inter_e),as.numeric(inter_e),
            as.numeric(inter_e),as.numeric(inter_e))
  res_e <- rbind(sum_e,int_e)
  expected_returns_e <- colSums(res_e)
  abnormal_e <- as.numeric(even_s) - expected_returns_e
  cumulative_abnormal_e <- sum(abnormal_e)
  exchange_car<-append(exchange_car,cumulative_abnormal_e)
  
}

dates<-as.POSIXct(unlist(dates))
dates <- as.Date(dates)
dates<-as.character(dates)
car_matrix<-cbind(dates,acquirer,target,full_car,full_R2,full_adjR2,any_zeros)
car_df_fullModel <- as.data.frame(car_matrix)
colnames(car_df_fullModel)<-c("Date","Acquirer","Target","CAR","R2","AdjR2", "windows removed")
car_df_fullModel$CAR <- ifelse(is.na(car_df_fullModel$CAR), "NA", car_df_fullModel$CAR)
car_df_fullModel$R2 <- ifelse(is.na(car_df_fullModel$R2), "NA", car_df_fullModel$R2)
car_df_fullModel$AdjR2 <- ifelse(is.na(car_df_fullModel$AdjR2), "NA", car_df_fullModel$AdjR2)
write.xlsx(car_df_fullModel, file = "CAR_fullModel.xlsx",na = "NA")
adjR2_filt <- na.omit(full_adjR2)# 2347
adjR2_biggerthanzero<- sum(adjR2_filt >0)#2055
adjR2_biggerthan.5 <- sum(adjR2_filt >0.5)#189

#
# write file for local market model
car_matrix_l<-cbind(dates,acquirer,target,local_car,loc_adjR2)
car_df_l <- as.data.frame(car_matrix_l)
colnames(car_df_l)<-c("Date","Acquirer","Target","CAR","adjR2")
car_df_l$CAR <- ifelse(is.na(car_df_l$CAR), "NA", car_df_l$CAR)
car_df_l$adjR2 <- ifelse(is.na(car_df_l$adjR2), "NA", car_df_l$adjR2)
write.xlsx(car_df_l, file = "CAR_localMarketModel.xlsx",na = "NA")
adjR2_local_filt <- na.omit(loc_adjR2)#2374
adjR2_local_biggerthanzero<- sum(adjR2_local_filt >0)#1967
adjR2_local_biggerthan.5 <- sum(adjR2_local_filt >0.5) # 173
#write file for global market model
car_matrix_g<-cbind(dates,acquirer,target,global_car,glob_adjR2)
car_df_g <- as.data.frame(car_matrix_g)
colnames(car_df_g)<-c("Date","Acquirer","Target","CAR","adjR2")
car_df_g$CAR <- ifelse(is.na(car_df_g$CAR), "NA", car_df_g$CAR)
car_df_g$adjR2 <- ifelse(is.na(car_df_g$adjR2), "NA", car_df_g$adjR2)
write.xlsx(car_df_g, file = "CAR_globalMarketModel.xlsx",na = "NA")
adjR2_global_filt <- na.omit(glob_adjR2)#2374
adjR2_global_biggerthanzero<- sum(adjR2_global_filt >0)#1770
adjR2_global_biggerthan.5 <- sum(adjR2_global_filt >0.5) # 50
# write file for exchange rate model
car_matrix_e<-cbind(dates,acquirer,target,exchange_car,ex_adjR2)
car_df_e <- as.data.frame(car_matrix_e)
colnames(car_df_e)<-c("Date","Acquirer","Target","CAR","adjR2")
car_df_e$CAR <- ifelse(is.na(car_df_e$CAR), "NA", car_df_e$CAR)
car_df_e$adjR2<- ifelse(is.na(car_df_e$adjR2), "NA", car_df_e$adjR2)
write.xlsx(car_df_e, file = "CAR_ExchangeRateModel.xlsx",na = "NA")
adjR2_exchange_filt <- na.omit(ex_adjR2)#2382
adjR2_exchange_biggerthanzero<- sum(adjR2_exchange_filt >0)#704
adjR2_exchange_biggerthan.5 <- sum(adjR2_exchange_filt >0.5) #1
# let s evaluate with a t.test that adjR2 del modello completo è significativamente piu
# alto rispetto agli altri modelli
shapiro.test(full_adjR2) # data not normally distributed
shapiro.test(loc_adjR2) # data not normally distributed
shapiro.test(glob_adjR2) # data not normally distributed
shapiro.test(ex_adjR2) # data not normally distributed
wilcox.test(full_adjR2,loc_adjR2,alternative="g")
# adjR2 del modello completo non sono significativamente piu alti del modello con solo local
wilcox.test(full_adjR2,glob_adjR2,alternative="g")
# adjR2 del modello completo sono significativamente piu alti del modello con solo global
wilcox.test(full_adjR2,ex_adjR2,alternative="g")
# adjR2 del modello completo sono significativamente piu alti del modello con solo exchange
wilcox.test(loc_adjR2,glob_adjR2,alternative="g")
# adjR2 del modello locale sono significativamente piu alti del globale
wilcox.test(glob_adjR2,ex_adjR2,alternative="g")
# adjR2 del modello globale sono significativamente piu alti del modello exchange
adjR2_df <- cbind(adjR2_filt,adjR2_local_filt,adjR2_global_filt,adjR2_exchange_filt)
colnames(adjR2_df) <- c("Full","Local","Global","Exchange")
adjR2_df <- as.data.frame(adjR2_df)
x11()
boxplot(adjR2_df, 
        main = "Box Plot of Adjusted R-squared Values",
        xlab = "Model Type",
        ylab = "Adjusted R-squared",
        col = c("lightblue", "lightgreen", "lightpink", "lightyellow"),
        outline = FALSE)

## let s see if there are difference on the CAR
shapiro.test(full_car) # not  normal distributed data
shapiro.test(local_car) # not  normal distributed data
shapiro.test(global_car) #  not  normal distributed data
shapiro.test(exchange_car) #not  normal distributed data
wilcox.test(full_car,local_car) # not enough statistical evidence to state they are different
wilcox.test(full_car,global_car) # not enough statistical evidence to state they are different
wilcox.test(full_car,exchange_car) # not enough statistical evidence to state they are different
wilcox.test(local_car,global_car) # not enough statistical evidence to state they are different
wilcox.test(local_car,exchange_car) # not enough statistical evidence to state they are different
wilcox.test(global_car,exchange_car) # not enough statistical evidence to state they are different

CAR_allmodels_df <- cbind(full_car,local_car,global_car,exchange_car)
colnames(CAR_allmodels_df) <- c("Full","Local","Global","Exchange")
CAR_allmodels_df <- as.data.frame(CAR_allmodels_df)
x11()
boxplot(CAR_allmodels_df, 
        main = "Box Plot of CAR values",
        xlab = "Model Type",
        ylab = "CAR",
        col = c("lightblue", "lightgreen", "lightpink", "lightyellow"),
        outline = FALSE)
length(full_car)
mean(full_car,na.rm=T)
sd(full_car,na.rm=T)

wilcox.test(full_car,x)

df_burt = read_excel("Burt-Constraint.xlsx")
df_eigen = read_excel("Eigen-Vector.xlsx")
df_CAR<- read_excel("CAR_fullModel.xlsx")
li<-df_burt$ID
acquirenti <- vector("list", length(li))  # Create an empty list to store first words
bersagli<-vector("list", length(li))
for (i in 1:length(li)) {
  acquirenti[[i]] <- strsplit(li[[i]], "-")[[1]][1]  # Split and take the first word
  bersagli[[i]]<- strsplit(li[[i]], "-")[[1]][2]
}
acquirenti<-unlist(acquirenti)
bersagli<- unlist(bersagli)
df_CAR$identific <- paste(df_CAR$Acquirer,df_CAR$Target,sep="-")

# If you want to check the filtered data frame
join_df <- merge(df_burt, df_CAR, by.x = "ID", by.y = "identific")

y<- as.numeric(join_df$CAR)

x <- as.numeric(join_df$delta)
mat_1 <- cbind(y,x)
colnames(mat_1)<-c("CAR","deltaburt")
df_1<-as.data.frame(mat_1)
df_2 <- na.omit(df_1)
df_2 <- df_2[!(df_2$CAR == "NA" | df_2$deltaburt == "NA"), ]
model_1 <- lm(CAR ~ deltaburt, data = df_2)

join_df_Eigen <- merge(df_eigen, df_CAR, by.x = "ID", by.y = "identific")
y<- as.numeric(join_df_Eigen$CAR)
x <- as.numeric(join_df_Eigen$edelta)
mat_1 <- cbind(y,x)
colnames(mat_1)<-c("CAR","Edelta")
df_1<-as.data.frame(mat_1)
df_2 <- na.omit(df_1)
df_2 <- df_2[!(df_2$CAR == "NA" | df_2$Edelta == "NA"), ]
model_1 <- lm(CAR ~ Edelta, data = df_2)

