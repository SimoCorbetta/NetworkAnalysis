# NetworkAnalysis
It contains work done in collaboration with Bocconi University graduated, in particular about the  analysis of Biotech companies networks.
We started from data containing information on merging and acquisitions (M&A) between biotech companies in the last 15 years; and we also retrieved for these companies informations about joint ventures in the same years.
For each company that participated in a M&A, we build the networks made of companies involved in the joint venture before and after the acquisition and summarize this with some centrality measures such as Burt constraint, degree centrality, eigen vector centrality and closeness centrality. We compute the difference of those centrality measures before and after the acquisition

We then evaluate using a global market model the stock price in the company in the 100 previous days before the acquisition and compute the cumulative abnormal return (CAR), which is the difference between the predicted stock price based on the market performance in the last 100 days before the acquisition and the actual price.

We finally performed a regression to understand wheter there was a link between CAR and and the difference of the centrality measures.

The rationale of the project was to understand if there was a link between the stock price at the day of acquisition and the position of the company inside the joint venture networks because several studies suggest that companies increase their value when they become "bottlenecks" nodes, thus limiting other companies the possibility to access particular resources and gaining a strategic advantage.
