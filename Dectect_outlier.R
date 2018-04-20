outlier.stock <- c('SUN','FTAI','ARWR','GIII','CASC','SRPT','MERC','LNDC','STRL','OSIS')
outlier.ind <- match(outlier.stock,names(stock_norm))

stock_norm_clean <- stock_norm[,-outlier.ind]

cluster.clean <- cluster(num.cluster = 5,
                         data = stock_norm_clean,
                         seed = 1,
                         run.kmeans = TRUE)

table(cluster.clean$meansd$sd)


match(outlier.stock,cluster.clean$stock.names) 
dim(stock_norm_clean)


#  35  76 197  92  82 