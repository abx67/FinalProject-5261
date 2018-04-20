library(matrixStats)

data_final <- read.csv('./data/data_final.csv',header = T)

stock.names <- colnames(data_final)[-1]
date <- as.Date(data_final$X.1)

data_mat <- as.matrix(data_final[,-1])
data_mat <- apply(data_mat,2,computereturn)
data_mat <- t(t(data_mat) - colMeans(data_mat))
stock_mat <- data_mat %*% diag(1/ colSds(data_mat) )

# add date
stock_norm <- as.data.frame(cbind(date[-1],as.data.frame(stock_mat)))
colnames(stock_norm) <- c('date',stock.names)
# do not date
stock_norm <- as.data.frame(stock_mat)
colnames(stock_norm) <- stock.names

write.csv(stock_norm, file = './data/stock_normalize_norm.csv', row.names = FALSE)


computereturn <- function(vec){
  return(diff(vec) / vec[-length(vec)])
}

tmp <- matrix(1:9,3)
computereturn(1:9)
