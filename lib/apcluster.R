library(apcluster)
library(gmm)
apclus = apcluster(negDistMat(r=8), t(data),q=0.0005)
apclus2 = apcluster(negDistMat(r=2), t(stock_mat))
show(apclus@clusters)

class(apclus)
which(length(apclus)>5)

apclus[[1]]

lapply(as.list(apclus),length)

len <- rep(NA,length(apclus))
for(i in 1:length(apclus)){
  len[i] <- apclus[[i]]
}

len[which(len>30)]


as.vector(apclus@clusters)

get_label_ap <- function(apclus){
  labels = c()
  ind = 1
  label_count = 1
  for(Cluster in apclus@clusters){
    for (item in Cluster){
      labels[ind] = label_count
      names(labels)[ind] = item
      ind = ind + 1
    }
    label_count = label_count + 1
  }
  return(sort(labels))
}
ap_labels <- get_label_ap(apclus)


table(ap_labels)
colnames(stock_mat) <- stock.names
stock_dis <- dist(t(stock_mat[,1:50]))

stock.hclust = hclust(stock_dis)
plot(stock.hclust,labels=1:50,main='Default from hclust')

stock.hclust$height
