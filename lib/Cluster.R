#######################################
############# Cluster #################
#######################################

cluster <- function(num.cluster = 5,
                    data = stock.mat,
                    seed = 1,
                    run.kmeans = FALSE){
  
  ### build cluster model
  
  ### Input: 
  ###   num.cluster - number of clusters assigned
  ###   data - normalized daily return, column 
  ###   seed - set.seed(seed)
  ###   run.xxxxxx - select which model to fit
  ### Output: 
  ###   labels assigned to each stock
  
  library(apcluster)
  
  set.seed(seed)
  
  # transpose data
  tdata <- t(as.matrix(data))
  
  if(run.kmeans == TRUE){
    kc <- kmeans(tdata,
                 centers = num.cluster,
                 iter.max = 100,
                 nstart = 40,
                 algorithm = "Forgy")
  }
  
  cluster.data <- data.frame(label = kc$cluster,means = rowMeans(tdata))
  cluster.mean <- tapply(cluster.data$means,cluster.data$label,mean)
  cluster.sd <- tapply(cluster.data$means,cluster.data$label,sd)
  
  if(is.null(colnames(data))) return(kc$cluster)
  else return(list(stock.names = colnames(data),
                   label = kc$cluster,
                   meansd = list(mean = cluster.mean,
                                 sd = cluster.sd,
                                 cluster.name = names(cluster.mean)
                                 )
                   )
              )
}