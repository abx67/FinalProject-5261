#######################################
############# Cluster #################
#######################################

cluster <- function(num.cluster = 3,
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
  
  set.seed(seed)
  
  if(run.kmeans == TRUE){
    kc <- kmeans(t(data),
                 centers = num.cluster,
                 iter.max = 100,
                 nstart = 40,
                 algorithm = "Forgy")
  }
  
  # table(kc$cluster)
  # set nstart = 40 and 3 clusters
  # 1   2   3 
  # 81 204 180 
  
  # 1   2   3   4 
  # 181 158  80  46 
  return(kc$cluster)
}