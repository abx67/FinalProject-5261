#######################################################
############# Calculate weight matrix #################
#######################################################

compute_weights <- function(data = stock.mat,
                            seed = 1,
                            run.kmeans = FALSE,
                            run.var = FALSE,
                            run.meanvar = FALSE){
  
  ### coumpute weight matrix
  
  ### Input: 
  ###   num.cluster - number of clusters assigned
  ###   data - normalized daily return in matrix form,
  ###           col : each stock
  ###           row : each day
  ###   seed - set.seed(seed)
  ###   run.xxxxxx - select which model to fit
  ### Output: 
  ###   weight matrix
  ###       col : each stock
  ###       row : each date range
  
  
  ## load functions
  source('../lib/Cluster.R')
  source('../lib/weight_vec.R') # pass

  ## compute some data info
  dcol = ncol(data)
  drow = nrow(data)
  
  ## initial weight matrix
  weight_mat_nrow <- floor((drow-250)/60)
  weight_mat <- matrix(NA, ncol = dcol, nrow = weight_mat_nrow)
  
  ## for loop to compute weight matrix
  for(i in 1:weight_mat_nrow){
    print(i)
    ini.ind <- (i-1) * 60 + 1
    sel.ind <- seq(ini.ind, ini.ind + 250)
    sub.dat <- data[sel.ind,]
    sub.labels <- cluster(num.cluster = 3,
                          data = sub.dat,
                          seed = seed,
                          run.kmeans = TRUE)
    weight_mat[i,] <- weight_vec(sub.labels,sub.dat)
  }
  
  return(weight_mat)
}




  