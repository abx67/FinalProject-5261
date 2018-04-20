#######################################################
############# Calculate weight matrix #################
#######################################################

compute_weights <- function(data = stock_mat,
                            seed = 1,
                            run.kmeans = FALSE,
                            run.var = FALSE,
                            run.meanvar = FALSE,
                            num.cluster = 5){
  
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
  if(run.meanvar) weight_mat_meanvar <- matrix(NA, ncol = dcol, nrow = weight_mat_nrow)
  if(run.var) weight_mat_var <- matrix(NA, ncol = dcol, nrow = weight_mat_nrow)
  ## for loop to compute weight matrix
  for(i in 1:weight_mat_nrow){
    cat('\r',i)
    ini.ind <- (i-1) * 60 + 1
    sel.ind <- seq(ini.ind, ini.ind + 250)
    sub.dat <- data[sel.ind,]
    sub.labels <- cluster(num.cluster = num.cluster,
                          data = sub.dat,
                          seed = seed,
                          run.kmeans = TRUE)
    if(run.meanvar){
      weight_mat_meanvar[i,] <- weight_vec(sub.labels,sub.dat,
                                           run.meanvar = TRUE)}
    if(run.var){
      weight_mat_var[i,] <- weight_vec(sub.labels,sub.dat,
                                       run.var = TRUE)}
  }
  cat('\n')
  if(run.meanvar == TRUE & run.var == FALSE)  return(weight_mat_meanvar)
  if(run.meanvar == FALSE & run.var == TRUE)  return(weight_mat_var)
  if(run.meanvar == TRUE & run.var == TRUE)  return(list(weight_mat_meanvar, weight_mat_var))
}

  