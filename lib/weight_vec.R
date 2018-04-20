#######################################################
############# Calculate weight vector #################
#######################################################

weight_vec = function(labels,
                      data,
                      run.var = FALSE,
                      run.meanvar = FALSE){
  
  ### coumpute weight vector
  
  ### Input: 
  ###   labels - number of clusters assigned
  ###   data - normalized daily return in matrix form,
  ###           col : each stock
  ###           row : each day, and it should be 250
  ###   seed - set.seed(seed)
  ###   run.xxxxxx - select which model to fit
  ### Output: 
  ###   weight matrix
  ###       col : each stock
  ###       row : each date range
  
  # load function
  # source('../lib/mean_variance.R')
  
  data.dframe <- data.frame(sum = colSums(data), labels = factor(labels))
  ind.maxsum <- rep(NA,length(unique(data.dframe$labels)))
  names(ind.maxsum) <- paste(unique(data.dframe$labels))
  count = 1
  for(i in unique(data.dframe$labels)){
    i.ind = which(data.dframe$labels == i)
    maxsum = max(data.dframe$sum[i.ind])
    ind.maxsum[count] = which(data.dframe$sum == maxsum)
    count = count + 1
  }
  # ind.maxsum <- tapply(data.dframe$sum, data.dframe$labels, which.max)
  
  # initial weight vector as 0
  weight_vec=rep(0,ncol(data))
  # initial variance as 0
  vari=rep(0,ncol(data))
  
  # compute selected stock variance
  for (i in ind.maxsum) {
    vari[i]=var(data[,i])
  }
  
  total_var=sum(vari) # total sum variance
  
  for (i in ind.maxsum) {
    if(run.var == TRUE){
      weight_vec[i]=var(data[,i])/total_var
    }
  }
  
  if(run.meanvar == TRUE){
    weight_vec = mean_variance(ind.maxsum)
  }
  return(weight_vec)
}