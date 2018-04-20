# 
# mean_variance <- function(index){
#   library(MASS)
#   library(quadprog)
#   price <- data_final[,index]
#   n = dim(price)[1]
#   return = log(price[2:n,]/price[1:(n-1),])
#   mu = colMeans(return)
#   sigma = cov(return)
# 
#   muP = seq(min(mu),max(mu),length=200) # target portfolio return
#   sdP = muP
#   weight = matrix(0,nrow=200,ncol=ncol(price))
#   for (i in 1:length(muP))  # find the optimal portfolios
#   {
#     result = solve.QP(Dmat=2*sigma,dvec=rep(0,ncol(price)),
#                       Amat=cbind(rep(1,ncol(price)),mu,diag(1,ncol(price))),
#                       bvec=c(1,muP[i],rep(0,ncol(price))),meq=2)
#     sdP[i] = sqrt(result$value)
#     weight[i,] = result$solution
#   }
# 
#   ind1 = (sdP == min(sdP)) # find the minimum variance portfolio
# 
#   rf = .05/253 # riskfree rate
#   sharpe =(muP-rf)/sdP # Sharpe's ratios
#   ind3 = (sharpe == max(sharpe)) # find maximum Sharpe's ratio
# 
#   wP <- weight[ind3,] #weight of the portfolio
#   wP2 <- ifelse(wP<=0.0000000001, 0,wP)
#  return(wP2)
# }


mean_variance <- function(index,
                          data_price = data_final_clean){
  library(MASS)
  library(quadprog)
  price <- data_price[,index+1]
  n = dim(price)[1]
  return = log(price[2:n,]/price[1:(n-1),])
  mu = colMeans(return)
  sigma = cov(return)
  diev=sum(diag(sigma))*diag(ncol(return))

  muP = seq(min(mu)+10e-6,max(mu)-10e-6,length=200) # target portfolio return
  sdP = muP
  weight = matrix(0,nrow=200,ncol=ncol(price))
  for (i in 1:length(muP))  # find the optimal portfolios
  {
    result = solve.QP(Dmat=diev+2*sigma,dvec=rep(0,ncol(price)),
                      Amat=cbind(rep(1,ncol(price)),mu,diag(1,ncol(price))),
                      bvec=c(1,muP[i],rep(0,ncol(price))),meq=2)
    sdP[i] = sqrt(result$value)
    weight[i,] = result$solution
  }

  ind1 = (sdP == min(sdP)) # find the minimum variance portfolio

  rf = .05/253 # riskfree rate
  sharpe =(muP-rf)/sdP # Sharpe's ratios
  ind3 = (sharpe == max(sharpe)) # find maximum Sharpe's ratio

  wP <- weight[ind3,] #weight of the portfolio
  wP2 <- ifelse(wP<=0.0000000001, 0,wP)
  weight <- rep(0, ncol(data_price) - 1)
  counti = 0
  for(i in index){
    counti = counti + 1
    weight[i] <- wP2[counti]
  }
  return(weight)
}
