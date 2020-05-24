#Function for Hotelling's T2, used for the Shiny App
T2 <- function(t_ic,t_oc){
  library(ggplot2)

  p <- 19
  t_bind <- as.matrix.data.frame(rbind(t_ic, t_oc))
  n = length(t_bind[,1])
  
  mu0 = colMeans(t_ic)
  mu1 = colMeans(t_oc)
  S = cov(t_ic)
  
  T2 = rep(0,n)
  
  for(i in 1:n){
    T2[i] = t(t_bind[i,]-mu0) %*% solve(S) %*% (t_bind[i,]-mu0) #T2 statistic
  }
  return(T2)
}
