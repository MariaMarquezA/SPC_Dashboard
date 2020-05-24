#Funtion for generating Healy's CUSUM, used for the Shiny App
CUSUM <- function(t_ic, t_oc, shift = 0.01){
  t_bind <- as.matrix(rbind(t_ic,t_oc))
  mu0 <- colMeans(t_ic)
  mu1 <- mu0 + shift
  S = cov(t_ic)
  n_bind = length(t_bind[,1])
  n_ic = length(t_ic[,1])
  p = 19
  
  cn = rep(0,n_bind)   
  D = sqrt(t(mu1-mu0) %*% solve(S) %*% (mu1-mu0))[1]
  a = (t(mu1-mu0)) %*% solve(S)/D
  
  for(i in 2:n_bind){
    cn[i]=max(0, cn[i-1] + a %*% (t_bind[i,]-mu0) - D/2)#Healy's Statistic
  }
  
  return(cn)
}