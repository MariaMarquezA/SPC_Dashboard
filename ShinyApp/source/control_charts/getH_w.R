getH_w <- function(t_ic, t_oc, shift = 0.01){
  library(CUSUMdesign)
  mu0 = colMeans(t_ic)
  mu1 = mu0 + shift
  S = cov(t_ic)

  D = sqrt(t(mu1-mu0) %*% solve(S) %*% (mu1-mu0))[1]
  H = getH(distr=1, ARL=370, ICmean=0, ICsd=1, 
           OOCmean=D,
           ref=NULL,
           type="zero start")
  return(H$DI)
}
