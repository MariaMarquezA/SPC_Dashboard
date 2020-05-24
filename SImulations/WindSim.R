#Function to get ARL for a given h, calibration mu (as a mean vector), mu0 (as a mean vector),
#mu1 (as a mean vector), RLx= from which observation mu0 will change to mu1, W= winsorization limit,
#pts= number of variables that will change to mu1
getRL.Healy <- function(h, Sigma,muc, mu0, mu1, RLx = -1, W = 0, pts = 0) {
  library(MASS,robustHD)
  p = length(mu0)
  D = sqrt(t(muc-mu0)%*% solve(Sigma) %*% (muc-mu0))[1]
  a = (t(muc-mu0)) %*% solve(Sigma)/D
  IC = TRUE
  mu = rep(0,p)
  RL = 0
  C0 = 0
  C1 = 0
  while (C0 <= h) {
    RL = RL+1
    if (RL >= RLx && RLx != -1) {
      if (pts != 0){
        mu = mu0
        for (i in pts) {
          mu[i] = mu1[i]
        }
      }
      else {
        mu = mu1
      }
    }
    else {
      mu = mu0
    }
    x = mvrnorm(1,mu,Sigma)
    Z = ((a) %*% (x - mu0))[1]
    # windsorizaci?n
    if(Z > W && W != 0) Z = W
    C0 = max(0, C1 + Z - D/2)
    #print(C0)
    C1 = C0;
  }
  return(RL)
}

getARL.Healy <- function(h, Sigma, muc, mu0, mu1, repl=10000, RLx = -1, W = 0, pts = 0) {
  RL=rep(NA,repl)
  for (i in 1:repl){
    RL[i]=getRL.Healy(h,Sigma, muc, mu0, mu1, W = W, RLx = RLx, pts = pts)
    if (i%%100==0) cat("ARL =",mean(RL[1:i]),"in replication",i,"\n")
  }
  ARL = mean(RL)
  SDRL=sd(RL)
  return(list(ARL=ARL,SDRL=SDRL))
}