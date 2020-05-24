#------------------------------------------------------------------------
#FUNCIONES
#------------------------------------------------------------------------

getRL.Healey <- function(h, w, mu, muG, muB, Sigma0, noise.p, noise.size) {
  # noise.p: noise probability
  # noise.size: multiplies muB, if D(muB) = 1 then noise.size is standard
  library(MASS,robustHD)
  
  muG = matrix(data=muG, nrow = length(muG), ncol = 1)
  muB = matrix(data=muB, nrow = length(muB), ncol = 1)
  mu = matrix(data=mu, nrow = length(mu), ncol = 1)
  
  Sigma0.inv = solve(Sigma0)
  d1 = t(muB - muG) %*% Sigma0.inv
  dim(d1)
  d2 = (muB - muG)
  dim(d2)
  d1%*% d2
  a = t(t(muB - muG) %*% Sigma0.inv * ( (d1 %*% d2)[1] )^(-1/2))
  D = sqrt((d1 %*% d2)[1])
  IC = TRUE
  RL = 0
  C = 0
  while (IC) {
    RL = RL+1
    x = matrix(mvrnorm(1,mu,Sigma0), ncol = 1, nrow = length(mu))
    if (runif(1) < noise.p) x = noise.size
    TS = (t(a)%*%(x-muG))[1]
    if (!is.null(w)){ #if w is not null then proceed
      if ( TS > w ) TS = w
    }
    C = max(0,C+TS-D/2)
    if(C > h) IC = FALSE
    }
  return(RL)
}

getARL.Healy <- function(h, w, mu, muG, muB, Sigma0, repl=5000, noise.p = 0, noise.size) {
  RL=rep(NA,repl)
  for (i in 1:repl){
    RL[i]=getRL.Healey(h = h, w = w, mu = mu, muG = muG, muB = muB, Sigma0 = Sigma0, noise.p = noise.p, noise.size = noise.size)
    if (i%%100==0) cat("ARL =",mean(RL[1:i]),"in replication",i,"\n")
  }
  ARL = mean(RL)
  SDRL=sd(RL)
  return(list(ARL=ARL,SDRL=SDRL))
}

get.Ddiff <- function(mu){
  # mu en este caso es un escalar, es la media de cada dimensi?n
  TARGET.D = 1 # <--------------------------------------- EDITAR
  k = 19 # <--------------------------------------------- EDITAR
  muG = rep(0,k)
  muB = rep(mu,k)
  Sigma0 = diag(k)
  Sigma0.inv = solve(Sigma0)
  d1 = t(muB - muG) %*% Sigma0.inv
  dim(d1)
  d2 = (muB - muG)
  dim(d2)
  d1%*% d2
  a = t(t(muB - muG) %*% Sigma0.inv * ( (d1 %*% d2)[1] )^(-1/2))
  D = sqrt((d1 %*% d2)[1])
  Ddiff = abs(D - TARGET.D)
  return(Ddiff)
}


#------------------------------------------------------------------------
# PRUEBAS -  PRELIMINARES
#------------------------------------------------------------------------

## PASO 1: Edita TARGET.D y k en funci?n get.Ddiff

## PASO 2: Edita lo siguiente:

noise.p = 0.005 #probability to make noise size noise.size. p = 0 for no noise
noise.size = 6*0.3162278 # 6*0.3162278 s a noise of 6 sd from center if Sigma0 = diag(k) and muG = rep(0,k)
ARL0 = 370 #ARL0 objetivo
w=3 # valor a winsorizar, w = NULL si no se requiere
D = 1 #D deseado
k = 19 # n?mero de variables
replicas = 5000 #n?mero de r?plicas a usar en simulaci?n
Sigma0 = diag(k) #sigma a simular, cualquier sigma factible si se estandariza.
mu = rep(0,k)# El valor de mu para simular

# Pre?mbulo autom?tico: No es necesario editar nada
library(CUSUMdesign)
muG = rep(0,k)
rss = optim(par = 0.1, fn = get.Ddiff, gr = NULL,
      method = c("Nelder-Mead", "BFGS", "CG", "L-BFGS-B", "SANN",
                 "Brent")[6],
      lower = 0, upper = 5, hessian = FALSE)
muB = rep(rss$par,k)
h = getH(distr=1, ARL=ARL0, ICmean=0, ICsd=1, 
         OOCmean=D,
         ref=NULL, winsrl=NULL, winsru=w,
         type="zero start")
h = h$DI

#------------------------------------------------------------------------
# PRUEBAS -  EJECUCI?N
#------------------------------------------------------------------------
# No es necesario editar nada aqu?

getARL.Healy(h = h, w = w, mu = mu, muG = muG, muB = muB, Sigma0 = Sigma0, repl=replicas, noise.p = noise.p, noise.size =  noise.size)
