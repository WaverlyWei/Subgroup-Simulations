library(ggplot2)
library(fANCOVA)
library(doParallel)

set.seed(1928361)
n <- 100000

## Generate Data
# binary outcome
W <- rnorm(n)
A <- rbinom(n,1, plogis(0.6*W))
Y <- rbinom(n,1, plogis(A + 0.2*W ))
Y1 <- rbinom(n,1, plogis(1 + 0.2*W ))
Y0 <- rbinom(n,1, plogis(0 + 0.2*W))
dat <- data.frame(W,A,Y)
# subgroup W<0.3
truePsi <- mean(Y1[W<0.3])-mean(Y0[W<0.3])

targeting <- function(maxit,Qinit,D){
  it <- 0
  while(it < maxit){
    it <- it + 1 
    # Qbar epsilon
    Qepsilon <- function(epsilon) {inv.logit(logit(Qinit) + epsilon*D)}
    
    logLikelihood <- function(epsilon) {
      -sum(Y*log(Qepsilon(epsilon)) + 
             (1-Y)*log(1-Qepsilon(epsilon)))}
    
    gradLogLik <- function(epsilon) {-sum(D*(Y-Qepsilon(epsilon)))}
    
    eps <- optim(par = 0, fn = logLikelihood,
                 gr = gradLogLik,
                 method = "BFGS")$par
    Qinit <- Qepsilon(eps)
  }
  return(Qinit)
}



subTMLEdf <- matrix(NA,nrow = 4,ncol = 2)
subseTMLEdf <- matrix(NA,nrow = 4,ncol = 2)

## 
# 500,1000,2500,5000
n <- 500
est <- NULL
for (i in 1:1000){
W <- rnorm(n)
A <- rbinom(n,1, plogis(0.6*W))
Y <- rbinom(n,1, plogis(A + 0.2*W ))
dat <- data.frame(W,A,Y)

txt<- control <- dat
txt$A <-1
control$A <-0

# TMLE
Qform <- glm(Y~A+W, data=dat, family="binomial")
# Q init est.
Q1W<- predict(Qform,newdata = txt,type="response")
Q0W <- predict(Qform,newdata = control,type="response")
QAW<- predict(Qform,type="response")
# estimate propensity score 
gform <- glm(A~W, data=dat, family="binomial")
g1W <- predict(gform,type="response")
g0W <- 1- g1W

# subgroup cdf 
P <- ecdf(W)    
pW <- P(W)       

# direction to perturb
# D <- (W<0.3)/pW * (A/g1W - (1-A)/g0W)*(Y-QAW)
# D1 <- (W<0.3)/pW * (A/g1W)*(Y-Q1W)
# D0 <- (W<0.3)/pW * (1-A)/g0W*(Y-Q0W)
# Dnorm <- sqrt(sum(D^2))
# D1norm <- sqrt(sum(D1^2))
# D0norm <- sqrt(sum(D0^2))
#Dir <- D/Dnorm * (W<0.6)/pW * (A/g1W - (1-A)/g0W)
#Dir1 <- D1/D1norm * (W<0.6)/pW * (A/g1W)
#Dir0 <- D0/D0norm * (W<0.6)/pW * (1-A)/g0W

Dir <-  (W<0.3)/pW * (A/g1W - (1-A)/g0W)
Dir1 <-  (W<0.3)/pW * (A/g1W)
Dir0 <-  (W<0.3)/pW * (1-A)/g0W

# targeting step 
QAWstar <- targeting(100,QAW,Dir)
Q1Wstar <- targeting(100,Q1W,Dir1)
Q0Wstar <- targeting(100,Q0W,Dir0)
mu1 <- mean(Q1Wstar)
mu0 <- mean(Q0Wstar)
tmlePsi <- mu1 - mu0
est[i] <- tmlePsi
}

# Bias
biasTMLE <- mean(est) - truePsi
# [1] -0.005711634
seTMLE <- sd(est)/sqrt(1000)
#RMSE <- sqrt(biasTMLE^2 + seTMLE^2)

subTMLEdf[1,] <- c(biasTMLE,seTMLE)

## SE
sd_hat <- sd(est)
se_sd_hat <- sqrt(mean((est - mean(est))^4))/sqrt(1000)
subseTMLEdf[1,] <- c(sd_hat,se_sd_hat)

save(subTMLEdf,file = "Desktop/streaming_data/streaming data project/Wang-Wei-2019/subTMLEdf.RData")
save(subseTMLEdf,file = "Desktop/streaming_data/streaming data project/Wang-Wei-2019/subseTMLEdf.RData")


## =============== sub group DR =========== #
subDRdf <- matrix(NA,nrow = 4,ncol = 2)
subseDRdf <- matrix(NA,nrow = 4,ncol = 2)

n <- 500
res <- NULL

for (i in 1: 1000){
  W <- rnorm(n)
  A <- rbinom(n,1, plogis(0.6*W))
  Y <- rbinom(n,1, plogis(A + 0.2*W ))
  dat <- data.frame(W,A,Y)
  #dat <- dat[W<0.3,]
  txt<- control <- dat
  txt$A <-1
  control$A <-0
  psModel <- glm(A~W, data=dat, family="binomial")
  ps <- predict(psModel, newdata=txt,type="response")
  # estimate counterfactual mean 
  muModel <- glm(Y~A+W, data=dat, family="binomial")
  mu <- predict(muModel,type = "response")
  mu1 <- predict(muModel, newdata=txt, type="response")
  mu0 <- predict(muModel, newdata=control, type="response")
  
  drPsi <- mean((W<0.3)*((A/ps - (1-A)/(1-ps))*(Y-mu) +(mu1-mu0)))
  res[i] <- drPsi
}

# Bias 
biasDR <- mean(res) - truePsi
biasDR

seDR <- sd(res)/sqrt(1000)
seDR


subDRdf[2,] <- c(biasDR,seDR)

# SE
sd_hat <- sd(res)
se_sd_hat <- sqrt(mean((res - mean(res))^4))/sqrt(1000)
subseDRdf[2,] <- c(sd_hat,se_sd_hat)

save(subDRdf,file = "Desktop/streaming_data/streaming data project/Wang-Wei-2019/subDRdf.RData")
save(subseDRdf,file = "Desktop/streaming_data/streaming data project/Wang-Wei-2019/subseDRdf.RData")

