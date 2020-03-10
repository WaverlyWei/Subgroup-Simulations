library(ggplot2)
library(fANCOVA)
library(doParallel)

set.seed(1928361)
n <- 100000

## Generate Data
# binary outcome
W <- rnorm(n)
A <- rbinom(n,1, plogis(0.6*W^2))
Y <- rbinom(n,1, plogis(A + 0.2*W^2 ))
Y1 <- rbinom(n,1, plogis(1 + 0.2*W^2 ))
Y0 <- rbinom(n,1, plogis(0 + 0.2*W^2))
dat <- data.frame(W,A,Y)
# subgroup W<0.7
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

subTMLENPdf <- matrix(NA,nrow = 4,ncol = 2)
subseTMLENPdf <- matrix(NA,nrow = 4,ncol = 2)

n <- 5000
est <- NULL
for (i in 1:1000){
  W <- rnorm(n)
  A <- rbinom(n,1, plogis(0.6*W^2))
  Y <- rbinom(n,1, plogis(A + 0.2*W^2 ))
  dat <- data.frame(W,A,Y)
  txt<- control <- dat
  txt$A <-1
  control$A <-0
  
  # TMLE
  #Qform <- loess_cv(cbind(W,A), Y)
  Qform <- loess.as(cbind(W,A),Y ,degree = 2, 
                    criterion = "gcv", 
                    user.span = NULL, plot = F)
  # Q init est.
  Q1W<- predict(Qform,cbind(W,txt$A),type="response")
  Q0W <- predict(Qform,cbind(W,control$A),type="response")
  QAW<- predict(Qform,type="response")
  # estimate propensity score 
  #gform <- loess_cv(W, A)
  gform <- loess.as(W, A, degree = 1, 
                    criterion = "gcv", 
                    user.span = NULL, plot = F)
  g1W <- predict(gform,type="response")
  g0W <- 1- g1W
  # direction to perturb
  # subgroup cdf 
  P <- ecdf(W)    
  pW <- P(W)       
  
  # direction to perturb
  # D <- (W<0.6)/pW * (A/g1W - (1-A)/g0W)*(Y-QAW)
  # D1 <- (W<0.6)/pW * (A/g1W)*(Y-Q1W)
  # D0 <- (W<0.6)/pW * (1-A)/g0W*(Y-Q0W)
  # Dnorm <- sqrt(sum(D^2))
  # D1norm <- sqrt(sum(D1^2))
  # D0norm <- sqrt(sum(D0^2))
  
  Dir <-  (W<0.3)/pW * (A/g1W - (1-A)/g0W)
  Dir1 <-  (W<0.3)/pW * (A/g1W)
  Dir0 <-  (W<0.3)/pW * (1-A)/g0W
  
  # targeting step 
  QAW[QAW>=1] <- 0.9
  Q1W[Q1W>=1] <- 0.9
  Q0W[Q0W>=1] <- 0.9
  QAWstar <- targeting(10,abs(QAW),Dir)
  Q1Wstar <- targeting(10,abs(Q1W),Dir1)
  Q0Wstar <- targeting(10,abs(Q0W),Dir0)
  mu1 <- mean(Q1Wstar)
  mu0 <- mean(Q0Wstar)
  tmlePsi <- mu1 - mu0
  est[i] <- tmlePsi
  #IC <- (A/g1W - (1-A)/g0W)*(Y-QAWstar) +
  # (Q1Wstar - Q1Wstar) - (mu1-mu0)
  #varTMLE <- var(IC)/n
  
}

# Bias
biasTMLE <- mean(est) - truePsi
seTMLE <- sd(est)/sqrt(1000)
#RMSE <- sqrt(biasTMLE^2 + seTMLE^2)

subTMLENPdf[4,] <- c(biasTMLE,seTMLE)

## SE
sd_hat <- sd(est)
se_sd_hat <- sqrt(mean((est - mean(est))^4))/sqrt(1000)
subseTMLENPdf[4,] <- c(sd_hat,se_sd_hat)

save(subTMLENPdf,file = "Desktop/streaming_data/streaming data project/Wang-Wei-2019/subTMLENPdf.RData")
save(subseTMLENPdf,file = "Desktop/streaming_data/streaming data project/Wang-Wei-2019/subseTMLENPdf.RData")


##  ========== sub group DR ======== #
subDRNPdf <- matrix(NA,nrow = 4,ncol = 2)
subseDRNPdf <- matrix(NA,nrow = 4,ncol = 2)

n <- 1000
res <- NULL
for (i in 1:1000){
  W <- rnorm(n)
  A <- rbinom(n,1, plogis(0.6*W^2))
  Y <- rbinom(n,1, plogis(A + 0.2*W^2 ))
  dat <- data.frame(W,A,Y)
  # get subgroup 
  #dat <- dat[W<0.6,]
  txt<- control <- dat
  txt$A <-1
  control$A <-0
  
  psModel <- fANCOVA::loess.as(W, A, degree =1,criterion = "gcv",
                               user.span = NULL, plot = F)
  #psModel <- loess_cv(W, A)
  ps <- predict(psModel, newdata=txt,type="response")
  # estimate counterfactual mean 
  #muModel <- loess_cv(cbind(W,A), Y)
  muModel <-  fANCOVA::loess.as(cbind(W,A),Y ,degree = 2, criterion = "gcv",
                                user.span = NULL, plot = F)
  mu <- predict(muModel,type = "response")
  mu1 <- predict(muModel,cbind(W,txt$A), type="response")
  mu0 <- predict(muModel,cbind(W,control$A), type="response")
  
  #drPsi <- mean(dat$A*(dat$Y-mu1)/ps + mean(mu1)) -
  #mean((1-dat$A)*(dat$Y-mu0)/(1-ps)+mean(mu0))
  drPsi <- mean((W<0.3)*((A/ps - (1-A)/(1-ps))*(Y-mu) +(mu1-mu0)))
  res[i] <- drPsi
}

biasDR <- mean(res) - truePsi
biasDR

seDR <- sd(res)/sqrt(1000)
seDR

subDRNPdf[2,] <- c(biasDR,seDR)

# SE
sd_hat <- sd(res)
se_sd_hat <- sqrt(mean((res - mean(res))^4))/sqrt(1000)
subseDRNPdf[2,] <- c(sd_hat,se_sd_hat)

save(subDRNPdf,file = "Desktop/streaming_data/streaming data project/Wang-Wei-2019/subDRNPdf.RData")
save(subseDRNPdf,file = "Desktop/streaming_data/streaming data project/Wang-Wei-2019/subseDRNPdf.RData")

