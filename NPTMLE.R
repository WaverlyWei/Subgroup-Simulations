
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
truePsi <- mean(Y1)-mean(Y0)

## ============= ##
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


# select best span
loess_cv <- function (x, y, span.vals = seq(0.25, 0.75, by = 0.15), folds = 3){
  # Do model selection using mean absolute error, which is more robust than squared error.
  mean.abs.error <- numeric(length(span.vals))
  
  # Quantify error for each span, using CV
  loess.model <- function(x, y, span){
    loess(y ~ x, span = span, control=loess.control(surface="direct"))
  }
  
  loess.predict <- function(fit, newdata) {
    predict(fit, newdata = newdata)
  }
  
  span.index <- 0
  for (each.span in span.vals) {
    span.index <- span.index + 1
    y.hat.cv <- crossval(x, y, theta.fit = loess.model, 
                         theta.predict = loess.predict, 
                         span = each.span, 
                         ngroup = folds)$cv.fit
    non.empty.indices <- !is.na(y.hat.cv)
    mean.abs.error[span.index] <- mean(abs(y[non.empty.indices] - 
                                             y.hat.cv[non.empty.indices]))
  }
  
  # find the span which minimizes error
  best.span <- span.vals[which.min(mean.abs.error)]
  
  # fit and return the best model
  best.model <- loess(y ~ x, span = best.span, 
                      control=loess.control(surface="direct"))
  return(best.model)
}


TMLENPdf <- matrix(NA,nrow = 4,ncol = 3)
seTMLENPdf <- matrix(NA,nrow = 4,ncol = 2)

n <- 1000
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
  D <- dat$A/g1W - (1-dat$A)/g0W
  D1 <- dat$A/g1W
  D0 <- (1-dat$A)/g0W
  # targeting step 
  QAW[QAW>=1] <- 0.9
  Q1W[Q1W>=1] <- 0.9
  Q0W[Q0W>=1] <- 0.9
  QAWstar <- targeting(10,abs(QAW),D)
  Q1Wstar <- targeting(10,abs(Q1W),D1)
  Q0Wstar <- targeting(10,abs(Q0W),D0)
  mu1 <- mean(Q1Wstar)
  mu0 <- mean(Q0Wstar)
  tmlePsi <- mu1 - mu0
  est[i] <- tmlePsi
  #IC <- (A/g1W - (1-A)/g0W)*(Y-QAWstar) +
 # (Q1Wstar - Q1Wstar) - (mu1-mu0)
  #varTMLE <- var(IC)/n
  
}
biasTMLE <- mean(est) - truePsi
seTMLE <- sd(est)/sqrt(1000)
RMSE <- sqrt(biasTMLE^2 + seTMLE^2)

TMLENPdf[2,] <- c(biasTMLE,seTMLE,RMSE)

TMLENPdf <- data.frame(TMLENPdf)

TMLENPdf$Size <- c(500,1000,5000,10000)
TMLENPdf$Estimator <- rep("TMLE",4)
names(TMLENPdf) <- c("Bias","SE","RMSE","Size","Estimator")

save(TMLENPdf,file = "Desktop/streaming_data/streaming data project/Wang-Wei-2019/TMLENPdf.RData")


# SE
sd_hat <- sd(est)
se_sd_hat <- sqrt(mean((est - mean(est))^4))/sqrt(1000)
seTMLENPdf[2,] <- c(sd_hat,se_sd_hat)


# organize
seTMLENPdf <- data.frame(seTMLENPdf)
seTMLENPdf$Size <- c(500,1000,5000,10000)
seTMLENPdf$Estimator <- rep("TMLE",4)
names(seTMLENPdf) <- c("SE","seSE","Size","Estimator")
save(seTMLENPdf,file = "Desktop/streaming_data/streaming data project/Wang-Wei-2019/seTMLENPdf.RData")
