library(boot)
library(ggplot2)
set.seed(1928361)
n <- 100000

## Generate Data
# binary outcome

W <- matrix(rnorm(n*3), ncol=3)
colnames(W) <- paste("W",1:3, sep="")
A <- rbinom(n,1, plogis(0.6*W[,1] +0.4*W[,2] + 0.5*W[,3]))
Y <- rbinom(n,1, plogis(A + 0.2*W[,1] + 0.1*W[,2] + 0.2*W[,3] ))
Y1 <- rbinom(n,1, plogis(1 + 0.2*W[,1] + 0.1*W[,2] + 0.2*W[,3] ))
Y0 <- rbinom(n,1, plogis(0 + 0.2*W[,1] + 0.1*W[,2] + 0.2*W[,3] ))
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


TMLEdf <- matrix(NA,nrow = 4,ncol = 3)
seTMLEdf <- matrix(NA,nrow = 4,ncol = 2)

n <- 10000
est <- NULL
for (i in 1:1000){
W <- matrix(rnorm(n*3), ncol=3)
colnames(W) <- paste("W",1:3, sep="")
A <- rbinom(n,1, plogis(0.6*W[,1] +0.4*W[,2] + 0.5*W[,3]))
Y <- rbinom(n,1, plogis(A + 0.2*W[,1] + 0.1*W[,2] + 0.2*W[,3] ))
dat <- data.frame(W,A,Y)

    txt<- control <- dat
    txt$A <-1
    control$A <-0
    
    # TMLE
    Qform <- glm(Y~A+W1+W2+W3, data=dat, family="binomial")
    # Q init est.
    Q1W<- predict(Qform,newdata = txt,type="response")
    Q0W <- predict(Qform,newdata = control,type="response")
    QAW<- predict(Qform,type="response")
    # estimate propensity score 
    gform <- glm(A~W1+W2+W3, data=dat, family="binomial")
    g1W <- predict(gform,type="response")
    g0W <- 1- g1W
    # direction to perturb
    D <- dat$A/g1W - (1-dat$A)/g0W
    D1 <- dat$A/g1W
    D0 <- (1-dat$A)/g0W
    # targeting step 
    QAWstar <- targeting(10,QAW,D)
    Q1Wstar <- targeting(10,Q1W,D1)
    Q0Wstar <- targeting(10,Q0W,D0)
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
RMSE <- sqrt(biasTMLE^2 + seTMLE^2)

TMLEdf[4,] <- c(biasTMLE,seTMLE,RMSE)

## SE
sd_hat <- sd(est)
se_sd_hat <- sqrt(mean((est - mean(est))^4))/sqrt(1000)
seTMLEdf[4,] <- c(sd_hat,se_sd_hat)

# organize TMLEdf
TMLEdf <- data.frame(TMLEdf)
TMLEdf$Size <- c(500,1000,5000,10000)
TMLEdf$Estimator <- rep("TMLE",4)
names(TMLEdf) <- c("Bias","SE","RMSE","Size","Estimator")
save(TMLEdf,file = "Desktop/streaming_data/streaming data project/Wang-Wei-2019/TMLEdf.RData")

# organize seTMLEdf
seTMLEdf <- data.frame(seTMLEdf)
seTMLEdf$Size <- c(500,1000,5000,10000)
seTMLEdf$Estimator <- rep("TMLE",4)
names(seTMLEdf) <- c("SE","seSE","Size","Estimator")
save(seTMLEdf,file = "Desktop/streaming_data/streaming data project/Wang-Wei-2019/seTMLEdf.RData")

