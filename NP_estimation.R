
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


DRNPdf <- matrix(NA,nrow = 4,ncol = 3)
seDRNPdf <- matrix(NA,nrow = 4,ncol = 2)

n <- 1000
#res <- NULL

cl <- makeCluster(4)
registerDoParallel(cl)

res <- foreach(i=1:1000,.combine = c ) %dopar% {
  n <- 1000
  W <- rnorm(n)
  A <- rbinom(n,1, plogis(0.6*W^2))
  Y <- rbinom(n,1, plogis(A + 0.2*W^2 ))
  dat <- data.frame(W,A,Y)
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
  drPsi <- mean((A/ps - (1-A)/(1-ps))*(Y-mu) +(mu1-mu0))
  drPsi
}

biasDR <- mean(res) - truePsi
biasDR

seDR <- sd(res)/sqrt(1000)
seDR

RMSE <- sqrt(biasDR^2+seDR^2) 
RMSE

DRNPdf[3,] <- c(biasDR,seDR,RMSE)

DRNPdf <- data.frame(DRNPdf)
DRNPdf$Size <- c(500,1000,5000,10000)
DRNPdf$Estimator <- rep("DR",4)
names(DRNPdf) <- c("Bias","SE","RMSE","Size","Estimator")


save(DRNPdf,file = "Desktop/streaming_data/streaming data project/Wang-Wei-2019/DRNPdf.RData")


# SE
sd_hat <- sd(res)
se_sd_hat <- sqrt(mean((res - mean(res))^4))/sqrt(1000)
seDRNPdf[2,] <- c(sd_hat,se_sd_hat)

# organize seDRdf
seDRNPdf <- data.frame(seDRNPdf)
seDRNPdf$Size <- c(500,1000,5000,10000)
seDRNPdf$Estimator <- rep("DR",4)
names(seDRNPdf) <- c("SE","seSE","Size","Estimator")

save(seDRNPdf,file = "Desktop/streaming_data/streaming data project/Wang-Wei-2019/seDRNPdf.RData")




# DR plot
ggplot()+geom_line(data=DRNPdf, aes(x = Size, y = Bias,color = Estimator)) +
  geom_line(data=DRNPdf,aes(x=Size,y = Bias+1.96*SE)
            , color = "red", linetype = "dotted") +
  geom_line(data=DRNPdf,aes(x=Size,y = Bias-1.96*SE)
            , color = "red", linetype = "dotted")+
  geom_line(data=TMLENPdf,aes(x = Size, y = Bias,color =Estimator))+ 
  geom_line(data=TMLENPdf,aes(x=Size,y = Bias+1.96*SE
                            , color = Estimator), linetype = "dotted") +
  geom_line(data=TMLENPdf,aes(x=Size,y = Bias-1.96*SE
                            , color = Estimator), linetype = "dotted")+
  theme_bw()+
  ggtitle("Bias and Confidence Intervals")
