
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

DRdf <- matrix(NA,nrow = 4,ncol = 3)
seDRdf <- matrix(NA,nrow = 4,ncol = 2)

n <- 10000
res <- NULL

for (i in 1: 1000){
  W <- matrix(rnorm(n*3), ncol=3)
  colnames(W) <- paste("W",1:3, sep="")
  A <- rbinom(n,1, plogis(0.6*W[,1] +0.4*W[,2] + 0.5*W[,3]))
  Y <- rbinom(n,1, plogis(A + 0.2*W[,1] + 0.1*W[,2] + 0.2*W[,3] ))
  dat <- data.frame(W,A,Y)
  txt<- control <- dat
  txt$A <-1
  control$A <-0
  psModel <- glm(A~W1+W2+W3, data=dat, family="binomial")
  ps <- predict(psModel, newdata=txt,type="response")
# estimate counterfactual mean 
  muModel <- glm(Y~A+W1+W2+W3, data=dat, family="binomial")
  mu <- predict(muModel,type = "response")
  mu1 <- predict(muModel, newdata=txt, type="response")
  mu0 <- predict(muModel, newdata=control, type="response")
  
  drPsi <- mean((A/ps - (1-A)/(1-ps))*(Y-mu) +(mu1-mu0))
  res[i] <- drPsi
}

# Bias 
biasDR <- sqrt(n)*(mean(res) - truePsi)
biasDR

seDR <- (sd(res)/sqrt(1000))
seDR

RMSE <- sqrt(biasDR^2+(seDR)^2) 
RMSE

DRdf[4,] <- c(biasDR,seDR,RMSE)


# SE
sd_hat <- n*sd(res)
se_sd_hat <- sqrt(mean((res - mean(res))^4))/sqrt(1000)
seDRdf[4,] <- c(sd_hat,se_sd_hat)


# organize DRdf
DRdf <- data.frame(DRdf)
DRdf$Size <- c(500,1000,5000,10000)
DRdf$Estimator <- rep("DR",4)
names(DRdf) <- c("Bias","SE","RMSE","Size","Estimator")
save(DRdf,file = "Desktop/streaming_data/streaming data project/Wang-Wei-2019/DRdf.RData")

# organize seDRdf
seDRdf <- data.frame(seDRdf)
seDRdf$Size <- c(500,1000,5000,10000)
seDRdf$Estimator <- rep("DR",4)
names(seDRdf) <- c("SE","seSE","Size","Estimator")
save(seDRdf,file = "Desktop/streaming_data/streaming data project/Wang-Wei-2019/seDRdf.RData")

# DR plot
ggplot()+geom_line(data=DRdf, aes(x = Size, y = Bias,color = Estimator)) +
  geom_line(data=DRdf,aes(x=Size,y = Bias+1.96*SE)
            , color = "red", linetype = "dotted") +
  geom_line(data=DRdf,aes(x=Size,y = Bias-1.96*SE)
            , color = "red", linetype = "dotted")+
   geom_line(data=TMLEdf,aes(x = Size, y = Bias,color =Estimator))+ 
   geom_line(data=TMLEdf,aes(x=Size,y = Bias+1.96*SE
    , color = Estimator), linetype = "dotted") +
   geom_line(data=TMLEdf,aes(x=Size,y = Bias-1.96*SE
            , color = Estimator), linetype = "dotted")+
  theme_bw()+ylim(-0.01,0.01)+
  ggtitle("Bias and Confidence Intervals")
  

  
  

# TMLE plot
load("Desktop/streaming_data/streaming data project/Wang-Wei-2019/TMLEdf.RData")
