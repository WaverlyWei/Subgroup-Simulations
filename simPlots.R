library(ggplot2)
library(gridExtra)
## load data
load("Desktop/streaming_data/streaming data project/Wang-Wei-2019/DRdf.RData")
load("Desktop/streaming_data/streaming data project/Wang-Wei-2019/seDRdf.RData")
load("Desktop/streaming_data/streaming data project/Wang-Wei-2019/DRNPdf.RData")
load("Desktop/streaming_data/streaming data project/Wang-Wei-2019/seDRNPdf.RData")

load("Desktop/streaming_data/streaming data project/Wang-Wei-2019/TMLEdf.RData")
load("Desktop/streaming_data/streaming data project/Wang-Wei-2019/seTMLEdf.RData")
load("Desktop/streaming_data/streaming data project/Wang-Wei-2019/TMLENPdf.RData")
load("Desktop/streaming_data/streaming data project/Wang-Wei-2019/seTMLENPdf.RData")

load("Desktop/streaming_data/streaming data project/Wang-Wei-2019/BRdf.RData")
load("Desktop/streaming_data/streaming data project/Wang-Wei-2019/BRNPdf.RData")

## Bias Ratio
BRdf <- TMLEdf
BRdf$Bias <- TMLEdf$Bias/DRdf$Bias
BRdf$SE <- TMLEdf$SE/DRdf$SE
names(BRdf)[1] <- "BiasRatio"
save(BRdf,file = "Desktop/streaming_data/streaming data project/Wang-Wei-2019/BRdf.RData")

# NP Bias Ratio
BRNPdf <- TMLENPdf
BRNPdf$Bias <- TMLENPdf$Bias/DRNPdf$Bias
BRNPdf$SE <- TMLENPdf$SE/DRNPdf$SE
names(BRNPdf)[1] <- "BiasRatio"
save(BRNPdf,file = "Desktop/streaming_data/streaming data project/Wang-Wei-2019/BRNPdf.RData")


# Bias Ratio Plot
r1 <- ggplot(data=BRdf, aes(x = Size, y = BiasRatio))+
  geom_point(shape = 4) +
  geom_line()+
  geom_line(data=BRdf,aes(x=Size,y = BiasRatio+1.96*SE) 
            , color = "red", linetype = "dotted") +
  geom_line(data=BRdf,aes(x=Size,y = BiasRatio-1.96*SE)
            , color = "red", linetype = "dotted")+
  theme_bw()+ylim(0,1)+theme(axis.line = element_line(colour = "black"),
                                      panel.grid.major = element_blank(),
                                      panel.grid.minor = element_blank(),
                                      panel.border = element_blank(),
                                      panel.background = element_blank()) +
  ggtitle("Bias Ratio and Confidence Intervals")

# NP Bias Ratio
r2 <- ggplot(data=BRNPdf, aes(x = Size, y = BiasRatio))+
  geom_point(shape = 4) +
  geom_line()+
  geom_line(data=BRNPdf,aes(x=Size,y = BiasRatio+1.96*SE) 
            , color = "red", linetype = "dotted") +
  geom_line(data=BRNPdf,aes(x=Size,y = BiasRatio-1.96*SE)
            , color = "red", linetype = "dotted")+
  theme_bw()+ylim(0,1)+theme(axis.line = element_line(colour = "black"),
                             panel.grid.major = element_blank(),
                             panel.grid.minor = element_blank(),
                             panel.border = element_blank(),
                             panel.background = element_blank()) +
  ggtitle("Bias Ratio and Confidence Intervals(NP)")

## Bias Plot
p1 <- ggplot(data=DRdf, aes(x = Size, y = BiasRatio,color = Estimator))+
  geom_point(shape = 4) +
  geom_line()+
  geom_line(data=DRdf,aes(x=Size,y = Bias+1.96*SE) 
            , color = "red", linetype = "dotted") +
  geom_line(data=DRdf,aes(x=Size,y = Bias-1.96*SE)
            , color = "red", linetype = "dotted")+
  geom_line(data=TMLEdf,aes(x = Size, y = Bias,color =Estimator))+
  geom_point(data=TMLEdf,aes(x = Size, y = Bias,color =Estimator))+
  geom_line(data=TMLEdf,aes(x=Size,y = Bias+1.96*SE
                            , color = Estimator), linetype = "dotted") +
  geom_line(data=TMLEdf,aes(x=Size,y = Bias-1.96*SE
                            , color = Estimator), linetype = "dotted")+
  theme_bw()+ylim(-0.005,0.015)+theme(axis.line = element_line(colour = "black"),
                                    panel.grid.major = element_blank(),
                                    panel.grid.minor = element_blank(),
                                    panel.border = element_blank(),
                                    panel.background = element_blank()) +
  ggtitle("Bias and Confidence Intervals")

# NP Bias Plot
p2 <- ggplot()+geom_point(data=DRNPdf, shape = 4,aes(x = Size, y = Bias,color = Estimator)) +
  geom_line(data=DRNPdf, aes(x = Size, y = Bias,color = Estimator))+
  geom_line(data=DRNPdf,aes(x=Size,y = Bias+1.96*SE)
            , color = "red", linetype = "dotted") +
  geom_line(data=DRNPdf,aes(x=Size,y = Bias-1.96*SE)
            , color = "red", linetype = "dotted")+
  geom_line(data=TMLENPdf,aes(x = Size, y = Bias,color =Estimator))+
    geom_point(data=TMLENPdf,aes(x = Size, y = Bias,color =Estimator))+
  geom_line(data=TMLENPdf,aes(x=Size,y = Bias+1.96*SE
                            , color = Estimator), linetype = "dotted") +
  geom_line(data=TMLENPdf,aes(x=Size,y = Bias-1.96*SE
                            , color = Estimator), linetype = "dotted")+
  theme_bw()+ylim(-0.005,0.015)+theme(axis.line = element_line(colour = "black"),
                                    panel.grid.major = element_blank(),
                                    panel.grid.minor = element_blank(),
                                    panel.border = element_blank(),
                                    panel.background = element_blank()) +
  ggtitle("Bias and Confidence Intervals(NP)")


# SE plot
p3 <- ggplot()+geom_point(data=seDRdf, shape = 4,
                          aes(x = Size, y = SE,color = Estimator)) +
  geom_line(data=seDRdf, aes(x = Size, y = SE,color = Estimator))+
  geom_line(data=seDRdf,aes(x=Size,y = SE+1.96*seSE)
            , color = "red", linetype = "dotted") +
  geom_line(data=seDRdf,aes(x=Size,y = SE-1.96*seSE)
            , color = "red", linetype = "dotted")+
  geom_point(data=seTMLEdf,aes(x = Size, y = SE,color =Estimator))+ 
  geom_line(data=seDRdf, aes(x = Size, y = SE,color = Estimator))+
  geom_line(data=seTMLEdf,aes(x=Size,y = SE+1.96*seSE
                              , color = Estimator), linetype = "dotted") +
  geom_line(data=seTMLEdf,aes(x=Size,y = SE-1.96*seSE
                              , color = Estimator), linetype = "dotted")+
  theme_bw()+ylim(0,0.08)+xlim(0,5000)+theme(axis.line = element_line(colour = "black"),
                               panel.grid.major = element_blank(),
                               panel.grid.minor = element_blank(),
                               panel.border = element_blank(),
                               panel.background = element_blank()) +
  ggtitle("Standard Error and Confidence Intervals")


# NP plot 
p4 <- ggplot()+geom_point(data=seDRNPdf, shape = 4,
                          aes(x = Size, y = SE,color = Estimator)) +
  geom_line(data=seDRNPdf, aes(x = Size, y = SE,color = Estimator)) +
  geom_line(data=seDRNPdf,aes(x=Size,y = SE+1.96*seSE)
            , color = "red", linetype = "dotted") +
  geom_line(data=seDRNPdf,aes(x=Size,y = SE-1.96*seSE)
            , color = "red", linetype = "dotted")+
  geom_point(data=seTMLENPdf,aes(x = Size, y = SE,color =Estimator))+ 
  geom_line(data=seTMLENPdf,aes(x = Size, y = SE,color =Estimator))+ 
  geom_line(data=seTMLENPdf,aes(x=Size,y = SE+1.96*seSE
                              , color = Estimator), linetype = "dotted") +
  geom_line(data=seTMLENPdf,aes(x=Size,y = SE-1.96*seSE
                              , color = Estimator), linetype = "dotted")+
  theme_bw()+ylim(0,0.08)+xlim(0,5000)+theme(axis.line = element_line(colour = "black"),
                               panel.grid.major = element_blank(),
                               panel.grid.minor = element_blank(),
                               panel.border = element_blank(),
                               panel.background = element_blank()) +
  ggtitle("Standard Error and Confidence Intervals(NP)")


grid.arrange(r1, r2, p3,p4,nrow = 2)

