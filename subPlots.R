library(ggplot2)
library(gridExtra)
## load data
load("Desktop/streaming_data/streaming data project/Wang-Wei-2019/subDRdf.RData")
load("Desktop/streaming_data/streaming data project/Wang-Wei-2019/subseDRdf.RData")
load("Desktop/streaming_data/streaming data project/Wang-Wei-2019/subDRNPdf.RData")
load("Desktop/streaming_data/streaming data project/Wang-Wei-2019/subseDRNPdf.RData")

load("Desktop/streaming_data/streaming data project/Wang-Wei-2019/subTMLEdf.RData")
load("Desktop/streaming_data/streaming data project/Wang-Wei-2019/subseTMLEdf.RData")
load("Desktop/streaming_data/streaming data project/Wang-Wei-2019/subTMLENPdf.RData")
load("Desktop/streaming_data/streaming data project/Wang-Wei-2019/subseTMLENPdf.RData")


## Bias Plot
p1 <- ggplot()+geom_point(data=subDRdf, shape = 4,aes(x = Size, y = Bias,color = Estimator)) +
  geom_line(data=subDRdf, aes(x = Size, y = Bias,color = Estimator))+
  geom_line(data=subDRdf,aes(x=Size,y = Bias+1.96*SE)
            , color = "red", linetype = "dotted") +
  geom_line(data=subDRdf,aes(x=Size,y = Bias-1.96*SE)
            , color = "red", linetype = "dotted")+
  geom_line(data=subTMLEdf,aes(x = Size, y = Bias,color =Estimator))+
  geom_point(data=subTMLEdf,aes(x = Size, y = Bias,color =Estimator))+
  geom_line(data=subTMLEdf,aes(x=Size,y = Bias+1.96*SE
                              , color = Estimator), linetype = "dotted") +
  geom_line(data=subTMLEdf,aes(x=Size,y = Bias-1.96*SE
                              , color = Estimator), linetype = "dotted")+
  theme_bw()+ylim(-0.1,0)+theme(axis.line = element_line(colour = "black"),
                                      panel.grid.major = element_blank(),
                                      panel.grid.minor = element_blank(),
                                      panel.border = element_blank(),
                                      panel.background = element_blank()) +
  ggtitle("Subgroup Bias and Confidence Intervals")




p3 <- ggplot()+geom_point(data=subseDRdf, shape = 4,
                          aes(x = Size, y = SE,color = Estimator)) +
  geom_line(data=subseDRdf, aes(x = Size, y = SE,color = Estimator))+
  geom_line(data=subseDRdf,aes(x=Size,y = SE+1.96*seSE)
            , color = "red", linetype = "dotted") +
  geom_line(data=subseDRdf,aes(x=Size,y = SE-1.96*seSE)
            , color = "red", linetype = "dotted")+
  geom_point(data=subseTMLEdf,aes(x = Size, y = SE,color =Estimator))+ 
  geom_line(data=subseTMLEdf, aes(x = Size, y = SE,color = Estimator))+
  geom_line(data=subseTMLEdf,aes(x=Size,y = SE+1.96*seSE
                              , color = Estimator), linetype = "dotted") +
  geom_line(data=subseTMLEdf,aes(x=Size,y = SE-1.96*seSE
                              , color = Estimator), linetype = "dotted")+
  theme_bw()+ylim(0,0.05)+xlim(0,5000)+theme(axis.line = element_line(colour = "black"),
                                             panel.grid.major = element_blank(),
                                             panel.grid.minor = element_blank(),
                                             panel.border = element_blank(),
                                             panel.background = element_blank()) +
  ggtitle("Subgroup Standard Error and Confidence Intervals")


grid.arrange(p1,p3,nrow = 1)

