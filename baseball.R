#libraries----------------------------------------------
library(scales)
library(DescTools)
library(tidyr)
library(tidyverse) 
library(dplyr)             # data manipulation
library(cluster)           # clustering algorithms
require(ggplot2)
library(ggiraph)           # radar graphs
require(plyr)
require(reshape2)
library(ggiraphExtra)      # radar graphs
library(seewave)           #KLDIVERGENCE
library(mclust)            # EM with GMM
library(car)
library(MVN)
library(caret)
library(gridExtra)
#functions------------------------------------------------
hac <- function(df,cN,start = 2){
  set.seed(123)
  Clusters <- hclust(dist(df[start:cN]))
  plot(Clusters, labels = F)
  for (i in 2:9){
    df[,paste0("C",i)] <- cutree(Clusters, i)
    print(i)
    print(mean(silhouette(df[,paste0("C",i)], dist(df[start:cN]))[,3]))
  }
  return(df)
}
em <- function(df, cN, start = 1){
  mod <- Mclust(df[,start:cN])
  df$emClass <- mod$classification
  print("BIC is: ")
  print(mod$bic)
  return(df)
}
#dataLoad-------------------------------------------------
d <- read.csv(file.choose())
d2 <- data.frame(lapply(d[,3:dim(d)[2]],scale))
d2$name <- d$player_name
d2 <- d2[,-c(8,13)]
#dataCheck-----------------------------------------------
plot(d2[,2:dim(d2)[2]-1])
#HAC------------------------------------------------------
hDat <- hac(d2, cN = dim(d2)[2]-1, start = 1)
#EM-------------------------------------------------------
#mDat <- em(d2,dim(d2)[2]-1)
#Plots----------------------------------------------------
for (i in 2:9){
  dt <- hDat[,c(1:12,12+i)]
  names(dt) <- c(names(dt)[1:12], "ClusterAssign")
  dt$ClusterAssign <- factor(dt$ClusterAssign)
ggRadar(data = dt, aes(group = ClusterAssign), rescale = F, legend.position = "right")+
  theme(legend.title=element_blank(),legend.text=element_text(size=20)) + theme_minimal() + ggtitle(paste(i,"Cluster Characteristics"))
ggsave(paste0("/Users/chelseaparlett/Desktop/radar/RadarnoWAR",i,".png"),, units = "in", height = 10, width = 10)
}
#pca PLOTS------------------------------------------------
#saveData-------------------------------------------------
write.csv(hDat, "/Users/chelseaparlett/Desktop/hierarchicalBaseBallnoWAR.csv", row.names = F)
