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
library(class)
library(MASS)
#functions------------------------------------------------
hac <- function(df,cN,start = 1){
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
d2 <- data.frame(lapply(d[,3:14],scale))
d2$name <- d$name

#dataCheck-----------------------------------------------
plot(d2[,1:13])
#HAC------------------------------------------------------
hDat <- hac(d2, cN = dim(d2)[2]-1, start = 1)
#EM-------------------------------------------------------
#mDat <- em(d2,dim(d2)[2]-1)
#Plots----------------------------------------------------
#hDat <- read.csv(file.choose())[,2:22]
for (i in 2:9){
  dt <- hDat[,c(1:12,12+i)]
  names(dt) <- c(names(dt)[1:12], "ClusterAssign")
  dt$ClusterAssign <- factor(dt$ClusterAssign)
ggRadar(data = dt, aes(group = ClusterAssign), rescale = F, legend.position = "right")+
  theme(legend.title=element_blank(),legend.text=element_text(size=20)) + theme_minimal() + ggtitle(paste(i,"Cluster Characteristics"))
ggsave(paste0("/Users/chelseaparlett/Desktop/Desktop/Github/BaseBall/radar/RadarnoWAR",i,".png"), units = "in", height = 10, width = 10)
}
#pca PLOTS------------------------------------------------
#saveData-------------------------------------------------
write.csv(hDat, "/Users/chelseaparlett/Desktop/Desktop/Github/BaseBall/hierarchicalBaseBallnoWAR.csv", row.names = F)
#knn------------------------------------------------------
# oldPs <- read.csv(file.choose())
oldPs <- hDat
oldPsExtra <- read.csv(file.choose())
newPs <- read.csv(file.choose())
OP <- read.csv(file.choose())
OP$whiffs <- d$whiffs
ns <- names(oldPs[1:12])
for (i in ns){
  newPs[,i] <- (newPs[,i] - mean(OP[,i]))/sd(OP[,i])
}


oldPs$C2 <-factor(oldPs$C2)
oldPs$C3 <-factor(oldPs$C3)

Knn3 <- knn(oldPs[,ns],newPs[,ns], cl = oldPs$C3, k = 5, prob = T)
Knn2 <- knn(oldPs[,ns],newPs[,ns], cl = oldPs$C2, k = 5, prob = T)

newPs$C2 <- Knn2
newPs$C3 <- Knn3

newPs2 <- newPs[,c(ns,"C2")]
ggRadar(data = newPs2, aes(group = C2), rescale = F, legend.position = "right")+
  theme(legend.title=element_blank(),legend.text=element_text(size=20)) + theme_minimal() + ggtitle(paste(2,"Cluster Characteristics"))
ggsave(paste0("/Users/chelseaparlett/Desktop/Desktop/Github/BaseBall/radar/NEWPLAYERS",2,".png"), units = "in", height = 10, width = 10)

newPs3 <- newPs[,c(ns,"C3")]
ggRadar(data = newPs3, aes(group = C3), rescale = F, legend.position = "right")+
  theme(legend.title=element_blank(),legend.text=element_text(size=20)) + theme_minimal() + ggtitle(paste(3,"Cluster Characteristics"))
ggsave(paste0("/Users/chelseaparlett/Desktop/Desktop/Github/BaseBall/radar/NEWPLAYERS",3,".png"), units = "in", height = 10, width = 10)
#analyses--------------------------------------------------
oldPs$WPA <- oldPsExtra$WPA
oldPs$WAR <- oldPsExtra$WAR
oldPs$SIERA <- oldPsExtra$SIERA

write.csv(oldPs, "PlayersUSEwhiffperc.csv")

Y <- cbind(oldPs$WPA,oldPs$WAR,oldPs$SIERA)
man <- manova(Y ~ oldPs$C2)
summary(man)

m1 <- lm(WPA ~ C2,data = oldPs)
anova(m1)
m1 <- lm(SIERA ~ C2,data = oldPs)
anova(m1)
m1 <- lm(WAR ~ C2,data = oldPs)
anova(m1)


man <- manova(Y ~ oldPs$C3)
summary(man)

m1 <- lm(WPA ~ C3,data = oldPs)
anova(m1)
m1 <- lm(SIERA ~ C3,data = oldPs)
anova(m1)
m1 <- lm(WAR ~ C3,data = oldPs)
anova(m1)

#3
ggplot(oldPs, aes(x = C3,y = WAR)) + geom_boxplot(aes(fill = C3)) + theme_minimal()
ggsave(paste0("/Users/chelseaparlett/Desktop/Desktop/Github/BaseBall/scoreComp/WAR3",".png"), units = "in", height = 10, width = 10)
ggplot(oldPs, aes(x = C3,y = WPA)) + geom_boxplot(aes(fill = C3)) + theme_minimal()
ggsave(paste0("/Users/chelseaparlett/Desktop/Desktop/Github/BaseBall/scoreComp/WPA3",".png"), units = "in", height = 10, width = 10)
ggplot(oldPs, aes(x = C3,y = SIERA)) + geom_boxplot(aes(fill = C3)) + theme_minimal()
ggsave(paste0("/Users/chelseaparlett/Desktop/Desktop/Github/BaseBall/scoreComp/SIERA3",".png"), units = "in", height = 10, width = 10)

#2
ggplot(oldPs, aes(x = C2,y = WAR)) + geom_boxplot(aes(fill = C2)) + theme_minimal()
ggsave(paste0("/Users/chelseaparlett/Desktop/Desktop/Github/BaseBall/scoreComp/WAR2",".png"), units = "in", height = 10, width = 10)
ggplot(oldPs, aes(x = C2,y = WPA)) + geom_boxplot(aes(fill = C2)) + theme_minimal()
ggsave(paste0("/Users/chelseaparlett/Desktop/Desktop/Github/BaseBall/scoreComp/WPA2",".png"), units = "in", height = 10, width = 10)
ggplot(oldPs, aes(x = C2,y = SIERA)) + geom_boxplot(aes(fill = C2)) + theme_minimal()
ggsave(paste0("/Users/chelseaparlett/Desktop/Desktop/Github/BaseBall/scoreComp/SIERA2",".png"), units = "in", height = 10, width = 10)
