###########################################################
#ADMIN
###########################################################
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
library(kohonen)          # SOMs
library(igraph)           # for NBRC
library(ggraph)           # for NBRC
library(som)              # SOMs round 2
library(microbenchmark)
library(gRbase)
#LI-------------------------------------------------------
LI <- graph( edges=c("Dani","Jack",
                     "LauraA", "Wes","LauraA", "JackFo","LauraA", "Paul",
                     "JoshD","Georgia", "JoshD", "Kazimir",
                     "Megan", "Eyal", "Megan", "AlexM", "Megan", "Wes",
                     "AlexG","Samira","AlexG","EllieB", "AlexG","Grace", "AlexG","Alexandra",
                     "JackFo", "LauraC",
                     "JoshM","Stephanie",
                     "Georgia","Niall","Georgia","Sam",
                     "Sam","Samira", "Sam", "EllieJ",
                     "CharlieB","EllieB",
                     "Samira","Frankie",
                     "Darylle","Adam",
                     "Zara","Adam",
                     "Eyal", "Hayley",
                     "Rosie", "Adam",
                     "CharlieF", "Megan",
                     "Niall", "Kendall",
                     "Kendall","Adam"
), directed=F) 
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
makekNNgraph <- function(df, k = 5, names = F){
  d <- as.matrix(dist(df))
  rnames <- rownames(df)
  connections <- c()
  for (i in 1:dim(d)[1]){
    d[i,i] <- 999999999 #don't count self as match
    indices <- which(d[,i] %in% sort(d[,i])[1:k])
    indices <- indices[1:k] #incase theres too many
    for (j in indices){
      if (names){
        connections <- c(connections, rnames[i], rnames[j])
      } else {
        connections <- c(connections, i, j)
      }
    }
  }
  connections <- as.character(connections)
  graph <- graph(edges = connections,
                 n = dim(d)[1],
                 directed = F)
  graph <- simplify(graph)
  
  return(graph)
}
nbrClust <- function(graph, method = "vat", attackUpperBound = 5){
  graph <- LI
  method = "vat"
  attackUpperBound = 2
  n <- vcount(graph)
  print("finding attak set options...")
  
  trythis <- function(n,x){
    as.list(data.frame(combn(n,x)))
    print(x)
  }
  attacks <- lapply(1:attackUpperBound,
                    FUN = function(x) trythis(n,x))
  attacksTest <- do.call(c, attacks)
  
  resilience <- function(graph, attackset){
    V_S <- delete_vertices(graph, attackset)
    comps <- components(V_S)
    sCmax <- max(comps$csize)
    sV <- vcount(graph)
    sS <- length(attackset)
    k <- length(comps$csize)
    sV_S <- vcount(V_S)
    
    integrity <- (sS + sCmax)/(sV)
    vat <- (sS)/((sV_S - sCmax) +1)
    toughness <- (sS)/k
    
    return(list(integrity = integrity,
                vat = vat,
                toughness = toughness,
                k = k,
                attackset = attackset,
                V_S = V_S))
  }
  
  
  
  print("testing attack sets...")
  results <- lapply(attacksTest, function(x) resilience(graph,x))
  print("almost done...")
  res <- as.numeric(sapply(results, function(x) x[method]))
  
  candidates <- which(res == min(res))
  
  if (length(candidates) > 1){
    optimalchoice <- results[[sample(candidates,1)]]
  } else {
    optimalchoice <- results[[candidates[1]]]
  }
  
  return(optimalchoice)
}
greedyBC_NBR <- function(graph = NULL, method = "integrity", test = FALSE, treatAS = NULL){
  if (test){
    graph <- knnBB
    method = "integrity"
    test = TRUE
  }
  
  resilience <- function(graph){
    comps <- components(graph)
    sCmax <- max(comps$csize)
    sV <- vcount(graph)
    sS <- length(attackset)
    k <- length(comps$csize)
    sV_S <- vcount(graph)
    
    integrity <- (sS + sCmax)/(sV)
    vat <- (sS)/((sV_S - sCmax) +1)
    toughness <- (sS)/k
    
    return(list(integrity = integrity,
                vat = vat,
                toughness = toughness,
                k = k))
  }
  graph2 <- graph
  n <- vcount(graph)
  resils <- c()
  
  attackset <- c()
  
  while (vcount(graph) > 0){
    BC <- betweenness(graph)
    maxBC <- which(max(BC) == BC)[1]
    attackset <- c(attackset,V(graph)$name[maxBC])
    graph <- delete_vertices(graph, V(graph)$name[maxBC])
    res <- resilience(graph)[method]
    resils <- c(resils, as.numeric(res))
  }
  
  resils[length(resils)] <- 99999
  minR <- which(min(resils) == resils)
  AS <- attackset[1:minR]
  
  V_S <- delete_vertices(graph2, AS)
  comps <- components(V_S)
  k <- length(comps$csize)
  
  ggraph(V_S) + geom_edge_link(edge_linetype = "longdash", colour = "dark gray") +
    geom_node_point(size = 5, colour = "dark gray") +
    geom_node_text(aes(label = V(V_S)$name),
                   repel = T, colour = "black", fontface = "bold", size = 4) +
    theme_void()
  
  return(list(resilience = min(resils),
              k = k,
              attackset = AS,
              V_S = V_S))
}
graphGraph <- function(graph){
  
  ggraph(graph) + geom_edge_link(edge_linetype = "longdash", colour = "dark gray") +
    geom_node_point(size = 5, colour = "dark gray") +
    geom_node_text(aes(label = V(graph)$name),
                   repel = T, colour = "black", fontface = "bold", size = 4) +
    theme_void()
}
somDat <- function(n1,n2,df){
  somMod <- som(scale(df[1:11]), grid = somgrid(n1,n2))
  plot(somMod)
  df[,paste0("som", n1, "_", n2)] <- somMod$unit.classif
  return(list(model = somMod, dataFrame = df))
}
somDat2 <- function(n1,n2,df){
  somMod <- som::som(df[1:11], n1, n2, init="linear", alpha=NULL, alphaType="inverse",
      neigh="gaussian", topol="rect", radius=NULL, rlen=NULL, err.radius=1,
      inv.alp.c=NULL)
  df[,paste0("som", n1, "_",n2)] <- paste0(somMod$visual$x,somMod$visual$y)
  return(list(model = somMod, dataFrame = df))
}
#dataLoad-------------------------------------------------
setwd("/Users/chelseaparlett/Desktop/Desktop/Github/BaseBall/Data/AnalysisDat")
d <- read.csv("OldPitch.csv")
select <- names(d) %in% c("C2","C3","C4",
                          "C5","C6","C7",
                          "C8","C9")
d <- d[, !select ]
d2 <- data.frame(lapply(d[,3:13],scale))
d2$name <- d$name

#dataCheck-----------------------------------------------
plot(d2[,1:11])

###########################################################
#CLUSTERING
###########################################################
#HAC------------------------------------------------------
hDat <- hac(d2, cN = 11, start = 1)
#EM-------------------------------------------------------
#mDat <- em(d2,dim(d2)[2]-1)
#NBRC------------------------------------------------------
rownames(d2) <- d2$name

knnBB <- makekNNgraph(d2[1:11], names = T, k = 3)

ggraph(knnBB) + geom_edge_link(edge_linetype = "longdash", colour = "dark gray") +
  geom_node_point(size = 5, colour = "dark gray") +
  geom_node_text(aes(label = V(knnBB)$name),
                 repel = T, colour = "black", fontface = "bold", size = 4) +
  theme_void()

greedyBB <- greedyBC_NBR(knnBB)

ggraph(greedyBB$V_S) + geom_edge_link(edge_linetype = "longdash", colour = "dark gray") +
  geom_node_point(size = 5, colour = "dark gray") +
  geom_node_text(aes(label = V(greedyBB$V_S)$name),
                 repel = T, colour = "black", fontface = "bold", size = 4) +
  theme_void()

#nbrcBB <- nbrClust(knnBB, method = "integrity", attackUpperBound = 7)
#SOM-------------------------------------------------------

som1_2 <- somDat2(1,2,d2)
d2 <- som1_2$dataFrame

som1_3 <- somDat2(1,3,d2)
d2 <- som1_3$dataFrame

som2_2 <- somDat2(2,2,d2)
d2 <- som2_2$dataFrame

som2_3 <- somDat2(2,3,d2)
d2 <- som2_3$dataFrame

###########################################################
#PLOTTING
###########################################################
#Plots----------------------------------------------------
#hDat <- read.csv(file.choose())[,2:22]
for (i in 2:9){
  dt <- hDat[,c(1:11,11+i)]
  names(dt) <- c(names(dt)[1:11], "ClusterAssign")
  dt$ClusterAssign <- factor(dt$ClusterAssign)
ggRadar(data = dt, aes(group = ClusterAssign), rescale = F, legend.position = "right")+
  theme(legend.title=element_blank(),legend.text=element_text(size=20)) + theme_minimal() + ggtitle(paste(i,"Cluster Characteristics"))
ggsave(paste0("/Users/chelseaparlett/Desktop/Desktop/Github/BaseBall/radar/RadarnoWAR",i,".png"), units = "in", height = 10, width = 10)
}
#pca PLOTS------------------------------------------------
###########################################################
#SAVE DATA
###########################################################
#saveData-------------------------------------------------
write.csv(hDat, "/Users/chelseaparlett/Desktop/Desktop/Github/BaseBall/CurrentData.csv", row.names = F)
###########################################################
#EVALUATING MODELS
###########################################################
#knn------------------------------------------------------
# oldPs <- read.csv(file.choose())
oldPs <- hDat
newPs <- read.csv("NewPitch.csv")

ns <- names(oldPs[1:12])
for (i in ns){
  newPs[,i] <- (newPs[,i] - mean(oldPs[,i]))/sd(OldPs[,i])
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
