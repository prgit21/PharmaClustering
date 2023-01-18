data <- read.csv("/Users/pr21/Desktop/DSclab/Pharmaceuticals.csv")
summary(data)
str(data)
head(data)
pairs(data[3:11])
plot(data$Rev_Growth~ data$Net_Profit_Margin, data = data)
with(data,text(data$Rev_Growth ~ data$Net_Profit_Margin,
               labels=data$Symbol,pos=2, cex= 0.8)) 
z = data[, -c(1,2,12,13,14)]
z
means = apply(z,2,mean)
sds = apply(z,2,sd)
nor = scale(z,center=means,scale=sds)
nor
distance = dist(nor)
print(distance, digits = 3)
hc.c <- hclust(distance)
plot(hc.c)
plot(hc.c,labels=data$Company,main='Cluster Dendrogram')
plot(hc.c,hang=-1, labels = data$Median_Recommendation, main='Cluster Dendrogram based on 
Median Recommendation')

plot(hc.c,hang=-1, labels = data$Location, main='Cluster Dendrogram based on Location')
plot(hc.c,hang=-1, labels = data$Exchange, main='Cluster Dendrogram based on Stock Exchange')
# Hierarchical agglomerative clustering using "average" linkage 
#Cluster Dendogram with Average Linkage
hc.a<-hclust(distance,method="average")


   # Hierarchical agglomerative clustering using "average" linkage 
   #Cluster Dendogram with Average Linkage
   hc.a<-hclust(distance,method="average")
 plot(hc.a,hang=-1)
 # Cluster membership
 member = cutree(hc.c,3)
 table(member)


 
 member.c <- cutree(hc.c,3)
 member.a <- cutree(hc.a,3)
table(member.c, member.a)

 # Characterizing clusters 
   aggregate(nor,list(member),mean)
aggregate(data[, -c(1,2,12,13,14)],list(member),mean)
# Silhouette Plot
library(cluster) 
 plot(silhouette(cutree(hc.c,3), distance))
 # Scree Plot
   wss <- (nrow(nor)-1)*sum(apply(nor,2,var))
 for (i in 2:20) wss[i] <- sum(kmeans(nor, centers=i)$withinss)
 plot(1:20, wss, type="b", xlab="Number of Clusters", ylab="Within groups sum of squares")
 # K-means clustering
   set.seed(123)
 kc<-kmeans(nor,3)
kc
 plot(ROE~Net_Profit_Margin, data, col= kc$cluster)
 with(data,text(data$Rev_Growth ~ data$Net_Profit_Margin, labels=data$Name,pos=2, cex=0.7))
 
   plot(Rev_Growth~Net_Profit_Margin, data, col= kc$cluster)
 with(data,text(data$Rev_Growth ~ data$Net_Profit_Margin, labels=data$Symbol,pos=2, cex=0.6))

   
   plot(Rev_Growth~Net_Profit_Margin, data, col= kc$cluster)
 with(data,text(data$Rev_Growth ~ data$Net_Profit_Margin, labels=data$Median_Recommendation,pos=2, cex=0.6))
 
   plot(Rev_Growth~Net_Profit_Margin, data, col= kc$cluster)
 with(data,text(data$Rev_Growth ~ data$Net_Profit_Margin, labels=data$Location,pos=2, cex=0.6))
 
   plot(Rev_Growth~Net_Profit_Margin, data, col= kc$cluster)
 with(data,text(data$Rev_Growth ~ data$Net_Profit_Margin, labels=data$Exchange,pos=2, cex=0.6))

   library(kohonen)
 str(data)

 X <- scale(data[, -c(1,2,12,13,14)])
  summary(X)
   set.seed(222)
   g <- somgrid(xdim = 4, ydim = 4, topo = "rectangular" )
  
  map <- som(X,
                        grid = g,
                         alpha = c(0.05, 0.01),
                         radius = 1)
  plot(map)
  
  #plot(map, type = 'changes')
  
  install.packages("kohonen")
