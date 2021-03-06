## R code  for clustering 
## ref: http://cran.r-project.org/doc/contrib/Zhao_R_and_data_mining.pdf
# R and Data Mining: Examples and Case Studies
# time: 2013.10.18

### K means method

iris2 <- iris
 iris2$Species <- NULL
 (kmeans.result <- kmeans(iris2,3))
kmeans.result$cluster
 table(iris$Species, kmeans.result$cluster)
 plot(iris2[c("Sepal.Length", "Sepal.Width")], col = kmeans.result$cluster)
 # plot cluster centers
 points(kmeans.result$centers[,c("Sepal.Length", "Sepal.Width")], col = 1:3,
pch = 8, cex=2)


### The k-Medoids Clustering

 library(fpc)
 pamk.result <- pamk(iris2)
 # number of clusters
 pamk.result$nc


# check clustering against actual species
 table(pamk.result$pamobject$clustering, iris$Species)

 layout(matrix(c(1,2),1,2)) # 2 graphs per page
plot(pamk.result$pamobject)
layout(matrix(1)) # change back to one graph per page


## pam=3
pam.result <- pam(iris2, 3)
 table(pam.result$clustering, iris$Species)

 layout(matrix(c(2,1),2,1)) # 2 graphs per page
 plot(pam.result)
 layout(matrix(1)) # change back to one graph per page


### hierarchical method

 idx <- sample(1:dim(iris)[1], 40)
irisSample <- iris[idx,]
 irisSample$Species <- NULL
 hc <- hclust(dist(irisSample), method="ave")

 plot(hc, hang = -1, labels=iris$Species[idx])
 # cut tree into 3 clusters
 rect.hclust(hc, k=4)
groups <- cutree(hc, k=3)



### density based cluster

 library(fpc)
 iris2 <- iris[-5] # remove class tags
 ds <- dbscan(iris2, eps=0.42, MinPts=5)
 # compare clusters with original class labels
 table(ds$cluster, iris$Species)
plot(ds, iris2)
plot(ds, iris2[c(1,4)])
plotcluster(iris2, ds$cluster)

# predition
 # create a new dataset for labeling
 set.seed(435)
 idx <- sample(1:nrow(iris), 10)
 newData <- iris[idx,-5]
 newData <- newData + matrix(runif(10*4, min=0, max=0.2), nrow=10, ncol=4)
 # label new data
 myPred <- predict(ds, iris2, newData)
 # plot result
 plot(iris2[c(1,4)], col=1+ds$cluster)
 points(newData[c(1,4)], pch="*", col=1+myPred, cex=3)
 # check cluster labels
 table(myPred, iris$Species[idx])
