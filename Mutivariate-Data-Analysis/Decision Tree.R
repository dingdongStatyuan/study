######################################################### Zhao_R_and_data_mining.pdf
# desicion tree study
# 2013.11.12

# data:iris. package:party
library(party)
# get traning data(70%) and test data(30%) randomly
str(iris)
set.seed(1234)
ind <- sample(2, nrow(iris), replace=TRUE, prob=c(0.7, 0.3))
trainData <- iris[ind==1,]
testData <- iris[ind==2,]

# build trees with traning data
myFormula <- Species ~ Sepal.Length + Sepal.Width + Petal.Length + Petal.Width
iris_ctree <- ctree(myFormula, data=trainData)
# check the prediction
table(predict(iris_ctree), trainData$Species)
print(iris_ctree)
# plot tree
plot(iris_ctree)
plot(iris_ctree, type="simple")

# predict on test data
testPred <- predict(iris_ctree, newdata = testData)
table(testPred, testData$Species)

# # data:bodyfat. package:rpart
library(rpart)
library(mboost)
data("bodyfat", package = "mboost")
dim(bodyfat)
attributes(bodyfat)

set.seed(1234)
ind <- sample(2, nrow(bodyfat), replace=TRUE, prob=c(0.7, 0.3))
bodyfat.train <- bodyfat[ind==1,]
bodyfat.test <- bodyfat[ind==2,]
# train a decision tree
 myFormula <- DEXfat ~ age + waistcirc + hipcirc + elbowbreadth + kneebreadth
 bodyfat_rpart <- rpart(myFormula, data = bodyfat.train,
 control = rpart.control(minsplit = 10))
attributes(bodyfat_rpart)
print(bodyfat_rpart$cptable)
print(bodyfat_rpart)
plot(bodyfat_rpart)
text(bodyfat_rpart, use.n=T)

opt <- which.min(bodyfat_rpart$cptable[,"xerror"])
cp <- bodyfat_rpart$cptable[opt, "CP"]
bodyfat_prune <- prune(bodyfat_rpart, cp = cp)
 print(bodyfat_prune)
 plot(bodyfat_prune)
text(bodyfat_prune, use.n=T)

# prediction
 DEXfat_pred <- predict(bodyfat_prune, newdata=bodyfat.test)
 xlim <- range(bodyfat$DEXfat)
 plot(DEXfat_pred ~ DEXfat, data=bodyfat.test, xlab="Observed",
 ylab="Predicted", ylim=xlim, xlim=xlim)
 abline(a=0, b=1)
 
 
 
 ################################## http://www.statmethods.net/advstats/cart.html
 # Classification Tree with rpart
library(rpart)
par(mar=c(0.1,.1,1,.1))
# grow tree 
fit <- rpart(Kyphosis ~ Age + Number + Start,
  	 method="class", data=kyphosis)

printcp(fit) # display the results 
plotcp(fit) # visualize cross-validation results 
summary(fit) # detailed summary of splits

# plot tree 
plot(fit, uniform=TRUE, 
  	 main="Classification Tree for Kyphosis")
text(fit, use.n=TRUE, all=TRUE, cex=.8)

# create attractive postscript plot of tree 
post(fit, file = "c:/tree.ps", 
  	 title = "Classification Tree for Kyphosis")

# prune the tree 
pfit<- prune(fit, cp=   fit$cptable[which.min(fit$cptable[,"xerror"]),"CP"])

# plot the pruned tree 
plot(pfit, uniform=TRUE, 
   main="Pruned Classification Tree for Kyphosis")
text(pfit, use.n=TRUE, all=TRUE, cex=.8)
post(pfit, file = "c:/ptree.ps", 
   title = "Pruned Classification Tree for Kyphosis")



# Regression Tree Example
library(rpart)

# grow tree 
fit <- rpart(Mileage~Price + Country + Reliability + Type, 
   method="anova", data=cu.summary)

printcp(fit) # display the results 
plotcp(fit) # visualize cross-validation results 
summary(fit) # detailed summary of splits

# create additional plots 
par(mfrow=c(1,2)) # two plots on one page 
rsq.rpart(fit) # visualize cross-validation results   

# plot tree 
plot(fit, uniform=TRUE, 
   main="Regression Tree for Mileage ")
text(fit, use.n=TRUE, all=TRUE, cex=.8)

# create attractive postcript plot of tree 
post(fit, file = "c:/tree2.ps", 
   title = "Regression Tree for Mileage ")

# prune the tree 
pfit<- prune(fit, cp=0.01160389) # from cptable   

# plot the pruned tree 
plot(pfit, uniform=TRUE, 
   main="Pruned Regression Tree for Mileage")
text(pfit, use.n=TRUE, all=TRUE, cex=.8)
post(pfit, file = "c:/ptree2.ps", 
   title = "Pruned Regression Tree for Mileage")

library(randomForest)
fit <- randomForest(Kyphosis ~ Age + Number + Start,   data=kyphosis)
print(fit) # view results 
importance(fit) # importance of each predictor 


