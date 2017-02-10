rm(list=ls())
#Install the rpart package used to construct classification and regression trees and the tree package if you have
#not already done so, then intitialize rpart and the tree package
#install.packages("rpart")
#install.packages("tree")
library(rpart)
library(tree)
library(MASS)
set.seed(5082)


#####################################################################
#Create a regression tree with the solder data set in rpart
?solder
summary(solder$skips)

#Fit the tree
tree.solder = tree(skips~., data = solder)

#Take a look at the tree, number of nodes, variables used and 
#Residual mean deviance = deviance / (n-terminal nodes)
summary(tree.solder)

#Plot the tres with node labels
par(mfrow=c(1,1))
plot(tree.solder, type = "uniform")
text(tree.solder, splits = TRUE, label = "yval", all = FALSE, pretty=0, cex = 7/8)

#Examine the branch split criterion in a list by node level
tree.solder

#Evaluate the performance of the tree, returns the test MSE
train = sample(1: nrow(solder), nrow(solder)/2)
test = solder[-train,]
skips.test=solder$skips[-train]
tree.trainsolder = tree(skips~., solder, subset=train)
tree.pred = predict(tree.trainsolder, test)
tree.testMSE <- mean((tree.pred-skips.test)^2)
tree.testMSE

#Pruning for potentially reduced test error rate
cv.solder=cv.tree(tree.solder, FUN=prune.tree, K=5) #FUN is the function to identify the CV error
names(cv.solder)
cv.solder

#Plotting the cross validation error rate as a function of both size and alpha.
par(mfrow=c(1,2))
plot(cv.solder$size ,cv.solder$dev ,type="b")
plot(cv.solder$k ,cv.solder$dev ,type="b")

  # The lowest cross-validation error rate (dev) appears at size 12 and alpha = 0 (k)

# Use prune.tree() to prune the tree, setting best size equal to 12 as a result of the 
#cross-validation
prune.solder = prune.tree(tree.solder, best=12)
par(mfrow=c(1,1))
plot(prune.solder, type = "uniform")
text(prune.solder, pretty =0)

#Evaluate the performance of the subtree


  #Same as the main tree

#Write the data into a csv file to explore further, make sure working directory is correct
write.csv(solder, file = "solder.csv", row.names = FALSE)


#Another Example, using the UScrime data set in the MASS library. 
#This tree explores the effect of factors such as population, population makeup, schooling,
#unemployment rates and likelihood of going to prison & how long on crime rate
?UScrime
str(UScrime)

#Fit the tree
tree.crime = tree(y~., data = UScrime)

#Take a look at the tree
summary(tree.crime)
    #It only includes police expenditure, state population, labor force participation rate and
    #Number of non-white individuals per 1000 people in the tree

#Plot the tree & examine the split criterion
par(mfrow=c(1,1))
plot(tree.crime)
text(tree.crime, pretty=0)

tree.crime

#Test to see if pruning the tree into a smaller subtree will improve performance
#Plot the cross validation error rate as a function of both size and alpha
cv.crime <- cv.tree(tree.crime)
cv.crime
par(mfrow=c(1,2))
plot(cv.crime$size, cv.crime$dev, type = 'b')
plot(cv.crime$k, cv.crime$dev, type='b')



#Example 3, Uses MASS data frame birthwt which includes various factors that influence
#infant birth weight in grams

train = sample(1:nrow(birthwt), .8*nrow(birthwt))
tree.birthwt = tree(bwt~.-low, data = birthwt, subset = train)
summary(tree.birthwt)
par(mfrow=c(1,1))
plot(tree.birthwt)
text(tree.birthwt, pretty=0)

#Prune the tree to minimize overfitting/test error
cv.birthwt <- cv.tree(tree.birthwt)
cv.birthwt
par(mfrow=c(1,2))
plot(cv.birthwt$size, cv.birthwt$dev, type = 'b')
plot(cv.birthwt$k, cv.birthwt$dev, type='b')
prune.birthwt = prune.tree(tree.birthwt, best=5)
par(mfrow=c(1,1))
plot(prune.birthwt)
text(prune.birthwt, pretty=0)

par(mfrow=c(1,2))
plot(tree.birthwt)
text(tree.birthwt, pretty=0, cex = 3/4)
plot(prune.birthwt)
text(prune.birthwt, pretty=0)

# Use the unpruned tree to make predictions on the test set.
pred.birthwt = predict(tree.birthwt, newdata=birthwt[-train,])
bwt.test = birthwt[-train, "medv"]
plot(pred.birthwt, bwt.test)
mean((pred.birthwt-bwt.test)^2)


#############EXTRA#######################
#Plotting a binary tree
install.packages("igraph")
library(igraph)


binTree <- graph.tree(n=14,children=2, mode="out")

# plot the binary tree, you can adjust the shape of the tree, whether the parents or leaves are first, the colors, the size, etc
par(mfrow=c(1,1))
plot(binTree, layout = layout.reingold.tilford(binTree, root=1), vertex.size=25)
plot(binTree, layout = layout_as_tree(binTree, root = numeric(), circular = TRUE, rootlevel = numeric(), 
                          mode = "out", flip.y = TRUE), vertex.size=30, vertex.color=rainbow(6), margin = -0.25)

