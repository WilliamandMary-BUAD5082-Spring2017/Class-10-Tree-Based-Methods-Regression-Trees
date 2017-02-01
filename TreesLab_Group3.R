rm(list=ls())
#Install the rpart package used to construct classification and regression trees and intitialize that and the tree
#subpackage
install.packages("rpart")
install.packages("tree")
library(rpart)
library(tree)
set.seed(5082)

#Exploring the data set located in rpart library containing information on kyphosis corrective spinal surgery
?kyphosis
names(kyphosis)

####Create a classification tree with the data set
kyphosis.df <- kyphosis

#Fit the tree to predict the Y variable
tree.kyphosis = tree(Kyphosis~., kyphosis.df)

#Examine the tree for the variables used, importance of the different variables, number of nodes
#and the training error rate
summary(tree.kyphosis)
par(mfrow=c(1,1))
plot(tree.kyphosis)

#Return the node labels including category names
text(tree.kyphosis, pretty=0)

#Examine the branch split criterion in a list by node level
tree.kyphosis

#Evaluate the performance of the tree, type = class means to return the class prediction
#Returns the percentage of correct predictions for the test data
train= sample(1: nrow(kyphosis.df), nrow(kyphosis.df)/2)
test=kyphosis.df[-train ,]
Y.test=kyphosis.df$Kyphosis[-train]
tree.kyphosis =tree(Kyphosis~., kyphosis.df, subset=train)
tree.pred=predict(tree.kyphosis, test ,type="class")
table(tree.pred, Y.test)

#Pruning for potentially reduced test error rate
#cv.tree() performs cross-validation to determine the optimal level of tree complexity of the sequence of trees
#The cv.tree() function reports the number of terminal nodes of each tree considered (size) as well as the 
#corresponding error rate and the value of the cost-complexity parameter( alpha ).

cv.kyphosis =cv.tree(tree.kyphosis, FUN=prune.misclass) #FUN indicates that classification error rate guides CV
names(cv.kyphosis)
cv.kyphosis


#Plotting the cross validation error rate as a function of both size and alpha.
par(mfrow=c(1,2))
plot(cv.kyphosis$size ,cv.kyphosis$dev ,type="b")
plot(cv.kyphosis$k ,cv.kyphosis$dev ,type="b")
  #The subtree with the lowest cross-validation error (dev) has a size of 5 and -infinity alpha value (k)

# Use prune.misclass()to prune the tree and obtain the correct tree
prune.kyphosis =prune.misclass(tree.kyphosis, best=5)
plot(prune.kyphosis)
text(prune.kyphosis, pretty =0)

#Test the error rate of the subtree
subtree.pred=predict(prune.kyphosis, test, type="class")
table(subtree.pred, Y.test)


#####################################################################
#Repeat withe solder data set in rpart
?solder
summary(solder$skips)

#Create a binary variable indcating if it was a good soldering job (below median number of visual solder skips)
GoodJob <- ifelse(solder$skips > median(solder$skips), "No", "Yes")

#create new data set with the new binary variable
solder.df <- data.frame(solder, GoodJob)

#Fit the tree
tree.solder = tree(GoodJob~.-skips, solder.df)

#Plot the tres with node labels
summary(tree.solder)
par(mfrow=c(1,1))
plot(tree.solder)
text(tree.solder, pretty=0)

#Examine the branch split criterion in a list by node level
tree.solder

#Evaluate the performance of the tree,Returns the percentage of correct predictions for the test data
train2= sample(1: nrow(solder.df), nrow(solder.df)/2)
test2=solder.df[-train2,]
GoodJob.test=solder.df$GoodJob[-train2]
tree.solder =tree(GoodJob~.-skips, solder.df, subset=train2)
tree.pred2=predict(tree.solder, test2, type="class")
table(tree.pred2, GoodJob.test)

#Pruning for potentially reduced test error rate
cv.solder =cv.tree(tree.solder, FUN=prune.misclass) #FUN indicates that classification error rate guides CV
names(cv.solder)
cv.solder

#Plotting the cross validation error rate as a function of both size and alpha.
par(mfrow=c(1,2))
plot(cv.solder$size ,cv.solder$dev ,type="b")
plot(cv.solder$k ,cv.solder$dev ,type="b")

  # The lowest cross validation error rate (dev) appears at size 12 and alpha = 1.0 (k)

# Use prune.misclass() to prune the tree, setting best size equal to 12 as a result of the cross-validation
prune.solder =prune.misclass(tree.solder, best=12)
par(mfrow=c(1,1))
plot(prune.solder)
text(prune.solder, pretty =0)

#Test the error rate of the subtree
subtree.pred2=predict(prune.solder, test2, type="class")
table(subtree.pred2, GoodJob.test)


                  
###########If we would like to use the data in rpart#######
install.packages("rpart")
library(rpart)
?cu.summary
names(cu.summary) #Automobile data from Consumer Reports 1990
names(kyphosis) #Data on kids that had corrective spinal surgury
fit <- rpart(Kyphosis ~ Age + Number + Start, data = kyphosis)
fit2 <- rpart(Kyphosis ~ Age + Number + Start, data = kyphosis,
              parms = list(prior = c(0.65, 0.35), split = "information"))
fit3 <- rpart(Kyphosis ~ Age + Number + Start, data=kyphosis,
              control = rpart.control(cp = 0.05))
par(mfrow = c(1,2), xpd = TRUE)
plot(fit)
text(fit, use.n = TRUE)
plot(fit2)
text(fit2, use.n = TRUE)
labels(object, digits = 4, minlength = 1L, pretty, collapse = TRUE, ...) #create split labels
plot(x, uniform = FALSE, branch = 1, compress = FALSE, nspace, margin = 0, minbranch = 0.3, ...)  #plot R object


#############EXTRA#######################
#Plotting a binary tree
install.packages("igraph")
library(igraph)


binTree <- graph.tree(n=nodenum,children=2)

#add names to vertex (just assign a upper-case letter to each)
V(binTree)$name <- LETTERS[1:length(V(G))]

# plot the binary tree
lay <- layout.reingold.tilford(binTree, params=list(root='A')) 
plot(binTree, layout=lay, vertex.size=25)

