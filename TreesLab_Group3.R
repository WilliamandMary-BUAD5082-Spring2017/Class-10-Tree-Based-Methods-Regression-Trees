#Install the tree package used to construct classification and regression trees
install.packages("tree")
library(tree)

#Need to import a data set here

#Create a classification tree with the data set

#Create a binary Y variable that will use the tree to predict
y <- ifelse(condition, "No", "Yes")

#Add the new binary to the data frame
newdata <- data.frame(oldData, y)

#Fit the tree to predict the Y variable
tree.newdata = tree(y~.-oldvar, newdata)

#Examine the tree for the variables used, importance of the different variables, number of nodes
#and the training error rate
summary(tree.newdata)

plot(tree.newdata)

#Return the node labels including category names
text(tree.newdata, pretty=0)

#Examine the branch split criterion in a list by node level
tree.newdata

#Evaluate the performance of the tree, type = class means to return the class prediction
#Returns the percentage of correct predictions for the test data
train= sample (1: nrow(newdata), nrow(newData)/2)
test=newdata[-train ,]
Y.test=y[-train]
tree.newdata =tree(y~.-oldVar, newdata ,subset=train)
tree.pred=predict(tree.newdata, test ,type="class")
table(tree.pred, Y.test)

#Pruning for potentially reduced test error rate
#cv.tree() performs cross-validation to determine the optimal level of tree complexity of the sequence of trees
#The cv.tree() function reports the number of terminal nodes of each tree considered (size) as well as the 
#corresponding error rate and the value of the cost-complexity parameter( ?? ).

cv.newdata =cv.tree(tree.newdata, FUN=prune.misclass) #FUN indicates that classification error rate guides CV
names(cv.newdata)
cv.newdata


#Plotting the error rate as a function of both size and ??.
par(mfrow=c(1,2))
plot(cv.newdata$size ,cv.newdata$dev ,type="b")
plot(cv.newdata$k ,cv.newdata$dev ,type="b")

# Use prune.misclass()to prune the tree and obtain the correct tree
prune.newdata =prune.newdata (tree.newdata ,best=nodenum)
plot(prune.newdata)
text(prune.newdata, pretty =0)

#Test the error rate of the subtree
tree.pred=predict(prune.newdata, test, type="class")
table(tree.pred, Y.test)

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

