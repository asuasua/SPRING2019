####################-------Decision Trees---------############
library(tree)
library(randomForest)
library(gbm)
library(PRROC)
library(reprtree)

###################### Decision Tree (requires factor)
######################
############## Now I will use all factors from the original train/test

train.hospital$readmitted = factor(train.hospital$readmitted)
test.hospital$readmitted = factor(test.hospital$readmitted)




tree.hospital = tree(readmitted~., data=train.hospital, minsize=5)


pred.tree.hospital = predict(tree.hospital, test.hospital, type='class')


table(truth=test.hospital$readmitted, predicted=pred.tree.hospital)
mean(test.hospital$readmitted==pred.tree.hospital)


plot(tree.hospital)
text(tree.hospital, pretty=0)


###################### Random Forest (recquires factor)
###################### Random Forest (recquires factor)
###################### Random Forest (recquires factor)
###################### Random Forest (recquires factor)


train.hospital$readmitted = factor(train.hospital$readmitted)
test.hospital$readmitted = factor(test.hospital$readmitted)


set.seed(2)

rf.hospital = randomForest(readmitted~., data=train.hospital, 
                           mtry=5, ntree=100, importance=T)

#getTree(rf.hospital, k=3)

pred.rf.hospital = predict(rf.hospital, test.hospital, type='class')
table(truth=test.hospital$readmitted, predicted=pred.rf.hospital)
mean(test.hospital$readmitted==pred.rf.hospital)
plot(rf.hospital, main='5 Features : 100 Trees')
legend("top", colnames(rf.hospital$err.rate),col=1:4,cex=0.8,fill=1:4)
  

#######
set.seed(122)

rf.hospital = randomForest(readmitted~., data=train.hospital, 
                           mtry=5, ntree=500, importance=T)

#getTree(rf.hospital, k=3)

pred.rf.hospital = predict(rf.hospital, test.hospital, type='class')
table(truth=test.hospital$readmitted, predicted=pred.rf.hospital)
mean(test.hospital$readmitted==pred.rf.hospital)
plot(rf.hospital, main='5 Features : 500 Trees')
legend("top", colnames(rf.hospital$err.rate),col=1:4,cex=0.8,fill=1:4)



#######
set.seed(4422)

rf.hospital = randomForest(readmitted~., data=train.hospital, 
                           mtry=4, ntree=1000, importance=T)

#getTree(rf.hospital, k=3)

pred.rf.hospital = predict(rf.hospital, test.hospital, type='class')
table(truth=test.hospital$readmitted, predicted=pred.rf.hospital)
mean(test.hospital$readmitted==pred.rf.hospital)
plot(rf.hospital, main='4 Features : 1000 Trees')
legend("top", colnames(rf.hospital$err.rate),col=1:4,cex=0.8,fill=1:4)


#Not enough sub-categories. 


###################### Boosted (requires integer, bc we're using gradiend descent)
###################### Boosted (requires integer)
###################### Boosted (requires integer)
###################### Boosted (requires integer)

# train.hospital$readmitted = as.numeric(as.character(train.hospital$readmitted))
# test.hospital$readmitted = as.numeric(as.character(test.hospital$readmitted))


boost.hospital = gbm(readmitted~., data=train.hospital, n.trees=5000, interaction.depth = 2, shrinkage = 0.2)
pred.boost.hospital = predict(boost.hospital, test.hospital, n.trees=5000, type='response')

pred.boost.hospital = ifelse(pred.boost.hospital>0.5, 1, 0)

table(truth=test.hospital$readmitted, predicted=pred.boost.hospital)

mean(test.hospital$readmitted==pred.boost.hospital)

  
  
  