##### Diabetes Hospital


setwd('C:\\Users\\A Sua\\Documents\\FIU\\Spring2019\\HDDA\\Project2')
list.files()


hospital_discharge = read.csv(file = list.files()[2])



######################
train_index = sample(x = dim(hospital_discharge)[1], size = dim(hospital_discharge)[1]*0.8, replace = F)

hospital_train = hospital_discharge[train_index, ]
hospital_test = hospital_discharge[-train_index, ]

###################### Logistic Regression

log_g30 = glm(formula = g30_code~.-readmitted-l30_code-no_code, data=hospital_train, family='binomial')

summary(log_g30)

coef_g30 = summary(log_g30)$coef



log_l30 = glm(formula = l30_code~.-readmitted-g30_code-no_code, data=hospital_train, family='binomial')

summary(log_l30)

coef_l30 = summary(log_l30)$coef


log_nocode = glm(formula = no_code~.-readmitted-g30_code-l30_code, data=hospital_train, family='binomial')
summary(log_nocode)

coef_none = summary(log_nocode)$coef



pred_g = predict(object = log_g30, newdata = hospital_test, type = 'response')
pred_l = predict(object = log_l30, newdata = hospital_test, type = 'response')
pred_n = predict(object = log_nocode, newdata = hospital_test, type = 'response')
probs_df = data.frame(greater=pred_g, less=pred_l, none=pred_n)
max_prob_vec = c()

for(i in seq(dim(probs_df)[1])){
  
  b = max(probs_df[i, ])
  a = match(b, probs_df[i, ])
  
  if(a==1){
    max_prob_vec = append(max_prob_vec, '>30', after = length(max_prob_vec))
  }else if(a==2){
    max_prob_vec = append(max_prob_vec, '<30', after = length(max_prob_vec))    
  }else{
    max_prob_vec = append(max_prob_vec, 'NO', after = length(max_prob_vec))        
  }
  
}



probs_df$max_prob = max_prob_vec


table(truth=hospital_test$readmitted, predicted=max_prob_vec)


mean(hospital_test$readmitted==max_prob_vec)


######################
library(tree)

############# Decision Tree
library(tree)

tree.hospital = tree(readmitted~.-g30_code-l30_code-no_code, data=hospital_train)

plot(tree.hospital)
text(tree.hospital, pretty=0)

tree.hospital.pred = predict(tree.hospital, newdata = hospital_test, type='class')
table(truth=hospital_test$readmitted, predicted=tree.hospital.pred)
mean(hospital_test$readmitted==tree.hospital.pred)


############## Random Forest
library(randomForest)

forest.hospital = randomForest(readmitted~.-g30_code-l30_code-no_code, data=hospital_train, mtry=14, ntree=50)
forest.hospital$forest
forest.hospital$forest$xbestsplit

importance_hospital = data.frame(importance(forest.hospital))

varImpPlot(forest.hospital, sort = T)




text(forest.hospital)
pred.forest.hospital = predict(forest.hospital, newdata = hospital_test, response='class')
table(truth=hospital_test$readmitted, predicted=pred.forest.hospital)
mean(hospital_test$readmitted==pred.forest.hospital)



############ Boosted
library(gbm)
set.seed(1)

boost.hospital = gbm(readmitted~.-g30_code-l30_code-no_code, data=hospital_train, 
                     n.trees = 100, interaction.depth = 4)

boost.hospital = 
  
  
  
  pred.boost.hospital = predict(boost.hospital, newdata=hospital_test, n.trees = 100)




table(truth=hospital_test$readmitted, predicted=pred.boost.hospital)




############## Support Vector Machine




######## Linear
hospital.svm10 = svm(readmitted~.-g30_code-l30_code-no_code, data=hospital_train,
                   kernel='linear',cost=10)

pred.hospital.svm = predict(hospital.svm, hospital_test)
table(truth=hospital_test$readmitted, predicted=pred.hospital.svm)
mean(hospital_test$readmitted==pred.hospital.svm)


hospital.svm100 = svm(readmitted~.-g30_code-l30_code-no_code, data=hospital_train,
                      kernel='linear',cost=100)

pred.hospital.svm100 = predict(hospital.svm100, hospital_test)
table(truth=hospital_test$readmitted, predicted=pred.hospital.svm100)
mean(hospital_test$readmitted==pred.hospital.svm100)


######## Nonlinear

hospital.svm = svm(readmitted~g30_code-l30_code-no_code, data=hospital_train,
                      kernel='radial',cost=10000, gamma=0.001)



pred.hospital.svm = predict(hospital.svm, hospital_test, type='response')


table(truth=hospital_test$readmitted, predicted=pred.hospital.svm)
mean(hospital_test$readmitted==pred.hospital.svm)







tune.hospital.svm = tune(svm, readmitted~g30_code-l30_code-no_code, kernel='linear', 
                         ranges=list(cost=c(0.01, 0.1, 1, 10, 100, 1000)), data = hospital_train, )

tune.hospital.svm$best.model

predict()



max_prob_vec = c()

for(i in seq(dim(probs_df)[1])){
  
  b = max(probs_df[i, ])
  a = match(b, probs_df[i, ])
  
  if(a==1){
    max_prob_vec = append(max_prob_vec, '>30', after = length(max_prob_vec))
  }else if(a==2){
    max_prob_vec = append(max_prob_vec, '<30', after = length(max_prob_vec))    
  }else{
    max_prob_vec = append(max_prob_vec, 'NO', after = length(max_prob_vec))        
  }
  
}





############## Linear Discriminant Analysis





############## KNN








######################




######################
