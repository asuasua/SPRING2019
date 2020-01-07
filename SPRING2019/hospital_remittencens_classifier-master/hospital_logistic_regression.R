
library(PRROC)
library(pROC)
#################### Logistic Regression

log.hospital = glm(readmitted~., data=train.hospital, family='binomial')

##### take out the race



pred.log.hospital = predict(log.hospital, test.hospital, type='response')

pred.log10 = ifelse(pred.log.hospital>0.1, 1, 0)
pred.log20 = ifelse(pred.log.hospital>0.2, 1, 0)
pred.log30 = ifelse(pred.log.hospital>0.3, 1, 0)
pred.log40 = ifelse(pred.log.hospital>0.4, 1, 0)
pred.log50 = ifelse(pred.log.hospital>0.5, 1, 0)

pred.log60 = ifelse(pred.log.hospital>0.6, 1, 0)
pred.log70 = ifelse(pred.log.hospital>0.7, 1, 0)
pred.log80 = ifelse(pred.log.hospital>0.8, 1, 0)


table(truth=test.hospital$readmitted, predicted=pred.log10)
table(truth=test.hospital$readmitted, predicted=pred.log20)
table(truth=test.hospital$readmitted, predicted=pred.log30)
table(truth=test.hospital$readmitted, predicted=pred.log40)
table(truth=test.hospital$readmitted, predicted=pred.log50)

table(truth=test.hospital$readmitted, predicted=pred.log60)
table(truth=test.hospital$readmitted, predicted=pred.log70)
table(truth=test.hospital$readmitted, predicted=pred.log80)




mean(test.hospital$readmitted==pred.log10)
mean(test.hospital$readmitted==pred.log20)
mean(test.hospital$readmitted==pred.log30)
mean(test.hospital$readmitted==pred.log40)
mean(test.hospital$readmitted==pred.log50)

mean(test.hospital$readmitted==pred.log60)
mean(test.hospital$readmitted==pred.log70)
mean(test.hospital$readmitted==pred.log80)
##################
##################
################## Remove statitistcically insignificant 
################## Remove statitistcically insignificant 
################## Remove statitistcically insignificant 

hospital_discharge_binary_red = hospital_discharge_binary[, -c(1,2)]
train.hospital.red = train.hospital[, -c(1,2)]
test.hospital.red = test.hospital[, -c(1,2)]


log.hospital.red = glm(readmitted~., data = train.hospital.red, family='binomial')



################## Remove Twice
################## Remove Twice
################## Remove Twice
################## Remove Twice

hospital_discharge_binary_red2 = hospital_discharge_binary_red[, -c(5, 10, 11)]

train.hospital.red2 = train.hospital.red[, -c(5, 10, 11)]
test.hospital.red2 = test.hospital.red[, -c(5, 10, 11)]

log.hospital.red2 = glm(readmitted~., data = train.hospital.red2, family='binomial')







######### Final Logistic Regression Model
######### Final Logistic Regression Model
######### Final Logistic Regression Model
######### Final Logistic Regression Model



#Converting age into numerical was very helpful....gives us more flexible weights
#Why does this work? 

pred.log.hospital.red2 = predict(log.hospital.red2, test.hospital.red2, type='response')
pred.log.hospital.red2= ifelse(pred.log.hospital.red2 > 0.5, 1, 0)



table(truth=test.hospital.red2$readmitted, predicted=pred.log.hospital.red2)


mean(test.hospital.red2$readmitted==pred.log.hospital.red2)

# roc.log.hospital.red2 = roc.curve(scores.class0 = pred.log.hospital.red2, 
#                                   weights.class0=test.hospital.red2$readmitted, curve=TRUE)
# 
# plot(roc.log.hospital.red2)


plot(roc(predictor = pred.log.hospital.red2, 
         response = test.hospital.red2$readmitted), main='AUC = 0.6016')


roc(predictor = pred.log.hospital.red2, 
    response = test.hospital.red2$readmitted)












# num_medications, 
# 
# max_glu_serumNone,max_glu_serumNorm, 
# change

### change glucose
glu300 = ifelse(hospital_discharge_binary$max_glu_serum=='>300', 1, 0)



### remove max_clu_serom, num_medications
hospital_discharge_binary=hospital_discharge_binary[, -c(7, 12, 14)]
hospital_discharge_binary$glu300 = glu300


#### change A1

A18 = ifelse(hospital_discharge_binary$A1Cresult=='>8', 1, 0)

hospital_discharge_binary = hospital_discharge_binary[, -11]
hospital_discharge_binary$A18 = A18


###################### Reduce dataframe
train_index = sample(x = dim(hospital_discharge_binary)[1], size = dim(hospital_discharge_binary)[1]*0.8, replace = F)

train.hospital= hospital_discharge_binary[train_index, ]
test.hospital = hospital_discharge_binary[-train_index, ]
