
library(PRROC)
library(pROC)
#################### Logistic Regression

log.hospital = glm(readmitted~., data=train.hospital, family='binomial')

pred.log.hospital = predict(log.hospital, test.hospital, type='response')

pred.log.hospital = ifelse(pred.log.hospital>0.5, 1, 0)

table(truth=test.hospital$readmitted, predicted=pred.log.hospital)

mean(test.hospital$readmitted==pred.log.hospital)




backwards = step(log.hospital,trace=0)


############## Remove variables


drops <- c("max_glu_serum","A1Cresult", 'repaglinide', 'nateglinide', 'chlorpropamide', 'glimepiride', 'acetohexamide', 'glipizide', 'glyburide',
           'tolbutamide', 'pioglitazone', 'troglitazone', 'tolazamide', 'examide', 'citoglipton', 'glyburide.metformin ', 'glipizide.metformin', 
           'glimepiride.pioglitazone', 'metformin.rosiglitazone', 'metformin.pioglitazone', 'num_medications', 'rosiglitazone', 'glyburide.metformin', 'miglitol')


hospital_discharge_binary.red = hospital_discharge_binary[ ,!(names(hospital_discharge_binary) %in% drops)]



train_index = sample(x = dim(hospital_discharge_binary.red)[1], size = dim(hospital_discharge_binary.red)[1]*0.8, replace = F)

train.hospital.red= hospital_discharge_binary.red[train_index, ]
test.hospital.red = hospital_discharge_binary.red[-train_index, ]



######## Run regression model again



log.hospital.red = glm(readmitted~., data=train.hospital.red, family='binomial')

pred.log.hospital.red = predict(log.hospital.red, test.hospital.red, type='response')

pred.log.hospital.red = ifelse(pred.log.hospital.red > 0.5, 1, 0)



table(truth=test.hospital$readmitted, predicted=pred.log.hospital.red)

mean(test.hospital$readmitted==pred.log.hospital.red)


drop2 = c('race', 'admission_source_id', '')










