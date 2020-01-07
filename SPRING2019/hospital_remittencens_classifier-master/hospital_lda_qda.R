
library(MASS)
##### LDA / QDA


qda.hospital = qda(readmitted~., data=train.hospital.red2)

pred.qda.hospital = predict(qda.hospital, test.hospital.red2)$class


table(truth=test.hospital.red2$readmitted, predicted=pred.qda.hospital)

