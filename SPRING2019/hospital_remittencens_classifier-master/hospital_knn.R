
library(class)

#### KNN 3 

train.hospital.knn = as.matrix(train.hospital.red2[, -11])
test.hospital.knn = as.matrix(test.hospital.red2[, -11])




pred.knn.hospital = knn(train = train.hospital.knn, 
                        test = test.hospital.knn, 
                        cl = train.hospital.red2$readmitted, k = 3)


table(truth=test.hospital.red2$readmitted, predicted=pred.knn.hospital)

mean(train.hospital.red2$readmitted==pred.knn.hospital)




#### KNN 3 FULL
#### KNN 3 FULL
#### KNN 3 FULL
#### KNN 3 FULL

train.hospital.knn = as.matrix(train.hospital[, -c(1, 16)])
test.hospital.knn = as.matrix(test.hospital[, -c(1, 16)])




pred.knn.hospital = knn(train = train.hospital.knn, 
                        test = test.hospital.knn, 
                        cl = train.hospital$readmitted, k = 3)


table(truth=test.hospital$readmitted, predicted=pred.knn.hospital)

mean(test.hospital$readmitted==pred.knn.hospital)




#### KNN 5 FULL
#### KNN 5 FULL
#### KNN 5 FULL
#### KNN 5 FULL



pred.knn.hospital = knn(train = train.hospital.knn, 
                        test = test.hospital.knn, 
                        cl = train.hospital$readmitted, k = 5)


table(truth=test.hospital$readmitted, predicted=pred.knn.hospital)

mean(test.hospital$readmitted==pred.knn.hospital)




