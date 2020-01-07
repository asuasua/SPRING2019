##### Diabetes Hospital


#setwd('C:\\Users\\A Sua\\Documents\\FIU\\Spring2019\\HDDA\\Project2')
#list.files()


#hospital_discharge = read.csv(file = list.files()[4])
hospital_discharge=read.csv(file=file.choose())

hospital_discharge_binary = hospital_discharge

### Make into Binary Classifier
readmission_vec = ifelse(hospital_discharge_binary$readmitted=='NO', 0, 1)

#### Remove last 3 columns
hospital_discharge_binary$readmitted=readmission_vec


#hospital_discharge_binary = hospital_discharge_binary[, -c(17,18,19)]




######## Chance race

#hospital_discharge_binary$race <- as.numeric(as.factor(hospital_discharge_binary$race));



########## Change drug columns
########## Change drug columns
########## Change drug columns
########## Change drug columns
########## Change drug columns
columns <- c("metformin", "repaglinide", "nateglinide", "chlorpropamide", "glimepiride", "acetohexamide", "glipizide", "glyburide", "tolbutamide", "pioglitazone", "rosiglitazone", "acarbose", "miglitol", "troglitazone", "tolazamide", "examide", "citoglipton", "insulin", "glyburide.metformin", "glipizide.metformin", "glimepiride.pioglitazone", "metformin.rosiglitazone", "metformin.pioglitazone");
for( c in columns ){
  hospital_discharge_binary[[c]] = as.character(hospital_discharge_binary[[c]])
  
  hospital_discharge_binary[[c]] <- ifelse(hospital_discharge_binary[[c]] == "Up",     10, hospital_discharge_binary[[c]]);
  hospital_discharge_binary[[c]] <- ifelse(hospital_discharge_binary[[c]] == "Down",   -10, hospital_discharge_binary[[c]]);
  hospital_discharge_binary[[c]] <- ifelse(hospital_discharge_binary[[c]] == "Steady", 0, hospital_discharge_binary[[c]]);
  hospital_discharge_binary[[c]] <- ifelse(hospital_discharge_binary[[c]] == "No",     -20, hospital_discharge_binary[[c]]);
  hospital_discharge_binary[[c]] <- as.numeric(hospital_discharge_binary[[c]]);
}









##### Turn the ages into numbers
##### Turn the ages into numbers
##### Turn the ages into numbers
##### Turn the ages into numbers

hospital_discharge_binary$age = as.character(hospital_discharge_binary$age)

hospital_discharge_binary$age = ifelse(hospital_discharge_binary$age=='[0-10)', 0, hospital_discharge_binary$age)
hospital_discharge_binary$age = ifelse(hospital_discharge_binary$age=='[10-20)', 10, hospital_discharge_binary$age)
hospital_discharge_binary$age = ifelse(hospital_discharge_binary$age=='[20-30)', 20, hospital_discharge_binary$age)
hospital_discharge_binary$age = ifelse(hospital_discharge_binary$age=='[30-40)', 30, hospital_discharge_binary$age)
hospital_discharge_binary$age = ifelse(hospital_discharge_binary$age=='[40-50)', 40, hospital_discharge_binary$age)
hospital_discharge_binary$age = ifelse(hospital_discharge_binary$age=='[50-60)', 50, hospital_discharge_binary$age)
hospital_discharge_binary$age = ifelse(hospital_discharge_binary$age=='[60-70)', 60, hospital_discharge_binary$age)
hospital_discharge_binary$age = ifelse(hospital_discharge_binary$age=='[70-80)', 70, hospital_discharge_binary$age)
hospital_discharge_binary$age = ifelse(hospital_discharge_binary$age=='[80-90)', 80, hospital_discharge_binary$age)
hospital_discharge_binary$age = ifelse(hospital_discharge_binary$age=='[90-100)', 90, hospital_discharge_binary$age)

hospital_discharge_binary$age = as.numeric(hospital_discharge_binary$age)


########## Transform glucose serum coulumn
########## Transform glucose serum coulumn
########## Transform glucose serum coulumn
########## Transform glucose serum coulumn
hospital_discharge_binary$max_glu_serum = as.character(hospital_discharge_binary$max_glu_serum)

hospital_discharge_binary$max_glu_serum = ifelse(hospital_discharge_binary$max_glu_serum == "None",  0, hospital_discharge_binary$max_glu_serum);
hospital_discharge_binary$max_glu_serum = ifelse(hospital_discharge_binary$max_glu_serum == "Norm",  100, hospital_discharge_binary$max_glu_serum);
hospital_discharge_binary$max_glu_serum = ifelse(hospital_discharge_binary$max_glu_serum == ">200",  200, hospital_discharge_binary$max_glu_serum);
hospital_discharge_binary$max_glu_serum = ifelse(hospital_discharge_binary$max_glu_serum == ">300",  300, hospital_discharge_binary$max_glu_serum);

hospital_discharge_binary$max_glu_serum = as.numeric(as.character(hospital_discharge_binary$max_glu_serum));


########## Transform A1C column
########## Transform A1C column
########## Transform A1C column
########## Transform A1C column
hospital_discharge_binary$A1Cresult = as.character(hospital_discharge_binary$A1Cresult)

hospital_discharge_binary$A1Cresult <- ifelse(hospital_discharge_binary$A1Cresult == "None",  0, hospital_discharge_binary$A1Cresult);
hospital_discharge_binary$A1Cresult <- ifelse(hospital_discharge_binary$A1Cresult == "Norm",  5, hospital_discharge_binary$A1Cresult);
hospital_discharge_binary$A1Cresult <- ifelse(hospital_discharge_binary$A1Cresult == ">7",    7, hospital_discharge_binary$A1Cresult);
hospital_discharge_binary$A1Cresult <- ifelse(hospital_discharge_binary$A1Cresult == ">8",    8, hospital_discharge_binary$A1Cresult);

hospital_discharge_binary$A1Cresult <- as.numeric(hospital_discharge_binary$A1Cresult);









########## Transform Change & Diabets med
########## Transform Change & Diabets med
########## Transform Change & Diabets med
########## Transform Change & Diabets med

hospital_discharge_binary$change = ifelse(hospital_discharge_binary$change=='No', 0, 1)
hospital_discharge_binary$diabetesMed = ifelse(hospital_discharge_binary$diabetesMed=='No', 0, 1)


########## Transform Gender column
########## Transform Gender column
########## Transform Gender column
########## Transform Gender column

hospital_discharge_binary$gender = ifelse(hospital_discharge_binary$gender=='Male', 1, 0)




### cut data in half
### cut data in half
### cut data in half
### cut data in half
### cut data in half

sample_index = sample(x = dim(hospital_discharge_binary)[1], size = dim(hospital_discharge_binary)[1]*0.5, replace = F)
hospital_discharge_binary = hospital_discharge_binary[sample_index, ]




###################### Set Train/Test datasets
###################### Set Train/Test datasets
###################### Set Train/Test datasets
###################### Set Train/Test datasets
train_index = sample(x = dim(hospital_discharge_binary)[1], size = dim(hospital_discharge_binary)[1]*0.8, replace = F)

train.hospital= hospital_discharge_binary[train_index, ]
test.hospital = hospital_discharge_binary[-train_index, ]






####################-------Decision Trees---------############




#################### SVM
library(e1071)
###### Linear
### Remove: num_medications, max_glu_serumNone, max_glu_serumNorm


#svm.hospital = svm(readmitted~., data=train.hospital, kernel='linear', cost=10)





###### Linear, with Tune



###### Radial


###### Radial with Tune












