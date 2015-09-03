## This code works for clustering calibration and random Forest model benchmarking.

## specify the working directionary, where the R codes and train/test file folder place
setwd("~/Dropbox/Credit Score")
## need to re-calibrate after using a new set of training data
calibration<-c('1'='E','2'='A','3'='B','4'='C','5'='D')


wd<-getwd()
## loading functions
source('myFun.R')

#===== data cleaning/feature extraction =====
cat('loading data ... ')
## locate the train/test data folder
train_folder_path<-file.path(wd,'train_data')
test_folder_path<-file.path(wd,'test_data')

## data cleaning for train and test data
source('data_cleaning.R')
train<-data_cleaning(train_folder_path)

cat('done!\n')
cat('\nextracting features ... ')

## transform data into features

## loading location category file and merge
source('location_category.R')
## convert to upper case
train$district2<-sapply(train$district2,toupper)
## remove the space before the words
train$district2<-gsub('^ ','',train$district2)
loc_cat<-load_location_category()
train<-merge(train,loc_cat,all.x=T)

source('feature_extraction.R')
train<-feature_extraction(train)

## get payment features from payment history
source('payment_feature_extraction.R')
feature_payment<-payment_feature_extraction(train_folder_path)

## left join to train set to add payment features
feature_payment<-merge(train,feature_payment,all.x=T)
payment_variables<-c("default_payment","delayed_payment","repayment","first_default_payment","mean_delayed_in_day",'mean_advance_in_day')
train<-cbind(train,feature_payment[payment_variables])

## define feature variables name
demographic_variables<-c("gender","age","type_of_work","village","district1","district2","category")
loan_variables<-c("tenure","principal")
cat('done!\n')
cat('feature list: \n')
for (i in names(train)[-c(1,2)]){
  print(i)
}

#===== label grade with clustering ======
## loading clustering functions
source('clustering.R')
cat('\nclustering ... \n')
feature_payment<-train[payment_variables]

## doing clustering and assign the calibrated grade to the clusters
train$grade<-clustering(feature_payment,calibration)
cat('\nthe calibration used:\n')
print (calibration)
cat('done!\n')

#=====random forest fitting =======
## loading caret machine learning package
library(caret)
data<-train

## randomly splitting into train (80%) and test (20%)
inTrain<-createDataPartition(data$grade,p=0.8,list=F)
train<-data[inTrain,]
test<-data[-inTrain,]

## prepare demographic and loan features for train data 
feature_demographic<-train[demographic_variables]
feature_loan<-train[loan_variables]

## loading random forest package
library(randomForest)
cat ('\nfitting random forest... ')
grade<-train$grade

## training with random Forest using grade over all demographic and loan features
rf_fit3<-randomForest(factor(grade)~.,data=cbind(feature_demographic,feature_loan),ntree=100)
cat ('done!\nfitting result:')
print (rf_fit3)
cat ('\npredicting ... ')

## prediction
prediction<-predict(rf_fit3,test)
## calculate the confusionMatrix
rf_performance<-confusionMatrix(prediction,test$grade)
print (rf_performance)
