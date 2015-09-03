## specify the working directionary
setwd("~/Dropbox/Credit Score")
## define the calibration of grading
calibration<-c('1'='E','2'='A','3'='B','4'='C','5'='D')
## feature extraction threshold (default is 1%)
thres=0.005

#===== loading components =====
source('myFun.R')
source('data_cleaning.R')

source('payment_feature_extraction.R')
source('clustering.R')

#===== data cleaning/feature extraction =====
cat('loading data ... ')
# setting the folder directory of training and testing files
wd<-getwd()
train_folder_path<-file.path(wd,'train_data')
test_folder_path<-file.path(wd,'test_data')

## data cleaning for train and test data
train<-data_cleaning(train_folder_path)
test<-data_cleaning(test_folder_path)

train$mark<-'train'
test$mark<-'test'

## concatenate train,test before extract features
data<-rbind(train,test)

## loading location category file and merge
source('location_category.R')
## convert to upper case
data$district2<-sapply(data$district2,toupper)
## remove the space before the words
data$district2<-gsub('^ ','',data$district2)
loc_cat<-load_location_category()
data<-merge(data,loc_cat,all.x=T)
cat('done!\n')
cat('\nextracting features ... ')

## extracting features
source('feature_extraction.R')
data<-feature_extraction(data,thres)

## split into train and test
train<-data[data$mark=='train',]
test<-data[data$mark=='test',]
train$mark<-NULL
test$mark<-NULL

## removing duplicates in test set 
## (to make sure number of record is same as original test file)
test<-test[!duplicated(test),]

## get payment features from payment history, prepare for clustering analysis
feature_payment<-payment_feature_extraction(train_folder_path)
feature_payment<-merge(train,feature_payment,all.x=T)

##=====define the feature variable name======
payment_variables<-c("default_payment","delayed_payment","repayment","first_default_payment","mean_delayed_in_day","mean_advance_in_day")
demographic_variables<-c("gender","age","type_of_work","village","district1","category")
loan_variables<-c("tenure","principal")

## subset the feature data
train<-cbind(train,feature_payment[payment_variables])
feature_demographic<-train[demographic_variables]
feature_loan<-train[loan_variables]
feature_payment<-train[payment_variables]

cat('done!\n')
cat('feature list: \n')
for (i in names(train)[-c(1,2)]){
  print(i)
}

#===== label grade with clustering ======

cat('\nclustering ... \n')
train$grade<-clustering(feature_payment,calibration)
cat('\nthe calibration used:\n')
print (calibration)
cat('done!\n')

#=====random forest fitting =======
library(randomForest)
cat ('\nfitting random forest... ')
grade<-train$grade
rf_fit3<-randomForest(factor(grade)~.,data=cbind(feature_demographic,feature_loan),ntree=100)
cat ('done!\nfitting result:')
print (rf_fit3)
cat ('\npredicting ... ')
prediction<-predict(rf_fit3,test)
confidence<-apply(predict(rf_fit3,test,type='prob'),1,max)
test$prediction<-prediction
test$confidence<-confidence
write.csv(test,'prediction.csv',row.names=F)
cat ('done!\n')
cat ('the prediction file is located at: ',getwd(),'/predition.csv',sep='')
