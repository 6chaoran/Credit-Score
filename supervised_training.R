source('~/Dropbox/Credit Score/myFun.R')
source('~/Dropbox/Credit Score/data_cleaning.R')
source('~/Dropbox/Credit Score/feature_extraction.R')

setwd("~/Dropbox/Credit Score")

#===== data cleaning/feature extraction =====
data<-data_cleaning()
feature_data<-feature_extraction(data)

#===== label grade with clustering ======
source('~/Dropbox/Credit Score/kmeans.R')
cat('clustering analysis segmentate customers into 5 groups:\n')
feature_data$grade<-clustering(data)
cat('\n')

#===== decision tree fitting =======
require(caret)
inTrain<-createDataPartition(feature_data$grade,p=0.8,list=F)
train<-feature_data[inTrain,]
test<-feature_data[-inTrain,]
library(rpart)
#control=trainControl(method='cv',number=10)
#decision_tree_cluster<-train(factor(cluster)~.,data=train_cluster,method='rpart',trControl = control)

dt_fit<-rpart(factor(grade)~.,data=train,cp=0.013)
x<-predict(dt_fit,test,type='class')
dt_performance<-confusionMatrix(x,test$grade)
cat ('\nDecision Tree Model Performance:\n')
print (dt_performance)
cat ('\n ')

library(rattle)
fancyRpartPlot(dt_fit)

#=====random forest fitting =======
library(randomForest)
cat ('fitting random forest...\n')
rf_fit<-randomForest(factor(grade)~.,data=train,ntree=100)
print ('fitting is done.\n')
x<-predict(rf_fit,test)
rf_performance<-confusionMatrix(x,test$grade)

cat ('Random Forest Model Performance:\n')
print (rf_performance)
cat ('\n')
