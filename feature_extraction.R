feature_extraction<-function(data,thres=0.01){
data<-transform(data,age=cal_customer_age(date_of_birth))

numeric_variable<-c('principal','tenure','age')
categorical_variable<-c('gender','type_of_work','village','district1','district2','category')

for (var in categorical_variable){
  data<-group_categorical(data,var,thres)
}

for (var in numeric_variable){
  data<-impute_numeric(data,var)
}

index<-c('customer_id','loan_id')
feature_variable<-c(index,numeric_variable,categorical_variable)
if ('mark' %in% names(data)) feature_variable<-c(feature_variable,'mark')
feature<-data[,feature_variable]

return(feature)
}