feature_extraction<-function(data){
data<-transform(data,age=cal_customer_age(date_of_birth),installment=principal/tenure)

numeric_variable<-c('principal','net_dusburse','interest','tenure','age','installment')
categorical_variable<-c('gender','type_of_work','village','district1','district2')
date_variable<-c('date_of_birth', 'transaction_date', 'maturity_date')
payment_variable<-c('repayment','delayed_payment','default_payment')

for (var in categorical_variable){
  data<-group_categorical(data,var)
}

for (var in numeric_variable){
  data<-impute_numeric(data,var)
}

feature_variable<-c(numeric_variable,categorical_variable)
feature<-data[,feature_variable]
grade<-data$GRADE
grade_data<-cbind(grade,feature)

return(grade_data)
}