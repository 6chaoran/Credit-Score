library(plyr)
payment_feature_extraction<-function(folder_path){
current_dir<-getwd()
setwd(folder_path)
payment<-read_csv('payment_history.csv',na=' ',col_types=list(customer_id=col_character(),
                                                       loan_id=col_character(),
                                                       payment_invoice_date=col_date(format='%d-%b-%Y'),
                                                       paument_due_date=col_date(format='%d-%b-%Y'),
                                                       payment_date=col_date(format='%d-%b-%Y')))

#change invoice amount to numeric
payment$invoice_amount<-as.numeric(gsub(',','',payment$invoice_amount))

#translate payment_status
payment_status_en<-c('AKTIF'='ACTIVE','BELUM AKTIF'='YET ACTIVE','DIBAYAR'='PAID',
                     'JATUH TEMPO'='DUE DATE','TERTUNGGAK'='OUTSTANDING')
payment$payment_status<-revalue(payment$payment_status,payment_status_en)

#assigning on-time, delayed, default repayment
payment$delay_in_day<-with(payment,payment_date-paument_due_date)
payment$status<-'not_due'
payment$status[payment$delay_in_day<=0]<-'repayment'
payment$status[payment$delay_in_day>0 & payment$delay_in_day <=30]<-'delayed_payment'
payment$status[payment$delay_in_day >30]<-'default_payment'
require(lubridate)
today<-as.Date(now())
payment$status[is.na(payment$delay_in_day) & (payment$paument_due_date<today)]<-'default_payment'
payment<-cbind(payment,binaryTable(payment,'status'))

# add feature: first_default_payment 
# (from 0 (default in first repayment) to 1 (no default))
first_default_payment<-function(x){
  if (all(x==0)) return (1)
  else return (min(which(x==1))/length(x))
}
x<-aggregate(default_payment~customer_id+loan_id,payment,first_default_payment)
names(x)[3]<-'first_default_payment'
payment<-merge(payment,x,all.x=T)

# add feature: mean delayed in day normalized by tenure
mean_delayed<-function(x){
  x<-as.numeric(x)
  sum_delayed<-sum(x[x>0],na.rm=T)
  return(sum_delayed/length(x))
}
x<-aggregate(delay_in_day~customer_id+loan_id,payment,mean_delayed)
names(x)[3]<-'mean_delayed_in_day'
payment<-merge(payment,x,all.x=T)

# add feature: mean advance_in_day normalized by tenure
mean_advance<-function(x){
  x<-as.numeric(x)
  sum_advance<-abs(sum(x[x<0],na.rm=T))
  return(sum_advance/length(x))
}
x<-aggregate(delay_in_day~customer_id+loan_id,payment,mean_advance)
names(x)[3]<-'mean_advance_in_day'
payment<-merge(payment,x,all.x=T)
# drop all non-feature parameters
features<-c("customer_id","loan_id","default_payment","delayed_payment",
"repayment","first_default_payment","mean_delayed_in_day",'mean_advance_in_day')
payment<-payment[features]

# aggregate by customer_id and loan_id
payment_features<-ddply(payment,c('customer_id','loan_id'),summarise,
         default_payment=sum(default_payment),
         delayed_payment=sum(delayed_payment),
         repayment=sum(repayment),
         first_default_payment=mean(first_default_payment,na.rm=T),
         mean_delayed_in_day=mean(mean_delayed_in_day,na.rm=T),
         mean_advance_in_day=mean(mean_advance_in_day,na.rm=T)
         )
payment_features$mean_delayed_in_day[is.na(payment_features$mean_delayed_in_day)]<-0
payment_features$mean_advance_in_day[is.na(payment_features$mean_advance_in_day)]<-0

setwd(current_dir)

return(payment_features)
}
