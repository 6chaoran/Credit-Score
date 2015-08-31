## set the working directory
data_cleaning<-function(){
library(readr)
library(plyr)
# 1. CUSTOMER TAB
## loading data from customer tab
customer<-read_csv('customer.csv',,na=' ',
                   col_types = list(NO_IDENTITAS=col_character(),
                                    customer_id=col_character(),
                                    RT=col_character(),
                                    RW=col_character(),
                                    KODE_POS=col_character()))

## rename variables
names(customer)<-c('customer_id','customer_type','date_of_birth','customer_card_id',
                   'gender','religion','IC','type_of_work','address','RT',
                   'RW','postal_code','village','district1','district2')

customer<-transform(customer,date_of_birth=as.Date(date_of_birth,format='%d-%B-%Y'))


# 2. ORIGINAL TAB
## loading the original data from original tab
original<-read_csv('original.csv',na=' ',col_types=list(customer_id=col_character(),
                                                transaction_date=col_character()))


### remove single/too many level variable
drop_variable<-c('borrower_performance','collateral_type','LTV','description')
for (i in drop_variable){
  original[i]<-NULL
}

## transform to date format
original<-transform(original,transaction_date=as.Date(transaction_date,format='%d-%B-%Y'),
                maturity_date=as.Date(maturity_date,format='%d-%B-%Y'))

## transform principal, net_dusburse into number
original$principal<-as.numeric(gsub(',','',original$principal))
original$net_dusburse<-as.numeric(gsub(',','',original$net_dusburse))
original$total_remaining_payment<-as.numeric(gsub(',','',original$total_remaining_payment))

## rename interest
colnames(original)[7]<-"interest"

## original data left merge with customer
data<-merge(original,customer,by='customer_id',all.x=T)

## translate type_of_work:
type_of_work_en<-c("IBU RUMAH TANGGA"='HOUSEWIFE',
  "LAIN-LAIN"='OTHERS',
  "PETANI/NELAYAN"="FARMER/FISHMAN",
  "SWASTA"="PRIVATE",
  "TNI/POLRI"="POLICE",
  "WIRASWASTA"="ENTREPRENEUR")
gender_en<-c('PRIA'='MALE','WANITA'='FEMALE')
data$type_of_work<-revalue(data$type_of_work,type_of_work_en)
data$gender<-revalue(data$gender,gender_en)

## drop all non-used columns:
numeric_variable<-c('principal','net_dusburse','interest','tenure')
categorical_variable<-c('customer_type','gender','religion','type_of_work','village',
                        'RT','RW','district1','district2')
date_variable<-c('date_of_birth', 'transaction_date', 'maturity_date')
payment_variable<-c('repayment','delayed_payment','default_payment','total_remaining_payment')
response<-c('SCORE','GRADE')
data<-data[,c(numeric_variable,categorical_variable,payment_variable,response,date_variable)]

return(data)
}
