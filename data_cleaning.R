## set the working directory
data_cleaning<-function(fold_path){
library(readr)
library(plyr)
current_dir<-getwd()
setwd(fold_path)
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

customer<-customer[c('customer_id','date_of_birth','gender','type_of_work','village','district1','district2')]

# 2. ORIGINAL TAB
## loading the original data from original tab
original<-read_csv('original.csv',na=' ',col_types=list(customer_id=col_character(),
                                                transaction_date=col_character()))


### remove single/too many level variable
drop_variable<-c('borrower_performance','collateral_type','LTV','description')
for (i in drop_variable){
  original[i]<-NULL
}

## transform principal, net_dusburse into number
original$principal<-as.numeric(gsub(',','',original$principal))

## select feature principal and tenure only
original<-original[c('customer_id','loan_id','principal','tenure')]

## original data left merge with customer
data<-merge(original,customer,by='customer_id',all.x=T)

setwd(current_dir)

return(data)
}
