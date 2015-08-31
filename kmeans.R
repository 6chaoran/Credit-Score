## Clustering Analysis
### Clutering the customer payment
clustering<-function(data){
payment_variable<-c("repayment","delayed_payment","default_payment")
payment<-data[,payment_variable]
for (var in payment_variable){
  payment[,var][is.na(payment[,var])]<-0
}
### normalize payment based on tenure
row_sum<-apply(payment,1,sum)
row_sum[row_sum==0]<-1
for (var in payment_variable){
  payment[,var]<-payment[,var]/row_sum*100
}

### kmeans to find 5 groups
set.seed(999)
groups<-kmeans(payment,5)
print(groups$centers)
cat('\ngood-of-fittness: ',groups$betweenss/groups$totss)
require(plyr)
cluster_to_grade<-c('1'='E','2'='B','3'='C','4'='A','5'='D')
grade<-revalue(as.character(groups$cluster),cluster_to_grade)
return(grade)
#data$cluster<-groups$cluster
#ddply(data,'cluster',summarise,mean=mean(SCORE),
#      count=length(SCORE),median=median(SCORE))
#ggplot(data,aes(SCORE,fill=as.factor(cluster)),)+geom_density(alpha=0.8)+theme_bw()
}


