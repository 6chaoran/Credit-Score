## Clustering Analysis
### Clutering the customer payment
clustering<-function(data,calibration){
normalize_variable<-c("repayment","delayed_payment","default_payment")
normalize_payment<-data[normalize_variable]
### normalize payment based on tenure
row_sum<-apply(normalize_payment,1,sum)
row_sum[row_sum==0]<-1
for (var in normalize_variable){
  data[,var]<-data[,var]/row_sum
}

### kmeans to find 5 groups
set.seed(999)
groups<-kmeans(data,5)
centers<-data.frame(groups$centers)
size<-groups$size
print(cbind(centers,size))
cat('good-of-fittness: ',groups$betweenss/groups$totss)
require(plyr)
grade<-revalue(as.character(groups$cluster),calibration)
return(grade)

}


