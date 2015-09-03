## functions to check data class,levels,completion 
info=function(df){
	df=data.frame(df)
	level=function(x) length(unique(x))
	missing=function(x) paste0(round(sum(!is.na(x))/length(x),4)*100,'%')
	df1=data.frame(
		class=sapply(df,function(x) class(x)[[1]]),
		level=sapply(df,level),
		complete=sapply(df,missing)
		)
	return (df1)
}

# Convert to Binary Table
binaryTable<-function(df,col){
  label<-unique(df[col])
  label<-label[!is.na(label)]
  label<-label[order(label)]
  expr<-paste0('model.matrix(~',col,'+0,df)')
  binaryTable<-eval(parse(text=expr))
  colnames(binaryTable)<-label
  return (binaryTable)
}

## calculate customer age
require(lubridate)
cal_customer_age<-function(df){
  this_year<-year(now())
  df<-this_year-year(df)
  return (df)
}

## clean categorical data 
## less than 1% occurrance to be 'others'
group_categorical<-function(df,col,thres=0.01){
  if (class(df[,col])=='character') {
    df[,col]<-sapply(df[,col],toupper)
  }
  freq.table<-table(df[col])
  trivial_variable<-row.names(freq.table)[freq.table<thres*nrow(df)]
  top1<-row.names(freq.table)[freq.table==max(freq.table)]
  # levels > 10, assign trivial groups to OTHERS
  nlevel<- length(unique(df[,col]))
  if (nlevel>10){
    df[,col][df[,col] %in% trivial_variable]<-'OTHERS'
  }
  df[,col][is.na(df[,col])]<-top1
  df[,col]<-factor(df[,col])
  return (df)
}

## clean numeric data
## replace missing value with median
impute_numeric<-function(df,col){
  df[,col][is.na(df[,col])]<-median(df[,col],na.rm=T)
  return (df)
}
