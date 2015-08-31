## chekcing data class,levels,completion 
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

## stack data
stack_data=function(df1,df2){
  n=ncol(df2)
  name=colnames(df1)
  df1=rep(df1,n)
  colnames(df1)=name
  df2=stack(df2)
  return (cbind(df1,df2))
}

## Frequency Plot
FreqPlot<-function(df,col,top=20,fill='#33CCFF'){
  data=data.frame(table(df[col]))
  data=data[order(data$Freq,decreasing=T),]
  top=min(c(top,nrow(data)))
  data=data[1:top,]
  data=transform(data,Var1=reorder(strtrim(Var1,12),Freq))
  require(ggplot2)
  return (ggplot(data,aes(Var1,Freq))+geom_bar(stat='identity',fill=fill)+coord_flip()+theme_bw(base_size=18)+
    labs(title=col,x='',y='')+
    geom_text(aes(y=Freq,ymax=1.08*max(Freq),label=paste0(round(Freq/nrow(data),3)*100,'%'),hjust=-0.1)))
}

## Density Plot
DensityPlot<-function(df,col,by){
  require(ggplot2)
  expr<-paste0('ggplot(',df,',aes(',col,',fill=',by,'))+geom_density(alpha=0.8)+theme_bw()')
  plot<-eval(parse(text=expr))
  return(plot)
}

## SummarizeScoreBy
SummarizeScoreBy<-function(df,col){
  x<-ddply(df,col,summarise,count=length(score),
           mean=mean(score),median=median(score))
  x<-x[order(x$mean,decreasing = T),]
  return(x)
}

# Convert to Binary Table
binaryTable<-function(df,col){
  label<-unique(df[col])[,1]
  expr<-paste0('model.matrix(~',col,'+0,df)')
  binaryTable<-eval(parse(text=expr))
  colnames(binaryTable)<-label
  return (binaryTable)
}

library(lubridate)
## calculate customer age
cal_customer_age<-function(df){
  this_year<-as.integer(strsplit(date()," ")[[1]][5])
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
impute_numeric<-function(df,col){
  df[,col][is.na(df[,col])]<-median(df[,col],na.rm=T)
  return (df)
}

## score to grade
score_to_grade<-function(x){
  cutoff<-c(-Inf,-5,0,10,20,Inf)
  grades<-c('E','D','C','B','A')
  y<-cut(x,cutoff,label=grades)
  return (y)
}
