load_location_category<-function(){
  library(readr)
  location_category<-read_csv('location_category.csv')
  names(location_category)<-c('district2','category')
  for (var in names(location_category)){
    location_category[,var]<-sapply(location_category[,var],toupper)
  }
  return(location_category)
}

