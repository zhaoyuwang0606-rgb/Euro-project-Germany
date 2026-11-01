# Convert saved World Bank API responses to the original WDI schema.
library(jsonlite)
base <- 'SCM_Germany_Validation/Treatment_1999_Trial'
read_indicator <- function(filename, name) {
  j<-fromJSON(file.path(base,filename))
  stopifnot(j[[1]]$pages==1,nrow(j[[2]])==360)
  z<-j[[2]]
  d<-data.frame(iso2c=z$country$id,year=as.integer(z$date),value=z$value)
  names(d)[3]<-name
  stopifnot(all(is.finite(d[[name]])),!anyDuplicated(d[c('iso2c','year')]))
  d
}
d<-merge(read_indicator('WDI_GDP.json','NY.GDP.MKTP.CD'),read_indicator('WDI_POP.json','SP.POP.TOTL'),by=c('iso2c','year'))
stopifnot(nrow(d)==360)
saveRDS(d,file.path(base,'WDI_raw.rds'))
write.csv(d,file.path(base,'WDI_raw.csv'),row.names=FALSE)
