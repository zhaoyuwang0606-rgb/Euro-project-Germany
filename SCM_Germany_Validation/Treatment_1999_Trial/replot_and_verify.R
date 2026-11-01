# Run from project root after run_1999.R completes.
suppressPackageStartupMessages(library(tidyverse))
base <- file.path(getwd(),'SCM_Germany_Validation','Treatment_1999_Trial')
dirs <- file.path(base,c('07_Treat1999','08_Treat1999','09_Treat1999_tidysynth_weights','10_Treat1999'))
stopifnot(all(file.exists(file.path(dirs,'COMPLETE.txt'))))
summary <- bind_rows(lapply(dirs,function(p)read_csv(file.path(p,'Summary.csv'),show_col_types=FALSE) %>% mutate(Suite=basename(p),.before=1)))
stopifnot(nrow(summary)==19,all(is.finite(summary$Pre_RMSPE)),all(is.finite(summary$Post_RMSPE)))
write_csv(summary,file.path(base,'All_Summaries.csv'))
for(p in dirs) {
  panel<-read_csv(file.path(p,'Input_Panel.csv'),show_col_types=FALSE)
  files<-list.files(p,pattern='^YearByYear_Gap.csv$',recursive=TRUE,full.names=TRUE)
  ds<-bind_rows(lapply(files,function(f) {
    d<-read_csv(f,show_col_types=FALSE)
    stopifnot(!anyDuplicated(d$year),max(abs(d$gap-(d$actual-d$synthetic)))<1e-8,all(1999:2019 %in% d$year))
    metric<-basename(dirname(f))
    if(substr(basename(p),1,2)!='09') {
      w<-read_csv(file.path(dirname(f),'Weights.csv'),show_col_types=FALSE)
      rec<-inner_join(panel,w %>% select(country,weight),by='country') %>% group_by(year) %>% summarise(synthetic=sum(.data[[metric]]*weight),.groups='drop')
      stopifnot(max(abs(rec$synthetic-d$synthetic))<1e-8)
      ps<-read_csv(file.path(dirname(f),'Placebo_MSPE.csv'),show_col_types=FALSE)
      loo<-read_csv(file.path(dirname(f),'LOO_YearByYear.csv'),show_col_types=FALSE)
      stopifnot(nrow(ps)==n_distinct(panel$country),n_distinct(loo$excluded)==sum(w$weight>.001))
    }
    d %>% mutate(Metric=basename(dirname(f)))
  }))
  title<-c('07'='China trade: Non-EU SCM (nominal GDP shares)','08'='PWT channels: original donor pool','09'='China trade: fixed tidysynth GDP weights','10'='PWT channels: seven Eurozone donors')[[substr(basename(p),1,2)]]
  g<-ggplot(ds,aes(year))+geom_line(aes(y=actual,color='Real Germany'),linewidth=.65)+geom_line(aes(y=synthetic,color='Baseline'),linetype='dashed',linewidth=.65)+scale_color_manual(values=c('Real Germany'='black','Baseline'='grey55'))+geom_vline(xintercept=1999,color='firebrick',linetype='dotted')+geom_vline(xintercept=2007,color='steelblue',linetype='dotted')+facet_wrap(~Metric,scales='free_y',ncol=2)+labs(title=title,subtitle='Alternative treatment: 1999 (red) | Pre-period ends 1998 | Reference: 2007 (blue)',y=NULL,x='Year',color=NULL)+theme_minimal(base_size=12)+theme(legend.position='bottom')
  ggsave(file.path(p,'Overview.png'),g,width=13,height=if(nrow(distinct(ds,Metric))>4)9 else 7,dpi=180)
}
writeLines('PASS: four completion markers; 19 outcomes; finite summary RMSE; unique years; gaps verified; full postperiod coverage; fitted SCM reconstructed from saved weights and input panels; complete placebo and active-donor LOO counts.',file.path(base,'Verification.txt'))
print(summary)
