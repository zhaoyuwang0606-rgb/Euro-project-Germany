# Run from project root: Rscript SCM_Germany_Validation/Treatment_1999_Trial/run_1999.R [07|08|09|10|all]
# Original data preparation is reused via an explicit assignment allowlist.
suppressPackageStartupMessages({library(tidyverse); library(Synth); library(readxl); library(WDI); library(pwt10)})
set.seed(1999)
root <- normalizePath(getwd(), winslash='/')
base <- file.path(root, 'SCM_Germany_Validation')
out <- file.path(base, 'Treatment_1999_Trial')
dir.create(out, recursive=TRUE, showWarnings=FALSE)
args <- commandArgs(TRUE); selected <- if(length(args)) args[1] else 'all'
sources <- c('07'='07_China_WTO_Trade_Suite/run_china_wto_trade_scm.R', '08'='08_PWT_Channel_Treat_2003_Suite/run_full_pwt_2003_suite.R', '09'='09_GDPpc_Weighted_China_WTO_Trade_Suite/run_gdppc_weighted_trade_scm.R', '10'='10_Eurozone_PWT_Channel_Treat_2003_Suite/run_exact7_eurozone_pwt_2003_suite.R')
prepare <- function(code, allowed, env=new.env(parent=globalenv())) {
  for(ex in parse(file.path(base, sources[[code]]))) {
    if(is.call(ex) && identical(ex[[1]], as.name('<-')) && as.character(ex[[2]]) %in% allowed) eval(ex, env)
  }
  env
}
stats <- function(d) tibble(Pre_RMSPE=sqrt(mean(d$gap[d$year<1999]^2)), Post_RMSPE=sqrt(mean(d$gap[d$year>=1999]^2)), Post_Mean_Gap=mean(d$gap[d$year>=1999]), Mean_Gap_1999_2007=mean(d$gap[d$year>=1999 & d$year<=2007]), Gap_2007=d$gap[d$year==2007], MSPE_Ratio=mean(d$gap[d$year>=1999]^2)/mean(d$gap[d$year<1999]^2))
marks <- function() list(geom_vline(xintercept=1999, color='firebrick', linetype='dotted'),geom_vline(xintercept=2007,color='steelblue',linetype='dotted'))
plot_result <- function(d, folder, title, loo=NULL) {
  subtitle <- sprintf('Alternative treatment: 1999 | Pre: %d-1998 | Post: 1999-2019 | Reference: 2007',min(d$year))
  p <- ggplot(d,aes(year)) + geom_line(aes(y=actual,color='Real Germany'),linewidth=.8) + geom_line(aes(y=synthetic,color='Baseline'),linetype='dashed',linewidth=.8) + scale_color_manual(values=c('Real Germany'='black','Baseline'='grey55')) + marks() + labs(title=title,subtitle=subtitle,y='Outcome',x='Year',color=NULL) + theme_minimal(base_size=11) + theme(legend.position='bottom')
  ggsave(file.path(folder,'SCM_Trends.png'),p,width=12,height=5,dpi=180)
  q <- ggplot(d,aes(year,gap))+geom_line(color='black')+geom_hline(yintercept=0,linetype='dashed')+marks()+labs(title=paste(title,'- gap'),subtitle=subtitle,y='Actual - baseline')+theme_minimal(base_size=11)
  ggsave(file.path(folder,'SCM_Gap.png'),q,width=12,height=5,dpi=180)
  if(!is.null(loo) && nrow(loo)) {
    z <- ggplot(loo,aes(year,synthetic,group=excluded,color=excluded))+geom_line(alpha=.65,linewidth=.5)+geom_line(data=d,aes(year,actual),inherit.aes=FALSE,color='black',linewidth=1)+geom_line(data=d,aes(year,synthetic),inherit.aes=FALSE,color='grey55',linetype='dashed',linewidth=1)+marks()+labs(title=paste(title,'- leave one out'),subtitle=paste(subtitle,'\nBlack: real Germany | Grey dashed: baseline'),y='Outcome',color='Excluded donor')+theme_minimal(base_size=11)+theme(legend.position='bottom')
    ggsave(file.path(folder,'SCM_LOO.png'),z,width=12,height=6,dpi=180)
  }
}
run_suite <- function(code,panel,predictors,years,blocks,metrics,donor_only_placebo=FALSE) {
  folder <- file.path(out,paste0(code,'_Treat1999')); dir.create(folder,recursive=TRUE,showWarnings=FALSE)
  panel <- panel %>% select(-any_of('unit_num')) %>% arrange(country,year)
  map <- tibble(country=sort(unique(panel$country)), unit_num=seq_along(sort(unique(panel$country))))
  panel <- left_join(panel,map,by='country')
  stopifnot(!anyDuplicated(panel[c('country','year')]), all(is.finite(as.matrix(panel[c(predictors,metrics)]))))
  write_csv(panel,file.path(folder,'Input_Panel.csv'))
  germany <- map$unit_num[map$country=='Germany']; donors <- setdiff(map$unit_num,germany)
  fit <- function(tr,controls,metric) {
    capture.output({dp <- dataprep(foo=as.data.frame(panel), predictors=predictors,predictors.op='mean',time.predictors.prior=years[years<1999],special.predictors=lapply(blocks,function(b)list(metric,b,'mean')),dependent=metric,unit.variable='unit_num',unit.names.variable='country',time.variable='year',treatment.identifier=tr,controls.identifier=sort(controls),time.optimize.ssr=years[years<1999],time.plot=years); so <- synth(dp,method='BFGS')})
    w <- tibble(unit_num=as.integer(rownames(so$solution.w)),weight=as.numeric(so$solution.w)) %>% left_join(map,by='unit_num')
    stopifnot(all(is.finite(so$solution.w)),abs(sum(w$weight)-1)<1e-5,all(w$weight>=-1e-8))
    d <- tibble(year=years,actual=as.numeric(dp$Y1plot),synthetic=as.numeric(dp$Y0plot %*% so$solution.w)) %>% mutate(gap=actual-synthetic)
    list(data=d,weights=w,balance=tibble(variable=rownames(dp$X0),actual=as.numeric(dp$X1),synthetic=as.numeric(dp$X0 %*% so$solution.w),donor_mean=rowMeans(dp$X0)))
  }
  summaries <- list()
  for(metric in metrics) {
    cat(code,metric,'baseline/placebos/LOO\n'); flush.console()
    dest <- file.path(folder,metric);dir.create(dest,showWarnings=FALSE)
    main <- fit(germany,donors,metric)
    write_csv(main$data,file.path(dest,'YearByYear_Gap.csv'));write_csv(main$weights,file.path(dest,'Weights.csv'));write_csv(main$balance,file.path(dest,'Balance.csv'))
    placebo <- list(Germany=main$data)
    for(u in donors) {cat(' placebo',map$country[map$unit_num==u],'\n');flush.console();placebo[[map$country[map$unit_num==u]]] <- fit(u,setdiff(if(donor_only_placebo) donors else map$unit_num,u),metric)$data}
    pd <- bind_rows(placebo,.id='country');write_csv(pd,file.path(dest,'Placebo_Gaps.csv'))
    ps <- bind_rows(lapply(names(placebo),function(n)bind_cols(tibble(country=n),stats(placebo[[n]]))))
    write_csv(ps,file.path(dest,'Placebo_MSPE.csv'))
    s <- stats(main$data) %>% mutate(Metric=metric,Placebo_P_Value=mean(ps$MSPE_Ratio>=MSPE_Ratio),Placebo_N=nrow(ps))
    summaries[[metric]] <- s
    ldata <- list(); lw <- list()
    for(u in main$weights$unit_num[main$weights$weight>.001]) {n <- map$country[map$unit_num==u];cat(' LOO',n,'\n');flush.console();f<-fit(germany,setdiff(donors,u),metric);ldata[[n]]<-f$data;lw[[n]]<-f$weights}
    ld <- bind_rows(ldata,.id='excluded');write_csv(ld,file.path(dest,'LOO_YearByYear.csv'));write_csv(bind_rows(lw,.id='excluded'),file.path(dest,'LOO_Weights.csv'))
    plot_result(main$data,dest,paste(code,metric),ld)
    pg <- ggplot(filter(pd,country!='Germany'),aes(year,gap,group=country))+geom_line(color='grey75')+geom_line(data=main$data,aes(year,gap),inherit.aes=FALSE,color='black',linewidth=.9)+marks()+theme_minimal()+labs(title=paste(code,metric,'- placebo gaps'),subtitle='Alternative treatment 1999; Germany black; all donor placebos shown')
    ggsave(file.path(dest,'SCM_Placebo.png'),pg,width=12,height=5,dpi=180)
    saveRDS(list(baseline=main,placebos=placebo,loo=ldata),file.path(dest,'Fits.rds'))
    write_csv(bind_rows(summaries),file.path(folder,'Summary.csv'))
  }
  writeLines('Complete: baseline, all specified placebos, active-donor LOO; no failed fits skipped.',file.path(folder,'COMPLETE.txt'))
}
if(selected %in% c('08','all')) {
  e<-new.env(parent=globalenv());e$raw_hybrid<-read_csv(file.path(base,'Hybrid_MPD_PWT_Panel.csv'),show_col_types=FALSE)
  e<-prepare('08',c('clean_countries','panel','unit_map'),e)
  run_suite('08',e$panel,c('gdppc_mpd','gdp_size','pop_mpd'),1970:2019,list(1980:1986,1987:1993,1994:1998),c('csh_i','csh_x','csh_m','csh_c','csh_g'))
}
if(selected %in% c('10','all')) {
  data('pwt10.01');e<-prepare('10',c('target_countries','panel_data'))
  run_suite('10',e$panel_data,c('gdppc','gdp_size','pop_size'),1970:2019,list(1980:1986,1987:1993,1994:1998),c('csh_i','csh_x','csh_m','csh_c','csh_g'))
}
if(selected %in% c('09','all')) {
  e<-prepare('09',c('weights_path_1','raw_w1','country_map','mpd_iso_map','w_df1','mpd_path','mpd_raw','mpd_sub','trade_fpath','dots_raw','clean_trade','panel'))
  folder<-file.path(out,'09_Treat1999_tidysynth_weights');dir.create(folder,showWarnings=FALSE)
  write_csv(e$panel,file.path(folder,'Input_Panel.csv'));write_csv(e$w_df1,file.path(folder,'Used_Weights.csv'))
  ss<-list()
  for(m in c('exp_pct_gdp','imp_pct_gdp','trade_pct_gdp','exp','imp','total_mn')) {
    d<-e$panel %>% filter(country=='Germany') %>% arrange(year) %>% transmute(year,actual=.data[[m]])
    sy<-e$panel %>% filter(country!='Germany') %>% left_join(select(e$w_df1,trade_country,norm_weight),by=c('country'='trade_country')) %>% group_by(year) %>% summarise(synthetic=sum(.data[[m]]*norm_weight),.groups='drop')
    d<-left_join(d,sy,by='year') %>% mutate(gap=actual-synthetic);stopifnot(all(is.finite(as.matrix(d))))
    dest<-file.path(folder,m);dir.create(dest,showWarnings=FALSE);write_csv(d,file.path(dest,'YearByYear_Gap.csv'));plot_result(d,dest,paste('09 fixed tidysynth GDP weights:',m));ss[[m]]<-bind_cols(tibble(Metric=m),stats(d))
  }
  write_csv(bind_rows(ss),file.path(folder,'Summary.csv'));writeLines('Complete: six outcomes; original GDP tidysynth weights, Taiwan excluded and remaining weights renormalized. GDP denominator is Maddison real PPP size, not nominal GDP.',file.path(folder,'COMPLETE.txt'))
}
if(selected %in% c('07','all')) {
  cache<-file.path(out,'WDI_raw.rds');e<-new.env(parent=globalenv())
  if(file.exists(cache)) e$wdi_raw<-readRDS(cache)
  allowed<-c('clean_donors','all_countries','iso_map','fetch_wdi_with_retry',if(!file.exists(cache)) 'wdi_raw','wdi_sub','fpath','dots_raw','clean_trade_data','trade_all','panel','unit_map')
  e<-prepare('07',allowed,e);saveRDS(e$wdi_raw,cache)
  run_suite('07',e$panel,c('nom_gdppc_usd','pop_mn','nom_gdp_usd_mn'),1980:2019,list(1980:1986,1987:1992,1993:1998),c('exp_pct_gdp','imp_pct_gdp','trade_pct_gdp'),TRUE)
}
writeLines(capture.output(sessionInfo()),file.path(out,paste0('SessionInfo_',selected,'.txt')))
cat('COMPLETED',selected,'\n')
