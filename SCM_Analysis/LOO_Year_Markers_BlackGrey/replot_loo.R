# Sources: the six original folders WITHOUT the _Synth suffix.
# Baseline uses saved weights; LOO uses each original script's fitting function.
# Subsequent runs reuse saved plotting data. Pass --refit-loo to refit LOO.
suppressPackageStartupMessages({library(tidyverse); library(readxl); library(tidysynth)})
arg <- grep("^--file=", commandArgs(FALSE), value=TRUE)
out <- if(length(arg)) dirname(normalizePath(sub("^--file=", "", arg[1]))) else normalizePath(getwd())
root <- dirname(out)
refit <- "--refit-loo" %in% commandArgs(TRUE)
specs <- tibble(
  folder=c("SCM_Results wide panel", "noneu SCM result", "western SCM result",
           "TimeRob_noneu_Start1950_Treat1999", "TimeRob_noneu_Start1970_Treat1990", "TimeRob_noneu_Start1970_Treat2002"),
  id=c("Wide_Pre1970_1998", "NonEU_Pre1970_1998", "Western_Pre1970_1998",
       "NonEU_Start1950_Treat1999", "NonEU_Start1970_Treat1990", "NonEU_Start1970_Treat2002"),
  title=c("Wide panel", "Non-EU panel", "Western panel", "Non-EU: start 1950", "Non-EU: treatment 1990", "Non-EU: treatment 2002"),
  start=c(1970,1970,1970,1950,1970,1970), treatment=c(1999,1999,1999,1999,1990,2002),
  script=c("run_scm wide panel.R", "run_scm_noneu.R", "run_scm_western.R", NA,NA,NA))
summaries <- list()
for(i in seq_len(nrow(specs))) {
  s <- specs[i,]; cat("\nSPECIFICATION:",s$id,"\n"); flush.console()
  cached <- file.path(out,paste0(s$id,"_PlotData.rds"))
  if(file.exists(cached) && !refit) {
    dat <- readRDS(cached)
  } else {
    env <- new.env(parent=globalenv())
    if(i<=3) {
      script <- file.path(root,s$folder,s$script)
      oldwd <- getwd(); setwd(dirname(script))
      # Evaluate only original imports, preparation, and the fitting function.
      for(ex in parse(script, encoding="UTF-8")) {
        if(is.call(ex) && identical(ex[[1]],as.name("<-")) && identical(ex[[2]],as.name("baseline_scm"))) break
        eval(ex,env)
      }
      setwd(oldwd)
      df <- env$df_clean
      fit_loo <- env$run_my_scm
    } else {
      script <- file.path(root,"run_time_robustness_noneu.R")
      oldwd <- getwd(); setwd(root)
      for(ex in parse(script, encoding="UTF-8")) {
        if(is.call(ex) && identical(ex[[1]],as.name("run_experiment"))) break
        eval(ex,env)
      }
      setwd(oldwd)
      env$start_year <- s$start; env$treat_year <- s$treatment
      env$pre_window <- s$start:(s$treatment-1)
      # Extract the original nested run_my_scm definition, without running its outputs.
      body_expr <- as.list(body(env$run_experiment))[-1]
      for(ex in body_expr) {
        if(is.call(ex) && identical(ex[[1]],as.name("<-")) && identical(ex[[2]],as.name("run_my_scm"))) eval(ex,env)
      }
      fit_loo <- env$run_my_scm
      df <- env$raw_data %>%
        filter(country %in% c(env$treated_country,env$donor_countries), !country %in% env$spillover_and_japan,
               year>=s$start,year<=2022) %>%
        select(country,year,gdppc,pop) %>% mutate(gdp_size=gdppc*pop) %>% drop_na() %>%
        group_by(country) %>% filter(n()==length(s$start:2022)) %>% ungroup()
    }
    weights <- read_csv(file.path(root,s$folder,"SCM_Weights.csv"),show_col_types=FALSE)
    stopifnot(abs(sum(weights$weight)-1)<1e-5, all(weights$unit %in% df$country))
    baseline <- df %>% inner_join(weights,by=c("country"="unit")) %>% group_by(year) %>%
      summarise(value=sum(gdppc*weight),.groups="drop") %>% mutate(type="Baseline Synthetic") %>% rename(time_unit=year)
    actual <- df %>% filter(country=="Germany") %>% transmute(time_unit=year,value=gdppc,type="Real Germany")
    active <- weights %>% filter(weight>.01) %>% pull(unit)
    loo <- lapply(active,function(donor) {
      cat("  LOO: Drop",donor,"\n"); flush.console()
      fit_loo(df,exclude_country=donor) %>% grab_synthetic_control() %>%
        transmute(time_unit,value=synth_y,type=paste("Drop",donor))
    })
    dat <- bind_rows(loo, list(baseline,actual))
    stopifnot(all(is.finite(dat$value)),nrow(dat)==(length(active)+2)*(2022-s$start+1))
    saveRDS(dat,cached)
    write_csv(dat,file.path(out,paste0(s$id,"_LOO_YearByYear.csv")))
  }
  types <- unique(dat$type)
  cols <- setNames(scales::hue_pal()(length(types)),types)
  cols["Real Germany"] <- "black"; cols["Baseline Synthetic"] <- "#808080"
  ltypes <- setNames(rep("solid",length(types)),types); ltypes["Baseline Synthetic"] <- "dashed"
  widths <- setNames(rep(.65,length(types)),types); widths["Real Germany"] <- 1.15; widths["Baseline Synthetic"] <- .9
  # Draw actual and baseline last to make them legible where paths overlap.
  p <- ggplot(dat,aes(time_unit,value,color=type,linetype=type,linewidth=type)) +
    geom_line(data=filter(dat,!type %in% c("Real Germany","Baseline Synthetic"))) +
    geom_line(data=filter(dat,type=="Baseline Synthetic")) + geom_line(data=filter(dat,type=="Real Germany")) +
    geom_vline(xintercept=s$treatment,linetype="dashed",color="#C62828",linewidth=.6) +
    geom_vline(xintercept=2007,linetype="dashed",color="#2474B5",linewidth=.6) +
    annotate("text",x=s$treatment-.5,y=Inf,label=paste("Treatment",s$treatment),hjust=1,vjust=1.6,color="#C62828",size=3.4) +
    annotate("text",x=2007+.5,y=Inf,label="2007",hjust=0,vjust=1.6,color="#2474B5",size=3.4) +
    scale_color_manual(values=cols)+scale_linetype_manual(values=ltypes)+scale_linewidth_manual(values=widths)+
    scale_y_continuous(expand=expansion(mult=c(.04,.13)))+
    labs(title=paste("Leave-One-Out:",s$title),subtitle=sprintf("Pre-treatment: %d-%d | Original tidysynth specification",s$start,s$treatment-1),
         x="Year",y="GDP per capita",color="Model",linetype="Model",linewidth="Model")+
    theme_minimal(base_size=11)+theme(legend.position="right",panel.grid.minor=element_blank())
  ggsave(file.path(out,paste0(s$id,"_LOO_2007.png")),p,width=11,height=6.5,dpi=300,bg="white")
  base <- dat %>% filter(type %in% c("Real Germany","Baseline Synthetic")) %>%
    pivot_wider(names_from=type,values_from=value) %>%
    mutate(gap=`Real Germany`-`Baseline Synthetic`,pct_gap=100*gap/`Baseline Synthetic`)
  post <- base %>% filter(time_unit>=s$treatment,time_unit<=2007)
  summaries[[i]] <- tibble(Specification=s$id,Source=s$folder,Start=s$treatment,End=2007,N=nrow(post),
    Average_Gap=mean(post$gap),Average_Pct_Gap=mean(post$pct_gap),Gap_2007=base$gap[base$time_unit==2007],Pct_Gap_2007=base$pct_gap[base$time_unit==2007])
  cat("SAVED:",s$id,"\n");flush.console()
}
write_csv(bind_rows(summaries),file.path(out,"Baseline_Gaps_to_2007.csv"))
write_csv(specs %>% select(id,folder,start,treatment),file.path(out,"Sources.csv"))
cat("All six tidysynth figures completed.\n")
