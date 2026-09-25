##Transforming/preparing data
format_data_raleigh <- function(data, facet_var, fstart_hour=10, lon_hour=7){
  points_d<-data%>%
    mutate(bout_id=sample(1:nrow(data)))%>%
    pivot_longer(cols=c(start,stop),names_to="start_stop", values_to = "start_stop_aligned_time")%>%
    arrange(start_stop_aligned_time, start_stop)%>%
    mutate(start_stop_zt = start_stop_aligned_time + (fstart_hour-lon_hour))%>%
    mutate(start_stop_zt = case_when(start_stop_zt>36 ~ (start_stop_zt - (floor((start_stop_zt-12)/24)*24)),
                                     start_stop_zt<12 ~ start_stop_zt+24,
                                     T ~ start_stop_zt))
  
  lines_d<-points_d%>%pivot_wider(names_from = start_stop, values_from=start_stop_zt, id_cols=c("mouse","bout_id",facet_var,"duration"))
  
  normal_lines_d<-lines_d%>%filter(start<stop)
  patched_lines_d<-lines_d%>%filter(start>=stop)%>%
    mutate(stop=35.99)%>%
    rbind(lines_d%>%filter(start>=stop)%>%
            mutate(start=12.01,bout_id=bout_id+0.1))
  lines_d<-rbind(normal_lines_d,patched_lines_d)%>%
    pivot_longer(cols=c(start,stop),names_to="start_stop", values_to = "start_stop_zt")
    
  d<-list("points"=points_d, "lines"=lines_d)
  return(d)
}

add_tsi <- function(data, y_var="temp", time_col="chron", animal_col="mouse", output_col = "tsi_", windows, window_units="minutes"){
  for (window in windows){
    if (window_units=="minutes" & "chron" %in% class(data[[time_col]])){wnd<-window/(24*60)
    }else{stop("check that time_col and windows_units are set correctly")}
    col_name<-paste0(output_col,window)
    
    data<-data%>%group_by(.data[[animal_col]])%>%
      mutate(!!col_name :=slide_index_dbl(
        .x=.data[[y_var]],
        .i=.data[[time_col]],
        .f= ~ exp(-sd(.x, na.rm=T)),
        .before=wnd/2,
        .after=wnd/2,
        .complete=F
      ))
  }
  return(data)
}

##Saving plots/data
save_plot<- function(name, plot=last_plot(), direc="./output/",w=NA,h=NA,units="in", ...){
  # print("Saving pdf...")
  ggsave(filename = paste0(direc,name,".pdf"), plot = plot, width=w, height=h, units=units, device=cairo_pdf,...)
  # print("Saving svg...")
  ggsave(filename = paste0(direc,name,".svg"), plot = plot, width=w, height=h, units=units, ...)
  # print("Saving png...")
  ggsave(filename = paste0(direc,name,".png"), plot = plot, width=w, height=h, units=units, ...)
  # print("Saving RDS...")
  # saveRDS(object=last_plot(), file = paste0(direc,name,".rds"))
}

save_plot_lowdpi <- function(name, plot=last_plot(), direc="./output/",w=NA,h=NA,units="in"){
  # print("Saving pdf...")
  ggsave(filename = paste0(direc,name,".pdf"), plot = plot, width=w, height=h, units=units,dpi=72)
  # print("Saving svg...")
  ggsave(filename = paste0(direc,name,".svg"), plot = plot, width=w, height=h, units=units)
  # print("Saving png...")
  ggsave(filename = paste0(direc,name,".png"), plot = plot, width=w, height=h, units=units)
  # print("Saving RDS...")
  # saveRDS(object=last_plot(), file = paste0(direc,name,".rds"))
}

save_plot_facet <- function(name,facet_name=NULL, plot=last_plot(), by="mouse", direc="./output/",w=NA,h=NA,fw=NA,fh=NA,units="in"){
  #Define facet name
  if (is.null(facet_name)){facet_name<-paste(name, "by", by)}
  
  # print("Saving pdf...")
  ggsave(filename = paste0(direc,name,".pdf"), plot = plot, width=w, height=h, units=units)
  # print("Saving svg...")
  ggsave(filename = paste0(direc,name,".svg"), plot = plot, width=w, height=h, units=units)
  # print("Saving png...")
  ggsave(filename = paste0(direc,name,".png"), plot = plot, width=w, height=h, units=units)
  # print("Saving pdf...")
  ggsave(filename = paste0(direc,facet_name,".pdf"), plot = facet(plot,by), width=fw, height=fh, units=units)
  # print("Saving svg...")
  ggsave(filename = paste0(direc,facet_name,".svg"), plot = facet(plot,by), width=fw, height=fh, units=units)
  # print("Saving png...")
  ggsave(filename = paste0(direc,facet_name,".png"), plot = facet(plot,by), width=fw, height=fh, units=units)
  # print("Saving RDS...")
  # saveRDS(object=last_plot(), file = paste0(direc,name,".rds"))
}

save_plot_facet_exp <- function(name,facet_name=NULL, plot=last_plot(), by="exp", direc="./output/",w=NA,h=NA,fw=NA,fh=NA,units="in"){
  #Define facet name
  if (is.null(facet_name)){facet_name<-paste(name, "by", by)}
  
  # print("Saving pdf...")
  ggsave(filename = paste0(direc,name,".pdf"), plot = plot, width=w, height=h, units=units)
  # print("Saving svg...")
  ggsave(filename = paste0(direc,name,".svg"), plot = plot, width=w, height=h, units=units)
  # print("Saving png...")
  ggsave(filename = paste0(direc,name,".png"), plot = plot, width=w, height=h, units=units)
  # print("Saving pdf...")
  ggsave(filename = paste0(direc,facet_name,".pdf"), plot = facet(plot,by), width=fw, height=fh, units=units)
  # print("Saving svg...")
  ggsave(filename = paste0(direc,facet_name,".svg"), plot = facet(plot,by), width=fw, height=fh, units=units)
  # print("Saving png...")
  ggsave(filename = paste0(direc,facet_name,".png"), plot = facet(plot,by), width=fw, height=fh, units=units)
  # print("Saving RDS...")
  # saveRDS(object=last_plot(), file = paste0(direc,name,".rds"))
}

save_pdf <- function(name, plot=last_plot(), direc="./output/",w=NA,h=NA,units="in"){
  # print("Saving pdf...")
  ggsave(filename = paste0(direc,name,".pdf"),plot = plot, width=w, height=h, units=units)
}

save_png_large <- function(name, plot=last_plot(), direc="./output/",w=NA,h=NA,units="in",dpi=300){ #to be used with very large plots (raster plots is what I wrote this for). Using ggsave seems to cause issues with color
  png(filename = paste0(direc,name,".png"), width=w, height=h, res=dpi, units=units)
  print(plot)
  dev.off()
}

stat_save<- function(data, name_prefix = NULL, name=NA, direc="./output/"){
  #fix issues with vectors in dataframe
  if ("groups" %in% colnames(data)){
    data$groups<-as.character(data$groups)}
  
  #set column names for renaming
  labels <- c(
    "mouse" = "mouse_id",
    "time" = "time_in_deep_torpor",
    "pre_fast_weight" = "body_weight",
    "uterine_weight" = "uterine_weight",
    "weight_change"  = "weight_change",
    "deltaT" = "max_delta",
    "hIndex" = "mean_delta",
    "stdev" = "std_dev",
    "time_to_torpor" = "latency",
    "bouts" = "bouts"
  )
  
  #set output path
  file_out<-paste0(direc,
                   name_prefix,
                   ifelse(is.na(name),
                          deparse(substitute(data)),
                          name),
                   ".csv")
  
  #detect test type and prepare for export
  if (any(data%>%class() == "t_test") | all(data%>%class() == "data.frame")){
    d<-data%>%mutate(.y. = coalesce(labels[.y.], .y.))
    } else if (any(data%>%class() == "anova_test")){
    d<-data%>%as_tibble()%>%mutate(Effect = coalesce(labels[Effect], Effect))
    } else if (any(data%>%class() == "anova.lme")){
    data$Effect<-rownames(data)
    d<-data%>%as_tibble()%>%mutate(Effect = coalesce(labels[Effect], Effect))
    } else {
      d<-data
      print("Warning, data exported without processing")
    }
  
  #export data
  write_csv(d, file_out)
  }

write_output<-function(data, direc="./output/", name = NA){
  saveRDS(object=data, file=paste0(direc,ifelse(is.na(name),deparse(substitute(data)),name),".rds"))
  write_csv(x=data, file=paste0(direc,ifelse(is.na(name),deparse(substitute(data)),name),".csv"))}

write_source_data_df <- function(data, name_prefix=NULL, name="df_sourcedata.csv", direc="./output/", add_cols=c(), cols_out=c(), group_var="pellet", x_limits=c(-6,51)){
  cols<-c("core_body_temperature" = "temp",
          "hours_fasted"= "aligned_time",
          "mouse_id" = "mouse",
          "group" = group_var,
          add_cols)
  
  if (group_var != "group"){data<-data%>%select(-group)}

  d<-data%>%
    rename(any_of(cols))%>%
    select(any_of(c(names(cols),cols_out)))%>%
    filter(hours_fasted %>% between(x_limits[1],x_limits[2]))
  write_csv(d, paste0(direc, name_prefix, name))
}

write_source_data_tdf <- function(data, name_prefix=NULL, name="tdf_sourcedata.csv", direc="./output/", add_cols=c(), cols_out=c(), group_var="pellet"){
  cols<-c("mouse_id" = "mouse",
          "group" = group_var,
          "time_in_deep_torpor" = "time",
          "body_weight" = "pre_fast_weight",
          "uterine_weight" = "uterine_weight",
          "weight_change" = "weight_change",
          "max_delta" = "deltaT",
          "mean_delta" = "hIndex",
          "std_dev" = "stdev",
          "latency" = "time_to_torpor",
          "bouts" = "bouts",
          add_cols)
  
  if (group_var != "group"){data<-data%>%select(-group)}
  
  d<-data%>%
    rename(any_of(cols))%>%
    select(any_of(c(names(cols),cols_out)))
  write_csv(d, paste0(direc, name_prefix, name))
}

write_source_data_raleigh <- function(data, name_prefix=NULL, name="raleigh_sourcedata.csv", direc="./output/", add_cols=c(), cols_out=c(), group_var="pellet"){
  cols<-c("mouse_id" = "mouse",
          "group" = group_var,
          "bout_id" = "bout_id",
          "bout_duration_minutes" = "duration",
          "bout_phase" = "start_stop",
          "zt_hours" = "start_stop_zt",
          add_cols)
  
  if (group_var != "group"){data<-data%>%select(-group)}
  
  d<-data%>%
    rename(any_of(cols))%>%
    select(any_of(c(names(cols),cols_out)))%>%
    mutate(zt_hours = ifelse(zt_hours>24, zt_hours-24,zt_hours))
  write_csv(d, paste0(direc, name_prefix, name))
}

##ggplot themes/aesthetics/settings
point_summary <- function(...,size=8){
  geom_point(size=size,stat="summary",...)
}

point_indiv <- function(...,fill="grey60",size=3,seed=123,position=position_jitter(width=0.05,height=0,seed=seed),shape=21,color="grey20",alpha=0.7){
  geom_point(fill=fill,size=size,position=position,shape=shape,color=color,alpha=alpha,...)
}

point_errorbar <- function(...,width=0.5,size=1,color="grey15"){
  geom_errorbar(stat="summary",width=width,size=size,color=color,...)
}

continuous_line <- function(...,stat="summary",size=1){
  geom_line(size=size,stat=stat,...)
}

continuous_errorbar<- function(...,width=0,alpha=0.2){
  geom_errorbar(stat="summary",width=width,alpha=alpha,...)
}

xy_point <- function(...,size=3.5){
  geom_point(size=size,...)
}

xy_point2 <- function(...,size=3,shape=21,color="grey20",alpha=0.7){
  geom_point(size=size,shape=shape,color=color,alpha=alpha,...)
}

regression_line <- function(..., size=1.25, method="lm", se=F){
  geom_smooth(size=size,method = method, se = se,...)
}
  
line_error <- function(..., width=0,alpha=0.3){
  geom_errorbar(width=width,alpha=alpha,stat="summary",...)
}

line_pair <- function(..., color="grey",seed=123,position=position_jitter(width=0.05,height=0,seed=seed),size=1,alpha=0.8){
  geom_path(color=color,position=position,size=size,alpha=alpha,...)
}

draw_pvalue <- function(..., data,label.size=12/.pt,bracket.size=1,fontface="bold"){
  stat_pvalue_manual(data=data,label.size=label.size,bracket.size=bracket.size, fontface=fontface,...)
}

p_to_stars <- function(p){
  stars<-case_when(p>0.05 ~ "ns",
                   p<0.0001 ~ "****",
                   p<0.001 ~ "***",
                   p<0.01 ~ "**",
                   p<0.05 ~ "*")
  return(stars)
}

annotate_pvalue <- function(..., geom="text",hjust=0,x=0,y=0,p,size=4.7,fontface="bold"){ #set p equal to cell in t_test where p-value is located!
  annotate(geom=geom,hjust=hjust,x=x,y=y,label=paste0("p=",p),size=size,fontface=fontface,...)
} 

annotate_text <- function(..., geom="text",hjust=0,x=0,y=0,label="Lorem Ipsum",size=4.7,fontface="bold"){
  annotate(geom=geom,hjust=hjust,x=x,y=y,label=label,size=size,fontface=fontface,...)
}

plot_raleigh <- function(data, facet_var, sum_data=NULL,x="start_stop_zt", y="duration", xy_set=NULL, hline_increment=100, y_max=1.05){
  if (is.null(sum_data)){
    print("No summary data provided, calculating from data")
    sum_data<-(data$points)%>%group_by(across(all_of(facet_var)))%>%summarize(zt=mean(.data[[x]]),duration=mean(.data[[y]]))
  }
  if (is.null(xy_set)){
    xy_set<-list(scale_x_continuous(limits=c(12,36),breaks=seq(0,35.9,4),labels = c(seq(0,20,4),"0/24",seq(4,11.9,4))),
                 scale_y_continuous(limits=c(0,max((data$points)%>%pull({{y}}))*y_max),breaks=seq(0,100000,hline_increment)))
  }
  raleigh_set<-list(geom_rect(fill="grey90",xmin=12, xmax=24, ymin=-Inf,ymax=Inf,alpha=0.5),
                    geom_hline(yintercept=seq(0,max((data$points)%>%pull({{y}}))*y_max,hline_increment),color="grey60"),
                    geom_vline(xintercept=seq(12,36,4),color="grey60"),
                    geom_line(data=data$lines,aes(group=bout_id),color="grey",size=0.5,alpha=0.4),
                    geom_point(color="grey20",shape=21,fill="grey60",size=1.5,alpha=0.4),
                    geom_segment(data=sum_data, aes(x=zt,xend=zt,yend=duration,color=.data[[facet_var]]),y=0,size=1.5, arrow=arrow(length=unit(0.3,"cm")),alpha=0.7),
                    coord_radial(expand=F,r.axis.inside=T),
                    xy_set
                    )
  p<-ggplot(data$points, aes(x=.data[[x]], y=.data[[y]]))+raleigh_set
  return(p)
}

##Misc
`%nin%` <- Negate(`%in%`)

pca_matrix<- function(df, row_names_from = 1){
  df<-as.data.frame(df)
  rownames(df)<-df[,row_names_from]
  df<-df%>%select(!row_names_from)
  return(df)
}

write_sessioninfo<- function(){
  writeLines(capture.output(sessionInfo()), "./output/sessionInfo.txt")
}
