library(dplyr)
library(tidyverse)
library(ggplot2)
library(ggpubr)
library(ggprism)
library(chron)
library(rstatix)
library(emmeans)
library(EnvStats)
library(nlme)
library(gtools)
library(svglite)
library(openxlsx)
summarize<-dplyr::summarize
source("C:/Users/paulv/Box/correalab/Member Folders/Paul Vander/Code/Functions.R")

colors<-c("#000000", "#E69F00", "#56B4E9", "#009E73","#F0E442", "#0072B2", "#D55E00", "#CC79A7") #http://bconnelly.net/posts/creating_colorblind-friendly_figures/

######Things to set manually:
direc<-"C:/Users/paulv/Box/correalab/Member Folders/Paul Vander/Experiments/230914_circulating_E2_torpor_CR/telem data"
#Run analysis from other scripts (temp formatting and estrus analysis)
setwd(direc)
source("./STARR formatting.R")
# setwd(direc)
# source("../estrous tracking/estrous analysis.R")
setwd(direc)

lon<-7 #set this to the hour that the lights turn on (ZT0)
ftime<-16 #set this to the hour that fasting was started 
cr_order <- c("100%", "100%", "60%", "60%", "60%", "60%", "50%", "50%", "50%", "50%", "50%", "50%")
cr_length <- 24*length(cr_order) #length of CR feeding (in hours)
start<-chron(dates.="10/3/2023", times.="07:00:00")
# trial1_end<-chron(dates.="6/11/2023",times.="10:00:00")
micetoexclude<-c("T32", #Excluded because it's pellet popped out of the skin
                 "T37"  #Excluded because it had open sores that required euthanasia 11/4/23
                 )
mouse_trials_toexclude<-c(
                          )
trials_to_exclude<-c()
torpormeasures<-c("Tmin","deltaT","bouts","time","stdev","hIndex","time_to_torpor","imobile_time","iIndex","avg_act", "torpor_days") #Change this if you add more

#Colors for graphing:
group_gonad_scale<-c("black","#56B4E9","grey40","#D55E00")
group_gonad_scale2<-c("black","#56B4E9","black","#D55E00")
injection_scale<-c("black","#D55E00")
injection_scale2<-c("black","#56B4E9","#D55E00")
post_ovx_scale<-c("#56B4E9","#D55E00")
post_ovx_scale2<-c("black","#D55E00")
pellet_scale<-c("black","#D55E00")
pre_ovx_scale<-c("black","grey50")
estrous_scale<-c("black","#009E73","#F0E442","#0072B2")

######Read in metadata
mdf<-read.csv("metadata.csv")%>%
  mutate(group=as.factor(group),
         group_gonad=factor(paste0(group,"_",gonad), levels=c("vehicle_intact","vehicle_ovx","E2_intact","E2_ovx")),
         pellet = factor(pellet, levels=c(#"none",
                                          "vehicle","E2"),labels=c(#"pre-OVX", 
                                                                   "OVX+Vehicle","OVX+E2")),
         weight_lost=pre_fast_weight-post_fast_weight,
         weight_change=post_fast_weight-pre_fast_weight,
         fstart = chron(dates.=fstart, times=paste0(ftime,":00:00")),
         fstop = chron(dates.=fstop,times.=paste0(ftime,":00:00")),
         mouse_trial = paste0(mouse,"_",trial))

###Tidy up dataframe, merge with metadata. Exclude mice
#Iteratively extract the torpor stop dates for each trial (for distinguishing trials in df in next step)
# for (trl in mdf$trial%>%unique){ 
#   trl_num<-substr(trl, start=nchar(trl), stop=nchar(trl))
#   trl_fstop<-paste0("fstop",trl_num)
#   df<-mdf%>%filter(trial==trl)%>%mutate(!!trl_fstop:=fstop)%>%select(any_of(c("mouse",trl_fstop)))%>%merge(df) ##add first fstop as a column
# }
#Previous way of doing the above manually (deprecated)
# df<-mdf%>%filter(trial=="trial1")%>%mutate(fstop1=fstop)%>%select(mouse,fstop1)%>%merge(df) ##add first fstop as a column
# df<-mdf%>%filter(trial=="trial2")%>%mutate(fstop2=fstop)%>%select(mouse,fstop2)%>%merge(df) ##add second fstop as a column
# df<-mdf%>%filter(trial=="trial3")%>%mutate(fstop3=fstop)%>%select(mouse,fstop3)%>%merge(df) ##add third fstop as a column

df<-df%>%mutate(zt=chron(times.=time))%>%mutate(zt=round(24*ifelse(zt>=(lon/24),(zt-lon/24),(zt+(24/24)-(lon/24))),digits=4),
                                                trial="trial1",
                                                mouse_trial = paste0(mouse,"_",trial))
unique(df$trial) #check for any missed data points in trial assignment
df<-merge(df, mdf)
# df<-df%>%mutate(date_for_stage = case_when(zt>=12 & zt<(24-lon) ~ chron(dates=date) + 1,
#                                            T ~ chron(dates=date)))
# df<-merge(df, sdf%>%mutate(date_for_stage=chron)%>%select(mouse,date_for_stage,stage,sub_stage), all.x=T)
df<-df%>%mutate(aligned_time= round(as.numeric((chron-fstart)*24),digits = 2))
length(df%>%filter(!between(temp,20,50))%>%pull(temp))
length(df%>%filter(is.na(temp))%>%pull(temp))#Tells you how many "NaN"s or other weird values are in your dataset. Should be a low number!
df<-df%>%filter(mouse %nin% micetoexclude, mouse_trial %nin% mouse_trials_toexclude, trial %nin% trials_to_exclude, chron>start, temp!="NaN")
mdf<-mdf%>%filter(mouse %nin% micetoexclude, mouse_trial %nin% mouse_trials_toexclude, trial %nin% trials_to_exclude)
length(df%>%filter(!between(temp,20,50))%>%pull(temp))
length(df%>%filter(is.na(temp))%>%pull(temp))#Tells you how many "NaN"s or other weird values are in your dataset. Should be a low number!

# df<-df%>%select(!c(fstart,fstop,fstop1,fstop2,fstop3)) #remove extra columns for easy merging with other datasets later
# mdf<-mdf%>%select(!c(fstart,fstop)) #remove extra columns for easy merging with other datasets later

#Calculate temp change relative to pre-fast average
df<-df%>%filter(aligned_time>-48, aligned_time<=0)%>%group_by(mouse,trial)%>%summarize(baseline_temp=mean(temp))%>%merge(df)
df<-df%>%mutate(temp_change = temp-baseline_temp)

##Calculate time (duration) and time to torpor (latency) with different temp change cutoffs
thresholds<-seq(-3,-10,-1)
thres_df<-tibble()
thres_bdf<-tibble()
for (thres in thresholds){
  cd<-df%>%
    arrange(mouse,chron)%>%
    mutate(temp_change_lag=lag(temp_change,default=first(temp_change)))%>%
    mutate(t_bout=case_when(temp_change>=thres & temp_change_lag>=thres~"none",
                            temp_change<thres & temp_change_lag>=thres~"start", 
                            temp_change>=thres & temp_change_lag<thres~"end",
                            temp_change<thres & temp_change_lag<thres~"torpor"))%>%
    select(!temp_change_lag)
  cd<-cd%>%
    group_by(mouse,trial)%>%
    mutate(first_bout= t_bout=="start" & !duplicated(t_bout=="start"))
  
  #Make bout duration data frame using temp change
  i=1
  startrows<-grep(cd$t_bout, pattern="start")
  stoprows<-grep(cd$t_bout, pattern="end")
  bd<-data.frame(row.names=1:sum(cd$bouts))
  for (row in startrows){
    dur<-60*(cd[stoprows[i],"aligned_time"]-cd[startrows[i],"aligned_time"]) ###This now works with gaps in data
    bd[i,"duration"]<-dur
    bd[i,"mouse"]<-cd[startrows[i],"mouse"]
    bd[i,"trial"]<-cd[startrows[i],"trial"]
    bd[i,"start"]<-cd[startrows[i],"aligned_time"]
    bd[i,"stop"]<-cd[stoprows[i],"aligned_time"]
    bd[i,"threshold"]<-thres
    i=i+1
  }
  bd<-bd%>%mutate(duration=ifelse(stop>cr_length, duration-(60*(stop-cr_length)),duration))%>%filter(start<cr_length) ##Excludes time after fasting is stopped as counting in bouts calculation
  bd<-merge(bd,mdf)
  thres_bdf<-rbind(thres_bdf,bd)
  
  #Summarize torpor measures
  cd<-cd%>%
    filter(between(aligned_time,0,cr_length))%>%
    group_by(mouse,trial)%>%
    summarize(bouts=sum(t_bout=="start"),
              time=((sum(temp_change<thres)+bouts)/12),
              time_to_torpor=ifelse(TRUE %in% first_bout,aligned_time[first_bout==TRUE],NA),
              deltaT = min(temp_change),
              stdev = sd(temp_change),
              hindex = mean(temp_change))
  cd<-cd%>%mutate(threshold = thres)
  cd<-merge(cd,mdf)
  thres_df<-rbind(thres_df,cd)
  
}

auc_df<-thres_df%>%group_by(mouse,trial)%>%summarize(time_auc = mean(time)*length(thresholds),
                                                     bouts_auc=mean(bouts)*length(threshold),
                                                     time_to_torpor_auc=mean(time_to_torpor)*length(threshold))%>%merge(mdf)


######Summarize prefast data by zt, quantify
ztdf<-df%>%
  filter(aligned_time%>%between(-48,0))%>%
  group_by(mouse,zt,trial)%>%
  summarize(zt_temp=mean(temp),zt_act=mean(act))%>%
  mutate(phase=ifelse(zt<12, "day","night"))

phdf<-ztdf%>%group_by(mouse,phase,trial)%>%summarize(phase_temp = mean(zt_temp),phase_act=mean(zt_act))

pdf<-merge(df%>%filter(aligned_time%>%between(-48,0))%>%group_by(mouse,trial)%>%summarize(pre_temp=mean(temp),pre_act=mean(act)),
           merge(df%>%filter(aligned_time%>%between(-48,0),zt%>%between(0,11.99))%>%group_by(mouse,trial)%>%summarize(day_temp=mean(temp),day_act = mean(act)),
                 df%>%filter(aligned_time%>%between(-48,0),zt%>%between(12,24))%>%group_by(mouse,trial)%>%summarize(night_temp=mean(temp),night_act=mean(act))))

# #Summarize by torpor stage and ZT   
# estrous_df<-df%>%
#   filter(aligned_time%>%between(-48,0))%>%
#   group_by(mouse,zt,stage,trial)%>%
#   summarize(stage_temp=mean(temp),stage_act=mean(act))


######Quantify torpor data
#Add column for bouts in df
df<-df%>%
  arrange(mouse,trial,chron)%>%
  mutate(temp_lag=lag(temp,default=first(temp)))%>%
  mutate(t_bout=case_when(temp>=31 & temp_lag>=31~"none",
                          temp<31 & temp_lag>=31~"start", 
                          temp>=31 & temp_lag<31~"end",
                          temp<31 & temp_lag<31~"torpor"))%>%
  select(!temp_lag)
#Find first torpor time for each mouse
df<-df%>%
  group_by(mouse,trial)%>%
  mutate(first_bout= t_bout=="start" & !duplicated(t_bout=="start"))
#Calculate measures
tdf<-df%>%
  filter(between(aligned_time,0,cr_length))%>%
  group_by(mouse,trial)%>%
  summarize(Tmin=min(temp), 
            stdev=sd(temp),
            bouts=sum(t_bout=="start"),
            time=((sum(temp<31)+bouts)/12),
            avg_temp=mean(temp),
            avg_act=mean(act),
            time_to_torpor=ifelse(TRUE %in% first_bout,aligned_time[first_bout==TRUE],NA))
tdf<-merge(tdf, pdf) #add pre-fasting temperature averages to data table
tdf<-tdf%>%mutate(deltaT=(Tmin-pre_temp),hIndex=pre_temp-avg_temp) #Add last two torpor measures

#Make data frame for bout duration
i=1
startrows<-grep(df$t_bout, pattern="start")
stoprows<-grep(df$t_bout, pattern="end")
bdf<-data.frame(row.names=1:sum(tdf$bouts))
for (row in startrows){
  dur<-60*(df[stoprows[i],"aligned_time"]-df[startrows[i],"aligned_time"]) ###This now works with gaps in data
  bdf[i,"duration"]<-dur
  bdf[i,"mouse"]<-df[startrows[i],"mouse"]
  bdf[i,"trial"]<-df[startrows[i],"trial"]
  bdf[i,"start"]<-df[startrows[i],"aligned_time"]
  bdf[i,"stop"]<-df[stoprows[i],"aligned_time"]
  i=i+1
}
bdf<-bdf%>%mutate(duration=ifelse(stop>cr_length, duration-(60*(stop-cr_length)),duration)) ##Excludes time after fasting is stopped as counting in bouts calculation

######Add metadata to data frames
tdf<-merge(mdf,tdf)
bdf<-merge(bdf,mdf)
pdf<-merge(pdf,mdf)
ztdf<-merge(ztdf,mdf)
phdf<-merge(phdf,mdf)

bdf<-merge(bdf,bdf%>%group_by(pellet)%>%summarize(mean_duration = mean(duration)))
tdf<-tdf%>%mutate(weight_lost=pre_fast_weight - post_fast_weight,
                  weight_change = post_fast_weight - pre_fast_weight)

#Add activity torpor measures to tdf
atdf<-df%>% #count imboile data points (0s)
  group_by(mouse_trial)%>%
  filter(aligned_time%>%between(0,cr_length),act==0)%>%
  summarize(imobile_time = n())%>%
  merge(mdf) 
tdf<-atdf%>%merge(  #Get average activity and find difference between pre-fast activity and activity during fasting period and add to tdf
  df%>%group_by(mouse_trial)%>%
    filter(aligned_time%>%between(0,cr_length))%>%
    summarize(avg_act = mean(act)))%>%
  merge(tdf)%>%
  mutate(iIndex = avg_act - pre_act)

#Add torpor_days measure
tdf<-df%>%mutate(aligned_time_bin = cut(aligned_time, breaks = seq(0,cr_length, 24)))%>%
  group_by(mouse, trial, aligned_time_bin)%>%
  summarize(torpor_day = ifelse(min(temp) < 31, "yes", "no"))%>%
  filter(torpor_day == "yes")%>%
  group_by(mouse, trial)%>%
  summarize(torpor_days = n())%>%
  merge(tdf, all=T)%>%
  mutate(torpor_days = ifelse(is.na(torpor_days), 0, torpor_days))

#######Graph data and run stats
###Set graph settings
ms<-list(theme_prism())
ddf<-data.frame("zt_xmin"=c(12+(24*(0:3))),"zt_xmax"=c(24+(24*(0:3))))###use this df below to create automatic shading below

fddf<-data.frame("xmax"=c((lon-ftime)+(-5:30*24)),"xmin"=c((lon-ftime)+(-5:30*24)-12))###use this df below to create automatic shading below 

##Set order for facet graphs
df$mouse<-factor(df$mouse,levels=mdf%>%arrange(pellet)%>%pull(mouse)%>%unique())
df$mouse_trial<-factor(df$mouse_trial, levels=mdf%>%arrange(pellet)%>%pull(mouse_trial))

###Baseline temperature
#By ZT
ztset<-list(geom_rect(fill="grey",xmin=12,xmax=24,ymin=-Inf,ymax=Inf),
            scale_x_continuous(breaks=seq(0,24,4),expand=c(0,0)))
zs1<-list(geom_line(aes(color=pellet),size=1.5,stat="summary"),
          geom_errorbar(aes(color=pellet),width=0,alpha=0.3,stat="summary"),
          scale_color_manual(values=pellet_scale))

p1<-ggplot(ztdf,aes(x=zt,y=zt_temp))+ztset+zs1+ms
p1 #w=10, h=6

p2<-ggplot(ztdf,aes(x=zt,y=zt_act))+ztset+zs1+ms
p2 #w=10, h=6


#Across days of cycle
p1<-ggplot(df,aes(x=aligned_time, y=temp))+
  geom_rect(fill="grey",data=fddf,inherit.aes=FALSE,aes(xmin=xmin,xmax=xmax),ymin=-Inf,ymax=Inf)+ #Adds shading to graph
  geom_line(aes(color=stage),size=1)+
  scale_x_continuous(limits=c(-90,90))+
  coord_cartesian(ylim=c(34,42))+
  scale_color_manual(values=estrous_scale)+
  ms
facet(p1,"mouse_trial")

p2<-ggplot(df,aes(x=aligned_time, y=act))+
  geom_rect(fill="grey",data=fddf,inherit.aes=FALSE,aes(xmin=xmin,xmax=xmax),ymin=-Inf,ymax=Inf)+ #Adds shading to graph
  geom_line(aes(color=stage),size=1.2)+
  scale_x_continuous(limits=c(-72,48))+
  # coord_cartesian(ylim=c(35,41))+
  scale_color_manual(values=estrous_scale)+
  ms
facet(p2,"mouse_trial")

#By estrus cycle day
# p2<-ggplot(estrous_df, aes(x=zt, y=stage_temp))+
#   geom_rect(fill="grey",xmin=12,xmax=24,ymin=-Inf,ymax=Inf)+ #Adds shading to graph
#   coord_cartesian(xlim=c(0,24))+
#   # geom_smooth(aes(color=stage),span=0.2,method="loess",se=F,size=1.5)+
#   geom_line(aes(color=stage),size=1.5,stat="summary")+
#   geom_errorbar(aes(color=stage),width=0,alpha=0.3,stat="summary")+
#   scale_color_manual(values=estrous_scale)+
#   ms
# p2

###Fasting temp+activity data
##Run stats
f_anova<-anova(lme(fixed=temp~pellet*aligned_time, random = ~1 | mouse, data = df%>%filter(aligned_time>=0 & aligned_time<=cr_length)))
write.csv(f_anova,"./output/torpor data anova (temp by group x gonad x time).csv")
f_ttests<-pairwise_t_test(data=df%>%filter(aligned_time>0 & aligned_time<=cr_length)%>%group_by(aligned_time),formula = temp~pellet,p.adjust.method="bonferroni")%>%add_xy_position()

# testdf<-df%>%filter(aligned_time>0 & aligned_time<=2)%>%group_by(aligned_time,pellet)%>%summarize(mean=mean(temp),sd=sd(temp),n=n())
# write_csv(testdf,"./output/t_test df.csv")

##Graph
# fdf<-df%>%filter(aligned_time>(-7) & aligned_time<(51))

fset<-list(scale_x_continuous("Caloric intake",expand=c(0,0),breaks=seq(-24,cr_length,24),labels=c("ad lib",cr_order,"ad lib")),
           coord_cartesian(xlim=c(-30,cr_length+30)),
           scale_y_continuous(expand=c(0,0),breaks=seq(21,43,2)),
           # geom_rect(fill="grey",xmin=-7,xmax=first(fddf$xmax),ymin=-Inf,ymax=Inf), #fixes issue with first rectangle not showing up
           geom_rect(fill="grey",data=fddf,inherit.aes=FALSE,aes(xmin=xmin,xmax=xmax),ymin=-Inf,ymax=Inf), #Adds shading to graph
           geom_vline(xintercept=(0),linetype="dotdash",size=1),
           geom_vline(xintercept=(cr_length), linetype="dotdash",size=1),
           geom_hline(yintercept=31, linetype="dashed",size=1)) 
s1<-list(continuous_line(aes(color=pellet)),
         continuous_errorbar(size=0.2,aes(color=pellet)))
s2<-list(geom_line(aes(color=group_gonad),size=1,stat="summary"),
         geom_errorbar(aes(color=group_gonad),width=0,alpha=0.2,stat="summary"))

p3<-ggplot(df%>%mutate(pellet=factor(pellet,levels=c("OVX+Vehicle","OVX+E2"),labels=c("OVX + Vehicle","OVX + 17β-estradiol (E2)"))), aes(x=aligned_time,y=temp))+ms+fset+s1+
  # annotate("text",hjust=0,x=2,y=26,label="p=0.051 Hours fasted x pellet",size=4.7,fontface="bold")+
  # annotate("text",hjust=0,x=2,y=26.6,label="* pellet (Veh vs E2)",size=4.7,fontface="bold")+
  # annotate("text",hjust=0,x=2,y=27.2,label="* Hours fasted",size=4.7,fontface="bold")+
  # annotate("text",hjust=0,x=2,y=27.8,label="n=6 each",size=4.7,fontface="bold")+
  guides(color = guide_legend(override.aes = list(linewidth=2, size = 6)))+
  geom_point(aes(shape=pellet,color=pellet),x=1.5,y=-10)+
  theme(text=element_text(size=12),
        legend.text=element_text(size=12,face="bold"),
        legend.position = "top",
        legend.key.width = unit(0.6,"in"),
        legend.margin = margin(t = 0, r = 0, b = -9, l = 0, unit = "pt"))+
  labs(y="Core body temperature (Deg. C)")+
  scale_linetype_manual(values=c(1,12))+
  geom_segment(x=15,xend=39,y=28,yend=28,size=1)+
  annotate_text(size=12/.pt,label="24 hours",x=27,y=27.2,hjust=0.5)+
  # scale_color_manual(values=c("black","grey40","#D55E00","#d6996b"))
  scale_color_manual(values=post_ovx_scale2)
p3 #w=10, h=6
save_plot_facet("torpor","torpor by mouse",by="mouse",w=20,h=6,fw=20,fh=8)
save_plot("torpor narrow", plot=p3+theme(legend.position = "none",text=element_text(size=18)), w=10, h=5)
save_plot("E2 CR torpor",plot=p3,w=9,h=4)

write_source_data_df(df%>%ungroup(), "E2_CR_", group_var = "pellet", x_limits=c(-30, cr_length+30))

#Temp change
tcfset<-list(scale_x_continuous("Hours fasted",expand=c(0,0),breaks=seq(0,cr_length+2,24),labels=c(cr_order,"ad lib","ad lib")),
           coord_cartesian(xlim=c(-6,cr_length+6)),
           scale_y_continuous(expand=c(0,0)),
           # geom_rect(fill="grey",xmin=-7,xmax=first(fddf$xmax),ymin=-Inf,ymax=Inf), #fixes issue with first rectangle not showing up
           geom_rect(fill="grey",data=fddf,inherit.aes=FALSE,aes(xmin=xmin,xmax=xmax),ymin=-Inf,ymax=Inf), #Adds shading to graph
           geom_vline(xintercept=(0),linetype="dotdash",size=1.3),
           geom_vline(xintercept=(cr_length), linetype="dotdash",size=1.3),
           geom_hline(yintercept=-6, linetype="dashed",size=1.3)) 

p4<-ggplot(df, aes(x=aligned_time,y=temp_change))+ms+tcfset+s1+
  # annotate("text",hjust=0,x=2,y=26,label="p=0.051 Hours fasted x pellet",size=4.7,fontface="bold")+
  # annotate("text",hjust=0,x=2,y=26.6,label="* pellet (Veh vs E2)",size=4.7,fontface="bold")+
  # annotate("text",hjust=0,x=2,y=27.2,label="* Hours fasted",size=4.7,fontface="bold")+
  # annotate("text",hjust=0,x=2,y=27.8,label="n=6 each",size=4.7,fontface="bold")+
  guides(color = guide_legend(override.aes = list(size = 2)))+
  theme(legend.text=element_text(size=15,face="bold"))+
  labs(y="Core body temperature change (Deg. C)")+
  scale_linetype_manual(values=c(1,12))+
  # scale_color_manual(values=c("black","grey40","#D55E00","#d6996b"))
  scale_color_manual(values=pellet_scale)
p4 #w=10, h=6
save_plot_facet("torpor temp change","torpor temp change by trial",by="trial",w=12,h=6, fw=18,fh=6)
facet(p4,"mouse") #w=15, h=8
save_plot("torpor by mouse temp change",w=18,h=8)

######Torpor quantification
###Run stats
for (meas in torpormeasures){
  assign(paste0(meas,"_anova"),anova(lme(fixed = as.formula(paste0(as.name(meas), "~ pellet ")), random = ~1|mouse, data = tdf%>%filter(gonad=="ovx"), na.action = na.omit)))
  print(meas)
  print(get(paste0(meas,"_anova")))
  stat_save(get(paste0(meas,"_anova")),name=paste0(meas,"_anova"))
  anov<-get(paste0(meas,"_anova"))
  # if (anov["group","p-value"] < 0.05){
  #   print("Running t-test...")
  #   assign(paste0(meas, "_ttest"), t_test(tdf, as.formula(paste0(meas,"~pellet")), comparisons = list(c("vehicle_intact","E2_intact"),c("vehicle_ovx","E2_ovx")))%>%add_xy_position(step.increase=0.2))
  #   stat_save(get(paste0(meas,"_ttest")),paste0(meas,"_ttest"))
  # }
}

time_ttest<-t_test(tdf%>%filter(gonad=="ovx"), time~pellet)%>%add_xy_position()%>%add_significance()%>%mutate(y.position = y.position + 5)
stat_save(time_ttest, name="E2_CR_time_ttest")

###Graph
tset<-list()
ts1<-list(point_errorbar(aes()),
          line_pair(aes(group=mouse)),
          point_summary(aes(color=pellet,shape=pellet)),
          # scale_shape_manual(values = c(21,19)),
          scale_color_manual(values=pellet_scale),
          point_indiv(aes(group=mouse)),
          labs(x=element_blank()),
          theme(legend.position = "none"))

p1<-ggplot(tdf%>%filter(gonad=="ovx"),aes(x=pellet,y=Tmin))+ms+ts1
p2<-ggplot(tdf%>%filter(gonad=="ovx"),aes(x=pellet,y=deltaT))+ms+ts1+coord_cartesian(ylim=c(NA,0))
p3<-ggplot(tdf%>%filter(gonad=="ovx"),aes(x=pellet,y=bouts))+ms+ts1+coord_cartesian(ylim=c(0,NA))
p4<-ggplot(tdf%>%filter(gonad=="ovx"),aes(x=pellet,y=time))+ms+ts1+coord_cartesian(ylim=c(0,NA))
p5<-ggplot(tdf%>%filter(gonad=="ovx"),aes(x=pellet,y=stdev))+ms+ts1+coord_cartesian(ylim=c(0,NA))
p6<-ggplot(tdf%>%filter(gonad=="ovx"),aes(x=pellet,y=hIndex))+ms+ts1
p7<-ggplot(tdf%>%filter(gonad=="ovx"),aes(x=pellet,y=time_to_torpor))+ms+ts1
p8<-ggplot(tdf%>%filter(gonad=="ovx"),aes(x=pellet,y=iIndex))+ms+ts1
p9<-ggplot(tdf%>%filter(gonad=="ovx"),aes(x=pellet,y=imobile_time))+ms+ts1
p10<-ggplot(tdf%>%filter(gonad=="ovx"),aes(x=pellet,y=avg_act))+ms+ts1
p11<-ggplot(tdf%>%filter(gonad=="ovx"),aes(x=pellet,y=torpor_days))+ms+ts1
ggarrange(p1,p2,p3,p4,p5,p6,p7,p8,p9,p10,p11) # w=10, h=13
save_plot("torpor measures",w=16,h=10)

p<-ggplot(tdf%>%filter(gonad=="ovx"),aes(x=pellet,y=time))+ms+ts1+
  coord_cartesian(ylim=c(0,NA))+
  # scale_x_discrete(labels=c("Veh","E2"))+
  labs(y="Time in deep torpor (hours)",x=element_blank())+
  theme(legend.position = "none",text=element_text(size=12))+
  draw_pvalue(data=time_ttest,label="p.signif",label.size=12/.pt,fontface="bold")
p
save_plot("torpor time",w=4,h=4.5)
save_plot("E2 CR torpor time",w=2.7,h=3.1)

write_source_data_tdf(tdf, "E2_CR_", group_var = "pellet", add_cols = c("food_intake"="food_intake"))

measures<-c("deltaT","bouts","stdev","hIndex","time_to_torpor")
labels<-c("Max Δ (Deg. C)", "Bouts (#)", "Std dev (Deg. C)","Mean Δ (Deg. C)","Latency (hours)")
stat_labels<-c("max_delta", "bouts", "std_dev", "mean_delta", "latency")
i=1
for (measure in measures){
  print(measure)
  print(stat_labels[i])
  d<-tdf%>%mutate(meas=.data[[measure]])%>%filter(!is.na(meas), gonad=="ovx")
  test<-t_test(d, meas ~ group)%>%add_significance()%>%add_xy_position()
  stat_save(test%>%mutate(.y.=stat_labels[i]), name=paste0("E2_CR_",stat_labels[i],"_ttest"))
  p<-test$p
  signif<-case_when(p>=0.05 ~ " ns",
                    p<0.0001 ~ "****",
                    p<0.001 ~ "***",
                    p<0.01 ~ "**",
                    p<0.05 ~ "*")
  
  p<-ggplot(d,aes(x=pellet,y=.data[[measure]]))+ms+ts1
  if (measure=="hIndex"){p<-ggplot(d,aes(x=pellet,y=.data[[measure]]*-1))+ms+ts1}
  
  if (measure %in% c("time_to_torpor")){
    p<-p+coord_cartesian(ylim=c(0,270))
    test<-test%>%mutate(y.position=y.position+10)}
  if (measure %in% c("bouts")){
    p<-p+coord_cartesian(ylim=c(0,18))
    test<-test%>%mutate(y.position=y.position+1)}
  if (measure %in% c("deltaT")){
    p<-p+coord_cartesian(ylim=c(NA,0))
    test<-test%>%mutate(y.position=y.position+2)}
  if (measure %in% c("stdev")){
    p<-p+coord_cartesian(ylim=c(0,NA))
    test<-test%>%mutate(y.position=y.position+0.5)}
  if (measure %in% c("hIndex")){
    p<-p+coord_cartesian(ylim=c(NA,1))
    test<-test%>%mutate(y.position=y.position*-1+2.5)}
  
  p<-p+
    labs(y=labels[i],x=element_blank())+
    theme(legend.position = "none",text=element_text(size=12, face="bold"))+
    draw_pvalue(data=test,label="p.signif")
  save_plot(paste("E2 CR",measure),plot=p, w=2.7,h=3.25)
  
  i=i+1
}

#####Torpor quantification with temp change 
###Varying thresholds
##Run stats
anova_test(thres_df%>%filter(gonad=="ovx"), time ~ threshold * pellet)
anova_test(auc_df%>%filter(gonad=="ovx"), time_auc ~ pellet)

anova(lme(fixed = time ~ threshold * pellet, random = ~1|mouse, data=thres_df%>%filter(gonad=="ovx")))
anova(lme(fixed = time_auc ~ pellet, random = ~1|mouse, data=auc_df%>%filter(gonad=="ovx")))

##Graph
ths1<-list(xy_point(aes(color=pellet),stat="summary"),
           continuous_line(aes(color=pellet)),
           continuous_errorbar(aes(color=pellet),size=1),
           scale_color_manual(values=pellet_scale),
           labs(x="Temp decrease threshold"))

p1<-ggplot(thres_df%>%filter(gonad=="ovx"), aes(x=-1*threshold,y=time))+ths1+ms
p2<-ggplot(thres_df%>%filter(gonad=="ovx"), aes(x=-1*threshold,y=bouts))+ths1+ms
p3<-ggplot(thres_df%>%filter(gonad=="ovx"), aes(x=-1*threshold,y=time_to_torpor))+ths1+ms
ggarrange(p1,p2,p3) #w=10,h=9
save_plot("torpor measures with varying thresholds",w=10,h=8)

##Graph AUC
aucs1<-list(point_summary(aes(color=pellet)),
            # geom_line(aes(group=mouse),color="grey40")+
            point_errorbar(),
            point_indiv(),
            scale_color_manual(values=pellet_scale))

p4<-ggplot(auc_df%>%filter(gonad=="ovx"),aes(x=pellet,y=time_auc))+aucs1+ms
p5<-ggplot(auc_df%>%filter(gonad=="ovx"),aes(x=pellet,y=bouts_auc))+aucs1+ms
p6<-ggplot(auc_df%>%filter(gonad=="ovx"),aes(x=pellet,y=time_to_torpor_auc))+aucs1+ms
ggarrange(p4,p5,p6) #w=10, h=9
save_plot("torpor measures varying thresholds auc",w=10,h=8)

##Static threshold
#Run stats
cutoff<--6

for (meas in torpormeasures){
  if (meas %nin% c("Tmin","hIndex","imobile_time","iIndex","avg_act")){
    assign(paste0(meas,"_anova"),anova(lme(fixed = as.formula(paste0(as.name(meas), "~ pellet")), random = ~1|mouse, data = thres_df%>%filter(threshold==cutoff, gonad=="ovx"), na.action = na.omit)))
    print(meas)
    print(get(paste0(meas,"_anova")))
    stat_save(get(paste0(meas,"_anova")),name=paste0(meas,"_anova"))
    anov<-get(paste0(meas,"_anova"))
    if (anov["pellet","p-value"] < 0.05){
      print("Running t-test...")
      assign(paste0(meas, "_ttest"), t_test(thres_df%>%filter(threshold==cutoff), as.formula(paste0(meas,"~pellet")), comparisons = list(c("vehicle_intact","E2_intact"),c("vehicle_ovx","E2_ovx")))%>%add_xy_position(step.increase=0.2))
      stat_save(get(paste0(meas,"_ttest")),paste0(meas,"_ttest"))
    }
  }
}

time_tc_ttest<-t_test(thres_df%>%filter(threshold==cutoff,gonad=="ovx"), time ~ pellet)%>%add_xy_position(step.increase = 0.2)

#Graph
tset<-list()
ts1<-list(geom_line(aes(group=mouse),color="grey",size=1,alpha=0.8),
          geom_point(aes(color=pellet),stat="summary",size=8,stroke=2),
          point_errorbar(),
          # scale_shape_manual(values = c(21,19)),
          scale_color_manual(values=pellet_scale),
          point_indiv(aes(group=mouse)),
          labs(x=element_blank()),
          # scale_x_discrete(labels = c("pre-OVX", "OVX+Vehicle", "pre-OVX","OVX+EB")),
          theme(legend.position = "none"))

# p1<-ggplot(thres_df%>%filter(threshold==cutoff),aes(x=fast_stage,y=Tmin))+ms+ts1
p2<-ggplot(thres_df%>%filter(threshold==cutoff,gonad=="ovx"),aes(x=pellet,y=deltaT))+ms+ts1
p3<-ggplot(thres_df%>%filter(threshold==cutoff,gonad=="ovx"),aes(x=pellet,y=bouts))+ms+ts1
p4<-ggplot(thres_df%>%filter(threshold==cutoff,gonad=="ovx"),aes(x=pellet,y=time))+ms+ts1
p5<-ggplot(thres_df%>%filter(threshold==cutoff,gonad=="ovx"),aes(x=pellet,y=stdev))+ms+ts1
p6<-ggplot(thres_df%>%filter(threshold==cutoff,gonad=="ovx"),aes(x=pellet,y=hindex))+ms+ts1
p7<-ggplot(thres_df%>%filter(threshold==cutoff,gonad=="ovx"),aes(x=pellet,y=time_to_torpor))+ms+ts1
ggarrange(p2,p3,p4,p5,p6,p7) # w=10, h=9
save_plot("torpor measures temp change",w=16,h=8)
ggarrange(facet(p2,"trial"),facet(p3,"trial"),facet(p4,"trial"),facet(p5,"trial"),facet(p6,"trial"),facet(p7,"trial")) # w=10, h=9
save_plot("torpor measures temp change by trial",w=16,h=8)

p4+
  # scale_x_discrete(labels=c("Veh","E2"))+
  labs(y="Time in torpor (hours)",x=element_blank())+
  theme(legend.position = "none")+
  coord_cartesian(ylim=c(0,NA))+
  draw_pvalue(data=time_tc_ttest)
# annotate("text",hjust=0,x=2,y=0,label=paste0("p=",time_anova["fast_stage","p-value"]),size=4.7,fontface="bold")
# annotate("text",hjust=0,x=1,y=80,label="* Gonadal status (OVX vs pre-OVX)",size=4.7,fontface="bold")+
# annotate("text",hjust=0,x=1,y=10,label="* Pellet (E2 vs Placebo)",size=4.7,fontface="bold")+
# stat_pvalue_manual(time_ttest,label="p.adj.signif",label.size=6,bracket.size=1)
save_plot("torpor time temp change", w=6,h=4)

######Bout timing
set<-list(labs(x="Zeitgeber time (hours)",y="Bout duration (minutes)"),#,title="Timing , duration ")+
          scale_color_manual(values=post_ovx_scale2),
          ms,
          theme(axis.line=element_blank(),
                    legend.position = "none",
                    text=element_text(size=12),
                    strip.text.x=element_text(size=12,face="bold"),
                    plot.title=element_text(size=12, hjust=0,margin=margin(t=0,b=5,l=0,r=0)),
          panel.spacing.y=unit(0.2,"in")))
hline<-geom_hline(data=tibble("pellet"=c("OVX+Vehicle","OVX+E2"),"yintercept"=c(600,300))%>%mutate(pellet=factor(pellet,levels=c("OVX+Vehicle","OVX+E2"))),color="red",aes(yintercept=yintercept))

p<-format_data_raleigh(bdf%>%filter(gonad=="ovx"),"pellet")%>%plot_raleigh("pellet",hline_increment = 250)+set+hline
p+facet_wrap(vars(pellet),axes="all",nrow=1)
save_plot("E2 CR torpor timing",w=5,h=3)

write_source_data_raleigh(p$data, "E2_CR_", group_var = "pellet")

p<-format_data_raleigh(bdf%>%filter(gonad=="ovx"),"pellet")%>%plot_raleigh("pellet",hline_increment = 250,y_max=0.51)+set+hline
p+facet_wrap(vars(pellet),axes="all",nrow=1)
save_plot("E2 CR torpor timing zoom y",w=5,h=3)

p<-format_data_raleigh(bdf%>%filter(gonad=="ovx"),"pellet")%>%plot_raleigh("pellet",hline_increment = 250,y_max=0.255)+set+hline
p+facet_wrap(vars(pellet),axes="all",nrow=1)
save_plot("E2 CR torpor timing more zoom y",w=5,h=3)

######Torpor-weight relationship
###Graph weight
weight_ttest<-t_test(mdf, pre_fast_weight ~ pellet)%>%add_significance()%>%add_xy_position()%>%mutate(y.position=y.position+2)
stat_save(weight_ttest, name="E2_CR_body-weight_ttest")

p1<-ggplot(mdf%>%filter(gonad=="ovx"),aes(x=pellet,y=pre_fast_weight))+
  point_errorbar()+
  line_pair(aes(group=mouse))+
  point_summary(aes(color=pellet,shape=pellet))+
  point_indiv()+
  scale_color_manual(values=pellet_scale)+
  labs(y="Body weight (g)",x=element_blank())+
  coord_cartesian(ylim=c(0,NA))+
  draw_pvalue(data=weight_ttest,label.size = 12/.pt,fontface="bold")+
  ms+theme(legend.position="none")
p1 #w=4.5, h=4.5
save_plot("body weights",w=4,h=4.2)
save_plot("E2 CR body weight",plot=p1+theme(text=element_text(size=12)),w=2.7,h=3.2)

#Graph weight lost
weight_lost_ttests<-t_test(tdf%>%filter(gonad=="ovx"), weight_change ~ pellet)%>%add_xy_position()%>%add_significance()%>%mutate(y.position=y.position+0.2)
stat_save(weight_lost_ttests,name="E2_CR_delta-body-weight_ttest")

p1<-ggplot(tdf%>%filter(gonad=="ovx"),aes(x=pellet,y=(weight_change)))+
  point_errorbar()+
  # geom_line(aes(group=mouse),color="grey40")+
  point_summary(aes(color=pellet,shape=pellet))+
  point_indiv()+
  coord_cartesian(ylim=c(NA,0))+
  labs(y="Δ body weight (g)",x=element_blank())+
  scale_color_manual(values=pellet_scale)+
  draw_pvalue(data=weight_lost_ttests,label="p.signif")+
  # annotate_text(label="E2, MPrP, Interaction ns",x=0.5)+
  ms+theme(legend.position="none")
p1 #w=6, h=4
save_plot("weight lost", w=4,h=4.2)
save_plot("E2 CR weight lost",plot=p1+theme(text=element_text(size=12)), w=2.7,h=3.2)

##Graph weight lost - torpor correlation
p1<-ggplot(tdf%>%filter(gonad=="ovx"), aes(x=time, y=weight_lost))+
  coord_cartesian(xlim=c(0,NA),ylim=c(0,NA))+
  xy_point(aes(color=pellet))+
  regression_line(aes(color=pellet))+
  scale_color_manual(values=pellet_scale)+
  theme_prism()
p1
save_plot("weight lost by torpor time",w=8,h=6)

###Graph torpor-weight graphs by pellet
#Run stats
for (meas in torpormeasures){
  assign(paste0(meas,"_weight_anova"),anova(lme(fixed = as.formula(paste0(as.name(meas), "~ pellet + pre_fast_weight")), random = ~1|mouse, data = tdf%>%filter(gonad=="ovx"), na.action = na.omit)))
  print(meas)
  print(get(paste0(meas,"_weight_anova")))
  stat_save(get(paste0(meas,"_weight_anova")),name=paste0("E2_CR_",meas,"_weight_ancova"))
}

#Graph
cs1<-list(xy_point(aes(color=pellet,shape=pellet)),
          regression_line(aes(color=pellet)),
          scale_color_manual(values=pellet_scale),
          theme(legend.position = "none"))

p1<-ggplot(tdf%>%filter(gonad=="ovx"),aes(x=pre_fast_weight,y=time))+ms+cs1
p2<-ggplot(tdf%>%filter(gonad=="ovx"),aes(x=pre_fast_weight,y=deltaT))+ms+cs1
p3<-ggplot(tdf%>%filter(gonad=="ovx"),aes(x=pre_fast_weight,y=hIndex))+ms+cs1
p4<-ggplot(tdf%>%filter(gonad=="ovx"),aes(x=pre_fast_weight,y=bouts))+ms+cs1
p5<-ggplot(tdf%>%filter(gonad=="ovx"),aes(x=pre_fast_weight,y=stdev))+ms+cs1
p6<-ggplot(tdf%>%filter(gonad=="ovx"),aes(x=pre_fast_weight,y=time_to_torpor))+ms+cs1
p7<-ggplot(tdf%>%filter(gonad=="ovx"),aes(x=pre_fast_weight,y=Tmin))+ms+cs1
p8<-ggplot(tdf%>%filter(gonad=="ovx"),aes(x=pre_fast_weight,y=iIndex))+ms+cs1
p9<-ggplot(tdf%>%filter(gonad=="ovx"),aes(x=pre_fast_weight,y=imobile_time))+ms+cs1
p10<-ggplot(tdf%>%filter(gonad=="ovx"),aes(x=pre_fast_weight,y=avg_act))+ms+cs1
p11<-ggplot(tdf%>%filter(gonad=="ovx"),aes(x=pre_fast_weight,y=torpor_days))+ms+cs1
ggarrange(p1,p2,p3,p4,p5,p6,p7,p8,p9,p10,p11) #w=18,h=9
save_plot("torpor measures by weight",w=18,h=9)

p<-ggplot(tdf%>%filter(gonad=="ovx"),aes(x=pre_fast_weight,y=time))+ms+cs1+
  labs(y="Time in deep torpor (hours)",x="Body weight (g)")+
  coord_cartesian(ylim=c(0,NA))+
  scale_y_continuous(expand=c(0.02,0.02))+
  theme(text=element_text(size=18))
  # annotate_text(label="",x=max(tdf$pre_fast_weight),y=10,hjust=1)+
  # annotate_text(label="Body weight p=",x=max(tdf$pre_fast_weight),y=8,hjust=1)
p
save_plot("torpor time by weight",w=5,h=4.5)

p<-ggplot(tdf%>%filter(gonad=="ovx"),aes(x=pre_fast_weight,y=time))+ms+cs1+
  labs(y="Time in deep torpor (hours)",x="Body weight (g)", title="Treatment*, body weight*")+
  coord_cartesian(ylim=c(0,NA))+
  scale_y_continuous(expand=c(0.05,0.05))+
  theme(text=element_text(size=12),
        plot.title=element_text(size=12,hjust=0,margin = margin(t=0,b=5,l=0,r=0)))
# annotate_text(label="",x=max(tdf$pre_fast_weight),y=10,hjust=1)+
# annotate_text(label="Body weight p=",x=max(tdf$pre_fast_weight),y=8,hjust=1)
p
save_plot("E2 CR torpor time by weight",w=4.5,h=3.72)

##Temp change
cs1<-list(geom_point(aes(color=pellet),size=4),
          geom_smooth(aes(color=pellet),size=1.5,method = "lm", se = FALSE),
          scale_color_manual(values=pellet_scale),
          theme(legend.position = "none"))

p1<-ggplot(thres_df%>%filter(threshold==cutoff,gonad=="ovx"),aes(x=pre_fast_weight,y=time))+ms+cs1
p2<-ggplot(thres_df%>%filter(threshold==cutoff,gonad=="ovx"),aes(x=pre_fast_weight,y=deltaT))+ms+cs1
p4<-ggplot(thres_df%>%filter(threshold==cutoff,gonad=="ovx"),aes(x=pre_fast_weight,y=bouts))+ms+cs1
p5<-ggplot(thres_df%>%filter(threshold==cutoff,gonad=="ovx"),aes(x=pre_fast_weight,y=stdev))+ms+cs1
p6<-ggplot(thres_df%>%filter(threshold==cutoff,gonad=="ovx"),aes(x=pre_fast_weight,y=time_to_torpor))+ms+cs1
ggarrange(p1,p2,p4,p5,p6) #w=18,h=9
save_plot("torpor measures by weight temp change",w=18,h=9)

p1+
  coord_cartesian(ylim=c(0,NA))+
  labs(x="Body weight (g)",y="Torpor time (hours)")
  # annotate_text(label="",x=max(thres_df%>%filter(threshold==cutoff)$pre_fast_weight),y=10,hjust=1)+
  # annotate_text(label="Body weight p=",x=max(thres_df%>%filter(threshold==cutoff)$pre_fast_weight),y=8,hjust=1)
save_plot("torpor time by weight temp change",w=5,h=4)

##Bout histograms
#Run stats
bdf_anova<-anova(lme(fixed = duration ~ pellet, random = ~1|mouse, data=bdf%>%filter(gonad=="ovx")))
mean_dur_anova<-anova(lme(fixed = mean_dur ~ pellet, 
                          random = ~1|mouse, 
                          data=bdf%>%group_by(mouse,trial)%>%summarize(mean_dur=mean(duration))%>%merge(mdf)%>%filter(gonad=="ovx")))


#Graph density
dset<-list(labs(x="Torpor bout duration (minutes)",y="fraction of all bouts"),
           scale_x_continuous(expand=c(0,0)),
           scale_y_continuous(expand=c(0,0)),
           coord_cartesian(xlim=c(0,NA)),
           # annotate_text(label="Stage p=0.06 (mean duration)",x=200,y=5)
           geom_density(aes(color=pellet),size=1.5),
           scale_fill_manual(values=pellet_scale),
           # geom_vline(aes(color=post_fast_stage,xintercept=mean_duration),size=1,linetype="dashed"),
           scale_color_manual(values=pellet_scale)
)

p1<-ggplot(bdf%>%filter(gonad=="ovx"), aes(x=duration,color=pellet))+ms+dset
p1
save_plot("torpor bout duration density",w=10,h=7)

#Graph count
hset<-list(labs(x="Torpor bout duration (minutes)",y="# of bouts"),
           scale_x_continuous(expand=c(0,0)),
           scale_y_continuous(expand=c(0,0)),
           geom_histogram(aes(fill=pellet),color="white",position=position_dodge(),binwidth=30),
           scale_fill_manual(values=pellet_scale),
           geom_vline(aes(color=pellet,xintercept=mean_duration),size=1,linetype="dashed"),
           scale_color_manual(values=pellet_scale)#,
           # annotate("text",hjust=0,x=180,y=3,size=4.7,fontface="bold",label=paste0("p=",bdf_ttest$p, " (Mean bout duration)"))
)

p1<-ggplot(bdf%>%filter(gonad=="ovx"), aes(x=duration))+ms+hset
p1
save_plot("torpor bouts",w=12,h=7)

#####Activity graphs
fset<-list(scale_x_continuous("Caloric intake",expand=c(0,0),breaks=seq(0,cr_length-1,24),labels=cr_order),
           coord_cartesian(xlim=c(-6,cr_length+30)),
           scale_y_continuous(expand=c(0,0)),
           # geom_rect(fill="grey",xmin=-7,xmax=first(fddf$xmax),ymin=-Inf,ymax=Inf), #fixes issue with first rectangle not showing up
           geom_rect(fill="grey",data=fddf,inherit.aes=FALSE,aes(xmin=xmin,xmax=xmax),ymin=-Inf,ymax=Inf), #Adds shading to graph
           geom_vline(xintercept=(0),linetype="dotdash",size=1.3),
           geom_vline(xintercept=(cr_length), linetype="dotdash",size=1.3),
           scale_color_manual(values=pellet_scale))

s1<-list(continuous_line(aes(color=pellet),size = 1),
         continuous_errorbar(aes(color=pellet)))

p3<-ggplot(df, aes(x=aligned_time,y=act))+ms+fset+s1+
  # annotate("text",hjust=0,x=2,y=26,label="* Hours fasted x pellet",size=4.7,fontface="bold")+
  # annotate("text",hjust=0,x=2,y=26.6,label="* pellet (Veh vs E2)",size=4.7,fontface="bold")+
  # annotate("text",hjust=0,x=2,y=27.2,label="* Hours fasted",size=4.7,fontface="bold")+
  # annotate("text",hjust=0,x=2,y=24.8,label="n=2 Veh, n=2 E2",size=4.7,fontface="bold")+
  guides(color = guide_legend(override.aes = list(size = 1.5)))+
  labs(y="Activity counts")
# scale_color_manual(values=c("black","#D55E00"))
# scale_color_manual(values=c("black","grey40","#D55E00","#d6996b"))
p3
save_plot_facet("torpor activity","torpor activity by mouse", by="mouse",w=18, h=6, fw=18,fh=10)
facet(p3,"mouse_trial")

######Food intake
fi_anova<-anova(lme(fixed=food_intake~pellet, random = ~1 | mouse, data = tdf))
fi_ttest<-t_test(data = mdf, formula = food_intake~pellet)%>%add_xy_position()%>%add_significance()%>%mutate(y.position=y.position+0.5)
stat_save(fi_ttest, name="E2_CR_food-intake_ttest")

ggplot(mdf ,aes(x=pellet, y=food_intake))+
  point_errorbar(aes(group=pellet))+
  point_summary(aes(color=pellet,shape=pellet))+
  # geom_line(aes(group=mouse),color="grey",size=1)+
  point_indiv()+
  coord_cartesian(ylim=c(0,NA))+
  labs(y="Food intake (g/day)",x=element_blank(),title="Post-OVX + pre-treatment")+
  draw_pvalue(data=fi_ttest, label="p.signif")+
  scale_color_manual(values=pellet_scale)+
  ms+
  theme(legend.position = "none",text=element_text(size=12),
        plot.title=element_text(size=12,hjust=0.5,margin = margin(t=0,b=5,l=0,r=0)))
save_plot("food intake", w=4,h=4.5)
save_plot("E2 CR food intake", w=2.7,h=3.45)

ggplot(mdf,aes(x=pre_fast_weight, y=food_intake))+cs1+ms

#######Uterine weights
ut_ttest<-t_test(data=tdf,formula=uterine_weight~pellet)%>%add_xy_position()%>%add_significance()
stat_save(ut_ttest, name="E2_CR_uterus-weight_ttest")

ggplot(mdf%>%filter(!is.na(uterine_weight)),aes(x=pellet, y=uterine_weight))+
  coord_cartesian(ylim=c(0,NA))+
  point_errorbar()+
  point_summary(aes(color=pellet,shape=pellet))+
  point_indiv()+
  # geom_hline(yintercept=100,linetype="dotdash",size=1)+
  labs(y="Uterus weight (mg)",x=element_blank())+
  scale_color_manual(values=pellet_scale)+
  draw_pvalue(data=ut_ttest, label="p.signif")+
  ms+
  theme(legend.position = "none",text=element_text(size=12))
save_plot("E2 CR uterus weights", w=2.7,h=3.2)


###########Write data
write_output(df)
write_output(tdf)
write_output(ztdf)
write_output(mdf)
write_output(bdf)
write_output(phdf)
write_output(pdf)
write_output(estrous_df)

#For prism
prismdf<-df%>%group_by()%>%
  mutate(pellet_mouse=paste0(pellet,"_",mouse))%>%
  select(pellet_mouse,aligned_time,temp)%>%
  spread(key=pellet_mouse,value=temp)
write_csv(prismdf, "./output/raw temp for prism.csv")

#Write session info
write_sessioninfo()

#Source data and stats for paper
wb<-loadWorkbook("C:/Users/paulv/Box/correalab/Member Folders/Paul Vander/Writing/Papers/Vander et al 2026/source data and stats.xlsx")
files<-c("E2_CR_df_sourcedata.csv",
         "E2_CR_tdf_sourcedata.csv",
         "E2_CR_raleigh_sourcedata.csv",
         "E2_CR_time_ttest.csv",
         "E2_CR_time_weight_ancova.csv",
         "E2_CR_uterus-weight_ttest.csv",
         "E2_CR_body-weight_ttest.csv",
         "E2_CR_delta-body-weight_ttest.csv",
         "E2_CR_max_delta_ttest.csv",
         "E2_CR_mean_delta_ttest.csv",
         "E2_CR_std_dev_ttest.csv",
         "E2_CR_latency_ttest.csv",
         "E2_CR_bouts_ttest.csv"
)

for (file in files){
  sheet_name=strsplit(file, ".csv")[[1]][1]
  if(nchar(sheet_name)>31){
    sheet_name = gsub("pairwise","pw",sheet_name)
    sheet_name = gsub("t-tests","",sheet_name)
    sheet_name = gsub("body_weight","bw",sheet_name)
  }
  d<-read_csv(paste0("./output/",file),show_col_types = F)
  addWorksheet(wb, sheet_name)
  writeData(wb, sheet_name, d)
}

saveWorkbook(wb, "C:/Users/paulv/Box/correalab/Member Folders/Paul Vander/Writing/Papers/Vander et al 2026/source data and stats.xlsx", overwrite = T)
