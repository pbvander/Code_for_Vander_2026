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
library(svglite)
library(gtools)
summarize<-dplyr::summarize

setwd("C:/Users/paulv/Box/correalab/Member Folders/Paul Vander/Experiments/250324_MPO_ERabKO_circulating_E2_torpor/estrous tracking")
virus_color_scale<-c("black","#009E73")

###Read in data from image file names
sdf<-tibble()
for (file in list.files()){
  if (file%>%endsWith(".tif")){
    name=strsplit(file,"_")[[1]]
    mous=name[1]
    dat=name[2]
    chron_dat=chron(dates.=as.character(name[2]),format=c(dates="ymd"),out.format=c(dates="m/d/y"))
    stage=strsplit(name[3],".tif")[[1]][1]
    sd<-tibble(mouse=mous,date=dat,chron=chron_dat,stage=stage)
    sdf<-rbind(sd,sdf)
  }
}

##Set order for graphing
sdf<-sdf%>%mutate(virus=factor(virus,levels=c("GFP","Cre"),labels=c("Control", "MPO ERαβ-KO")),
                  mouse=factor(mouse, levels=torpor_mdf%>%arrange(desc(virus))%>%pull(mouse)))

##Graph
c_plot2<-ggplot(sdf,aes(x=day,y=stage))+
  geom_line(aes(group=mouse,color=virus),size=1)+
  geom_point(aes(color=virus),size=3)+
  scale_color_manual(values=virus_color_scale)+
  ms
facet(c_plot2,"mouse",scales="free",nrow=2)+theme(strip.text = element_blank())
ggsave("estrous cycle by mouse.png")