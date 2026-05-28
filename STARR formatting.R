library(dplyr)
library(tidyverse)
library(lubridate)
library(chron)
setwd("C:/Users/paulv/Box/correalab/Member Folders/Paul Vander/Experiments/230914_circulating_E2_torpor_CR/telem data")
#Run this code first to make format compatible with "telemetry analysis" code
###Code written by Paul
#This code assumes one CSV with ALL of the data
#This code assumes that temp and activity is collected at 5 minute intervals

###########SET MANUALLY
idinfo<-c("mouse", "misc", "measure", "misc2") #Order of info in ID strings, set unwanted columns to "misc[index]". MOUSE SHOULD BE FIRST!


######Read in data, and format it correctly
d<-read_csv("telem data combined.csv")
colnames(d)<-gsub(" ", "_", colnames(d))#remove spaces from column names and add underscores
# mdf<-read.csv("telem metadata.csv")
# mdf$ID<-gsub(" ", "_", mdf$ID)#remove spaces from column names and add underscores
# mdf<-mdf%>%separate(ID, sep="_","mouse")
df<-d%>%
  pivot_longer(3:length(colnames(d)),names_to="ID",values_to="data")%>%
  separate(ID, sep="_",idinfo)%>% ##Ignore warning about additional pieces
  select(!starts_with("misc"))%>%
  mutate(measure=case_when(measure=="Deg."~"temp",measure=="Cnts"~"act",T~"unknown"))%>%
  mutate(chron=chron(dates.=date, times.=time))
grep(df$measure, pattern="unknown")#Should return integer(0)
df<-df%>%spread(measure,data)
