
####
# Sofia Gil-Clavel
# September 22, 2021.
# Code to replicate Julia's Workflow
####


rm(list = ls())

library(tidyverse)
library(ggplot2)
library(sjPlot)

#### Import the data ####
DATA<-read.table("DATA/cchs2012_mh.tab",sep="\t",header = TRUE)

#### Extract raw variables ####
DATA<-DATA%>%select("DHHGAGE","DHH_SEX","DHHGMS","EDUDR04","NSIDSC","GENGSWL")

#### Variable transformations ####
# DHH_SEX: MALE(1) & FEMALE(2)
DATA$DHH_SEX<-factor(DATA$DHH_SEX,
                     levels = c(1,2),
                     labels = c("MALE","FEMALE"))

# DHHGAGE: Age - grouped variable
A1<-seq(15,80,5)
A2<-A1+4; A2[length(A2)]="more"
Alabels<-paste0(A1,"-",A2)

DATA$DHHGAGE<-factor(DATA$DHHGAGE,
                     levels = 1:14,
                     labels = Alabels)

# DHHGMS: MARRIED(1), COMMON-LAW(2), WIDOWED(3), DIVORCED/SEPARATED(4)
#         SINGLE(5), NOT STATED(9)
DHHGMS<-c("MARRIED", "COMMON-LAW", "WIDOWED", "DIVORCED/SEPARATED",
          "SINGLE")
#** NOTE: Transform NOT STATED into NA

INDEX=which(DATA$DHHGMS==9)
DATA$DHHGMS[INDEX]=NA

DATA$DHHGMS<-factor(DATA$DHHGMS,
                    levels = c(1:5),
                    labels = DHHGMS)

# EDUDH04: Education
# LESS THAN SECONDARY SCHOOL GRADUATION(1)
# SECONDARY SCHOOL GRADUATION(2)
# SOME POST-SECONDARY(3)
# POST-SECONDARY GRADUATION(4)
# NOT STATED(9)
EDUDR04<-c("LessSec.School","Sec.School","Some Post.Sec.","Post.Sec.")

#** NOTE: Transform NOT STATED into NA

INDEX=which(DATA$EDUDR04==9)
DATA$EDUDR04[INDEX]=NA

DATA$EDUDR04<-factor(DATA$EDUDR04,
                     levels = c(1:4),
                     labels = EDUDR04)

# NSIDSC (continuous): NEGATIVE SOCIAL INTERACTIONS SCALE 
#** NOTE: Transform NOT STATED into NA
INDEX=which(DATA$NSIDSC==99)
DATA$NSIDSC[INDEX]=NA

# GENGSWL: Satisfaction with life in general
GENGSWL<-c("VERY SATISFIED", "SATISFIED", "NEITHER SATISFIED NOR DISATISFIED", "DISSATISFIED","VERY DISSATISFIED")

#** NOTE: Transform NOT STATED into NA
INDEX=which(DATA$GENGSWL==9)

# Reverse the coding:
DATA$GENGSWL<-abs(DATA$GENGSWL-5)+1

DATA$GENGSWL[INDEX]=NA

DATA$GENGSWL2<-factor(DATA$GENGSWL,
                      levels = c(5:1),
                      labels = GENGSWL)

#### Define Analytic Sample ####
DATA<-DATA%>%filter(DHHGAGE%in%Alabels[10:14])

DATA$DHHGAGE<-droplevels(DATA$DHHGAGE)


#### Analysis 1: Regression Line ####
ggplot(DATA,aes(NSIDSC,GENGSWL))+
  geom_smooth(method='lm', formula= y~x)

#### Analysis 2: Regression Line ####

# GENGSWL~NSIDSC
FIT1<-lm("GENGSWL~NSIDSC",data = DATA)
(SUM1<-summary(FIT1))

# GENGSWL~DHHGAGE+DHH_SEX+DHHGMS+EDUDR04+NSIDSC
FIT2<-lm("GENGSWL~DHHGAGE+DHH_SEX+DHHGMS+EDUDR04+NSIDSC",data = DATA)
(SUM2<-summary(FIT2))

#### Place results side-by-side in the same table ####
tab_model(FIT1,FIT2)







