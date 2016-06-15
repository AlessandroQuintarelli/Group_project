library("sjPlot")
library("ggplot2")
library("reshape2")
library("dplyr")
library("grid")
library("gridExtra")
library("lme4")
library("LMERConvenienceFunctions")
library("MCMCglmm")
library(tidyr)
library(plyr)

setwd("/Users/Judy1/Documents/Insead/P3/BDA/Speeddating")

source("theme_darrel.R")

d <- read.csv("./Data/Speed.csv")


d$gender <- ifelse(d$gender==0, "Female", "Male")
d$gender <- factor(d$gender)

d_what <- d%>%
  select(attr1_s,
         sinc1_s,
         fun1_1,
         amb1_1,
         shar1_1,
         gender,
         intel1_s)%>%
  gather(Characteristic, Value, -gender, na.rm = TRUE)

p_gender <- ggplot(aes(x=Characteristic), data=d_what)+
  geom_point(aes(y=Value, x=Characteristic, group= gender, colour=gender), size=1.0, shape=16, position=position_dodge(w=0.2,h=0))+
  stat_summary(aes(y=Value, group=gender, colour=gender), fun.y="mean", geom="point", shape=18, size=5, position=position_dodge(width=0.2, height=0))+
  stat_summary(aes(y=Value, group=gender), colour="black", fun.y="mean", geom="point", shape=23, size=4.7, stroke=5, position=position_dodge(width=0.2, height=0))+
  #stat_summary(aes(y=Value, group=gender), fun.data="mean_cl_normal", geom="errorbar", width=0.25, position=position_dodge(width=0.6, height=0))+
  theme_darrel()+
  scale_y_continuous("% Importance of attribute in partner")+
  coord_cartesian(ylim = c(0, 100))+
  scale_x_discrete(labels=c("Attractive", "Sincere", "Fun", "Ambitious", "Shared Interests", "Intelligent"))

