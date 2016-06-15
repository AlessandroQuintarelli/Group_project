library(ggplot2)
library(data.table)
library(sqldf)
source("theme_darrel.R")


# Read data

dt <- data.table(read.csv("./Data/Speed.csv"))


# Keys

racekey <- data.table(num = 1:6, descr =c("Black","White","Latino",
                       "Asian","Native American","Other"))
setkey(racekey, num)

# Racial profiling

race.data <- dt[, mean(like, na.rm = T), by = c("race", "race_o","imprace")]
race.data <- race.data[is.finite(rowSums(race.data))]
race.data[,equal := as.factor(race == race_o)]


race.data$equal <- ifelse(race.data$equal==FALSE, "Dating a different race", "Dating same race")
plot.data <-race.data[,mean(V1, na.rm = T), by = c("imprace", "equal")]

#plot.data <- factor(plot.data)

# how important people think race is vs how much they enjoy their date?
# (there are a few donalds here)
Plot1<- ggplot(plot.data,
       aes(x = imprace, y = V1, colour = equal)) + 
  geom_point(size = 2) +
  geom_line(size = 1)+
  theme(panel.background =  element_rect(fill = NA, colour = NA, size = 0.25), 
        panel.border =      element_blank(),
        panel.grid.major =  element_blank(),
        panel.grid.minor =  element_blank(),
        axis.line =         element_line(colour = "black", size=0.25),
        axis.ticks =        element_line(colour = "black", size=0.25),
        axis.text.x = element_text(size = 6 , lineheight = 0.9, colour = "black", vjust = 1),
        axis.text.y = element_text(size = 6, lineheight = 0.9, colour = "black", hjust = 1))+
  coord_cartesian(ylim = c(4, 8), xlim= c(0,10))+
  scale_y_continuous("To what extent did you enjoy your speed date?")+
  scale_x_continuous("How important is it that the person you date is your race?")



# who likes variety

plot.data2 <- race.data[,mean(V1), by = c("equal","race")]
setnames(plot.data2, "race", "num")
plot.data2 <- merge(plot.data2, racekey, by = "num")


Plot2<- ggplot(plot.data2,
       aes(x = descr, y = V1, colour = equal)) + 
  geom_point(size = 2) +
  geom_line(size = 1)+
  theme(legend.title = element_blank())+
  theme(panel.background =  element_rect(fill = NA, colour = NA, size = 0.25), 
        panel.border =      element_blank(),
        panel.grid.major =  element_blank(),
        panel.grid.minor =  element_blank(),
        axis.line =         element_line(colour = "black", size=0.25),
        axis.ticks =        element_line(colour = "black", size=0.25),
        axis.text.x = element_text(size = 6 , lineheight = 0.9, colour = "black", vjust = 1),
        axis.text.y = element_text(size = 6, lineheight = 0.9, colour = "black", hjust = 1))+
  coord_cartesian(ylim = c(4, 8))+
  scale_y_continuous("To what extent did you enjoy your speed date?")+
  scale_x_discrete("")

#false dating different race

# heatmap
racedt <- race.data[,mean(V1, na.rm = T), by =  c("race","race_o")]



racedt <- racedt[race< 5]
racedt <- racedt[race_o< 5]

racedt <- data.table(sqldf("select * from racedt left join racekey on race == num"))
setnames(racedt, "descr", "descr.this")
racedt <- data.table(sqldf("select * from racedt left join racekey on race_o == racekey.num"))
setnames(racedt, "descr", "descr.other")


Plot3 <- ggplot(racedt, aes(descr.this,descr.other))+ 
  geom_tile(aes(fill = V1))+
  scale_fill_gradient(low = "white", high = "red")+
  scale_y_discrete("Race of speed dating partner")+
  scale_x_discrete("Race of speed dater who is reporting satisfaction score")

