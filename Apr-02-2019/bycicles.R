library(readr)
library(tidyverse)
library(lubridate)
library(readr)
datos <- read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-04-02/bike_traffic.csv", 
                         col_types = cols(date = col_datetime(format = "%m/%d/%Y %H:%M:%S %p"), 
                                          ped_count = col_double()))
View(datos) 

datos%>%mutate(Year=year(date))->datos
datos%>%mutate(Month=month(date))->datos

cbd<-datos%>%group_by(day=floor_date(date, "day"))%>%
  summarize(bikes=sum(bike_count))
cbd%>%mutate(Month=month(day))->cbd
cbd%>%filter(day<ymd("2018-01-01") & day>ymd("2014-01-01")  )%>%
  ggplot(aes(x=day, y=bikes))+
  geom_point(aes(color=month(day, label = TRUE, abbr = FALSE, locale = "US")))+
  geom_smooth(color="red")+
  labs(color="Month", x="Date", y= "Bikes Count", title = "Bicycles Are for the Summer")+
  theme_classic()+
  theme(panel.background = element_rect(fill = "black"))
