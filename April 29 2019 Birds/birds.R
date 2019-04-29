library(tidyverse)
library(magrittr)
library(lubridate)

bird_collisions <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-04-30/bird_collisions.csv")

mp_light <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-04-30/mp_light.csv")


joined<-left_join(bird_collisions %>% filter(locality=="MP"), mp_light , by = "date")

joined %>%
  filter(!is.na(light_score)&family!="Laniidae"&family!="Icteridae") %>% 
  group_by(family) %>% 
  mutate(MedianLight=median(light_score)) %>% 
  ggplot(aes(x=reorder(family, -light_score, FUN=median), y=light_score))+
  geom_boxplot(aes(fill=MedianLight))+
  scale_fill_gradient(low="NA", high="steelblue")+
  theme_minimal()+
  theme(panel.grid = element_blank(),
        legend.position = "NA")+
  coord_flip()+
  ylab("Light Score")+
  xlab("Family")+
  labs(title="Birds' collisions vary according\nto Light Score",
          caption="Source:\nhttps://doi.org/10.1098/rspb.2019.0364\nhttps://doi.org/10.5061/dryad.8rr0498")
