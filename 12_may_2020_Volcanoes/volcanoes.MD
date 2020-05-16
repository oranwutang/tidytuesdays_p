```markdown
library(tidyverse)
library(drlib)
library(ggtext)
library(extrafont)
# font_import()
# windowsFonts()
# loadfonts(device = "win")
# install.packages("remotes")
# remotes::install_github("dgrtwo/drlib") # drlibr includes the functions used for correctly sorting data within facets
# remotes::install_github("wilkelab/ggtext")

volcano <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-05-12/volcano.csv')
eruptions <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-05-12/eruptions.csv')

eruptions %>% 
  left_join(volcano, by="volcano_name") %>% 
  filter(region!="Alaska") %>%
  filter(!is.na(vei) & vei>0) %>% 
  group_by(region, volcano_name) %>% 
  summarise(median_vei=median(vei), Eruptions=n()) %>%
  top_n(median_vei, n=5) %>% 
  ungroup() %>% 
  ggplot(aes(x=drlib::reorder_within(x=volcano_name, by=median_vei, within = region), y=median_vei))+
  facet_wrap(vars(region), scales = "free_y", ncol = 3)+
  geom_col(aes(fill=Eruptions))+
  drlib::scale_x_reordered() + 
  coord_flip()+
  scale_fill_gradient2(low = "beige", mid = "yellow", high = "red", midpoint = 60)+
  theme_minimal()+
  theme(legend.position = "bottom", 
        text = element_text(color = "cornflowerblue",
                             family="Montserrat Subrayada"),
        plot.background = element_rect(fill="gray90"),
        plot.title = element_markdown(lineheight = 1.1, size = 41),
        plot.subtitle = element_markdown(size=15),
        plot.title.position = "plot",
        strip.background = element_rect(fill="darkolivegreen1"),
        strip.text = element_text(size=9.5),
        panel.grid.major.y = element_blank(),
        legend.key.width = unit(2, "cm"),
        legend.key.size = unit(.25, "cm"),
        axis.title = element_text(size = 15))+
  labs(x="", y="VEI (median)",
       title = "The most dangerous volcanoes by region",
       subtitle = "<span style = 'color:red;'>V</span>olcanic
       <span style = 'color:red;'>E</span>xplosivity <span style = 'color:red;'>I</span>ndex
       is a relative measure of the explosiveness of volcanic eruptions",
       caption = "plot by @oranwutan")+
  ggsave("volcanoes.png", height = 18, width = 15, dpi = 600)
  ```
  
  
