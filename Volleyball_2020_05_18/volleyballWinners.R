# remotes::install_github("rensa/ggflags") 

library(tidyverse)
library(countrycode)
library(ggflags)
library(extrafont)
library(patchwork)

# font_import()
# windowsFonts()
# loadfonts(device = "win")

vb_matches <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-05-19/vb_matches.csv', guess_max = 76000)

wp1countries <- vb_matches %>% 
  filter(circuit=="FIVB" & year>=2009) %>% 
  group_by(year) %>% 
  count(country=w_p1_country) %>% 
  top_n(wt = n, n = 5)

wp2countries <- vb_matches %>% 
  filter(circuit=="FIVB" & year>=2009) %>% 
  group_by(year) %>% 
  count(country=w_p2_country) %>% 
  top_n(wt = n, n = 5)

winners <- left_join(wp1countries, wp2countries, by = c("year", "country")) %>% 
  mutate(Country=
           tolower(
             countrycode(
               sourcevar = toupper(country), origin = "country.name.en", destination = "genc2c")),
         Total=n.x+n.y)

p1 <- winners %>% ggplot(aes(x=as.factor(year), y=n.x, country=Country))+
  geom_point(size=8, color="orange", alpha=0.7)+
  geom_flag(size=6)+
  theme_minimal()+
  theme(text = element_text(color = "cornflowerblue",
                            family="Russo One"),
        plot.title = element_text(lineheight = 1.1, size=12),
        plot.subtitle = element_text(lineheight = 1.1, size=9),
        axis.text = element_text(color = "cornflowerblue",
                                 family="Russo One"),
        panel.background = element_rect(fill = "black"),
        plot.background = element_rect(fill="black"),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.y = element_line(linetype = "longdash", color = "gray50"),
        plot.title.position = "plot")+
  labs(title = "Where are the best volleyball players from?",
       subtitle = "Sum of matches won by country",
       y="", x="")
  

p2 <- winners %>%
  group_by(Country) %>% 
  summarise(Total=sum(Total)) %>% 
  mutate(Country, Total) %>% 
  ggplot(aes(x=Total, y=reorder(Country, Total), country=Country))+
  geom_col(width = 0.3, fill="orange", alpha=0.4)+
  geom_flag(size=6)+
  labs(title = "Total matches won by country",
       subtitle = "Sum of matches won by country 2009-19",
       y="", x="")+
  theme_minimal()+
  theme(text = element_text(color = "cornflowerblue",
                            family="Russo One"),
        plot.title = element_text(lineheight = 1.1, size=12),
        plot.subtitle = element_text(lineheight = 1.1, size=9),
        axis.text = element_text(color = "cornflowerblue",
                                 family="Russo One"),
        axis.text.y = element_blank(),
        panel.background = element_rect(fill = "black"),
        plot.background = element_rect(fill="black"),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.x = element_line(linetype = "longdash", color = "gray50"),
        plot.title.position = "plot")



p1_2 <- p1/p2 + plot_annotation(title = 'International Volleyball Federation\nTop Performers',
  caption = "Plot by @oranwutan")& 
  theme(plot.background = element_rect(fill="black"),
    text = element_text(color = "cornflowerblue",
                            family="Russo One"))

ggsave(filename = "Volleyball_2020_05_18/FIVB.png", p1_2, dpi = 600, width = 4.35, height = 8.51)

