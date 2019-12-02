library(tidyverse)
library(lubridate)
library(ggmap)
library(osmdata)
library(cowplot)

tickets <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-12-03/tickets.csv")

# Unify categories by supressing "CC" 
tickets$Violation <- str_remove(tickets$violation_desc, " CC")

# Get the Philadelphia map
phi_map <- get_map(getbb("Philadelphia"))

# Get 5 most frequent categories of Violation
recuento <- tickets %>% count(Violation) %>% top_n(5)

#Subset tickets according to categories from previous step
tickets <- tickets[tickets$Violation %in% recuento$Violation,]

#Make the plots
p1 <- ggmap(phi_map)+geom_bin2d(aes(x=lon, y=lat, fill=Violation), 
                          data = tickets,
                          alpha=0.6)+
  scale_fill_viridis_d(option = "inferno")+
  theme_void(12)+
  # labs(title = "Top 5 Parking Violations in Philadelphia 2017")+
  theme(legend.title = element_blank(), legend.position = "NA")+
  facet_wrap(vars(Violation),nrow=1)

p2 <- ggmap(phi_map)+
  stat_density_2d(aes(x=lon, y=lat, fill = stat(nlevel)), geom = "polygon", data=tickets)+
  scale_fill_viridis_c(option = "inferno")+
  theme_void(12)+
  # labs(title = "Top 5 Parking Violations in Philadelphia 2017")+
  theme(legend.title = element_blank(), legend.position = "NA")+
  facet_wrap(vars(Violation),nrow=1)


# This is a workaround for making a common title for both plots
title <- ggdraw() + 
  draw_label("Top 5 Parking Violations in Philadelphia 2017",
    fontface = 'bold',
    x = 0, hjust = 0) +
  theme(plot.margin = margin(0, 0, 0, 7))

# Join the plots
plot_grid(title, p1, p2, labels = c("", 'A', 'B'), label_size = 12, nrow = 3,rel_heights = c(0.1, 1, 1))
