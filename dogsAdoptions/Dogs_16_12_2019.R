# install.packages("devtools")
# devtools::install_github("tidyverse/ggplot2")
# devtools::install_github("clauswilke/ggtext")

library(tidyverse)
library(lubridate)
library(ggtext) #Permite poner markdown y colores a palabras específicas en los títulos, versión beta.


extrafont::loadfonts(device = "win")

dog_moves <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-12-17/dog_moves.csv')
# dog_travel <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-12-17/dog_travel.csv')
# dog_descriptions <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-12-17/dog_descriptions.csv')

# colores <- as.character( colors())
# colores[str_detect(colores, "pink")]

#Plot 1 Recuento de perros por ubicación ----
colScale <- scales::seq_gradient_pal(low="red", high = "khaki")(seq(0,1,length.out=51))

dog_moves %>% filter(!is.na(total)) %>%
  mutate(location=fct_reorder(location, total)) %>% 
  ggplot(aes(x=location, fill=location, y=total)) + 
  geom_col(width = 0.5) +
  geom_hline(yintercept = median(dog_moves$total, na.rm=TRUE), color="red", linetype=2, size=1)+
  coord_flip()+
  labs(x="", title="Where are more available dogs for adoption?", y="",
       caption = "<b style='color:red'>- - -</b> Median of count  
       Source: *The Pudding*  
       *@oranwutan*") + 
  theme_minimal()+
  theme(legend.position = "NULL",
        text = element_text(family = "Roboto", colour = "grey90"),
        axis.text = element_text(color="grey90"),
        panel.grid = element_blank(), 
        plot.background = element_rect(fill="grey20"), 
        plot.title.position = "plot", #esta línea es posible por ser la versión development de ggplot2
        plot.title = element_markdown(lineheight = 1.6, face = "bold"),
        plot.caption = element_markdown(face="bold"))+
  scale_fill_manual(values = colScale)


#Plot 2 Balanza de exportación importación ----

dog_moves %>% 
  filter_all(all_vars(!is.na(.))) %>% #quito todos los datos na 
  mutate(resta=imported-exported) %>%
  mutate(location=fct_reorder(location, resta)) %>% 
  ggplot(aes(x=location))+
  geom_col(aes(x=location, y=exported*-1, fill="Exported"), alpha=0.4, width = 0.4, )+
  geom_col(aes(x=location, y=imported, fill="Imported"), alpha=0.4, width = 0.4)+
  geom_segment(aes(y=0, yend=resta, xend=location, color="Net Gain"), size=1.5)+
  theme_minimal()+
  theme(text = element_text(family = "Roboto", colour = "grey90"),
        axis.text = element_text(color="grey90"),
        panel.grid = element_blank(),
        plot.background = element_rect(fill="grey10"),
        plot.title.position = "plot", #esta línea es posible por ser la versión development de ggplot2
        plot.caption.position = "plot",
        plot.title = element_markdown(face = "bold", size = 18),
        plot.caption = element_markdown(face="bold"))+
  scale_colour_manual(name="", values=c("Net Gain"="yellow"))+
  scale_fill_manual(name="", values=c("Exported"="red", Imported="white"))+
  labs(x="", 
       y="", 
       title="Net gain of puppies by location  ",
       subtitle = "  ",
       caption = "Source: *The Pudding*  
       <b style='color:green'>*@oranwutan*<b>")+
  coord_flip()
