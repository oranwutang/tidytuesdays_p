library(tidyverse)
library(ggalluvial)
library(emojifont)
# Install font (run once):
# sysfonts::font.add.google("Notable", family = "Notable", regular.wt = 400, bold.wt = 400,
#                 repo = "http://fonts.gstatic.com/", handle = curl::new_handle())


characters <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-06-30/characters.csv')

characters$character <- str_remove_all(characters$character, pattern = "=.*")
characters$character <- str_trim(characters$character, side = "right")
characters$character <- str_remove(characters$character, "\\(scientist helper\\)")
characters$kiss_with_which_character <- str_remove_all(characters$kiss_with_which_character, "\\*")
# Removing "x#" (numbers of kisses in the same chapter) (the regex selects from x followed by a number)
characters$kiss_with_which_character <- str_remove(characters$kiss_with_which_character, "x(?=[:digit:]).*")
characters$kiss_with_which_character <- str_replace(characters$kiss_with_which_character, "Cylops", "Cyclops")


characters %>% select(character, kiss_with_which_character) %>% 
  group_by(character, kiss_with_which_character) %>% 
  filter(!is.na(kiss_with_which_character)) %>% 
  separate_rows(kiss_with_which_character, sep=",") %>% 
  mutate(character = str_trim(character, side = "right"),
         character = str_replace(character, "Ariel/Sprite/Shadowcat", "Ariel"),
         character = str_replace(character, "Marvel Girl/Phoenix", "Phoenix"),
         character = str_replace(character, "\\(2\\)", ""),
         kiss_with_which_character=str_trim(kiss_with_which_character, "both"),
         kiss_with_which_character=str_replace(kiss_with_which_character, 
                                               "(Kitty Pryde Future Self)|(Kitty Pyde)", "Kitty Pryde"),
         kiss_with_which_character=str_replace(kiss_with_which_character, 
                                               "(Madelyn Pryor)|(Madelyne  Pryor)", "Madelyne Pryor"),
         kiss_with_which_character=str_replace(kiss_with_which_character, 
                                               "(Moia MacTaggart)|(Moira Mactaggart)|(Moira MacTaggart)",
                                               "Moira MacTaggert"))  %>%
  group_by(character, kiss_with_which_character) %>%
  count() %>% 
  ggplot(aes(x=character, y=fct_rev(kiss_with_which_character)))+
    geom_text(aes(size=n, color=n),family="EmojiOne", label=emoji("heart"), vjust=0)+
    scale_size_area()+
    labs(title = "Mutants do have a heart",
         subtitle = "How often does a mutant kiss another?",
         caption = "Source: #TidyTuesday
          by @oranwutan",
         x="",
         y="")+
    theme_minimal()+
    theme(text = element_text(family = "Notable"),
          plot.title.position = "plot",
          legend.position = "bottom",
          axis.text.x=element_text(angle=45, vjust = 1, hjust=1),
          axis.text = element_text(color="black", family = "Notable" ),
          panel.grid = element_line(size=0.1))+
    scale_color_gradient(low="orangered", high = "orangered3")+ 
    guides(color = guide_legend("Number of \nkisses"), size = guide_legend("Number of \nkisses"))+
    ggsave("X-Men 29_jun_2020/x-men.png", height = 12, width = 11, dpi=90)
