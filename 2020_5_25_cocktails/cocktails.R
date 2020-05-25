library(tidyverse)
library(ggrepel)

cocktails <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-05-26/cocktails.csv')
boston_cocktails <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-05-26/boston_cocktails.csv')

boston_cocktails %>% 
  filter(str_detect(measure, "oz")) %>% 
  group_by(measure) %>% 
  count() %>% 
  view()


fractionToDecimal <- function(fraction) {
  
  if_else(!str_detect(fraction, "/"),
          fraction,
          head(str_locate_all(boston_cocktails$measure, "/"))
          
          )
  
}

fractionToDecimal(boston_cocktails$measure)

reemplazo <- str_replace(boston_cocktails$measure, "oz", "")

reemplazo <- str_trim(reemplazo, side = "right")

reemplazo <- str_replace(reemplazo, " ", "+")

reemplazo[reemplazo=="1/2+or 1"] <- 1

reemplazo[reemplazo=="5+or 6"] <- 1

reemplazo[reemplazo=="1c"] <- 1

reemplazo[reemplazo=="2+-  3 drops"] <- 1
# solving <- function(x) eval(parse(text=x))


eval(parse(text=reemplazo))

eval(parse(text=reemplazo), )
solving(reemplazo)

sapply(frac, function(x) eval(parse(text=x)))


# Obtener 6 que tengan Whiskey
whiskeys <- boston_cocktails %>% 
  filter(str_detect(measure, "oz")) %>% 
  filter(str_detect(ingredient, pattern = "Whiskey|whiskey")) %>% 
  group_by(ingredient) %>% 
  count() %>% 
  filter(n>5) 
whiskeys <- whiskeys$ingredient

# Reparar la columna measure
for_plot <- boston_cocktails %>% 
  filter(ingredient %in% whiskeys) %>% 
  mutate(quantity=str_replace(measure, "oz", ""),
         quantity=str_trim(quantity, side = "right"),
         quantity=str_replace(quantity, " ", "+"))

for_plot$quantity <-  map_dbl(for_plot$quantity, ~eval(parse(text = .x)))

# for_plot %>% 
#   ggplot(aes(x =ingredient, y=quantity ))+
#   geom_text(aes(label=name))

niveles <- levels(as.factor(for_plot$measure))
cantidad <- as.numeric(levels(as.factor(for_plot$quantity)))


#Hay nombres repetidos con guisquies diferentes, hay que elimianr los repetidos
for_plot %>% 
  # filter(name==unique(name)) %>% 
  ggplot(aes(x =ingredient, y=quantity, label=name, color=as.factor(quantity)))+
  geom_text_repel(direction="y", segment.color="NA")+ggsave("prueba.png", width = 10, height = 50)
  scale_y_discrete(breaks=cantidad)

breaks = c(3, 6.5, 9), labels = c('a', 'b', 'c')