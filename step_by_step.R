library(tidyverse)
library(gganimate)
library(readxl)
df = read_excel("data/gdp_ppp.xlsx")

df = gather(df, key = year,  value = "gdp", 3:31)
gdp_tidy = filter(df, code %in% c("POL", "DEU", "CZE", "KOR", "ESP", "WLD"))

#gdp_tidy <- read_csv("./data/gdp_tidy.csv")

test = filter(gdp_tidy, year==2018)
ggplot(test, aes(x = country, y = gdp)) + 
  geom_bar(stat = 'identity') 


anim = ggplot(gdp_tidy2, aes(x = as.factor(rank), y = gdp, fill = as.factor(country))) + 
  geom_bar(stat = 'identity') +
  coord_flip() + 
  geom_text(aes(y = gdp, label = label), hjust = 2, size= 5, col = 'white')+
  theme(axis.text=element_text(size=14),
        legend.position = "none") +
  labs(title = 'GDP PPP per Year : {closest_state}',  
       subtitle  =  "For selected 6 countries",
       caption  = "GDP PPP in USD/capita | Data Source: World Bank Data") +
  transition_states(
    year,
    transition_length = 2,
    state_length = 1
  )

install.packages("gifski")
library(gifski)
animate(anim, 100, fps = 10,  width = 1200, height = 1000, 
        renderer = gifski_renderer("gganim.gif")) 

# For MP4
animate(anim, 200, fps = 10,  width = 900, height = 750, 
        renderer = ffmpeg_renderer()) -> for_mp4

anim_save("animation.mp4", animation = for_mp4 )



gdp_tidy2 = gdp_tidy %>% group_by(year) %>%  mutate(rank = rank(gdp)) %>% arrange(year, rank)
gdp_tidy2$label = paste(gdp_tidy2$country, round(gdp_tidy2$gdp))

getwd()
