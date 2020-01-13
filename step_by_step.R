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



gdp_tidy2 = gdp_tidy %>% group_by(year) %>%   arrange(year, gdp)
gdp_tidy2$rank = 6:1
gdp_tidy2$label = paste(gdp_tidy2$country, round(gdp_tidy2$gdp))

getwd()


anim = ggplot(gdp_tidy2, aes(rank, group = country, 
               fill = as.factor(country), color = as.factor(country))) +
  geom_tile(aes(y = gdp/2, height = gdp, width = 0.9), alpha = 0.8, color = NA) +
  geom_text(aes(y = 0, label = country), vjust = 0.2, hjust = 1, size=5) +
  geom_text(aes(y=gdp,label = round(gdp), hjust=0, size = 6)) +
  coord_flip(clip = "off", expand = FALSE) +
  scale_y_continuous(labels = scales::comma) +
  scale_x_reverse() +
  guides(color = FALSE, fill = FALSE) +
  theme(axis.line=element_blank(),
        axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        legend.position="none",
        panel.background=element_blank(),
        panel.border=element_blank(),
        panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        panel.grid.major.x = element_line( size=.1, color="grey" ),
        panel.grid.minor.x = element_line( size=.1, color="grey" ),
        plot.title=element_text(size=25, hjust=0.5, face="bold", colour="grey", vjust=-1),
        plot.subtitle=element_text(size=18, hjust=0.5, face="italic", color="grey"),
        plot.caption =element_text(size=12, hjust=0.5, face="italic", color="grey"),
        plot.background=element_blank(),
        plot.margin = margin(2,2, 2, 4, "cm")) +
  transition_states(year, transition_length = 4, state_length = 1) +
  view_follow(fixed_x = TRUE)  +
  labs(title = 'GDP PPP per Year : {closest_state}',  
       subtitle  =  "Chosen 5 countries on the background of world average",
       caption  = "GDP PPP in USD/capita | Data Source: World Bank Data") 

for_mp4 = animate(anim, 200, fps = 10,  width = 900, height = 750, 
        renderer = ffmpeg_renderer()) 
anim_save("animation.mp4", animation = for_mp4 )
