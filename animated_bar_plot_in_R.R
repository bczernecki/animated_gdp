library(tidyverse)
library(gganimate)
library(readxl)
df = read_excel("data/gdp_ppp.xlsx")

df = gather(df, key = year,  value = "gdp", 3:32)
colnames(df) = c('country', 'code', 'year', 'gdp')
gdp_tidy = filter(df, code %in% c("POL", "DEU", "CZE", "KOR", "ESP", "HUN","WLD"))

#gdp_tidy <- read_csv("./data/gdp_tidy.csv")

gdp_formatted <- gdp_tidy %>%
  group_by(year) %>%
  # The * 1 makes it possible to have non-integer ranks while sliding
  mutate(rank = rank(-gdp),
         Value_rel = gdp/gdp[rank==1],
         Value_lbl = paste0(" ",round(gdp))) %>%
  group_by(country) %>% 
  ungroup()

gdp_formatted$Value_lbl = round(gdp_formatted$gdp)
gdp_formatted = gdp_formatted[gdp_formatted$year<2019,]
# Animation

test = filter(gdp_formatted, year == 2018)
test$Value_lbl = round(test$gdp)

ggplot(gdp_formatted, aes(rank, group = country, 
                          fill = as.factor(country), color = as.factor(country))) +
  geom_tile(aes(y = gdp/2,
                height = gdp,
                width = 0.9), alpha = 0.8, color = NA) +
  geom_text(aes(y = 0, label = paste(country, " ")), vjust = 0.2, hjust = 1, size= 2.1) +
  #geom_text(aes(y=gdp,label = Value_lbl, hjust=0)) +
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
        plot.caption =element_text(size=15, hjust=0.5, face="italic", color="grey"),
        plot.background=element_blank(),
        plot.margin = margin(2,2, 2, 4, "cm")) +
  # view_follow(fixed_x = TRUE)  +
  labs(title = 'GDP PPP per Year : {closest_state}',  
       subtitle  =  "Top 10 Countries",
       caption  = "GDP PPP in USD/capita | Data Source: World Bank Data") +
  facet_wrap(~year)


# d

anim <- ggplot(gdp_formatted, aes(rank, group = country, 
                fill = as.factor(country), color = as.factor(country))) +
  geom_tile(aes(y = gdp/2,
                height = gdp,
                width = 0.9), alpha = 0.8, color = NA) +
  geom_text(aes(y = 0, label = paste(country, " ")), vjust = 0.2, hjust = 1, size=3) +
  geom_text(aes(y=gdp,label = Value_lbl, hjust=0)) +
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
         subtitle  =  "Top 10 Countries",
         caption  = "GDP PPP in USD/capita | Data Source: World Bank Data") 

# For GIF

animate(anim, 200, fps = 20,  width = 1200, height = 1000, 
        renderer = gifski_renderer("gganim.gif")) 

# For MP4

animate(anim, 200, fps = 10,  width = 900, height = 750, 
        renderer = ffmpeg_renderer()) -> for_mp4

anim_save("animation.mp4", animation = for_mp4 )
getwd()
