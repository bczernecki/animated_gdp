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