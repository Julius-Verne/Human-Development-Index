#HDI 17

ggplot(hdi.17, aes(x = HDI.17))+
  geom_histogram(binwidth = 0.01, color="white")+
  scale_x_continuous(breaks=seq(0.3,1,0.1), limits = c(0.3,1))+
  theme(panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_blank(),
        axis.text.x = element_blank())+
  theme_minimal()+
  labs(x="Human Development Index 2017",
       title="HDI Distribution in 2017",
       caption="Source: UNDP")+
  theme(plot.title = element_text(family = "Trebuchet MS", color="#666666", face="bold", size=24, hjust=0))+
  theme(axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 10)))+
  theme(axis.title.x = element_text(margin = margin(t = 20, r = 0, b = 10, l = 0)))+
  theme(plot.title = element_text(margin = margin(t = 20, r = 0, b = 20, l = 0)))+
  theme(plot.margin = unit(c(0,1,0,1), "cm"))

#######################################
hdi.regions <- hdi.regions %>% 
  mutate(Regions=fct_reorder(Regions, `Human Development Index (HDI) 2017`))

ggplot(hdi.regions, aes(x = Regions, y = `Human Development Index (HDI) 2017`, fill=Regions))+
  geom_bar(stat = "identity")+
  scale_y_continuous(breaks=seq(0,1,0.1), limits = c(0,1))+
  theme(panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_blank(),
        axis.text.x = element_blank())+
  theme_minimal()+
  scale_fill_manual( values = c( "Latin America and the Caribbean"="tomato",
                                 "Arab States"="gray",
                                 "East Asia and the Pacific"="gray",
                                 "Europe and Central Asia"="gray",
                                 "South Asia"="gray",
                                 "Sub-Saharan Africa"="gray"),
                     guide = FALSE )+
  labs(y="Human Development Index 2017",
       x="",
       title="HDI in 2017 per Region",
       caption="Source: UNDP")+
  coord_flip()+
  theme(plot.title = element_text(family = "Trebuchet MS", color="#666666", face="bold", size=24, hjust=0))+
  theme(axis.title.y = element_text(margin = margin(t = 0, r = 0, b = 0, l = 5)))+
  theme(axis.title.x = element_text(margin = margin(t = 20, r = 0, b = 10, l = 0)))+
  theme(plot.title = element_text(margin = margin(t = 20, r = 0, b = 20, l = 0)))+
  theme(plot.margin = unit(c(0,1,0,0), "cm"))



##########################################

install.packages("CGPfunctions")

?newggslopegraph

library(CGPfunctions)
newggslopegraph(
  dataframe =  hdi.change,
  Times =  Year,
  Measurement =  HDI,
  Grouping =  Countries,
  Title = "Estimates of Percent Survival Rates",
  SubTitle = "Based on: Edward Tufte, Beautiful Evidence, 174, 176.",
  Caption = NULL,
  YTextSize = 2.5,
  LineThickness = .5,
  LineColor = `CHANGE_HDI_Rank 2012-2017`)



# HDI Change

ggplot(hdi.change)+
  geom_line(aes(x=`Countries`,y=HDI, color=`CHANGE_HDI_Rank 2012-2017`))
  
  
  geom_histogram(binwidth = 0.01, color="white")+
  scale_x_continuous(breaks=seq(0.3,1,0.1), limits = c(0.3,1))+
  theme(panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_blank(),
        axis.text.x = element_blank())+
  theme_minimal()+
  labs(x="Human Development Index 2017",
       title="HDI Distribution in 2017",
       caption="Source: UNDP")+
  theme(plot.title = element_text(family = "Trebuchet MS", color="#666666", face="bold", size=24, hjust=0))+
  theme(axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 10)))+
  theme(axis.title.x = element_text(margin = margin(t = 20, r = 0, b = 10, l = 0)))+
  theme(plot.title = element_text(margin = margin(t = 20, r = 0, b = 20, l = 0)))+
  theme(plot.margin = unit(c(0,1,0,1), "cm"))



  
  ggplot(hdi.yearly.regions, aes(x=Year, y = HDI, color = Regions))+
    geom_point()+
    geom_line()

  #get some world data and geometries from Natural Earth
#exclude Antarctica
world <- st_as_sf(countries110) %>% 
  filter(sovereignt!="Antarctica")

?scale_fill_viridis
#get some world data and geometries from Natural Earth
#exclude Antarctica
world <- st_as_sf(countries110) %>% 
  filter(sovereignt!="Antarctica")
#join data with geometries

names <- read_csv(here("input-data","Names.csv"))
world <- left_join(world, names, by = c("name"="names"))

world.hdi <- left_join(world, hdi.17, by=c("HDI_NAMES"="Country"))

la.hdi <- subset(world.hdi, region_wb == "Latin America & Caribbean")


ggplot(world.hdi) +
  geom_sf(data=world, col="grey", fill=NA) +
  geom_sf(col="grey", aes(fill=HDI.17)) +
  coord_sf(crs="+proj=robin") +
  scale_fill_viridis(option = "viridis",
                     name='Human \nDevelopment \nIndex',
                     direction = 1,
                     begin = 0,
                     end = 1) +
  theme_minimal() +
  theme(panel.grid.major = element_line(colour = "transparent"),
        axis.text.x = element_blank(),
        axis.text.y = element_blank())+
  labs(x=NULL,
       y=NULL,
       title="Human Development Index in countries, 2017",
       subtitle="Course \"Introduction to R for Journalists\"",
       caption='Source: United Nation Development Report. Map: Natural Earth.')+
  theme(plot.title = element_text(family = "Trebuchet MS", color="#666666", face="bold", size=20, hjust=0))+
  theme(axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 10)))+
  theme(axis.title.x = element_text(margin = margin(t = 20, r = 0, b = 0, l = 0)))+
  theme(plot.title = element_text(margin = margin(t = 20, r = 0, b = 0, l = 0)))+
  theme(plot.margin = unit(c(0,1,0,1), "cm"))
