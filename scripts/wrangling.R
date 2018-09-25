## HDI 217

summary(HDI_2017)
str(HDI_2017)

?as.factor()
HDI_2017$`HDI rank 2017` <- as.numeric(as.character(HDI_2017$`HDI rank 2017`))
HDI_2017$`HDI rank 2016` <- as.numeric(as.character(HDI_2017$`HDI rank 2016`))




# import and select birth rate indicator from world bank
# https://datacatalog.worldbank.org/dataset/gender-statistics


#get some world data and geometries from Natural Earth
#exclude Antarctica
world <- st_as_sf(countries110) %>% 
  filter(sovereignt!="Antarctica")
#join data with geometries

names <- read_csv(here("input-data","Names.csv"))
world <- left_join(world, names, by = c("name"="names"))

world.birth.rates <- left_join(world, HDI_2017, by=c("HDI_NAMES"="Country"))

library(viridis)
library(ggrepel)
ggplot(world.birth.rates) +
  geom_sf(data=world, col="grey", fill=NA) +
  geom_sf(col="grey", aes(fill=`Human Development Index (HDI) 2017`)) +
  coord_sf(crs="+proj=robin") +
  scale_fill_viridis(option = "plasma",
                     name='Human \nDevelopment \nIndex',
                     direction = -1) +
  theme_minimal() +
  theme(panel.grid.major = element_line(colour = "transparent"),
        axis.text.x = element_blank(),
        axis.text.y = element_blank())+
  labs(x=NULL,
       y=NULL,
       title="World birth rates in countries by income, 2016",
       subtitle="Course \"Introduction to R for Journalists\"",
       caption='Source: Gender Statistics, The World Bank. Map: Natural Earth.')
