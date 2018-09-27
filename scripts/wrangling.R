## HDI 2017

summary(HDI_2017)
str(HDI_2017)

?as.factor()
HDI_2017$`HDI rank 2017` <- as.numeric(as.character(HDI_2017$`HDI rank 2017`))
HDI_2017$`HDI rank 2016` <- as.numeric(as.character(HDI_2017$`HDI rank 2016`))

hdi.17 <- HDI_2017 %>% 
  select(`HDI rank 2017`,Country,`Human Development Index (HDI) 2017`,`HDI rank 2016`)

##Measurements
meas.17 <- MEAS_2017[1:6,]

#World Average 2017
world.17 <- MEAS_2017[9,]

#HDI per region
regions.17 <- REG_2017[,1:2]


rm(HDI_2017,MEAS_2017,REG_2017)


## HDI_TIME

HDI_TIME$`HDI Rank 2017` <- as.numeric(as.character(HDI_TIME$`HDI Rank 2017`))
HDI_TIME$`CHANGE_HDI_Rank 2012-2017` <- as.numeric(as.character(HDI_TIME$`CHANGE_HDI_Rank 2012-2017`))


gathered <- HDI_TIME %>% 
  gather(, "HDI", 3:10) %>% 
  select(Countries,key, HDI)

gathered$key <- substring(gathered$key, 5)


change <- HDI_TIME %>% 
  mutate(`HDI-Rank-2012`= `HDI Rank 2017` + `CHANGE_HDI_Rank 2012-2017`) %>% 
  select(Countries,`HDI-Rank-2012`, `HDI Rank 2017`, `CHANGE_HDI_Rank 2012-2017`)
