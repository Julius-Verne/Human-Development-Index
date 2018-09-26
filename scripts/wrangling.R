## HDI 2017

summary(HDI_2017)
str(HDI_2017)

?as.factor()
HDI_2017$`HDI rank 2017` <- as.numeric(as.character(HDI_2017$`HDI rank 2017`))
HDI_2017$`HDI rank 2016` <- as.numeric(as.character(HDI_2017$`HDI rank 2016`))




## HDI_TIME

HDI_TIME$`HDI Rank 2017` <- as.numeric(as.character(HDI_TIME$`HDI Rank 2017`))
HDI_TIME$`CHANGE_HDI_Rank 2012-2017` <- as.numeric(as.character(HDI_TIME$`CHANGE_HDI_Rank 2012-2017`))


gathered <- HDI_TIME %>% 
  gather(, "HDI", 3:10) %>% 
  select(Countries,key, HDI)


change <- HDI_TIME %>% 
  mutate(`HDI-Rank-2012`= `HDI Rank 2017` + `CHANGE_HDI_Rank 2012-2017`)
