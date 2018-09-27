## HDI 2017
#summary(hdi.17)
str(HDI_2017)

?as.factor()
HDI_2017$`HDI rank 2017` <- as.numeric(as.character(HDI_2017$`HDI rank 2017`))
HDI_2017$`HDI rank 2016` <- as.numeric(as.character(HDI_2017$`HDI rank 2016`))

hdi.17 <- HDI_2017 %>% 
  select(`HDI rank 2017`,Country,`Human Development Index (HDI) 2017`,`HDI rank 2016`) 


##Measurements
meas.17 <- MEAS_2017[,1:2]

hdi.17$Development <- NA  

hdi.17 <- hdi.17 %>% 
  mutate(Development = case_when(
    (`Human Development Index (HDI) 2017` < meas.17[3,2]$`Human Development Index (HDI) 2017` ) ~ "Low",
    (`Human Development Index (HDI) 2017` > meas.17[3,2]$`Human Development Index (HDI) 2017` &
       `Human Development Index (HDI) 2017` < meas.17[2,2]$`Human Development Index (HDI) 2017` ) ~ "Medium",
    (`Human Development Index (HDI) 2017` > meas.17[2,2]$`Human Development Index (HDI) 2017` &
       `Human Development Index (HDI) 2017` < meas.17[1,2]$`Human Development Index (HDI) 2017`)  ~ "High",
    (`Human Development Index (HDI) 2017` > meas.17[1,2]$`Human Development Index (HDI) 2017`) ~ "Very High"
  ))


#World Average 2017
hdi.world <- MEAS_2017[9,]

#HDI per region
hdi.regions <- REG_2017[,1:2]

#Delete Past DF



## HDI_TIME
HDI_TIME$`HDI Rank 2017` <- as.numeric(as.character(HDI_TIME$`HDI Rank 2017`))
HDI_TIME$`CHANGE_HDI_Rank 2012-2017` <- as.numeric(as.character(HDI_TIME$`CHANGE_HDI_Rank 2012-2017`))

summary(hdi.yearly)


hdi.yearly <- HDI_TIME %>% 
  gather(, "HDI", 3:10) %>% 
  select(Countries,key, HDI)

hdi.yearly$HDI <- as.numeric(as.character(hdi.yearly$HDI))

hdi.yearly$key <- substring(hdi.yearly$key, 5)
colnames(hdi.yearly)[2] <- "Year"


temp <- MEAS_TIME[1:4,1:9]

hdi.yearly$Development <- NA
hdi.yearly <- hdi.yearly %>% 
  mutate(Development = case_when(
    (HDI < temp[3,2]$`HDI 1990` & Year == 1990 ) ~ "Low",
    (HDI < temp[3,3]$`HDI 2000` & Year == 2000 ) ~ "Low",
    (HDI < temp[3,4]$`HDI 2010` & Year == 2010 ) ~ "Low",
    (HDI < temp[3,5]$`HDI 2012` & Year == 2012 ) ~ "Low",
    (HDI < temp[3,6]$`HDI 2014` & Year == 2014 ) ~ "Low",
    (HDI < temp[3,7]$`HDI 2015` & Year == 2015 ) ~ "Low",
    (HDI < temp[3,8]$`HDI 2016` & Year == 2016 ) ~ "Low",
    (HDI < temp[3,9]$`HDI 2017` & Year == 2017 ) ~ "Low",
    (HDI > temp[1,2]$`HDI 1990` & Year == 1990 ) ~ "Very High",
    (HDI > temp[1,3]$`HDI 2000` & Year == 2000 ) ~ "Very High",
    (HDI > temp[1,4]$`HDI 2010` & Year == 2010 ) ~ "Very High",
    (HDI > temp[1,5]$`HDI 2012` & Year == 2012 ) ~ "Very High",
    (HDI > temp[1,6]$`HDI 2014` & Year == 2014 ) ~ "Very High",
    (HDI > temp[1,7]$`HDI 2015` & Year == 2015 ) ~ "Very High",
    (HDI > temp[1,8]$`HDI 2016` & Year == 2016 ) ~ "Very High",
    (HDI > temp[1,9]$`HDI 2017` & Year == 2017 ) ~ "Very High",
    (HDI < temp[1,2]$`HDI 1990` & HDI > temp[2,2]$`HDI 1990` & Year == 1990 ) ~ "High",
    (HDI < temp[1,3]$`HDI 2000` & HDI > temp[2,3]$`HDI 2000` & Year == 2000 ) ~ "High",
    (HDI < temp[1,4]$`HDI 2010` & HDI > temp[2,4]$`HDI 2010` & Year == 2010 ) ~ "High",
    (HDI < temp[1,5]$`HDI 2012` & HDI > temp[2,5]$`HDI 2012` & Year == 2012 ) ~ "High",
    (HDI < temp[1,6]$`HDI 2014` & HDI > temp[2,6]$`HDI 2014` & Year == 2014 ) ~ "High",
    (HDI < temp[1,7]$`HDI 2015` & HDI > temp[2,7]$`HDI 2015` & Year == 2015 ) ~ "High",
    (HDI < temp[1,8]$`HDI 2016` & HDI > temp[2,8]$`HDI 2016` & Year == 2016 ) ~ "High",
    (HDI < temp[1,9]$`HDI 2017` & HDI > temp[2,9]$`HDI 2017` & Year == 2017 ) ~ "High",
    (HDI < temp[2,2]$`HDI 1990` & HDI > temp[3,2]$`HDI 1990` & Year == 1990 ) ~ "Medium",
    (HDI < temp[2,3]$`HDI 2000` & HDI > temp[3,3]$`HDI 2000` & Year == 2000 ) ~ "Medium",
    (HDI < temp[2,4]$`HDI 2010` & HDI > temp[3,4]$`HDI 2010` & Year == 2010 ) ~ "Medium",
    (HDI < temp[2,5]$`HDI 2012` & HDI > temp[3,5]$`HDI 2012` & Year == 2012 ) ~ "Medium",
    (HDI < temp[2,6]$`HDI 2014` & HDI > temp[3,6]$`HDI 2014` & Year == 2014 ) ~ "Medium",
    (HDI < temp[2,7]$`HDI 2015` & HDI > temp[3,7]$`HDI 2015` & Year == 2015 ) ~ "Medium",
    (HDI < temp[2,8]$`HDI 2016` & HDI > temp[3,8]$`HDI 2016` & Year == 2016 ) ~ "Medium",
    (HDI < temp[2,9]$`HDI 2017` & HDI > temp[3,9]$`HDI 2017` & Year == 2017 ) ~ "Medium"
  ))




#There 195 countries total

grouped.dev <- hdi.yearly %>% 
  group_by(Year,Development) %>% 
  summarize(count= n(), percentage = (count*100)/195)



hdi.change <- HDI_TIME %>% 
  mutate(`HDI-Rank-2012`= `HDI Rank 2017` + `CHANGE_HDI_Rank 2012-2017`) %>% 
  select(Countries,`HDI-Rank-2012`, `HDI Rank 2017`, `CHANGE_HDI_Rank 2012-2017`)





##Regions Time
hdi.yearly.regions <- REG_TIME %>% 
  gather(, "HDI", 2:9) %>% 
  select(Regions,key, HDI)

hdi.yearly.regions$key <- substring(hdi.yearly.regions$key, 5)
colnames(hdi.yearly.regions)[2] <- "Year"


##Regions Time
hdi.yearly.regions <- REG_TIME %>% 
  gather(, "HDI", 2:9) %>% 
  select(Regions,key, HDI)

hdi.yearly.regions$key <- substring(hdi.yearly.regions$key, 5)
colnames(hdi.yearly.regions)[2] <- "Year"



##Measurements Time
hdi.yearly.meas <- MEAS_TIME[1:4,1:9]

hdi.yearly.meas <- hdi.yearly.meas %>% 
  gather(, "HDI", 2:9) %>% 
  select(Countries,key, HDI)

hdi.yearly.meas$key <- substring(hdi.yearly.meas$key, 5)
colnames(hdi.yearly.meas)[2] <- "Year"

rm(HDI_2017,MEAS_2017,REG_2017, meas.17,
   HDI_TIME, MEAS_TIME,REG_TIME, temp, num)

