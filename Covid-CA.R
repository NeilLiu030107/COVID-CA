#loading libraries
library(tidyverse)
library(dplyr)
library(leaflet)
library(sp)
library(rgdal)
library(raster)
library(lubridate)
library(sf)
library(spdep)
library(cartography)

options(scipen=999)

###Part 1: Read data

# Statewide COVID-19 Data from https://data.ca.gov/dataset/covid-19-time-series-metrics-by-county-and-state1/resource/6a1aaf21-2a2c-466b-8738-222aaceaa168
data <- read.csv("covid-data/covid19cases_test.csv")

# Adding prevalence and fatility to data
data <- data %>%
  mutate(cases_per_1000= cumulative_cases/population*1000, fatality= cumulative_deaths/cumulative_cases)

data$fatality[which(is.nan(data$fatality))]<-0

#CA Boundaries from ca.gov
CA_state_boundary <- readOGR(dsn=".",layer="CA_State_TIGER2016")
CA_counties_boundary <- readOGR(dsn="./CA_Counties", layer="CA_Counties_TIGER2016")
cov_ca <-readOGR(dsn="./CA_Counties", layer="CA_Counties_TIGER2016")

##align crs coordinates with raster objects
CA_state_boundary <-spTransform(CA_state_boundary, crs(maxTemp))
CA_counties_boundary <-spTransform(CA_counties_boundary,crs(maxTemp))
cov_ca <- spTransform(cov_ca, crs(maxTemp))

#annual average max temperature! from cal-adapt GRIDMET OBSERVED METEROLOGICAL DATA (unit=K)
maxTemp <- raster("covid-data/tmmx_year_avg_2020.tif")

##align crs coordinates with raster objects
CA_state_boundary <-spTransform(CA_state_boundary, crs(maxTemp))
cov_ca <- spTransform(cov_ca, crs(maxTemp))

ca_maxTemp <- raster::crop(x=maxTemp, y=CA_state_boundary)
ca_maxTemp_mask <-raster::mask(x=ca_maxTemp, mask=CA_state_boundary)

#annual average min temperature resolution 0.04
minTemp <- raster("covid-data/tmmin_year_average_2020.tif")
ca_minTemp <- raster::crop(x=minTemp, y=CA_state_boundary)
ca_minTemp_mask <- raster::mask(x=ca_minTemp, mask=CA_state_boundary)

#yearly average precipitation! (unit=mm) resolution 0.04
rainfall <- raster("covid-data/pr_year_average_2020.tif") 
ca_rainfall <- raster::crop(x=rainfall, y=CA_state_boundary)
ca_rainfall_mask <- raster::mask(x=ca_rainfall, mask=CA_state_boundary)


#elevation resolution 0.00833
elev <- raster::getData(name="alt", country="USA")

usa<-elev[1]$`C:/Users/Neil Liu/Desktop/summer project/Covid-CA/USA1_msk_alt.grd`

ca_elev <- raster::crop(x=usa, y=CA_state_boundary)
plot(ca_elev)
ca_elev_mask <- raster::mask(x=ca_elev, CA_state_boundary)

#pop density!
pop_density <- read.csv("covid-data/pop_density.csv")
pop_density <- pop_density %>% 
  rename(region=ï..region) %>%
  select(region, popDensity)

#health coverage(unit min) resolution 0.008

#travel_time<- raster("covid-data/motorized_travel_time_to_healthcare.geotiff")
#ca_travel_time <- raster::crop(x=travel_time, y=CA_state_boundary)
#ca_travel_time_mask <- raster::mask(x=ca_travel_time, mask=CA_state_boundary)
#writeRaster(ca_travel_time_mask,"ca_travel_time_to_healthcare.tif")

ca_travel_time_mask <-raster("covid-data/ca_travel_time_to_healthcare.tif")


#health capacity 3/29
#https://raw.githubusercontent.com/datadesk/california-coronavirus-data/master/cdph-hospital-patient-county-totals.csv
hospital <- read.csv("covid-data/hospital.csv")
hospital <- hospital %>%
  mutate(icu_occupancy= (icu_positive_patients+icu_suspected_patients)/(icu_available_beds+icu_positive_patients+icu_suspected_patients)) %>%
  select(region, date, icu_occupancy)

#poverty rate according to California measure from PPIC
#poverty <- read.csv("covid-data/poverty-across-california.csv")

#poverty <- poverty %>%
  #rename(poverty_rate=Rate) %>%
  #select(region, poverty_rate)



#night light(development level) resolution 0.0008

#night_light <- raster("covid-data/usa_viirs_100m_2016.tif")
#ca_night_light <-raster::crop(x=night_light, y=CA_state_boundary)
#ca_night_light_mask <-raster::mask(x=ca_night_light, mask=CA_state_boundary)
#writeRaster(ca_night_light_mask,"ca_night_light.tif")

ca_night_light_mask<-raster("covid-data/ca_night_light.tif")

#fully vaccinated population 12/15
vax <- read.csv("covid-data/covid19vaccinesbycounty.csv")
vax <- vax %>%
  select(region, date, cumulative_fully_vaccinated)

#social distancing--from Apple--direction search frequency 1/13
ca_mobility_trend <- read.csv("covid-data/ca_mobility_trend.csv")
ca_mobility_trend <- ca_mobility_trend %>%
  group_by(region, date) %>%
  summarise(mobility_trend=mean(mobility_trend))
  

#match resolution for raster objects
ca_elev_mask <- resample(ca_elev_mask,ca_maxTemp_mask, method="bilinear")
ca_travel_time_mask <-resample(ca_travel_time_mask,ca_maxTemp_mask, method="bilinear")
ca_night_light_mask <- resample(ca_night_light_mask, ca_maxTemp_mask, method="bilinear")

#assign covariants to counties
cov_ca <- sp::merge(cov_ca, pop_density, by.x="NAMELSAD", by.y="region", all.x=TRUE)
cov_ca <- sp::merge(cov_ca, poverty, by.x="NAMELSAD", by.y="region", all.x=TRUE)
cov_ca$ppt <- raster::extract(ca_rainfall_mask, cov_ca, fun=mean, na.rm=TRUE)
cov_ca$mxTemp <- raster::extract(ca_maxTemp_mask, cov_ca, fun=mean, na.rm=TRUE)
cov_ca$minTemp <-raster::extract(ca_minTemp_mask, cov_ca, fun=mean, na.rm=TRUE)
cov_ca$elevation <-raster::extract(ca_elev_mask, cov_ca, fun=mean, na.rm=TRUE)
cov_ca$travel_time <-raster::extract(ca_travel_time_mask, cov_ca, fun=mean, na.rm=TRUE)
cov_ca$night_light <-raster::extract(ca_night_light_mask, cov_ca, fun=mean, na.rm=TRUE)

#join dataframes
ca_mobility_trend$X <-NULL
cov_data <- data %>%
  left_join(ca_mobility_trend) %>%
  left_join(hospital) %>%
  left_join(vax) %>%
  mutate(vax_ratio=cumulative_fully_vaccinated/population)
  

##Part.2 Visualization
#March 4th 2020 CA declared emergency
data_March042020 <- cov_data %>%
  filter(date=="3/4/2020") %>%
  select(date,region, cases, cases_per_1000, fatality, mobility_trend, icu_occupancy, vax_ratio)

#March 13th 2020 School closure
data_March132020 <- cov_data %>%
  filter(date=="3/13/2020") %>%
  select(date,region, cases, cases_per_1000, fatality, mobility_trend, icu_occupancy, vax_ratio)

#March 29th 
data_March292020 <- cov_data %>%
  filter(date=="3/29/2020") %>%
  select(date,region, cases, cases_per_1000, fatality, mobility_trend, icu_occupancy, vax_ratio)

#May 25 slowly reopening
data_May252020 <- cov_data %>%
  filter(date=="5/25/2020") %>%
  select(date,region, cases, cases_per_1000, fatality, mobility_trend, icu_occupancy, vax_ratio)

#June 18 mask mandate
data_June182020 <- cov_data %>%
  filter(date=="6/18/2020") %>%
  select(date,region, cases, cases_per_1000, fatality, mobility_trend, icu_occupancy, vax_ratio)

#July 13 business closed again
data_July132020 <- cov_data %>%
  filter(date=="7/13/2020") %>%
  select(date,region, cases, cases_per_1000, fatality, mobility_trend, icu_occupancy, vax_ratio)

#August 28 new tier system
data_August282020 <- cov_data %>%
  filter(date=="8/28/2020") %>%
  select(date,region, cases, cases_per_1000, fatality, mobility_trend, icu_occupancy, vax_ratio)

#Octo 14 deaths plummet
data_Octo142020 <- cov_data %>%
  filter(date=="10/14/2020") %>%
  select(date,region, cases, cases_per_1000, fatality, mobility_trend, icu_occupancy, vax_ratio)

#Nov 19 curfew
data_Nov192020 <- cov_data %>%
  filter(date=="11/19/2020") %>%
  select(date,region, cases, cases_per_1000, fatality, mobility_trend, icu_occupancy, vax_ratio)

#Dec 3 lockdown
data_Dec042020 <- cov_data %>%
  filter(date=="12/3/2020") %>%
  select(date,region, cases, cases_per_1000, fatality, mobility_trend, icu_occupancy, vax_ratio)

#Dec 15 vaccination
data_Dec152020 <- cov_data %>%
  filter(date=="12/15/2020") %>%
  select(date,region, cases, cases_per_1000, fatality, mobility_trend, icu_occupancy, vax_ratio)

#Dec 29 stay at home order renewed
data_Dec292020 <- cov_data %>%
  filter(date=="12/29/2020") %>%
  select(date,region, cases, cases_per_1000, fatality, mobility_trend, icu_occupancy, vax_ratio)

#Jan 25 2021 stay-at-home order lifted
data_Jan252021 <- cov_data %>%
  filter(date=="1/25/2021") %>%
  select(date,region, cases, cases_per_1000, fatality, mobility_trend, icu_occupancy, vax_ratio)

#March 1 2021 school reopening compromise
data_March012021 <- cov_data %>%
  filter(date=="3/1/2021") %>%
  select(date,region, cases, cases_per_1000, fatality, mobility_trend, icu_occupancy, vax_ratio)

#April 15 2021 Vaccination expanded to over 16
data_April152021 <- cov_data %>%
  filter(date=="4/15/2021") %>%
  select(date,region, cases, cases_per_1000, fatality, mobility_trend, icu_occupancy, vax_ratio)

#June 15 2021 CA reopen
data_June152021 <- cov_data %>%
  filter(date=="6/15/2021") %>%
  select(date,region, cases, cases_per_1000, fatality, mobility_trend, icu_occupancy, vax_ratio)

#June 27 2021 delta variant 
data_June272021 <- cov_data %>%
  filter(date=="6/27/2021") %>%
  select(date,region, cases, cases_per_1000, fatality, mobility_trend, icu_occupancy, vax_ratio)

#August 9
data_August092021 <- cov_data %>%
  filter(date=="8/9/2021") %>%
  select(date,region, cases, cases_per_1000, fatality, mobility_trend, icu_occupancy, vax_ratio)

##Mapping function
Choropleths <-function(data){
  #Join attributes
  Map <- sp::merge(CA_counties_boundary, data, by.x="NAMELSAD", by.y="region", all.x=TRUE)
  
  #color
  bins <-c(0,20,40,100,400,1000,2500,5000,10000,Inf)
  pal <- colorBin("YlOrRd", domain = Map$cases, bins = bins)
  
  #label
  labels <- sprintf(
    "<strong>%s</strong><br/>%g new case(s)",
    Map$NAME, Map$cases
  ) %>% lapply(htmltools::HTML)
  
  #mapping
  leaflet(Map) %>%
    setView(-119,37,4) %>%
    addTiles() %>%
    addPolygons(
      fillColor = ~pal(cases),
      weight = 2,
      color="white",
      dashArray = "3",
      opacity=1,
      fillOpacity = 0.7,
      label = labels,
      labelOptions = labelOptions(
        style = list("font-weight" = "normal", padding = "3px 16px"),
        textsize = "15px",
        direction = "auto")
    ) %>%
    addLegend(pal = pal, values = ~cases, opacity = 0.7, title = sprintf("New Case(s) on %s", data[1,1]))
}

Choropleths(data_March042020)
Choropleths(data_March292020)
Choropleths(data_May252020)
Choropleths(data_July132020)
Choropleths(data_Dec152020)
Choropleths(data_April152021)
Choropleths(data_June152021)
Choropleths(data_August092021)

##Part.3 Spatial Analysis
#summarise cov_data with average mobility trend and icu occupancy
data_1 <- cov_data %>%
  group_by(region) %>%
  summarise(mobility_trend=mean(mobility_trend, na.rm = TRUE),icu_occupancy=mean(icu_occupancy, na.rm=TRUE))
#August 10 data
data_2 <-data_August092021 %>%
  select(region, cases_per_1000, fatality, vax_ratio)
#data from spatial polygon data frame
data_3 <- cov_ca@data %>%
  select(NAMELSAD, popDensity, ppt, mxTemp, minTemp, elevation, travel_time, night_light)

#making a general dataset for analysis
data_general <- merge(data_1,data_2, by="region")
data_general <- merge(data_general, data_3, by.x="region", by.y="NAMELSAD")


#stepwise regression modelling


glm_mod_1 <- glm(cases_per_1000 ~ vax_ratio+mobility_trend+night_light+ppt+mxTemp+minTemp+elevation+popDensity, data=data_general, family="poisson")

summary(glm_mod_1)

#vaccination ratio, precipitation, population density, and max temperature are significant predictors of covid incidence rate.

glm_mod_1_vax <- glm(cases_per_1000 ~ vax_ratio,  data=data_general, family="poisson")

summary(glm_mod_1_vax)

glm_mod_1_precipitation<- glm(cases_per_1000 ~ ppt,  data=data_general, family="poisson")

summary(glm_mod_1_precipitation)

glm_mod_1_mxTemp <- glm(cases_per_1000 ~ mxTemp,  data=data_general, family="poisson")

summary(glm_mod_1_mxTemp)

glm_mod_1_popDensity <- glm(cases_per_1000 ~ popDensity,  data=data_general, family="poisson")

summary(glm_mod_1_popDensity)

glm_mod_1_elev <- glm(cases_per_1000 ~ elevation,  data=data_general, family="poisson")

summary(glm_mod_1_elev)

glm_mod_1_minTemp <-glm(cases_per_1000 ~ minTemp,  data=data_general, family="poisson")

summary(glm_mod_1_minTemp)

glm_mod_1_sig <- glm(cases_per_1000 ~ vax_ratio+ppt+mxTemp+minTemp+elevation+popDensity, data=data_general, family="poisson")

summary(glm_mod_1_sig)
#since p-value for minTemp is greater than 0.05, remove minTemp

glm_mod_1_sig_2 <- glm(cases_per_1000 ~ vax_ratio+ppt+mxTemp+elevation+popDensity, data=data_general, family="poisson")

summary(glm_mod_1_sig_2)
ggplot()+geom_point(aes(glm_mod_1_elev$fitted.values, data_general$cases_per_1000))

#fatality
glm_mod_2 <- glm(fatality ~ icu_occupancy+ vax_ratio+popDensity+ppt+mxTemp+minTemp+elevation+travel_time+night_light, data=data_general, family="binomial")

summary(glm_mod_2)
#No statistically significant predictor of covid fatality was found

#autocorrelation
ca_nb <- poly2nb(CA_counties_boundary)

ca_w<- nb2listw(ca_nb)

moran.test(data_general$cases_per_1000, listw= ca_w)
#spatial autocorrelation is rejected as p-value > 0.05
moran.test(data_general$fatality, listw= ca_w)
#spatial autocorrelation is rejected

#visualization of model and actual case

choroLayer(spdf=CA_counties_boundary, df=data_general, 
           spdfid="NAMELSAD", dfid="region",
           var="cases_per_1000", legend.pos = "bottomleft", 
           legend.horiz = T, legend.title.txt = "Cumulative cases per 1000 people")
title("Observed Cases Count")

data_general$fitted_glm <- fitted(glm_mod_1_sig_2)

choroLayer(spdf=CA_counties_boundary, df=data_general, 
           spdfid="NAMELSAD", dfid="region",
           var="fitted_glm", legend.pos = "bottomleft", 
           legend.horiz = T, legend.title.txt = "Cumulative cases per 1000 people")
title("GLM model")



