#loading libraries
library(tidyverse)
library(dplyr)
library(leaflet)
library(sp)
library(rgdal)
library(raster)
library(lubridate)


###Part 1: Read data

# Statewide COVID-19 Data from https://data.ca.gov/dataset/covid-19-time-series-metrics-by-county-and-state1/resource/6a1aaf21-2a2c-466b-8738-222aaceaa168
data <- read.csv("covid-data/covid19cases_test.csv")

# Adding prevalence and mortality to data
data <- data %>%
  mutate(cases_per_1000= cumulative_cases/population*1000, fatality= cumulative_deaths/cumulative_cases)

#CA Boundaries from ca.gov
CA_state_boundary <- readOGR(dsn=".",layer="CA_State_TIGER2016")
cov_ca <-readOGR(dsn="./CA_Counties", layer="CA_Counties_TIGER2016")

##align crs coordinates with raster objects
CA_state_boundary <-spTransform(CA_state_boundary, crs(maxTemp))
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
  rename(region=ï..Region) %>%
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
poverty <- read.csv("covid-data/poverty-across-california.csv")
poverty <- poverty %>%
  select(region, Rate)

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

#match resolution for raster objects
ca_elev_mask <- resample(ca_elev_mask,ca_maxTemp_mask, method="bilinear")
ca_travel_time_mask <-resample(ca_travel_time_mask,ca_maxTemp_mask, method="bilinear")
ca_night_light_mask <- resample(ca_night_light_mask, ca_maxTemp_mask, method="bilinear")

#assign covariants to counties
cov_ca <- merge(cov_ca, pop_density, by.x="NAMELSAD", by.y="region", all.x=TRUE)
cov_ca <- merge(cov_ca, poverty, by.x="NAMELSAD", by.y="region", all.x=TRUE)
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
  left_join(vax)
  

##Part.2 Visualization


##Part.3 Spatial Analysis
