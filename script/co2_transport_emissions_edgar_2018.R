#This code matches the EDGAR geospatial data sets to the 20 Latin American cities we are examining in ITHIM Global and produces and outputs the vehicle emissions inventory by each city in a text file
#SET UP

#load libraries
library(dplyr)
library(readr)
library(sf)
library(here)


######################
#PART 1: read EDGAR v6.1 emissions (2018 data)
######################

edgar_fname <- "EDGAR_v6.1_CO2_TRO_2018"

#read emissions from buses and view to check them
mode_bus <- read.table(file = here(edgar_fname, "monica_v6.1_EM_CO2_TRO_buses_CO2_2018.txt"), header = T, sep= ";", skip = 2)
colnames(mode_bus)[3] <- "co2_bus"

#read emissions from heavy trucks and view to check them
mode_htruck <- read.table(file = here(edgar_fname, "monica_v6.1_EM_CO2_TRO_heavy_duty_CO2_2018.txt"), header = T, sep=";", skip = 2)
colnames(mode_htruck)[3] <- "co2_htr"

#read emissions from light trucks and view to check them
mode_lttrucks <- read.table(file = here(edgar_fname, "monica_v6.1_EM_CO2_TRO_light_duty_CO2_2018.txt"), header = T, sep=";", skip = 2)
colnames(mode_lttrucks)[3] <- "co2_ltr"

#read emissions from motorcycles and mopeds and view to check them
mode_mtw <- read.table(file = here(edgar_fname, "monica_v6.1_EM_CO2_TRO_mopeds_motorcycles_CO2_2018.txt"), header = T, sep=";", skip = 2)
colnames(mode_mtw)[3] <- "co2_mtw"

#read emissions from passenger cars and view to check them
mode_car <- read.table(file = here(edgar_fname, "monica_v6.1_EM_CO2_TRO_cars_CO2_2018.txt"), header = T, sep=";", skip = 2)
colnames(mode_car)[3] <- "co2_car"

#merge mode emissions in one data.frame called "modes"
modes <- merge(mode_bus, mode_htruck, by = c("lat", "lon"), all = TRUE)
modes <- merge(modes, mode_lttrucks, by = c("lat", "lon"), all = TRUE)
modes <- merge(modes, mode_mtw, by = c("lat", "lon"), all = TRUE)
modes <- merge(modes, mode_car, by = c("lat", "lon"), all = TRUE)

#create spatial points 'sf' object 
modes <- modes %>% st_as_sf(coords = c("lon", "lat"), crs = "+proj=longlat +datum=WGS84")

#check if all observations are contained and view the data frame
sum(!is.na(modes$co2_bus))
sum(!is.na(modes$co2_htr))
sum(!is.na(modes$co2_ltr))
sum(!is.na(modes$co2_mtw))
sum(!is.na(modes$co2_car))

######################
#PART 2: add Latin American city names
######################
#read cities boundaries from the same directory in our shared ITHIM Global Dropbox folder. The shape file is also available on Github at https://github.com/ITHIM/ITHIM-R/blob/latam_paper/code/pm25%20emissions%20edgar/All%20Latin%20American%20Cities%20Boundaries.zip 
cities <- st_read(here("cities_shape_files", "cities.shp")) %>% st_zm() #drop Z coordinate
sf::sf_use_s2(FALSE)

#spatial join of attributes by location
modes <- st_join(modes, cities, join = st_intersects) 

#remove cities not in south america
modes <- modes[!is.na(modes$name),]

st_write(modes, dsn = here("shape_file", "CO2_2018_All_Cities.shp"), driver = "ESRI Shapefile", append=FALSE)


######################
#PART 3: get percentages of mode shares for each city
######################
#summarize mode shares found in same city (drop spatial geometry)
modes_analysis <- modes %>% group_by(name) %>% summarise(co2_bus = sum(co2_bus, na.rm = TRUE), co2_car = sum(co2_car, na.rm = TRUE), 
                                                         co2_htr = sum(co2_htr, na.rm = TRUE), co2_ltr = sum(co2_ltr, na.rm = TRUE),
                                                         co2_mtw = sum(co2_mtw, na.rm = TRUE)) %>% st_drop_geometry() 
#get percentage of mode shares
modes_analysis <- modes_analysis %>% group_by(name) %>% mutate(bus_share = round(co2_bus / sum(co2_bus, co2_car, co2_htr, co2_ltr, co2_mtw) * 100, 1),
                                                               car_share = round(co2_car / sum(co2_bus, co2_car, co2_htr, co2_ltr, co2_mtw) * 100, 1),
                                                               htr_share = round(co2_htr / sum(co2_bus, co2_car, co2_htr, co2_ltr, co2_mtw) * 100, 1),
                                                               ltr_share = round(co2_ltr / sum(co2_bus, co2_car, co2_htr, co2_ltr, co2_mtw) * 100, 1),
                                                               mtw_share = round(co2_mtw / sum(co2_bus, co2_car, co2_htr, co2_ltr, co2_mtw) * 100, 1))
# modes_analysis |> filter(name == "Bogota City") |> dplyr::select(name, ends_with("share")) |> tidyr::pivot_longer(cols = cols(-"name"))

#write output as .CSV file
readr::write_csv(modes_analysis, here("output", "CO2_2018_ALL_cities.csv"))
#note that only 18 cities will be outputted because data is missing for two cities. We used the EDGAR data from the closest city to assign missing data for these two cities
#Iquique and Alto Hospicio: use values from Antofagasta
#Temuco: use values from Valdivia
