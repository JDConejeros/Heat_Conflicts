# Code 3: Join data ----

rm(list=ls())
## Settings ----
source("Code/0.1 Functions.R")
source("Code/0.2 Settings.R")

# Data path 
data_inp <- "Data/Input/"
data_out <- "Data/Output/"

## Load data ---- 

crime <- "data_crime_2005_2010.RData"
cr2 <- "hw_data_1980_2021.RData"
#sinca <- "series_daily_cont12h_2000_2023_wide.RData"
#sinca_max <- "crim.temp.df3.RData"
#zc <- "zc_geometry.RData"
#mun <- "mun_geometry.RData"
  
## Open data -----
crime <- rio::import(paste0(data_out, crime)) %>% janitor::clean_names() |> filter(year>=2005 & year <=2010) 
cr2 <- rio::import(paste0(data_out, cr2)) %>% janitor::clean_names() |> filter(year>=2005 & year <=2010)
#sinca <- rio::import(paste0(data_out, sinca)) %>% janitor::clean_names() |> filter(year>=2005 & year <=2010)
#sinca_max <- rio::import(paste0(data_out, sinca_max)) %>% janitor::clean_names() 

# Prepare data
crime <- crime |> arrange(cod_mun, date_crime)

vars_drop <- c("name_com", "lat", "long", "year_month", "year", "month", "day")
cr2 <- cr2 |> dplyr::select(!all_of(vars_drop))

glimpse(crime)
glimpse(cr2)
#glimpse(sinca)
#glimpse(sinca_max)

## Join data -----

crime <- crime |> 
  left_join(cr2, by=c("cod_mun"="com", "date_crime"="date"))

crime <- crime |> drop_na()

crime <- crime |> 
  mutate(id_mun = str_pad(as.integer(factor(cod_mun)), width = 2, pad = "0")) |> 
  relocate(id_mun, .before = cod_mun)

glimpse(crime)
summary(crime)

# Save data 
save(crime, file=paste0(data_out, "data_crime_tmax_2005_2010", ".RData"))
