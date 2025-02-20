# Code 5: Poisson models ----

rm(list=ls())
## Settings ----
source("Code/0.1 Functions.R")
source("Code/0.2 Settings.R")

# Data path 
data_inp <- "Data/Input/"
data_out <- "Data/Output/"

ref <- "crim.temp.df3.RData"
ref <- rio::import(paste0(data_out, ref)) 

## Open Data -----
crime <- "data_crime_tmax_2005_2010.RData"
crime <- rio::import(paste0(data_out, crime)) 
glimpse(crime)

## Prepare count data for the models -----

temp <- crime |> select(cod_mun, name_mun, date_crime, year, month, day_month, day_week, weekends, sup, tmax:tmax_group) |> 
  distinct()

crime_count <- crime |> 
  group_by(cod_mun, name_mun, date_crime, ifv) |> 
  summarise(crime_count = n()) |> 
  filter(ifv==1) |> 
  ungroup() |> 
  left_join(temp, by=c("cod_mun", "name_mun", "date_crime"))

glimpse(crime_count)


m1 <- glm(crime_count ~ tmax + factor(cod_mun), family = poisson, data = crime_count)
m2 <- glm(crime_count ~ tmax + factor(cod_mun) + factor(year), family = poisson, data = crime_count)
m3 <- glm(crime_count ~ tmax + factor(cod_mun) + factor(year) + factor(month), family = poisson, data = crime_count)
m4 <- glm(crime_count ~ tmax + factor(cod_mun) + factor(year) + factor(month) + factor(weekends), family = poisson, data = crime_count)

screenreg(l=list(m1, m2, m3, m4), digits = 4)


# Lista para almacenar los modelos
crim_multfix_prop = list()

all_FEs = c("cod_mun", "year", "month", "weekends")

for(i in 1:length(all_FEs)){
  crim_multfix_prop[[i]] = fixest::fepois(crime_count ~ tmax_group, crime_count, fixef = all_FEs[1:i])
}

etable(crim_multfix_prop, cluster = ~ cod_mun)
