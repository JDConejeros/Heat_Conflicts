# Code 3: CASEN + NDVI data ----

rm(list=ls())
## Settings ----
source("Code/0.1 Functions.R")
source("Code/0.2 Settings.R")

# Data path 
data_inp <- "Data/Input/"
data_out <- "Data/Output/"

#ref <- "crim.temp.df3.RData"
#ref <- rio::import(paste0(data_out, ref)) 

## NDVI -----

# Open data and adjust variables 
ndvi <- "NDVI_2005_2010.csv"
green <- rio::import(paste0(data_inp, ndvi)) |> 
  dplyr::select(Comuna, `2005_01_01`:`2010_12_19`) |> 
  clean_names() |> 
  pivot_longer(cols = starts_with("x"), 
               names_to = "date", 
               values_to = "green_index") |> 
  mutate(date = str_remove(date, "x")) |> 
  mutate(date = as.Date(date, format = "%Y_%m_%d"))

# Edit dates and values
green <- green %>%
  group_by(comuna) %>%
  complete(date = seq(min(date), max(as.Date("2010-12-31")), by = "day")) %>%  # Generar todas las fechas diarias
  fill(green_index, .direction = "down") %>%  # Rellenar con el valor anterior
  ungroup()

# Municipality codes
com <- chilemapas::codigos_territoriales |> 
  mutate(nombre_comuna=stringr::str_to_title(nombre_comuna)) %>% 
  filter(codigo_region==13) %>%
  dplyr::select(1:2) %>% 
  mutate(codigo_comuna=as.numeric(codigo_comuna)) %>% 
  rename(name_com="nombre_comuna")

glimpse(com)

# Manual Homologation 
data_names <- data.frame(
  comuna = sort(unique(green$comuna)), 
  name_com = sort(unique(com$name_com))
)

glimpse(data_names)

green <- green |> 
  left_join(data_names, by=c("comuna")) |> 
  left_join(com, by=c("name_com")) |> 
  dplyr::select(-comuna) |> 
  dplyr::select(codigo_comuna, name_com, date, green_index)

glimpse(green)
summary(green)

save(green, file=paste0(data_out, "green_spaces_2005_2010", ".RData"))

