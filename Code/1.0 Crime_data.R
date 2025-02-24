# Code 1: Birth data preparation ----
rm(list=ls())
## Settings ----
source("Code/0.1 Functions.R")
source("Code/0.2 Settings.R")

# Data path 
data_inp <- "Data/Input/"
data_out <- "Data/Output/"

## Crime data select ---- 

# Open data in R
vars <- c("idevent3",
          #"comuna",
          #"descrip_co",
          "cod_com",
          "plancdt_rm", 
          #"delito",
          "del_agr7", 
          "typecrime", 
          "dmcs_vif",
          #"cod_lug", 
          "fech_del", 
          "hr",
          #"hr_del",
          # Coordinates 
          "point_x",
          "point_y",
          "zc" # Near FID
          )

# "del_agr7"
#1   1_Robo con violencia o intimidaci�n
#2   2_Robo por sorpresa
#3   3_Robo de veh�culo motorizado
#4   4_Robo de objetos de o desde veh�culo
#5   5_Robo en lugar habitado
#6   6_Robo en lugar no habitado
#7   7_Otros robos con fuerza
#8   8_Abigeato
#9   9_Hurtos
#10   10_Lesiones menos graves, graves o grav�simas
#11   11_Lesiones leves
#12   12_Homicidios
#13   13_Violaciones
#14   14_Abusos sexuales
#15   15_Otros delitos sexuales
#16   16_VIF a mujer
#17   17_VIF a hombre
#18   18_VIF a ni�o o ni�a
#19   19_VIF a anciano o anciana
#20   20_VIF no clasificada
#21   21_Delitos de car�cter econ�mico
#22   22_Droga
#23   23_Secuestros
#24   24_Porte de armas
#25   25_Amenazas
#26   26_Receptaci�n
#27   27_Da�os
#28   28_Robo frustrado
#29   29_Muertes y hallazgos
#30   30_Suicidio
#31   31_Orden de aprehensi�n
#32   32_Tenencia de armas
#33   33_Atentados
#34   34_Tr�nsito
#99   99_No aplica

# "typecrime"
#1   1_Robbery
#2   2_Larceny
#3   3_Vehicle theft
#4   4_Burglary
#5   5_Injuries
#6   6_Intrafamily violence

data_crime <- rio::import(paste0(data_inp, "pre-001.dta")) |> 
  janitor::clean_names() |> 
  dplyr::select(all_of(vars))

glimpse(data_crime)

## Crime data new variables ---- 

# Time variables
data_crime <- data_crime |> 
  mutate(
    year = lubridate::year(fech_del), 
    month = lubridate::month(fech_del),
    day_month = lubridate::day(fech_del),
    day_week = lubridate::wday(data_crime$fech_del, label = TRUE, abbr = FALSE),
    weekends = if_else(day_week %in% c("Saturday", "Sunday"), 1, 0)
        ) 

# Spatial variables
comunas <- chilemapas::codigos_territoriales |>
  mutate(codigo_comuna=as.numeric(codigo_comuna),
         codigo_provincia=as.numeric(codigo_provincia),
         codigo_region=as.numeric(codigo_region)) |> 
  filter(codigo_region==13) |> 
  dplyr::select(codigo_comuna, nombre_comuna, codigo_provincia, nombre_provincia)

# Add district information 
data_crime <- data_crime |> 
  rename(codigo_comuna=cod_com) |> 
  left_join(comunas, "codigo_comuna")

# Adjust english name, crime variables and remove NA values 
glimpse(data_crime)

data_crime <- data_crime |> 
  rename(
    idevent = idevent3, 
    cod_mun = codigo_comuna,
    crime_20 = del_agr7, 
    crime_6 = typecrime, 
    ifv = dmcs_vif,  
    date_crime = fech_del,
    name_mun = nombre_comuna
  ) |> 
  mutate(ifv=if_else(ifv==2, 1, 
                    if_else(ifv==1, 0, NA_real_))) |> # VIF vs DMCS
  #mutate(ifv=factor(ifv, levels = c(0,1), labels=c("Don't IFV", "IFV")))
  mutate(crime_6=factor(crime_6, 
                        levels=1:6, 
                        labels=c("Robbery", "Larceny", "Vehicle theft", "Burglary", "Injuries", "Intrafamily violence"))) |> 
  dplyr::select(idevent, cod_mun, name_mun, ifv, crime_6, crime_20, 
         date_crime, year, month, day_month, day_week, weekends, 
         point_x, point_y)

# Adjust NA values 
data_crime <- data_crime |> 
  drop_na(ifv, point_x, point_y) # 1,344,140 crimes

# Save data 
save(data_crime, file=paste0(data_out, "data_crime_2005_2010", ".RData"))

## Geometries ---- 

# Data with municipality geometry 
geom_dis <- chilemapas::mapa_comunas |> 
  mutate(
    codigo_comuna=as.numeric(codigo_comuna), 
    codigo_provincia=as.numeric(codigo_provincia), 
    codigo_region=as.numeric(codigo_region)
  ) |> 
  filter(codigo_region==13)

save(geom_dis, file=paste0(data_out, "mun_geometry", ".RData"))

# Add geometry censal zone in 2017
# Extract data from: https://www.ine.gob.cl/herramientas/portal-de-mapas/geodatos-abiertos
geom_zones <- st_read(paste0(data_inp, "microdatos_manzana/", "Microdatos_Manzana.shp")) |> 
  janitor::clean_names() |> 
  filter(region=="REGIÓN METROPOLITANA DE SANTIAGO") |> 
  dplyr::select(cut, zona_censa, manzana, 
    total_pers, total_homb, total_muje, geometry) |> 
  mutate(
    total_homb=as.numeric(total_homb), 
    total_muje=as.numeric(total_muje)
  ) |> 
  rename(geometry_zone = geometry)

glimpse(geom_zones)

# Join both data (sf format)
geom_zones <- st_as_sf(geom_zones)

save(geom_zones, file=paste0(data_out, "zc_geometry", ".RData"))

# Add table with crimes 

glimpse(data_crime)

tab_crime <- data_crime |> 
  group_by(crime_6, crime_20) |> 
  summarise(n = n()) |> 
  ungroup() |> 
  group_by(crime_6) |> 
  mutate(porc = n/base::sum(n)) |> 
  ungroup()

tab_crime <- tab_crime |> 
  mutate(crime_20 = factor(crime_20,
  levels = c(1:34),
  labels = c("Robbery with violence or intimidation",
             "Pickpocketing",
             "Motor vehicle theft",
             "Theft of objects from vehicle",
             "Burglary (inhabited place)",
             "Burglary (uninhabited place)",
             "Other forceful thefts",
             "Cattle rustling",
             "Larceny",
             "Serious or severe injuries",
             "Minor injuries",
             "Homicides",
             "Rape",
             "Sexual abuse",
             "Other sexual offenses",
             "IPV against women",
             "IPV against men",
             "IPV against children",
             "IPV against elderly",
             "Unclassified IPV",
             "Economic crimes",
             "Drug-related offenses",
             "Kidnapping",
             "Illegal possession of firearms",
             "Threats",
             "Receiving stolen goods",
             "Vandalism",
             "Attempted robbery",
             "Deaths and discoveries",
             "Suicide",
             "Arrest warrants",
             "Illegal firearm possession",
             "Attacks",
             "Traffic offenses")))

writexl::write_xlsx(tab_crime, "Output/Tables/Tab_crime.xlsx")
