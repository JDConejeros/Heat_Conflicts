# Code 6: Poisson models extra ----

rm(list=ls())
## Settings ----
source("Code/0.1 Functions.R")
source("Code/0.2 Settings.R")

# Data path 
data_inp <- "Data/Input/"
data_out <- "Data/Output/"

#ref <- "crim.temp.df3.RData"
#ref <- rio::import(paste0(data_out, ref)) 

## Open Data -----
crime <- "data_crime_tmax_2005_2010.RData"
crime <- rio::import(paste0(data_out, crime)) 
glimpse(crime)

## Prepare count data -----

temp <- crime |> 
  mutate(week = floor_date(date_crime, unit = "week", week_start = 1)) |> 
  relocate(week, .after = month) |> 
  dplyr::select(cod_mun, id_mun, name_mun, date_crime, year, month, week, day_month, day_week, weekends, sup, tmax:tmax_group) |> 
  distinct()

crime_count <- crime |> 
  group_by(cod_mun, name_mun, date_crime, crime_6) |> 
  summarise(crime_count = n()) |> 
  pivot_wider(names_from = crime_6, values_from = crime_count) |> 
  janitor::clean_names() 

crime_count <- temp |> 
  left_join(crime_count, by=c("cod_mun", "name_mun", "date_crime")) |> 
  arrange(cod_mun, date_crime) |> 
    mutate(
      robbery = replace_na(robbery, 0),   
      larceny = replace_na(larceny, 0),   
      vehicle_theft = replace_na(vehicle_theft, 0),   
      burglary = replace_na(burglary, 0),   
      injuries = replace_na(injuries, 0),   
      intrafamily_violence = replace_na(intrafamily_violence, 0)
    )

glimpse(crime_count)

## Count crimes -----

crime_count |> 
  summarise(
    robbery = sum(robbery, na.rm = TRUE),
    larceny = sum(larceny, na.rm = TRUE),
    vehicle_theft = sum(vehicle_theft, na.rm = TRUE),
    burglary = sum(burglary, na.rm = TRUE),
    injuries = sum(injuries, na.rm = TRUE),
    intrafamily_violence = sum(intrafamily_violence, na.rm = TRUE)
  )


## Models -----

generate_poisson_models <- function(data, out_path, dep_vars, indep_var, cluster_var) {
  
  # Lista para almacenar modelos
  models <- list()
  
  # Efectos fijos completos
  fixed_effects <- c("id_mun", "year", "month", "weekends")
  fe_formula <- paste(fixed_effects, collapse = " + ")

  # Iterar sobre cada variable dependiente
  for (dep_var in dep_vars) {
    
    # Construcción de la fórmula del modelo
    formula <- as.formula(paste(dep_var, "~", indep_var, "|", fe_formula))
    
    # Estimación del modelo Poisson con efectos fijos y errores agrupados
    models[[dep_var]] <- fepois(formula, cluster = cluster_var, data = data)
  }

  # Guardar salida en tabla
  modelsummary(models,
               stars = TRUE, 
               fmt = 4,
               output = out_path,  
               align = "lcccccc")

  return(models)
}

generate_poisson_models(
  data = crime_count, 
  out_path = "Output/Models/Models_tmax_crime.docx", 
  dep_vars = c("robbery", "larceny", "vehicle_theft", "burglary", "injuries", "intrafamily_violence"), 
  indep_var = "tmax",
  cluster_var = "cod_mun"
)
