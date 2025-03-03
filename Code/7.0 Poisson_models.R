# Code 5: Poisson models ----

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

## Prepare count data for the models -----

temp <- crime |> dplyr::select(cod_mun, name_mun, date_crime, year, month, day_month, day_week, weekends, sup, tmax:tmax_group) |> 
  distinct()

crime_count <- crime |> 
  group_by(cod_mun, name_mun, date_crime, ifv) |> 
  summarise(ifv_count = n()) |> 
  filter(ifv==1) |> 
  dplyr::select(-ifv) |> 
  ungroup() 

crime_count <- temp |> 
  left_join(crime_count, by=c("cod_mun", "name_mun", "date_crime")) |> 
  arrange(cod_mun, date_crime) |> 
    mutate(
      ifv_count = replace_na(ifv_count, 0),   
    )

glimpse(crime_count)

## Models Checks -----

## Models function 
generate_poisson_models <- function(data, out_path, dep_var, indep_var, fixed_effects, cluster_var) {
  
  # Models
  models <- list()
  
  # Fixed effects 
  for (i in seq_along(fixed_effects)) {

    fe_formula <- paste(fixed_effects[1:i], collapse = " + ") 
    formula <- as.formula(paste(dep_var, "~", indep_var, "|", fe_formula))  
    models[[paste0("Model ", i)]] <- fepois(formula, cluster = cluster_var, data = data)
  
  }

  #coef_map <- setNames("Daily Max Temp", indep_var)
  
  # Table models
  modelsummary(models,
               stars = TRUE, 
               fmt = 4,
               output = out_path,  
               #coef_map = coef_map, 
               align = "lcccc")
  
  return(models)
}

# Estimate models
generate_poisson_models(
  data = crime_count,
  out_path = "Output/Models/Models_tmax.docx",  
  dep_var = "ifv_count",
  indep_var = "tmax",
  fixed_effects = c("factor(cod_mun)", "factor(year)", "factor(month)", "factor(weekends)"),
  cluster_var = ~cod_mun
)

# Check Binominal negative vs poisson 
generate_nb_models <- function(data, out_path, dep_var, indep_var, fixed_effects, cluster_var) {
  
  # Models and vcov matrix
  models <- list()
  vcov_list <- list()
  
  # Models with fixed effects
  for (i in seq_along(fixed_effects)) {
    fe_formula <- paste(fixed_effects[1:i], collapse = " + ")  # fixed effects
    formula <- as.formula(paste(dep_var, "~", indep_var, "+", fe_formula))  
    
    # BNM
    model_nb <- glm.nb(formula, data = data)
    
    # Cluster errors
    vcov_list[[paste0("Negative Binomial Model ", i)]] <- vcovCL(model_nb, cluster = data[[cluster_var]], type = "HC0")
    
    # Save models
    models[[paste0("Negative Binomial Model ", i)]] <- model_nb
  }
  
  # Coef_map
  #coef_map <- setNames("Daily Max Temp", indep_var)

  # Table with the models
  modelsummary(models,
               stars = TRUE, 
               fmt = 4,
               vcov = vcov_list,  
               output = out_path,  
               #coef_map = coef_map, 
               align = "lcccc")
}

generate_nb_models(
  data = crime_count,
  out_path = "Output/Models/models_NB_testing.docx", 
  dep_var = "ifv_count",
  indep_var = "tmax",
  fixed_effects = c("cod_mun", "year", "month", "weekends"),
  cluster_var = "cod_mun" 
)

# Conclussion: Poisson model has a better adjust respect BNM model

## Models TMAX -----

generate_poisson_models(
  data = crime_count,
  out_path = "Output/Models/models_tmax.docx",  
  dep_var = "ifv_count",
  indep_var = "tmax",
  fixed_effects = c("cod_mun", "year", "month", "weekends"),
  cluster_var = ~cod_mun
)

## Models TMAX group -----

tmax_group_models <- generate_poisson_models(
  data = crime_count,
  out_path = "Output/Models/models_tmax_group.docx",  
  dep_var = "ifv_count",
  indep_var = "relevel(tmax_group, ref = 3)",
  fixed_effects = c("cod_mun", "year", "month", "weekends"),
  cluster_var = ~cod_mun
)

# Figure with TMAX group (correct reference category)

tmax_g_coef <- tidy(tmax_group_models$`Model 4`, conf.int = TRUE, conf.level = 0.95)
tmax_g_coef <- rbind(tmax_g_coef, data.frame(term="relevel(tmax_group, ref = 3)18.3 - 21.1", 
                                              estimate=0, 
                                              std.error=0, 
                                              statistic=0, 
                                              p.value=0, 
                                              conf.low=0, 
                                              conf.high=0))
tmax_g_coef <- tmax_g_coef |> 
  mutate(term=factor(term, 
                     levels=c("relevel(tmax_group, ref = 3)<15.6" , 
                              "relevel(tmax_group, ref = 3)15.6 - 18.3", 
                              "relevel(tmax_group, ref = 3)18.3 - 21.1", 
                              "relevel(tmax_group, ref = 3)21.1 - 23.9",
                              "relevel(tmax_group, ref = 3)23.9 - 26.7", 
                              "relevel(tmax_group, ref = 3)26.7 - 29.4", 
                              "relevel(tmax_group, ref = 3)29.4 - 32.2", 
                              "relevel(tmax_group, ref = 3)32.2 - 35", 
                              "relevel(tmax_group, ref = 3)>35"), 
                     labels=c("<15.6", "15.6 - 18.3", "18.3 - 21.1", 
                              "21.1 - 23.9", "23.9 - 26.7", "26.7 - 29.4",
                              "29.4 - 32.2", "32.2 - 35", ">35")
  ))

ggplot(tmax_g_coef, aes(x = term, y = estimate)) + 
  geom_point(size = 1, fill="white", color="black") +
  geom_line(aes(x = term, y = estimate, group = 1), linewidth = 0.5) +
  geom_line(aes(x = term,y = conf.low, group = 1),linetype = "dotted")+
  geom_line(aes(x= term ,y = conf.high, group = 1),linetype = "dotted")+
  geom_hline(yintercept = 0, linetype = "dashed", color = "red", alpha = 0.5) +
  geom_text(aes(label = round(estimate, 3)), vjust = -0.75, hjust=0.5, size = 3) +
  scale_y_continuous(limits = c(-0.2, 0.3)) +
  labs(y="Estimated Effects", x="Temperature Interval") +
  theme_light() +
  theme(panel.grid = element_blank(),
        legend.position = "top",
        legend.text = element_text(size = 11), 
        axis.text.x = element_text(angle = 0))

ggsave(
  filename = paste0("Output/Models/Tmax_interval_effect.png"), 
  res = 300,
  width = 15,
  height = 9,
  units = 'cm',
  scaling = 0.85,
  device = ragg::agg_png)

## Percentile measures -----

# P90
generate_poisson_models(
  data = crime_count,
  out_path = "Output/Models/models_tmax_p90.docx",  
  dep_var = "ifv_count",
  indep_var = "hw_day_p90",
  fixed_effects = c("cod_mun", "year", "month", "weekends"),
  cluster_var = ~cod_mun
)

# P95
generate_poisson_models(
  data = crime_count,
  out_path = "Output/Models/models_tmax_p95.docx",  
  dep_var = "ifv_count",
  indep_var = "hw_day_p95",
  fixed_effects = c("cod_mun", "year", "month", "weekends"),
  cluster_var = ~cod_mun
)

# P99
generate_poisson_models(
  data = crime_count,
  out_path = "Output/Models/models_tmax_p99.docx",  
  dep_var = "ifv_count",
  indep_var = "hw_day_p99",
  fixed_effects = c("cod_mun", "year", "month", "weekends"),
  cluster_var = ~cod_mun
)


## Models with other heat measures ----

fit_heats_model <- function(dep_var, predictor, data, fixed_effects, cluster_var) {
  # Construcción de la fórmula con efectos fijos
  fe_formula <- paste(fixed_effects, collapse = " + ")  
  formula <- as.formula(paste(dep_var, "~", predictor, "|", fe_formula))
  
  # Ajustar el modelo de Poisson con efectos fijos
  model_fit <- fepois(formula, cluster = cluster_var, data = data)
  
  # Extraer resultados con broom::tidy() para análisis posterior
  results <- tidy(model_fit, conf.int = TRUE, conf.level = 0.95) %>%
    mutate(
      estimate = round(estimate, 3),
      std.error = round(std.error, 3),
      conf.low = round(conf.low, 3),
      conf.high = round(conf.high, 3),
      p.value = round(p.value, 3)
    ) %>%
    dplyr::select(term, estimate, std.error, p.value, conf.low, conf.high) %>%
    mutate(dependent_var = dep_var, predictor = predictor)  # Agregar nombres de variables
  
  return(results)
}

# Estimate the models 

dep_var <- "ifv_count"  
indep_vars <- c(
  #"p90_tmax", "p95_tmax", "p99_tmax",
  "hw_day_30c", "hw_day_31c", "hw_day_32c", "hw_day_33c", "hw_day_34c",
  "hw_day_p90", "hw_day_p95", "hw_day_p99",
  "hw_30c_2d", "hw_31c_2d", "hw_32c_2d", "hw_33c_2d", "hw_34c_2d",
  "hw_p90_2d", "hw_p95_2d", "hw_p99_2d",
  "hw_30c_3d", "hw_31c_3d", "hw_32c_3d", "hw_33c_3d", "hw_34c_3d",
  "hw_p90_3d", "hw_p95_3d", "hw_p99_3d",
  "hw_30c_4d", "hw_31c_4d", "hw_32c_4d", "hw_33c_4d", "hw_34c_4d",
  "hw_p90_4d", "hw_p95_4d", "hw_p99_4d",
  "hw_ehf_tad_2d", "hw_ehf_tad_3d", "hw_ehf_tad_4d"
                ) 

fixed_effects <- c("cod_mun", "year", "month", "weekends")  
cluster_var <- "cod_mun" 

results_list <- future_lapply(indep_vars, function(var) {
  fit_heats_model(dep_var, var, data = crime_count, fixed_effects, cluster_var)
})

results_poisson <- bind_rows(results_list)

writexl::write_xlsx(results_poisson, path =  paste0("Output/", "Models/", "Table_POIS_heat", ".xlsx"))

# Prepare figure 
results_poisson <- bind_rows(results_list) |> 
  mutate(
    duration = str_extract(term, "\\d+d"), 
    duration_label = case_when( 
    duration == "2d" ~ "2 days",
    duration == "3d" ~ "3 days",
    duration == "4d" ~ "4 or more days",
    TRUE ~ "1 day"
  ),
  duration_label = factor(duration_label, levels = c("1 day", "2 days", "3 days", "4 or more days")) 
) |> 
  mutate(term=factor(term, 
    levels = c(
    "hw_ehf_tad_4d", "hw_ehf_tad_3d", "hw_ehf_tad_2d", 
    "hw_p99_4d", "hw_p99_3d", "hw_p99_2d", "hw_day_p99", 
    "hw_p95_4d", "hw_p95_3d", "hw_p95_2d", "hw_day_p95", 
    "hw_p90_4d", "hw_p90_3d", "hw_p90_2d", "hw_day_p90", 
    "hw_34c_4d", "hw_34c_3d", "hw_34c_2d", "hw_day_34c", 
    "hw_33c_4d", "hw_33c_3d", "hw_33c_2d", "hw_day_33c", 
    "hw_32c_4d", "hw_32c_3d", "hw_32c_2d", "hw_day_32c", 
    "hw_31c_4d", "hw_31c_3d", "hw_31c_2d", "hw_day_31c", 
    "hw_30c_4d", "hw_30c_3d", "hw_30c_2d", "hw_day_30c" 
  ), 
    labels = c(
    "HW-EHF 4D", "HW-EHF 3D", "HW-EHF 2D", 
    "HW-P99 4D", "HW-P99 3D", "HW-P99 2D", "HW-P99 1D", 
    "HW-P95 4D", "HW-P95 3D", "HW-P95 2D", "HW-P95 1D", 
    "HW-P90 4D", "HW-P90 3D", "HW-P90 2D", "HW-P90 1D", 
    "HW-34ºC 4D", "HW-34ºC 3D", "HW-34ºC 2D", "HW-34ºC 1D", 
    "HW-33ºC 4D", "HW-33ºC 3D", "HW-33ºC 2D", "HW-33ºC 1D", 
    "HW-32ºC 4D", "HW-32ºC 3D", "HW-32ºC 2D", "HW-32ºC 1D", 
    "HW-31ºC 4D", "HW-31ºC 3D", "HW-31ºC 2D", "HW-31ºC 1D", 
    "HW-30ºC 4D", "HW-30ºC 3D", "HW-30ºC 2D", "HW-30ºC 1D"
    ))) |> 
  mutate(
    estimate=exp(estimate), 
    conf.low=exp(conf.low), 
    conf.high=exp(conf.high)
)

glimpse(results_poisson)

g1 <- results_poisson |> 
  filter(term %in% c(
    "HW-34ºC 4D", "HW-34ºC 3D", "HW-34ºC 2D", "HW-34ºC 1D", 
    "HW-33ºC 4D", "HW-33ºC 3D", "HW-33ºC 2D", "HW-33ºC 1D", 
    "HW-32ºC 4D", "HW-32ºC 3D", "HW-32ºC 2D", "HW-32ºC 1D", 
    "HW-31ºC 4D", "HW-31ºC 3D", "HW-31ºC 2D", "HW-31ºC 1D", 
    "HW-30ºC 4D", "HW-30ºC 3D", "HW-30ºC 2D", "HW-30ºC 1D"
  )) |> 
  ggplot(aes(x = estimate, y = term, color = duration_label)) +
    geom_point(size = 2.5, shape = 15) +
    geom_errorbarh(aes(xmin = conf.low, xmax = conf.high), height = 0.2) +
    geom_hline(yintercept = 16.5, color = "gray") +  
    geom_hline(yintercept = 12.5, color = "gray") +
    geom_hline(yintercept = 8.5, color = "gray") +
    geom_hline(yintercept = 4.5, color = "gray") +
    geom_vline(xintercept = 1, linetype = "dashed", color = "red", alpha = 0.5) +
    scale_colour_manual(name = "Duration HW:", values = c("#F5CBA7", "#e59866", "#d35400", "#873600")) +
    scale_x_continuous(limits = c(0.65, 1.2), expand = c(0.005, 0.005)) +
    geom_text(aes(x = 0.75, label = paste0(format(round(estimate, 3), nsmall = 2), " (", 
                                                      format(round(conf.low, 3), nsmall = 2), " - ", 
                                                      format(round(conf.high, 3), nsmall = 2), ")")), 
              position = position_dodge(width = 0.75), size = 3, show.legend = FALSE) + 
    labs(title = "A. Maximum temperature celcius measures \n",
         x = "IRR and 95% CI", 
         y = "Heatwave Definition") +
    theme_light() +
    theme(panel.grid = element_blank(),
          legend.position = "top",
          legend.text = element_text(size = 11))

g2 <- results_poisson |> 
  filter(term %in% c(
    "HW-EHF 4D", "HW-EHF 3D", "HW-EHF 2D", 
    "HW-P99 4D", "HW-P99 3D", "HW-P99 2D", "HW-P99 1D", 
    "HW-P95 4D", "HW-P95 3D", "HW-P95 2D", "HW-P95 1D", 
    "HW-P90 4D", "HW-P90 3D", "HW-P90 2D", "HW-P90 1D"
  )) |> 
  ggplot(aes(x = estimate, y = term, color = duration_label)) +
    geom_point(size = 2.5, shape = 15) +
    geom_errorbarh(aes(xmin = conf.low, xmax = conf.high), height = 0.2) +
    geom_hline(yintercept = 11.5, color = "gray") +
    geom_hline(yintercept = 7.5, color = "gray") +
    geom_hline(yintercept = 3.5, color = "gray") +
    geom_vline(xintercept = 1, linetype = "dashed", color = "red", alpha = 0.5) +
    scale_colour_manual(name = "Duration HW:", values = c("#F5CBA7", "#e59866", "#d35400", "#873600")) +
    scale_x_continuous(limits = c(0.65, 1.2), expand = c(0.005, 0.005)) +
    geom_text(aes(x = 0.75, label = paste0(format(round(estimate, 3), nsmall = 2), " (", 
                                                      format(round(conf.low, 3), nsmall = 2), " - ", 
                                                      format(round(conf.high, 3), nsmall = 2), ")")), 
              position = position_dodge(width = 0.75), size = 3, show.legend = FALSE) + 
    labs(title = "B. Maximum temperature percentile and \n Excess Heat Factor measures",
         x = "IRR and 95% CI", 
         y = "Heatwave Definition") +
    theme_light() +
    theme(panel.grid = element_blank(),
          legend.position = "top",
          legend.text = element_text(size = 11))

ggarrange(g1, g2, common.legend = TRUE)

ggsave(
  filename = paste0("Output/Models/Tmax_heat_effects.png"), 
  res = 300,
  width = 30,
  height = 22,
  units = 'cm',
  scaling = 1.2,
  device = ragg::agg_png)
 