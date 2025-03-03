# Code 4: CASEN 2006 (BASELINE) ----

rm(list=ls())
## Settings ----
source("Code/0.1 Functions.R")
source("Code/0.2 Settings.R")

# Data path 
data_inp <- "Data/Input/"
data_out <- "Data/Output/"

# CASEN DATA ----

# Open data and adjust variables 
casen06 <- "casen2006.dta"
casen <- rio::import(paste0(data_inp, casen06)) |> 
  clean_names() |> 
  filter(r==13) |> 
  dplyr::select(comuna, expc, estrato, corte, qautr) |> 
  mutate(pob=if_else(corte %in% c(1,2), 1,
        if_else(corte==3, 0, NA_real_))) |> 
  mutate(index_quintil=if_else(qautr==0, NA_real_, qautr)) |> 
  drop_na()

quintiles_dummy <- as.data.frame(make_dummies(casen$index_quintil, prefix = "q_"))
colnames(quintiles_dummy) <- paste0("q", 1:5)

glimpse(quintiles_dummy)
casen <- bind_cols(casen, quintiles_dummy)

glimpse(casen)

# Complex desing survey 
design <- svydesign(id = ~1, strata = ~estrato, weights = ~expc, data = casen)

# Share Poverty, which measures the % of families below poverty line in reporting district i.

pob_mun <- svyby(~pob, ~comuna, design, svymean, na.rm = TRUE) %>%
  mutate(pob = pob*100) |> 
  dplyr::select(comuna, pob) |> 
  mutate(pob_median = if_else(pob > median(pob), 1, 0))
pob_mun

# Quintile Income, wich measure the % of families in each income quintile in district i.
quintil <-  svyby(~q1 + q2 + q3 + q4 + q5,
                 ~comuna, design, svymean, na.rm = TRUE) |> 
  mutate(across(starts_with("q"), ~ . * 100)) |> 
  dplyr::select(1:6) |> 
  rowwise() |> 
  mutate(quintil_mayor = names(which.max(c(q1, q2, q3, q4, q5)))) |> 
  ungroup()
  
quintil

# Join both data CASEN 
pob_mun