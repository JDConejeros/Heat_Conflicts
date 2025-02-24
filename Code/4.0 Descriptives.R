# Code 4: Descriptive analysis ----

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

## Distribution of crimes -----

time_count <- crime_count |> 
  group_by(week) |> 
  summarise(
    intrafamily_violence = sum(intrafamily_violence, na.rm = TRUE),  
    robbery = sum(robbery, na.rm = TRUE),
    larceny = sum(larceny, na.rm = TRUE),
    vehicle_theft = sum(vehicle_theft, na.rm = TRUE),
    burglary = sum(burglary, na.rm = TRUE),
    avg_tmax = mean(tmax, na.rm = TRUE)  
  ) |> 
  ungroup() |> 
  mutate(year=year(week)) |> 
  filter(year %in% c(2005:2010))


g1 <- ggplot(time_count, aes(x = week)) +
  geom_line(aes(y = intrafamily_violence, color = "Intrafamily violence"), size = 0.7) +  
  geom_line(aes(y = avg_tmax * 30, color = "Mean Week Max. Temp."), size = 0.7) + # linetype="dashed"
  scale_y_continuous(
    name = "Number of cases",
    sec.axis = sec_axis(~ . / 30, name = "Mean Week Max. Temp.", breaks=seq(0, 40, by=10)), 
    limits = c(0, 1200, 40), 
    breaks=seq(0, 1200, by=300)
  ) +
  scale_x_date(
    date_breaks = "6 months",
    date_labels = "%b %Y"
  ) +
  scale_color_manual(values = c("Intrafamily violence" = "gray60", "Mean Week Max. Temp." = "#d35400"), name=NULL) +
  labs(y=NULL, x=NULL, title = "A. Time Trends") +
  theme_light() +
  theme(panel.grid = element_blank(),
        legend.position = "top",
        title = element_text(size=11),
        legend.text = element_text(size = 11), 
        axis.text.x = element_text(angle = 45, vjust = 0.7, hjust=0.75))

g1

g2 <- crime_count |> 
  ggplot(aes(x =intrafamily_violence)) + 
  geom_histogram(bins = 40, alpha = 1, fill="white", color="black") +
  #geom_density(color = "#d35400", fill = "#d35400", linewidth=1, linetype="dashed", alpha=0.25) +
  labs(title = "B. Distribution of Intrafamily violence counts", x = "Number of crimes", y = "Count") +
  theme_light() +
  theme(panel.grid = element_blank(),
        legend.position = "top")
      
g3 <- crime_count |> 
  ggplot(aes(x = tmax)) + 
  geom_histogram(bins = 40, alpha = 1, fill="white", color="black") +
  #geom_density(color = "#d35400", fill = "#d35400", linewidth=1, linetype="dashed", alpha=0.25) +
  labs(title = "C. Distribution of daily max. temperature", x = "Max. Temperature (Celcius)", y = "Count") +
  theme_light() +
  theme(panel.grid = element_blank(),
        legend.position = "top")

g1 | (g2 / g3)

ggsave(
  filename = paste0("Output/Figures/Distribution.png"), 
  res = 300,
  width = 25,
  height = 12,
  units = 'cm',
  scaling = 0.9,
  device = ragg::agg_png)
 
g3 <- ggplot(crime_count, aes(x=tmax_group, y=intrafamily_violence, fill=tmax_group)) +
  geom_boxplot() + #fill='#A4A4A4', color="black"
  labs(y="Number of cases", x="Max. Temp. Interval (celcius)") +
  #scale_fill_gradient_d(low = "#FFD700", high = "#8B0000")  +
  #scale_fill_viridis_d(option = "magma", direction = -1) +
  scale_fill_manual(
    values = c("white", "#FFE4B2", "#FFC56C", "#FF9E40", "#FF7800", "#E65C00", "#CC4000", "#B23000", "red")
  ) +
  theme_light() +
  theme(panel.grid = element_blank(),
          legend.position = "none")

g3

ggarrange(g1, g3, common.legend = TRUE)

ggsave(
  filename = paste0("Output/Figures/TS_temp_crime.png"), 
  res = 300,
  width = 25,
  height = 12,
  units = 'cm',
  scaling = 0.7,
  device = ragg::agg_png)


## Crime and tmax by mun and year -----

# Function create tables
summary_table <- function(data, crime_var, group_var) {
  data |> 
    group_by(across(all_of(group_var))) |> 
    summarise(
      crime_mean = mean(.data[[crime_var]], na.rm=TRUE), 
      crime_var = var(.data[[crime_var]], na.rm=TRUE), 
      crime_min = min(.data[[crime_var]], na.rm=TRUE), 
      crime_median = median(.data[[crime_var]], na.rm=TRUE), 
      crime_max = max(.data[[crime_var]], na.rm=TRUE), 

      temp_mean = mean(tmax, na.rm=TRUE), 
      temp_var = var(tmax, na.rm=TRUE), 
      temp_min = min(tmax, na.rm=TRUE), 
      temp_median = median(tmax, na.rm=TRUE), 
      temp_max = max(tmax, na.rm=TRUE)
    )
}

# Values
crime_vars <- c("robbery", "larceny", "vehicle_theft", "burglary", "injuries", "intrafamily_violence")

group_vars <- list(
  mun = c("cod_mun", "name_mun"),
  year = "year",
  month = "month",
  day_week = "day_week"
)

# Loop generate tables 
for (crime_var in crime_vars) {
  for (group_name in names(group_vars)) {
    
    # Descriptives
    sum <- summary_table(crime_count, crime_var, group_vars[[group_name]])
    
    # Output
    file_name <- paste0("Output/tables/", group_name, "_tmax_crime_", crime_var, ".xlsx")
    
    # Save file
    writexl::write_xlsx(sum, file_name)
  }
}

# Heat maps with crimes ------

# Count crime by year-month 

crime_count <- crime_count |> 
  mutate(year_month = format(date_crime, "%Y-%m"))

# Table with information 
ifv_count_mun <- crime_count |> 
  group_by(id_mun, name_mun, year_month) |> 
  summarise(n_ifv = sum(intrafamily_violence, na.rm=TRUE)) |> 
  ungroup() |> 
  mutate(mun = paste(id_mun, name_mun, sep=" - ")) |> 
  mutate(mun = factor(mun, levels = unique(mun[order(id_mun, decreasing = TRUE)])))

# Plot
ggplot(ifv_count_mun, aes(x = year_month, y = mun, fill = n_ifv)) +
  geom_tile(colour = "white") +
    scale_fill_viridis_c(
      option = "magma",
      direction = -1,
      breaks = seq(0, 400, by = 50),
      limits = c(0, 400),
      guide = guide_colorbar(title = "Number of Crimes", barwidth = 15, barheight = 0.5)
    ) +
  labs(x = NULL, y = "Municipality", 
        title=NULL) +
  #facet_grid(service~., scales = "free", space = "free",  switch = "y") +
  theme_light() +
  theme(
    legend.position = "top", 
    axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1), 
    plot.margin = margin(t = 10, r = 10, b = 5, l = 10),
    panel.grid = element_blank(),
    strip.text.y = element_text(angle = 0),
    strip.background = element_rect(fill=NA, color="gray70"), 
    strip.text=element_text(color="black"),
    strip.text.y.left = element_text(angle = 0)
  )

ggsave(
    filename = paste0("Output/Figures/TS_ifv_crime.png"), 
    res = 300,
    width = 25,
    height = 12,
    units = 'cm',
    scaling = 0.7,
    device = ragg::agg_png)
  

# Maps -------

# Map Chile

america <- ne_countries(scale = "medium", continent = "South America", returnclass = "sf")
chile <- ne_states(country = "Chile", returnclass = "sf")
santiago <- chile[chile$name == "Región Metropolitana de Santiago", ]

chile$color <- "gray50"
santiago$color <- "white"

america_centroids <- st_centroid(america) %>% 
  mutate(long = st_coordinates(.)[,1], lat = st_coordinates(.)[,2]) |> 
  filter(!iso_a3 %in% c("FLK", "CHL"))

base_map <- ggplot() +
  geom_sf(data = america, fill = "white", color = "black") +
  geom_sf(data = chile, aes(fill = color), color = "black") +
  geom_sf(data = santiago, fill = "white", color = "black") +  
  geom_text(data = america_centroids, aes(x = long, y = lat, label = iso_a3), size = 4, fontface="bold") + 
  scale_fill_manual(values = c("gray80", "gray50", "white")) +
  #annotation_scale(location = "bl", width_hint = 0.4, text_cex = 0.6, tick_height = 0.3) +
  labs(x=NULL, y=NULL, title = "A.") +
  theme_light() +
  theme(
      plot.title = element_text(size = 16),
      legend.position = "none", 
      #axis.text.x = element_text(angle = 0, hjust = 1, vjust = 1), 
      plot.margin = margin(0, 0, 0, 0),
      panel.grid = element_blank(),
      axis.text = element_blank(),
      axis.ticks = element_blank(),
      strip.text.y = element_text(angle = 0),
      strip.background = element_rect(fill=NA, color="gray70"), 
      strip.text=element_text(color="black"),
      strip.text.y.left = element_text(angle = 0),
      
    )

zoom_map <- ggplot() +
  geom_sf(data = chile, fill = "gray70", color = "black") +
  geom_sf(data = santiago, fill = "white", color = "black") +
  coord_sf(xlim = c(-72, -69.5), ylim = c(-34.5, -32.5)) + 
  geom_sf_text(data=santiago, aes(label = name_en, geometry = geometry), size = 5, fontface="bold", color = "black", stat = "sf_coordinates") +
  labs(x=NULL, y=NULL) +
  theme_light() + 
  theme(
      legend.position = "none", 
      plot.title = element_text(size=11, hjust = 0.5),
      plot.margin = margin(0, 0, 0, 0),
      panel.grid = element_blank(),
      axis.text.y = element_text(size=9),
      axis.text.x = element_text(size=9, angle=45, hjust = 1),
      #axis.ticks = element_blank(),
      strip.text.y = element_text(angle = 0),
      strip.background = element_rect(fill=NA, color="gray70"), 
      strip.text=element_text(color="black"),
      strip.text.y.left = element_text(angle = 0)
    )

zoom_map

map_plot <- base_map + inset_element(zoom_map, 
                                        left = -0.1, 
                                        bottom = 0.2, 
                                        right = 0.5, 
                                        top = 0.5)

map_plot

ggsave(
  filename = paste0("Output/Figures/Maps.png"), 
  res = 300,
  width = 20,
  height = 25,
  units = 'cm',
  scaling = 1,
  device = ragg::agg_png) 

# Map crime santiago

crime_stgo <- crime_count |> 
  group_by(id_mun, cod_mun, name_mun) |> 
  summarise(
    count = sum(intrafamily_violence, na.rm=TRUE), 
    tmax = mean(tmax, na.rm = TRUE),
    tmax_max = max(tmax, na.rm = TRUE),
    id = 1:n(),
    id = paste0("00", id)
  ) |> 
  ungroup() |> 
  mutate(g5=ntile(count, n=5)) |> 
  mutate(g5=factor(g5, 
                  levels = 1:5, 
                  labels = c("Lower quantile", 
                             "2nd quantile",
                             "3nd quantile",
                             "4nd quantile",
                             "Upper quantile"
                            )))

stgo <- chilemapas::mapa_comunas |> 
  mutate(codigo_comuna = as.numeric(codigo_comuna)) |> 
  filter(codigo_comuna %in% unique(crime_count$cod_mun)) |> 
  left_join(crime_stgo, by=c("codigo_comuna"="cod_mun"))

crime_map <- ggplot(stgo) +
  geom_sf(aes(fill=g5, geometry = geometry), lwd = 0.05, color = "white") +
  geom_sf_text(aes(label = id_mun, geometry = geometry), size = 3.5,  color = "black", stat = "sf_coordinates") +
  labs(x=NULL, y=NULL, title="B.") +
  annotation_scale(location = "bl", width_hint = 0.4, text_cex = 0.6, tick_height = 0.3) +
  scale_fill_brewer(
    name = "Reporting Intrafamily Violence\n(2005-2010)",
    direction = 1,
  ) +
  theme_light() +
   theme(
    legend.position = c(0.8, 0.2),  
    legend.key.size = unit(0.6, "cm"), 
    legend.text = element_text(size = 10), 
    legend.title = element_text(size = 10, face = "bold"), 
    plot.margin = margin(t = 10, r = 10, b = 5, l = 10),
    panel.grid = element_blank(),
    strip.text.y = element_text(angle = 0),
    strip.background = element_rect(fill = NA, color = "gray70"),
    strip.text = element_text(color = "black"),
    strip.text.y.left = element_text(angle = 0)
  )


crime_map

temp_map <- ggplot(stgo) +
  geom_sf(aes(fill=tmax, geometry = geometry), lwd = 0.05, color = "white") +
  geom_sf_text(aes(label = id_mun, geometry = geometry), size = 3.5, color = "black", stat = "sf_coordinates") +
  labs(x=NULL, y=NULL, title="C.") +
  annotation_scale(location = "bl", width_hint = 0.4, text_cex = 0.4, tick_height = 0.6) +
  scale_fill_gradientn(
    colours = c("white", "#FFE4B2", "#FFC56C", "#FF9E40", "#FF7800", "#E65C00", "#CC4000", "#B23000"), 
    limits = c(10, 25), # Límite del rango
    guide = guide_colorbar(title = "Daily Temp. Max. \n(2005-2010)", barwidth = 7, barheight = 0.5, 
                           direction = "horizontal", 
                           title.position = "top",
                           label.theme = element_text(angle = 0))
  ) +
  theme_light() +
   theme(
    legend.position = c(0.8, 0.15),  
    legend.key.size = unit(0.6, "cm"), 
    legend.text = element_text(size = 10), 
    legend.title = element_text(size = 10, face = "bold"), 
    plot.margin = margin(t = 10, r = 10, b = 5, l = 10),
    panel.grid = element_blank(),
    strip.text.y = element_text(angle = 0),
    strip.background = element_rect(fill = NA, color = "gray70"),
    strip.text = element_text(color = "black"),
    strip.text.y.left = element_text(angle = 0)
  )

temp_map

crime_temp_map <- ggarrange(crime_map, temp_map, nrow=2)
crime_temp_map

ggsave(
  filename = paste0("Output/Figures/Maps_count_tmax_stgo.png"), 
  res = 300,
  width = 18,
  height = 25,
  units = 'cm',
  scaling = 0.8,
  device = ragg::agg_png) 

 id_mun <- stgo |>
  group_by(id_mun, name_mun) |> 
  summarise(n=n()) |> 
  dplyr::select(-n)

id_mun_text <- c(paste(id_mun$id_mun, "-", id_mun$name_mun), "", "")
id_mun_matrix <- matrix(id_mun_text, ncol = 4, byrow = FALSE)

text_mun <- tableGrob(id_mun_matrix, theme = ttheme_minimal(
  core = list(fg_params = list(cex = 0.5, hjust = 0, x = 0),
              bg_params = list(fill = "white", col = "white") 
) ,
  padding = unit(c(0.2, 0.2, 0.2, 0.2), "cm") 
))

text_municipality <- textGrob("Municipality:\n", gp = gpar(fontsize = 8, fontface="bold"), hjust = -0.4, x = 0, y = -6)

tab <- ggarrange(
  text_municipality, text_mun,  
  ncol = 1, heights = c(0.02, 1) 
)

tab

ggsave(
  filename = paste0("Output/Figures/Stgo_tab.png"), 
  res = 300,
  width = 10,
  height = 6 ,
  units = 'cm',
  scaling = 1,
  device = ragg::agg_png) 
   