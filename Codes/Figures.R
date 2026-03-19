library(ggplot2)
library(dplyr)
library(scales)
library(tidyr)
library(patchwork)
library(tibble)

# Figure 1. Market locations in Tanzania for which local price data were obtained. The background layer displays long-term average rainfall (January 2000-January 2026).

# Convert mkt_pts to a spatial object
mkt_pts <- vect(prices.monthly.long, geom=c("Longitude", "Latitude"), crs=crs(tza0), keepgeom=TRUE)  

# Bring in chirps data
path <- "C:/H/Tanzania Price data/Datasets/RAINFALL"

c_files <- list.files(path, pattern = ".tif$", full.names = TRUE)
length(c_files)

# Read all CHIRPS data files into a SpatRaster collection
c_rasters <- rast(c_files)
plot(c_rasters)

#crop to Tanzania boundary
Chirps_Tza <- crop(c_rasters, tza1)

#Replace -9999 with NA
Chirps_Tza <- classify(Chirps_Tza, cbind(-9999,NA))
plot(Chirps_Tza)

# mean annual rainfall for each year from 2000 to 2024
years <- 2000:2026
layers_by_year <- split(1:nlyr(Chirps_Tza), ceiling(seq_along(1:nlyr(Chirps_Tza)) / 12))

# Calculate total annual rainfall per year
annual_rainfall_rasters <- lapply(seq_along(years), function(i) {
  yr_raster <- sum(Chirps_Tza[[layers_by_year[[i]]]], na.rm = TRUE)
  names(yr_raster) <- paste0("rain_", years[i])
  return(yr_raster)
})

rainfall_stack <- rast(annual_rainfall_rasters)
plot(rainfall_stack)

# Get the long term average rainfall from 2000 to 2025
rain_mean <- terra::mean(rainfall_stack)
plot(rain_mean)

# Mask rainfall
rain_mean_masked <- mask(rain_mean, tza1)
plot(rain_mean_masked)

# Convert rainfall raster to data frame for ggplot
rain_df <- as.data.frame(rain_mean_masked, xy = TRUE, na.rm = TRUE)
colnames(rain_df)[3] <- "rainfall"

# Convert shapefiles and points to sf
tza_sf <- st_as_sf(tza1)
markets_sf <- st_as_sf(mkt_pts)
markets_sf$type <- "Markets"

pal <- mako(100, direction = -1)

png("C:/H/Tanzania Price data/Datasets/figures_and_maps/market_prices_map.png", width = 800, height = 800, res = 125)
ggplot() +
  # Rainfall raster
  geom_raster(data = rain_df, aes(x = x, y = y, fill = rainfall)) +
  scale_fill_gradientn(
    colours = pal,
    breaks = seq(0, 2600, by = 400),
    name = "Rainfall (mm)",
    guide = guide_colorbar(order = 1) 
  ) +
  
  # Country boundary 
  geom_sf(data = tza_sf, fill = NA, color = "black", linewidth = 0.4) +
  
  # Market points with legend mapping
  geom_sf(data = markets_sf, aes(color = type), shape = 19, size = 2) +
  scale_color_manual(
    values = c("Markets" = "red"),
    name = "",
    guide = guide_legend(override.aes = list(size = 3), order = 2) # points legend second
  ) +
  
  # Scale bar
  annotation_scale(
    location = "bl",
    width_hint = 0.3,
    bar_cols = c("black", "white")
  ) +
  
  # Labels
  labs(x = "Longitude", y = "Latitude") +
  
  # Theme tweaks
  theme_minimal(base_size = 10) +
  theme(
    panel.grid.major = element_line(color = "grey80", size = 0.2),
    legend.position = "right",
    legend.spacing.y = unit(-0.2, "cm"),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.5),
    legend.title = element_text(size = 10),   
    legend.text  = element_text(size = 9),    
    axis.text.x = element_text(angle = 0, vjust = 1, size = 9),   
    axis.text.y = element_text(size = 9),                        
    axis.title.y = element_text(size = 10)                      
  )
dev.off()


# Figure 2. Average monthly real wholesale prices (TZS/kg) per crop. The time series (2021–2026) shows the mean price across 43 markets with 95% confidence bands indicating the extent of spatial price variability.
unique(mypts$Crop)
plot_data <- mypts %>%
  # Rename the crops to their full  names
  mutate(Crop = recode(Crop, 
                       "B.Millet" = "Bulrush Millet",
                       "F.Millet" = "Finger Millet",
                       "Potato"   = "Potato", 
                       "Beans"    = "Bean")) %>%
  
  mutate(date = as.Date(paste(Year, Month, "01", sep = "-"))) %>%
  group_by(Crop, date) %>%
  summarise(
    mean_pkg = mean(pkg_real, na.rm = TRUE),
    sd_pkg = sd(pkg_real, na.rm = TRUE),
    n_markets = n(),
    .groups = "drop"
  ) %>%
  mutate(
    se = sd_pkg / sqrt(n_markets),
    lower_ci = mean_pkg - (1.96 * se),
    upper_ci = mean_pkg + (1.96 * se))

# Create Grayscale Figure 2
fig2_bw <- ggplot(plot_data, aes(x = date, y = mean_pkg)) +
  geom_ribbon(aes(ymin = lower_ci, ymax = upper_ci), 
              fill = "gray80", alpha = 0.5, color = NA) +
  geom_line(color = "black", linewidth = 0.7) +
  facet_wrap(~Crop, scales = "free_y", ncol = 2) +
  scale_y_continuous(labels = comma) +
  scale_x_date(date_labels = "%Y", date_breaks = "1 year") +
  theme_minimal(base_size = 14) +
  labs(
    x = "",
    y = "Real Price (TZS/kg)",
    title = "",
    caption = ""
  ) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.5),
    strip.text = element_text(face = "bold", size = 12),
    axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1, size = 10),
    axis.text.y = element_text(size = 10),
    axis.title.y = element_text(size = 12),
    legend.position = "none")


ggsave("C:/H/Tanzania Price data/Datasets/figures_and_maps/Figure2_Grayscale_Final.png", 
       plot = fig2_bw, width = 9, height = 11, dpi = 300)



# Figure 3. Results of two cross-validation strategies: temporal split (standard random forest), and temporal split (autoregressive random forest) for pooled versus crop-specific random forest models. Bar plots display the coefficient of determination (R2), and the root mean squared error (RMSE) in TZS/kg. 
# Explicitly convert Crop to character to prevent numeric indexing 
main_df <- bind_rows(
  comparison_results_ts %>% 
    mutate(Strategy = "Temporal Split (Standard RF)", Crop = as.character(Crop)),
  comparison_ar_final %>% 
    mutate(Strategy = "Temporal Split (Autoregressive RF)", Crop = as.character(Crop)) 
) %>%
  # Now Rename Beans to Bean
  mutate(Crop = ifelse(Crop == "Beans", "Bean", Crop)) %>%
  pivot_longer(cols = starts_with("R2") | starts_with("RMSE"), 
               names_to = "Metric_Type", values_to = "Value") %>%
  mutate(Model = ifelse(grepl("Pooled", Metric_Type), "Pooled", "Crop-Specific"),
         Metric = ifelse(grepl("R2", Metric_Type), "R²", "RMSE"))

# Plot R-Squared
p_r2 <- ggplot(filter(main_df, Metric == "R²"), 
               aes(x = Crop, y = Value, fill = Model)) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_wrap(~Strategy, ncol = 1) +
  scale_fill_manual(values = c("#999999", "#D55E00")) + 
  theme_minimal() +
  labs(y = expression(R^{2}), x = NULL)

# Plot RMSE
p_rmse <- ggplot(filter(main_df, Metric == "RMSE"), 
                 aes(x = Crop, y = Value, fill = Model)) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_wrap(~Strategy, ncol = 1) +
  scale_fill_manual(values = c("#999999", "#D55E00")) +
  theme_minimal() +
  labs(y = "RMSE (TZS/kg)", x = NULL)


# Use '&' to apply this theme to ALL subplots simultaneously
final_main_plot <- p_r2 + p_rmse + plot_layout(guides = "collect") & 
  theme(
    legend.position = 'bottom', 
    legend.title = element_blank(),
    legend.text = element_text(size = 12),
    # THIS controls "Temporal Split..." labels
    strip.text = element_text(face = "bold", size = 13), 
    axis.text.x = element_text(size = 11, color = "black"),
    axis.text.y = element_text(size = 11, color = "black"),
    axis.title.y = element_text(size = 12, face = "bold"),
    # Removes the empty space where the X-axis title would be
    axis.title.x = element_blank())
final_main_plot

ggsave("C:/H/Tanzania Price data/Datasets/figures_and_maps/Figure3_Main_Results.png", 
       final_main_plot, width = 12, height = 10, dpi = 300)

# Plot Train test validation results which will be put in the appendix
appendix_df <- comparison_results_tt %>%
  mutate(Crop = as.character(Crop)) %>% 
  mutate(Crop = ifelse(Crop == "Beans", "Bean", Crop)) %>%
  pivot_longer(cols = starts_with("R2") | starts_with("RMSE"), 
               names_to = "Metric_Type", values_to = "Value") %>%
  mutate(Model = ifelse(grepl("Pooled", Metric_Type), "Pooled", "Crop-Specific"),
         Metric = ifelse(grepl("R2", Metric_Type), "R²", "RMSE"))

#  R-Squared Plot for Appendix 
p_app_r2 <- ggplot(filter(appendix_df, Metric == "R²"), 
                   aes(x = Crop, y = Value, fill = Model)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_manual(values = c("#999999", "#D55E00")) + 
  theme_minimal() +
  labs(y = expression(R^{2}), x = NULL)

# RMSE Plot for Appendix 
p_app_rmse <- ggplot(filter(appendix_df, Metric == "RMSE"), 
                     aes(x = Crop, y = Value, fill = Model)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_manual(values = c("#999999", "#D55E00")) +
  theme_minimal() +
  labs(y = "RMSE (TZS/kg)", x = NULL)


final_appendix_plot <- p_app_r2 + p_app_rmse + 
  plot_layout(guides = "collect") & 
  theme(
    legend.position = 'bottom', 
    legend.title = element_blank(),
    legend.text = element_text(size = 12),
    axis.text.x = element_text(size = 11, color = "black"),
    axis.text.y = element_text(size = 11, color = "black"),
    axis.title.y = element_text(size = 12, face = "bold"),
    axis.title.x = element_blank())

final_appendix_plot

ggsave("C:/H/Tanzania Price data/Datasets/figures_and_maps/Appendix_TrainTest_Plot.png", 
       final_appendix_plot, width = 12, height = 6, dpi = 300)

# Figure 4.  Variable importance rankings from the random forest model predicting wholesale crop prices across Tanzania 
# Extract variable importance and handle naming
imp <- as.data.frame(importance(rf))
imp$Variable <- rownames(imp)

# Correctly target %IncMSE as used in your results
importance_col <- "%IncMSE"

# Update labels 
pretty_labels <- c(
  CPI          = "CPI",
  maize        = "Maize",
  rice         = "Rice",
  sorghum      = "Sorghum",
  bmillet      = "Bulrush millet",
  fmillet      = "Finger millet",
  wheat        = "Wheat",
  beans        = "Bean",
  potato       = "Potato",
  Month        = "Month",
  Year         = "Year",
  ttport_1     = "Travel time to port",
  ttcity_u5    = "Travel time to cities",
  popdens      = "Population density",
  bio_3        = "BIO3",
  bio_6        = "BIO6",
  bio_9        = "BIO9",
  bio_12       = "BIO12",
  bio_18       = "BIO18",
  rain.sum.lag = "Lagged rainfall"
)

# Prepare data for plotting
imp_plot <- imp %>%
  # Ensure we use the correct column name even if R formatted it strangely
  rename(Importance = !!sym(importance_col)) %>%
  mutate(
    PrettyVar = recode(Variable, !!!pretty_labels),
    # Reorder by importance value
    PrettyVar = reorder(PrettyVar, Importance)
  ) %>%
  arrange(desc(Importance))

# Generate Plot
fig4_imp <- ggplot(imp_plot, aes(x = PrettyVar, y = Importance)) +
  # Using points and segments (Lollipop plot) for a cleaner academic look
  geom_segment(aes(xend = PrettyVar, y = 0, yend = Importance), color = "gray70") +
  geom_point(size = 3, color = "black") +
  coord_flip() +
  labs(
    x = "",
    y = "% Increase in Mean Squared Error (%IncMSE)",
    title = ""
  ) +
  theme_bw(base_size = 12) +
  theme(
    axis.text = element_text(color = "black"),
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.8)
  )

ggsave("C:/H/Tanzania Price data/Datasets/figures_and_maps/Figure4_Variable_Importance.png", plot = fig4_imp, width = 7, height = 8, dpi = 300)




