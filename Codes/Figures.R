library(ggplot2)
library(dplyr)
library(scales)
library(tidyr)
library(patchwork)
library(tibble)
library(openxlsx)
library(rlang)
library(ggh4x)

fig_path <- "C:/Users/LMADAGA/OneDrive - CGIAR/H/Tanzania Price data/Datasets/Qopen_figures/"

# Figure 1. Market locations in Tanzania for which local price data were obtained. The background layer displays long-term average rainfall (January 2000-January 2026).

# Convert market points to a spatial object
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

# mean annual rainfall for each year from 2000 to 2026
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

# Get the long term average rainfall from 2000 to 2026
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

p <- ggplot() +
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
    axis.title.y = element_text(size = 10))
p

ggsave(
  filename = "C:/Users/LMADAGA/OneDrive - CGIAR/H/Tanzania Price data/Datasets/Qopen_figures/Figure 1.jpg",
  plot = p,
  width = 6,
  height = 6,
  units = "in",
  dpi = 300)


# Figure 2. Average monthly real wholesale prices (TZS/kg) per crop. The time series (2021–2026) shows the mean price across 43 markets with shaded areas representing 95% confidence intervals indicating the extent of spatial price variability. 
unique(mypts$Crop)
mypts <- as.data.frame(mypts)
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
  ggh4x::facet_wrap2(~Crop, scales = "free_y", ncol = 2, axes = "all") +
  scale_y_continuous(labels = comma) +
  scale_x_date(date_labels = "%Y", date_breaks = "1 year") +
  theme_minimal(base_size = 14) +
  labs(
    x = "Year",
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
fig2_bw

ggsave("C:/Users/LMADAGA/OneDrive - CGIAR/H/Tanzania Price data/Datasets/Qopen_figures/Figure 2.jpg",
  plot = fig2_bw,   
  width = 9,
  height = 11,
  units = "in",
  dpi = 300)

# Figure 2 colored
fig2_color <- ggplot(plot_data, aes(x = date, y = mean_pkg)) +
  geom_ribbon(aes(ymin = lower_ci, ymax = upper_ci), 
              fill = "#9ecae1", alpha = 0.4) +
  geom_line(color = "#08519c", linewidth = 0.8) +
  facet_wrap(~Crop, scales = "free_y", ncol = 2) +
  scale_y_continuous(labels = comma) +
  scale_x_date(date_labels = "%Y", date_breaks = "1 year") +
  theme_minimal(base_size = 14) +
  labs(
    x = "",
    y = "Real Price (TZS/kg)"
  ) +
  theme(
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.5),
    strip.text = element_text(face = "bold", size = 12),
    axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1, size = 10),
    axis.text.y = element_text(size = 10),
    axis.title.y = element_text(size = 12),
    legend.position = "none")
fig2_color

ggsave(
  paste0(fig_path, "Figure_2_TimeSeries_coloured.jpg"),
  plot = fig2_color,
  width = 9,
  height = 11,
  units = "in",
  dpi = 300)



# Figure 3. Results of two cross-validation strategies: temporal split (standard random forest), and temporal split (autoregressive random forest) for pooled versus crop-specific random forest models. Bar plots display the coefficient of determination (R2), and the root mean squared error (RMSE) in TZS/kg. 
file_path <- "C:/Users/LMADAGA/OneDrive - CGIAR/H/Tanzania Price data/Datasets/RF-Validation-results.xlsx"

comparison_results_ts <- readxl::read_xlsx(file_path, sheet = "comparison_results_ts")
comparison_ar_final   <- readxl::read_xlsx(file_path, sheet = "comparison_ar_final")
comparison_results_tt <- readxl::read_xlsx(file_path, sheet = "comparison_results_tt")

# Explicitly convert Crop to character to prevent numeric indexing
main_df <- bind_rows(
  comparison_results_ts %>%
    mutate(
      Strategy = "Temporal Split (Standard RF)",
      Crop = as.character(Crop)
    ),
  comparison_ar_final %>%
    mutate(
      Strategy = "Temporal Split (Autoregressive RF)",
      Crop = as.character(Crop)
    )
) %>%
  mutate(
    Crop = recode(Crop,
                  "Beans" = "Bean",
                  "B.Millet" = "Bulrush Millet",
                  "F.Millet" = "Finger Millet")
  ) %>%
  pivot_longer(
    cols = c(`RMSE Pooled model`,
             `RMSE Crop-specific model`,
             `R2 Pooled model`,
             `R2 Crop-specific model`),
    names_to = "Metric_Type",
    values_to = "Value"
  ) %>%
  mutate(
    Model = case_when(
      grepl("Pooled", Metric_Type) ~ "Pooled",
      grepl("Crop-specific", Metric_Type) ~ "Crop-Specific"
    ),
    Metric = case_when(
      grepl("^R2", Metric_Type) ~ "R²",
      grepl("^RMSE", Metric_Type) ~ "RMSE"
    )
  )

# set crop order
crop_order <- c("Maize", "Rice", "Wheat", "Bean", "Sorghum",
                "Bulrush Millet", "Finger Millet", "Potato")

main_df <- main_df %>%
  mutate(Crop = factor(Crop, levels = crop_order))


crop_labels <- c(
  "Maize" = "Maize",
  "Rice" = "Rice",
  "Wheat" = "Wheat",
  "Bean" = "Bean",
  "Sorghum" = "Sorghum",
  "Bulrush Millet" = "B.Millet",
  "Finger Millet" = "F.Millet",
  "Potato" = "Potato")

# Plot R-Squared
p_r2 <- ggplot(filter(main_df, Metric == "R²"), 
               aes(x = Crop, y = Value, fill = Model)) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_wrap(~Strategy, ncol = 1) +
  scale_fill_manual(values = c("#999999", "#D55E00")) +
  scale_x_discrete(labels = crop_labels) +   # <-- ADD THIS
  theme_minimal() +
  labs(y = expression(R^{2}), x = NULL)

# Plot RMSE
p_rmse <- ggplot(filter(main_df, Metric == "RMSE"), 
                 aes(x = Crop, y = Value, fill = Model)) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_wrap(~Strategy, ncol = 1) +
  scale_fill_manual(values = c("#999999", "#D55E00")) +
  scale_x_discrete(labels = crop_labels) +   # <-- ADD THIS
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

ggsave(
  "C:/Users/LMADAGA/OneDrive - CGIAR/H/Tanzania Price data/Datasets/Qopen_figures/Figure 3.jpg",
  plot = final_main_plot,
  width = 12,
  height = 10,
  units = "in",
  dpi = 300)

# Plot Train-test validation results for appendix
# Supplementary Figure 2. R² and RMSE results from train-test cross-validation.

appendix_df <- comparison_results_tt %>%
  mutate(
    Crop = as.character(Crop),
    Crop = trimws(Crop),
    Crop = recode(
      Crop,
      "Beans" = "Bean",
      "B.Millet" = "Bulrush Millet",
      "F.Millet" = "Finger Millet"
    )
  ) %>%
  pivot_longer(
    cols = c(`RMSE Pooled model`,
             `RMSE Crop-specific model`,
             `R2 Pooled model`,
             `R2 Crop-specific model`),
    names_to = "Metric_Type",
    values_to = "Value"
  ) %>%
  mutate(
    Model = ifelse(grepl("Pooled", Metric_Type), "Pooled", "Crop-Specific"),
    Metric = ifelse(grepl("^R2", Metric_Type), "R²", "RMSE")
  ) %>%
  filter(Crop %in% crop_order) %>%
  mutate(Crop = factor(Crop, levels = crop_order))

# Short labels for display
crop_labels <- c(
  "Maize" = "Maize",
  "Rice" = "Rice",
  "Wheat" = "Wheat",
  "Bean" = "Bean",
  "Sorghum" = "Sorghum",
  "Bulrush Millet" = "B.Millet",
  "Finger Millet" = "F.Millet",
  "Potato" = "Potato"
)

# R² plot
p_app_r2 <- ggplot(filter(appendix_df, Metric == "R²"),
                   aes(x = Crop, y = Value, fill = Model)) +
  geom_col(position = position_dodge(width = 0.8), width = 0.7) +
  scale_fill_manual(values = c("Crop-Specific" = "#999999", "Pooled" = "#D55E00")) +
  scale_x_discrete(labels = crop_labels) +
  theme_minimal() +
  labs(y = expression(R^2), x = NULL)

# RMSE plot
p_app_rmse <- ggplot(filter(appendix_df, Metric == "RMSE"),
                     aes(x = Crop, y = Value, fill = Model)) +
  geom_col(position = position_dodge(width = 0.8), width = 0.7) +
  scale_fill_manual(values = c("Crop-Specific" = "#999999", "Pooled" = "#D55E00")) +
  scale_x_discrete(labels = crop_labels) +
  theme_minimal() +
  labs(y = "RMSE (TZS/kg)", x = NULL)

final_appendix_plot <- p_app_r2 + p_app_rmse +
  plot_layout(guides = "collect") &
  theme(
    legend.position = "bottom",
    legend.title = element_blank(),
    legend.text = element_text(size = 12),
    axis.text.x = element_text(size = 11, color = "black"),
    axis.text.y = element_text(size = 11, color = "black"),
    axis.title.y = element_text(size = 12, face = "bold"),
    axis.title.x = element_blank()
  )

final_appendix_plot

ggsave(
  "C:/Users/LMADAGA/OneDrive - CGIAR/H/Tanzania Price data/Datasets/Qopen_figures/Appendix_TrainTest_Plot.jpg",
  plot = final_appendix_plot,
  width = 12,
  height = 6,
  units = "in",
  dpi = 300)

# Figure 4.  Variable importance rankings from the random forest model predicting wholesale crop prices across Tanzania 
# Extract variable importance and handle naming
imp <- as.data.frame(importance(rf))
imp$Variable <- rownames(imp)

# Column to use
importance_col <- "%IncMSE"

# Pretty labels
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
  rain.sum.lag = "Lagged rainfall")

# Prepare data
imp_plot <- imp %>%
  rename(Importance = !!sym(importance_col)) %>%
  mutate(
    PrettyVar = recode(Variable, !!!pretty_labels, .default = Variable)
  ) %>%
  arrange(Importance) %>%
  mutate(
    PrettyVar = factor(PrettyVar, levels = PrettyVar)
  )

# Horizontal bar plot
fig4_imp <- ggplot(imp_plot, aes(x = PrettyVar, y = Importance)) +
  geom_col(fill = "#2c7fb8", color = "black", width = 0.7) +
  coord_flip() +
  labs(
    x = "",
    y = "Variable importance (%IncMSE)",
    title = ""
  ) +
  theme_bw(base_size = 12) +
  theme(
    axis.text = element_text(color = "black"),
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.8))

fig4_imp

ggsave(
  "C:/Users/LMADAGA/OneDrive - CGIAR/H/Tanzania Price data/Datasets/Qopen_figures/Figure 4.jpg",
  plot = fig4_imp,
  width = 7,
  height = 8,
  units = "in",
  dpi = 300)


