library(foreach)
library(doParallel)
library(randomForest)
library(tidyverse)
library(ggplot2)
library(segmented)
library(doRNG)

# Use the same predictors as pooled temporal model
predictors_lnmo <- c("maize", "rice", "sorghum", "bmillet", "fmillet", "wheat", "beans", "potato",
                     "Month", "Year", "CPI", "ttport_1", "ttcity_u5", "popdens", 
                     "bio_3", "bio_6", "bio_9", "bio_12", "bio_18", "rain.sum.lag")

trf <- tuneRF(x = mypts[, predictors_lnmo], 
              y = mypts$pkg_real, 
              stepFactor = 1.5, improve = 0.01, ntreeTry = 500, plot = FALSE, trace = TRUE)

best_mtry_val <- as.numeric(trf[which.min(trf[, 2]), 1])


registerDoParallel(cores = parallel::detectCores() - 1)

set.seed(1983)
unique_markets <- unique(mypts$Market)
unique_markets
total_markets <- length(unique_markets)
total_markets

unique(mypts[, c("Region", "Market", "Latitude", "Longitude")])
nrow(unique(mypts[, c("Region", "Market", "Latitude", "Longitude")]))

market_holdout_results <- list()


for (num_holdout in seq(2, total_markets - 1, by = 5)) { 
  
  r2_vals <- foreach(rep = 1:50, .combine = c, 
                     .packages = c("randomForest", "tidyverse"), 
                     .options.RNG = 1983) %dorng% {
                       
                       heldout_markets <- sample(unique_markets, num_holdout)
                       
                       train_subset <- mypts %>% filter(!Market %in% heldout_markets)
                       test_subset  <- mypts %>% filter(Market %in% heldout_markets)
                       
                       model_rf <- randomForest(x = train_subset[, predictors_lnmo],
                                                y = train_subset$pkg_real,
                                                ntree = 500,
                                                mtry = best_mtry_val, 
                                                importance = FALSE, 
                                                na.action = na.omit)
                       
                       preds <- predict(model_rf, newdata = test_subset)
                       actual_vals <- test_subset$pkg_real
                       
                       if (length(unique(actual_vals)) > 1) {
                         return(summary(lm(actual_vals ~ preds))$r.squared)
                       } else {
                         return(NA_real_)
                       }
                     }
  
  market_holdout_results[[num_holdout]] <- data.frame(
    Markets_Heldout = num_holdout,
    Avg_R2 = mean(r2_vals, na.rm = TRUE),
    SD_R2 = sd(r2_vals, na.rm = TRUE)
  )
  
  cat("Completed LNMO for N =", num_holdout, "\n")
}

market_holdout_df <- bind_rows(market_holdout_results)
market_holdout_df

# Visualization
lm_fit <- lm(Avg_R2 ~ Markets_Heldout, data = market_holdout_df)
seg_fit <- segmented(lm_fit, seg.Z = ~Markets_Heldout)
market_holdout_df$seg_fit <- predict(seg_fit)

lnmo_plot <- ggplot(market_holdout_df, aes(x = Markets_Heldout, y = Avg_R2)) +
  geom_ribbon(aes(ymin = Avg_R2 - SD_R2, ymax = Avg_R2 + SD_R2), fill = "grey", alpha = 0.3) +
  geom_line(aes(y = seg_fit), color = "#D55E00", linewidth = 1.2) + # Using your bicolor orange
  geom_point(color = "black", size = 2) +
  labs(title = "",
       x = "Number of Markets Held Out",
       y = expression(Mean~R^{2})) +
  theme_minimal() +
  theme(
    axis.line = element_line(color = "black"),
    # Matching font sizes to your Figure 3
    axis.text = element_text(size = 11, color = "black"),
    axis.title = element_text(size = 12, face = "bold"),
    plot.title = element_text(size = 13, face = "bold", hjust = 0.5),
    panel.grid.minor = element_blank()
  )

print(lnmo_plot)

ggsave("C:/H/Tanzania Price data/Datasets/figures_and_maps/Figure_LNMO_CV.png", 
       lnmo_plot, width = 8, height = 5, dpi = 300)



