library(foreach)
library(doParallel)
library(randomForest)
library(tidyverse)
library(ggplot2)
library(segmented)
library(doRNG)

# The same predictors as pooled temporal model
predictors_lnmo <- c("maize", "rice", "sorghum", "bmillet", "fmillet", "wheat", "beans", "potato",
                     "Month", "Year", "CPI", "ttport_1", "ttcity_u5", "popdens", 
                     "bio_3", "bio_6", "bio_9", "bio_12", "bio_18", "rain.sum.lag")

# Find the optimal mtry for the Random Forest
trf <- tuneRF(x = mypts[, predictors_lnmo], 
              y = mypts$pkg_real, 
              stepFactor = 1.5, improve = 0.01, ntreeTry = 500, plot = FALSE, trace = TRUE)

best_mtry_val <- as.numeric(trf[which.min(trf[, 2]), 1])

# PARALLEL SETUP: Speeds up the 50-repetition simulation
registerDoParallel(cores = parallel::detectCores() - 1)

set.seed(1983)
unique_markets <- unique(mypts$Market)
market_holdout_results <- list()

# SPATIAL CROSS-VALIDATION LOOP
for (num_holdout in 1:(total_markets - 1)) {
  
  # Run 50 iterations for each number of held-out markets 
  r2_vals <- foreach(rep = 1:50, .combine = c, 
                     .packages = c("randomForest", "tidyverse"), 
                     .options.RNG = 1983) %dorng% {
                       
                       # Split data: Hold out n random markets entirely from training
                       heldout_markets <- sample(unique_markets, num_holdout)
                       train_subset <- mypts %>% filter(!Market %in% heldout_markets)
                       test_subset  <- mypts %>% filter(Market %in% heldout_markets)
                       
                       model_rf <- randomForest(x = train_subset[, predictors_lnmo],
                                                y = train_subset$pkg_real,
                                                ntree = 1000, mtry = best_mtry_val)
                       
                       # Validate on unseen markets
                       preds <- predict(model_rf, newdata = test_subset)
                       actual_vals <- test_subset$pkg_real
                       
                       if (length(unique(actual_vals)) > 1) {
                         return(summary(lm(actual_vals ~ preds))$r.squared)
                       } else {
                         return(NA_real_)
                       }
                     }
  
  # Store results for plotting
  market_holdout_results[[num_holdout]] <- data.frame(
    Markets_Heldout = num_holdout,
    Avg_R2 = mean(r2_vals, na.rm = TRUE),
    SD_R2 = sd(r2_vals, na.rm = TRUE)
  )
}

# Segmented regression
market_holdout_df <- bind_rows(market_holdout_results)
lm_fit <- lm(Avg_R2 ~ Markets_Heldout, data = market_holdout_df)
seg_fit <- segmented(lm_fit, seg.Z = ~Markets_Heldout)
market_holdout_df$seg_fit <- predict(seg_fit)

# Plot Mean R2 vs. Spatial Sparsity
lnmo_plot <- ggplot(market_holdout_df, aes(x = Markets_Heldout, y = Avg_R2)) +
  geom_ribbon(aes(ymin = Avg_R2 - SD_R2, ymax = Avg_R2 + SD_R2), fill = "grey", alpha = 0.3) +
  geom_line(aes(y = seg_fit), color = "#D55E00", linewidth = 1.2) + 
  geom_point(color = "black") +
  labs(title = "",
       x = "Number of Markets Held Out",
       y = "Mean R²") +
  theme_minimal() +
  theme(
    axis.line = element_line(color = "black", size = 0.6),  
    axis.line.x = element_line(color = "black", size = 0.6),  
    axis.line.y = element_line(color = "black", size = 0.6), 
    panel.border = element_blank())

lnmo_plot