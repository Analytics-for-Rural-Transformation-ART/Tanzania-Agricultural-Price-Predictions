library(foreach)
library(doParallel)
library(randomForest)
library(tidyverse)
library(ggplot2)
library(segmented)
library(doRNG)

# This analysis evaluates the spatial generalizability of a random forest model for predicting crop prices (pkg) across 43 markets. Using a leave-N-markets-out cross-validation approach, we assess how model performance (measured by R²) changes as we incrementally increase the number of markets held out from model training. For each number of held-out markets (num_holdout from 1 to 43), the process is repeated 50 times with different random market combinations. A random forest is trained on the remaining markets and evaluated on the held-out ones. Average and standard deviation of R² values across repetitions are computed for each holdout size. A segmented (piecewise) regression is then fitted to the resulting data to visualize trends in prediction accuracy as market coverage declines.

# Predictors
predictors_lnmo <- c("maize", "rice", "sorghum", "bmillet", "fmillet", "wheat", 
                     "beans", "potato", "Month", "Year", "CPI", "ttport_1", 
                     "ttcity_u5", "popdens", "bio_3", "bio_6", "bio_9", 
                     "bio_12", "bio_18", "rain.sum.lag")

# Parallel setup
registerDoParallel(cores = parallel::detectCores() - 1)

set.seed(1983)
unique_markets <- unique(mypts$Market)
total_markets <- length(unique_markets)

market_holdout_results <- list()

# SPATIAL CROSS-VALIDATION LOOP
for (num_holdout in 1:(total_markets - 1)) {
  
  r2_vals <- foreach(rep = 1:50, .combine = c,
                     .packages = c("randomForest", "dplyr"),
                     .options.RNG = 1983) %dorng% {
                       
                       # Sample held-out markets
                       heldout_markets <- sample(unique_markets, num_holdout)
                       
                       train_subset <- mypts %>% filter(!Market %in% heldout_markets)
                       test_subset  <- mypts %>% filter(Market %in% heldout_markets)
                       
                       # Skip if too few test observations
                       if (nrow(test_subset) < 10) return(NA_real_)
                       
                       # Tune mtry ONLY on training data
                       trf <- tuneRF(
                         x = train_subset[, predictors_lnmo],
                         y = train_subset$pkg_real,
                         stepFactor = 1.5,
                         improve = 0.01,
                         ntreeTry = 300,
                         plot = FALSE,
                         trace = FALSE
                       )
                       
                       best_mtry_val <- as.numeric(trf[which.min(trf[, 2]), 1])
                       
                       # Train model
                       model_rf <- randomForest(
                         x = train_subset[, predictors_lnmo],
                         y = train_subset$pkg_real,
                         ntree = 1000,
                         mtry = best_mtry_val)
                       
                       # Predict on unseen markets
                       preds <- predict(model_rf, newdata = test_subset)
                       actual_vals <- test_subset$pkg_real
                       
                       # True out-of-sample R²
                       if (length(unique(actual_vals)) > 1) {
                         r2 <- 1 - sum((actual_vals - preds)^2) / sum((actual_vals - mean(actual_vals))^2)
                         return(r2)
                       } else {
                         return(NA_real_)
                       }
                     }
  
  # Store results
  market_holdout_results[[num_holdout]] <- data.frame(
    Markets_Heldout = num_holdout,
    Avg_R2 = mean(r2_vals, na.rm = TRUE),
    SD_R2 = sd(r2_vals, na.rm = TRUE)
  )
}

# Combine results
market_holdout_df <- bind_rows(market_holdout_results)

# Segmented regression (for visualization)
lm_fit <- lm(Avg_R2 ~ Markets_Heldout, data = market_holdout_df)
seg_fit <- segmented(lm_fit, seg.Z = ~Markets_Heldout)
market_holdout_df$seg_fit <- predict(seg_fit)

# Plot
lnmo_plot <- ggplot(market_holdout_df, aes(x = Markets_Heldout, y = Avg_R2)) +
  geom_ribbon(aes(ymin = Avg_R2 - SD_R2, ymax = Avg_R2 + SD_R2),
              fill = "grey", alpha = 0.3) +
  geom_line(aes(y = seg_fit), color = "#D55E00", linewidth = 1.2) +
  geom_point(color = "black") +
  labs(x = "Number of Markets Held Out",
       y = "Mean R²") +
  theme_minimal() +
  theme(
    axis.line = element_line(color = "black", linewidth = 0.6),
    panel.border = element_blank())

lnmo_plot
