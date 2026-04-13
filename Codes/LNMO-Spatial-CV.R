# Leave-N-Markets-Out Cross-Validation (LNMO-CV)
# This script evaluates how well the pooled random forest model
# generalizes to markets that were not used in training.
#
# For each value of n (number of markets held out), the model is:
# 1. trained on the remaining N - n markets
# 2. tested on the n held-out markets
# 3. repeated across up to 50 unique market combinations
#
# The goal is to see how prediction accuracy changes as the
# number of observed/training markets becomes smaller.

library(foreach)
library(doParallel)
library(randomForest)
library(tidyverse)
library(ggplot2)
library(segmented)
library(doRNG)


# Predictor variables used in the pooled random forest model
predictors_lnmo <- c(
  "maize", "rice", "sorghum", "bmillet", "fmillet", "wheat",
  "beans", "potato", "Month", "Year", "CPI", "ttport_1",
  "ttcity_u5", "popdens", "bio_3", "bio_6", "bio_9",
  "bio_12", "bio_18", "rain.sum.lag")


# Parallel setup
# Parallel computing allows several model fits to run at the
# same time, which reduces runtime.
#
# Here we use only 2 cores rather than all available cores.
# This is safer for memory usage and reduces the risk of the
# computer crashing during repeated random forest fitting.
n_cores <- 2
cl <- parallel::makeCluster(n_cores)
doParallel::registerDoParallel(cl)

# Set random seed for reproducibility
set.seed(1983)


# Define market universe
# unique_markets = all distinct markets in the dataset
# total_markets  = total number of markets available i.e 43
unique_markets <- unique(mypts$Market)
unique_markets

total_markets <- length(unique_markets)
total_markets

# Empty list to store results for each holdout size
market_holdout_results <- list()


# Tune mtry ONCE outside the LNMO loop
# Why tune outside the loop?
# - tuneRF() is computationally expensive
# - tuning inside every iteration would make the code much
#   slower and heavier on memory
# - using one tuned mtry value across all LNMO iterations
#   keeps the model specification consistent across holdout sizes
#
# We therefore tune mtry once on the full dataset, then hold
# that value fixed when fitting all LNMO models.
trf_global <- tuneRF(
  x = mypts[, predictors_lnmo],
  y = mypts$pkg_real,
  stepFactor = 1.5,
  improve = 0.01,
  ntreeTry = 500,
  plot = FALSE,
  trace = FALSE)

best_mtry_val <- as.numeric(trf_global[which.min(trf_global[, 2]), 1])
best_mtry_val


# Spatial cross-validation loop
# num_holdout = number of markets held out from model training
# This ranges from 1 to total_markets - 1
#
# Example:
# - if num_holdout = 1, train on 42 markets and test on 1
# - if num_holdout = 42, train on 1 market and test on 42
for (num_holdout in 1:(total_markets - 1)) {
  
  # Set a reproducible seed for each holdout size
  set.seed(1983 + num_holdout)
  
  # Count how many unique combinations of markets are possible
  total_combos <- choose(total_markets, num_holdout)
  
  # Build the list of held-out market combinations
  # If there are 50 or fewer possible combinations, use ALL of them.
  # If there are more than 50 possible combinations, sample 50
  # unique combinations at random.
  #
  # This keeps the exercise computationally manageable while still
  # covering a broad set of holdout patterns.
  if (total_combos <= 50) {
    combo_list <- combn(unique_markets, num_holdout, simplify = FALSE)
  } else {
    combo_list <- vector("list", 50)
    seen_combos <- character(0)
    i <- 1
    
    while (i <= 50) {
      combo <- sort(sample(unique_markets, num_holdout))
      combo_key <- paste(combo, collapse = " | ")
      
      if (!(combo_key %in% seen_combos)) {
        combo_list[[i]] <- combo
        seen_combos <- c(seen_combos, combo_key)
        i <- i + 1
      }
    }
  }
  

  # Fit and evaluate the model across the selected combinations
  # foreach() loops over each held-out combination
  # %dopar% means these runs are executed in parallel
  #
  # For each combination:
  # - heldout_markets define the test set
  # - the remaining markets define the training set
  # - a random forest is trained on the training markets
  # - predictions are generated for the held-out markets
  # - R² is computed using only the held-out observations
  r2_vals <- foreach(
    heldout_markets = combo_list,
    .combine = c,
    .packages = c("randomForest", "dplyr")
  ) %dopar% {
    
    train_subset <- mypts %>% filter(!Market %in% heldout_markets)
    test_subset  <- mypts %>% filter(Market %in% heldout_markets)
    
    # Safety check
    # Skip combinations with too little training or testing data.
    # This prevents unstable model fitting and unreliable R² values.
    if (nrow(test_subset) < 10 || nrow(train_subset) < 20) {
      return(NA_real_)
    }
    
    # Fit RF using fixed hyperparameters
    model_rf <- randomForest(
      x = train_subset[, predictors_lnmo],
      y = train_subset$pkg_real,
      ntree = 500,
      mtry = best_mtry_val,
      importance = FALSE)
    
    # Predict on held-out markets only
    preds <- predict(model_rf, newdata = test_subset)
    actual_vals <- test_subset$pkg_real
    

    # Compute R² only if the test data have some variation
    # If all actual values are identical, R² is not meaningful,
    # so we return NA for that combination.
    if (length(unique(actual_vals)) > 1) {
      return(summary(lm(actual_vals ~ preds))$r.squared)
    } else {
      return(NA_real_)
    }
  }
  

  # Summarize performance for this holdout size
  # Avg_R2 = mean predictive performance across evaluated combinations
  # SD_R2  = variability in predictive performance across combinations
  market_holdout_results[[as.character(num_holdout)]] <- data.frame(
    Markets_Heldout = num_holdout,
    N_Combinations_Evaluated = length(combo_list),
    Avg_R2 = mean(r2_vals, na.rm = TRUE),
    SD_R2 = sd(r2_vals, na.rm = TRUE))
  
  # Save progress after each holdout size
  # This is useful in long runs so that partial results are not
  # lost if the session stops unexpectedly.
  tmp_df <- bind_rows(market_holdout_results)
  write.csv(
    tmp_df,
    "C:/Users/LMADAGA/OneDrive - CGIAR/H/Tanzania Price data/Datasets/Qopen_figures/market_holdout_df_progress.csv",
    row.names = FALSE)
  
  cat(
    "Completed LNMO for N =", num_holdout,
    "using", length(combo_list), "combinations\n"
  )
}

# Stop the parallel workers after the loop is complete
parallel::stopCluster(cl)


# Final results table
market_holdout_df <- bind_rows(market_holdout_results)
market_holdout_df

# Save final summary results
write.csv(
  market_holdout_df,
  "C:/Users/LMADAGA/OneDrive - CGIAR/H/Tanzania Price data/Datasets/Qopen_figures/market_holdout_df.csv",
  row.names = FALSE)


# Segmented regression
# This is used only for visualization.
# It fits a piecewise linear trend to show how mean R² changes
# as more markets are held out.
lm_fit <- lm(Avg_R2 ~ Markets_Heldout, data = market_holdout_df)
seg_fit <- segmented(lm_fit, seg.Z = ~Markets_Heldout)
market_holdout_df$seg_fit <- predict(seg_fit)

# Plot output
lnmo_plot <- ggplot(market_holdout_df, aes(x = Markets_Heldout, y = Avg_R2)) +
  geom_ribbon(
    aes(ymin = Avg_R2 - SD_R2, ymax = Avg_R2 + SD_R2),
    fill = "grey70",
    alpha = 0.35
  ) +
  geom_line(aes(y = seg_fit), color = "#D55E00", linewidth = 1.2) +
  geom_point(color = "black", size = 2) +
  labs(
    x = "Number of markets held out",
    y = expression("Mean " * R^2)
  ) +
  theme_minimal(base_size = 12) +
  theme(
    axis.line = element_line(color = "black", linewidth = 0.6),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.8),
    panel.grid.major = element_line(color = "grey85", linewidth = 0.3),
    panel.grid.minor = element_blank(),
    axis.text = element_text(color = "black"),
    axis.title = element_text(color = "black"))

lnmo_plot

# Save final figure for the paper
ggsave(filename = "C:/Users/LMADAGA/OneDrive - CGIAR/H/Tanzania Price data/Datasets/Qopen_figures/Figure 6.jpg",
  plot = lnmo_plot,
  width = 8,
  height = 6,
  units = "in",
  dpi = 300,
  quality = 100)
