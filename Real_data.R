#############################################
# CLASS IMBALANCE EXPERIMENTS FOR REAL DATA #
#############################################
# This file is used to run 10x10 CV on data from "data_X.csv" where X can be set in variable `data_id`.
# The script will run the experiments for both MLE and LASSO but the dataset needs to be changed manually by setting `data_id`.
# Reproducibility ensured by do_experiment() parameter seed_number (set by default).
# WARNING: RDS files created by this script are large in size (in total around 2GB).

library(tidyverse) # General R work
library(caret) # Modeling framework
library(glmnet) # LASSO
library(pROC) # AUC
library(doSNOW) # Parallel processing
library(DMwR) # SMOTE

source("functions.R")

# PARAMETERS ----
data_id <- 271 # IDS used in the paper are c(271, 326, 332)
# For k x n CV, k = REPEATS and n = FOLDS.
FOLDS <- 10
REPEATS <- 10
# NUM_THREADS is the number of threads used by the script.
NUM_THREADS <- min(10, future::availableCores() - 1)

# DATA ----
data_path <- paste0("data_", data_id, ".csv")
data <- data.table::fread(file = data_path, sep = ";", data.table = FALSE)
data <- mutate(data, y = factor(y, levels = c("BAD", "GOOD")))
tau <- mean(data$y == "GOOD")

# EXPERIMENTS ----
type_vec <- c("MLE", "LASSO")
for (i in seq_along(type_vec)) {
  res <- do_experiment(dataset = data,
                       model_info = get_model_info(type = type_vec[i]), 
                       cv_folds = FOLDS, 
                       cv_repeats = REPEATS, 
                       parallel_threads = NUM_THREADS)
  saveRDS(res, file = paste0("data_", data_id, "_", type_vec[i], "_res.RDS"))
}