#############
# FUNCTIONS #
#############
# Separate file for storing functions used in other script files.

# Modified existing glm info
get_model_info <- function(type) {
  
  if (type == "MLE") { 
    model <- getModelInfo("glm", regex = FALSE)[[1]]
    model$label <- "MLE"
    model$library <- "glmnet"
    
    model$fit <- function(x, y, wts, param, lev, last, classProbs, ...) {
      
      x_mat <- if(is.matrix(x)) x else as.matrix(x)
      
      # This has been modified for confidentiality, this version will get MLE estimates without
      # variable selection.
      
      fit_logit <- glm(data = as.data.frame(x_mat), 
                       formula = y ~ .,
                       family = "binomial")
      
      # Beta_0 correction
      y_avg <- mean(y == "GOOD")
      fit_logit$coefficients[[1]] <- fit_logit$coefficients[[1]] - log((1 - tau) / tau * y_avg / (1 - y_avg))
      
      return(fit_logit)
      
    }
    
  } else if (type == "LASSO") {
    
    model <- getModelInfo("glmnet", regex = FALSE)[[1]]
    model$label <- "LASSO"
    
    # Do not search over parameters
    glm_info <- getModelInfo("glm", regex = FALSE)[[1]]
    model$parameters <- glm_info$parameters
    model$grid <- glm_info$grid
    model$loop <- glm_info$loop
    model$sort <- glm_info$sort
    
    model$fit <- function(x, y, wts, param, lev, last, classProbs, ...) {
      x_mat <- if(is.matrix(x)) x else as.matrix(x)
      
      lasso_cv <- glmnet::cv.glmnet(x_mat, y,
                                     alpha = 1,
                                     family = "binomial",
                                     type.measure = "auc",
                                     nfolds = 5, 
                                     parallel = TRUE)
      
      lambda_cv <- lasso_cv$lambda.1se
      
      fit_lasso <- glmnet::glmnet(x_mat, y,
                                  lambda = lambda_cv,
                                  alpha = 1,
                                  family = "binomial")
      # Beta_0 correction
      y_avg <- mean(y == "GOOD")
      fit_lasso$a0 <- fit_lasso$a0 - log((1 - tau) / tau * y_avg / (1 - y_avg))
      
      return(fit_lasso)
    }
    
  } else {
    stop("Incorrect type provided to get_model_info()")
  }
  
  return(model)
}

# Modified twoClassSummary() function from `caret`.
auc_and_qs <- function (data, lev = NULL, model = NULL) {
  
  if (length(lev) > 2) {
    stop(paste("Your outcome has", length(lev), "levels. The auc_and_qs() function isn't appropriate."))
  }
  if (!requireNamespace("pROC", quietly = TRUE)) {
    stop(paste("package pROC is required"), 
         call. = FALSE)
  }
  if (!all(levels(data[, "pred"]) == lev)) {
    stop("levels of observed and predicted data do not match")
  }
  rocObject <- try(pROC::roc(data$obs, data[, lev[1]], direction = ">", 
                             quiet = TRUE), silent = TRUE)
  rocAUC <- if (inherits(rocObject, "try-error")) NA else rocObject$auc
  
  # This function cannot be found by parallelization when it's defined in global env,
  # which is why it is defined here.
  qs <- function(prob, result) {
    b <- mean((prob - result)^2)
    return(b)
  }
  out <- c(rocAUC, qs(data[, lev[1]], as.numeric(data[, "obs"] == lev[1])))
  names(out) <- c("AUC", "QS")
  
  return(out)
}

# dataset should have y as a column name for dependent variable.
# y should be a factor variable with values `good` and `bad`, where levels(y)[1] == "bad".
do_experiment <- function(dataset, model_info, 
                          cv_folds, cv_repeats, 
                          parallel_threads, 
                          seed_number = 2020, # This makes sure that the results are reproducible.
                          sub_samplings = list("control" = NULL, "up" = "up", "smote" = "smote")) {
  
  start_time <- Sys.time()
  res <- list()
  # Setting the seed and generating row numbers that will be used in CV splits.
  set.seed(seed_number)
  train_id <- createMultiFolds(y = dataset$y, k = cv_folds, times = cv_repeats)
  
  # Prepare training scheme.
  control <- trainControl(method="repeatedcv",
                          number = cv_folds, 
                          repeats = cv_repeats, 
                          index = train_id, 
                          classProbs = TRUE,
                          summaryFunction = auc_and_qs, 
                          allowParallel = TRUE)
  res$control <- control
  
  cl <- makeCluster(parallel_threads)
  registerDoSNOW(cl)
  clusterExport(cl, list = "tau")
  
  # Loop over the different kinds of subsampling with the same folds (from `control` and `train_id`)
  model_ls <- list()
  for (i in seq_along(sub_samplings)) {
    
    control$sampling <- sub_samplings[[i]]
    model_ls[[paste0(c("model", 
                       sub_samplings[[i]]), 
                     collapse = "_")]] <- train(y ~ ., 
                                                data = dataset, 
                                                method = model_info, 
                                                trControl = control, 
                                                metric = "AUC") # This is irrelevant. Set to not throw errors.
    
  }
  stopCluster(cl)
  res$model_ls <- model_ls
  
  results <- resamples(model_ls, 
                       modelNames = names(sub_samplings))
  res$results <- results
  
  end_time <- Sys.time()
  res$time <- end_time - start_time
  return(res)
}

# n1 is training set size and n2 is testing set size.
adjusted_t_test <- function(diff_vec, n2_to_n1) {
  J <- length(diff_vec)
  adj_var <- (1/J + n2_to_n1) * var(diff_vec)
  stat <- mean(diff_vec) / sqrt(adj_var)
  p_value <- 2 * pt(q = abs(stat), df = J - 1, lower.tail = FALSE)
  CI_lower <- mean(diff_vec) - qt(p = 0.975, df = J - 1) * sqrt(adj_var)
  CI_upper <- mean(diff_vec) + qt(p = 0.975, df = J - 1) * sqrt(adj_var)
  return(list(t = stat,
              p_value = p_value,
              mean = mean(diff_vec),
              CI_lower = CI_lower,
              CI_upper = CI_upper))
}

make_pretty_diff_table <- function(diff_table) {
  pretty_diff_table <- diff_table %>% 
    ungroup(name) %>% 
    mutate(method = str_extract(name, pattern = "(?<=_)[^_]*$"),
           measure = str_extract(name, pattern = "^[^_]*(?=_)")) %>% 
    select(type, method, measure, mean, adj_t_test_p_val) %>% 
    arrange(desc(type), desc(method), measure) %>% 
    mutate(sig = case_when(adj_t_test_p_val > 0.05/24 ~ "", # Bonferroni correction for 24 hypothesis tests.
                           adj_t_test_p_val > 0.01/24 ~ "*",
                           adj_t_test_p_val > 0.001/24 ~ "**",
                           TRUE ~ "***"),
           mean = paste0(format(round(mean, 4), scientific = FALSE), sig)) %>% 
    select(-adj_t_test_p_val, -sig)
  return(pretty_diff_table)
}
