### Load packages
library("dplyr")
library("tidyverse")
library("rsample")
library("h2o")
library("httr")
library("dplyr")
library("doParallel")
library("purrr")

rm(list = ls(all = TRUE))

exc_alert <- readRDS("/Volumes/daiyy/2024_paper/RedAlert/data/chap_era5_daily/aqmet_excalert.rds")

exc_alert <- lapply(exc_alert, function(df) {
  df$date <- as.POSIXct(df$date, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
  return(df)
})

pollutant <- c("pm25", "pm10", "so2", "no2")

j = 1

y <- pollutant[j]
x <- c("ws", "wd", "t2m", "sp", "blh", "tp", "trend", "dow", "month", "year")

as_ftr <- c("")
as_num <- c("ws", "wd", "t2m", "sp", "blh", "tp", "trend", "y", "dow", "month", "year")

# function
xy_split <- function(df, y, x = "default") {
  # Handle default or user-specified x-vars
  df_x <- if (identical(x, "default")) {
    df %>% dplyr::select(-dplyr::all_of(y))  # Select all columns except the y variables
  } else if (is.character(x)) {
    df %>% dplyr::select(dplyr::all_of(x))
  } else {
    stop("Invalid format for predictor variables. Please provide column names as a character vector.")
  }
  
  # Create list of data frames for each response variable
  list <- setNames(lapply(y, function(y) {
    df <- cbind(df %>% dplyr::select(dplyr::all_of(y)), df_x)
    colnames(df)[1] <- "y"
    df
  }), paste0("df_", y))
  
  return(list)
}

i = 1

for(i in 1:57){

  # Process dataframe
  proc_list <- xy_split(exc_alert[[i]], y = y, x = x)
  
  # Set up parallel processing
  num_cores <- parallel::detectCores() - 1
  cluster <- parallel::makeCluster(num_cores)
  doParallel::registerDoParallel(cluster)
  
  # Start the H2O cluster (locally)
  h2o::h2o.init(max_mem_size = "20g")
  
  # Prepare dataframe
  proc_df <- data.frame(proc_list[[1]]) %>% tidyr::drop_na(all_of("y"))
  set.seed(1014)
  
  if (is.null(x) || identical(x, "default")) {
    x <- setdiff(names(proc_df), "y")  # All column names except "y"
  }
  
  # Split data based on time or randomly
  split_function <- rsample::initial_split
  
  # Apply the chosen split function with the given parameters
  data_split <- split_function(proc_df, prop = 0.8, strata = y)
  
  # Obtain training and testing data as H2O frames
  training_data <- as.data.frame(training(data_split))
  testing_data  <- as.data.frame(testing(data_split))
  
  # Function to convert columns to specified types
  type_convert <- function(data, cols_factor = NULL, cols_numeric = NULL) {
    # Ensure cols_factor and cols_numeric contain only valid column names in data
    cols_factor <- intersect(cols_factor, names(data))
    cols_numeric <- intersect(cols_numeric, names(data))
    
    # Convert specified columns to factors and numerics
    if (length(cols_factor) > 0) {
      data[cols_factor] <- lapply(data[cols_factor], as.factor)
    }
    if (length(cols_numeric) > 0) {
      data[cols_numeric] <- lapply(data[cols_numeric], as.numeric)
    }
    
    return(data)
  }
  
  # Apply type conversion to training and testing data
  training_data <- type_convert(training_data, as_ftr, as_num)
  testing_data  <- type_convert(testing_data, as_ftr, as_num)
  
  # Convert training and testing data back to H2O frames
  training_data <- h2o::as.h2o(training_data)
  testing_data  <- h2o::as.h2o(testing_data)
  
  # Perform AutoML
  auto_ml <- h2o::h2o.automl(x = x,
                             y = "y",
                             training_frame = training_data,
                             max_models = 30,
                             max_runtime_secs = NULL,
                             seed = 1014
  )
  
  # Select the best model
  model <- h2o::h2o.get_best_model(auto_ml, algorithm = "gbm", criterion = "AUTO")
  
  # Predict
  preds <- as.data.frame(h2o::h2o.predict(object = model, newdata = testing_data))
  obser <- cbind(as.data.frame(testing_data), preds)
  
  # Define evaluation metric functions
  evaluation_metrics <- list(
    n = function(x, mod = "mod", obs = "obs") {
      x <- na.omit(x[, c(mod, obs)])
      res <- nrow(x)
      data.frame(n = res)
    },
    FB = function(x, mod = "mod", obs = "obs") {
      x <- na.omit(x[, c(mod, obs)])
      res <- 2 * mean(x[[mod]] - x[[obs]]) / mean(x[[mod]] + x[[obs]])
      data.frame(FB = res)
    },
    MG = function(x, mod = "mod", obs = "obs") {
      x <- na.omit(x[, c(mod, obs)])
      res <- exp(mean(log(x[[mod]])) - mean(log(x[[obs]])))
      data.frame(MG = res)
    },
    FAC2 = function(x, mod = "mod", obs = "obs") {
      x <- na.omit(x[, c(mod, obs)])
      ratio <- x[[mod]] / x[[obs]]
      ratio <- na.omit(ratio)
      len <- length(ratio)
      if (len > 0) {
        res <- length(which(ratio >= 0.5 & ratio <= 2)) / len
      } else {
        res <- NA
      }
      data.frame(FAC2 = res)
    },
    COE = function(x, mod = "mod", obs = "obs") {
      x <- na.omit(x[, c(mod, obs)])
      res <- 1 - sum(abs(x[[mod]] - x[[obs]])) / sum(abs(x[[obs]] - mean(x[[obs]])))
      data.frame(COE = res)
    },
    r = function(x, mod = "mod", obs = "obs", ...) {
      x <- na.omit(x[, c(mod, obs)])
      res <- suppressWarnings(cor(x[[mod]], x[[obs]], ...))
      data.frame(r = res)
    },
    IOAr = function(x, mod = "mod", obs = "obs") {
      x <- na.omit(x[, c(mod, obs)])
      LHS <- sum(abs(x[[mod]] - x[[obs]]))
      RHS <- 2 * sum(abs(x[[obs]] - mean(x[[obs]])))
      if (LHS <= RHS) res <- 1 - LHS / RHS else res <- RHS / LHS - 1
      data.frame(IOAr = res)
    },
    IOA = function(x, mod = "mod", obs = "obs") {
      x <- na.omit(x[, c(mod, obs)])
      LHS <- sum(abs(x[[mod]] - x[[obs]]))
      RHS <- sum(abs(x[[mod]] - mean(x[[obs]]) + abs(x[[obs]] - mean(x[[obs]]))))
      res <- 1 - LHS / RHS
      data.frame(IOA = res)
    },
    RMSE = function(x, mod = "mod", obs = "obs") {
      x <- na.omit(x[, c(mod, obs)])
      res <- mean((x[[mod]] - x[[obs]]) ^ 2) ^ 0.5
      data.frame(RMSE = res)
    },
    RMSEu = function(x, mod = "mod", obs = "obs") {
      x <- na.omit(x[, c(mod, obs)])
      res <- mean((fitted.values(lm(formula = x[[mod]] ~ x[[obs]])) - x[[mod]]) ^ 2) ^ 0.5
      data.frame(RMSEu = res)
    },
    RMSEs = function(x, mod = "mod", obs = "obs") {
      x <- na.omit(x[, c(mod, obs)])
      res <- mean((fitted.values(lm(formula = x[[mod]] ~ x[[obs]])) - x[[obs]]) ^ 2) ^ 0.5
      data.frame(RMSEs = res)
    },
    VG = function(x, mod = "mod", obs = "obs") {
      x <- na.omit(x[, c(mod, obs)])
      res <- exp(mean((log(x[[obs]] - log(x[[mod]]))) ^ 2))
      data.frame(VG = res)
    }
  )
  #'
  #' Evaluate the performance of a model compared to observed data.
  #'
  mod_stats <- function(mydata, mod = "mod", obs = "obs",
                        stats = c("n", "FB", "MG", "FAC2", "VG", "r",
                                  "RMSEs", "RMSEu", "RMSE", "COE", "IOA", "IOAr", "default")) {
    # Check if provided stats are valid
    valid_stats <- c(names(evaluation_metrics), "default")
    if (any(!stats %in% valid_stats)) {
      stop(cat("Can't find the statistic(s)", stats[!stats %in% valid_stats], "\n"))
    }
    
    # If stats is "default", output all other parameters
    if ("default" %in% stats) {
      stats <- c("n","FAC2","r","IOAr","RMSE","RMSEu","RMSEs")
    }
    
    # Calculate the requested statistics
    results <- purrr::map_dfr(stats, function(stat) {
      evaluation_metrics[[stat]](x = mydata, mod = mod, obs = obs)
    })
    
    df_clean <- data.frame(lapply(results, na.omit))
    
    return(df_clean)
  }
  
  stats <- as.data.frame(mod_stats(obser, mod = "predict", obs = "y", stats = c("n", "FB", "FAC2", "r",
                                                         "RMSEs", "RMSEu", "RMSE", "COE", "IOAr")))
  
  city_name <- names(exc_alert[i])
  
  write.csv(stats, paste0("/rds/homes/d/daiyy/2024_paper/RedAlert/output/mod_stat/", y, "/", city_name, "_stat.csv"))
  h2o.saveModel(object = model, paste0("/rds/homes/d/daiyy/2024_paper/RedAlert/output/mod/", y, "/"), force = TRUE, filename = city_name)
  
  # Clean up
  parallel::stopCluster(cluster)
}