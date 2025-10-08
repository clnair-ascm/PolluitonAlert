### =====================================================
### Load Packages
### =====================================================
library("dplyr")
library("tidyverse")
library("rsample")
library("h2o")
library("httr")
library("doParallel")
library("purrr")

rm(list = ls(all = TRUE))

### =====================================================
### Read Data
### =====================================================
exc_alert <- readRDS("/rds/projects/s/shiz-wm-netzero/users/yuqing/PNAS_TR/data/pnas_tr/era5L/aqmet_excalert.rds")

# Ensure date format
exc_alert <- lapply(exc_alert, function(df) {
  df$date <- as.POSIXct(df$date, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
  return(df)
})

### =====================================================
### Settings
### =====================================================
pollutant <- c("pm25", "pm10", "so2", "no2")
j <- as.numeric(Sys.getenv("POLLUTANT_ID"))

# Default to 3 (so2) if environment variable not set
if (is.na(j) || j < 1 || j > length(pollutant)) {
  j <- 3
  message("⚠️ POLLUTANT_ID not set or invalid — defaulting to so2")
}

y <- pollutant[j]
message("✅ Using pollutant ID = ", j, " → ", y)

x <- c("ws", "wd", "t2m", "sp", "blh", "tp", "trend", "dow", "month", "year")

as_ftr <- c("")
as_num <- c("ws", "wd", "t2m", "sp", "blh", "tp", "trend", "y", "dow", "month", "year")

# Helpers (same as before) -----------------------------------------------------
xy_split <- function(df, y, x = "default") {
  df_x <- if (identical(x, "default")) {
    df %>% dplyr::select(-dplyr::all_of(y))
  } else if (is.character(x)) {
    df %>% dplyr::select(dplyr::all_of(x))
  } else {
    stop("Invalid format for predictor variables.")
  }
  list <- setNames(lapply(y, function(y) {
    df <- cbind(df %>% dplyr::select(dplyr::all_of(y)), df_x)
    colnames(df)[1] <- "y"
    df
  }), paste0("df_", y))
  return(list)
}

type_convert <- function(data, cols_factor = NULL, cols_numeric = NULL) {
  cols_factor <- intersect(cols_factor, names(data))
  cols_numeric <- intersect(cols_numeric, names(data))
  if (length(cols_factor) > 0) data[cols_factor] <- lapply(data[cols_factor], as.factor)
  if (length(cols_numeric) > 0) data[cols_numeric] <- lapply(data[cols_numeric], as.numeric)
  return(data)
}

# Evaluation metrics (unchanged) ----------------------------------------------
evaluation_metrics <- list(
  n = function(x, mod = "mod", obs = "obs") {
    x <- na.omit(x[, c(mod, obs)])
    data.frame(n = nrow(x))
  },
  FB = function(x, mod = "mod", obs = "obs") {
    x <- na.omit(x[, c(mod, obs)])
    data.frame(FB = 2 * mean(x[[mod]] - x[[obs]]) / mean(x[[mod]] + x[[obs]]))
  },
  MG = function(x, mod = "mod", obs = "obs") {
    x <- na.omit(x[, c(mod, obs)])
    data.frame(MG = exp(mean(log(x[[mod]])) - mean(log(x[[obs]]))))
  },
  FAC2 = function(x, mod = "mod", obs = "obs") {
    x <- na.omit(x[, c(mod, obs)])
    ratio <- x[[mod]] / x[[obs]]
    ratio <- na.omit(ratio)
    len <- length(ratio)
    res <- if (len > 0) length(which(ratio >= 0.5 & ratio <= 2)) / len else NA
    data.frame(FAC2 = res)
  },
  COE = function(x, mod = "mod", obs = "obs") {
    x <- na.omit(x[, c(mod, obs)])
    data.frame(COE = 1 - sum(abs(x[[mod]] - x[[obs]])) / sum(abs(x[[obs]] - mean(x[[obs]]))))
  },
  r = function(x, mod = "mod", obs = "obs", ...) {
    x <- na.omit(x[, c(mod, obs)])
    data.frame(r = suppressWarnings(cor(x[[mod]], x[[obs]], ...)))
  },
  IOAr = function(x, mod = "mod", obs = "obs") {
    x <- na.omit(x[, c(mod, obs)])
    LHS <- sum(abs(x[[mod]] - x[[obs]]))
    RHS <- 2 * sum(abs(x[[obs]] - mean(x[[obs]])))
    res <- if (LHS <= RHS) 1 - LHS / RHS else RHS / LHS - 1
    data.frame(IOAr = res)
  },
  IOA = function(x, mod = "mod", obs = "obs") {
    x <- na.omit(x[, c(mod, obs)])
    LHS <- sum(abs(x[[mod]] - x[[obs]]))
    RHS <- sum(abs(x[[mod]] - mean(x[[obs]]) + abs(x[[obs]] - mean(x[[obs]]))))
    data.frame(IOA = 1 - LHS / RHS)
  },
  RMSE = function(x, mod = "mod", obs = "obs") {
    x <- na.omit(x[, c(mod, obs)])
    data.frame(RMSE = mean((x[[mod]] - x[[obs]]) ^ 2) ^ 0.5)
  },
  RMSEu = function(x, mod = "mod", obs = "obs") {
    x <- na.omit(x[, c(mod, obs)])
    data.frame(RMSEu = mean((fitted.values(lm(formula = x[[mod]] ~ x[[obs]])) - x[[mod]]) ^ 2) ^ 0.5)
  },
  RMSEs = function(x, mod = "mod", obs = "obs") {
    x <- na.omit(x[, c(mod, obs)])
    data.frame(RMSEs = mean((fitted.values(lm(formula = x[[mod]] ~ x[[obs]])) - x[[obs]]) ^ 2) ^ 0.5)
  },
  VG = function(x, mod = "mod", obs = "obs") {
    x <- na.omit(x[, c(mod, obs)])
    data.frame(VG = exp(mean((log(x[[obs]] - log(x[[mod]]))) ^ 2)))
  }
)

mod_stats <- function(mydata, mod = "mod", obs = "obs",
                      stats = c("n", "FB", "FAC2", "r", "RMSEs", "RMSEu", "RMSE", "COE", "IOAr")) {
  results <- purrr::map_dfr(stats, function(stat) {
    evaluation_metrics[[stat]](x = mydata, mod = mod, obs = obs)
  })
  df_clean <- data.frame(lapply(results, na.omit))
  return(df_clean)
}

### =====================================================
### SLURM ARRAY INDEX — one city per task
### =====================================================
task_id <- as.numeric(Sys.getenv("SLURM_ARRAY_TASK_ID"))
if (is.na(task_id)) stop("SLURM_ARRAY_TASK_ID not found!")

city_name <- names(exc_alert)[task_id]
message("Processing city ", task_id, ": ", city_name)

log_file <- "/rds/projects/s/shiz-wm-netzero/users/yuqing/PNAS_TR/data/pnas_tr/era5L/error_log.txt"

tryCatch({
  proc_list <- xy_split(exc_alert[[city_name]], y = y, x = x)
  proc_df <- data.frame(proc_list[[1]]) %>% tidyr::drop_na(all_of("y"))
  set.seed(1014)
  if (is.null(x) || identical(x, "default")) x <- setdiff(names(proc_df), "y")
  
  # Split
  data_split <- rsample::initial_split(proc_df, prop = 0.8, strata = y)
  training_data <- as.data.frame(training(data_split))
  testing_data  <- as.data.frame(testing(data_split))
  training_data <- type_convert(training_data, as_ftr, as_num)
  testing_data  <- type_convert(testing_data, as_ftr, as_num)
  
  num_cores <- parallel::detectCores() - 1
  cluster <- parallel::makeCluster(num_cores)
  doParallel::registerDoParallel(cluster)
  
  max_retries <- 3
  base_port   <- 54000 + (as.numeric(Sys.getenv("SLURM_ARRAY_TASK_ID", "1")) * 2)
  
  for (try_i in seq_len(max_retries)) {
    port_use <- base_port + (try_i - 1) * 2  # ensure each retry jumps by 2 ports
    cat("Attempt", try_i, "starting H2O on port", port_use, "\n")
    
    h2o_conn <- try(
      h2o::h2o.init(
        ip = "localhost",
        port = port_use,
        max_mem_size = "20g",
        nthreads = num_cores
      ),
      silent = TRUE
    )
    
    if (!inherits(h2o_conn, "try-error")) {
      cat("✅ Successfully connected to H2O on port", port_use, "\n")
      break
    }
    
    cat("⚠️  Failed on port", port_use, "— retrying...\n")
    Sys.sleep(5)
  }
  
  if (inherits(h2o_conn, "try-error")) stop("❌ Failed to start H2O after multiple retries")
  
  training_data <- h2o::as.h2o(training_data)
  testing_data  <- h2o::as.h2o(testing_data)
  
  auto_ml <- h2o::h2o.automl(
    x = x, y = "y", training_frame = training_data,
    max_models = 30, seed = 1014
  )
  
  model <- h2o::h2o.get_best_model(auto_ml, algorithm = "gbm", criterion = "AUTO")
  
  preds <- as.data.frame(h2o::h2o.predict(model, testing_data))
  obser <- cbind(as.data.frame(testing_data), preds)
  stats <- as.data.frame(mod_stats(obser, mod = "predict", obs = "y"))
  
  dir.create(paste0("/rds/projects/s/shiz-wm-netzero/users/yuqing/PNAS_TR/data/pnas_tr/era5L/mod_stat/", y), showWarnings = FALSE, recursive = TRUE)
  dir.create(paste0("/rds/projects/s/shiz-wm-netzero/users/yuqing/PNAS_TR/data/pnas_tr/era5L/mod/", y), showWarnings = FALSE, recursive = TRUE)
  
  write.csv(stats, paste0("/rds/projects/s/shiz-wm-netzero/users/yuqing/PNAS_TR/data/pnas_tr/era5L/mod_stat/",
                          y, "/", city_name, "_stat.csv"), row.names = FALSE)
  
  h2o.saveModel(model, path = paste0("/rds/projects/s/shiz-wm-netzero/users/yuqing/PNAS_TR/data/pnas_tr/era5L/mod/",
                                     y, "/"), force = TRUE, filename = city_name)
  
  parallel::stopCluster(cluster)
  h2o::h2o.shutdown(prompt = FALSE)
  Sys.sleep(10)
  write(paste(Sys.time(), "- Success:", city_name), file = log_file, append = TRUE)
  
}, error = function(e) {
  message("Error in city ", city_name, ": ", e$message)
  write(paste(Sys.time(), "- Error:", city_name, ":", e$message),
        file = log_file, append = TRUE)
  try(parallel::stopCluster(cluster), silent = TRUE)
  try(h2o::h2o.shutdown(prompt = FALSE), silent = TRUE)
  Sys.sleep(5)
}
)
