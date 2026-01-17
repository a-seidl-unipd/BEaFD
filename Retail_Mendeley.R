# Required libraries
library(dplyr)
library(tidyr)
library(purrr)
library(lubridate) 

# Reading in of raw data
data <- read.csv("./Projekt/Data/Retail_Dataset_Mendeley.csv")
# Removing Product_id & Product_Code
data$Product_id <- NULL
data$Product_Code <- NULL
# Setting date from char to Date format
data$Date <- as.Date(data$Date, format = "%m/%d/%Y")
sum(is.na(data$Date))
# Function for converting booleans into 
convert_to_boolean <- function(data, column_names) {
  for (col in column_names) {
    print(col)
    print(unique(data[[col]]))
    data[[col]] <- as.logical(data[[col]])
    print(unique(data[[col]]))
  }
  return(data)
}
# Setting Open, Promo and SchoolHoliday to binary variable
data <- convert_to_boolean(data, c("Open", "Promo", "SchoolHoliday"))
# Setting Warehouse from Whse_[A-Z] to [A-Z]
unique(data$Warehouse)
data$Warehouse <- substr(data$Warehouse, nchar(data$Warehouse), nchar(data$Warehouse))
unique(data$Warehouse)
# Setting Product Category from char to int
unique(data$Product_Category)
data$Product_Category <- as.integer(substr(data$Product_Category, nchar(data$Product_Category) - 2, nchar(data$Product_Category)))
unique(data$Product_Category)
# Grouping data to main categories while setting Petrol_price to weighted mean
data <- data %>%
  group_by(Warehouse, Product_Category, Date, Open, Promo, StateHoliday, SchoolHoliday) %>%
  summarise(
    Petrol_price = weighted.mean(x = Petrol_price, w = Order_Demand),
    Order_Demand = sum(Order_Demand),
  ) %>%
  ungroup()
# Turning the parameters of ARIMA into a function increasing the n-th of 6 parameters
increment_arima_param <- function(order, seasonal, i) {
  if(i >= 1 && i <= 3) {
    order[i] <- order[i] + 1
  } else if(i >= 4 && i <= 6) {
    seasonal[i - 3] <- seasonal[i - 3] + 1
  }
  
  list(order = order, seasonal = seasonal)
}
# Increasing every parameter by 1 while keeping the rest equal comparing the different p-values
select_best_candidate_simple <- function(ts_data, current_order, current_seasonal, lag_lb = 4, period = 13) {
  
  best_p <- -Inf # Base p-value
  best_fit <- NULL # best ARIMA
  best_order <- current_order # Best order array
  best_seasonal <- current_seasonal # best seasonal array
  
  for(i in 1:6) {
    # Increment one parameter
    params <- increment_arima_param(current_order, current_seasonal, i)
    
    # Trying ARIMA and catching errors in case of dysfunctional Model
    fit <- tryCatch(
      arima(ts_data, order = params$order, seasonal = list(order = params$seasonal, period = period)),
      error = function(e) NULL,
      warning = function(w) {
        # Skipping non-convergent fits
        NULL
      }
    )
    
    if(!is.null(fit)) {
      # Compute Ljung-Box p-value
      fitdf <- sum(params$order[c(1,3)]) + sum(params$seasonal[c(1,3)])  # Sum of parameter values
      lb <- Box.test(residuals(fit), lag = lag_lb, type = "Ljung-Box", fitdf = fitdf) # Ljung-Box p-value
      
      # Keep best p-value
      if(lb$p.value > best_p) {
        best_p <- lb$p.value # best p-value
        best_fit <- fit # best fit
        best_order <- params$order # best order array
        best_seasonal <- params$seasonal # best seasonal array
      }
    }
    # Otherwise skip
  }
  # Returning best candidate
  return(list(fit = best_fit,
              order = best_order,
              seasonal = best_seasonal,
              p_value = best_p))
}

# Preparation of weekly data
build_time_series <- function(df, warehouse = NULL, product_category = NULL,
                              period = 13, p_value_threshold = 0.05, max_iter = 5,
                              lag_lb = 4) {
  
  # Filter only if arguments are provided (necessary dependent on how is grouped)
  filtered_df <- df
  if(!is.null(warehouse)) {
    filtered_df <- filtered_df %>% filter(Warehouse == warehouse)
  }
  if(!is.null(product_category)) {
    filtered_df <- filtered_df %>% filter(Product_Category == product_category)
  }
  
  # Fill missing dates
  filtered_df <- filtered_df %>%
    select(Date, Order_Demand) %>%
    arrange(Date) %>%
    tidyr::complete(
      Date = seq(min(Date), max(Date), by = "day"),
      fill = list(Order_Demand = 0)
    )
  # Getting time series data
  ts_data <- ts(filtered_df$Order_Demand, frequency = period)
  
  # Initialize ARIMA parameters
  current_order <- c(0,0,0)
  current_seasonal <- c(0,0,0)
  
  # First candidate
  best <- tryCatch(
    select_best_candidate_simple(ts_data, current_order, current_seasonal,
                                 lag_lb = lag_lb, period = period),
    error = function(e) NULL
  )
  # Return NA in case of misfit
  if(is.null(best$fit)) {
    return(list(fit = NULL, order = NA, seasonal = NA, p_value = NA))
  }
  
  last_successful <- best  # Keep the last successful fit
  iter <- 1 # iteration counter
  # Continuning until p_value is high enough or threshold of max_iter reached
  while(last_successful$p_value < p_value_threshold & iter <= max_iter) {
    # selecting best candidate for ARIMA
    temp <- tryCatch(
      select_best_candidate_simple(ts_data, last_successful$order, last_successful$seasonal,
                                   lag_lb = lag_lb, period = period),
      error = function(e) NULL
    )
    # Trying up to two times improving p-value
    ## Idea of improving p-value even if last increase of best parameter lead to decrease
    if(!is.null(temp$fit)) {
      if(temp$p_value > last_successful$p_value) {
        last_successful <- temp
      } else {
        temp2 <- tryCatch(
          select_best_candidate_simple(ts_data, temp$order, temp$seasonal,
                                       lag_lb = lag_lb, period = period),
          error = function(e) NULL
        )
        if(!is.null(temp2$fit) && temp2$p_value > last_successful$p_value) {
          last_successful <- temp2
        } else {
          break
        }
      }
    } else {
      break
    }
    
    iter <- iter + 1
  }
  
  return(last_successful)
}

# ARIMA analysis
run_arima_analysis <- function(df, 
                               group_warehouse = TRUE, 
                               group_product_category = TRUE, 
                               period = 13, p_value_threshold = 0.05, max_iter = 5,
                               lag_lb = 4) {
  
  # Determine grouping columns
  group_cols <- c()
  if(group_warehouse) group_cols <- c(group_cols, "Warehouse")
  if(group_product_category) group_cols <- c(group_cols, "Product_Category")
  
  # If no grouping, create a dummy column for easier handling
  if(length(group_cols) == 0) {
    df <- df %>% mutate(dummy_group = 1)
    group_cols <- "dummy_group"
  }
  
  # Get all unique combinations
  combinations <- df %>% distinct(across(all_of(group_cols)))
  
  # Apply build_time_series to each combination
  results <- combinations %>%
    mutate(
      model_result = pmap(
        select(., all_of(group_cols)),
        function(...) {
          args <- list(...)
          # Extract warehouse or product group if present
          wh <- if("Warehouse" %in% names(args)) args$Warehouse else NULL
          pc <- if("Product_Category" %in% names(args)) args$Product_Category else NULL
          
          build_time_series(df, wh, pc, period = period,
                            p_value_threshold = p_value_threshold,
                            max_iter = max_iter, lag_lb = lag_lb)
        }
      )
    ) %>%
    # add order array, seasonal array and p-value
    mutate(
      order = map(model_result, "order"),
      seasonal = map(model_result, "seasonal"),
      p_value = map_dbl(model_result, "p_value")
    )
  
  return(results)
}

# Grouping after Warehouse and Product Category by Date
data_wh_pc <- data %>%
  mutate(
    # Week starting with Monday
    Date = floor_date(Date, unit = "week", week_start = 1)
  ) %>%
  # Group data by warehouse, product_category, date with sum of order_demand
  group_by(Warehouse, Product_Category, Date) %>%
  summarise(
    Order_Demand = sum(Order_Demand, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(Warehouse, Product_Category, Date)

# Train/Test Split
cutoff_date <- max(data_wh_pc$Date) - weeks(4)  # last 4 weeks for comparison

train_data <- data_wh_pc %>% filter(Date <= cutoff_date)
test_data  <- data_wh_pc %>% filter(Date > cutoff_date)

# Set grouping flags
group_warehouse <- TRUE
group_product_category <- FALSE

# --- Run ARIMA analysis on training data ---
results <- run_arima_analysis(
  train_data,
  group_warehouse = group_warehouse,
  group_product_category = group_product_category,
  p_value_threshold = 0.2,
  max_iter = 5,
  lag_lb = 4
)
# Refitting and forecasting
refit_and_forecast <- function(df, warehouse = NULL, product_category = NULL,
                                      order, seasonal, h = 4, period = 13, level = 0.2) {
  
  # z-Score for confidence interval
  z <- qnorm(0.5 + level / 2)
  
  # Filter for Warehouse/Product_Category
  filtered_df <- df
  if(!is.null(warehouse)) filtered_df <- filtered_df %>% filter(Warehouse == warehouse)
  if(!is.null(product_category)) filtered_df <- filtered_df %>% filter(Product_Category == product_category)
  
  # Weekly aggregation
  filtered_df <- filtered_df %>%
    arrange(Date) %>%
    mutate(Date = lubridate::floor_date(Date, "week")) %>%
    group_by(Date) %>%
    summarise(Order_Demand = sum(Order_Demand, na.rm = TRUE)) %>%
    ungroup()
  
  # last h weeks
  last_weeks <- tail(filtered_df$Date, h)
  
  # Weeks before as training data
  train_df <- filtered_df %>% filter(Date < min(last_weeks))
  
  # Check for enough data being available
  if(nrow(train_df) < 2) return(NULL)
  
  # Time row creation
  ts_data <- ts(train_df$Order_Demand, frequency = period)
  
  # ARIMA fit
  fit <- tryCatch(
    arima(ts_data, order = order, seasonal = list(order = seasonal, period = period)),
    error = function(e) NULL
  )
  if(is.null(fit)) return(NULL)
  
  # Forecast for n weeks
  fc <- predict(fit, n.ahead = h)
  
  # Forecast-Tibble
  tibble(
    Date = last_weeks + 1, # Shift due to first prediction date being 30.10.2016 instead of 31.10.2026
    Lo = pmax(fc$pred - z * fc$se, 0),           # Non-negative lower bound
    Hi = pmax(fc$pred + z * fc$se, 0),           # Non-negative upper bound
    Forecast = pmax(as.numeric(fc$pred), 0)      # Non-negative Forecast
  )
}

# --- Generate forecasts for all groups ---
forecasts <- results %>%
  filter(!is.na(p_value)) %>%
  mutate(
    forecast = pmap(
      select(., any_of(c("Warehouse", "Product_Category", "model_result"))),
      function(...){
        args <- list(...)
        wh <- if("Warehouse" %in% names(args)) args$Warehouse else NULL
        pc <- if("Product_Category" %in% names(args)) args$Product_Category else NULL
        mr <- args$model_result
        
        # refit_and_forecast on weekly base
        refit_and_forecast(
          df = data_wh_pc, 
          warehouse = wh, 
          product_category = pc,
          order = mr$order, 
          seasonal = mr$seasonal, 
          h = 4,                   # Prediction for 4 weeks
          period = 13,              # Seasonal frequency (quarterly and optional)
          level = 0.2
        )
      }
    )
  ) %>%
  select(any_of(c("Warehouse", "Product_Category", "forecast"))) %>%
  unnest(forecast)

# --- Aggregate test data to match grouping level dynamically ---
group_cols <- c()
if(group_warehouse) group_cols <- c(group_cols, "Warehouse")
if(group_product_category) group_cols <- c(group_cols, "Product_Category")

# Aggregation of test_data based on split
agg_test_data <- test_data %>%
  group_by(across(all_of(c(group_cols, "Date")))) %>%
  summarise(Order_Demand = sum(Order_Demand, na.rm = TRUE),
            .groups = "drop")

# Filling dates in case of empty spaces in test_data
all_dates <- seq(min(test_data$Date), max(test_data$Date), by = "week")

# --- Full grid of evaluation data
test_eval <- agg_test_data %>%
  tidyr::complete(
    !!!rlang::syms(group_cols),  # completing dynamically through the grouped data
    Date = all_dates,
    fill = list(Order_Demand = 0)
  ) %>%
  arrange(across(all_of(c(group_cols, "Date"))))

# Grouping to Evaluation data (forecasts joined with test_eval)
evaluation <- forecasts %>%
  left_join(test_eval, by = c(group_cols, "Date")) %>%
  mutate(
    error = Order_Demand - Forecast,
    abs_error = abs(error),
    sq_error = error^2
  )

# Metrics per row (RMSE, MAE, MAPE)
metrics <- evaluation %>%
  group_by(across(all_of(group_cols))) %>%
  summarise(
    RMSE = sqrt(mean(sq_error, na.rm = TRUE)),
    MAE  = mean(abs_error, na.rm = TRUE),
    MAPE = mean(abs_error / Order_Demand, na.rm = TRUE) * 100,
    .groups = "drop"
  )

# Only for case of grouping on warehouse level
library(ggplot2)
library(dplyr)

# Plotting using data in evaluation 
plot_data <- evaluation %>%
  pivot_longer(
    cols = c(Order_Demand, Forecast),
    names_to = "Type",
    values_to = "Value"
  ) %>%
  mutate(Type = recode(Type,
                       Order_Demand = "Actual",
                       Forecast = "Forecast"))

# Plot with all four warehouses
ggplot(plot_data, aes(x = Date, y = Value, color = Type)) +
  geom_line(size = 1) +
  facet_wrap(~ Warehouse, scales = "free_y") +  # 1 Plot pro Warehouse
  labs(
    title = "Order Demand vs Forecast per Warehouse",
    x = "Week Start Date",
    y = "Order Demand"
  ) +
  theme_minimal() +
  scale_color_manual(values = c("Actual" = "black", "Forecast" = "blue"))