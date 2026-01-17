# Library required for grouping of data
library(dplyr)
library(tidyr)
library(purrr)

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

increment_arima_param <- function(order, seasonal, i) {
  if(i >= 1 && i <= 3) {
    order[i] <- order[i] + 1
  } else if(i >= 4 && i <= 6) {
    seasonal[i - 3] <- seasonal[i - 3] + 1
  }
  
  list(order = order, seasonal = seasonal)
}

select_best_candidate_simple <- function(ts_data, current_order, current_seasonal, lag_lb = 20, period = 7) {
  
  best_p <- -Inf
  best_fit <- NULL
  best_order <- current_order
  best_seasonal <- current_seasonal
  
  for(i in 1:6) {
    # Increment one parameter
    params <- increment_arima_param(current_order, current_seasonal, i)
    
    # Try to fit ARIMA; if it fails, skip this candidate
    fit <- tryCatch(
      arima(ts_data, order = params$order, seasonal = list(order = params$seasonal, period = period)),
      error = function(e) NULL,
      warning = function(w) {
        # convert warnings into NULL if you want to skip non-convergent fits
        NULL
      }
    )
    
    if(!is.null(fit)) {
      # Compute Ljung-Box p-value
      fitdf <- sum(params$order[c(1,3)]) + sum(params$seasonal[c(1,3)])  # p+q+P+Q
      lb <- Box.test(residuals(fit), lag = lag_lb, type = "Ljung-Box", fitdf = fitdf)
      
      # Keep best p-value
      if(lb$p.value > best_p) {
        best_p <- lb$p.value
        best_fit <- fit
        best_order <- params$order
        best_seasonal <- params$seasonal
      }
    }
    # If fit is NULL (failed), just skip to next candidate
  }
  
  return(list(fit = best_fit,
              order = best_order,
              seasonal = best_seasonal,
              p_value = best_p))
}

# Daily data, fill missing dates
build_time_series <- function(df, warehouse = NULL, product_category = NULL,
                              period = 7, p_value_threshold = 0.05, max_iter = 5,
                              lag_lb = 20) {
  
  # Filter only if arguments are provided
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
  
  if(is.null(best$fit)) {
    return(list(fit = NULL, order = NA, seasonal = NA, p_value = NA))
  }
  
  last_successful <- best  # Keep the last truly successful fit
  iter <- 1
  
  while(last_successful$p_value < p_value_threshold & iter <= max_iter) {
    
    temp <- tryCatch(
      select_best_candidate_simple(ts_data, last_successful$order, last_successful$seasonal,
                                   lag_lb = lag_lb, period = period),
      error = function(e) NULL
    )
    
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


run_arima_analysis <- function(df, 
                               group_warehouse = TRUE, 
                               group_product_category = TRUE, 
                               max_iter = 5) {
  
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
          # Extract warehouse/product if present, else NULL
          wh <- if("Warehouse" %in% names(args)) args$Warehouse else NULL
          pc <- if("Product_Category" %in% names(args)) args$Product_Category else NULL
          
          build_time_series(df, wh, pc, max_iter = max_iter)
        }
      )
    ) %>%
    mutate(
      order = map(model_result, "order"),
      seasonal = map(model_result, "seasonal"),
      p_value = map_dbl(model_result, "p_value")
    )
  
  return(results)
}

# Grouping after Warehouse and Product Category by Date
data_wh_pc <- data %>%
  group_by(Warehouse, Product_Category, Date) %>%
  summarise(
    Order_Demand = sum(Order_Demand),
    .groups = "drop"
  )

#  Train/Test Split
cutoff_date <- max(data_wh_pc$Date) - 28

train_data <- data_wh_pc %>% filter(Date <= cutoff_date)
test_data <- data_wh_pc %>% filter(Date > cutoff_date)

# ARIMA nur für Warehouse
results <- run_arima_analysis(train_data, TRUE, FALSE, max_iter = 5)