# Library required for grouping of data
library(dplyr)
# Reading in of raw data
data <- read.csv("./Data/Retail_Dataset_Mendeley.csv")

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

# --- Data Description Analysis ---
# 1. General Structure Check
str(data)
summary(data)

# 2. Count of Unique Entities
entity_counts <- data %>%
  summarise(
    Unique_Warehouses = n_distinct(Warehouse),
    Unique_Categories = n_distinct(Product_Category),
    Total_Observations = n(),
    Start_Date = min(Date),
    End_Date = max(Date)
  )
print(entity_counts)

# 3. Descriptive Statistics for Key Variables
library(psych) # Useful for descriptive stats
desc_stats <- describe(data[, c("Order_Demand", "Petrol_price")])
print(desc_stats)

# 4. Frequency of Categorical Variables
table(data$Warehouse)
prop.table(table(data$Promo)) # Percentage of days with promotions

# --- Testing Hypotheses ---

# We need the ggplot2 library for professional charts
library(ggplot2)

# 1. Testing H1 (Promotions): Boxplot of Demand by Promo status
# We use a log scale because demand has huge outliers (some days are massive)
ggplot(data, aes(x = Promo, y = Order_Demand, fill = Promo)) +
  geom_boxplot() +
  scale_y_log10() + 
  labs(title = "Hypothesis 1: Impact of Promotions on Demand",
       subtitle = "Comparing Demand on Promo vs. Non-Promo days",
       y = "Order Demand (Log Scale)", x = "Promotion Active") +
  theme_minimal()

# 2. Testing H2 (Petrol Price): Correlation Test
# We calculate the correlation coefficient (r)
# A negative number means as price goes UP, demand goes DOWN.
cor_test <- cor.test(data$Petrol_price, data$Order_Demand)
print(cor_test)

# Visualize Petrol Price vs Demand
ggplot(data, aes(x = Petrol_price, y = Order_Demand)) +
  geom_point(alpha = 0.1, color = "blue") +
  geom_smooth(method = "lm", color = "red") + # Adds a trend line
  labs(title = "Hypothesis 2: Petrol Price vs. Demand",
       subtitle = paste("Correlation Coefficient:", round(cor_test$estimate, 4)),
       x = "Petrol Price", y = "Order Demand") +
  theme_minimal()

# 3. Testing H3 (Warehouse Variation): Mean Demand per Warehouse
warehouse_summary <- data %>%
  group_by(Warehouse) %>%
  summarise(Mean_Demand = mean(Order_Demand),
            Total_Events = n())

print(warehouse_summary)

ggplot(warehouse_summary, aes(x = reorder(Warehouse, -Mean_Demand), y = Mean_Demand, fill = Warehouse)) +
  geom_bar(stat = "identity") +
  labs(title = "Hypothesis 3: Mean Demand by Warehouse",
       x = "Warehouse ID", y = "Average Daily Demand") +
  theme_minimal()

# --- Section 4: Time Series Analysis ---

# We need the 'forecast' and 'tseries' libraries
library(forecast)
library(tseries)

# 1. Prepare the data for Time Series
# We aggregate total daily demand for the whole company
daily_ts_data <- data %>%
  group_by(Date) %>%
  summarise(Total_Demand = sum(Order_Demand)) %>%
  arrange(Date)

# 2. Create a Time Series Object (Assuming daily data, frequency = 7 for weekly patterns)
ts_obj <- ts(daily_ts_data$Total_Demand, frequency = 7)

# 3. Decompose the Time Series
# This breaks the data into Trend, Seasonal, and Random
decomp <- stl(ts_obj, s.window = "periodic")

# 4. Plot the Decomposition
plot(decomp, main = "Time Series Decomposition of Retail Demand", 
     col = "darkblue", lwd = 2)

# 5. Testing for Stationarity (ADF Test)
# This is a standard "Financial Data" test. 
# A p-value < 0.05 means the data is "stationary" (good for forecasting).
adf_test <- adf.test(ts_obj)
print(adf_test)

# --- Section 5: Final Forecasting Model ---

# 1. Fit an ARIMA model automatically
# auto.arima finds the best mathematical parameters for your specific data
fit_arima <- auto.arima(ts_obj)

# 2. Forecast the next 30 days
forecast_30d <- forecast(fit_arima, h = 30)

# 3. Plot the Forecast
# This will show the history (line) and the future (shaded area)
plot(forecast_30d, 
     main = "30-Day Demand Forecast", 
     xlab = "Time (Weeks)", 
     ylab = "Total Demand",
     col = "black", fcol = "red", shadecols = c("gray80", "gray90"))

# 4. View the numerical values for your report
print(forecast_30d)
