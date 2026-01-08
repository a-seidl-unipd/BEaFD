# Library required for grouping of data
library(dplyr)
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