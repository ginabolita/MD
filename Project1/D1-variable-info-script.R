# In order to install the necessary library, uncomment the next line
# install.packages("dplyr", repos = "https://cloud.r-project.org/")

library(dplyr)

# Load dataset (change working directory depending to where you store your file)
dataset <- read.csv("/home/alex/Documents/FIB/6e_quadrimestre/MD/LAB/hotel_booking_5000_rows.csv")

# a. Number of records (sample if needed)
num_records <- nrow(dataset)
if (num_records > 5000) {
  dataset <- dataset %>% sample_n(5000)
  num_records <- 5000
} else if (num_records < 2000) {
  stop("Dataset has fewer than 2000 records.")
}

# b. Number of variables
num_variables <- ncol(dataset)

# c. Number of numerical variables
# Checks wether it's numeric and columns don't have only 2 unique values
date_vars <- sapply(dataset, function(x) inherits(x, "Date") | inherits(x, "POSIXct") | inherits(x, "POSIXlt"))
numerical <- sapply(dataset, is.numeric) & sapply(dataset, function(x) length(unique(na.omit(x))) != 2) & !date_vars
num_numerical <- sum(numerical)

# d. Number of binary variables
# Checks how many columns contain only two unique values (after removing missing values)
binary <- sapply(dataset, function(x) length(unique(na.omit(x))) == 2)
num_binary <- sum(binary) 

# e. Number of qualitative (categorical) variables
qualitative <- sapply(dataset, is.factor) | sapply(dataset, is.character) | (!date_vars & sapply(dataset, is.character))
num_qualitative <- sum(qualitative)

# f. Number and % of missing data per column
missing_data_per_column <- colSums(is.na(dataset))
missing_percentage_per_column <- (missing_data_per_column / num_records) * 100
missing_info <- data.frame(Variable = names(dataset), 
                           Missing_Count = missing_data_per_column, 
                           Missing_Percentage = missing_percentage_per_column)

# g. % of missing data in the whole dataset
total_elements = num_records * num_variables # whole matrix x * y
total_missing <- sum(is.na(dataset))
total_missing_percentage <- (total_missing / total_elements) * 100

# Print results
print("Numerical variables:")
print(names(dataset[numerical]))

print("categorical variables:")
print(names(dataset[qualitative]))

print("Binary variables:")
print(names(dataset[binary]))

print("Date variables:")
print(names(dataset[date_vars]))

list(
  num_records = num_records,
  num_variables = num_variables,
  num_numerical = num_numerical,
  num_binary = num_binary,
  num_qualitative = num_qualitative,
  missing_info = missing_info,
  total_missing_percentage = total_missing_percentage
)
