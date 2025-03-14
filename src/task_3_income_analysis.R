
# Task 3

# Load necessary libraries
library(dplyr)

# Load the data set
data <- read.csv("data/Prestige_New.csv")

#Calculate Statistical Measures for Income

# Clean the data: Remove any rows with NA values in income column
data_subset <- na.omit(data$income)

# Calculate minimum, maximum, mean, median, and mode
min_income <- min(data_subset)
max_income <- max(data_subset)
mean_income <- mean(data_subset)
median_income <- median(data_subset)

# Mode Calculation
mode_income <- as.numeric(names(which.max(table(data_subset))))

# Create a results table
results_table <- data.frame(
  Measure = c("Minimum", "Maximum", "Mean", "Median", "Mode"),
  Value = c(min_income, max_income, mean_income, median_income, mode_income)
)

# Save the results table in the 'results' directory
write.csv(results_table, "result/task_3_income_statistics.csv", row.names = FALSE)

# Print the results table for verification
print(results_table)

