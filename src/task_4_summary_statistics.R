# Task 4
# Load necessary libraries
library(dplyr)

# Load the dataset
data <- read.csv("data/Prestige_New.csv")

# Task 4: Calculate Summary Statistics for Prestige, Education, and Income

# Clean the data: Remove any rows with NA values in relevant columns
data_subset <- na.omit(data[, c("prestige", "education", "income")])

# Calculate summary statistics for prestige, education, and income
summary_prestige <- summary(data_subset$prestige)
summary_education <- summary(data_subset$education)
summary_income <- summary(data_subset$income)

# Create a results table with summary statistics
results_table <- data.frame(
  Variable = c("Prestige", "Education", "Income"),
  Min = c(summary_prestige[1], summary_education[1], summary_income[1]),
  Q1 = c(summary_prestige[2], summary_education[2], summary_income[2]),
  Median = c(summary_prestige[3], summary_education[3], summary_income[3]),
  Mean = c(summary_prestige[4], summary_education[4], summary_income[4]),
  Q3 = c(summary_prestige[5], summary_education[5], summary_income[5]),
  Max = c(summary_prestige[6], summary_education[6], summary_income[6]),
  IQR = c(summary_prestige[5] - summary_prestige[2], 
          summary_education[5] - summary_education[2], 
          summary_income[5] - summary_income[2])
)


# Save the results table in the 'results' directory
write.csv(results_table, "result/task_4_summary_statistics.csv", row.names = FALSE)

# Print the results table for verification
print(results_table)
