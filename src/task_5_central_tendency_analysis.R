# Load necessary libraries
library(ggplot2)
library(dplyr)

# Load the dataset
data <- read.csv("data/Prestige_New.csv")




# Central Tendency Analysis and Bell Curve Plotting for Prestige, Education, and Income

# Clean the data by removing NA values for prestige, education, and income
data_subset <- na.omit(data[, c("prestige", "education", "income")])

# Calculate central tendency for each variable (mean, median, and mode)
calculate_central_tendency <- function(variable_data) {
  mean_value <- mean(variable_data)
  median_value <- median(variable_data)
  mode_value <- as.numeric(names(which.max(table(variable_data))))  # Mode calculation
  return(c(mean = mean_value, median = median_value, mode = mode_value))
}

# Apply central tendency calculation to each variable
central_tendency_prestige <- calculate_central_tendency(data_subset$prestige)
central_tendency_education <- calculate_central_tendency(data_subset$education)
central_tendency_income <- calculate_central_tendency(data_subset$income)





# Store the results in a table
results_table <- data.frame(
  Variable = c("Prestige", "Education", "Income"),
  Mean = c(central_tendency_prestige["mean"], central_tendency_education["mean"], central_tendency_income["mean"]),
  Median = c(central_tendency_prestige["median"], central_tendency_education["median"], central_tendency_income["median"]),
  Mode = c(central_tendency_prestige["mode"], central_tendency_education["mode"], central_tendency_income["mode"])
)

write.csv(results_table, "result/task_5_central_tendency_statistics.csv", row.names = FALSE)




# Function for plotting Bell Curve with Histogram
plot_bell_curve <- function(data, column_name, title, x_label, y_label, file_name) {
  data_subset <- na.omit(data[[column_name]])
  mode_value <- as.numeric(names(which.max(table(data_subset))))
  
  plot <- ggplot(data.frame(x = data_subset), aes(x)) +
    geom_histogram(aes(y = ..density..), bins = 30, color = "black", fill = "lightblue", alpha = 0.5) +
    geom_density(color = "red", alpha = 0.5) +
    geom_vline(aes(xintercept = mean(data_subset)), color = "green", linetype = "dashed", size = 1) +
    geom_vline(aes(xintercept = median(data_subset)), color = "blue", linetype = "dashed", size = 1) +
    geom_vline(aes(xintercept = mode_value), color = "purple", linetype = "dashed", size = 1) +
    labs(
      title = title,
      x = x_label,
      y = y_label
    ) +
    theme_minimal()
  
  ggsave(file_name, plot = plot)
  return(plot)
}




# Generate and save Bell Curve plots for Prestige, Education, and Income
plot_bell_curve(data, "prestige", "Bell Curve for Prestige", "Prestige", "Density", "result/task_5_bell_curve_prestige.png")
plot_bell_curve(data, "education", "Bell Curve for Education", "Education", "Density", "result/task_5_bell_curve_education.png")
plot_bell_curve(data, "income", "Bell Curve for Income", "Income", "Density", "result/task_5_bell_curve_income.png")

