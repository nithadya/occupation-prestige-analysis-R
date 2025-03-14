# Load necessary libraries
library(ggplot2)
library(dplyr)

# Load the dataset
data <- read.csv("data/Prestige_New.csv")

# Task 6: Statistical Analysis for Prestige by Occupation Type

# Clean the data by removing NA values for prestige and type
data_subset <- na.omit(data[, c("prestige", "type")])

# Calculate the mean prestige for each occupation type
mean_prestige <- data_subset %>%
  group_by(type) %>%
  summarise(mean_prestige = mean(prestige, na.rm = TRUE))

# Bar plot for mean prestige by occupation type
ggplot(mean_prestige, aes(x = type, y = mean_prestige, fill = type)) +
  geom_bar(stat = "identity") +
  labs(
    title = "Mean Prestige by Occupation Type",
    x = "Occupation Type",
    y = "Mean Prestige"
  ) +
  theme_minimal()

# Save the bar plot in the 'results' directory
ggsave("results/mean_prestige_by_occupation_type.png")

# Perform ANOVA test to check if prestige varies significantly by occupation type
anova_result <- aov(prestige ~ type, data = data_subset)

# Extract p-value from ANOVA result
anova_p_value <- summary(anova_result)[[1]]$`Pr(>F)`[1]
cat("ANOVA p-value:", anova_p_value, "\n")

# Interpret the p-value: If the p-value is less than 0.05, reject the null hypothesis.
if (anova_p_value < 0.05) {
  conclusion <- "The difference in prestige between occupation types is statistically significant. We reject the null hypothesis."
} else {
  conclusion <- "There is no significant difference in prestige between occupation types. We fail to reject the null hypothesis."
}

# Save the p-value and conclusion in a results file
results <- data.frame(
  Test = "ANOVA",
  P_Value = anova_p_value,
  Conclusion = conclusion
)

# Save the results as a CSV file
write.csv(results, "result/task_6_anova_p_value_results.csv", row.names = FALSE)

# Boxplot for prestige by occupation type to visualize the distribution
ggplot(data_subset, aes(x = type, y = prestige, fill = type)) +
  geom_boxplot() +
  labs(
    title = "Boxplot of Prestige by Occupation Type",
    x = "Occupation Type",
    y = "Prestige"
  ) +
  theme_minimal()

# Save the boxplot in the 'results' directory
ggsave("result/task_6_boxplot_prestige_by_occupation_type.png")
