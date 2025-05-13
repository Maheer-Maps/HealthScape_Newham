# Load necessary libraries
library(ggplot2)
library(gridExtra)
library(dplyr)

# Filter data for condition 1: Life Expectancy at 65 (Average of Male and Female)
filtered_data_1 <- subset(health_data, Indicator == "Life Expectancy at 65" & 
                            `Time.Period` >= 2001 & `Time.Period` <= 2012)

# Calculate the average of Male and Female for each year
filtered_data_1_avg <- filtered_data_1 %>%
  filter(Sex %in% c("Male", "Female")) %>%
  group_by(`Time.Period`) %>%
  summarise(Value = mean(Value))

# Filter data for condition 2: Injuries due to fall, 65+ yrs (Only Person)
filtered_data_2 <- subset(health_data, Indicator == "Injuries due to fall" & 
                            Age == "65+ yrs" & Value >= 1200 & Value <= 7000 & Sex == "Persons")

# Filter data for condition 3: Injuries due to fall, 65-79 yrs (Only Person)
filtered_data_3 <- subset(health_data, Indicator == "Injuries due to fall" & 
                            Age == "65-79 yrs" & Value >= 1200 & Value <= 7000 & Sex == "Persons")

# Filter data for condition 4: Injuries due to fall, 80+ yrs (Only Person)
filtered_data_4 <- subset(health_data, Indicator == "Injuries due to fall" & 
                            Age == "80+ yrs" & Value >= 1200 & Value <= 7000 & Sex == "Persons")

# Determine the common y-axis limit based on the range of "Injuries due to fall"
y_limit <- c(1200, 7000)  # Setting the same y-axis range for all injury plots

# Plot for condition 1: Life Expectancy at 65 (Average of Male and Female)
plot1 <- ggplot(filtered_data_1_avg, aes(x = `Time.Period`, y = Value)) +
  geom_line(size = 3, color = "green") +  # green line
  labs(title = "Life Expectancy at 65 (2001-2012)", 
       x = "Year", 
       y = "Average Life Expectancy Value") +
  theme_minimal() +
  theme(
    panel.background = element_rect(fill = "white"),
    axis.line = element_line(size = 1.5, color = "green"),
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold", color = "black"),
    axis.text.x = element_text(size = 12, color = "black"),
    axis.text.y = element_text(size = 12, color = "black"),
    legend.position = "none"  # Remove legend
  ) +
  scale_x_continuous(breaks = seq(2001, 2012, by = 1))

# Plot for condition 2: Injuries due to fall, 65+ yrs (Only Person)
plot2 <- ggplot(filtered_data_2, aes(x = `Time.Period`, y = Value)) +
  geom_line(size = 3, color = "green") +  # green line
  labs(title = "Injuries Due to Fall (65+ yrs)", 
       x = "Year", 
       y = "Injury Value") +
  theme_minimal() +
  theme(
    panel.background = element_rect(fill = "black"),
    axis.line = element_line(size = 1.5, color = "green"),
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold", color = "black"),
    axis.text.x = element_text(size = 12, color = "black"),
    axis.text.y = element_text(size = 12, color = "black"),
    legend.position = "none"  # Remove legend
  ) +
  scale_x_continuous(breaks = seq(2001, 2012, by = 1)) +
  ylim(y_limit)  # Set common y-axis limit

# Plot for condition 3: Injuries due to fall, 65-79 yrs (Only Person)
plot3 <- ggplot(filtered_data_3, aes(x = `Time.Period`, y = Value)) +
  geom_line(size = 3, color = "green") +  # green line
  labs(title = "Injuries Due to Fall (65-79 yrs)", 
       x = "Year", 
       y = "Injury Value") +
  theme_minimal() +
  theme(
    panel.background = element_rect(fill = "black"),
    axis.line = element_line(size = 1.5, color = "green"),
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold", color = "black"),
    axis.text.x = element_text(size = 12, color = "black"),
    axis.text.y = element_text(size = 12, color = "black"),
    legend.position = "none"  # Remove legend
  ) +
  scale_x_continuous(breaks = seq(2001, 2012, by = 1)) +
  ylim(y_limit)  # Set common y-axis limit

# Plot for condition 4: Injuries due to fall, 80+ yrs (Only Person)
plot4 <- ggplot(filtered_data_4, aes(x = `Time.Period`, y = Value)) +
  geom_line(size = 3, color = "green") +  # green line
  labs(title = "Injuries Due to Fall (80+ yrs)", 
       x = "Year", 
       y = "Injury Value") +
  theme_minimal() +
  theme(
    panel.background = element_rect(fill = "black"),
    axis.line = element_line(size = 1.5, color = "green"),
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold", color = "black"),
    axis.text.x = element_text(size = 12, color = "black"),
    axis.text.y = element_text(size = 12, color = "black"),
    legend.position = "none"  # Remove legend
  ) +
  scale_x_continuous(breaks = seq(2001, 2012, by = 1)) +
  ylim(y_limit)  # Set common y-axis limit

# Arrange the plots in a 2x2 matrix
grid.arrange(plot1, plot2, plot3, plot4, ncol = 2, nrow = 2)
