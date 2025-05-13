library(ggplot2)
library(dplyr)

# Define custom neon colors for Age groups
age_colors <- c(
  "65+ yrs" = "yellow",     # Neon Pink
  "65-79 yrs" = "green",   # Neon Purple
  "80+ yrs" = "#00BFFF"      # Neon Blue
)

# Define shapes for Indicator types
indicator_shapes <- c(
  "Hip Fractures" = 16,           # Circle
  "Injuries due to fall" = 17     # Triangle
)

# Clean and filter the data
health_data_filtered <- health_data %>%
  filter(Indicator != "Life Expectancy at 65") %>%
  mutate(Time_numeric = as.numeric(as.character(Time.Period))) %>%
  filter(Time_numeric >= 2010 & Time_numeric <= 2013)

# Plot
ggplot(health_data_filtered, aes(
  x = Time_numeric, 
  y = Value, 
  color = Age,
  shape = Indicator
)) +
  geom_point(size = 4) +
  scale_color_manual(values = age_colors) +
  scale_shape_manual(values = indicator_shapes) +
  labs(x = "Year", y = "Value", title = "Accidents per Year for Newham's Elderly (2010–2013)") +
  theme(
    panel.background = element_rect(fill = "black"),
    plot.background = element_rect(fill = "black"),
    panel.grid.major = element_line(color = "grey30", size = 0.3),
    panel.grid.minor = element_line(color = "grey20", size = 0.2),
    axis.text = element_text(color = "white"),
    axis.title = element_text(color = "white", face = "bold"),
    axis.line = element_line(color = "white", size = 1.2),
    plot.title = element_text(color = "white", face = "bold", hjust = 0.5),
    legend.title = element_blank(),
    legend.background = element_rect(fill = "black"),
    legend.text = element_text(color = "white"),
  )
