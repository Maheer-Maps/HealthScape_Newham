library(tidyverse)

# Read CSV
data <- read.csv("speaker_words_by_conversation.csv")

# Reshape to long format
long_data <- pivot_longer(
  data,
  cols = starts_with("Conv"),
  names_to = "Conversation",
  values_to = "Words"
)

# Create ConvLevel and reverse factor order so Conv1 is innermost
long_data$ConvLevel <- as.numeric(gsub("Conv", "", long_data$Conversation))
long_data$ConvLevel <- factor(long_data$ConvLevel, levels = rev(1:21))  # Reversed

# Create alternating colors: red/black, with outermost (Conv21) as green
colors <- rep(c("red", "black"), length.out = 21)
colors[21] <- "green"  # OUTERMOST ring (Conv21)

# Since factor levels are reversed, we reverse the colors too
color_map <- setNames(rev(colors), levels(long_data$ConvLevel))

# Plot
ggplot(long_data, aes(x = Speaker, y = Words, fill = ConvLevel)) +
  geom_col(position = "stack", width = 1) +
  coord_polar(theta = "x") +
  scale_fill_manual(
    values = color_map,
    name = "Legend"  # Change legend title
  ) +
  labs(title = "Sundial Diagram: Words Spoken per Conversation and Speaker") +
  theme_minimal() +
  theme(
    axis.title = element_blank(),
    axis.text.y = element_blank(),
    axis.text.x = element_text(size = 8),
    axis.ticks = element_blank(),
    panel.grid = element_blank(),
    plot.title = element_text(hjust = 0.5),
    legend.title = element_text(size = 12)
  )
