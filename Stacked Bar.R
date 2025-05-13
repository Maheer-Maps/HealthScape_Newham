# Load necessary libraries
library(ggplot2)
library(tidyr)
library(dplyr)
library(RColorBrewer)
library(tidyverse)

# Load the data
data <- read.csv("speaker_words_by_conversation.csv")

# Add a player ID column if not present
data$Player <- factor(1:nrow(data))

# Reshape data and prepare labels
data_long <- data %>%
  pivot_longer(cols = starts_with("Conv"),
               names_to = "Conversation",
               values_to = "Words") %>%
  mutate(
    Conversation = factor(Conversation, levels = c(paste0("Conv", 1:20), "Conv21")),  # Make Conv21 last
    LegendLabel = ifelse(Conversation == "Conv21", "Current Game", "Prior Games")
  )

# Create alternating black and white colors for Conv1 to Conv20, and parrot green for Conv21
prior_game_colors <- rep(c("black", "white"), 10)  # Alternate black and white
colors <- c(prior_game_colors, "#76c043")  # Parrot green for Conv21

# Plot
ggplot(data_long, aes(x = Words, y = Player, fill = Conversation)) +
  geom_bar(stat = "identity", position = "stack", color = "black", size = 0.2) +  # Add black outline around bars
  scale_fill_manual(
    values = colors,
    breaks = c("Conv1", "Conv2", "Conv3", "Conv4", "Conv5", "Conv6", "Conv7", "Conv8", "Conv9", 
               "Conv10", "Conv11", "Conv12", "Conv13", "Conv14", "Conv15", "Conv16", "Conv17", 
               "Conv18", "Conv19", "Conv20", "Conv21"),  # Show all in legend
    labels = c("Prior Games", "Prior Games", "Prior Games", "Prior Games", "Prior Games", 
               "Prior Games", "Prior Games", "Prior Games", "Prior Games", "Prior Games",
               "Prior Games", "Prior Games", "Prior Games", "Prior Games", "Prior Games", 
               "Prior Games", "Prior Games", "Prior Games", "Prior Games", "Prior Games", 
               "Current Game"),  # Label appropriately in legend
    guide = guide_legend(title = "Legend", title.position = "top")  # Change legend title to "Legend"
  ) +
  scale_x_continuous(
    labels = function(x) paste0(x / 1000),  # Format labels as thousands
    expand = c(0, 0),  # Remove any extra padding
    breaks = scales::pretty_breaks(n = 10)  # Automatically generate 10 breaks
  ) +
  labs(
    x = "Words Spoken per Player per Game (in Thousands)",
    y = "Player number",
    title = "Summary of Words spoken per Sandringham Primary School Year 6 Football Team (24/25 Season)"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(size = 15, face = "bold"),
    axis.title = element_text(size = 13),
    axis.line = element_line(color = "black", size = 1.2),  # Make axis lines black and bolder
    axis.ticks = element_line(color = "black", size = 1),    # Make axis ticks black and bolder
    axis.text.x = element_text(size = 11),                   # Adjust x-axis text size
    axis.text.y = element_text(size = 11),                    # Adjust y-axis text size
    legend.key = element_rect(fill = "white", color = "black"), # Make legend keys have black borders
    legend.title = element_text(size = 13) # Set legend title size
  ) +
  guides(fill = guide_legend(override.aes = list(shape = 15, size = 5))) # Square legend keys

ggsave("word_summary_stacked_plot.png", plot = plot, width = 10, height = 6, dpi = 300)  # You can adjust width, height, and dpi
