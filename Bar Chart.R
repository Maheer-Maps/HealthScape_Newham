# Load necessary libraries
library(ggplot2)
library(tidyr)
library(dplyr)

# Load the CSV file
data <- read.csv("speaker_words_by_conversation.csv", stringsAsFactors = FALSE)

# Rename columns if needed (ensure correct column names)
colnames(data)[2:4] <- c("Conv21", "Conv2", "Conv3")

# Reshape the data to long format
data_long <- pivot_longer(data, cols = c("Conv21", "Conv2", "Conv3"),
                          names_to = "Conversation", values_to = "Words")

# Define Fill column: only Conv21 gets unique colors; others are black
data_long <- data_long %>%
  mutate(
    Fill = case_when(
      Conversation == "Conv21" ~ Speaker,
      TRUE ~ "Other"
    ),
    Width = ifelse(Conversation == "Conv21", 0.8, 0.3)
  )

# Create manual color scale: black for Conv2 and Conv3, distinct colors for Conv21
unique_speakers <- unique(data$Speaker)
color_palette <- scales::hue_pal()(length(unique_speakers))
names(color_palette) <- unique_speakers
color_palette["Other"] <- "black"

# Plot the graph
ggplot(data_long, aes(x = Speaker, y = Words, fill = Fill, width = Width)) +
  geom_col(position = position_dodge2(preserve = "single")) +
  coord_flip() +
  scale_fill_manual(values = color_palette) +
  theme_minimal(base_family = "sans") +
  theme(
    panel.background = element_rect(fill = "black", color = NA),
    plot.background = element_rect(fill = "black", color = NA),
    legend.position = "none",
    axis.text = element_text(color = "white"),
    axis.title = element_text(color = "white", size = 12),
    axis.text.y = element_blank(),        # Hide y-axis speaker names
    axis.ticks.y = element_blank(),
    plot.title = element_text(hjust = 0.5, color = "white", size = 14, face = "bold")
  ) +
  xlab("Football Team Player") +
  ylab("Number of words Spoken per Game") +
  ggtitle("Words spoken by Sandringham Primary School Year 6 Football Team (Game 3) 24/25 Season")

# Save the last plot as a PNG
ggsave("words_spoken_football_team.png", width = 12, height = 8, dpi = 300, bg = "black")
