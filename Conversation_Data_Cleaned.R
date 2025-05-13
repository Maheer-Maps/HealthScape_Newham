library(dplyr)
library(tidyr)

# Read the file
lines <- readLines("BNCSplitWordsCorpus.txt")

# Parameters
chunk_size <- 30000  # Lines per conversation
n_chunks <- ceiling(length(lines) / chunk_size)
n_speakers <- 11     # Total speakers (Person1 to Person11)

# Initialize a list to store results per conversation
results <- list()

for (i in 1:n_chunks) {
  # Get lines for current conversation
  start <- (i - 1) * chunk_size + 1
  end <- min(i * chunk_size, length(lines))
  chunk <- lines[start:end]
  
  # Assign speakers cyclically (1-11, then repeat)
  speaker_ids <- ((seq_along(chunk) - 1) %% n_speakers) + 1
  speakers <- paste0("Person_", speaker_ids)
  
  # Extract dialogues (remove speaker labels if needed)
  dialogues <- sub(".*?:", "", chunk)  # Assumes format "Person1: dialogue"
  
  # Count words per line
  word_counts <- sapply(dialogues, function(x) {
    words <- strsplit(trimws(x), "\\s+")[[1]]
    length(words[words != ""])
  })
  
  # Sum words by speaker for this conversation
  conv_df <- data.frame(Speaker = paste0("Person_", 1:n_speakers)) %>%
    left_join(
      data.frame(Speaker = speakers, Words = word_counts) %>%
        group_by(Speaker) %>%
        summarise(Total_Words = sum(Words)),
      by = "Speaker"
    ) %>%
    mutate(Total_Words = ifelse(is.na(Total_Words), 0, Total_Words)) %>%
    mutate(Conversation = paste0("Conv", i))
  
  results[[i]] <- conv_df
}

# Combine into a wide table (speakers as rows, conversations as columns)
final_table <- bind_rows(results) %>%
  pivot_wider(
    names_from = Conversation,
    values_from = Total_Words,
    values_fill = 0
  )

# Save and print
write.csv(final_table, "speaker_words_by_conversation.csv", row.names = FALSE)
print(final_table)

# Change "Person_" to "Player_" in Speaker column
final_table$Speaker <- gsub("Person_", "Player ", final_table$Speaker)

# Save the updated CSV
write.csv(final_table, "speaker_words_by_conversation.csv", row.names = FALSE)

###BAR CHART

# Create bar plot for Conv1 (filtering range 17926-20000)
library(ggplot2)

# Use final_table directly (Player 1 to Player 11)
plot_data <- final_table %>%
  select(Speaker, Conv1)  # Keep only Speaker and Conv1 columns

# Create bar plot
ggplot(plot_data, aes(x = Speaker, y = Conv1)) +
  geom_bar(stat = "identity", fill = "steelblue", width = 0.7) +
  labs(
    title = "Conversation data from Game 1\n(Shaftesbury Primary School Vs Elmhurst Primary School)",
    x = "Players from Shaftesbury Primary School year 6 football team",
    y = "Number of words spoken in the game"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
    axis.text.y = element_text(size = 10),
    plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
    axis.title.x = element_text(size = 12, margin = margin(t = 10)),
    axis.title.y = element_text(size = 12, margin = margin(r = 10))
  ) +
  scale_y_continuous(expand = c(0, 0))  # Remove gap below bars

# Save the plot
ggsave("game1_player_words.png", width = 8, height = 6, dpi = 300)

### VERITCAL BAR GRAPH

# Ensure Player order is correct (1 to 11)
plot_data <- final_table %>%
  mutate(Speaker = factor(Speaker, levels = paste0("Player ", 1:11))) %>%
  arrange(Speaker) %>%
  select(Speaker, Conv1)

# Create horizontal bar plot
ggplot(plot_data, aes(x = Conv1, y = reorder(Speaker, 1:11))) +
  geom_bar(stat = "identity", fill = "steelblue", width = 0.7) +
  labs(
    title = "Conversation data from Game 1\n(Shaftesbury Primary School Vs Elmhurst Primary School)",
    x = "Number of words spoken in the game",
    y = "Players from Shaftesbury Primary School year 6 football team"
  ) +
  theme_minimal() +
  theme(
    axis.text.y = element_text(size = 10, hjust = 1),
    axis.text.x = element_text(size = 10),
    plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
    axis.title.x = element_text(size = 12, margin = margin(t = 10)),
    axis.title.y = element_text(size = 12, margin = margin(r = 10)),
    panel.grid.major.y = element_blank()  # Remove horizontal grid lines
  ) +
  scale_x_continuous(expand = c(0, 0))  # Remove gap at start of bars

# Save the plot
ggsave("game1_player_words_horizontal.png", width = 10, height = 6, dpi = 300)