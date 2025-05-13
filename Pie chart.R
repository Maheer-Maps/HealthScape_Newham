library(plotly)

# Read the CSV
data <- read.csv("speaker_words_by_conversation.csv")

# Prepare hover labels
data$Percentage <- round((data$Conv1 / sum(data$Conv1)) * 100, 1)
data$Label <- paste0(data$Speaker, ": ", data$Conv1, " words (", data$Percentage, "%)")

# Create the pie chart
fig <- plot_ly(
  data,
  labels = ~Speaker,
  values = ~Conv1,
  type = 'pie',
  textinfo = 'none',                 # No text on the segments
  hoverinfo = 'text',
  text = ~Label,
  marker = list(
    line = list(color = 'rgba(0,0,0,0)', width = 1)  # Transparent outline by default
  ),
  sort = FALSE,
  direction = "clockwise",  # To make sure it's clockwise
  rotation = 90,  # Start the pie chart from top (12 o'clock position)
  height = 400,  # Move height here
  width = 650  # Move width here
) %>%
  layout(
    title = list(
      text = "Words Spoken per Sandringham Primary School<br>Football Team (Game 3)",
      x = 0.1,  # Move title to the left (shifts graph to the right)
      font = list(color = 'white')  # Title text color to white
    ),
    showlegend = FALSE,
    margin = list(l = 40, r = 40, b = 0, t = 80),  # More top margin to avoid cutoff
    polar = list(
      angularaxis = list(
        direction = "clockwise",
        rotation = 90
      ),
      radialaxis = list(
        visible = FALSE
      )
    ),
    shapes = list(
      list(
        type = 'path',
        path = 'M 0,0 L 1,0 A 1,1 0 0,1 0,1 Z',  # Path to make it a semi-circle
        fillcolor = 'rgba(255, 255, 255, 0)',
        line = list(width = 0)
      )
    ),
    plot_bgcolor = 'black',  # Background color of the plot area
    paper_bgcolor = 'black',  # Background color of the entire figure
    font = list(color = 'white'),  # Text color for all labels and text
    hoverlabel = list(
      bgcolor = 'black',  # Hover label background
      font = list(color = 'white')  # Hover label text color
    )
  )

fig
