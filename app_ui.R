# This file builds the Shiny UI. It has five tabs.
# The file partners with app_server.R for inputs and outputs.
# The app.R file is the main file.
library(shiny)

# Grab the music data.
# See documentation for how to format the data
album_data <- read.csv("data/album-rankings.csv")

# List all of the bands in alphabetical order
# Used to populate the pull-down menu
all_bands <- sort(unique(album_data$Artist))

# List all of the years
# Used to populate the pull-down menu
all_years <- sort(unique(album_data$Year))

ui <- fluidPage(
    mainPanel(htmlOutput("title"),
      tabsetPanel(
        id = "tabset",
        tabPanel("Number One Albums",
                 htmlOutput("text3"),
                 sliderInput("rng", "Choose the Years", value = c(1993, 1998), min = 1993, max = 2024),
                 tableOutput("number_one_table")),
        tabPanel("Bands and Artists",
                 htmlOutput("text"),
                 selectInput("band_name", "Choose a band or artist:", all_bands),
                 actionButton("action_button", label = "Submit"),
                 htmlOutput("text2"),
                 tableOutput("album_table"),
                 textOutput("album_count"),
                 textOutput("avg_rating")),
        tabPanel("Top Albums by Year",
                 htmlOutput("text4"),
                 selectInput("year", "Choose a year:", all_years),
                 actionButton("action_button2", label = "Submit"),
                 htmlOutput("text5"),
                 tableOutput("year_table")),
        tabPanel("Vinyl",
                 htmlOutput("text6"),
                 tableOutput("missing_vinyl_table"),
                 htmlOutput("text7"),
                 tableOutput("most_vinyl_table")),
        tabPanel("Band Comparison",
                 htmlOutput("text8"),
                 selectInput("band_name_1", "First band or artist:", all_bands),
                 selectInput("band_name_2", "Second band or artist:", all_bands),
                 htmlOutput("text9"),
                 plotOutput("compare_bands")),
        tabPanel("Top Bands and Artists",
                 htmlOutput("text9"),
                 tableOutput("top_bands_table")),
      )
    )
)
