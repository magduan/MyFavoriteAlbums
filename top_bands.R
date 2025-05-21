library(dplyr)
library(DT)

# Grab the music data.
# See documentation for how to format the data
album_data <- read.csv("data/album-rankings.csv")

# List the highest ranking bands (average rating 9+) 
top_bands <- function(){
  album_data %>%
    group_by(Artist) %>%
    summarise(avg_rating = mean(Rating)) %>%
    filter(avg_rating >= 9) %>%
    arrange(desc(avg_rating))
}

