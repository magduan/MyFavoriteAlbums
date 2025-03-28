# Pull in required packages
library(dplyr)
library(ggplot2)
library(ggpubr)

# Grab the music data.
# See documentation for how to format the data
album_data <- read.csv("albums-rankings.csv")

# List the top albums for a year, in ranking order
year_albums <- function(year.var){
year_list <- select(filter(album_data[order(album_data$Ranking),], Year == year.var), Ranking, Album, Artist)
print(paste0("Top Albums of ", year.var))
print(year_list)
}
year_albums(2024)

typeof(album_data$Year)

# List all of the albums with a rating of 9 or higher
top_ratings <- function(){
high_rating <- select(filter(album_data[order(-album_data$Rating, album_data$Year),], Rating >= 9), Album, Artist, Rating, Year)
print("All Albums with a 9 or 10 Rating")
print(high_rating)
}
top_ratings()

# Find top-rated albums that the person does not own on vinyl
missing_vinyl <- function(){
  buy_vinyl <- select(filter(album_data[order(-album_data$Rating),], Rating >= 9, Vinyl ==""), Album, Artist, Rating)
  print("All albums with a 9 or 10 rating that I don't own on vinyl")
  print(buy_vinyl)
}
missing_vinyl()

# List the Number 1 album for each year in the indicate range
number_one_album <- function(var.startyear, var.endyear){
  top_albums <- select(filter(album_data, Ranking ==1, Year >= var.startyear, Year <= var.endyear), Album, Artist, Year)
  print(paste0("The Number One Album of Each Year from " , var.startyear, " and " , var.endyear))
  print(top_albums)
}
number_one_album(2000,2009)

# List all of the albums by a band, including the year of release and rating. 
# Order by year of release
# Calculate and display the number of albums for that band and the average rating
albums_by_band <- function(band.var){
  band_albums <- select(filter(album_data[order(album_data$Year),], Artist==band.var), Album, Year, Rating)
  mean_rating <-mean(band_albums$Rating)
  band_count <-count(band_albums)
  print(paste0("Albums by ", band.var))
  print(band_albums)
  print(paste0("Number of Albums Ranked: ", band_count))
  print(paste0("Average Rating:", format(round(mean_rating, 2), nsmall =2)))
}

albums_by_band("Belle and Sebastian")
albums_by_band("Oasis")
albums_by_band("The Smashing Pumpkins")
albums_by_band("Bjork")
albums_by_band("Elliott Smith")
albums_by_band("Radiohead")
albums_by_band("Mason Jennings")

# List all of the albums from a given year with a specific numerical rating
year_rating_albums <- function(year.var, rating.var){
  year_rating <- select(filter(album_data[order(album_data$Artist),], Year == year.var, Rating == rating.var), Album, Artist)
  print(paste0("All of the albums from ", year.var, " with a rating of ", rating.var))
  print(year_rating)
}
year_rating_albums(1995, 7)

# List all of the albums with a rating of 10 for a given decade
# Input should be 1990, 2000, 2010, or 2020
decade_best <- function(var.decade){
  best_of_decade <- select(filter(album_data[order(album_data$Year),], Rating == 10, substr(as.character(var.decade), 1, 3) == substr(as.character(album_data$Year), 1, 3)), Album, Artist, Year)
  print(paste0("Best Albums of the ", var.decade, "s"))
  print(best_of_decade)
}
decade_best(1990)
decade_best(2000)
decade_best(2010)
decade_best(2020)



# Find the number of albums that have been ranked for each year.
# List the years in order based on how many albums were ranked for each year.
year_most_ranked <-function(){
  most_ranked <- filter(album_data) %>% count(Year)
  most_ranked_sort <- most_ranked[order(-most_ranked$n),] 
  print("Years with the most albums ranked")
  print(most_ranked_sort)
}
year_most_ranked()

# Find the years that have the most albums rated 8 or higher
# List the years in order based on how may 8+ albums they had 
year_most_rated_8_plus <-function(){
  most_rated_8_plus <- select(filter(album_data, Rating >= 8), Year)
  most_rated_count_8_plus <- filter(most_rated_8_plus) %>% count(Year)
  most_rated_sort_8_plus <- most_rated_count_8_plus[order(-most_rated_count_8_plus$n),] 
  print("Years with the most albums rated 8 or higher")
  print(most_rated_sort_8_plus)
}
year_most_rated_8_plus()

# Find the release years where the person owns the most vinyl records
# List the years in order based on how many records for that year
year_most_vinyl <-function(){
  most_vinyl <- select(filter(album_data, Vinyl == "v"), Year)
  most_vinyl_count <- filter(most_vinyl) %>% count(Year)
  most_vinyl_sort <- most_vinyl_count[order(-most_vinyl_count$n),] 
  print("Years for which I own the most vinyl")
  print(most_vinyl_sort)
}
year_most_vinyl()


#Find all of the EPs, sort by rating
find_eps <- function(){
  eps <-select(filter(album_data[order(-album_data$Rating),], EP == "EP"), Artist, Album, Year, Rating)
  print(eps)
}
find_eps()


#Find all live albums, sort by rating
find_live <- function(){
  live <-select(filter(album_data[order(-album_data$Rating),], Live == "Live"), Artist, Album, Year, Rating)
  print(live)
}
find_live()

band_album_chart <- function(var.artist){
band_albums <- select(filter(album_data[order(album_data$Year),], Artist==var.artist), Artist, Album, Year, Rating)
  
ggplot(band_albums, aes(Year, Rating)) +
  geom_point() +
  geom_line()+
    xlab("Year")+
    ylab("Rating")+
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
    ggtitle(paste0("Album Ratings for " , band_albums$Artist))+
  scale_x_continuous(breaks=seq(1993,2024,1))+
  scale_y_continuous(breaks=seq(0,10,1))+
  expand_limits(x=c(1993,2024), y=c(0,10))
}

band_album_chart("Elliott Smith")
band_album_chart("Mason Jennings")

band_album_comparison_chart <- function(var.artist1, var.artist2){
  band1_albums <- select(filter(album_data[order(album_data$Year),], Artist==var.artist1), Artist, Album, Year, Rating)
  band2_albums <- select(filter(album_data[order(album_data$Year),], Artist==var.artist2), Artist, Album, Year, Rating)
  
  ggplot() +
    geom_line(data = band1_albums, aes(x = Year, y = Rating), color = "red") +
    geom_line(data = band2_albums, aes(x = Year, y = Rating), color = "blue") +
    geom_point(data = band1_albums, aes(x = Year, y = Rating)) +
    geom_point(data = band2_albums, aes(x = Year, y = Rating)) +
    xlab("Year")+
    ylab("Rating")+
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
    ggtitle(paste0("Album Ratings for " , band1_albums$Artist, "(Red) and ", band2_albums$Artist, "(Blue)"))+
    scale_x_continuous(breaks=seq(1993,2024,1))+
    scale_y_continuous(breaks=seq(0,10,1))+
    expand_limits(x=c(1993,2024), y=c(0,10))
}

band_album_comparison_chart("Taylor Swift", "Waxahatchee")
