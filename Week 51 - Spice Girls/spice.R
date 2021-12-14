# Load libraries
library(tidyverse)
library(ggplot2)
library(ggtext)
library(fmsb)

# Read data into R
studio_album_tracks <- readr::read_csv("https://github.com/jacquietran/spice_girls_data/raw/main/data/studio_album_tracks.csv")
lyrics <- readr::read_csv("https://github.com/jacquietran/spice_girls_data/raw/main/data/lyrics.csv")

#data wrangling
#line totals for songs
lines_total <- lyrics %>%
  group_by(song_name) %>%
  summarise(n = max(line_number))

#disaggregate lines by artist
artist_total <- lyrics %>% group_by(song_name) %>%
  summarise(All = sum(grepl("All", section_artist)),
            Baby = sum(grepl("Baby", section_artist)),
            Ginger = sum(grepl("Ginger", section_artist)),
            Posh = sum(grepl("Posh", section_artist)),
            Scary = sum(grepl("Scary", section_artist)),
            Sporty = sum(grepl("Sporty", section_artist))
            )

artist_total <- cbind(artist_total, lines_total[2])

#calculate proportions
dt <- artist_total %>% mutate(
  All = round(All/n, 2)*100,
  Baby = round(Baby/n, 2)*100,
  Ginger = round(Ginger/n, 2)*100,
  Posh = round(Posh/n, 2)*100,
  Scary = round(Scary/n, 2)*100,
  Sporty = round(Sporty/n, 2)*100
) %>% select(1:7)

#clean names
dt$song_name[19] <- "Say You'll Be There"
dt$song_name[7] <- "If U Can't Dance"

#insert max min rows for radarchart
d <- data.frame(song_name = c("Max", "Min"),
                All = c(100, 0), Baby = c(100, 0), Ginger = c(100, 0),
                Posh = c(100, 0), Scary = c(100, 0), Sporty = c(100, 0))
dt <- rbind(dt, d)

#select songs
df <- dt[c(28, 1, 21, 19, 26, 13, 18, 3, 31, 32, 33), ]

#create color palette
cPalette <- c("#FCC916", "#2671b9", "#76d2ea", "#ee8301", "#f9dce1", "#664596",
              "#85da73", "#b8c1c0", "#522D11")

#plot
par(mfrow = c(3, 3))
for(i in 1:9) {
  radarchart(df[c(10, 11, i), 2:7],
             axistype = 1,
             pfcol = alpha(cPalette[i], 0.7),
             caxislabels = c(0, 25, 50, 75, 100),
             axislabcol = "grey",
             vlcex = 1.3,
             title = df$song_name[i])
}
par(mar = rep(2, 4))
