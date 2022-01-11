library(tidyverse)
library(ggplot2)
library(geojsonio)
library(broom)
library(rgeos)
library(mapproj)
library(viridis)
library(cowplot)

#import data
tuesdata <- tidytuesdayR::tt_load('2022-01-11')
colony <- tuesdata$colony

#data wrangling
colony_total_year <- colony %>% group_by(year) %>% summarize(total = sum(colony_n, na.rm = TRUE))
colony_total_season <- colony %>% group_by(months, year) %>% summarize(total = sum(colony_n, na.rm = TRUE))
colony_pct_loss_year <- colony %>% group_by(year) %>% summarize(lost_pct = mean(colony_lost_pct, na.rm = TRUE))
colony_pct_loss_season <- colony %>% group_by(months, year) %>% summarize(total = mean(colony_lost_pct, na.rm = TRUE))

colony_2020 <- colony %>% filter(year == 2020)
colony_spring <- colony_2020 %>% filter(months == "April-June") %>% select(state, colony_lost_pct)
colony_summer <- colony_2020 %>% filter(months == "July-September") %>% select(state, colony_lost_pct)
colony_fall <- colony_2020 %>% filter(months == "October-December") %>% select(state, colony_lost_pct)
colony_winter <- colony_2020 %>% filter(months == "January-March") %>% select(state, colony_lost_pct)

#hexagon map,courtesty of https://www.r-graph-gallery.com/328-hexbin-map-of-the-usa.html
spdf <- geojson_read("us_states_hexgrid.geojson",  what = "sp")
spdf@data = spdf@data %>%
  mutate(google_name = gsub(" \\(United States\\)", "", google_name))
plot(spdf)

spdf_fortified <- tidy(spdf, region = "google_name")
centers <- cbind.data.frame(data.frame(gCentroid(spdf, byid=TRUE), id=spdf@data$iso3166_2))

ggplot() +
  geom_polygon(data = spdf_fortified, aes( x = long, y = lat, group = group), fill="skyblue", color="white") +
  geom_text(data=centers, aes(x=x, y=y, label=id)) +
  theme_void() +
  coord_map()

#merge geospatial and colony data
df_spring <- spdf_fortified %>% 
  left_join(., colony_spring, by = c("id" = "state"))
df_summer <- spdf_fortified %>% 
  left_join(., colony_summer, by = c("id" = "state"))
df_fall <- spdf_fortified %>% 
  left_join(., colony_fall, by = c("id" = "state"))
df_winter <- spdf_fortified %>% 
  left_join(., colony_winter, by = c("id" = "state"))

#test initial choropleth map for spring
ggplot() +
  geom_polygon(data = df_spring, 
               aes(x = long, y = lat, group = group, fill = colony_lost_pct)) +
  geom_text(data = centers, aes(x = x, y = y, label = id)) +
  theme_void() +
  scale_fill_viridis() +
  coord_map() +
  theme(legend.position = "none")

#add full names to centers
centers <- centers %>% mutate(
  state = state.name[match(id, state.abb)]
)

#detail map for spring
centers_spring <- centers %>% 
  left_join(colony_spring, by = "state")

spring <- ggplot() +
  geom_polygon(data = subset(df_spring, !is.na(colony_lost_pct)), 
               aes(x = long, y = lat, group = group, fill = colony_lost_pct), color = "white") +
  geom_polygon(data = subset(df_spring, is.na(colony_lost_pct)), 
               aes(x = long, y = lat, group = group), fill = "dark green", color = "white") +
  geom_text(data = centers, aes(x = x, y = y, label = id), vjust = -0.6, size = 3.2, color = "white") +
  geom_text(data = centers_spring, aes(x = x, y = y, label = colony_lost_pct), 
            vjust = 1, size = 4, fontface = c("bold"), color = "white") +
  geom_text(data = subset(centers_spring, is.na(colony_lost_pct)), aes(x = x, y = y, label = "NA"), 
            vjust = 1, size = 3, color = "white") +
  theme_void() +
  scale_fill_viridis(begin = 0.6, end = 0.9, option = "A") +
  coord_map() +
  theme(legend.position = "none")

#detail map for summer
centers_summer <- centers %>% 
  left_join(colony_summer, by = "state")

summer <- ggplot() +
  geom_polygon(data = subset(df_summer, !is.na(colony_lost_pct)), 
               aes(x = long, y = lat, group = group, fill = colony_lost_pct), color = "white") +
  geom_polygon(data = subset(df_summer, is.na(colony_lost_pct)), 
               aes(x = long, y = lat, group = group), fill = "darkgoldenrod2", color = "white") +
  geom_text(data = centers, aes(x = x, y = y, label = id), vjust = -0.6, size = 3.2, color = "white") +
  geom_text(data = centers_summer, aes(x = x, y = y, label = colony_lost_pct), 
            vjust = 1, size = 4, fontface = c("bold"), color = "white") +
  geom_text(data = subset(centers_summer, is.na(colony_lost_pct)), aes(x = x, y = y, label = "NA"), 
            vjust = 1, size = 3, color = "white") +
  theme_void() +
  scale_fill_viridis(begin = 0.3, end = 0.8) +
  coord_map() +
  theme(legend.position = "none")

#detail map for fall
centers_fall <- centers %>% 
  left_join(colony_fall, by = "state")

fall <- ggplot() +
  geom_polygon(data = subset(df_fall, !is.na(colony_lost_pct)), 
               aes(x = long, y = lat, group = group, fill = colony_lost_pct), color = "white") +
  geom_polygon(data = subset(df_fall, is.na(colony_lost_pct)), 
               aes(x = long, y = lat, group = group), fill = "burlywood3", color = "white") +
  geom_text(data = centers, aes(x = x, y = y, label = id), vjust = -0.6, size = 3.2, color = "white") +
  geom_text(data = centers_fall, aes(x = x, y = y, label = colony_lost_pct), 
            vjust = 1, size = 4, fontface = c("bold"), color = "white") +
  geom_text(data = subset(centers_fall, is.na(colony_lost_pct)), aes(x = x, y = y, label = "NA"), 
            vjust = 1, size = 3, color = "white") +
  theme_void() +
  scale_fill_viridis(begin = 0.3, end = 0.7, option = "B") +
  coord_map() +
  theme(legend.position = "none")

#detail map for winter
#detail map for fall
centers_winter <- centers %>% 
  left_join(colony_winter, by = "state")

colorPalette <- mako(30, begin = 0.2, end = 0.7)

winter <- ggplot() +
  geom_polygon(data = subset(df_winter, !is.na(colony_lost_pct)), 
               aes(x = long, y = lat, group = group, fill = colony_lost_pct), color = "white") +
  geom_polygon(data = subset(df_winter, is.na(colony_lost_pct)), 
               aes(x = long, y = lat, group = group), fill = "grey", color = "white") +
  geom_text(data = centers, aes(x = x, y = y, label = id), vjust = -0.6, size = 3.2, color = "white") +
  geom_text(data = centers_winter, aes(x = x, y = y, label = colony_lost_pct), 
            vjust = 1, size = 4, fontface = c("bold"), color = "white") +
  geom_text(data = subset(centers_winter, is.na(colony_lost_pct)), aes(x = x, y = y, label = "NA"), 
            vjust = 1, size = 3, color = "white") +
  theme_void() +
  scale_fill_gradientn(colors = colorPalette) +
  coord_map() +
  theme(legend.position = "none")

#export plots
tiff("spring.tiff", height = 1000, width = 1000, units = "px", pointsize = 12, res = 300, compression = 'lzw')
spring
dev.off()

tiff("summer.tiff", height = 1000, width = 1000, units = "px", pointsize = 12, res = 300, compression = 'lzw')
summer
dev.off()

tiff("fall.tiff", height = 1000, width = 1000, units = "px", pointsize = 12, res = 300, compression = 'lzw')
fall
dev.off()

tiff("winter.tiff", height = 1000, width = 1000, units = "px", pointsize = 12, res = 300, compression = 'lzw')
winter
dev.off()
