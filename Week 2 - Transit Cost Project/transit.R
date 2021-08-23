library(tidyverse)

# import data
tuesdata <- tidytuesdayR::tt_load('2021-01-05')
transit_cost <- tuesdata$transit_cost

# filter data for U.S. and arrange lines into preferred order
transit_cost_US <- transit_cost %>% filter(country == "US")
transit_cost_US$line <- factor(transit_cost_US$line, levels = c("Green Line Extension", "Purple Phase 1", "Purple Phase 2", "Purple Phase 3", "Regional Connector", "7 extension", "Second Avenue Phase 1", "Second Avenue Phase 2", "East Side Access", "Gateway", "Central Subway", "BART", "U-Link"))

# plot horizontal bar graph of cost per km for each line in U.S.
ggplot(data = transit_cost_US, aes(x = line, y = cost_km_millions)) + geom_col(aes(fill = city)) + scale_y_continuous(expand = c(0, 0)) + labs(title="Cost of U.S. Transit Lines", x = "Line", y = "Cost (USD) per km") + guides(fill = guide_legend(title = "City")) + coord_flip() + theme_classic() + theme(plot.margin = margin(0.5, 0.5, 0.5, 0.5, "cm"))

# plot line range graphs of dates and costs of U.S. transit lines
ggplot(data = transit_cost_US, aes(x = line, colour = city, size = cost_km_millions)) + geom_linerange(aes(ymin = start_year, ymax = end_year)) + labs(title = "Costs and Dates of Select U.S. Transit Lines", x = "Line", y = "Date") + guides(colour = guide_legend(title = "City"), size = guide_legend(title = "Cost (USD, millions) per KM")) + coord_flip() + theme_classic() + theme(plot.margin = margin(0.5, 0.5, 0.5, 0.5, "cm"), plot.title = element_text(hjust = 0.5))
