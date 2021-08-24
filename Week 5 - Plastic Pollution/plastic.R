#libraries
library(tidyverse)
library(RColorBrewer)
library(ggsci)

#import data
tuesdata <- tidytuesdayR::tt_load('2021-01-26')
plastics <- tuesdata$plastics

#wrangling and inspection
plastics %>% filter(year == "2020") %>% group_by(parent_company) %>% summarise(sum = sum(grand_total), n = n()) %>% arrange(desc(sum)) %>% slice(4:n())

#obtain list of top companies
company_list <- plastics %>% filter(year == "2020") %>% group_by(parent_company) %>% summarise(sum = sum(grand_total), n = n()) %>% arrange(desc(sum)) %>% slice(4:13) %>% pull(parent_company)

#limit data set
plastics_top <- plastics %>% select(country, parent_company, grand_total) %>% filter(parent_company %in% company_list) %>% filter(country != "EMPTY")

#re-record long country names
plastics_top$country <- recode(plastics_top$country, "United Kingdom of Great Britain & Northern Ireland" = "U.K.", "United States of America" = "U.S.", "NIGERIA" = "Nigeria")

#create word wrap on parent company names
plastics_top$parent_company <- str_wrap(plastics_top$parent_company, width = 30)

#scale down the grand_totals for each parent_company because the range is too large
plastics_top$rt_grand_total = "^"(plastics_top$grand_total, 1/5)

#aggregate data
agg.plastics_top <- plastics_top %>% group_by(country) %>% summarise(sum = sum(grand_total), rt_sum = sum(rt_grand_total))

#calculate angle, alignment, and flips for labels
number_of_bar = nrow(agg.plastics_top)
vect = seq(1, nrow(agg.plastics_top))
angle = 90 - 360 * (vect - 0.5) / number_of_bar
agg.plastics_top$hjust <- ifelse(angle < -90, 1.05, -0.05)
agg.plastics_top$angle <- ifelse(angle < -90, angle + 180, angle)

#plot circular bar chart
ggplot(data = plastics_top, aes(fill = parent_company, x = country, y = rt_grand_total)) + 
  
  geom_bar(stat = "identity") + 
  
  geom_text(data = agg.plastics_top, aes(x = country, y = rt_sum, label = paste(country, sum, sep = ": "), hjust = hjust, angle = angle), size = 2.5, inherit.aes = FALSE) +
  
  ylim(-15, 40) +
  coord_polar(start = 0) + 
  
  theme_minimal() +
  theme(
  axis.text = element_blank(),
  axis.title = element_blank(), 
  panel.grid = element_blank(), 
  plot.title = element_text(color = "blue", vjust = -50),
  plot.subtitle = element_text(vjust = -60),
  legend.position = c(0.9, 0.5),
  plot.margin = unit(rep(0,4), "cm")) +
  
  labs(title = "Top Plastics Found in St. John's by Company (Global)", 
       subtitle = "This visualization is part of the TidyTuesday challenge. This week depicts the 'Break Free from \nPastic Brand Audit' that took place at St. John's. I chose to focus on the top 10 parent companies \nof the plastic goods found throughout St. John's") +
  
  guides(fill = guide_legend(title = "Parent Company"))
