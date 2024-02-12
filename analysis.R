# Load libraries
library(tidyverse)
library(gtrendsR)

# Get data
trends <- gtrends(keyword = c("data scientist",  "data analyst", "data engineer", 
                              "analytics engineer", "business intelligence"),
                  geo = "DE")

iot <- trends$interest_over_time
queries <- trends$related_queries
ibr <- trends$interest_by_region

# Wrangling
iot$date <- as.Date(iot$date)
queries$subject <- as.numeric(queries$subject)

# Analysis

# Are data scientists still cooler compared to data analysts?
iot %>% 
  filter(keyword %in% c('data scientist', 'data analyst')) %>% 
  ggplot(aes(date, hits, color = keyword)) +
  geom_line()
  

# Data analyst searches have increased over data scientist searches over the
# past year, 2023.

queries %>%
  filter(related_queries == "top") %>%
  top_n(20, value) %>% 
  ggplot(aes(x = reorder(value, subject), y = subject, fill = keyword)) + 
  geom_bar(stat = 'identity') + 
  coord_flip() + 
  labs(title = "Most searched queries")

# Is BI dead?
iot %>% 
  filter(keyword %in% c('business intelligence')) %>% 
  ggplot(aes(date, hits, color = keyword)) +
  geom_line()

# There is a decreasing interest.

# Is Berlin the data search hub?
ibr %>%
  arrange(desc(hits)) %>%
  ggplot(aes(hits, location, fill = keyword)) +
  geom_bar(stat = "identity") +
  labs(title = "Hits by Location", x = "Hits", y = "Location")
