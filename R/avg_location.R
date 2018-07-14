library(dplyr)
library(ggplot2)

location_file <- 'data/locations.tsv'
locations <- read.delim(location_file)

locations <- locations %>% mutate(TotalMonths = Years*12 + Months)

remove_outliers <- TRUE
cutoff <- 12
if (remove_outliers) {
locations <- locations %>% filter(TotalMonths > cutoff)
}

total_months <- locations %>% select(TotalMonths) %>% sum()

locations <- locations %>%
  mutate(Weighted_Lat = Lat * TotalMonths / total_months,
         Weighted_Long = Long * TotalMonths / total_months)

avg_lat <- locations %>% select(Weighted_Lat) %>% sum()
avg_long <- locations %>% select(Weighted_Long) %>% sum()
avg_location <- c(avg_lat,avg_long)

locations %>% 
  select(Lat,Long,TotalMonths) %>%
  mutate(TimePercent = TotalMonths/total_months) %>%
  ggplot(aes(x = Long, y = Lat)) + 
  geom_point(aes(size = TimePercent)) + 
  geom_point(aes(x = avg_long, y = avg_lat), color = 'red', shape = 8, size = 10) + 
  theme_minimal() + 
  theme(legend.position="none") + 
  xlab('Longitude') + 
  ylab('Latitude')

