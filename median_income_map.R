library(socviz)
library(tidyverse)
library(ggthemes)
View(county_data)
county_full <- county_full %>% 
  mutate(log_median_income = log(median_income))

county_full <- left_join(county_map, county_data, by = "id")
p <- ggplot(data = county_full,
            mapping = aes(x = long, y = lat, fill = log_median_income, 
                          group = group))
p1 <- p + geom_polygon(color = "gray90", size = 0.05) + coord_equal()

p1 + labs(fill = "US Median Income (in log)") +
  theme_map() + theme(legend.position = "top") +
  scale_fill_gradient2( #  mapping poverty rate to color level
    low = '#fff2ea', 
    mid = '#ffa970', 
    high = '#A63603', 
    na.value = "grey90",
    midpoint = quantile(
      county_full$log_median_income, .5, na.rm = T))
