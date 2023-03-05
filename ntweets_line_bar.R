# import delimited "/Users/byeong-hakchoe/Google Drive/wp-climate-opinion/users-by-social-media-platform.csv", clear
# rename monthlyactiveusersstatistaandtnw n_users
# replace n_users = n_users/1000000000
# bys year: egen n_users_sum = sum(n_users)
# bys year: gen dup = cond(_N==1,0,_n)
# drop if dup > 1
# drop dup
# keep year n_users_sum
# drop if year > 2018
# drop if year > 2017
# format n_users_sum %6.0fc
# twoway connected n_users_sum year, xlabel(2002(1)2017, angle(45)) ytitle("Number of social media users in the globe (billion)") 
# graph export "/Users/byeong-hakchoe/Google Drive/climate_opinion/paper/n_social_media_users.png", replace
# 
# 
# 
# 
# use "/Users/byeong-hakchoe/Documents/descriptive_stat_n_tweets.dta", clear
# egen n_us_tweets = sum(n_ot_us)
# egen n_tweets = sum(n_ot_wrld)
# 
# foreach v of varlist n_ot_us n_ot_wrld n_rt_us n_rt_wrld n_lk_us n_lk_wrld{
#   replace `v' = `v'/1000
# }
# 
# gen n_rt_lk_us = n_rt_us + n_lk_us
# gen n_rt_lk_wrld = n_rt_wrld + n_lk_wrld
# 
# twoway (bar n_ot_wrld year, bargap(5) color(blue*.5) barw(0.5)) (connected n_rt_lk_wrld year)  ///
# 		 (bar n_ot_us year, color(red*.5) barw(0.5))  (connected n_rt_lk_us year) ///
# 		, xtitle("Year") ytitle("Number of tweets, retweets & likes (thousand)") yscale(titlegap(*15)) ylabel(, angle(horizontal)) ///
# 		legend( label(3 "US tweets") label(1 "Worldwide tweets") ///
# 				label(4 "Retweets/likes to US tweets") label(2 "Retweets/likes to worldwide tweets")) ylabel(, format(%6.0fc))
# graph export "/Users/byeong-hakchoe/Google Drive/climate_opinion/paper/ts_n_tweets.png", replace
# 



library(tidyverse)
library(hrbrthemes)
n_users <- read_csv('/Users/byeong-hakchoe/Google Drive/wp-climate-opinion/users-by-social-media-platform-cleaned-2023-0303.csv')

p1 <- ggplot(data = filter(n_users, year < 2018), 
       aes(x = year, y = n_users_sum)) +
  geom_line(size = 1.5, color = '#428bca') +
  geom_point(size = 2) +
  labs(x = "Year", y = "Number of Social Media Users in the World (billion)",
       caption = "Sources: Our World in Data, Statista, and TNW") +
  scale_x_continuous(breaks = seq(2003, 2017, 2)) + 
  theme_ipsum() +
  theme(axis.title.y = element_text(size = rel(2),
                                    margin = margin(t = 0, r = 20, b = 0, l = 0)),
        axis.title.x = element_text(size = rel(2),
                                    margin = margin(t = 10, r = 0, b = 0, l = 0)),
        axis.text.x = element_text(size = rel(1.75)),
        axis.text.y = element_text(size = rel(1.75)),
        plot.caption = element_text(size = rel(1.25),
                                    hjust = .5)
  ) 

ggsave('/Users/byeong-hakchoe/Google Drive/wp-climate-opinion/paper/n_social_media_users_2023.png',
       plot = p1, height = 8, width = 15, units = "in") 



n_tweets <- read_csv('/Users/byeong-hakchoe/Google Drive/wp-climate-opinion/descriptive_stat_n_tweets-cleaned-2023-0303.csv')


ggplot(n_tweets, aes(x = year)) +
  geom_col(aes(y = n_ot_wrld), fill = '#5bc0de') +
  geom_col(aes(y = n_ot_us), fill = '#d9534f') +
  geom_line(aes(y = n_rt_lk_us), color = 'maroon') +
  geom_point(aes(y = n_rt_lk_us), color = 'maroon') +
  geom_line(aes(y = n_rt_lk_wrld), color = '#428bca') +
  geom_point(aes(y = n_rt_lk_wrld), color = '#428bca') 


n_tweets_long <- n_tweets %>% 
  select(year, n_ot_us, n_ot_wrld, n_rt_lk_us, n_rt_lk_wrld) %>% 
  pivot_longer(cols =  c('n_ot_us', 'n_ot_wrld', 'n_rt_lk_us', 'n_rt_lk_wrld'), 
               names_to = "type", 
               values_to = "n")
colnames(n_tweets)

n_tweets_long1 <- n_tweets_long %>% 
  filter(type %in% c("n_ot_us", "n_ot_wrld") ) %>% 
  mutate(type = ifelse(type == "n_ot_us", "US", "Worldwide"))

n_tweets_long2 <- n_tweets_long %>% 
  filter(type %in% c("n_rt_lk_us", "n_rt_lk_wrld") ) %>% 
  mutate(type = ifelse(type == "n_rt_lk_us", "US", "Worldwide"))
  

p2 <- ggplot(mapping = aes(x = year, y = n)) +
  geom_col(n_tweets_long1,
           mapping = aes(fill = type), 
           position = 'dodge', alpha = .67) + 
  geom_line(n_tweets_long2, 
            mapping = aes(color = type),
            size = 1.5) + 
  geom_point(data = n_tweets_long2, 
             size = 2,
             color = 'black')  +
  scale_x_continuous(breaks = seq(2012, 2017, 1)) +
  scale_y_continuous(label = scales::comma) +
  scale_color_manual(values = c('maroon', '#428bca')) +
  scale_fill_manual(values = c('maroon', '#428bca')) +  
  guides(fill = guide_legend(reverse = TRUE,
                             # title.position = "top",
                             label.position = "bottom",
                             keywidth = 2,
                             nrow = 1,
                             order = 1),
         color = guide_legend(reverse = TRUE,
                             # title.position = "top",
                             label.position = "bottom",
                             keywidth = 2,
                             nrow = 1,
                             order = 2)) +
  labs(x = "Year", 
       y = "Number of Tweets or Retweets & Likes (in thousand)",
       fill = "Tweets",
       color = "Retweets and likes",
       caption = 'Source: Choe, "Social Media Campaigns, Lobbying, and Climate Change Legislation: Evidence from #climatechange/#globalwarming and Energy Lobbies" (2023)') + 
  theme_ipsum() +
  theme(axis.title.y = element_text(size = rel(2),
                                    margin = margin(t = 0, r = 20, b = 0, l = 0)),
        axis.title.x = element_text(size = rel(2),
                                    margin = margin(t = 10, r = 0, b = 0, l = 0)),
    axis.text.x = element_text(size = rel(1.75)),
    axis.text.y = element_text(size = rel(1.75)),
    legend.position = 'top',
    legend.title = element_text(size= rel(1.75),
                                face = 'bold',
                                hjust = .5),    
    legend.text = element_text(size= rel(1.5)),
    legend.spacing.x = unit(1.25, "cm"),
    plot.caption = element_text(size = rel(1.25),
                                hjust = .5)
    ) 


ggsave('/Users/byeong-hakchoe/Google Drive/wp-climate-opinion/paper/ts_n_tweets_2023.png',
       plot = p2, height = 8, width = 15, units = "in") 




ame <- read_csv('/Users/byeong-hakchoe/Google Drive/wp-climate-opinion/average_causal_function_social_media.csv')

p <- ggplot(data = filter(ame, party %in% c("dem", "gop")),
            aes(x = social_campaigns,
                y = mer, ymin = lower_ci, ymax = upper_ci,
                color = party,
                fill = party,
                group = party))

party_colors <- c("#2E74C0", "#CB454A") 

p3 <- p + geom_line() +
  geom_ribbon(alpha = 0.2, color = FALSE) +
  geom_hline(yintercept = 0, color = 'black', lty = 2) +
  scale_color_manual(values = party_colors,
                     name = "Party",
                     labels = c("DEM", "GOP")) +
  scale_fill_manual(values = party_colors,
                    name = "Party",
                    labels = c("DEM", "GOP")) +
  guides(fill = guide_legend(reverse = T,
                              # title.position = "top",
                              label.position = "bottom",
                              keywidth = 2,
                              ncol = 1),
         color = guide_legend(reverse = T,
                              # title.position = "top",
                              label.position = "bottom",
                              keywidth = 2,
                              ncol = 1)) +
  labs(x = "Social Media Campaigns (per-capita)",
       y = "Average Causal Response for the Vote to Social Media Campaigns") +
  theme_ipsum() +
  theme(axis.title.y = element_text(size = rel(2),
                                    margin = margin(t = 0, r = 20, b = 0, l = 0)),
        axis.title.x = element_text(size = rel(2),
                                    margin = margin(t = 10, r = 0, b = 0, l = 0)),
        axis.text.x = element_text(size = rel(1.75)),
        axis.text.y = element_text(size = rel(1.75)),
        legend.position = 'right',
        legend.justification = c(.4,.5),
        legend.title = element_text(size= rel(1.75),
                                    face = 'bold',
                                    hjust = .5),
        legend.text = element_text(size= rel(1.5)),
        legend.spacing.x = unit(1.25, "cm"),
        plot.caption = element_text(size = rel(1.25),
                                    hjust = .5)
  ) 
  

p3
ggsave('/Users/byeong-hakchoe/Google Drive/wp-climate-opinion/paper/acr_tweets_2023.png',
       plot = p3, height = 8, width = 15, units = "in") 


library(tidyverse)
mer <- tibble(
  party = c("DEM", "GOP"),
  mer = c(-.0100071, .0017617),
  lower_ci = c(-.0209165, -.0004726),
  upper_ci = c(.0009022, .0039959)
)


p <- ggplot(mer, aes(x = party, y = mer, 
                     ymin = lower_ci,
                     ymax = upper_ci,
                     color = party))

p3 <- p + 
  geom_errorbar( width = .15, 
                 size = 1.25 ) +
  geom_hline(yintercept = 0, color = 'black') +
  geom_point(size = 3, color = 'black') + 
  guides(color = "none") +
  scale_color_manual(values = party_colors) +
  hrbrthemes::theme_ipsum() +
  labs(x = "", y = "Effects of Social Media Campaigns") +
  theme(axis.title.y = element_text(size = rel(2),
                                    margin = margin(t = 0, r = 20, b = 0, l = 0)),
        axis.title.x = element_text(size = rel(2),
                                    margin = margin(t = 10, r = 0, b = 0, l = 0)),
        axis.text.x = element_text(size = rel(1.75)),
        axis.text.y = element_text(size = rel(1.75)),
  )

p3
ggsave('/Users/byeong-hakchoe/Google Drive/wp-climate-opinion/paper/ame_social_media_users_2023.png',
       plot = p3, height = 8, width = 15, units = "in") 
