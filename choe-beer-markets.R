library(tidyverse)
library(gapminder)
library(skimr)   # a better summary of data.frame
library(scales)  # scales for ggplot
library(ggthemes)  # additional ggplot themes
library(hrbrthemes) # additional ggplot themes and color pallets
library(lubridate)
library(ggridges)
library(stargazer)
library(sf)
library(fuzzyjoin)
library(ggimage)
library(png)
library(grid)

theme_set(theme_ipsum())

beer_mkt <- read.csv('https://bcdanl.github.io/data/beer_markets.csv')


# agg ---------------------------------------------------------------------

beer_mkt_agg <- beer_mkt %>% 
  group_by(market, brand) %>% 
  mutate(spent = sum(spent, na.rm = T),
         beer_floz = sum(beer_floz, na.rm = T),
         price_per_floz = mean(beer_floz, na.rm = T)) %>% 
  ungroup() %>% 
  group_by(market) %>% 
  mutate(n_avg = n() / n_unique(hh) ) %>% 
  select(market, brand, 
         spent, beer_floz, price_per_floz, 
         n_avg) %>% 
  distinct() %>% 
  arrange(market, brand)

mkt_list <- unique(beer_mkt_agg$market)
mkt_list


beer_mkt_agg2 <- beer_mkt_agg %>% 
  group_by(market) %>% 
  mutate(pct = beer_floz / sum(beer_floz)) %>% 
  arrange(-beer_floz) %>% 
  slice(1)

# map data ----------------------------------------------------------------
library(geojsonsf)

dma_sf <- geojson_sf(
  "https://raw.githubusercontent.com/simzou/nielsen-dma/master/nielsen-mkt-map.json"
)

dma_sf2 <- geojson_sf(
  'https://raw.githubusercontent.com/simzou/nielsen-dma/master/nielsen-mkt-map_simplified.json'
)

save.image('/Users/byeong-hakchoe/Google Drive/suny-geneseo/teaching-materials/lecture-data/beer_map.RData')

ggplot(dma_sf) +
  geom_sf() +
  theme_map()


ggplot(dma_sf2) +
  geom_sf()

# devtools::install_github("UrbanInstitute/urbnmapr")
# library(urbnmapr)
# states_sf <- get_urbn_map(map = "states", sf = TRUE)
# state_data <- left_join(states_sf, statedata, by = "state_name")
# 
# 
# ggplot() +
#   geom_sf(data = state_data, fill = 'blue', alpha = .2) +
#   geom_sf(data = dma_sf2) 
# 
# 
# 
# dma_sf2_moved <- dma_sf
# st_geometry(dma_sf2_moved) <- st_geometry(dma_sf2_moved) + 
#   c(-100, 20)
# st_crs(dma_sf2_moved) <- st_crs(dma_sf2)
# 
# ggplot()+
#   geom_sf(data = state_data, aes(fill = hhpop))+
#   geom_sf(data = dma_sf2_moved)+ 
#   theme(legend.position = "none") +
#   theme_map()



# fuzzy matching ----------------------------------------------------------

beer_mkt_agg2 <- beer_mkt_agg2 %>% 
  mutate(mkt = str_to_lower(market))

dma_sf_merge <- dma_sf2 %>% 
  mutate(mkt0 = str_to_lower(dma_name)) %>% 
  separate(mkt0, into = c("mkt", "state"), sep = ", ") %>% 
  mutate(mkt = ifelse(mkt == "cleveland-akron (canton)",
                      "cleveland", mkt),
         mkt = ifelse(mkt == "birmingham (anniston and tuscaloosa)",
                      "birmingham", mkt),
         mkt = ifelse(mkt == "dallas-ft. worth",
                      "dallas", mkt),
         mkt = ifelse(mkt == "des moines-ames",
                      "des moines", mkt),
         mkt = ifelse(mkt == "hartford & new haven",
                      "hartford-new haven", mkt),
         mkt = ifelse(mkt == "little rock-pine bluff",
                      "little rock", mkt),
         mkt = ifelse(mkt == "miami-fort lauderdale",
                      "miami", mkt),
         mkt = ifelse(mkt == "minneapolis-st. paul",
                      "minneapolis", mkt),
         mkt = ifelse(mkt == "new orleans",
                      "new orleans-mobile", mkt),
         mkt = ifelse(mkt == "oklahoma city",
                      "oklahoma city-tulsa", mkt),
         mkt = ifelse(mkt == "orlando-daytona beach-melbourne",
                      "orlando", mkt),
         mkt = ifelse(mkt == "orlando-daytona beach-melbourne",
                      "orlando", mkt),
         mkt = ifelse(mkt == "portland",
                      "portland, or", mkt),
         mkt = ifelse(mkt == "raleigh-durham (fayetteville)",
                      "raleigh-durham", mkt),
         mkt = ifelse(mkt == "sacramento-stockton-modesto",
                      "sacramento", mkt),
         mkt = ifelse(mkt == "albany-schenectady-troy",
                      "albany", mkt),
         mkt = ifelse(mkt == "san francisco-oakland-san jose",
                      "san francisco", mkt),
         mkt = ifelse(mkt == "seattle-tacoma",
                      "seattle", mkt),
         mkt = ifelse(mkt == "tampa-st. petersburg (sarasota)",
                      "tampa", mkt),
         mkt = ifelse(mkt == "washington",
                      "washington dc", mkt),
         mkt = ifelse(mkt == "richmond-petersburg",
                      "richmond", mkt),
         mkt = ifelse(mkt == "grand rapids-kalamazoo-battle creek",
                      "grand rapids", mkt)) 

beer_mkt_dma <- 
  stringdist_join(beer_mkt_agg2, dma_sf_merge,  
                by='mkt', #match based on team
                mode='left', #use left join
                # method = "jw", #use jw distance metric
                max_dist=99, 
                distance_col='dist') %>% 
  select(mkt.x, mkt.y, state, dist, everything() ) %>% 
  arrange(mkt.x, dist) %>% 
  group_by(mkt.x) %>% 
  filter(dense_rank(dist) <= 2) %>% 
  ungroup() 

beer_mkt_dma_1 <- beer_mkt_dma %>% 
  filter(!str_detect(mkt.x, "rural")) %>% 
  filter(!str_detect(mkt.x, "urban")) %>% 
  filter( !(mkt.y == "rochester" & state == "mn-mason city") ) %>% 
  group_by(mkt.x) %>% 
  mutate(min_dist = min(dist, na.rm = T)) %>% 
  filter( (min_dist == 0 & dist == 0) | 
            min_dist != 0  )


ggplot() +
  geom_sf(data = dma_sf2, 
          mapping = aes(geometry = geometry)) +
  geom_sf(data = beer_mkt_dma_1, 
          mapping =  aes(geometry = geometry, fill = pct)) +
  theme_map()

beer_mkt_agg2_2 <- beer_mkt_agg2 %>% 
  filter(str_detect(market, "RURAL") | str_detect(market, "URBAN"))

str_to_lower(beer_mkt_agg2_2$market) 

dma_sf_merge2 <- dma_sf_merge %>% 
  filter( !(mkt %in% unique(beer_mkt_dma_1$mkt.y)) ) %>% 
  filter( !is.na(mkt) ) %>% 
  mutate(mkt = ifelse(state == "al", "rural alabama", mkt),
         mkt = ifelse(state == "ca", "rural california", mkt),
         mkt = ifelse(state == "co", "rural colorado", mkt),
         mkt = ifelse(state == "fl", "rural florida", mkt),
         mkt = ifelse(state == "ga", "rural georgia", mkt),
         mkt = ifelse(state == "ks", "rural kansas", mkt),
         mkt = ifelse(state == "la-el dorado", "rural arkansas", mkt),
         mkt = ifelse(state == "ky", "rural kentucky", mkt),
         mkt = ifelse(state == "la", "rural louisiana", mkt),
         mkt = ifelse(state == "in", "rural indiana", mkt),
         mkt = ifelse(state == "ia", "rural iowa", mkt),
         mkt = ifelse(state == "il", "rural illinois", mkt),
         mkt = ifelse(state == "id", "rural idaho", mkt),
         mkt = ifelse(state == "me", "rural maine", mkt),
         mkt = ifelse(state == "mi", "rural michigan", mkt),
         mkt = ifelse(state == "mn", "rural minnesota", mkt),
         mkt = ifelse(state == "ms", "rural mississippi", mkt),
         mkt = ifelse(state == "mo", "rural missouri", mkt),
         mkt = ifelse(state == "mt", "rural montana", mkt),
         mkt = ifelse(state == "ne", "rural nebraska", mkt),
         mkt = ifelse(state == "nv", "rural nevada", mkt),
         mkt = ifelse(state == "mo", "rural missouri", mkt),
         mkt = ifelse(dma_name == "Portland-Auburn, ME", "rural new hampshire", mkt),
         mkt = ifelse(state == "nm", "rural new mexico", mkt),
         mkt = ifelse(state == "nc", "rural north carolina", mkt),
         mkt = ifelse(state == "nd", "rural north dakota", mkt),
         mkt = ifelse(state == "ok", "rural oklahoma", mkt),
         mkt = ifelse(state == "oh", "rural ohio", mkt),
         mkt = ifelse(state == "or", "rural oregon", mkt),
         mkt = ifelse(state == "pa", "rural pennsylvania", mkt),
         mkt = ifelse(state == "sc", "rural south carolina", mkt),
         mkt = ifelse(state == "sd", "rural south dakota", mkt),
         mkt = ifelse(state == "tn", "rural tennessee", mkt),
         mkt = ifelse(state == "tx", "rural texas", mkt),
         mkt = ifelse(state == "vt-plattsburgh", "rural vermont", mkt),
         mkt = ifelse(state == "va", "rural virginia", mkt),
         mkt = ifelse(state == "wa", "rural washington", mkt),
         mkt = ifelse(state == "wv", "rural west virginia", mkt),
         mkt = ifelse(state == "wi", "rural wisconsin", mkt),
         mkt = ifelse(state == "wy", "rural wyoming", mkt),
         mkt = ifelse(state == "ny" & mkt == "binghamton", "rural new york", mkt),
         mkt = ifelse(state == "ny" & mkt == "elmira", "rural new york", mkt),
         mkt = ifelse(state == "ny" & mkt == "utica", "exurban ny", mkt),
         mkt = ifelse(state == "ny" & mkt == "watertown", "surburban ny", mkt),
         mkt = ifelse(state == "ny" & mkt == "albany-schenectady-troy", "urban ny", mkt),
         mkt = ifelse(state == "ny" & mkt == "new york", "urban ny", mkt)
  ) %>% 
  filter(dma_name != "Alaska")


beer_mkt_dma_2 <- 
  stringdist_join(beer_mkt_agg2_2, dma_sf_merge2,  
                  by='mkt', #match based on team
                  mode='left', #use left join
                  # method = "jw", #use jw distance metric
                  max_dist=99, 
                  distance_col='dist') %>% 
  select(mkt.x, mkt.y, state, dist, everything() ) %>% 
  arrange(mkt.x, dist) %>% 
  group_by(mkt.x) %>% 
  filter(dense_rank(dist) <= 2) %>% 
  ungroup() 


beer_mkt_dma_2 <- beer_mkt_dma_2 %>% 
  group_by(mkt.x) %>% 
  mutate(min_dist = min(dist, na.rm = T)) %>% 
  filter( (min_dist == 0 & dist == 0) | 
            min_dist != 0  )

beer_mkt_dma_full <- rbind(beer_mkt_dma_1, beer_mkt_dma_2) %>% 
  ungroup() 

dma_sf3 <- dma_sf2 %>% 
  filter(dma_name != "Alaska") %>% 
  filter(dma_name != "Honolulu, HI") %>% 
  filter(str_sub(dma_name, -4, -1) != ", AK")
  

# ggplot() +
#   geom_sf(data = dma_sf3, 
#           mapping = aes(geometry = geometry)) +
#   geom_sf(data = beer_mkt_dma_full, 
#           mapping =  aes(geometry = geometry, fill = pct)) +
#   theme_map()
# 
# bud_logo <- beer_mkt_dma_full %>% 
#   filter(brand == "BUD LIGHT") %>% 
#   select(latitude, longitude)
# 
# busch_logo <- beer_mkt_dma_full %>% 
#   filter(brand == "BUSCH LIGHT") %>% 
#   select(latitude, longitude)
# 
# coors_logo <- beer_mkt_dma_full %>% 
#   filter(brand == "COORS LIGHT") %>% 
#   select(latitude, longitude)
# 
# miller_logo <- beer_mkt_dma_full %>% 
#   filter(brand == "MILLER LITE") %>% 
#   select(latitude, longitude)
# 
# natural_logo <- beer_mkt_dma_full %>% 
#   filter(brand == "NATURAL LIGHT") %>% 
#   select(latitude, longitude)
# 
# 
# beer_mkt_dma_full <- beer_mkt_dma_full %>% 
#   mutate(logo = 
#            case_when(brand == "NATURAL LIGHT" ~ "img/natural-logo.png",
#                      brand == "MILLER LITE" ~ "img/miller-logo.png",
#                      brand == "COORS LIGHT" ~ "img/coors-logo.png",
#                      brand == "BUSCH LIGHT" ~ "img/busch-logo.png",
#                      brand == "BUD LIGHT" ~ "img/bud-logo.png",
#            ))




# dma_moved <- dma_sf3
# st_geometry(dma_moved) <- st_geometry(dma_moved) + 
#   c(-60, 20)
# st_crs(dma_moved) <- st_crs(dma_sf3)

# beer_mkt_dma_full_moved <- beer_mkt_dma_full
# st_geometry(beer_mkt_dma_full_moved) <- st_geometry(beer_mkt_dma_full_moved) + 
#   c(-60, 20)
# st_crs(beer_mkt_dma_full_moved) <- st_crs(beer_mkt_dma_full)



# ggplot() +
#   geom_sf(data = dma_sf3, 
#           mapping = aes(geometry = geometry)) +
#   geom_sf(data = beer_mkt_dma_full, 
#           mapping =  aes(geometry = geometry, fill = pct)) +
#   geom_image(data = beer_mkt_dma_full, 
#           mapping =  aes(x = (longitude + 95) * .785, 
#                          y = latitude - 22.5, 
#                          image = logo)) +
#   coord_sf(lims_method = "orthogonal") 

# annotation_custom(rasterGrob(readPNG("img/bud-logo.png"), 
#                              width = unit(1,"npc"),
#                              height = unit(1,"npc")), 
#                   -Inf, Inf, -Inf, Inf) 


# devtools::install_github("yutannihilation/ggsflabel")

beer_mkt_dma_full$brand <- 
  factor(beer_mkt_dma_full$brand,
         levels = c("BUD LIGHT", "COORS LIGHT", "MILLER LITE",
                    "BUSCH LIGHT", "NATURAL LIGHT"))

# beer_mkt_dma_full <- beer_mkt_dma_full %>% 
#   mutate(NY = ifelse(state == "ny",
#                      T, F))
# beer_mkt_dma_full_NY <- beer_mkt_dma_full %>% 
#   filter(state == "ny") %>% 
  

library(ggsflabel)
library(RColorBrewer)
my_breaks <- c(.3, .4, .5, .6, .7)
library(ggiraph)


p <- ggplot(mapping = aes(geometry = geometry)) +
  geom_sf(data = dma_sf3) +
  geom_sf(data = beer_mkt_dma_full,
          mapping =  aes(fill = pct)) +
  # geom_sf_label_repel(data = beer_mkt_dma_full,
                      # mapping =  aes(label = brand, color = brand)) +
  guides(color = "none") +
  guides(fill = guide_legend(reverse = TRUE,
                             title.position = "top",
                             title.hjust = .5,
                             label.position = "bottom",
                             keywidth = 3,
                             nrow = 1)) +
  scale_fill_viridis_c(breaks = my_breaks,
                       name = "Market Share",
                       label = scales::percent ) +
  scale_color_brewer(palette = "Dark2") +
  # scale_fill_gradient(name = "Market Share",
  #                     label = scales::percent,
  #                     low = "#ffffff",
  #                     high = "#506298") +
  facet_wrap(brand ~ .) +
  theme_map() +
  theme(legend.position = "top",
        legend.direction = "horizontal",
        legend.justification = "center",
        legend.box.margin=margin(30,30,30,30),
        legend.title=element_text(size = 15,
                                  face = 'bold'))
p

ggsave('lec_figs/beer_map.png',
       plot = p, height = 6, width = 8, units = "in") 



p2 <- ggplot(data = beer_mkt_dma_full,
             aes(geometry = geometry)) +
  geom_sf(data = dma_sf3) +
  geom_sf_interactive(mapping =  
                        aes(fill = brand,
                            tooltip = 
                              interaction(market,
                                          paste0(" ", round(pct*100, 2), "%")),
                                     data_id = market)) +
  guides(color = "none") +
  guides(fill = guide_legend(reverse = TRUE,
                             title.position = "top",
                             title.hjust = .5,
                             label.position = "bottom",
                             keywidth = 3,
                             nrow = 1)) +
  scale_fill_brewer_interactive(name = "Top Brand",
                                palette = 'Set1') +
  theme_map() +
  theme(legend.position = "top",
        legend.direction = "horizontal",
        legend.justification = "center",
        legend.box.margin=margin(30,30,30,30),
        legend.title=element_text(size = 15,
                                  face = 'bold'))

p2

widg <- ggiraph(ggobj = p2, width_svg = 6, height_svg = 8)
widg

# save the widget
library(htmlwidgets)
saveWidget(widg, 
           file=paste0( getwd(), "/beer_map.html"))





p_ny <- ggplot(data = filter(beer_mkt_dma_full, state == "ny"),
       aes(geometry = geometry)) +
  geom_sf(data = filter(dma_sf2, str_sub(dma_name, -4, -1) == ", NY")) +
  geom_sf(mapping =  aes(fill = pct)) +
  geom_sf_label_repel(mapping =  aes(label = brand, color = brand)) +
  guides(color = "none") +
  guides(fill = guide_legend(reverse = TRUE,
                             title.position = "top",
                             title.hjust = .5,
                             label.position = "bottom",
                             keywidth = 3,
                             nrow = 1)) +
  scale_fill_viridis_c(breaks = my_breaks,
                       name = "Market Share",
                       label = scales::percent ) +
  scale_color_brewer(palette = "Dark2") +
  theme_map() +
  theme(legend.position = "top",
        legend.direction = "horizontal",
        legend.justification = "center",
        legend.box.margin=margin(30,30,30,30),
        legend.title=element_text(size = 15,
                                  face = 'bold'))
p_ny


ggsave('lec_figs/beer_map_ny.png',
       plot = p_ny, height = 6, width = 8, units = "in") 

  


p_ny2 <- ggplot(data = filter(beer_mkt_dma_full, state == "ny"),
               aes(geometry = geometry)) +
  geom_sf(data = filter(dma_sf2, str_sub(dma_name, -4, -1) == ", NY")) +
  geom_sf_interactive(mapping =  aes(fill = pct)) +
  geom_sf_label_interactive(mapping =  aes(label = brand, color = brand,
                                           tooltip = 
                                             interaction(market,
                                                         paste0(" ", round(pct*100, 2), "%")),
                                           data_id = market)) +
  guides(color = "none") +
  guides(fill = guide_legend(reverse = TRUE,
                             title.position = "top",
                             title.hjust = .5,
                             label.position = "bottom",
                             keywidth = 3,
                             nrow = 1)) +
  scale_fill_viridis_c(breaks = my_breaks,
                       name = "Market Share",
                       label = scales::percent ) +
  scale_color_brewer(palette = "Dark2") +
  theme_map() +
  theme(legend.position = "top",
        legend.direction = "horizontal",
        legend.justification = "center",
        legend.box.margin=margin(30,30,30,30),
        legend.title=element_text(size = 15,
                                  face = 'bold'))
p_ny2


widg <- ggiraph(ggobj = p_ny2, width_svg = 6, height_svg = 8)
widg

# save the widget
library(htmlwidgets)
saveWidget(widg, 
           file=paste0( getwd(), "/beer_map_ny.html"))



