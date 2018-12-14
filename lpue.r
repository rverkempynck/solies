rm(list = ls())

library(dplyr)
library(tidyr)
library(readr)
library(ggplot2)

for(i in c(2003:2016)){
  landings_tmp <- read_csv(paste0("map__landings_by_rectangle_data_", i, ".csv"))
  if (i == 2003){
    landings_sol <- landings_tmp
  }else{
    landings_sol <- rbind(landings_sol, landings_tmp)
  }}

for (i in c(2003:2016)){
  effort_tmp <- read_csv(paste0("map__effort_by_rectangle_data_",i, ".csv"))
  if (i == 2003){
    effort <- effort_tmp
  }else{
    effort <- rbind(effort, effort_tmp)
  }}

rm(landings_tmp, effort_tmp)

# data per rectangle sommeren voor gears en vermenigvuldigen met 1000 en alle NA's eruit filteren
landings_sol <- landings_sol %>% filter(!is.na(landings)) %>%
  group_by(`regulated gear` , `mesh size range`, year, quarter, `ICES rectangle`) %>%
  summarise(landings = sum(landings)*1000)

effort <- effort %>% group_by(`regulated gear` , country, year, quarter, `ICES rectangle`) %>%
  summarise(effort = sum(`Effective Effort`))

effort %>% filter(`regulated gear` == "BT2", `ICES rectangle` %in% c("32F1", "32F2", "31F1", "31F2")) %>% 
  ggplot(aes(year, effort, fill = country)) +
  geom_col() +
  facet_wrap(~`ICES rectangle`) +
  theme_minimal()

# lpue maken door landings te delen door effort en van ices rectangle midpoint definieren om te plotten
lpue_sol <- full_join(landings_sol, effort) %>%
  mutate(lpue = landings/effort,
         lon = geo::ir2d(`ICES rectangle`)$lon,
         lat = geo::ir2d(`ICES rectangle`)$lat) %>% filter(!is.na(lpue))

# install.packages("mapdata") # haal hashtag weg vooraan rij en voer uit 
# install.packages("ggplot2")
library(mapdata)

# definieer gebied voor kaartjes uit worldHires database
north_sea <- map_data("worldHires") %>% filter(long > -10 & long < 20 & lat > 50 & lat < 60)


# plot gebied met lpue data per jaar
lpue_sol %>% ggplot() +
  theme_light() +
  geom_tile(aes(lon, lat, fill = lpue)) +
  geom_polygon(data = north_sea, aes(long, lat, group = group), color = "grey") +
  coord_fixed(1.3) +
  facet_wrap(~year)+
  scale_fill_continuous(low = "yellow", high = "red", trans = "log") +
  coord_quickmap(xlim = c(-5,10), ylim = c(50,60)) +
  theme(legend.title = element_text(face = "italic", size = 8))

# plot gebied met lpue data per jaar
lpue_sol %>% ggplot() +
  theme_light() +
  geom_tile(aes(lon, lat, fill = lpue)) +
  geom_polygon(data = north_sea, aes(long, lat, group = group), color = "grey") +
  coord_fixed(1.3) +
  facet_wrap(~year)+
  scale_fill_continuous(low = "yellow", high = "red", trans = "log") +
  coord_quickmap(xlim = c(-5,10), ylim = c(50,60)) +
  theme(legend.title = element_text(face = "italic", size = 8))
