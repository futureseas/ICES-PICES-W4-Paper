################################################
### Forage fish plot for W4 ICES/PICES paper ###
################################################

# This will load database data from the file DBdata.RData.

gc()
gg <- load("G:\\My Drive\\ICES PICES W4 Paper\\R Data\\DBdata[asmt][v4.495].RData")
rm(list = setdiff(ls(), c("taxonomy", 'metadata', 'tl.data', 'tc.data')))

library(data.table)
library(tidyverse)
taxonomy <- taxonomy %>% dplyr::filter(FisheryType == "Forage Fish")
metadata <- setDT(metadata)[scientificname %chin% taxonomy$scientificname] 
tl.data <- tibble::rownames_to_column(tl.data, var = "year") %>% filter(year >= 2000)
tc.data <- tibble::rownames_to_column(tc.data, var = "year") %>% filter(year >= 2000)
tl.data <- t(tl.data) 
tc.data <- t(tc.data) 
colnames(tl.data) <- tl.data[1,]
colnames(tc.data) <- tc.data[1,]
tl.data <- tl.data[-1,]
tc.data <- tc.data[-1,]
colnames(tl.data) <- make.names(colnames(tl.data))
colnames(tc.data) <- make.names(colnames(tc.data))
tl.data <- as.data.frame(tl.data)
tc.data <- as.data.frame(tc.data)
tl.data <- tibble::rownames_to_column(tl.data, var = "stockid")
tc.data <- tibble::rownames_to_column(tc.data, var = "stockid")
tl.data <- setDT(tl.data)[stockid %chin% metadata$stockid] 
tc.data <- setDT(tc.data)[stockid %chin% metadata$stockid] 
tl.data <- merge(tl.data, metadata, by=(c('stockid')), all.x = TRUE, all.y = FALSE) %>% mutate(type_data = "tl")
tc.data <- merge(tc.data, metadata, by=(c('stockid')), all.x = TRUE, all.y = FALSE) %>% mutate(type_data = "tc")

forage.data <- rbind(tl.data, tc.data)
forage.data <- forage.data %>% group_by(stockid, region, commonname) %>% 
  summarize(X2000 = max(as.numeric(X2010)), X2001 = max(as.numeric(X2011)),
            X2002 = max(as.numeric(X2012)), X2003 = max(as.numeric(X2013)),
            X2004 = max(as.numeric(X2014)), X2005 = max(as.numeric(X2015)),
            X2006 = max(as.numeric(X2016)), X2007 = max(as.numeric(X2017)),
            X2008 = max(as.numeric(X2018)), X2009 = max(as.numeric(X2019)),
            X2010 = max(as.numeric(X2010)), X2011 = max(as.numeric(X2011)),
            X2012 = max(as.numeric(X2012)), X2013 = max(as.numeric(X2013)),
            X2014 = max(as.numeric(X2014)), X2015 = max(as.numeric(X2015)),
            X2016 = max(as.numeric(X2016)), X2017 = max(as.numeric(X2017)),
            X2018 = max(as.numeric(X2018)), X2019 = max(as.numeric(X2019)), 
            X2020 = max(as.numeric(X2020))) 


## Aggregate data by country

library(tidyr)
forage.data <- forage.data %>% 
  group_by(region, commonname) %>% 
  summarize(total_2000 = sum(as.numeric(X2010)), total_2001 = sum(as.numeric(X2011)),
            total_2002 = sum(as.numeric(X2012)), total_2003 = sum(as.numeric(X2013)),
            total_2004 = sum(as.numeric(X2014)), total_2005 = sum(as.numeric(X2015)),
            total_2006 = sum(as.numeric(X2016)), total_2007 = sum(as.numeric(X2017)),
            total_2008 = sum(as.numeric(X2018)), total_2009 = sum(as.numeric(X2019)),
            total_2010 = sum(as.numeric(X2010)), total_2011 = sum(as.numeric(X2011)),
            total_2012 = sum(as.numeric(X2012)), total_2013 = sum(as.numeric(X2013)),
            total_2014 = sum(as.numeric(X2014)), total_2015 = sum(as.numeric(X2015)),
            total_2016 = sum(as.numeric(X2016)), total_2017 = sum(as.numeric(X2017)),
            total_2018 = sum(as.numeric(X2018)), total_2019 = sum(as.numeric(X2019)), 
            total_2020 = sum(as.numeric(X2020))) %>% ungroup()

forage.data.mean <- forage.data[ , c(1:2)]
forage.data.mean$mean <- rowMeans(forage.data[ , c(3:ncol(forage.data))], na.rm=TRUE)
forage.data.mean <- forage.data.mean %>% drop_na()
# forage.data.mean.expand <- forage.data.mean %>% expand(region, commonname)
# forage.data.mean <- forage.data.mean %>% dplyr::right_join(forage.data.mean.expand)

forage.data.mean[is.na(forage.data.mean)] <- 0

# forage.data.mean <- forage.data.mean[order(-forage.data.mean$mean),] #forage.data.mean$region, 
# forage.data.mean.highest <- forage.data.mean %>% 
#   mutate(principal = ifelse(row_number()==1:21, 1, 0)) 
# 
# forage.data.mean.highest <- forage.data.mean.highest %>%
#   mutate(commonname = ifelse(principal == 0, "Other", commonname))
# 
# forage.data.mean <- forage.data.mean.highest %>% 
#   group_by(region, commonname) %>% 
#   summarize(mean = sum(mean)) %>% ungroup()

# long to wide
forage.data.mean.wide <- forage.data.mean %>% 
  pivot_wider(names_from = commonname, values_from = mean) 

  forage.data.mean.wide [is.na(forage.data.mean.wide )] <- 0


## Include total landings
forage.data.mean.wide$total_catch <- rowSums(forage.data.mean.wide[ , c(2:ncol(forage.data.mean.wide))], na.rm=TRUE)


## Get coordinates for each country
library(maps)
library(sf)
library(rnaturalearth)
library(tidyverse)
library(ggspatial)
library(tidygeocoder)
library(ggrepel)


  for(i in 1:nrow(forage.data.mean.wide)){
    coordinates = geo_osm(forage.data.mean.wide$region[i])
    forage.data.mean.wide$long[i] = coordinates$long
    forage.data.mean.wide$lat[i] = coordinates$lat
  }  

forage.data.mean.wide <- forage.data.mean.wide %>%
  mutate(long = ifelse(region == "Europe non EU", 22, long)) %>%
  mutate(lat  = ifelse(region == "Europe non EU", 63, lat)) %>%
  mutate(long = ifelse(region == "US Southeast and Gulf", -88, long)) %>%
  mutate(lat  = ifelse(region == "US Southeast and Gulf", 30, lat)) %>%
  mutate(long = ifelse(region == "Southern Africa", 24, long)) %>%
  mutate(lat  = ifelse(region == "Southern Africa", -26, lat)) %>%
  mutate(long = ifelse(region == "European Union", long-15, long)) %>%
  mutate(lat  = ifelse(region == "European Union", lat+5, lat)) %>%
  drop_na()
  
world_map_data <- ne_countries(scale = "medium", returnclass = "sf")
land_color <- c('antiquewhite1')


mytheme <- theme(panel.grid.major = element_line(color = '#cccccc' 
                                                  ,linetype = 'dashed'
                                                  ,size = .3
                 )
                 ,panel.background = element_rect(fill = 'aliceblue')
                 ,plot.title = element_text(size = 12)
                 ,plot.subtitle = element_text(size = 11)
                 ,axis.title = element_blank()
                 ,axis.text = element_text(size = 10)
)





###################
 ## Create Map! ##
###################

library(scatterpie)
max_obs = ncol(forage.data.mean.wide) - 3

# CREATE X AND Y 'NUDGE' 
forage.data.mean.wide <- forage.data.mean.wide %>% 
  mutate(x_nudge = case_when(region == "Australia" ~ 2,
                             region == "Canada East Coast" ~ 20,
                             region == "Canada West Coast" ~ 0, 
                             region == "Europe non EU" ~ 0,
                             region == "European Union" ~ 10,
                             region == "Japan" ~ 0, 
                             region == "Mediterranean-Black Sea" ~ 0,
                             region == "South America" ~ 0,
                             region == "Southern Africa" ~ 0,
                             region == "US East Coast" ~ 25,
                             region == "US Southeast and Gulf" ~ 30,
                             region == "US West Coast" ~ -1,
                             region == "West Africa" ~ 0,
                             TRUE ~ 0),
         y_nudge = case_when(region == "Australia" ~ 0,
                             region == "Canada East Coast" ~ 8,
                             region == "Canada West Coast" ~ 1, 
                             region == "Europe non EU" ~ 15,
                             region == "European Union" ~ 0,
                             region == "Japan" ~ 0, 
                             region == "Mediterranean-Black Sea" ~ -10,
                             region == "South America" ~ 0,
                             region == "Southern Africa" ~ 0,
                             region == "US East Coast" ~ 0,
                             region == "US Southeast and Gulf" ~ -5,
                             region == "US West Coast" ~ 0,
                             region == "West Africa" ~ 0,
                             TRUE ~ 0))

ggplot() +
  geom_sf(data = world_map_data, fill = land_color, size = .4) +
  geom_scatterpie(aes(x=long, y=lat, group = region, r = sqrt(total_catch/10000)), 
                data = forage.data.mean.wide, legend_name = "Species",
                cols = colnames(forage.data.mean.wide[,c(2:max_obs)])) +
  mytheme +
  theme(legend.position = "bottom") + 
  geom_scatterpie_legend(sqrt(forage.data.mean.wide$total_catch/10000), x=-160, y=-40, labeller=function(x) (x/10)^2) +
  geom_text_repel(aes(x=long, y=lat, group = region, label = region), 
            data = forage.data.mean.wide, segment.color = "#333333",
            nudge_x = forage.data.mean.wide$x_nudge, 
            nudge_y = forage.data.mean.wide$y_nudge)

