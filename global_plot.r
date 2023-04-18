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
tl.data.year <- tibble::rownames_to_column(tl.data, var = "year") %>% filter(year >= 2000)
tc.data.year <- tibble::rownames_to_column(tc.data, var = "year") %>% filter(year >= 2000)

tl.data.T <- t(tl.data.year) 
colnames(tl.data.T) <- tl.data.T[1,]
tl.data.T <- tl.data.T[-1,]
colnames(tl.data.T) <- make.names(colnames(tl.data.T))
tl.data.T <- as.data.frame(tl.data.T)
tl.data.T <- tibble::rownames_to_column(tl.data.T, var = "stockid")
tl.data.T <- setDT(tl.data.T)[stockid %chin% metadata$stockid] # 1,927,235 row deleted
forage.data <- merge(tl.data.T, metadata, by=(c('stockid')), all.x = TRUE, all.y = FALSE)

## Aggregate data by country

library(tidyr)
forage.data <- forage.data %>% 
  group_by(primary_country, commonname) %>% 
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
    coordinates = geo_osm(forage.data.mean.wide$primary_country[i])
    forage.data.mean.wide$long[i] = coordinates$long
    forage.data.mean.wide$lat[i] = coordinates$lat
  }  
  
world_map_data <- ne_countries(scale = "medium", returnclass = "sf")
land_color <- c('antiquewhite1')
dev.off()

library(scatterpie)
ggplot() +
  geom_sf(data = world_map_data, fill = land_color, size = .4) + 
  geom_scatterpie(aes(x=long, y=lat, group = primary_country, r = total_catch/500000), 
                data = forage.data.mean.wide, cols = colnames(forage.data.mean.wide[,c(2:25)]))



# + coord_sf(xlim = c(-100, -91), ylim = c(25,33))

coord <- sf::st_centroid(world_map_data, geom = "point",
                         position = "identity")

## Create map ###
geocode.country_points <- geocode(df.country_points$country)
df.country_points <- cbind(df.country_points,geocode.country_points)



