###################################################################################
###################################################################################
###################################################################################
#
# This will load database data from the file DBdata.RData. Put the data file in the
# working directory, then run the line at the bottom of the file.
#
###################################################################################
###################################################################################
###################################################################################
#
# The following objects are tables from RAM (data frames):
#
# --- metadata
#	Summarized metadata
# --- stock
#	General stock metadata
# --- assessment
#	General assessment metadata
# --- taxonomy
#	Taxonomic metadata
# --- management
#	Management authority metadata
# --- assessor
#	Stock assessor metadata
# --- assessmethod
#	Assessment method metadata
# --- area
#	Area metadata
# --- biometrics
#	Parameter data types with descriptions
# --- tsmetrics
#	Time series data types with descriptions
# --- timeseries
#	Full time series data listing
# --- bioparams
#	Full parameter data listing
# --- timeseries_values_views
#	Values by stock and year of common time series types
# --- timeseries_units_views
#	Units corresponding to values in timeseries_values_views
# --- timeseries_ids_views
#	Time series IDs corresponding to values in timeseries_values_views
# --- timeseries_assessments_views
#	Assessment IDs corresponding to values in timeseries_values_views
# --- timeseries_notes_views
#	Notes corresponding to values in timeseries_values_views
# --- timeseries_sources_views
#	Sources corresponding to values in timeseries_values_views
# --- timeseries_years_views
#	Year range corresponding to values in timeseries_values_views
# --- bioparams_values_views
#	Values by stock of common parameter types
# --- bioparams_units_views
#	Units corresponding to values in bioparams_values_views
# --- bioparams_ids_views
#	Parameter IDs corresponding to values in bioparams_values_views
# --- bioparams_assessments_views
#	Assessment IDs corresponding to values in bioparams_values_views
# --- bioparams_sources_views
#	Sources corresponding to values in bioparams_values_views
# --- bioparams_notes_views
#	Notes corresponding to values in bioparams_values_views
#
# ---------------------------------------------------------------------------------------------------
#
# There are also dataframes for the individual most-used time series:
#
# --- tb.data --- Total biomass data
# --- ssb.data --- Spawning stock biomass data
# --- tn.data --- Total abundance data
# --- r.data --- Recruits data
# --- tc.data --- Total catch data
# --- tl.data --- Total landings data
# --- recc.data --- Recreational catch data
# --- f.data --- Fishing mortality data (usually an instantaneous rate)
# --- er.data --- Exploitation rate data (usually an annual fraction harvested)
# --- divtb.data --- TB/TBmsy data
# --- divssb.data --- SSB/SSBmsy data
# --- divf.data --- F/Fmsy data
# --- diver.data --- ER/ERmsy data
# --- divbpref.data --- B/Bmsy pref data (B/Bmsy if available, otherwise B/Bmgt)
# --- divupref.data --- U/Umsy pref data (U/Umsy if available, otherwise U/Umgt)
# --- tbbest.data --- TBbest data (all in MT)
# --- tcbest.data --- TCbest data (all in MT)
# --- erbest.data --- ERbest data (usually an annual fraction harvested)
# --- divtb.mgt.data --- TB/TBmgt data
# --- divssb.mgt.data --- SSB/SSBmgt data
# --- divf.mgt.data --- F/Fmgt data
# --- diver.mgt.data --- ER/ERmgt data
# --- divbpref.mgt.data --- B/Bmgt pref data (B/Bmgt if available, otherwise B/Bmsy)
# --- divupref.mgt.data --- U/Umgt pref data (U/Umgt if available, otherwise U/Umsy)
# --- cpair.data --- Catch data that pairs with tac.data and/or cadv.data
# --- tac.data --- TAC data
# --- cadv.data --- Scientific advice for catch limit data
# --- survb.data --- Fishery-independent survey abundance data
# --- cpue.data --- CPUE data (fishery-dependent)
# --- effort.data --- Fishing effort data (fishery-dependent)
# --- divtn.data --- TN/TNmsy data
# --- divtn.mgt.data --- TN/TNmgt data
# --- cdivmeanc.data --- Catch/(mean catch) data
# --- cdivmsy.data --- Catch/MSY data
#
###################################################################################
###################################################################################
###################################################################################
#
# Once the DBdata.RData file is in the working directory, simply run the following command to
# load up the database data into matrix/dataframe files for the model fits included version of the database.


load("G:\\My Drive\\ICES PICES W4 Paper\\R Data\\DBdata[asmt][v4.495].RData")

library(data.table)
library(tidyverse)
taxonomy <- taxonomy %>% dplyr::filter(FisheryType == "Forage Fish")
metadata <- setDT(metadata)[scientificname %chin% taxonomy$scientificname] 
tl.data.year <- tibble::rownames_to_column(tl.data, var = "year") %>% filter(year >= 2000)
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



