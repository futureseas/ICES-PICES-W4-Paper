################################################
### Forage fish plot for W4 ICES/PICES paper ###
################################################

# This will load database data from the file DBdata.RData.

gc()
gg <- read.csv("G:\\My Drive\\ICES PICES W4 Paper\\FAO_DATA\\FAO_fish_stat.csv")

library(data.table)
library(tidyverse)

colnames(gg)

forage.data <- gg %>% 
  select(-c('S', 'S.1', 'S.2', 'S.3', 'S.4', 'S.5', 
            'S.6', 'S.7', 'S.8', 'S.9', 'S.10')) %>% 
  group_by(Country..Name., ASFIS.species..Name.) %>% 
  summarize(X2010 = max(as.numeric(X.2010.)), 
            X2011 = max(as.numeric(X.2011.)),
            X2012 = max(as.numeric(X.2012.)), 
            X2013 = max(as.numeric(X.2013.)),
            X2014 = max(as.numeric(X.2014.)), 
            X2015 = max(as.numeric(X.2015.)),
            X2016 = max(as.numeric(X.2016.)), 
            X2017 = max(as.numeric(X.2017.)),
            X2018 = max(as.numeric(X.2018.)), 
            X2019 = max(as.numeric(X.2019.)), 
            X2020 = max(as.numeric(X.2020.))) %>%
  rename(Area.Name = Country..Name.) %>%
  rename(Species = ASFIS.species..Name.) %>% ungroup()


## Grouping species in smaller categories

print(forage.data %>% ungroup() %>% dplyr::select(c("Species")) %>% unique(), n=250) 
forage.data.filtered <- forage.data %>%
  dplyr::mutate(Species2 = 
    ifelse(str_detect(Species, "nchovy"), "Anchovy", 
    ifelse(str_detect(Species, "ardine"), "Sardines", 
    ifelse(str_detect(Species, "ardinella"), "Sardinellas", 
    ifelse(str_detect(Species, "herring"), "Herrings", 
    ifelse(str_detect(Species, "scad"), "Scads",
    ifelse(str_detect(Species, "ackerel"), "Mackerels",
    ifelse(str_detect(Species, "smelt"), "Smetls",
    ifelse(str_detect(Species, "ackerel"), "Mackerels",
    ifelse(str_detect(Species, "menhaden"), "Menhaden",
    ifelse(str_detect(Species, "saury"), "Saury",
    ifelse(str_detect(Species, "sprat"), "Sprat", NA)))))))))))) 
                                                                                
## Aggregate data by country
library(tidyr)
forage.data.Total <- forage.data.filtered%>% 
  group_by(Area.Name, Species2) %>% 
  summarize(total_2010 = sum(as.numeric(X2010), na.rm = TRUE), 
            total_2011 = sum(as.numeric(X2011), na.rm = TRUE),
            total_2012 = sum(as.numeric(X2012), na.rm = TRUE), 
            total_2013 = sum(as.numeric(X2013), na.rm = TRUE),
            total_2014 = sum(as.numeric(X2014), na.rm = TRUE), 
            total_2015 = sum(as.numeric(X2015), na.rm = TRUE),
            total_2016 = sum(as.numeric(X2016), na.rm = TRUE), 
            total_2017 = sum(as.numeric(X2017), na.rm = TRUE),
            total_2018 = sum(as.numeric(X2018), na.rm = TRUE), 
            total_2019 = sum(as.numeric(X2019), na.rm = TRUE), 
            total_2020 = sum(as.numeric(X2020), na.rm = TRUE)) %>% 
  ungroup() %>% drop_na() %>% 
  mutate(total_2010 = ifelse(total_2010 == 0, NA, total_2010)) %>% 
  mutate(total_2011 = ifelse(total_2011 == 0, NA, total_2011)) %>%
  mutate(total_2012 = ifelse(total_2012 == 0, NA, total_2012)) %>% 
  mutate(total_2013 = ifelse(total_2013 == 0, NA, total_2013)) %>%
  mutate(total_2014 = ifelse(total_2014 == 0, NA, total_2014)) %>% 
  mutate(total_2015 = ifelse(total_2015 == 0, NA, total_2015)) %>%
  mutate(total_2016 = ifelse(total_2016 == 0, NA, total_2016)) %>% 
  mutate(total_2017 = ifelse(total_2017 == 0, NA, total_2017)) %>%
  mutate(total_2018 = ifelse(total_2018 == 0, NA, total_2018)) %>% 
  mutate(total_2019 = ifelse(total_2019 == 0, NA, total_2019)) %>% 
  mutate(total_2020 = ifelse(total_2020 == 0, NA, total_2020))

forage.data.mean <- forage.data.Total[ , c(1:2)]
forage.data.mean$mean <- rowMeans(forage.data.Total[ , c(3:ncol(forage.data.Total))], na.rm=TRUE)
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
  pivot_wider(names_from = Species2, values_from = mean) 

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
  coordinates = geo(forage.data.mean.wide$Area.Name[i])
  forage.data.mean.wide$long[i] = coordinates$long
  forage.data.mean.wide$lat[i] = coordinates$lat
}  

# forage.data.mean.wide <- forage.data.mean.wide %>%
#   mutate(long = ifelse(region == "Europe non EU", 22, long)) %>%
#   mutate(lat  = ifelse(region == "Europe non EU", 63, lat)) %>%
#   mutate(long = ifelse(region == "US Southeast and Gulf", -88, long)) %>%
#   mutate(lat  = ifelse(region == "US Southeast and Gulf", 30, lat)) %>%
#   mutate(long = ifelse(region == "Southern Africa", 24, long)) %>%
#   mutate(lat  = ifelse(region == "Southern Africa", -26, lat)) %>%
#   mutate(long = ifelse(region == "European Union", long-15, long)) %>%
#   mutate(lat  = ifelse(region == "European Union", lat+5, lat)) %>%
#   drop_na()

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
  mutate(x_nudge = case_when(Area.Name == "Australia and New Zealand" ~ 0,
                             Area.Name == "Caribbean" ~ 0,
                             Area.Name == "Central America" ~ 0,
                             Area.Name == "Eastern Africa" ~ 0,
                             Area.Name == "Eastern Asia" ~ 0,
                             Area.Name == "Eastern Europe" ~ 0,
                             Area.Name == "Melanesia" ~ 0,
                             Area.Name == "Micronesia" ~ 0,
                             Area.Name == "Middle Africa" ~ 0, 
                             Area.Name == "Northern Africa" ~ 0,
                             Area.Name == "Northern America" ~ 0,
                             Area.Name == "Northern Europe" ~ 0,
                             Area.Name == "Others" ~ 0,
                             Area.Name == "Polynesia" ~ 0,
                             Area.Name == "South-Eastern Asia" ~ 0,
                             Area.Name == "South America" ~ 0,
                             Area.Name == "Southern Africa" ~ 0,
                             Area.Name == "Southern Asia" ~ 0,
                             Area.Name == "Southern Europe" ~ 0,
                             Area.Name == "Western Africa" ~ 0,
                             Area.Name == "Western Asia" ~ 0, 
                             Area.Name == "Western Europe" ~ 0,
                             TRUE ~ 0),
         y_nudge = case_when(Area.Name == "Australia and New Zealand" ~ 0,
                             Area.Name == "Caribbean" ~ 0,
                             Area.Name == "Central America" ~ 0,
                             Area.Name == "Eastern Africa" ~ 0,
                             Area.Name == "Eastern Asia" ~ 0,
                             Area.Name == "Eastern Europe" ~ 0,
                             Area.Name == "Melanesia" ~ 0,
                             Area.Name == "Micronesia" ~ 0,
                             Area.Name == "Middle Africa" ~ 0, 
                             Area.Name == "Northern Africa" ~ 0,
                             Area.Name == "Northern America" ~ 0,
                             Area.Name == "Northern Europe" ~ 0,
                             Area.Name == "Others" ~ 0,
                             Area.Name == "Polynesia" ~ 0,
                             Area.Name == "South-Eastern Asia" ~ 0,
                             Area.Name == "South America" ~ 0,
                             Area.Name == "Southern Africa" ~ 0,
                             Area.Name == "Southern Asia" ~ 0,
                             Area.Name == "Southern Europe" ~ 0,
                             Area.Name == "Western Africa" ~ 0,
                             Area.Name == "Western Asia" ~ 0, 
                             Area.Name == "Western Europe" ~ 0,
                               TRUE ~ 0))

ggplot() +
  geom_sf(data = world_map_data, fill = land_color, size = .4) +
  geom_scatterpie(aes(x=long, y=lat, group = Area.Name, r = sqrt(total_catch/10000)), 
                  data = forage.data.mean.wide, legend_name = "Species",
                  cols = colnames(forage.data.mean.wide[,c(2:max_obs)])) +
  mytheme +
  theme(legend.position = "bottom") + 
  geom_scatterpie_legend(sqrt(forage.data.mean.wide$total_catch/10000), x=-160, y=-40, labeller=function(x) (x/10)^2) +
  geom_text_repel(aes(x=long, y=lat, group = Area.Name, label = Area.Name), 
                  data = forage.data.mean.wide, segment.color = "#333333",
                  nudge_x = forage.data.mean.wide$x_nudge, 
                  nudge_y = forage.data.mean.wide$y_nudge)

