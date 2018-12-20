#Starting Functions
rm(list=ls())

if (!require('pacman')) install.packages('pacman'); library('pacman')
p_load(tidyverse)

#Name Cleanup function
cleanup<-function(x){
  x <- tolower(x)
  x <- gsub("-","",x)
  x <- gsub("'","",x)
  x
}


#Import Data ----
root.dir = getwd()

##Load in GIS attribute data
###Importing Watershed Attributes
watershed_attr<-read.csv(paste0(root.dir,"/Data/GIS_TransectWatershedAttr.csv")) %>%
  mutate(Transect = recode(Transect, "Bud Light" = "Bud Lite")) %>%
  unite(S_T, Transect, XS, sep="")
watershed_attr$S_T <- cleanup(watershed_attr$S_T)
levels(factor(watershed_attr$S_T))
watershed_attr <- watershed_attr %>%
  mutate(S_T = recode(S_T, "donnies placertrib" = "donnies place r trib")) %>%
  mutate(S_T = recode(S_T, "donnies placeltrib" = "donnies place l trib")) %>%
  mutate(S_T = recode(S_T, "stoney point lane" = "stoney point ln")) %>%
  mutate(S_T = recode(S_T, "depoycxn_xs11" = "depoy_xs11")) %>%
  mutate(S_T = recode(S_T, "depoycxn_h3" = "depoy#_3")) %>%
  mutate(p_dev_sum = (p_dev_os+p_dev_lo+p_dev_med+p_dev_high)) %>%
  mutate(p_forest_sum = (p_decid_forest+p_everg_forest+p_mixed_forest)) %>%
  mutate(p_ag_sum = (p_hay+p_crops)) %>%
  mutate(p_herbshrub = (p_herb+p_shrub))

levels(factor(watershed_attr$S_T))


###Importing pour points
attr_at_pour_points<-read.csv(paste0(root.dir,"/Data/GIS_transects_as_pour_points.csv")) %>%
  mutate(SiteID = recode(SiteID, "Bud Light" = "Bud Lite")) %>%
  unite(S_T, SiteID, XS)
attr_at_pour_points$S_T <- cleanup(attr_at_pour_points$S_T)
levels(factor(attr_at_pour_points$S_T))

attr_at_pour_points <- attr_at_pour_points %>%
  mutate(S_T = recode(S_T, "bruce_br6" = "bruce_#6")) %>%
  mutate(S_T = recode(S_T, "chicken reach_cd ds" = "chicken reach_cd")) %>%
  mutate(S_T = recode(S_T, "clicks lane_t1 most ds" = "clicks lane_t1")) %>%
  mutate(S_T = recode(S_T, "cousins house_cd ds" = "cousins house_cd")) %>%
  mutate(S_T = recode(S_T, "depoy_h3" = "depoy#_3")) %>%
  mutate(S_T = recode(S_T, "deskins mill_cd ds" = "deskins mill_cd")) %>%
  mutate(S_T = recode(S_T, "donnies place_l&rtrib xs2 ds" = "donnies place l trib_ds xs 2")) %>%
  mutate(S_T = recode(S_T, "donnies place_rtrib xs2 ds" = "donnies place r trib_ds xs 2")) %>%
  mutate(S_T = recode(S_T, "floreen_ab us" = "floreen_ab")) %>%
  mutate(S_T = recode(S_T, "gage_ef ds" = "gage_ef")) %>%
  mutate(S_T = recode(S_T, "grace_ef ds" = "grace_ef")) %>%
  mutate(S_T = recode(S_T, "hippie farm_cd ds" = "hippie farm_cd")) %>%
  mutate(S_T = recode(S_T, "longs dry fork_ef shooting slope" = "longs dry fork_ef")) %>%
  mutate(S_T = recode(S_T, "md lady_ds" = "md lady_xs2 ds")) %>%
  mutate(S_T = recode(S_T, "sellers mill_cd ds" = "sellers mill_cd")) %>%
  mutate(S_T = recode(S_T, "shomo farm_xs1 2nd order" = "shomo farm_xs2 2nd order")) %>%
  mutate(S_T = recode(S_T, "yancey_cd ds" = "yancey_cd")) %>%
  mutate(S_T = recode(S_T, "stoney point lane_all" = "stoney point ln_all"))

levels(factor(attr_at_pour_points$S_T))


###Importing channel attributes
attr_on_channel<-read.csv(paste0(root.dir,"/Data/GIS_all_transects_on_channel.csv")) %>%
  mutate(SiteID = recode(SiteID, "Bud Light" = "Bud Lite")) %>%
  unite(S_T, SiteID, XS)
attr_on_channel$S_T <- cleanup(attr_on_channel$S_T)

attr_on_channel <- attr_on_channel %>%
  mutate(S_T = recode(S_T, "floreen_us" = "floreen_ab")) %>%
  mutate(S_T = recode(S_T, "floreen_ds" = "floreen_cd")) %>%
  mutate(S_T = recode(S_T, "md lady_ds" = "md lady_xs2 ds")) %>%
  mutate(S_T = recode(S_T, "bruce_br1" = "bruce_#1")) %>%
  mutate(S_T = recode(S_T, "bruce_br2" = "bruce_#2")) %>%
  mutate(S_T = recode(S_T, "bruce_br3" = "bruce_#3")) %>%
  mutate(S_T = recode(S_T, "bruce_br4" = "bruce_#4")) %>%
  mutate(S_T = recode(S_T, "bruce_br5" = "bruce_#5")) %>%
  mutate(S_T = recode(S_T, "bruce_br6" = "bruce_#6")) %>%
  mutate(S_T = recode(S_T, "depoy_1" = "depoy#_1")) %>%
  mutate(S_T = recode(S_T, "depoy_2" = "depoy#_2")) %>%
  mutate(S_T = recode(S_T, "depoy_3" = "depoy#_3")) %>%
  mutate(S_T = recode(S_T, "stoney point lane_all" = "stoney point ln_all")) %>%
  mutate(S_T = recode(S_T, "donnies place_l trib ds" = "donnies place l trib_ds")) %>%
  mutate(S_T = recode(S_T, "donnies place_l trib us" = "donnies place l trib_us")) %>%
  mutate(S_T = recode(S_T, "donnies place_ltrib ds xs 2 left" = "donnies place l trib_ds xs 2 left")) %>%
  mutate(S_T = recode(S_T, "donnies place_r trib us" = "donnies place r trib_us")) %>%
  mutate(S_T = recode(S_T, "donnies place_rtrib xs2 ds" = "donnies place r trib_xs2 ds")) %>%
  mutate(S_T = recode(S_T, "bruce_cd" = "bruce_henhouse"))

replace1<- attr_on_channel %>%
  filter(S_T == "stultz mill_all") %>% 
  slice(rep(1:n(), each = 2))
replace1$S_T<-c("stultz mill_xs1 ds","stultz mill_xs2 us")

replace2<- attr_on_channel %>%
  filter(S_T == "stoney point ln_all") %>% 
  slice(rep(1:n(), each = 2))
replace2$S_T<-c("stoney point ln_xs1 ds","stoney point ln_xs2 ds")

replace3<- attr_on_channel %>%
  filter(S_T == "henton mill_all") %>% 
  slice(rep(1:n(), each = 2))
replace3$S_T<-c("henton mill_xs1 us","henton mill_xs2 ds")

replace<-rbind(replace1, replace2, replace3)
attr_on_channel <- attr_on_channel %>%
  rbind(replace) %>%
  filter(S_T != "stultz mill_all") %>%
  filter(S_T != "henton mill_all") %>%
  filter(S_T != "stony point ln_all")

rm(replace1, replace2, replace3, replace)
levels(factor(attr_on_channel$S_T))


##Importing Field Data
###Importing Tile Accumulations
#Import Tile Accumulations
tile_distance_thalweg<-read.csv(paste0(root.dir,"/Data/Clean_tile_dist_from_thalweg.csv")) 

tile_distance_thalweg$SiteXS <- cleanup(tile_distance_thalweg$SiteXS)

tile_distance_thalweg<- tile_distance_thalweg %>%
  mutate(SiteXS = recode(SiteXS, "donnies place_ltrib ds xs 2 left" = "donnies place l trib_ds xs 2 left")) %>%
  mutate(SiteXS = recode(SiteXS, "donnies place_ltrib ds xs 2 right" = "donnies place l trib_ds xs 2 right")) 

levels(factor(tile_distance_thalweg$SiteXS))


###Importing Cross-Sectional Surveys
#Import Cross-sectional Surveys
XS_Change<-read.csv(paste0(root.dir,"/Data/All_Change.csv")) %>%
  select(-X)
XS_Change$Site <- cleanup(XS_Change$Site)
XS_Change$Transect <- cleanup(XS_Change$Transect)

levels(factor(XS_Change$Site))
XS_Change <- XS_Change %>%
  unite(S_T,Site,Transect) %>%
  mutate(S_T = recode(S_T, "floreen_us" = "floreen_ab")) %>%
  mutate(S_T = recode(S_T, "floreen_ds" = "floreen_cd")) %>%
  mutate(S_T = recode(S_T, "depoy_1" = "depoy#_1")) %>%
  mutate(S_T = recode(S_T, "depoy_2" = "depoy#_2")) %>%
  mutate(S_T = recode(S_T, "depoy_3" = "depoy#_3")) %>%
  mutate(S_T = recode(S_T, "donnies place_l trib ds" = "donnies place l trib_ds")) %>%
  mutate(S_T = recode(S_T, "donnies place_l trib us" = "donnies place l trib_us")) %>%
  mutate(S_T = recode(S_T, "donnies place_r trib us" = "donnies place r trib_us"))%>%
  mutate(S_T = recode(S_T, "donnies place_ltrib ds xs 2 right" = "donnies place l trib_ds xs 2 right"))%>%
  mutate(S_T = recode(S_T, "donnies place_ltrib ds xs 2 left" = "donnies place l trib_ds xs 2 left"))


levels(factor(XS_Change$S_T))

###Importing Bank Pins
Bank_Change <- read.csv(paste0(root.dir,"/Data/Cleanedlastbank.csv")) %>%
  mutate(SiteName = recode(SiteName, "Stoney Point Ln" = "stoney point ln")) %>%
  unite(S_T,SiteName,XS) %>%
  mutate(S_T = recode(S_T, "bart johnson_a-b us" = "bart johnson_ab")) 
Bank_Change$S_T <- cleanup(Bank_Change$S_T)
Bank_Change <- Bank_Change %>%
  mutate(S_T = recode(S_T, "bruce_ab us" = "bruce_ab")) %>%
  mutate(S_T = recode(S_T, "bruce_br2" = "bruce_#2")) %>%
  mutate(S_T = recode(S_T, "bruce_middle" = "bruce_henhouse")) %>%
  mutate(S_T = recode(S_T, "chicken reach_cd ds" = "chicken reach_cd")) %>%
  mutate(S_T = recode(S_T, "clicks lane_t1 most ds" = "clicks lane_t1")) %>%
  mutate(S_T = recode(S_T, "clicks lane_t3 most us" = "clicks lane_t3")) %>%
  mutate(S_T = recode(S_T, "cousins house_ab us" = "cousins house_ab")) %>%
  mutate(S_T = recode(S_T, "cousins house_cd ds" = "cousins house_cd")) %>%
  mutate(S_T = recode(S_T, "deskins mill_ab us" = "deskins mill_ab")) %>%
  mutate(S_T = recode(S_T, "deskins mill_cd ds" = "deskins mill_cd")) %>%
  mutate(S_T = recode(S_T, "floreen_ab us" = "floreen_ab")) %>%
  mutate(S_T = recode(S_T, "floreen_cd ds" = "floreen_cd")) %>%
  mutate(S_T = recode(S_T, "gage_ab us" = "gage_ab")) %>%
  mutate(S_T = recode(S_T, "gage_cd ds" = "gage_cd")) %>%
  mutate(S_T = recode(S_T, "gage_ef ds" = "gage_ef")) %>%
  mutate(S_T = recode(S_T, "grace_ab us" = "grace_ab")) %>%
  mutate(S_T = recode(S_T, "grace_cd ds" = "grace_cd")) %>%
  mutate(S_T = recode(S_T, "grace_ef ds" = "grace_ef")) %>%
  mutate(S_T = recode(S_T, "hippie farm_ab us" = "hippie farm_ab")) %>%
  mutate(S_T = recode(S_T, "hippie farm_cd ds" = "hippie farm_cd")) %>%
  mutate(S_T = recode(S_T, "indian trail_xs1 us" = "indian trail_us")) %>%
  mutate(S_T = recode(S_T, "indian trail_xs2 ds" = "indian trail_ds")) %>%
  mutate(S_T = recode(S_T, "longs dry fork_ab us" = "longs dry fork_ab")) %>%
  mutate(S_T = recode(S_T, "p1_xs2 ds" = "p1_cd")) %>%
  mutate(S_T = recode(S_T, "sellers mill_ab us" = "sellers mill_ab")) %>%
  mutate(S_T = recode(S_T, "sellers mill_cd ds" = "sellers mill_cd")) %>%
  mutate(S_T = recode(S_T, "yancey_ab us" = "yancey_ab")) %>%
  mutate(S_T = recode(S_T, "yancey_cd ds" = "yancey_cd")) %>%
  mutate(S_T = recode(S_T, "donnies place_l&rtrib xs2 ds" = "donnies place l&r trib_xs2 ds")) %>%
  mutate(S_T = recode(S_T, "donnies place_ltrib xs1 us" = "donnies place l trib_us")) %>%
  mutate(S_T = recode(S_T, "donnies place_ltrib xs2 ds" = "donnies place l trib_ds")) %>%
  mutate(S_T = recode(S_T, "donnies place_rtrib us" = "donnies place r trib_us")) %>%
  mutate(S_T = recode(S_T, "donnies place_rtrib xs2 ds" = "donnies place r trib_xs2 ds"))

levels(factor(Bank_Change$S_T))

##importing Field secondary calculations 
field_attributes <- read.csv(paste0(root.dir,"/Data/field_attributes.csv")) %>%
  select(-unique_site) %>%
  unite(S_T, c("SiteID","TransectID"))

field_attributes$S_T <- cleanup(field_attributes$S_T)

field_attributes <- field_attributes %>%
  mutate(S_T = recode(S_T, "donnies place_l trib ds" = "donnies place l trib_ds")) %>%
  mutate(S_T = recode(S_T, "donnies place_l trib us" = "donnies place l trib_us")) %>%
  mutate(S_T = recode(S_T, "donnies place_ltrib ds xs 2 left" = "donnies place l trib_ds xs 2 left")) %>%
  mutate(S_T = recode(S_T, "donnies place_ltrib ds xs 2 right" = "donnies place l trib_ds xs 2 right")) %>%
  mutate(S_T = recode(S_T, "donnies place_r trib us" = "donnies place r trib_us")) %>%
  mutate(S_T = recode(S_T, "depoy_1" = "depoy#_1")) %>%
  mutate(S_T = recode(S_T, "depoy_2" = "depoy#_2")) %>%
  mutate(S_T = recode(S_T, "depoy_3" = "depoy#_3")) 
  
#Compare factor levels to make sure data align ----
##Extracting Site and Transect Layers
# Bank_levels <- data.frame(unique(Bank_Change$S_T)) %>% 
#   separate(unique.Bank_Change.S_T.,c("Site", "Transect"),"_") %>%
#   mutate(Dataframe = c("bank_levels"))
# 
# XS_levels <- data.frame(unique(XS_Change$S_T)) %>% 
#   separate(unique.XS_Change.S_T.,c("Site", "Transect"),"_") %>%
#   mutate(Dataframe = c("xs_levels"))
# 
# tile_levels <- data.frame(unique(tile_distance_thalweg$SiteXS)) %>% 
#   separate(unique.tile_distance_thalweg.SiteXS.,c("Site", "Transect"),"_") %>%
#   mutate(Dataframe = c("tile_levels"))
# 
# channel_levels <- data.frame(unique(attr_on_channel$S_T)) %>% 
#   separate(unique.attr_on_channel.S_T.,c("Site", "Transect"),"_") %>%
#   mutate(Dataframe = c("channel_levels"))
# 
# pour_levels <- data.frame(unique(attr_at_pour_points$S_T)) %>% 
#   separate(unique.attr_at_pour_points.S_T.,c("Site", "Transect"),"_") %>%
#   mutate(Dataframe = c("pour_levels"))
# 
# watershed_levels <- data.frame(unique(watershed_attr$S_T)) %>% 
#   separate(unique.watershed_attr.S_T.,c("Site", "Transect"),"_") %>%
#   mutate(Dataframe = c("watershed_levels"))
# 
# 
# ##Generate Table of levels for ALL Field And GIS Layers
# full_join(tile_levels,XS_levels, by = c("Site", "Transect")) %>%
#   full_join(Bank_levels, by = c("Site","Transect")) %>%
#   full_join(channel_levels, by = c("Site","Transect")) %>%
#   full_join(pour_levels, by = c("Site","Transect")) %>%
#   full_join(watershed_levels, by = c("Site","Transect")) %>%
#   arrange(Site, Transect)
# 
# #XS and GIS layers look good (but missing Channel_levels for one location - MD layer $$$ Ask Zach!)
# 
# 
# ###Levels Cleanup
# rm(Bank_levels, channel_levels, pour_levels, tile_levels, watershed_levels, XS_levels)
# 


#Merge datasets according to Site and Transect level attributes ----
##- Attributes at pour points and at watershed are only 1 value per site
##- Channel attributes should match up for each site location 
XS_Change <- XS_Change %>%
  separate(S_T, c("Site", "Transect"), sep = "_")
Bank_Change <- Bank_Change %>%
  separate(S_T, c("Site", "Transect"), sep = "_")
tile_distance_thalweg <- tile_distance_thalweg %>%
  separate(SiteXS, c("Site", "Transect"), sep = "_")

watershed_attr <- watershed_attr %>%
  separate(S_T, c("Site", "Transect"), sep = "_") %>%
  select(-Transect)

attr_at_pour_points <- attr_at_pour_points %>%
  separate(S_T, c("Site", "Transect"), sep = "_") %>%
  select(-Transect)

attr_on_channel <- attr_on_channel %>%
  separate(S_T, c("Site", "Transect"), sep = "_")

field_attributes <- field_attributes %>%
  separate(S_T, c("Site", "Transect"), sep = "_")

field_attributes_site_avg <- field_attributes %>%
  group_by(Site) %>%
  na.omit() %>%
  summarise_at(c("Bank_Angle","bankheight","channelwidth","floodwidth","channelflood_ratio"), mean)

  
  
XS_ws_site <- XS_Change %>% #Calculating mean based on Site and average of Left and Right sides of Channel an
  group_by(Site,Class) %>%
  summarise_at(c("scour.rate","fill.rate","net.rate"), mean)  %>%
  left_join(attr_at_pour_points, by = c("Site")) %>%
  left_join(field_attributes_site_avg, by = c("Site")) %>%
  left_join(watershed_attr, by = c("Site")) 

tile_ws_site <- tile_distance_thalweg %>% #No mean calculation yet due to possible variability in explanations
  group_by(Site,Transect) %>%
  summarise_at("deposit_mm_yr", mean) %>%
  left_join(attr_at_pour_points, by = c("Site")) %>%
  left_join(field_attributes, by = c("Site", "Transect")) %>%
  left_join(watershed_attr, by = c("Site")) 

Bank_ws_site <- Bank_Change %>% #Calculating mean bank change for locations
  select(-Avg_Max_DFT,-NormCumDiffarea_cm2_per_day,-NormDiffarea_cm2_per_day,-X) %>%
  na.omit() %>%
  group_by(Site) %>%
  summarise_at(c("Avg_NormDiff_cm_per_day","Avg_NormCumDiff_cm_per_day"), mean) %>%
  left_join(attr_at_pour_points, by = c("Site")) %>%
  left_join(field_attributes_site_avg, by = c("Site" )) %>%
  left_join(watershed_attr, by = c("Site"))  
  



XS_ws_transect <- XS_Change %>%
  group_by(Site,Transect, Class) %>%
  summarise_at(c("scour.rate","fill.rate","net.rate"), mean) %>%
  left_join(attr_on_channel, by = c("Site","Transect")) %>%
  left_join(field_attributes, by = c("Site", "Transect" )) %>%
  left_join(watershed_attr, by = c("Site")) 

tile_ws_transect <- tile_distance_thalweg %>% #No mean calculation yet due to possible variability in explanations
  left_join(attr_on_channel, by = c("Site","Transect")) %>%
  left_join(field_attributes, by = c("Site", "Transect")) %>%
  left_join(watershed_attr, by = c("Site")) 

Bank_ws_transect <- Bank_Change %>%
  select(-Avg_Max_DFT,-NormCumDiffarea_cm2_per_day,-NormDiffarea_cm2_per_day,-X) %>%
  na.omit() %>%
  group_by(Site, Transect) %>%
  summarise_at(c("Avg_NormDiff_cm_per_day","Avg_NormCumDiff_cm_per_day"), mean) %>%
  left_join(attr_on_channel, by = c("Site","Transect")) %>%
  left_join(watershed_attr, by = c("Site")) 

#Cleanup ----
rm(attr_at_pour_points,attr_on_channel,Bank_Change,tile_distance_thalweg, watershed_attr, XS_Change, field_attributes, field_attributes_site_avg)
