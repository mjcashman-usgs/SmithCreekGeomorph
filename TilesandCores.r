#setwd("Z:/Files/R_Projects/SmithCreekGeomorph")

####IMPORT DATA----
if (!require('tidyverse')) install.packages('tidyverse'); library('tidyverse')
if (!require('readxl')) install.packages('readxl'); library('readxl')
  
  #BankBar
  BankBar<-read_excel("Data/TilesandCores.xlsx")%>%
    unite_("SiteXS",c("SiteName","XS")) %>%
    drop_na(Station_m)
 
  #CrossSection Data
  CrossSections<-read_excel("Data/SmithCreekCrossSections.xlsx") %>%
    unite_("SiteXS",c("SiteID","TransectID")) %>%
    select(-X__1,-X__2)
  
  thalwegs<-CrossSections %>%
    group_by(SiteXS,DateandTime)  %>%
      slice(which.min(FinalElevation_m))
  
  CrossSections1 <- CrossSections %>%
    mutate(Station_m = round(FinalStation_m,1))
      
 #Merge and select
  thalwegs1 <- thalwegs %>% 
    select(NameofRiver,SiteXS,DateandTime,FinalStation_m,FinalElevation_m) %>%
    group_by(SiteXS) %>%
    rename(thalweg_station = FinalStation_m) %>%
    rename(thalweg_elev = FinalElevation_m) %>%
    mutate(LastDateandTime = max(DateandTime)) %>%
    mutate(FirstDateandTime = min(DateandTime)) %>%
    filter(DateandTime == max(DateandTime))
  
  Tiles <- BankBar %>% 
    select(SiteXS,Bank,Date,Station_m,Thickness_mm,Tile_width_cm,Tile_length_cm)

  Tile_height <- inner_join(thalwegs1,Tiles, by = "SiteXS")  %>%
    mutate(Dist_from_thalweg = abs(Station_m - thalweg_station)) %>%
    mutate(Station_m = round(Station_m,1))
  
  CrossSections2<-CrossSections1 %>%
    select(SiteXS, Station_m, FinalElevation_m)
  
  Tile_distance_height <- inner_join(Tile_height, CrossSections2, by = c("SiteXS","Station_m")) %>%
    rename(tile_elev = FinalElevation_m) %>%
    rename(tile_station = Station_m) %>%
    mutate(elev_above_thalweg = abs(tile_elev - thalweg_elev)) %>%
    mutate(diff_time = LastDateandTime - FirstDateandTime) %>%
    mutate(deposit_mm_yr = 365.25*(Thickness_mm/as.double(diff_time, units='days')))
  
  Clean_tile <- Tile_distance_height %>%
    select(NameofRiver,SiteXS,Bank,Thickness_mm,Dist_from_thalweg,elev_above_thalweg,diff_time,deposit_mm_yr) %>%
    na.omit()
  
write.csv(Clean_tile, "Data/Clean_tile_dist_from_thalweg.csv")

  ####Plot DATA----
  library(lme4)
  fit<-lm(sqrt(deposit_mm_yr)~sqrt(Dist_from_thalweg)*sqrt(elev_above_thalweg)+Bank+SiteXS, data=Clean_tile)
  summary(fit)
  library(car)
  Anova(fit)
  
  #plot interactions
  library(sjPlot) 
  sjp.int(fit, swap.pred = T)
  
  #Check normality
  hist((Clean_tile$diff_time))
  
  #Observed vs predicted
plot(predict(fit),Clean_tile$deposit_mm_yr)
abline(a=0,b=1)




  #Plot raw bank data
  Tile_distance_height %>%
  #  filter(SiteName == CurrentSite) %>%
    ggplot() +
      geom_point(aes(x=Dist_from_thalweg, y=Thickness_mm))+
      facet_wrap(~SiteXS) +
      ggtitle(paste("Distance from Thalweg"))
  
  Tile_distance_height %>%
  ggplot() +
    geom_point(aes(x=elev_above_thalweg, y=Thickness_mm))+
    facet_wrap(~SiteXS, scales="free") +
    ggtitle(paste("Height above Thalweg"))
