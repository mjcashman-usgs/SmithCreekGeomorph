if (!require('tidyverse')) install.packages('tidyverse'); library('tidyverse')
if (!require('readxl')) install.packages('readxl'); library('readxl')

rm(list=ls())

#Load in Data exported from previous scripts----

#Name Cleanup function
cleanup<-function(x){
  x <- tolower(x)
  x <- gsub("-","",x)
  x <- gsub("'","",x)
  x
}

#Load in GIS attribute data
watershed_attr<-read.csv("Data/GIS_TransectWatershedAttr.csv") %>%
  select(-FID) %>%
  mutate(Transect = recode(Transect, "Bud Light" = "Bud Lite"))
watershed_attr$Transect <- cleanup(watershed_attr$Transect)
levels(factor(watershed_attr$Transect))

attr_at_pour_points<-read.csv("Data/GIS_transects_as_pour_points.csv")%>%
  select(-FID) %>%
  mutate(SiteID = recode(SiteID, "Bud Light" = "Bud Lite"))
attr_at_pour_points$SiteID <- cleanup(attr_at_pour_points$SiteID)
levels(factor(attr_at_pour_points$SiteID))

attr_on_channel<-read.csv("Data/GIS_all_transects_on_channel.csv")%>%
  select(-FID) %>%
  mutate(SiteID = recode(SiteID, "Bud Light" = "Bud Lite"))
attr_on_channel$SiteID <- cleanup(attr_on_channel$SiteID)
levels(factor(attr_on_channel$SiteID))

#Import Tile Accumulations
tile_distance_thalweg<-read.csv("Data/Clean_tile_dist_from_thalweg.csv") %>%
  separate("SiteXS",c("SiteID","TransectID"), sep = "_") 
  
tile_distance_thalweg$SiteID <- cleanup(tile_distance_thalweg$SiteID)
levels(factor(tile_distance_thalweg$SiteID))

#Import Cross-sectional Surveys
XS_Change<-read.csv("Data/All_Change.csv")
XS_Change$Site <- cleanup(XS_Change$Site)
levels(factor(XS_Change$Site))


#Import Bank Pins
Bank_Change <- read.csv("Data/Cleanedlastbank.csv") %>%
  mutate(SiteName = recode(SiteName, "Stoney Point Ln" = "Stoney Point Lane"))
Bank_Change$SiteName <- cleanup(Bank_Change$SiteName)
Bank_Change <- Bank_Change %>%
  mutate(SiteName = recode(SiteName, "stoney point lane" = "stoney point ln"))
levels(factor(Bank_Change$SiteName))

############################################


#Merge Data
#Cross Section
XS_Change_ws <- inner_join(XS_Change, watershed_attr, by = c("Site"="Transect"))
XS_Change_wsch <- left_join(XS_Change_ws,attr_on_channel , by = c("Site"="SiteID"))

#Floodplain Tiles
tile_Change_ws <- inner_join(tile_distance_thalweg, watershed_attr, by = c("SiteID"="Transect"))
tile_Change_wsch <- left_join(tile_distance_thalweg,attr_on_channel , by = c("SiteID"="SiteID"))

#Bank pins
Bank_Change_ws <- anti_join(Bank_Change, watershed_attr, by = c("SiteName"="Transect"))
Bank_Change_wsch <- inner_join(Bank_Change_ws, watershed_attr, by = c("SiteName"="Transect"))

#Prediction Analysis----

#Linear model Tree
lmdata <- XS_Change_ws %>%
  filter(Class=="Floodplain") %>%
  select(-X:-fill.rate,-Id,-XS) 

lmfit<-lm(net.rate~., data=lmdata)
lmfit
summary(lmfit)

stepfit<-step(lmfit)
summary(stepfit)

#Boosted Regression Tree
if (!require('gbm')) install.packages('gbm'); library('gbm')

gbmdata <- XS_Change_ws %>%
  filter(Class=="Bank") %>%
  select(-X:-fill.rate,-Id,-XS) 

gbmModel <- gbm(net.rate~., data=gbmdata, distribution = "gaussian",
                interaction.depth=2,
                n.minobsinnode = 2,
                bag.fraction=1.0,
                n.trees = 50000,
                cv.folds=3)

gbmModel
summary(gbmModel)

#train
gbmGrid <- expand.grid(.interaction.depth = seq(1,7, by=2),
                       .n.trees = seq(100,1000, by = 50),
                       .shrinkage = c(0.01,0.1))
set.seed(100)
