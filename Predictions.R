if (!require('tidyverse')) install.packages('tidyverse'); library('tidyverse')

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
  mutate(Transect = recode(Transect, "Bud Light" = "Bud Lite")) %>%
  unite(S_T, Transect, XS, sep="")
watershed_attr$S_T <- cleanup(watershed_attr$S_T)
levels(factor(watershed_attr$S_T))

watershed_attr <- watershed_attr %>%
  mutate(S_T = recode(S_T, "donnies placertrib" = "donnies place r trib")) %>%
  mutate(S_T = recode(S_T, "donnies placeltrib" = "donnies place l trib")) %>%
  mutate(S_T = recode(S_T, "depoycxn_xs11" = "depoy_xs11")) %>%
  mutate(S_T = recode(S_T, "depoycxn_h3" = "depoy#_3"))
levels(factor(watershed_attr$S_T))

#Import and corrections to pour points
attr_at_pour_points<-read.csv("Data/GIS_transects_as_pour_points.csv")%>%
  select(-FID) %>%
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
  mutate(S_T = recode(S_T, "yancey_cd ds" = "yancey_cd"))
  
  levels(factor(attr_at_pour_points$S_T))

#Import and correct channel attributes
attr_on_channel<-read.csv("Data/GIS_all_transects_on_channel.csv")%>%
  select(-FID) %>%
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
  mutate(S_T = recode(S_T, "bruce_cd" = "bruce_henhouse"))

levels(factor(attr_on_channel$S_T))

#Import Tile Accumulations
tile_distance_thalweg<-read.csv("Data/Clean_tile_dist_from_thalweg.csv") 

tile_distance_thalweg$SiteXS <- cleanup(tile_distance_thalweg$SiteXS)
levels(factor(tile_distance_thalweg$SiteXS))

#Import Cross-sectional Surveys
XS_Change<-read.csv("Data/All_Change.csv")
XS_Change$Site <- cleanup(XS_Change$Site)
XS_Change$Transect <- cleanup(XS_Change$Transect)

levels(factor(XS_Change$Site))
XS_Change <- XS_Change %>%
                unite(S_T,Site,Transect) %>%
  mutate(S_T = recode(S_T, "floreen_us" = "floreen_ab")) %>%
  mutate(S_T = recode(S_T, "floreen_ds" = "floreen_cd")) %>%
  mutate(S_T = recode(S_T, "depoy_1" = "depoy#_1")) %>%
  mutate(S_T = recode(S_T, "depoy_2" = "depoy#_2")) %>%
  mutate(S_T = recode(S_T, "donnies place_l trib ds" = "donnies place l trib_ds")) %>%
  mutate(S_T = recode(S_T, "donnies place_l trib us" = "donnies place l trib_us")) %>%
  mutate(S_T = recode(S_T, "donnies place_r trib us" = "donnies place r trib_us"))%>%
  mutate(S_T = recode(S_T, "donnies place_ltrib ds xs 2 right" = "donnies place ltrib_ds xs 2 right"))%>%
  mutate(S_T = recode(S_T, "donnies place_ltrib ds xs 2 left" = "donnies place ltrib_ds xs 2 right"))
  
  
levels(factor(XS_Change$S_T))

#Import Bank Pins
Bank_Change <- read.csv("Data/Cleanedlastbank.csv") %>%
  mutate(SiteName = recode(SiteName, "Stoney Point Ln" = "stoney point ln")) %>%
  unite(S_T,SiteName,XS) %>%
  mutate(S_T = recode(S_T, "bart johnson_a-b us" = "bart johnson_ab")) 
Bank_Change$S_T <- cleanup(Bank_Change$S_T)
Bank_Change <- Bank_Change %>%
  mutate(S_T = recode(S_T, "bruce_ab us" = "bruce_ab")) %>%
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
  mutate(S_T = recode(S_T, "longs dry fork_ab us" = "longs dry fork_ab")) %>%
  mutate(S_T = recode(S_T, "sellers mill_ab us" = "sellers mill_ab")) %>%
  mutate(S_T = recode(S_T, "sellers mill_cd ds" = "sellers mill_cd")) %>%
  mutate(S_T = recode(S_T, "yancey_ab us" = "yancey_ab")) %>%
  mutate(S_T = recode(S_T, "yancey_cd ds" = "yancey_cd"))
  
  
levels(factor(Bank_Change$S_T))

############################################
#Compare factor levels to make sure of consistency
#Bank first
Bank_levels <- data.frame(unique(Bank_Change$S_T)) %>% 
  separate(unique.Bank_Change.S_T.,c("Site", "Transect"),"_") %>%
  mutate(Dataframe = c("bank_levels"))

XS_levels <- data.frame(unique(XS_Change$S_T)) %>% 
  separate(unique.XS_Change.S_T.,c("Site", "Transect"),"_") %>%
  mutate(Dataframe = c("xs_levels"))

tile_levels <- data.frame(unique(tile_distance_thalweg$SiteXS)) %>% 
  separate(unique.tile_distance_thalweg.SiteXS.,c("Site", "Transect"),"_") %>%
  mutate(Dataframe = c("tile_levels"))

channel_levels <- data.frame(unique(attr_on_channel$S_T)) %>% 
  separate(unique.attr_on_channel.S_T.,c("Site", "Transect"),"_") %>%
  mutate(Dataframe = c("channel_levels"))

pour_levels <- data.frame(unique(attr_at_pour_points$S_T)) %>% 
  separate(unique.attr_at_pour_points.S_T.,c("Site", "Transect"),"_") %>%
  mutate(Dataframe = c("pour_levels"))

watershed_levels <- data.frame(unique(watershed_attr$S_T)) %>% 
  separate(unique.watershed_attr.S_T.,c("Site", "Transect"),"_") %>%
  mutate(Dataframe = c("watershed_levels"))

#Test Comparing Bank and XS level names
full_join(XS_levels,watershed_levels, by = c("Site", "Transect")) %>%
  arrange(Site)


#Merge Data
XS_Change <- XS_Change %>%
  separate(S_T, c("Site","Transect"), "_")

watershed_attr <- watershed_attr %>%
  separate(S_T, c("Site","Transect"), "_")

XS_Change_ws <- left_join(XS_Change, watershed_attr, by = c("Site"))


#Prediction Analysis----

#Linear model Tree
XS_ws <- XS_Change %>%
  group_by(Site,Transect,Class) %>%
  summarise_at(c("scour.rate","fill.rate","net.rate"), mean) %>%
  inner_join(watershed_attr, by = c("Site"))

lmdata <- XS_ws %>%
  select(-Id,-Transect.y,-gridcode,-S100MIN,-S25MIN,-Rowid_, -SUM:-GRIDCODE_2) %>%
  filter(Class == "Bank") 

lmfit<-lm(log(net.rate+2)~contr_area+Strahler+sum(pcent_imp+p_dev_med+p_dev_lo+p_dev_os), data=lmdata[,c(-1:-5)])
summary(lmfit)


lmfit<-lm(log(net.rate+5)~., data=lmdata[,c(-1:-5)])
lmfit
summary(lmfit)
stepfit<-step(lmfit)
summary(stepfit)

#PCA and PCR regressions 
require(pls)
set.seed (1000)

pcr_model <- pcr(net.rate~., data = lmdata[,c(-1:-5)], scale = TRUE, validation = "CV")
summary(pcr_model)
validationplot(pcr_model)
validationplot(pcr_model, val.type="MSEP")# Plot the cross validation MSE
validationplot(pcr_model, val.type = "R2")# Plot the R2
coefplot(pcr_model)

# Train-test split
    train <- lmdata[1:120,]
    y_test <- lmdata[120:150, 1]
    test <- lmdata[120:150, 2:5]
    pcr_model <- pcr(Sepal.Length~., data = train, scale =TRUE, validation = "CV")
    
    pcr_pred <- predict(pcr_model, test, ncomp = 4)
    mean((pcr_pred - y_test)^2)


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
