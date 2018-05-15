#Order Check
check<-data.unnest %>% group_by(unique_site) %>%   
  summarize(first=first(DateandTime),last=last(DateandTime),diff=first(DateandTime)-last(DateandTime))

check<- all_transect %>%group_by(transect,Class,Bank) %>% summarize(diff.first = first(Segment)-first(no.intersections),diff.last = last(Segment)-last(no.intersections),no.intersections=mean(no.intersections))


getwd()
setwd("Z:/Files/R_Projects/Smith Creek")

#IMPORT DATA
CrossSections<-read.csv("SmithCreekCrossSections.csv")
####Data Pre-Processing####

#Rivers
unique(CrossSections$NameofRiver)
library(plyr)
CrossSections$NameofRiver<-revalue(CrossSections$NameofRiver, c("SmithCreek"="Smith Creek",
                                                                "Smith Creek "="Smith Creek"))
unique(CrossSections$NameofRiver)

#SiteID
unique(CrossSections$SiteID) 
CrossSections$SiteID<-revalue(CrossSections$SiteID, c("Bruce "="Bruce",
                                                      "Cousins Backyard "="Cousins Backyard",
                                                      "Cousin's Backyard"="Cousins Backyard",
                                                      "Hippie Farms"="Hippie Farm",
                                                      "P-1 "="P-1"))
unique(CrossSections$SiteID)

#Point Type
unique(CrossSections$PointType) 
CrossSections$PointType<-revalue(CrossSections$PointType, c("Cross Section "="Cross Section",
                                                            "Backsite Point "="Backsite Point",
                                                            "Extra Cross Section "="Extra Cross Section"))
unique(CrossSections$PointType)
CrossSections<-subset(CrossSections,PointType=="Cross Section")

#TransectID
unique(CrossSections$TransectID)
detach("package:plyr", unload=TRUE)

#Split commas into multiple rows
data<-CrossSections
library(tidyr)
library(dplyr)
data.unnest<-data %>% 
  mutate(ChannelFeature = strsplit(as.character(ChannelFeature), ",")) %>% 
  unnest(ChannelFeature)

data.unnest<-data.unnest %>% 
  mutate(ChannelFeature = strsplit(as.character(ChannelFeature), "/")) %>% 
  unnest(ChannelFeature)


####Identification of all unique and nested locations of cross-sections####
uniqueXS_count<-data.unnest %>%
  group_by(NameofRiver,SiteID,TransectID) %>% 
  summarise(SurveyCounts=length(unique(DateandTime)))
uniqueXS_count
write.csv(uniqueXS_count, file = "uniqueXS_count.csv")
data.unnest$unique_site<-paste0(data.unnest$NameofRiver,", ",data.unnest$SiteID,", ",data.unnest$TransectID)

#####Translation of LOcation Features to base without top/bottom
data.unnest$Class<-data.unnest$ChannelFeature

unique(data.unnest$Class)
data.unnest$Class[grepl("Bank",data.unnest$Class)]<-"Bank"
data.unnest$Class[grepl("Floodplain",data.unnest$Class)]<-"Floodplain"
data.unnest$Class[grepl("Bed",data.unnest$Class)]<-"Bed"
data.unnest$Class[grepl("Bar",data.unnest$Class)]<-"Bed"
data.unnest$Class[grepl("`",data.unnest$Class)]<-"Floodplain"
#new
data.unnest <- within(data.unnest, Bank[Bank == 'L' & Class == 'Bed'] <- 'C')
data.unnest <- within(data.unnest, Bank[Bank == 'R' & Class == 'Bed'] <- 'C')

unique(data.unnest$Class)


###Factor Reordering
#factor(data.unnest$Class)
data.unnest$Class = factor(data.unnest$Class,levels=c("Floodplain","Bank","Bed"))
levels(data.unnest$Class)

###Date Formating
data.unnest$DateandTime <-as.Date(data.unnest$DateandTime,"%d-%m-%y")

#####Manual Subsetting####

transect <- "Smith Creek, Bart Johnson, AB"

transect_subset<-subset(data.unnest, unique_site==transect)

####Plotting the Cross-sections####
library(ggplot2)  
transect_subset$Bank = factor(transect_subset$Bank, levels=c('L','C','R'))
transect_subset$Bank_Class<-paste0(transect_subset$Bank,"-",transect_subset$Class)
transect_subset$Bank_Class = factor(transect_subset$Bank_Class, levels=c('L-Floodplain','L-Bank','C-Bed','R-Bank','R-Floodplain'))
 
# plot<-ggplot(transect_subset, aes(x = FinalStation_m, y = FinalElevation_m, color=as.factor(DateandTime))) +
#   geom_point(aes(pch=Class),size=3)+geom_path()+
#   ggtitle(paste0(transect))+facet_grid(~Bank_Class,scales="free")+
#   theme(plot.title = element_text(hjust = 0.5))
# print(plot)
# name<-paste0(transect,".png")
# getwd()
# setwd("Z:/Files/R_Projects/Smith Creek/SmithCreek_Plots")
# ggsave(name,plot=last_plot(), width=10, height=5)
# setwd("Z:/Files/R_Projects/Smith Creek")


plot<-ggplot(subset(transect_subset, DateandTime==first(transect_subset$DateandTime)|DateandTime==last(transect_subset$DateandTime)),
             aes(x = FinalStation_m, y = FinalElevation_m, color=as.factor(DateandTime))) +
  geom_point(aes(pch=Class),size=3)+geom_path()+
  ggtitle(paste0(transect))+facet_grid(~Bank_Class,scales="free")+
  theme(plot.title = element_text(hjust = 0.5))
print(plot)
name<-paste0(transect,".png")
getwd()
setwd("Z:/Files/R_Projects/Smith Creek/SmithCreek_FirstLast_Plots")
ggsave(name,plot=last_plot(), width=10, height=5)
setwd("Z:/Files/R_Projects/Smith Creek")