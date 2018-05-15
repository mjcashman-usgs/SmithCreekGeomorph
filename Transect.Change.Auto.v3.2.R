#Designed by Matthew J Cashman, PhD of USGS Maryland-DC-DE Water Science Center for cross-sectional survey differences.
#Originally designed for analyzing Smith Creek datasets 
#Checked on 20-March-2018, Current problem with old data not being matched well

rm(list=ls())

getwd()
setwd("Z:/Files/R_Projects/Smith Creek")

#IMPORT DATA
#CrossSections<-read.csv("SmithCreekCrossSections.csv")
library(readxl)
CrossSections<-read_excel("SmithCreekCrossSections.xlsx")


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
unique(CrossSections$PointType)

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

####Manual Subsetting####
transect <- unique(unlist(data.unnest$unique_site))

#Manual Subsets
 # transect<-transect[1]
  
####CALCULATIONS####
#Start XS Loop
all_transect<-NULL
for (i in 1:length(transect)){
  transect_subset<-subset(data.unnest, unique_site==transect[i])
  print(paste("Plotting",transect[i]))
  
####Plotting the Cross-sections####
  library(ggplot2)  
  transect_subset$Bank = factor(transect_subset$Bank, levels=c('L','C','R'))
  transect_subset$Bank_Class<-paste0(transect_subset$Bank,"-",transect_subset$Class)
  transect_subset$Bank_Class = factor(transect_subset$Bank_Class, levels=c('L-Floodplain','L-Bank','C-Bed','R-Bank','R-Floodplain'))
   plot<-ggplot(transect_subset, aes(x = FinalStation_m, y = FinalElevation_m, color=as.factor(DateandTime))) +
     geom_point(aes(pch=Class),size=3)+geom_path()+
     ggtitle(paste0(transect[i]))+facet_grid(~Bank_Class,scale="free")+
     theme(plot.title = element_text(hjust = 0.5))
   print(plot)
   name<-paste0(transect[i],".png")
  # getwd()
   setwd("Z:/Files/R_Projects/Smith Creek/Plots/final")
  #  ggsave(name,plot=last_plot())
   setwd("Z:/Files/R_Projects/Smith Creek")

  plot<-ggplot(subset(transect_subset, DateandTime==first(transect_subset$DateandTime)|DateandTime==last(transect_subset$DateandTime)),
               aes(x = FinalStation_m, y = FinalElevation_m, color=as.factor(DateandTime))) +
    geom_point(aes(pch=Class),size=3)+geom_path()+
    ggtitle(paste0(transect[i]))+facet_grid(~Bank_Class,scale="free")+
    theme(plot.title = element_text(hjust = 0.5))
  print(plot)
  name<-paste0(transect[i],".png")
  # getwd()
   setwd("Z:/Files/R_Projects/Smith Creek//Plots/final/SmithCreek_FirstLast_Plots")
   ggsave(name,plot=last_plot(), width=10, height=5)
   setwd("Z:/Files/R_Projects/Smith Creek")
   
####Subsetting for Polygonal calculations####
  all_class<-NULL
  for (k in 1:length(rev(unique(transect_subset$Class)))){
    transect_subset_class<-subset(transect_subset,Class==rev(unique(transect_subset$Class))[k])
    Current_Class<-rev(unique(transect_subset$Class))[k]
    
    all_bank<-NULL
    for (l in 1:length(unique(transect_subset_class$Bank))){
      transect_subset_class_bank<-subset(transect_subset_class,Bank ==unique(transect_subset_class$Bank)[l])
     # transect_subset_class_bank <- transect_subset_class_bank %>% 
    #    arrange(DateandTime, FinalStation_m)
      Current_Bank<-unique(transect_subset_class$Bank)[l]
      
      survey_first<-subset(transect_subset_class_bank, DateandTime==first(transect_subset$DateandTime))
      survey1<-cbind(x=survey_first$FinalStation_m, y=survey_first$FinalElevation_m)
      survey1<-as.data.frame(survey1)
      
      survey_last<-subset(transect_subset_class_bank, DateandTime==last(transect_subset$DateandTime))
      survey2<-cbind(x=survey_last$FinalStation_m, y=survey_last$FinalElevation_m)
      survey2<-as.data.frame(survey2)
      
      ####Calculate Intersection Points  ####
      library(sf)
      a<-st_multipoint(as.matrix(survey1))
      ls_a<-st_linestring(a) #Convert Survey 1 to line A
      #plot(ls_a)
      b<-st_multipoint(as.matrix(survey2))
      ls_b<-st_linestring(b) #Convert Survey 2 to line B
      #plot(ls_b)
      #mls_ab<-st_multilinestring(list(ls_a,ls_b))
      #plot(mls_ab)
      intersections<-st_intersection(ls_a,ls_b) #Find intersections of A&B
      #intersections
      no.intersections<-length(intersections)/2
      paste0("Number of Intersections: ", no.intersections)
      
      all_areas<-NULL
      if (no.intersections==0) {
        local_area<-NULL
        print(paste0("Running ",Current_Class,"-", Current_Bank," with 0 Intersection, 1 area"))
        area.no<-1
        sub_suvey1<-paste0("sub",area.no,"_survey1")
        sub_survey1 <- subset(survey1)
        sub_survey1
        sub_survey2 <- subset(survey2)
        sub_survey2
        
        sub_survey1.bind<-rbind(sub_survey1)
        sub_survey1.mp <- st_multipoint(as.matrix(sub_survey1.bind))
        #plot(sub_survey1.mp)
        sub_survey2.bind<-rbind(sub_survey2)
        sub_survey2.mp <- st_multipoint(as.matrix(sub_survey2.bind))
        #plot(sub_survey2.mp)
        sub_opener.bind<-rbind(sub_survey1[1,],sub_survey2[1,])
        sub_opener.mp <- st_multipoint(as.matrix(sub_opener.bind))
        #plot(sub_opener.mp)
        sub_closer.bind<-rbind(sub_survey1[nrow(sub_survey1),] ,sub_survey2[nrow(sub_survey2),] )
        sub_closer.mp <- st_multipoint(as.matrix(sub_closer.bind))
        #plot(sub_closer.mp)
        
        sub_survey1.ls<-st_linestring(sub_survey1.mp)
        # plot(sub_survey1.ls)
        sub_survey2.ls<-st_linestring(sub_survey2.mp)
        # plot(sub_survey2.ls)
        sub_opener.ls <- st_linestring(sub_opener.mp)
        # plot(sub_opener.ls)
        sub_closer.ls <- st_linestring(sub_closer.mp)
        # plot(sub_closer.ls)
        
        sub_surveys.mls<-st_multilinestring(list(sub_opener.ls,sub_survey1.ls,sub_survey2.ls,sub_closer.ls))
        
        # plot(sub_surveys.mls)
        sub_area.poly <- st_polygonize(sub_surveys.mls)
        # plot(sub_area.poly)
        
        area<-st_area(sub_area.poly) #ABS area
        #Calculate which is higher in segment
        #Find midpoint of line segment that is less wide
        sub_survey1.width<-sub_survey1.bind[nrow(sub_survey1.bind),1]-sub_survey1.bind[1,1]
        sub_survey2.width<-sub_survey2.bind[nrow(sub_survey2.bind),1]-sub_survey2.bind[1,1]
        if(sub_survey1.width<sub_survey2.width){
          vertical.point<-(sub_survey1.bind[1,1]+sub_survey1.bind[nrow(sub_survey1.bind),1])/2
        } else{
          vertical.point<-(sub_survey2.bind[1,1]+sub_survey2.bind[nrow(sub_survey2.bind),1])/2
        }
        vertical.line<-st_linestring(rbind(cbind(x=vertical.point,y=100),cbind(x=vertical.point,y=-100)))
        
        vert.intersect1<-st_intersection(sub_survey1.ls,vertical.line) #Find intersections of 1 and vertical
        vert.intersect2<-st_intersection(sub_survey2.ls,vertical.line) #Find intersections of 2 and vertical
        
        if(is.na(st_dimension(vert.intersect1))|is.na(st_dimension(vert.intersect2))){
          if(Current_Bank=="L"){
            sub_survey1.midx<-(sub_survey1.bind[1,1]+sub_survey1.bind[nrow(sub_survey1.bind),1])/2
            sub_survey2.midx<-(sub_survey2.bind[1,1]+sub_survey2.bind[nrow(sub_survey2.bind),1])/2
            above <- sub_survey1.midx<sub_survey2.midx
          } else if (Current_Bank=="R"){
            sub_survey1.midx<-(sub_survey1.bind[1,1]+sub_survey1.bind[nrow(sub_survey1.bind),1])/2
            sub_survey2.midx<-(sub_survey2.bind[1,1]+sub_survey2.bind[nrow(sub_survey2.bind),1])/2
            above <- sub_survey1.midx>sub_survey2.midx
          } 
        } else {
          above<-vert.intersect1[2] < vert.intersect2[2]
        }
        
        
        areas<-cbind(area,above)
        areas<-as.data.frame(areas)
        areas$transect<-transect[i]
        areas$above[areas$above == 0] <- -1       
        if(areas$above==1){
          print(paste0(Current_Class,"-", Current_Bank," area ", area.no," is filled"))
        } else if(areas$above==-1){
          print(paste0(Current_Class,"-", Current_Bank," area ", area.no," is scoured"))
        }
        
        areas$net<-areas$area*areas$above
        areas$Segment<-area.no
        areas$no.intersections<-no.intersections
        areas$Class<-Current_Class
        areas$Bank<-Current_Bank
        
        all_areas<-rbind(all_areas,areas)
        all_areas
        #print(all_areas)
        #print(paste0("Net change is ", sum(all_areas$net), " sq meters"))
      } 
      else if (no.intersections == 1) {
        local_area<-NULL
        for (p in 1:((no.intersections)+1)){
          print(paste0("Running ",Current_Class,"-", Current_Bank," with ", no.intersections," Intersection, ", no.intersections+1, " areas"))
          if(p==1){
            area.no<-p
            From<-p-1
            To<-p
            intersection<-intersections
            intersection.matrix<-rbind(setNames(intersection,c('x','y')))
            intersection.matrix<-as.data.frame(intersection.matrix)
            
            ythresh.int<-as.vector(intersection.matrix[,2])
            ythresh.int
            xthresh.int<-as.vector(intersection.matrix[,1])
            xthresh.int
            
            sub_survey1 <- subset(survey1, x <= xthresh.int)
            sub_survey1
            sub_survey2 <- subset(survey2, x <= xthresh.int)
            sub_survey2
            
            sub_survey1.bind<-rbind(sub_survey1,intersection.matrix)
            sub_survey1.mp <- st_multipoint(as.matrix(sub_survey1.bind))
            #plot(sub_survey1.mp)
            sub_survey2.bind<-rbind(sub_survey2,intersection.matrix)
            sub_survey2.mp <- st_multipoint(as.matrix(sub_survey2.bind))
            #plot(sub_survey2.mp)
            sub_opener.bind<-rbind(sub_survey1[1,],sub_survey2[1,])
            sub_opener.mp <- st_multipoint(as.matrix(sub_opener.bind))
            #plot(sub_closer.mp)
            
            sub_survey1.ls<-st_linestring(sub_survey1.mp)
            # plot(sub_survey1.ls)
            sub_survey2.ls<-st_linestring(sub_survey2.mp)
            # plot(sub_survey2.ls)
            sub_opener.ls <- st_linestring(sub_opener.mp)
            # plot(sub_opener.ls)
            
            sub_surveys.mls<-st_multilinestring(list(sub_survey1.ls,sub_survey2.ls,sub_opener.ls))
            
            # plot(sub_surveys.mls)
            sub_area.poly <- st_polygonize(sub_surveys.mls)
            # plot(sub_area.poly)
            
            area<-st_area(sub_area.poly) #ABS area
            
            #Calculate which is higher in segment
            #Find midpoint of line segment that is less wide
            sub_survey1.width<-sub_survey1.bind[nrow(sub_survey1.bind),1]-sub_survey1.bind[1,1]
            sub_survey2.width<-sub_survey2.bind[nrow(sub_survey2.bind),1]-sub_survey2.bind[1,1]
            if(sub_survey1.width<sub_survey2.width){
              vertical.point<-(sub_survey1.bind[1,1]+sub_survey1.bind[nrow(sub_survey1.bind),1])/2
            } else{
              vertical.point<-(sub_survey2.bind[1,1]+sub_survey2.bind[nrow(sub_survey2.bind),1])/2
            }
            vertical.line<-st_linestring(rbind(cbind(x=vertical.point,y=100),cbind(x=vertical.point,y=-100)))
            
            vert.intersect1<-st_intersection(sub_survey1.ls,vertical.line) #Find intersections of 1 and vertical
            vert.intersect2<-st_intersection(sub_survey2.ls,vertical.line) #Find intersections of 2 and vertical
            
            if(is.na(st_dimension(vert.intersect1))|is.na(st_dimension(vert.intersect2))){
              if(Current_Bank=="L"){
                sub_survey1.midx<-(sub_survey1.bind[1,1]+sub_survey1.bind[nrow(sub_survey1.bind),1])/2
                sub_survey2.midx<-(sub_survey2.bind[1,1]+sub_survey2.bind[nrow(sub_survey2.bind),1])/2
                above <- sub_survey1.midx>sub_survey2.midx
              } else if (Current_Bank=="R"){
                sub_survey1.midx<-(sub_survey1.bind[1,1]+sub_survey1.bind[nrow(sub_survey1.bind),1])/2
                sub_survey2.midx<-(sub_survey2.bind[1,1]+sub_survey2.bind[nrow(sub_survey2.bind),1])/2
                above <- sub_survey1.midx<sub_survey2.midx
              } 
            } else {
              above<-vert.intersect1[2] < vert.intersect2[2]
            }
            
            
            areas<-cbind(area,above)
            areas<-as.data.frame(areas)
            areas$transect<-transect[i]
            areas$above[areas$above == 0] <- -1       

            if(areas$above==1){
              print(paste0(Current_Class,"-", Current_Bank," area ", area.no," is filled"))
            } else if(areas$above==-1){
                print(paste0(Current_Class,"-", Current_Bank," area ", area.no," is scoured"))
              }
            
            areas$net<-areas$area*areas$above
            areas$Segment<-area.no
            areas$no.intersections<-no.intersections
            areas$Class<-Current_Class
            areas$Bank<-Current_Bank
            areas
            
            local_area<-rbind(local_area,areas)
          }
          else if (p==2){
            area.no<-p
            from<-p-1
            
            intersection.from<-intersections
            intersection.from.matrix<-rbind(setNames(intersection.from,c('x','y')))
            intersection.from.matrix<-as.data.frame(intersection.from.matrix)
            
            ythresh.int.from<-as.vector(intersection.from.matrix[,2])
            ythresh.int.from
            xthresh.int.from<-as.vector(intersection.from.matrix[,1])
            xthresh.int.from
            
            sub_survey1 <- subset(survey1, x >= xthresh.int.from)
            sub_survey1
            sub_survey2 <- subset(survey2, x >= xthresh.int.from)
            sub_survey2
            
            
            sub_survey1.bind<-rbind(intersection.from.matrix,sub_survey1)
            sub_survey1.mp <- st_multipoint(as.matrix(sub_survey1.bind))
            #plot(sub_survey1.mp)
            sub_survey2.bind<-rbind(intersection.from.matrix,sub_survey2)
            sub_survey2.mp <- st_multipoint(as.matrix(sub_survey2.bind))
            #plot(sub_survey2.mp)
            sub_closer.bind<-rbind(sub_survey1[nrow(sub_survey1),],sub_survey2[nrow(sub_survey2),])
            sub_closer.mp <- st_multipoint(as.matrix(sub_closer.bind))
            #plot(sub_closer.mp)
            sub_survey1.ls<-st_linestring(sub_survey1.mp)
            # plot(sub_survey1.ls)
            sub_survey2.ls<-st_linestring(sub_survey2.mp)
            # plot(sub_survey2.ls)
            sub_closer.ls <- st_linestring(sub_closer.mp)
            # plot(sub_closer.ls)
            
            sub_surveys.mls<-st_multilinestring(list(sub_survey1.ls,sub_survey2.ls,sub_closer.ls))
            
            # plot(sub_surveys.mls)
            sub_area.poly <- st_polygonize(sub_surveys.mls)
            # plot(sub_area.poly)
            
            area<-st_area(sub_area.poly) #ABS area
            
            #Calculate which is higher in segment
            #Find midpoint of line segment that is less wide
            sub_survey1.width<-sub_survey1.bind[nrow(sub_survey1.bind),1]-sub_survey1.bind[1,1]
            sub_survey2.width<-sub_survey2.bind[nrow(sub_survey2.bind),1]-sub_survey2.bind[1,1]
            if(sub_survey1.width<sub_survey2.width){
              vertical.point<-(sub_survey1.bind[1,1]+sub_survey1.bind[nrow(sub_survey1.bind),1])/2
            } else{
              vertical.point<-(sub_survey2.bind[1,1]+sub_survey2.bind[nrow(sub_survey2.bind),1])/2
            }
            vertical.line<-st_linestring(rbind(cbind(x=vertical.point,y=100),cbind(x=vertical.point,y=-100)))
            
            vert.intersect1<-st_intersection(sub_survey1.ls,vertical.line) #Find intersections of 1 and vertical
            vert.intersect2<-st_intersection(sub_survey2.ls,vertical.line) #Find intersections of 2 and vertical
            
            if(is.na(st_dimension(vert.intersect1))|is.na(st_dimension(vert.intersect2))){
              if(Current_Bank=="L"){
                sub_survey1.midx<-(sub_survey1.bind[1,1]+sub_survey1.bind[nrow(sub_survey1.bind),1])/2
                sub_survey2.midx<-(sub_survey2.bind[1,1]+sub_survey2.bind[nrow(sub_survey2.bind),1])/2
                above <- sub_survey1.midx>sub_survey2.midx
              } else if (Current_Bank=="R"){
                sub_survey1.midx<-(sub_survey1.bind[1,1]+sub_survey1.bind[nrow(sub_survey1.bind),1])/2
                sub_survey2.midx<-(sub_survey2.bind[1,1]+sub_survey2.bind[nrow(sub_survey2.bind),1])/2
                above <- sub_survey1.midx<sub_survey2.midx
              } 
            } else {
              above<-vert.intersect1[2] < vert.intersect2[2]
            }
            
            
            areas<-cbind(area,above)
            areas<-as.data.frame(areas)
            areas$transect<-transect[i]
            areas$above[areas$above == 0] <- -1       
            
            if(areas$above==1){
              print(paste0(Current_Class,"-", Current_Bank," area ", area.no," is filled"))
            } else if(areas$above==-1){
              print(paste0(Current_Class,"-", Current_Bank," area ", area.no," is scoured"))
            }
            
            areas$net<-areas$area*areas$above
            areas$Segment<-area.no
            areas$no.intersections<-no.intersections
            areas$Class<-Current_Class
            areas$Bank<-Current_Bank
            areas
            local_area<-rbind(local_area,areas)            
            
            #print(local_area)
          }
        }
        all_areas<-rbind(all_areas,local_area)
      }
      else if (no.intersections > 1) {
        for (t in 1:((no.intersections)+1)){
          local_area<-NULL
          print(paste0("Running ",Current_Class,"-", Current_Bank," with ", no.intersections," Intersections: Area ", t, " of ",(no.intersections)+1))
           if (t==1){
          area.no<-t
          From<-t-1
          To<-t
          
          intersection<-intersections[To,]
          intersection.matrix<-rbind(setNames(intersection,c('x','y')))
          intersection.matrix<-as.data.frame(intersection.matrix)
          
          ythresh.int<-as.vector(intersection.matrix[,2])
          ythresh.int
          xthresh.int<-as.vector(intersection.matrix[,1])
          xthresh.int
          
          sub_survey1 <- subset(survey1, x <= xthresh.int)
          sub_survey1
          sub_survey2 <- subset(survey2, x <= xthresh.int)
          sub_survey2
          if(sub_survey1[,1]!=sub_survey2[,1]|sub_survey1[,2]!=sub_survey2[,2]){
           sub_survey1.bind<-rbind(sub_survey1,intersection.matrix)
          sub_survey1.mp <- st_multipoint(as.matrix(sub_survey1.bind))
          #plot(sub_survey1.mp)
          sub_survey2.bind<-rbind(sub_survey2,intersection.matrix)
          sub_survey2.mp <- st_multipoint(as.matrix(sub_survey2.bind))
          #plot(sub_survey2.mp)
          sub_opener.bind<-rbind(sub_survey1[1,],sub_survey2[1,])
          sub_opener.mp <- st_multipoint(as.matrix(sub_opener.bind))
          #plot(sub_closer.mp)
          
          sub_survey1.ls<-st_linestring(sub_survey1.mp)
          # plot(sub_survey1.ls)
          sub_survey2.ls<-st_linestring(sub_survey2.mp)
          # plot(sub_survey2.ls)
          sub_opener.ls <- st_linestring(sub_opener.mp)
          # plot(sub_opener.ls)
          
          sub_surveys.mls<-st_multilinestring(list(sub_survey1.ls,sub_survey2.ls,sub_opener.ls))
          
          # plot(sub_surveys.mls)
          sub_area.poly <- st_polygonize(sub_surveys.mls)
          # plot(sub_area.poly)
          
          area<-st_area(sub_area.poly) #ABS area
          
          
          #Calculate which is higher in segment
          #Find midpoint of line segment that is less wide
          sub_survey1.width<-sub_survey1.bind[nrow(sub_survey1.bind),1]-sub_survey1.bind[1,1]
          sub_survey2.width<-sub_survey2.bind[nrow(sub_survey2.bind),1]-sub_survey2.bind[1,1]
          if(sub_survey1.width<sub_survey2.width){
            vertical.point<-(sub_survey1.bind[1,1]+sub_survey1.bind[nrow(sub_survey1.bind),1])/2
          } else{
            vertical.point<-(sub_survey2.bind[1,1]+sub_survey2.bind[nrow(sub_survey2.bind),1])/2
          }
          vertical.line<-st_linestring(rbind(cbind(x=vertical.point,y=100),cbind(x=vertical.point,y=-100)))
          
          vert.intersect1<-st_intersection(sub_survey1.ls,vertical.line) #Find intersections of 1 and vertical
          vert.intersect2<-st_intersection(sub_survey2.ls,vertical.line) #Find intersections of 2 and vertical
          
          if(is.na(st_dimension(vert.intersect1))|is.na(st_dimension(vert.intersect2))){
            if(Current_Bank=="L"){
              sub_survey1.midx<-(sub_survey1.bind[1,1]+sub_survey1.bind[nrow(sub_survey1.bind),1])/2
              sub_survey2.midx<-(sub_survey2.bind[1,1]+sub_survey2.bind[nrow(sub_survey2.bind),1])/2
              above <- sub_survey1.midx>sub_survey2.midx
            } else if (Current_Bank=="R"){
              sub_survey1.midx<-(sub_survey1.bind[1,1]+sub_survey1.bind[nrow(sub_survey1.bind),1])/2
              sub_survey2.midx<-(sub_survey2.bind[1,1]+sub_survey2.bind[nrow(sub_survey2.bind),1])/2
              above <- sub_survey1.midx<sub_survey2.midx
            } 
          } else {
            above<-vert.intersect1[2] < vert.intersect2[2]
          }
          
          
          areas<-cbind(area,above)
          areas<-as.data.frame(areas)
          areas$transect<-transect[i]
          areas$above[areas$above == 0] <- -1       
          if(areas$above==1){
            print(paste0(Current_Class,"-", Current_Bank," area ", area.no," is filled"))
          } else if(areas$above==-1){
            print(paste0(Current_Class,"-", Current_Bank," area ", area.no," is scoured"))
          }
          
          areas$net<-areas$area*areas$above
          areas$Segment<-area.no
          areas$no.intersections<-no.intersections
          areas$Class<-Current_Class
          areas$Bank<-Current_Bank
          areas
          local_area<-rbind(local_area,areas)
          all_areas<-rbind(all_areas,local_area)
          }   
          }
           else if (t>1 & t<((no.intersections)+1)){
              area.no<-t
              from<-t-1
              to<-t
              
              intersection.from<-intersections[from,]
              intersection.from.matrix<-rbind(setNames(intersection.from,c('x','y')))
              intersection.from.matrix<-as.data.frame(intersection.from.matrix)
              ythresh.int.from<-as.vector(intersection.from.matrix[,2])
              ythresh.int.from
              xthresh.int.from<-as.vector(intersection.from.matrix[,1])
              xthresh.int.from
              
              intersection.to<-intersections[to,]
              intersection.to.matrix<-rbind(setNames(intersection.to,c('x','y')))
              intersection.to.matrix<-as.data.frame(intersection.to.matrix)
              ythresh.int.to<-as.vector(intersection.to.matrix[,2])
              ythresh.int.to
              xthresh.int.to<-as.vector(intersection.to.matrix[,1])
              xthresh.int.to  
              
              sub_survey1 <- subset(survey1, x >= xthresh.int.from & x <= xthresh.int.to)
              sub_survey1
              sub_survey2 <- subset(survey2, x >= xthresh.int.from & x <= xthresh.int.to)
              sub_survey2
              
              sub_survey1.bind<-rbind(intersection.from.matrix, sub_survey1,intersection.to.matrix)
              sub_survey1.mp <- st_multipoint(as.matrix(sub_survey1.bind))
              #plot(sub_survey1.mp)
              sub_survey2.bind<-rbind(intersection.from.matrix, sub_survey2,intersection.to.matrix)
              sub_survey2.mp <- st_multipoint(as.matrix(sub_survey2.bind))
              #plot(sub_survey2.mp)
              
              sub_survey1.ls<-st_linestring(sub_survey1.mp)
              # plot(sub_survey1.ls)
              sub_survey2.ls<-st_linestring(sub_survey2.mp)
              # plot(sub_survey2.ls)
              
              sub_surveys.mls<-st_multilinestring(list(sub_survey1.ls,sub_survey2.ls))
              # plot(sub_surveys.mls)

              sub_area.poly <- st_polygonize(sub_surveys.mls)
              # plot(sub_area.poly)
              
              area<-st_area(sub_area.poly) #ABS area
              
              #Calculate which is higher in segment
              #Find midpoint of line segment that is less wide
              sub_survey1.width<-sub_survey1.bind[nrow(sub_survey1.bind),1]-sub_survey1.bind[1,1]
              sub_survey2.width<-sub_survey2.bind[nrow(sub_survey2.bind),1]-sub_survey2.bind[1,1]
              if(sub_survey1.width<sub_survey2.width){
                vertical.point<-(sub_survey1.bind[1,1]+sub_survey1.bind[nrow(sub_survey1.bind),1])/2
              } else{
                vertical.point<-(sub_survey2.bind[1,1]+sub_survey2.bind[nrow(sub_survey2.bind),1])/2
              }
              vertical.line<-st_linestring(rbind(cbind(x=vertical.point,y=100),cbind(x=vertical.point,y=-100)))
              
              vert.intersect1<-st_intersection(sub_survey1.ls,vertical.line) #Find intersections of 1 and vertical
              vert.intersect2<-st_intersection(sub_survey2.ls,vertical.line) #Find intersections of 2 and vertical
              
              if(is.na(st_dimension(vert.intersect1))|is.na(st_dimension(vert.intersect2))){
                if(Current_Bank=="L"){
                  sub_survey1.midx<-(sub_survey1.bind[1,1]+sub_survey1.bind[nrow(sub_survey1.bind),1])/2
                  sub_survey2.midx<-(sub_survey2.bind[1,1]+sub_survey2.bind[nrow(sub_survey2.bind),1])/2
                  above <- sub_survey1.midx>sub_survey2.midx
                } else if (Current_Bank=="R"){
                  sub_survey1.midx<-(sub_survey1.bind[1,1]+sub_survey1.bind[nrow(sub_survey1.bind),1])/2
                  sub_survey2.midx<-(sub_survey2.bind[1,1]+sub_survey2.bind[nrow(sub_survey2.bind),1])/2
                  above <- sub_survey1.midx<sub_survey2.midx
                } 
              } else {
                above<-vert.intersect1[2] < vert.intersect2[2]
              }
              
              
              areas<-cbind(area,above)
              areas<-as.data.frame(areas)
              areas$transect<-transect[i]
              areas$above[areas$above == 0] <- -1         
              
              if(areas$above==1){
                print(paste0(Current_Class,"-", Current_Bank," area ", area.no," is filled"))
              } else if(areas$above==-1){
                print(paste0(Current_Class,"-", Current_Bank," area ", area.no," is scoured"))
              }
              
              areas$net<-areas$area*areas$above
              areas$Segment<-area.no
              areas$no.intersections<-no.intersections
              areas$Class<-Current_Class
              areas$Bank<-Current_Bank
              areas
              local_area<-rbind(local_area,areas)
              all_areas<-rbind(all_areas,local_area)
              #print(local_area)
              #print(paste0("Net change is ", sum(local_area$net), " sq meters"))
              
          } 
           else if (t==((no.intersections)+1)){
            area.no<-t
            from<-t-1
            
            intersection.from<-intersections[from,]
            intersection.from.matrix<-rbind(setNames(intersection.from,c('x','y')))
            intersection.from.matrix<-as.data.frame(intersection.from.matrix)
            
            ythresh.int.from<-as.vector(intersection.from.matrix[,2])
            ythresh.int.from
            xthresh.int.from<-as.vector(intersection.from.matrix[,1])
            xthresh.int.from
      
            sub_survey1 <- subset(survey1, x >= xthresh.int.from)
            sub_survey1
            sub_survey2 <- subset(survey2,x >= xthresh.int.from)
            sub_survey2
            
            
            sub_survey1.bind<-rbind(intersection.from.matrix,sub_survey1)
            sub_survey1.mp <- st_multipoint(as.matrix(sub_survey1.bind))
            #plot(sub_survey1.mp)
            sub_survey2.bind<-rbind(intersection.from.matrix,sub_survey2)
            sub_survey2.mp <- st_multipoint(as.matrix(sub_survey2.bind))
            #plot(sub_survey2.mp)
            sub_closer.bind<-rbind(sub_survey1[nrow(sub_survey1),],sub_survey2[nrow(sub_survey2),])
            sub_closer.mp <- st_multipoint(as.matrix(sub_closer.bind))
            #plot(sub_closer.mp)
            sub_survey1.ls<-st_linestring(sub_survey1.mp)
            # plot(sub_survey1.ls)
            sub_survey2.ls<-st_linestring(sub_survey2.mp)
            # plot(sub_survey2.ls)
            sub_closer.ls <- st_linestring(sub_closer.mp)
            # plot(sub_closer.ls)
            
            sub_surveys.mls<-st_multilinestring(list(sub_survey1.ls,sub_survey2.ls,sub_closer.ls))
            # plot(sub_surveys.mls)
            sub_area.poly <- st_polygonize(sub_surveys.mls)
            # plot(sub_area.poly)
            
            area<-st_area(sub_area.poly) #ABS area
            
            #Calculate which is higher in segment
            #Find midpoint of line segment that is less wide
            sub_survey1.width<-sub_survey1.bind[nrow(sub_survey1.bind),1]-sub_survey1.bind[1,1]
            sub_survey2.width<-sub_survey2.bind[nrow(sub_survey2.bind),1]-sub_survey2.bind[1,1]
            if(sub_survey1.width<sub_survey2.width){
              vertical.point<-(sub_survey1.bind[1,1]+sub_survey1.bind[nrow(sub_survey1.bind),1])/2
            } else{
              vertical.point<-(sub_survey2.bind[1,1]+sub_survey2.bind[nrow(sub_survey2.bind),1])/2
            }
            vertical.line<-st_linestring(rbind(cbind(x=vertical.point,y=100),cbind(x=vertical.point,y=-100)))
            
            vert.intersect1<-st_intersection(sub_survey1.ls,vertical.line) #Find intersections of 1 and vertical
            vert.intersect2<-st_intersection(sub_survey2.ls,vertical.line) #Find intersections of 2 and vertical
            
            if(is.na(st_dimension(vert.intersect1))|is.na(st_dimension(vert.intersect2))){
              if(Current_Bank=="L"){
                sub_survey1.midx<-(sub_survey1.bind[1,1]+sub_survey1.bind[nrow(sub_survey1.bind),1])/2
                sub_survey2.midx<-(sub_survey2.bind[1,1]+sub_survey2.bind[nrow(sub_survey2.bind),1])/2
                above <- sub_survey1.midx>sub_survey2.midx
              } else if (Current_Bank=="R"){
                sub_survey1.midx<-(sub_survey1.bind[1,1]+sub_survey1.bind[nrow(sub_survey1.bind),1])/2
                sub_survey2.midx<-(sub_survey2.bind[1,1]+sub_survey2.bind[nrow(sub_survey2.bind),1])/2
                above <- sub_survey1.midx<sub_survey2.midx
              } 
            } else {
              above<-vert.intersect1[2] < vert.intersect2[2]
            }
            
            
            areas<-cbind(area,above)
            areas<-as.data.frame(areas)
            areas$transect<-transect[i]
            areas$above[areas$above == 0] <- -1       
            if(areas$above==1){
              print(paste0(Current_Class,"-", Current_Bank," area ", area.no," is filled"))
            } else if(areas$above==-1){
              print(paste0(Current_Class,"-", Current_Bank," area ", area.no," is scoured"))
            }
            areas$net<-areas$area*areas$above
            areas$Segment<-area.no
            areas$no.intersections<-no.intersections
            areas$Class<-Current_Class
            areas$Bank<-Current_Bank
            areas
            local_area<-rbind(local_area,areas)
            all_areas<-rbind(all_areas,local_area)
            #print(local_area)
            #print(paste0("Net change is ", sum(local_area$net), " sq meters"))

            }
        }
      }
    
      all_bank<-rbind(all_bank,all_areas) 
    }
    all_class<-rbind(all_class,all_bank) 
  }
all_transect<-rbind(all_transect,all_class)
}
  
####Data Post-Processing####

transect_change <- all_transect %>% arrange(Class)  %>%  select(transect,everything()) %>% group_by(transect,Class,Bank,above) %>%
  summarize(netchange=sum(net))
transect_change <- spread(transect_change, above, netchange)
transect_change[is.na(transect_change)] <- 0
names(transect_change)[names(transect_change)=="-1"] <- "scour"
names(transect_change)[names(transect_change)=="1"] <- "fill"
transect_change$net <- transect_change$scour + transect_change$fill

transect_netchange <- all_transect %>% arrange(Class)  %>%  select(transect,everything()) %>% group_by(transect,Class,Bank) %>%
  summarize(netchange=sum(net))



transect.dates <- data.unnest %>% group_by(unique_site) %>%  arrange(DateandTime) %>% 
  summarize(first=first(DateandTime),last=last(DateandTime), 
            day.diff=(last(DateandTime)-first(DateandTime)),
            year.diff=(last(DateandTime)-first(DateandTime))/365.25)
colnames(transect.dates)[1] <- "transect"

All_Change<-merge(transect_change,transect.dates, by="transect")
All_Change$day.diff<-as.numeric(All_Change$day.diff,units="days")
All_Change <- All_Change %>% separate(transect, into = c("River","Site","Transect"),sep=", ") 
All_Change$scour.rate <- (All_Change$scour/All_Change$day.diff)*365.25
All_Change$fill.rate <- (All_Change$fill/All_Change$day.diff)*365.25
All_Change$net.rate <- (All_Change$net/All_Change$day.diff)*365.25

print(All_Change)

#Export
write.csv(All_Change, file = "All_Change.csv")
write.csv(all_transect, file = "All_Transects.csv")
#Plot histogram
library(ggplot2)

ggplot(All_Change) + 
  geom_histogram(aes(x=scour.rate),fill="red") + 
  geom_histogram(aes(x=fill.rate),fill="blue") + 
  facet_grid(~Class)

 
# Sums<-all_transect %>% 
#   group_by(transect,Class,Bank) %>%
#   summarize(n=n(),maxseg=max(Segment),no.intersections=max(no.intersections))
# Sums$Check<-Sums$maxseg-(Sums$no.intersections+1)
# 
# Check <- Sums %>%
#   group_by(transect,Class) %>%
#   summarize(n=n(),Banks=length(Bank))
# 
# Check <- Check %>%
#   group_by(transect) %>%
#   summarize(sum=sum(n))