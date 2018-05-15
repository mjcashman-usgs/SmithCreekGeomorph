setwd("Z:/Files/R_Projects/Smith Creek")

#IMPORT DATA
library(readxl)
BankBar<-read_excel("TilesandCores.xlsx")

library(dplyr)
library(ggplot2)

unique(BankBar$SiteName)

i<-4
CurrentSite<-unique(BankBar$SiteName)[i]
BankBar %>%
    group_by(SiteName,XS) %>%
    unite_("SiteXS",c("SiteName","XS")) %>%
#  filter(SiteName == CurrentSite) %>%
  ggplot() +
    geom_point(aes(x=Station_m, y=Thickness_mm))+
    facet_wrap(~SiteXS, scales="free") +
    ggtitle(paste(CurrentSite))
