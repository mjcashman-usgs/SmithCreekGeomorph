
root.dir = getwd()

source(paste0(root.dir,"/import_merge_gis_field.R"))

p_load(caret, e1071, corrplot,plotly)

##XS Data visualizations 
  #Strahler Stream Order for XS data
  (XS_ws_site %>%
             gather(key="change_type", value = "value", scour.rate, fill.rate, net.rate) %>%
             ggplot(aes(x=as.factor(strahler), y=value, fill=change_type))+
             # geom_violin()+
             geom_boxplot()+
             geom_point()+
             facet_grid(change_type~Class)+
             theme_bw()+
             geom_smooth()+
             ylab("Change in m^2")+
             scale_fill_viridis_d(begin=0.5))+
            ggtitle("SITE AVERAGE Calculations from Cross-Sectional Surveys")+
  theme(legend.position = "bottom")
  
ggsave(paste0(root.dir,"/output/test_figs/Site_Avg_XS_change_strahler.png"))

  #Contributing area for XS data
  (XS_ws_site %>%
    gather(key="change_type", value = "value", scour.rate, fill.rate, net.rate) %>%
    ggplot(aes(x=contr_area, y=value, color=change_type))+
   # geom_violin()+
   # geom_boxplot()+
    geom_point()+
    facet_grid(change_type~Class)+
    theme_bw()+
    geom_smooth()+
    ylab("Change in m^2")+
    scale_color_viridis_d(end=0.4))+
  ggtitle("SITE AVERAGE Calculations from Cross-Sectional Surveys")+
  theme(legend.position = "bottom")

ggsave(paste0(root.dir,"/output/test_figs/Site_Avg_XS_change_area.png"))


##Bank Data visualizations 
  #Strahler Stream Order for Bank data
  (Bank_ws_site %>%
      #gather(key="change_type", value = "value", scour.rate, fill.rate, net.rate) %>%
      ggplot(aes(x=as.factor(strahler), y=-Avg_NormCumDiff_cm_per_day))+
      # geom_violin()+
      geom_boxplot()+
      geom_point()+
  #    facet_grid(change_type~Class)+
      theme_bw()+
      geom_smooth()+
  #    ylab("Avg N2")+
      scale_fill_viridis_d(begin=0.5))+
  ggtitle("SITE AVERAGE Calculations from Bank Pins")
  
ggsave(paste0(root.dir,"/output/test_figs/Site_Avg_BankPin_change_strahler.png"))

  #Contributing area for Bank data
  (Bank_ws_site %>%
      #gather(key="change_type", value = "value", scour.rate, fill.rate, net.rate) %>%
      ggplot(aes(x=contr_area, y=-Avg_NormCumDiff_cm_per_day))+
      # geom_violin()+
      # geom_boxplot()+
      geom_point()+
  #    facet_grid(change_type~Class)+
      theme_bw()+
      geom_smooth(method="lm")+
      scale_color_viridis_d(end=0.4))+
  ggtitle("SITE AVERAGE Calculations from Bank Pins")

ggsave(paste0(root.dir,"/output/test_figs/Site_Avg_BankPin_change_area.png"))

##Tile/Floodplain Data visualizations 
#Strahler Stream Order for Bank data
tile_ws_site %>%
    group_by(Site) %>%
    summarize(mean_deposit_mm_yr = mean(deposit_mm_yr), strahler=first(strahler), shreve=first(shreve), contr_area=first(contr_area)) %>%
    ggplot(aes(x=as.factor(strahler), y=mean_deposit_mm_yr))+
    # geom_violin()+
    geom_boxplot()+
    geom_point()+
    #    facet_grid(change_type~Class)+
    theme_bw()+
    geom_smooth()+
    #    ylab("Avg N2")+
    #scale_fill_viridis_d(begin=0.5))+
  ggtitle("SITE AVERAGE Calculations from Floodplain Tiles")

ggsave(paste0(root.dir,"/output/test_figs/Site_Avg_FloodTiles_change_strahler.png"))

#Contributing area for Bank data
tile_ws_site %>%
  group_by(Site) %>%
  summarize(mean_deposit_mm_yr = mean(deposit_mm_yr), strahler=first(strahler), shreve=first(shreve), contr_area=first(contr_area)) %>%
  ggplot(aes(x=contr_area, y=mean_deposit_mm_yr))+
  # geom_violin()+
    # geom_boxplot()+
    geom_point()+
    #    facet_grid(change_type~Class)+
    theme_bw()+
    geom_smooth(method="lm")+
    scale_color_viridis_d(end=0.4)+
  #scale_fill_viridis_d(begin=0.5))+
  ggtitle("SITE AVERAGE Calculations from Floodplain Tiles")

ggsave(paste0(root.dir,"/output/test_figs/Site_Avg_FloodTiles_change_area.png"))

##Tile/Floodplain Data visualizations  - transect only
#Strahler Stream Order for
tile_ws_site %>%
  ggplot(aes(x=as.factor(strahler), y=deposit_mm_yr))+
  # geom_violin()+
  geom_boxplot()+
  geom_point()+
  #    facet_grid(change_type~Class)+
  theme_bw()+
  geom_smooth()+
  #    ylab("Avg N2")+
  #scale_fill_viridis_d(begin=0.5))+
  ggtitle("TRANSECT AVERAGE Calculations from Floodplain Tiles")

ggsave(paste0(root.dir,"/output/test_figs/Transect_Avg_FloodTiles_change_strahler.png"))

#Contributing area for 
tile_ws_site %>%
  ggplot(aes(x=contr_area, y=deposit_mm_yr))+
  # geom_violin()+
  # geom_boxplot()+
  geom_point()+
  #    facet_grid(change_type~Class)+
  theme_bw()+
  geom_smooth(method="lm")+
  scale_color_viridis_d(end=0.4)+
  #scale_fill_viridis_d(begin=0.5))+
  ggtitle("TRANSECT AVERAGE Calculations from Floodplain Tiles")

ggsave(paste0(root.dir,"/output/test_figs/Transect_Avg_FloodTiles_change_area.png"))
