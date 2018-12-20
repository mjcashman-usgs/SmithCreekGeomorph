#Let's derive some secondary variables from the XS data 

if (!require('pacman')) install.packages('pacman'); library('pacman')
p_load(tidyverse)

#data.unnest
XS_unnest <- read.csv("XS_unnest.csv") %>%
  mutate(ChannelFeature = recode(ChannelFeature, " Bank" = "Bank")) %>%
  mutate(ChannelFeature = recode(ChannelFeature, " Bed" = "Bed")) %>%
  mutate(ChannelFeature = recode(ChannelFeature, " Top of Bank" = "Top of Bank")) %>%
  mutate(ChannelFeature = recode(ChannelFeature, " Beg" = "floreen_ab")) %>%
  mutate(ChannelFeature = recode(ChannelFeature, "floreen_us" = "floreen_ab")) %>%
  select(-X)

XS_unnest$DateandTime <- as.Date(XS_unnest$DateandTime)

#Calculate bank heights - left and right then getting average per transect ID
Bank_heights <- XS_unnest %>% 
  group_by(SiteID, TransectID, DateandTime) %>%
  mutate(thalwegheight = min(FinalElevation_m)) %>%
  mutate(height_above_thal = (FinalElevation_m-thalwegheight)) %>%
  group_by(Bank) %>%
  filter(ChannelFeature == "Top of Bank"|ChannelFeature == " Top of Bank") %>%
  arrange(SiteID, TransectID, ChannelFeature, Bank,DateandTime) %>%
  select(-X__1,-X__2, -PointType) %>%
  group_by(SiteID, TransectID, DateandTime, ChannelFeature,unique_site) %>%
  summarise(bankheight = mean(height_above_thal)) %>%
  ungroup() %>%
  group_by(SiteID, TransectID, unique_site) %>%
  filter(DateandTime == min(DateandTime)|DateandTime == max(DateandTime))

ggplot(Bank_heights, aes(x=DateandTime, y=bankheight),color=as.factor(DateandTime),fill=as.factor(DateandTime))+
  geom_point()+
  facet_wrap(~unique_site, scales="free")

#Calculate floodplain widths m
Floodplain_widths <- XS_unnest %>% 
  group_by(SiteID, TransectID, DateandTime) %>%
  summarize(minflood = min(FinalStation_m),maxflood = max(FinalStation_m)) %>%
  mutate(floodwidth = (maxflood - minflood)) %>%
  ungroup() %>%
  group_by(SiteID, TransectID) %>%
  filter(DateandTime == min(DateandTime)|DateandTime == max(DateandTime)) %>%
  select(-minflood, -maxflood)

ggplot(Floodplain_widths, aes(x=DateandTime, y=floodwidth, color=TransectID)) +
  geom_line()+
  facet_wrap(~SiteID, scales="free")

#Calculate channel widths m
Channel_widths <- XS_unnest %>% 
  select(-X__1,-X__2, -PointType, -Remark, -FinalElevation_m) %>%
  filter(unique_site != "Smith Creek, Donnie's Place, L Trib DS") %>%
  group_by(SiteID, TransectID, DateandTime) %>%
  filter(ChannelFeature == "Top of Bank"|ChannelFeature == " Top of Bank") %>%
  spread(Bank, FinalStation_m) %>%
  mutate(channelwidth = (R - L)) %>%
  ungroup() %>%
  group_by(SiteID, TransectID) %>%
  filter(DateandTime == min(DateandTime)|DateandTime == max(DateandTime)) %>%
  select(-Class, -ChannelFeature,-L,-R) 

ggplot(Channel_widths, aes(x=DateandTime, y=channelwidth, color=TransectID)) +
  geom_line()+
  facet_wrap(~SiteID, scales="free")

#bank angles
Bank_Angle <- XS_unnest %>% 
  select(-X__1,-X__2, -PointType, -Remark) %>%
  filter(unique_site != "Smith Creek, Donnie's Place, L Trib DS") %>%
  group_by(SiteID, TransectID, DateandTime, Bank) %>%
  filter(Class == "Bank") %>%
  select(-ChannelFeature) %>%
  filter(row_number() == 1|row_number() ==n()) %>%
  mutate(id=c(1,-1)) %>%
  gather(key = Key, value, FinalStation_m:FinalElevation_m) %>%
  #unite(temp, Bank,Key) %>%
  mutate(id.value = id*value) %>%
  ungroup() %>%
  group_by(NameofRiver, SiteID, TransectID, Bank, DateandTime, unique_site,Class, Key) %>%
  summarize(diff = abs(sum(id.value))) %>%
  spread(Key, diff) %>%
  mutate(Bank_Slope = (FinalElevation_m/FinalStation_m)) %>%
  mutate(Bank_Angle = atan(Bank_Slope)*180/pi) %>%
  select(-FinalElevation_m, FinalStation_m) %>%
  group_by(SiteID, TransectID, DateandTime, unique_site) %>%
  summarise(Bank_Angle = mean(Bank_Angle)) %>%
  group_by(SiteID, TransectID, unique_site) %>%
  filter(DateandTime == min(DateandTime)|DateandTime == max(DateandTime))
  
ggplot(Bank_Angle, aes(x=DateandTime, y=Bank_Angle, color=TransectID)) +
  geom_line()+
  facet_wrap(~SiteID, scales="free_x")

#Unite into field_attributes
field_attributes <- full_join(Bank_Angle, Bank_heights) %>%
  select(-ChannelFeature) %>%
  full_join(Channel_widths) %>%
  full_join(Floodplain_widths) %>%
  select(-NameofRiver) %>%
  mutate(channelflood_ratio = channelwidth/floodwidth) %>%
  mutate(bankflood_ratio = bankheight/floodwidth)

#Take average from first and last survey for each site (only first and last because they are QA/QC)
field_attributes <- field_attributes %>%
  group_by(SiteID,TransectID,unique_site) %>%
  summarise_at(c("Bank_Angle","bankheight","channelwidth","floodwidth","channelflood_ratio"), mean)
  
  
write.csv(field_attributes, "Data/field_attributes.csv", row.names=FALSE)
