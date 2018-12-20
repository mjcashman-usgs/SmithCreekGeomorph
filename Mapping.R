---
title: "R Notebook"
output: html_notebook
---

Plotting

```{r}
if (!require('pacman')) install.packages('pacman'); library('pacman')
p_load(FedData,tidyverse,ggpubr,rasterVis,cowplot, ggmap)
```


```{r}

vepPolygon <- polygon_from_extent(raster::extent(672800,740000,4102000,4170000),
proj4string='+proj=utm +datum=NAD83 +zone=12')
# Get the NED (USA ONLY)
# Returns a raster
NED <- get_ned(template=vepPolygon, label='VEPIIN')
# Plot with raster::plot
plot(NED)
```
```{r}
#Calculate Bounding Box and Download NED and NHD ----

sbbox <- make_bbox(lon = XS_ws_transect$LONG, lat = XS_ws_transect$LAT, f = .1)
vepPolygon <- polygon_from_extent(raster::extent(c(sbbox[1],sbbox[3],sbbox[2],sbbox[4])), 
                                  proj4string='+init=EPSG:4269')


sq_map <- get_map(location = sbbox, source = c("osm"))

 
# Get NHD and NED (USA ONLY)
NHD <- get_nhd(template=vepPolygon, label='VEPIIN', force.redo = FALSE)
NED <- get_ned(template = vepPolygon, label = "VEPIIN",force.redo = FALSE)

# Plot the NHD data
slope = terrain(NED, opt='slope')
aspect = terrain(NED, opt='aspect')
hill = hillShade(slope, aspect, 40, 270)
plot(hill, col=grey(0:100/100), legend=FALSE, main='Area of interest')
plot(NED, col=rainbow(25, alpha=0.35), add=TRUE)

#   Convert rasters TO dataframes for plotting with ggplot
hdf <- rasterToPoints(hill); hdf <- data.frame(hdf)
colnames(hdf) <- c("X","Y","Hill")
ddf <- rasterToPoints(NED); ddf <- data.frame(ddf)
colnames(ddf) <- c("X","Y","DEM")

#   Create vectors for colour breaks
b.hs <- seq(min(hdf$Hill),max(hdf$Hill),length.out=100)
b.dem <- seq(min(ddf$DEM),max(ddf$DEM),length.out=100)


#Plot both together with transparency with DEM layer----
library(scales)
cols<-c(#"#06407F", "#317A9D","#4ABEBB",
  #"#40AE89", "#467B5D","#3C6D4D", "#1A572E", "#034C00", "#045D03", "#6C975F", "#6B823A", "#88A237",
  "#C5D16B", "#DDE580", "#FFF6AE", "#FBCB81", "#F0B16A",
  "#F2B16D", "#D18338", "#B16F33", "#825337")#, "#66422A", "#4F2C0C")
custom_cols <- c("#33602a","#75bd65","#c5e3be","#fff8eb")
green_pal <- c("#33602a",  "#3d7231","#468439", "#4f9541",  "#59a748", "#65b554","#75bd65","#85c477","#95cc89","#a5d49a","#b5dbac","#c5e3be","#d5ebd0","#e5f2e2","#f5faf3")
col <- terrain.colors(5)
colfunc <- colorRampPalette(green_pal)



#   Try to plot both together with transparency on the DEM layer
#Both Basin Map
ggplot(hdf) +
  geom_raster(data=ddf,aes(X,Y,fill=DEM,alpha=0.1)) +
  scale_fill_gradientn(name="Altitude",colours = rev(colfunc(100))) +
  geom_raster(aes(X,Y,alpha=Hill), fill = "grey20") +
  scale_alpha(range = c(1, 0)) +
  coord_equal()+
  guides(alpha = FALSE) +
  geom_polygon(data=fortify(NHD$`Area`), aes(x=long, y=lat, group=group),color="steelblue1",fill="steelblue1") +
  geom_polygon(data=fortify(NHD$`Waterbody`), aes(x=long, y=lat, group=group),fill="lightblue",color="lightblue") +
  geom_point(data=gageGPS, aes(x=dec_long_va, y=dec_lat_va), fill= "black", pch=23, size=4) +
  geom_point(data=source,  aes(x=lon, y = lat, group=Type, color=Type), size=4)+
  #scale_color_brewer(palette="Dark2")+ 
  scalebar(dist = 2, dd2km = TRUE, model  = "WGS84", y.min=41.99, y.max=42.17, x.min=-74.4, x.max=-74.2, anchor=c(x=-74.39, y=42.16), location="topleft")+
  north(y.min=41.99, y.max=42.17,   x.min=-74.4, x.max=-74.2, scale=0.1, symbol=14,   location="bottomright")+
  theme_void()
ggsave("Output/WholeSourcemap.png")

#Woodland Map
Wood_Source<-subset(source,Basin=="Woodland Creek")
sbbox <- make_bbox(lon = Wood_Source$lon, lat = Wood_Source$lat, f = .1)
sbbox[1]<- -74.4
sbbox[2]<- 41.99
#sbbox[3]<- -74.3
sbbox[4]<- 42.09

```

