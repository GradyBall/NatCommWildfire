#################################
# This script takes masterdata and spatial layers to create
# Figure 1.

#INPUTS: 
## "./data/gis/NA_CEC_Eco_Level2.shp"
## "./data/gis/mtbs_perims_DD.shp"
## "./data/gis/cb_2018_us_state_5m.shp"
## "./data/200414_MASTER_V4.csv"

#OUTPUTS: 
## Layers for Figure 1

# DISCLAIMER: This script make sures of an R project, and 
# assumes the following file structure:
# 1. all data files are located within a 'data' subfolder
# 2. all outputs are written to a 'figures' or 'tables' subfolder

# Created 4/21/20 by PR
# Cleaned up 11/15/20 by PR
# R v3.5.1
#################################

### Set up environment ####################################
rm(list = ls())
library(cowplot)
library(geos)
library(ggplot2)
library(ggpubr)
library(ggsci)
library(lwgeom)
library(scales)
library(sf)
library(tmaptools)
### END ---------------------------------------------------


### Spatial data prep #####################################
# Bring in three spatial layers:
# 1) b (biomes): EPA level 2 ecoregions, 
# 2) f (fires): MBTS wildfire polygons, and 
# 3) s (states): state boundaries baselayer

b0 <- st_read("./data/gis/NA_CEC_Eco_Level2.shp",quiet=TRUE)
f0 <- st_read("./data/gis/mtbs_perims_DD.shp",quiet=TRUE)
s0 <- st_read("./data/gis/cb_2018_us_state_5m.shp",quiet=TRUE)

# Bring in length burned data, trim to used columns
g0 <- read.csv("./data/200414_MASTER_V4.csv")
g1 <- g0[,c("fire_id","len")]

# Force all layers to the same projection so they play well together
b1 <- st_transform(b0,4269)
f2 <- st_transform(f0,4269)%>% 
  st_set_precision(1000000) %>% 
  st_make_valid()
s_df0 <- st_transform(s0,4269)

# Convert to points and add stream length
f3 <- f2[f2$Fire_ID %in% g0$fire_id,]
f4 <- merge(f3,g1,by.x="Fire_ID",by.y="fire_id") #filter fire polygons by master data fire IDs
f5 <- f4[!is.na(f4$len),] #remove fires without stream length burned reported
f6 <- f5[f5$Fire_ID!="NM3645510259320110525",]

#limit states layer to CONUS
s_cont <- s_df0[(s_df0$NAME!="American Samoa"&
                   s_df0$NAME!="Commonwealth of the Northern Mariana Islands"&
                   s_df0$NAME!="Puerto Rico"&
                   s_df0$NAME!="United States Virgin Islands"&
                   s_df0$NAME!="Guam"&s_df0$NAME!="Alaska"&
                   s_df0$NAME!="Hawaii"),]

#limit states layer to only western US
s1 <- s_df0[(s_df0$NAME=="Arizona"|
               s_df0$NAME=="California"|
               s_df0$NAME=="Colorado"|
                  s_df0$NAME=="Idaho"|
               s_df0$NAME=="Montana"|
               s_df0$NAME=="New Mexico"|
                  s_df0$NAME=="Nevada"|
               s_df0$NAME=="Oregon"|
               s_df0$NAME=="Utah"|
                  s_df0$NAME=="Washington"|
               s_df0$NAME=="Wyoming"),]

# Create CONUS and western US outlines
s_outline <- st_union(s1)
s_us_outline <- st_union(s_cont)

#trim biome based on state layer
b2 <- st_make_valid(b1) #prepare shapefile for cropping to s1 polygon
b3 <- crop_shape(b2,s1,polygon=TRUE)

#clean up to free up memory
rm(b0,b1,b2,f0,f1,f2,f3,f4,f5)
### END ---------------------------------------------------


### Create Fig 1 map ######################################
#Set up color endmembers for gradient
low_c <- "#FFFF99" #original light yellow
low2 <- "#F39B9B" # hot pink
low3 <- "#300D46" #dark purple
hi_c <- "#990000" #original dark red
hi2 <- "#55362E" #dark purple
hi3 <- "orange"

#Order by precip: 2,8,3,5,9,4,7,1,6
#MWCF,WC,MC,UGM,WSMP,SCSP,WCSP,CD,WD
clist5_1_ord <- list("#B5E2EF","#DA266C","#33CFFF","#6865FF","#FF5BFF","#689BFF","#910098","#A6FFD1","#33CF6E") 

#Choose color scheme
colorscheme <- clist5_1_ord

p1 <- ggplot() + geom_sf(data=s_outline,color="black",lwd=1) + 
  geom_sf(data=s1,fill="white",alpha=0) + 
  geom_sf(data=b3,aes(fill=NA_L2NAME),color="gray10",alpha=0.5) +
 geom_sf(data=f6,color="#800000",fill="yellow",lwd=0.1) +
  guides(fill=guide_legend(override.aes=list(size=1))) + labs(fill="") +
    scale_fill_manual(values=colorscheme,labels=c("Cold Deserts (CD)",
                          "Marine West Coast Forest (MWCF)",
                          "Mediterranean California (MC)",
                          "South Central Semiarid Prairies (SCSP)",
                          "Upper Gila Mountains (UGM)",
                          "Warm Deserts (WD)",
                          "West-Central Semiarid Prairies (WCSP)",
                          "Western Cordillera (WC)",
                          "Western Sierra Madre Piedmont (WSMP)")) + 
    coord_sf(crs = "+proj=aea +lat_1=25 +lat_2=50 +lon_0=-100") + 
  theme(legend.title = element_blank(), legend.text = element_text(size = 8)) +
    theme_bw() + theme(panel.border = element_blank(),
                       panel.grid.minor.x = element_blank())

p1 + theme(legend.position="none")
ggsave("./figures/Figure 1/Fig1_map2.pdf")

as_ggplot(get_legend(p1))
ggsave("./figures/Figure 1/Fig1_legend.pdf")

### END ---------------------------------------------------


### Create Fig 1 density plots ############################
m0 <- read.csv("./data/200414_MASTER_V4.csv")

#pull lat and long from fires 
f7 <- st_centroid(f6)
f_latlong <- data.frame(f7$Fire_ID,f7$len,st_coordinates(f7$geometry))
colnames(f_latlong)[1] <- "fire_id"
m_latlong <- merge(f_latlong,m0,by="fire_id")
colnames(m_latlong)[3] <- "long"
colnames(m_latlong)[4] <- "lat"

d_long <- ggplot(data=m_latlong) + 
  geom_density(aes(x=long,color=as.factor(biome),fill=as.factor(biome)),alpha=0.2,lwd=0.5) +
  geom_density(aes(x=long,y=..scaled..),linetype="dashed",lwd=1,show.legend=F) + 
  scale_color_manual(values=unlist(colorscheme)) + scale_fill_manual(values=unlist(colorscheme)) + 
  theme_classic() + theme(legend.position="none") +
  scale_x_continuous(breaks=c(-125,-115,-105),labels=c("125°W","115°W","105°W")) +
  scale_y_continuous(expand = c(0, 0),limits=c(0.,1.1),breaks=c(0,1),labels=c("min","max")) +
  labs(x="",y="Density")

d_lat <- ggplot(data=m_latlong) + 
  geom_density(aes(x=lat,color=as.factor(biome),fill=as.factor(biome)),alpha=0.2,lwd=0.5) +
  geom_density(aes(x=lat,y=..scaled..),linetype="dashed",lwd=1,show.legend=F,outline.type="upper") + 
  scale_color_manual(values=unlist(colorscheme)) + scale_fill_manual(values=unlist(colorscheme)) + 
  theme_classic() + theme(legend.position="none") +
  scale_x_reverse(breaks=c(35,40,45),labels=c("35°N","40°N","45°N")) +
  scale_y_continuous(expand = c(0, 0),limits=c(0.,1.1),breaks=c(0,1),labels=c("min","max")) +
  labs(x="",y="Density")

pdf("./figures/Figure 1/Fig1_density_long.pdf",height=1.6,width=5)
d_long
dev.off() 

pdf("./figures/Figure 1/Fig1_density_lat.pdf",height=1.6,width=7)
d_lat
dev.off() 

s2 <- st_union(s1)
s_us <- st_union(s_cont)
us1 <- ggplot() + geom_sf(data=s_us,fill="white",color="gray50",lwd=1) + 
  geom_sf(data=s2,fill="white",color=hi_c,lwd=2) +
  coord_sf(crs = "+proj=aea +lat_1=25 +lat_2=50 +lon_0=-100") + 
  theme_map() 
  
pdf("./figures/Figure 1/Fig1_us_map.pdf")
us1
dev.off() 
### END ---------------------------------------------------