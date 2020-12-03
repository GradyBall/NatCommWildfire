#################################
# This script takes masterdata and spatial layers to create
# Figure 1.

#INPUTS: 
## "./data/gis/cb_2018_us_state_5m.shp"
## "./data/gis/las_conchas_fire_perimeter_final_july19.shp"
## "./data/gis/Surface_Water_Basins.shp"
## "./data/gis/RioGrande_polyline.shp"
## "./data/gis/Rio_Peralta.shp"
## "./data/gis/jemez/Jemez.shp"
## "./data/gis/above_coh/AboveCochiti_burnedHUCS.shp"
## "./data/gis/below_coh/Below_Cochiti_burnedHUC.shp"
## "./data/gis/NHDFlowline.shp"
## "./data/gis/CitiesCommunities.shp"
## "./data/200303_dvh_wq.csv"
## "./data/200908_coh_wq.csv"

#OUTPUTS: 
## Components to assemble Figure 3

# DISCLAIMER: This script make sures of an R project, and 
# assumes the following file structure:
# 1. all data files are located within a 'data' subfolder
# 2. all outputs are written to a 'figures' or 'tables' subfolder

#Created 9/02/20 by PR
# Cleaned up 11/15/20 by PR
# R v3.5.1
#################################


### Set up environment ####################################
rm(list = ls())
library(cowplot) 
library(ggplot2)
library(ggsn)
library(ggspatial)
library(grid)
library(gridExtra)
library(raster)
library(scales) 
library(sf)
library(sp) 
library(tmaptools)

#Read states layers
s_df <- st_read("./data/gis/cb_2018_us_state_5m.shp",quiet=TRUE)
s_us <- st_transform(s_df,4269)
s_west <- s_us[c(s_us$NAME=="Arizona"|s_us$NAME=="California"|s_us$NAME=="Colorado"|s_us$NAME=="Idaho"|
                   s_us$NAME=="Montana"|s_us$NAME=="Nevada"|s_us$NAME=="New Mexico"|s_us$NAME=="Oregon"|
                   s_us$NAME=="Utah"|s_us$NAME=="Washington"|s_us$NAME=="Wyoming"),]
s0 <- st_union(s_west)

#Las Conchas layer
lc_df <- st_read("./data/gis/las_conchas_fire_perimeter_final_july19.shp",quiet=TRUE)
lc0 <- st_transform(lc_df,4269) 

#MRG basin layer
basin_df <- st_read("./data/gis/Surface_Water_Basins.shp",quiet=TRUE)
basin0 <- st_transform(basin_df,4269) 

#Rio Grande layer
rg_df <- st_read("./data/gis/RioGrande_polyline.shp",quiet=TRUE)
rg0 <- st_transform(rg_df,4269) 
rg1 <- crop_shape(rg0,basin0[basin0$BASIN=="MIDDLE RIO GRAND",])
rg2 <- st_crop(rg1,xmin=-107.19133,xmax=-106.14720,ymin=33.14970,ymax=35.59231) #impacted RG reach
rg3.blue <- st_crop(rg1,xmin=-107.19133,xmax=-106.14720,ymin=34.9,ymax=36.21178)
rg3.red <- st_crop(rg1,xmin=-107.19133,xmax=-106.14720,ymin=34.9,ymax=35.6)

#Peralta layer
per_df <- st_read("./data/gis/Rio_Peralta.shp",quiet=TRUE)
per0 <- st_transform(per_df,4269)

#Jemez layer and Above/Below Cochiti layers
j_df <- st_read("./data/gis/jemez/Jemez.shp",quiet=TRUE)
above <- st_read("./data/gis/above_coh/AboveCochiti_burnedHUCS.shp",quiet=TRUE)
below <- st_read("./data/gis/below_coh/Below_Cochiti_burnedHUC.shp",quiet=TRUE)
j0 <- st_transform(j_df,4269)
above0 <- st_transform(above,4269)
below0 <- st_transform(below,4269)

#RG watershed flowlines
rg_df <- st_read("./data/gis/NHDFlowline.shp",quiet=TRUE)
flow0 <- st_transform(rg_df,4269)
flow1 <- st_zm(flow0)
flow2 <- crop_shape(flow1,basin0[basin0$BASIN=="MIDDLE RIO GRAND",],polygon=T)
flow_rg <- flow1[flow1$GNIS_Name=="Rio Grande",]
flow_rg <- st_crop(flow_rg,xmin=st_bbox(flow_rg)[[1]],xmax=st_bbox(flow_rg)[[3]],
                  ymin=31.5,ymax=st_bbox(flow_rg)[[4]])

#ABQ outline
abq_df <- st_read("./data/gis/CitiesCommunities.shp",quiet=TRUE)
abq0 <- st_transform(abq_df,4269)
### END ---------------------------------------------------


### Context map ###########################################

blue <- "#547AA5"
map1 <- ggplot() + geom_sf(data=s_west,fill="white",color="gray50",lwd=0.3) + 
  geom_sf(data=s0,lwd=0.4,color="black",fill=NA) + 
  geom_sf(data=flow_rg,color=blue,lwd=0.4) +
  geom_sf(data=basin0[basin0$BASIN=="MIDDLE RIO GRAND",],fill="ivory1",color="black",lwd=0.3) + 
  theme_map() + coord_sf(crs = "+proj=aea +lat_1=25 +lat_2=50 +lon_0=-100")
ggsave("./figures/Figure 3/map1.png",width=2.5,height=2.5)
### END ---------------------------------------------------


### Watershed map #########################################
hi_c <- "#990000"
low_c <- "#FFFF99"
red <- "#A4031F"
size_outline <- 2.25
size_shape <- 1.25

sites <- data.frame(longitude = c(-106.5574,-106.6442,-106.6729,-106.6820,-106.3202,-107.1920), 
                    latitude = c(35.3221,35.1936,35.0272,34.9499,35.6201,33.1541),
                    siteid=c("1. B550","2. AL","3. RB","4. I25","COH","EB"))

map2 <- ggplot() + geom_sf(data=basin0[basin0$BASIN=="MIDDLE RIO GRAND",],fill="ivory1") + 
  geom_sf(data=flow2,color="skyblue1",lwd=0.1,alpha=0.005) +
  geom_sf(data=basin0[basin0$BASIN=="MIDDLE RIO GRAND",],fill=NA) + 
  geom_sf(data=lc0,color='sienna',fill=low_c,lwd=0.4,alpha=0.4) +
  geom_sf(data=flow2[flow2$GNIS_Name=="Rio Puerco",],color=blue,lwd=0.5) +
  geom_sf(data=j0,color=blue,lwd=0.5) + geom_sf(data=per0,color=red,lwd=0.8) +
  geom_sf(data=rg1,color=blue,lwd=1) + geom_sf(data=rg2,color=red,lwd=1) +
  geom_point(data=sites[1:4,],aes(x=longitude,y=latitude),size=size_outline,color="black") +
  geom_point(data=sites[1:4,],aes(x=longitude,y=latitude),size=size_shape,color="white") +
  geom_point(data=sites[5:6,],aes(x=longitude,y=latitude),size=size_outline,shape=17,color="black") +
  geom_point(data=sites[5:6,],aes(x=longitude,y=latitude),size=size_shape,shape=17,color="white") +
  annotation_scale(location="br",style = "ticks",width_hint = 0.3) + 
  annotation_north_arrow(location = "br", width = unit(0.6,"cm"), height = unit(0.7,"cm"), 
                         pad_x = unit(1, "cm"), pad_y = unit(1, "cm")) + theme_map() 
### END ---------------------------------------------------


### MRG map ###############################################
abq_df <- st_read("./data/gis/CitiesCommunities.shp")
abq0 <- st_transform(abq_df,4269) 

map3 <- ggplot() + geom_sf(data=lc0,color='sienna',fill=low_c,lwd=0.5,alpha=0.4) +
  geom_sf(data=rg3.blue,color=blue,lwd=1.2) + 
  geom_sf(data=rg3.red,color=red,lwd=1.2) + geom_sf(data=per0,color=red,lwd=1) +
  geom_sf(data=abq0[1,],color="gray20",fill="gray",alpha=0.2) +
  geom_point(data=sites[1:4,],aes(x=longitude,y=latitude),size=4,color="black") +
  geom_point(data=sites[1:4,],aes(x=longitude,y=latitude),size=2.5,color="white") +
  geom_point(data=sites[5,],aes(x=longitude,y=latitude),size=4,shape=17,color="black") +
  geom_point(data=sites[5,],aes(x=longitude,y=latitude),size=2.5,shape=17,color="white") +
  annotation_scale(location="br",style = "ticks",width_hint = 0.6) + theme_map()
#ggsave("./figures/Figure 3/map3.pdf",width=4,height=5)
### END ---------------------------------------------------


### DO sag time-series ####################################
wq_dvh <- read.csv("./data/200303_dvh_wq.csv")
wq_dvh$dateTime <- as.POSIXct(wq_dvh$dateTime,format="%Y-%m-%d %H:%M:%S")
wq <- subset(wq_dvh,dateTime >= as.POSIXct("2011-07-30 01:00:00 MST") &
               dateTime <= as.POSIXct("2011-08-02 00:00:00 MST"))
wq_coh <- read.csv("./data/200908_coh_wq.csv")
wq_coh$dateTime <- as.POSIXct(wq_coh$datetime,format="%m/%d/%y %H:%M")
head(wq_coh)
wq2 <- subset(wq_coh,dateTime >= as.POSIXct("2011-07-30 01:00:00 MST") &
                dateTime <= as.POSIXct("2011-08-02 00:00:00 MST"))
colnames(wq2)[4]<-"do"
wq$site_no <- paste0("S",substring(wq$site,1,1))
wq2$site_no <- "Cochiti"
wq3 <- rbind(wq2[,c(5,4,6)],wq[,c(2,3,8)])
head(wq3)
  
do_graph <- ggplot(wq3,aes(x=dateTime,y=do)) + geom_line() + 
  geom_point(size=1.5) + geom_point(aes(color=do),size=1,show.legend=F) +
  facet_wrap(vars(site_no),nrow=5) + 
  scale_color_viridis_c(option="inferno",direction=1) + 
  scale_y_continuous(position = "right", limits = c(0, max(wq3$do) + 1)) +
  labs(x="\n\n",y=expression(paste("Dissolved oxygen (mg ",L^-1, ")"))) +   theme_bw() +
  theme(strip.background=element_rect(fill="gray20")) + 
  theme(strip.text = element_text(colour='white')) + 
  theme(axis.text.x=element_text(angle=60,vjust=0.5))
ggsave("./figures/Figure 3/do.pdf",width=2.3,height=6)
### END ---------------------------------------------------


### Make map panels #######################################
plot1 <- plot_grid(map1,NULL,nrow=2,rel_heights=c(0.4,0.8))
plot1.1<-plot_grid(plot1,map2,nrow=1,rel_widths=c(0.5,1))
ggsave("./figures/Figure 3/Panel1.pdf",width=6,height=6)

#plot2.0 <- plot_grid(NULL,do_graph,nrow=2,rel_heights=c(0.1,1))
map3
ggsave("./figures/Figure 3/map3.pdf",width=3,height=6)

do_graph
ggsave("./figures/Figure 3/do_sags.pdf",width=2.3,height=6)

plot2 <- plot_grid(map3, NULL, do_graph, 
                   ncol=3,rel_widths=c(1,0.3,0.8))
ggsave("./figures/Figure 3/Panel2.pdf",width=6,height=6)



### END ---------------------------------------------------


### Layers for legend #####################################
ggplot() + geom_sf(data=lc0,color='sienna',fill=low_c,lwd=0.4,alpha=0.4) + theme_map()
ggsave("./figures/Figure 3/lc.pdf",width=1,height=2)

ggplot() + geom_sf(data=abq0[1,],color="gray20",fill="gray",alpha=0.2) + theme_map()
ggsave("./figures/Figure 3/abq.pdf",width=1.5,height=1.5)

plot_grid(plot1.1,map3,do_graph,nrow=1,rel_widths=c(1,0.5,0.4))
ggsave("./figures/Figure 3/figure.png",width=12,height=6)
### END ---------------------------------------------------
