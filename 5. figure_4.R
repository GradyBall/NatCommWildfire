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
## Figure 4

# DISCLAIMER: This script make sures of an R project, and 
# assumes the following file structure:
# 1. all data files are located within a 'data' subfolder
# 2. all outputs are written to a 'figures' or 'tables' subfolder

#Created 9/13/20 by PR
# Cleaned up 11/15/20 by PR
# R v3.5.1
#################################


### Set up environment ####################################
rm(list = ls())
library(cowplot) #plot_grid
library(dplyr)
library(ggplot2)
library(ggpubr)
library(ggsci) #great for testing a number of preset color schemes
library(mblm) #Theil-Sen slopes
library(png)
library(reshape2)
library(stringr) #allows text wrapping in graph annotations

m0 <- read.csv("./data/200414_MASTER_V4.csv")
biome_df <- read.csv("./data/biomes.csv")
biomes <- as.list(as.vector(biome_df$name))
### END ---------------------------------------------------


### Table S1 statistics ###################################
s0 <- read.csv("./data/200331_grady_table5.csv")
s1 <- m0 %>% group_by(biome) %>% summarize(area_burned=sum(sq_km),
                                           len_burned=sum(len[!is.na(len)]))
s1$biome_full <- as.character(biomes)
s2 <- m0 %>% group_by(biome) %>% summarize(count=n_distinct(fire_id))
biomes_abbrev <- as.list(c("CD","MWCF","MC","SCSP","UGM","WD","WCSP","WC","WSMP"))

t0 <- data.frame(biome=s1$biome_full,abbrev=unlist(biomes_abbrev),
                 no_fires=s2$count,
                 t_area=s0$t_area_km2_g,b_area=s1$area_burned,
                 t_len=s0$t_len_km_g,b_len=s1$len_burned)
t0$p_area <- t0$b_area/t0$t_area
t0$p_len <- t0$b_len/t0$t_len
t0$p_fires <- t0$no_fires/sum(t0$no_fires)
t1 <- t0[,c(1,2,3,10,4,5,8,6,7,9)]
### END ---------------------------------------------------


### Figure 4 ##############################################
clist <- list("#B5E2EF","#DA266C","#33CFFF","#6865FF","#FF5BFF","#689BFF","#910098","#A6FFD1","#33CF6E") 

b0 <- m0 %>% group_by(year,biome) %>% summarize(area_km2=sum(sq_km),len_km=sum(len[!is.na(len)]))
b0$t_area <- t0[b0$biome,4]
b0$p_area <- (b0$area_km2 / b0$t_area) * 100
b0$t_len <- t0[b0$biome,6]
b0$p_len <- (b0$len_km / b0$t_len)*100
b1 <- b0 %>% group_by(biome) %>% summarize(p_len=sum(p_len))
b1$p_len2 <- b1$p_len*2

#NOTE: error bars are 1/2 the percentage of the 95% CI because p_len2 is p_len*2 for graphing purposes
p0 <- ggplot(data=b1,aes(x=factor(biome))) + 
  geom_bar(stat="identity",aes(y=p_len2,fill=factor(biome)),alpha=0.5,show.legend=F,color="gray70") +
  geom_errorbar(aes(ymin=p_len2*0.843023256, ymax=p_len2*1.302325581),width=.2,color="gray70") +
  geom_bar(stat="identity",aes(y=p_len,fill=factor(biome)),alpha=0.5,show.legend=F,color="black") +
  theme_bw() + theme(legend.title=element_blank()) +
  theme(legend.position=c(0.7,0.85),axis.text.x = element_text(angle = 45,vjust=0.6)) + 
  theme(panel.grid.minor = element_blank(),panel.grid.major = element_blank()) +
  labs(x="Biome",y="% total length burned") + scale_x_discrete(labels=biomes_abbrev) + 
  scale_fill_manual(values=clist) +
  annotate(geom="text",x=1,3.1,label=expression(paste(SL[BA])),angle = 90) + 
  annotate(geom="text",x=1,10,label=expression(paste(SL[LE])),angle = 90)

p0
ggsave("./figures/Figure_4.pdf",height=4,width=4.5)
write.csv(b1, "./data/201130_Figure4_data_table.csv")
### END ---------------------------------------------------
