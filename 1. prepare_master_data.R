#################################
# This script takes data exported from Python and prepares
# it for analysis. Primarily, it reassigns biome for fires
# that burned in >1 biome to the biome with largest area
# burned.
#
# Filepaths are based on file architecture associated with
# an R project, which is located in the same directory as 
# folders "data" (inputs), "figures" and "tables" (outputs).
# GIS products are located in a "gis" sub-folder within the
# "data" folder.

# NOTE: Python output, and therefore the resulting master
# dataset, are based on spatial data products accessed at
# the start of the project, and therefore do not represent
# the current versions of MBTS, EPA, or NHD products used.

#INPUTS: 
## "./data/python_output.csv"
## "./data/gis/NA_CEC_Eco_Level2.shp"
## "./data/gis/mtbs_perims_DD.shp"
## ./data/gis/cb_2018_us_state_5m.shp"

#OUTPUTS: 
## "./data/200328_area_by_biome.csv"
## "./data/200414_MASTER_V4.csv"
## "./data/biomes.csv"

# DISCLAIMER: This script make sures of an R project, and 
# assumes the following file structure:
# 1. all data files are located within a 'data' subfolder
# 2. all outputs are written to a 'figures' or 'tables' subfolder

# Created 3/27/20 by PR
# Cleaned up 10/28/20 by PR
# R v3.5.1
#################################

### Set up environment ####################################
rm(list = ls())
library(cowplot) #plot_grid
library(dplyr)
library(ggplot2)
library(ggpubr)
library(ggsci)
library(lwgeom)
library(mblm)
library(png)
library(reshape2)
library(sf)
library(stringr) #allows text wrapping in graph annotations
library(tmaptools)
### END ---------------------------------------------------


### Bring in python output ################################
df1 <- read.csv("./data/python_output.csv")

df_sev <- df1 %>% group_by(na_l2name,gridcode) %>%
  summarize(acres=sum(acres),len=sum(len[!is.na(len)]))

df_sev_yr <- df1 %>% group_by(year,gridcode) %>%
  summarize(acres=sum(acres),len=sum(len[!is.na(len)]))

p1 <- ggplot(df_sev, aes(x=na_l2name,y=acres)) + 
  geom_bar(aes(color=as.factor(gridcode),fill=as.factor(gridcode)), stat="identity") +
  theme(axis.text.x = element_text(angle = 45)) + 
  scale_x_discrete(labels=c("CD","MWCF","MC","SCSP","UGM","WD","WCSP","WC","WSMP"))

p2 <- ggplot(df_sev, aes(x=na_l2name,y=len)) + 
  geom_bar(aes(color=as.factor(gridcode),fill=as.factor(gridcode)), stat="identity") +
  theme(axis.text.x = element_text(angle = 45)) + 
  scale_x_discrete(labels=c("CD","MWCF","MC","SCSP","UGM","WD","WCSP","WC","WSMP"))

p3 <- ggplot(df_sev_yr, aes(x=year, y=acres,group=as.factor(gridcode))) + 
  geom_line(aes(color=as.factor(gridcode))) + geom_point() +
  geom_smooth(aes(color=as.factor(gridcode)), method="lm", se=F)

p4 <- ggplot(df_sev_yr, aes(x=year, y=len,group=as.factor(gridcode))) + 
  geom_line(aes(color=as.factor(gridcode))) + geom_point() + 
  geom_smooth(aes(color=as.factor(gridcode)), method="lm", se=F)

plot_grid(p1,p2,p3,p4,nrow=2)
ggsave("./figures/201109_fire_intensity.png",height=8,width=12)

# Create list of biomes
biomes <- as.list(levels(df1$na_l2name))
df1$biome <- match(df1$na_l2name,biomes)

# Remove duplicates so each fire has one row
# The duplicated rows here indicate a fire burned in more than one biome
df_bin <- df1 %>% group_by(fire_id,biome) %>% 
  summarize(year=mean(year),area=mean(acres),len2=sum(len)) #total of 8202 combos
dup <- df_bin[duplicated(df_bin$fire_id),] #525 duplicate rows (3 are triplicates)
lost <- dup[duplicated(dup$fire_id),]
nodup <- df_bin[!duplicated(df_bin$fire_id),]
### END ---------------------------------------------------


### Bring in spatial layers ###############################
b_df <- st_read("./data/gis/NA_CEC_Eco_Level2.shp",quiet=TRUE)
f_df <- st_read("./data/gis/mtbs_perims_DD.shp",quiet=TRUE)
s_df <- st_read("./data/gis/cb_2018_us_state_5m.shp",quiet=TRUE)

#force same projection for all shapefiles
b_df0 <- st_transform(b_df,4269)
f_df0 <- st_transform(f_df,4269) 
s_df0 <- st_transform(s_df,4269)

#limit states layer to only western US
s_df1 <- s_df0[(s_df0$NAME=="Arizona"|s_df0$NAME=="California"|s_df0$NAME=="Colorado"|
                  s_df0$NAME=="Idaho"|s_df0$NAME=="Montana"|s_df0$NAME=="New Mexico"|
                  s_df0$NAME=="Nevada"|s_df0$NAME=="Oregon"|s_df0$NAME=="Utah"|
                  s_df0$NAME=="Washington"|s_df0$NAME=="Wyoming"),]

#trim fire and biome based on state layer
#previous scripts using st_buffer(b_df0,0) worked, but not here (something about arc_degrees??)
b_df01 <- st_make_valid(b_df0)
b_df1 <- crop_shape(b_df01,s_df1,polygon=TRUE)
f_df1 <- crop_shape(f_df0,s_df1,polygon=TRUE)

#cleanup
rm(df,df0,df_nvwa,b_df,b_df0,f_df,f_df0,s_df,s_df0)
### END ---------------------------------------------------


### Analyze burned extent by biome ########################
f_df2 <- subset(f_df1,Fire_ID %in% dup$fire_id)

#create two empty lists for the for loop
b_list <- list()
f_list <- list()

# For loop that crops fire layer to each individual biome
# Print out each biome as an indication R hasn't frozen...
for(i in 1:9)
{
  b_list[[i]] <- b_df1[b_df1$NA_L2NAME==as.character(biomes[i]),]
  f_list[[i]] <- crop_shape(st_make_valid(f_df2),b_list[[i]],polygon=T)
  f_list[[i]]$biome <- as.character(biomes[i])
  print(i)
}

# Recombine list of fires into dataframe
f_df3 <- do.call(rbind,f_list)

# Reorder by Fire_ID instead of biome
f_df4 <- arrange(f_df3,Fire_ID)

# Calculate area in m2 for each part of each fire
f_df4$Area_m2 <- st_area(f_df4)

#remove geometry and export data
f_df_export <- as.data.frame(f_df4[,c(1,3,7,8,10)])
f_df_export <- f_df_export[,c(1:5)]
write.csv(f_df_export,"./data/200328_area_by_biome.csv")

#housekeeping
rm(b_df01,b_df1,b_list,f_list)
### END ---------------------------------------------------


### Create/export master data #############################
#rename dataframes to merge because I'm lazy
f <- f_df_export
g <- nodup
g0 <- g[!(g$fire_id %in% f$Fire_ID),]

#prepare f: add len, convert area, and bin
colnames(f)[1] <- "fire_id"
f0 <- merge(f,g,by="fire_id")
f0$biome2 <- match(f0$biome.x,biomes)
f0$sq_km0 <- as.numeric(f0$Area_m2/1000000)
f1 <- f0 %>% group_by(fire_id) %>% summarize(biome=biome2[which.max(Area_m2)],year=mean(year),
                                             sq_km=sum(sq_km0),len=mean(len2))

#organize columns
g0$sq_km <- g0$area*0.00404686 #convert acres to sq. km
g0$len <- g0$len2
g1 <- g0[,-which(names(g0) %in% c("area","len2"))]

#Create and write master dataset
m0 <- rbind(as.data.frame(f1),as.data.frame(g1))
write.csv(m0,"./data/200414_MASTER_V4.csv")

#Create and write biomes dataset
biomes_abbrev <- as.list(c("CD","MWCF","MC","SCSP","UGM","WD","WCSP","WC","WSMP"))
biome_df <- data.frame(name=unlist(biomes),abbrev=unlist(biomes_abbrev))
write.csv(as.vector(biome_df),"./data/biomes.csv")
### END ---------------------------------------------------

