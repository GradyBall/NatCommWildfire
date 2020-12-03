#################################
# This script takes masterdata and spatial layers to create
# Figure 1.

#INPUTS: 
## "./data/200414_MASTER_V4.csv"
## "./data/biomes.csv"

#OUTPUTS: 
## Figure 2

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

w<-8
h<-2.5
### END ---------------------------------------------------

### Time-series graph #####################################
#bin by year
m_y <- m0 %>% group_by(year) %>% 
  summarize(area_km2=sum(sq_km),len_km=sum(len[!is.na(len)]),count=n())

#set up data for Theil-Sen slope calculations
x <- as.numeric(m_y$year)
ya <- m_y$area_km2
yl <- m_y$len_km

#calculate slopes and significance
summary(mblm(ya~x))[[4]]
summary(mblm(yl~x))[[4]]

ci <- confint(mblm(yl~x))
ci[2,1]

#create strings to report slope statistics
ma <- "+298 sq. km / year, p < 0.0001"
ml <- "+342 km / year, p < 0.0001"

#set up helper function to show Sen slopes
sen <- function(..., weights = NULL) {
  mblm::mblm(...)
}
#make time-series graph
yl <- ggplot(data=m_y,aes(x=year,y=len_km)) + 
  geom_smooth(method=sen,color="sienna3",lwd=1,se=T) + 
  geom_line(color="gray30",lwd=1) + geom_point(size=3.5,color="black") + 
  geom_point(aes(color=len_km),size=2.2) + theme_classic() + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  labs(x="",y=expression(paste(SL[BA]," (km)")),color=expression(paste(SL[BA]," (km)"))) + 
  ylim(0,35000) + scale_x_continuous(breaks=c(1985,1990,1995,2000,2005,2010),
                                     expand = c(0.02,0.02)) +
  scale_color_viridis_c(option="inferno",direction=-1,trans="sqrt",breaks=c(2000,14000,34000)) +
  annotate("text",x=1985,y=max(m_y$len_km)-2000,label=ml,hjust=0) 
### END ---------------------------------------------------


### Heatmap figure ########################################
m_h <- m0 %>% group_by(year,biome) %>% 
  summarize(area_km2=sum(sq_km),len_km=sum(len[!is.na(len)]))
m_sum <- m0 %>% group_by(year) %>% 
  summarize(biome=as.integer(0),area_km2=sum(sq_km),len_km=sum(len[!is.na(len)]))

m_h2 <- full_join(m_sum,m_h)
m_h2$biome3 <- factor(m_h2$biome,levels=c("2","4","6","9","7","5","3","8","1","0"))
m_h$biome2 <- factor(m_h$biome,levels=c("2","4","6","9","7","5","3","8","1"))

biomes_abbrev <- as.list(c("CD","MWCF","MC","SCSP","UGM","WD","WCSP","WC","WSMP"))
biomes_no_fires <- as.list(c("MWCF","SCSP","WD","WSMP","WCSP","UGM","MC","WC","CD"))
biomes_abbrev2 <- as.list(c("ALL","CD","MWCF","MC","SCSP","UGM","WD","WCSP","WC","WSMP"))
biomes_no_fires2 <- as.list(c("MWCF","SCSP","WD","WSMP","WCSP","UGM","MC","WC","CD","ALL"))

#Set up color endmembers for gradient
clist2 <- list("#F9C7A8","#F39B9B","#EC6F8D","#D74A88","#A93790","#7B2297","#5C1886","#461266","#300D46") 
low_c <- "#FFFF99"
hi_c <- "#990000"

#make graph
hl <- ggplot(data=m_h,aes(x=as.factor(year),y=as.factor(biome2),fill=len_km)) + geom_tile() +
  theme_classic() + labs(y="Ecoregion",x="Year",fill=expression(paste(SL[BA]," (km)"))) + 
  scale_fill_viridis_c(option="inferno",direction=-1,
                       na.value="gray",trans="sqrt",breaks=c(100,5000,14000)) +
  scale_y_discrete(labels=biomes_no_fires) + 
  scale_x_discrete(breaks=c("1985","1990","1995","2000","2005","2010")) +
  theme(panel.background=element_rect(fill="gray", color="gray"))

plot_grid(yl,hl,nrow=2,align="h",rel_heights=c(0.4,0.6),labels=c("A","B"))
ggsave("./figures/Figure2_v2.pdf",height=h*2,width=w)
### END ---------------------------------------------------


