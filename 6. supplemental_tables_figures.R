#################################
#This script to make supplemental Figures 1 and 2
#and supplemental Tables 1-3

#INPUTS
# "./data/200414_MASTER_V4.csv"
# "./data/biomes.csv"
# "./data/200331_grady_table5.csv"
# "./data/200910_DO_sags.csv"

#OUTPUTS
# "./tables/table_s1_prep.csv"
# "./tables/table_s2_prep.csv"
# "./tables/table_s3_prep.csv"
# "./figures/201109_TableS3_graphs.png"
# "./figures/Figure_S1_2.pdf"
# "./figures/Figure_S1.pdf"
# "./figures/Figure_S2.pdf"

# DISCLAIMER: This script make sures of an R project, and 
# assumes the following file structure:
# 1. all data files are located within a 'data' subfolder
# 2. all outputs are written to a 'figures' or 'tables' subfolder

# Collated 11/15/20 by PR
# R v3.5.1
#################################


### Set up environment ####################################
rm(list = ls())
library(cowplot) #plot_grid
library(dplyr)
library(ggplot2)
library(ggrepel)
library(ggpubr)
library(ggsci) 
library(mblm) #Theil-Sen slopes
library(png)
library(reshape2)
library(stringr) 

m0 <- read.csv("./data/200414_MASTER_V4.csv")
biome_df <- read.csv("./data/biomes.csv")
biomes <- as.list(as.vector(biome_df$name))
### END ---------------------------------------------------


### Table S1 statistics ###################################
s0 <- read.csv("./data/200331_grady_table5.csv") # from Ball Thesis
s1 <- m0 %>% group_by(biome) %>% summarize(area_burned=sum(sq_km),
                                           len_burned=sum(len[!is.na(len)]))
s1$biome_full <- as.character(biomes)
s2 <- m0 %>% group_by(biome) %>% summarize(count=n_distinct(fire_id))
biomes_abbrev <- as.list(c("CD","MWCF","MC","SCSP","UGM","WD","WCSP","WC","WSMP"))

t0 <- data.frame(biome=s1$biome_full,abbrev=unlist(biomes_abbrev),
                 no_fires=s2$count,
                 t_area=s0$t_area_km2_g,b_area=s1$area_burned,
                 t_len=s0$t_len_km_g,b_len=s1$len_burned,
                 rain=s0$mean_rain.mm)
t0$p_area <- t0$b_area/t0$t_area
t0$p_len <- t0$b_len/t0$t_len
t0$p_fires <- t0$no_fires/sum(t0$no_fires)
t1 <- t0[,c(1,2,3,10,4,5,8,6,7,9)]
write.csv(t1,"./tables/table_s1_prep.csv")
### END ---------------------------------------------------


### Table S2 prep #########################################
m_h <- m0 %>% group_by(year,biome) %>% 
  summarize(area_km2=sum(sq_km),len_km=sum(len[!is.na(len)]))
m_h$biome3 <- factor(m_h$biome,levels=c("2","4","6","9","7","5","3","8","1"))
m_h$l_a <- m_h$len_km/m_h$area_km2

biomes_abbrev <- as.list(c("CD","MWCF","MC","SCSP","UGM","WD","WCSP","WC","WSMP"))
biomes_no_fires <- as.list(c("MWCF","SCSP","WD","WSMP","WCSP","UGM","MC","WC","CD"))

#export data for Table S2
m_h1 <- m_h[,c(1,2,4)]
m_h2 <- t(tidyr::spread(m_h1,year,len_km))
m_h2[1,] <- unlist(biomes_abbrev)
write.csv(m_h2,"./tables/table_s2_prep.csv",na="")
### END ---------------------------------------------------


### Table S3 prep #########################################
m_s <- m0 %>% group_by(year,biome) %>% 
  summarize(area_km2=sum(sq_km),len_km=sum(len[!is.na(len)]))
m_s$biome3 <- factor(m_s$biome,levels=c("2","4","6","9","7","5","3","8","1"))

df_stats <- data.frame(biome=NA,R2adj=NA,m=NA,p=NA,n=NA)
for(i in 1:length(biomes_abbrev))
{
  df <- m_s[m_s$biome==i,]
  df_stats[i,1] <- biomes_abbrev[[i]]
  df_stats[i,2] <- round(summary(lm(df$len_km~df$area_km2))[[9]],2)
  df_stats[i,3] <- round(summary(lm(df$len_km~df$area_km2))[[4]][2,1],2)
  df_stats[i,4] <- round(summary(lm(df$len_km~df$area_km2))[[4]][2,4],5)
  df_stats[i,5] <- nrow(df)
}
write.csv(df_stats,"./tables/table_s3_prep.csv")

clist <- list("#B5E2EF","#DA266C","#33CFFF","#6865FF",
              "#FF5BFF","#689BFF","#910098","#A6FFD1","#33CF6E") 

p0 <- ggplot(m_s, aes(x=area_km2)) + 
  geom_density(aes(fill=biome3),alpha=0.2,show.legend=F) +
  scale_x_log10() + scale_y_continuous(breaks = c(0,1), labels = c("min", "max")) + 
  theme_bw() + labs(x="", y="Density") +
  scale_fill_manual(labels=biomes_abbrev, values=clist) 

p1 <- ggplot(m_s, aes(x=area_km2, y=len_km,color=biome3)) + geom_point() +
  geom_smooth(method="lm",se=F) + scale_x_log10() + scale_y_log10() + 
  scale_color_manual(labels=biomes_abbrev, values=clist) + 
  labs(x = expression(paste("Area burned (",km^2,")")), 
       y = expression(paste(SL[BA])), color="Ecoregion") + theme_bw() +
  theme(legend.key=element_blank(), legend.background = element_blank()) +
  theme(legend.position=c(0.1,0.7))

p2 <- ggplot(m_s, aes(x=len_km)) + 
  geom_density(aes(fill=biome3),alpha=0.2,show.legend=F) +
  scale_x_log10() + scale_y_continuous(breaks = c(0,1), labels = c("min", "max")) + 
  theme_bw() + labs(x="", y="Density") + 
  scale_fill_manual(labels=biomes_abbrev, values=clist) + 
  theme(axis.text.x = element_text(angle = -90)) + coord_flip() 

plot_grid(p0,NULL,p1,p2,
          rel_widths = c(1,0.3),
          rel_heights = c(0.2,1),align="hv")
ggsave("./figures/201109_TableS3_figure.pdf",width=7,height=7)
### END ---------------------------------------------------


### Figure S1 #############################################
t1$x <- t1$t_len / t1$t_area
t1$y <- t1$b_len / t1$b_area
t1$density <- t1$t_len / t1$t_area

#create dataset without MWCF and calculate statistics
t2 <- t1[-2,]
m_s3 <- round(summary(lm(t2$y~t2$x))[[4]][2,1],2) #slope
p_s3 <- round(summary(lm(t2$y~t2$x))[[4]][2,4],4) #slope p-value
r_s3 <- round(summary(lm(t2$y~t2$x))[[9]],2)

fig_s1 <- ggplot() + geom_point(data=t1[-2,],aes(x=x,y=y),color="black",size=2.5) + 
  geom_point(data=t1[-2,],aes(x=x,y=y),color="white",size=1) + 
  geom_point(data=t1[2,],aes(x=x,y=y),color="red",size=3,shape=4) + 
  geom_smooth(data=t1[-2,],aes(x=x,y=y),method="lm",se=F,color="blue",lwd=0.8) +
  theme_bw() + labs(x=expression(paste("total stream length (km) : total area (",km^2,")")),
                    y=expression(paste(SL[BA], " (km)"," : burned area (",km^2,")"))) + 
  annotate("text",x=t1[2,11]-0.25,y=t1[2,12],label="MWCF") +
  annotate("text",x=1.3,y=1.55,label=paste0("slope: ",m_s3)) +
  annotate("text",x=1.3,y=1.48,label=expression(paste(R["adj"]^2," = 0.66"))) +
  annotate("text",x=1.3,y=1.41,label=paste0("p = ",p_s3)) + 
  annotate("text",x=1.3,y=1.34,label=paste0("n = 8"))

fig_s1_2 <- ggplot() + 
  geom_smooth(data=t1[-2,],aes(x=x,y=y),method="lm",se=F,color="skyblue",lwd=0.8) +
  geom_point(data=t1,aes(x=x,y=y),color="black",size=4) + 
  geom_point(data=t1,aes(x=x,y=y,color=rain),size=3) + 
  geom_text_repel(data=t1,aes(x=x,y=y,label=abbrev),hjust=0.5,nudge_y=0.04) + 
  theme_bw() + labs(x=expression(paste("total stream length (km) : total area (",km^2,")")),
                    y=expression(paste(SL[BA], " (km)"," : burned area (",km^2,")"))) + 
  annotate("text",x=1.3,y=1.55,label=paste0("slope: ",m_s3)) +
  annotate("text",x=1.3,y=1.48,label=expression(paste(R["adj"]^2," = 0.66"))) +
  annotate("text",x=1.3,y=1.41,label=paste0("p = ",p_s3)) + 
  annotate("text",x=1.3,y=1.34,label=paste0("n = 8")) +
  scale_color_viridis_c(name="Precip. \n (mm/yr)", 
                          direction=-1, option="inferno")

ggsave("./figures/Figure_S1_2.pdf",height=4,width=5)
ggsave("./figures/Figure_S1.pdf",height=4,width=5)
### END ---------------------------------------------------


### Figure S2 #############################################
sags <- read.csv("./data/200910_DO_sags.csv")

# first estimates for parameters derived from "fit curve" in JMP
a1 = 13.941987
b1 = -0.009684

e_95 <- confint(nls(Delta_DO~a*exp(b*km),start=list(a=a1,b=b1),data=sags),level=0.95)
l_95 <- confint(lm(sags$Delta_DO ~ sags$km),level=0.95)

do_point5_fit <- log(0.5 / a1) / b1
do_point5_lower <- log(0.5 / e_90[1,1]) / e_90[2,1]
do_point5_upper <- log(0.5 / e_90[1,2]) / e_90[2,2]

fig_s2 <- ggplot(data=sags,aes(x=km,y=Delta_DO)) + geom_point() +
  geom_hline(yintercept=0.5,linetype="dashed") + 
  stat_function(fun=function(x) a1*exp(b1*x),color="blue",lwd=0.8) +
  xlim(60,370) + ylim(0,8) + theme_bw() + 
  labs(x=expression(paste(SL[LE]," (km)")),y="Decrease in DO (mg/L)")

fig_s2_95 <- ggplot(data=sags,aes(x=km,y=Delta_DO)) + geom_point() +
  geom_hline(yintercept=0.5,linetype="dashed") + 
  stat_function(fun=function(x) a1*exp(b1*x),color="blue",lwd=0.8) +
  stat_function(fun=function(x) e_95[1,1]*exp(e_95[2,1]*x),color="blue",lwd=0.4) + 
  stat_function(fun=function(x) e_95[1,2]*exp(e_95[2,2]*x),color="blue",lwd=0.4) + 
  xlim(60,370) + ylim(0,8) + theme_bw() + 
  labs(x=expression(paste(SL[LE]," (km)")),y="Decrease in DO (mg/L)") +
  ggtitle("95% CI")

ggplot(data=sags,aes(x=km,y=Delta_DO)) + geom_point() +
  geom_hline(yintercept=0.5,linetype="dashed") + 
  geom_smooth(method="nls", formula=y~a*exp(b*x)) + 
  xlim(60,370) + ylim(0,8) + theme_bw()

plot_grid(fig_s2_95,fig_s2_90,nrow=2)

ggsave("./figures/Figure_S2.pdf",height=3,width=5)
### END ---------------------------------------------------
