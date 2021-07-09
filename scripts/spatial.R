#map trees

#including newly validated trees
load('./data/formatted/data_all2.Rdata')

library(maps)
m = map_data('state', region = 'Utah')

spatial_df <- data_all %>%
  ungroup() %>%
  select(TRE_CN,LAT,LON,SPCD) %>%
  distinct() %>%
  filter(SPCD %in% c(93,122,202)) %>%
  mutate(Species = ifelse(SPCD==93,"Engelmann spruce",
                           ifelse(SPCD==122,"Ponderosa pine",
                                  "Douglas fir")))
length(unique(spatial_df$TRE_CN)) #318

ggplot() + 
  geom_polygon( data=m, aes(x=long, y=lat,group=group),colour="black", fill="white" )+
  geom_point(data=spatial_df,aes(x=LON,y=LAT,
                                 color=factor(Species),shape=factor(Species),alpha = .9))+
  ggtitle("Distribution of Trees in Utah")+
  xlab('Longitude')+
  ylab('Latitude')+
  coord_fixed() +
  theme(legend.position = "bottom")

val_spatial_df <- val_dset %>%
  ungroup() %>%
  select(TRE_CN,LAT,LON,SPCD) %>%
  distinct() %>%
  filter(SPCD %in% c(93,122,202)) %>%
  mutate(Species = ifelse(SPCD==93,"Engelmann spruce",
                          ifelse(SPCD==122,"Ponderosa pine",
                                 "Douglas fir")))

ggplot() + 
  geom_polygon(data=m, aes(x=long, y=lat,group=group),colour="black", fill="white" )+
  geom_point(data=val_spatial_df,aes(x=LON,y=LAT,
                                 color=factor(Species),shape=factor(Species),alpha = .9))+
  ggtitle("Validation Trees in Utah")+
  xlab('Longitude')+
  ylab('Latitude')+
  coord_fixed() +
  theme(legend.position = "bottom")


#https://eriqande.github.io/rep-res-web/lectures/making-maps-with-R.html
library(ggplot2)
library(ggmap)
library(maps)
library(mapdata)
library(viridis)

ut_df <- subset(states, region == "utah")

ditch_the_axes <- theme(
  axis.text = element_blank(),
  axis.line = element_blank(),
  axis.ticks = element_blank(),
  panel.border = element_blank(),
  panel.grid = element_blank(),
  axis.title = element_blank()
)

ggplot()+
  geom_polygon(data = m, aes(x = long, y = lat, group = group),
               color = "black", fill = "white") + 
  coord_fixed(1.3) + 
  geom_point(data=spatial_df,aes(x=LON,y=LAT,
                                     color=factor(Species),shape=factor(Species),alpha = .9))+
  scale_colour_manual(values = c("Douglas fir" = "purple4", "Ponderosa pine" = "turquoise4",
                                 "Engelmann spruce" = "gold1")) +
  theme_bw() +
  ditch_the_axes

#elev & climate with distribution

library(maps)
m = map_data('state', region = 'Utah')

map_df <- data_all %>%
  ungroup() %>%
  select(TRE_CN,LAT,LON,SPCD) %>%
  distinct() %>%
  filter(TRE_CN %in% tre_cal,
         !is.na(SPCD)) %>%
  mutate(Species = ifelse(SPCD==93,"Engelmann spruce",
                          ifelse(SPCD==122,"Ponderosa pine",
                                 "Douglas-fir")))
length(unique(map_df$TRE_CN)) #265

map_ut <- ggplot() + 
  geom_polygon( data=m, aes(x=long, y=lat,group=group),colour="black", fill="white" )+
  geom_point(data=map_df,aes(x=LON,y=LAT,color=Species,alpha = .5), show.legend = F)+
  scale_colour_manual(values = c("Douglas-fir" = "purple4", "Ponderosa pine" = "turquoise4",
                                 "Engelmann spruce" = "gold1")) +
  ggtitle("a)")+
  xlab('Longitude')+
  ylab('Latitude')+
  coord_fixed(1.3) +
  theme_bw() +
  ditch_the_axes + theme(legend.position = "none")


load("./data/formatted/data_all.Rdata")
length(unique(data_all$TRE_CN)) #531
tre_cal <- unique(cal_dia$TRE_CN)
spatial_df <- data_all %>%
  ungroup() %>%
  filter(TRE_CN %in% tre_cal,
         Year > 1960) %>%
  #filter(SPCD %in% c(93,122,202)) %>%
  mutate(ppt_an = ppt_Jan + ppt_Feb + ppt_Mar + ppt_Apr + ppt_May + ppt_Jun + ppt_Jul +
           ppt_Aug + ppt_Sep + ppt_Oct + ppt_Nov + ppt_Dec,
         tmax_an = (tmax_Jan + tmax_Feb + tmax_Mar + tmax_Apr + tmax_May + tmax_Jun + tmax_Jul +
           tmax_Aug + tmax_Sep + tmax_Oct + tmax_Nov + tmax_Dec)/12) %>%
  dplyr::select(TRE_CN,SPCD,ELEV,ppt_an,tmax_an,RW) %>%
  group_by(TRE_CN,SPCD,ELEV) %>%
  summarise(mn_ppt = mean(ppt_an, na.rm = TRUE),
            mn_tmax = mean(tmax_an, na.rm = TRUE),
            mn_growth = mean(RW, na.rm = TRUE)) %>%
  mutate(Species = factor(ifelse(SPCD==93,"Engelmann spruce",
                          ifelse(SPCD==122,"Ponderosa pine",
                                 "Douglas fir"))))

length(unique(spatial_df$TRE_CN)) #265

#precip
dis_prec <- ggplot()+
  geom_point(data=spatial_df,aes(x=mn_ppt,y=ELEV, color=Species, alpha = 0.5), 
             show.legend = F)+ #size = mn_growth, alpha = 0.5
  scale_colour_manual(values = c("Douglas fir" = "purple4", "Ponderosa pine" = "turquoise4",
                                 "Engelmann spruce" = "gold1")) +
  theme_bw() +   ggtitle("b)")+
  xlab("Average Annual Precipitation (mm)") + ylab("Elevation (ft)")
ggsave(filename = "/home/courtney/Documents/Masters/Research/Utah/UT_FVS/images/dis_prec.png",
       width = 3, height = 2, units = "in")

#temp
dis_temp <- ggplot()+
  geom_point(data=spatial_df,aes(x=mn_tmax,y=ELEV,color=Species, alpha = 0.5), 
             show.legend = F)+ #size = mn_growth, alpha = 0.5
  scale_colour_manual(values = c("Douglas fir" = "purple4", "Ponderosa pine" = "turquoise4",
                                 "Engelmann spruce" = "gold1")) +
  theme_bw() + ggtitle("c)") +
  xlab("Average Monthly Max Temperature (C)") + ylab("Elevation (ft)")
ggsave(filename = "/home/courtney/Documents/Masters/Research/Utah/UT_FVS/images/dis_temp.png",
       width = 3, height = 2, units = "in")

spat_plt <- list(map_ut,dis_prec,dis_temp)
lay = rbind(c(1,1,1,2,2,2),
            c(1,1,1,2,2,2),
            c(1,1,1,3,3,3),
            c(1,1,1,3,3,3))
spat_fig <- grid.arrange(grobs = spat_plt, layout_matrix = lay)
ggsave(filename = "/home/courtney/Documents/Masters/Research/Utah/UT_FVS/images/spat_fig.png", spat_fig,
       height = 8, width = 10, units = "in")

sp_leg <- ggplot()+
  geom_point(data=spatial_df,aes(x=mn_tmax,y=ELEV,color=Species, alpha = 0.5))+ #size = mn_growth, alpha = 0.5
  scale_colour_manual(values = c("Douglas fir" = "purple4", "Ponderosa pine" = "turquoise4",
                                 "Engelmann spruce" = "gold1")) +
  theme_bw() + 
  xlab("Average Monthly Max Temperature (C)") + ylab("Elevation (ft)") +
  theme(legend.position="left")
ggsave(filename = "/home/courtney/Documents/Masters/Research/Utah/UT_FVS/images/sp_leg.png",
       height = 8, width = 10, units = "in")

#arrange into one plot

