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

