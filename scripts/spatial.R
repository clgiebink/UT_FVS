#map trees

library(maps)
m = map_data('state', region = 'Utah')

spatial_df <- per_cov %>%
  filter(TRE_CN %in% data_all$TRE_CN) %>%
  filter(SPCD %in% c(93,122,202)) %>%
  select(LON,LAT,SPCD) %>%
  mutate(Species = ifelse(SPCD==93,"Engelmann spruce",
                           ifelse(SPCD==122,"Ponderosa pine",
                                  "Douglas fir")))

ggplot() + 
  geom_polygon( data=m, aes(x=long, y=lat,group=group),colour="black", fill="white" )+
  geom_point(data=spatial_df,aes(x=LON,y=LAT,
                                 color=factor(Species),shape=factor(Species),alpha = .9))+
  ggtitle("Distribution of Trees in Utah")+
  xlab('Longitude')+
  ylab('Latitude')+
  coord_fixed() +
  theme(legend.position = "bottom")
