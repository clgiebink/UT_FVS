#Compare model coefficients by species
#Courtney Giebink
#clgiebink@gmail.com


#visualize model coefficients for all models together
library(dotwhisker)
library(broom.mixed)


#all
#full
full_clim_df <-  tidy(clim_16) %>% 
  filter(effect == "fixed") %>%
  select(term,estimate,std.error) %>%
  mutate(model = "Douglas fir")%>%
  relabel_predictors(c(z.DIA_C = "DBH",                       # relabel predictors
                       "I(z.DIA_C^2)" = "DBH^2",
                       z.CR_fvs = "Crown Ratio",
                       "I(z.CR_fvs^2)" = "CR^2",
                       z.ppt_pJunSep = "Precipitation",
                       z.tmax_FebJul = "Temperature",
                       "z.ppt_pJunSep:z.tmax_FebJul" = "Climate Interaction",
                       z.BAL = "BAL",
                       z.CCF = "CCF", 
                       z.SDI = "SDI",
                       z.SICOND = "Site Index",
                       z.SLOPE = "Slope",
                       "I(z.SLOPE^2)" = "Slope^2",
                       z.sin = "sin(Aspect-0.7854)*Slope",
                       z.cos = "cos(Aspect-0.7854)*Slope",
                       z.solrad_MayAug = "solrad"))
full_clim_pp <-  tidy(clim_16_pp) %>% 
  filter(effect == "fixed") %>%
  select(term,estimate,std.error) %>%
  mutate(model = "Ponderosa pine")%>%
  relabel_predictors(c(z.DIA_C = "DBH",                       # relabel predictors
                       "I(z.DIA_C^2)" = "DBH^2",
                       z.CR_fvs = "Crown Ratio",
                       "I(z.CR_fvs^2)" = "CR^2",
                       z.ppt_pJunSep = "Precipitation",
                       z.tmax_JunAug = "Temperature",
                       "z.ppt_pJunSep:z.tmax_JunAug" = "Climate Interaction",
                       z.BAL = "BAL",
                       z.CCF = "CCF", 
                       z.SDI = "SDI",
                       z.SICOND = "Site Index",
                       z.SLOPE = "Slope",
                       "I(z.SLOPE^2)" = "Slope^2",
                       z.sin = "sin(Aspect-0.7854)*Slope",
                       z.cos = "cos(Aspect-0.7854)*Slope",
                       z.solrad_MayAug = "solrad"))
full_clim_es <-  tidy(clim_16_es) %>% 
  filter(effect == "fixed") %>%
  select(term,estimate,std.error) %>%
  mutate(model = "Engelmann spruce")%>%
  relabel_predictors(c(z.DIA_C = "DBH",                       # relabel predictors
                       "I(z.DIA_C^2)" = "DBH^2",
                       z.CR_fvs = "Crown Ratio",
                       "I(z.CR_fvs^2)" = "CR^2",
                       z.ppt_pJunSep = "Precipitation",
                       z.tmax_pAug = "Temperature",
                       "z.ppt_pJunSep:z.tmax_pAug" = "Climate Interaction",
                       z.BAL = "BAL",
                       z.CCF = "CCF", 
                       z.SICOND = "Site Index",
                       z.SLOPE = "Slope",
                       "I(z.SLOPE^2)" = "Slope^2",
                       z.sin = "sin(Aspect-0.7854)*Slope",
                       z.cos = "cos(Aspect-0.7854)*Slope"))
full_clim_mod <- full_join(full_clim_df,full_clim_pp) %>%
  full_join(.,full_clim_es)
dwplot(full_clim_mod, 
       vline = geom_vline(xintercept = 0, colour = "grey60", linetype = 2), 
       # plot line at zero _behind_ coefs
       dot_args = list(aes(shape = model)),
       whisker_args = list(aes(linetype = model))) +
  theme_bw() + xlab("Coefficient Estimate") + ylab("") +
  ggtitle("Full Climate") +
  theme(plot.title = element_text(face="bold"),
        legend.position = "right",
        legend.background = element_rect(colour="grey80"),
        legend.title.align = 0.5)

#reduced
red_clim_df <-  tidy(clim_red_df) %>% 
  filter(effect == "fixed") %>%
  dplyr::select(term,estimate,std.error) %>%
  mutate(model = "Douglas fir")%>%
  relabel_predictors(c(z.DIA_C = "DBH",                       # relabel predictors
                       "I(z.DIA_C^2)" = "DBH^2",
                       z.CR_fvs = "Crown Ratio",
                       "I(z.CR_fvs^2)" = "CR^2",
                       z.ppt_pJunSep = "Precipitation",
                       z.tmax_FebJul = "Temperature",
                       "z.ppt_pJunSep:z.tmax_FebJul" = "Precip:Temp",
                       z.BAL = "Competition",
                       z.CCF = "Competition", 
                       z.SDI = "Competition",
                       z.SICOND = "Site Index",
                       z.SLOPE = "Slope",
                       "I(z.SLOPE^2)" = "Slope^2",
                       z.sin = "sin(Aspect-0.7854)*Slope",
                       z.cos = "cos(Aspect-0.7854)*Slope",
                       z.solrad_MayAug = "Solar Radiation"))
red_clim_pp <-  tidy(clim_red_pp) %>% 
  filter(effect == "fixed") %>%
  dplyr::select(term,estimate,std.error) %>%
  mutate(model = "Ponderosa pine")%>%
  relabel_predictors(c(z.DIA_C = "DBH",                       # relabel predictors
                       "I(z.DIA_C^2)" = "DBH^2",
                       z.CR_fvs = "Crown Ratio",
                       "I(z.CR_fvs^2)" = "CR^2",
                       z.ppt_pJunSep = "Precipitation",
                       z.tmax_JunAug = "Temperature",
                       "z.ppt_pJunSep:z.tmax_JunAug" = "Precip:Temp",
                       z.BAL = "Competition",
                       z.CCF = "Competition", 
                       z.SDI = "Competition",
                       z.SICOND = "Site Index",
                       z.SLOPE = "Slope",
                       "I(z.SLOPE^2)" = "Slope^2",
                       z.sin = "sin(Aspect-0.7854)*Slope",
                       z.cos = "cos(Aspect-0.7854)*Slope",
                       z.solrad_MayAug = "Solar Radiation"))
red_clim_es <-  tidy(clim_red_es) %>% 
  filter(effect == "fixed") %>%
  dplyr::select(term,estimate,std.error) %>%
  mutate(model = "Engelmann spruce")%>%
  relabel_predictors(c(z.DIA_C = "DBH",                       # relabel predictors
                       "I(z.DIA_C^2)" = "DBH^2",
                       z.CR_fvs = "Crown Ratio",
                       z.CR = "Crown Ratio",
                       "I(z.CR_fvs^2)" = "CR^2",
                       z.ppt_pJunSep = "Precipitation",
                       z.tmax_pAug = "Temperature",
                       "z.ppt_pJunSep:z.tmax_pAug" = "Precip:Temp",
                       z.BAL = "Competition",
                       z.CCF = "Competition", 
                       z.SICOND = "Site Index",
                       z.SLOPE = "Slope",
                       "I(z.SLOPE^2)" = "Slope^2",
                       z.sin = "sin(Aspect-0.7854)*Slope",
                       z.cos = "cos(Aspect-0.7854)*Slope"))
red_clim_mod <- full_join(red_clim_df,red_clim_pp) %>%
  full_join(.,red_clim_es)
red_clim <- dwplot(red_clim_mod, 
       vline = geom_vline(xintercept = 0, colour = "grey60", linetype = 2), # plot line at zero _behind_ coefs
       dot_args = list(aes(shape = model)),
       whisker_args = list(aes(linetype = model))) +
  theme_bw() + xlab("Standardized Coefficient Estimate") + ylab("") +
  #ggtitle("Reduced: Tree-Rings + Climate") +
  theme(plot.title = element_text(face="bold"),
        legend.position = c(0.75,0.45),
        legend.background = element_rect(colour="grey80"),
        legend.title.align = 0.5) +
  scale_colour_manual(values = c("Douglas fir" = "purple4", "Ponderosa pine" = "turquoise4",
                                 "Engelmann spruce" = "gold1"))
ggsave(filename = "/home/courtney/Documents/Masters/Research/Utah/UT_FVS/images/red_clim_mods.png",red_clim,
       height = 4, width = 5, units = "in")

#changing color
#colorblind friendly
# The palette with grey:
cbp1 <- c("#999999", "#E69F00", "#56B4E9", "#009E73",
          "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

# To use for line and point colors, add
sp + scale_colour_manual(values=cbp1)

library(viridis)

# Discrete color. use the argument discrete = TRUE
ggplot(iris, aes(Sepal.Length, Sepal.Width))+
  geom_point(aes(color = Species)) +
  geom_smooth(aes(color = Species, fill = Species), method = "lm") + 
  scale_color_viridis(discrete = TRUE, option = "D")+
  scale_fill_viridis(discrete = TRUE) 

mod_pres <- dwplot(red_clim_mod, 
                   vline = geom_vline(xintercept = 0, colour = "grey60", linetype = 2), 
                   # plot line at zero _behind_ coefs
                   dot_args = list(aes(shape = model)),
                   whisker_args = list(aes(linetype = model))) +
  theme_bw() + xlab("Coefficient Estimate") + ylab("") +
  #ggtitle("Reduced Climate") +
  theme(plot.title = element_text(face="bold"),
        legend.position = c(.775,.560),
        legend.background = element_rect(colour="grey80"),
        legend.title.align = 0.5) +
  scale_color_viridis(discrete = TRUE, option = "D")+
  scale_fill_viridis(discrete = TRUE) 
mod_pres
ggsave(filename = "trday20.jpeg", path = "./images/",width = 6, height = 5, units = "in")

#constant cr
red_clim_df <-  tidy(clim_mod_df) %>% 
  filter(effect == "fixed") %>%
  dplyr::select(term,estimate,std.error) %>%
  mutate(model = "Douglas fir")%>%
  relabel_predictors(c(z.DIA_C = "DBH",                       # relabel predictors
                       "I(z.DIA_C^2)" = "DBH^2",
                       z.CR = "Crown Ratio",
                       z.ppt_pJunSep = "Precipitation",
                       z.tmax_FebJul = "Temperature",
                       "z.ppt_pJunSep:z.tmax_FebJul" = "Climate Interaction",
                       z.BAL = "Competition",
                       z.CCF = "Competition", 
                       z.SDI = "Competition",
                       z.SICOND = "Site Index",
                       z.SLOPE = "Slope",
                       "I(z.SLOPE^2)" = "Slope^2",
                       z.sin = "sin(Aspect-0.7854)*Slope",
                       z.cos = "cos(Aspect-0.7854)*Slope",
                       z.solrad_MayAug = "Solar Radiation"))
red_clim_pp <-  tidy(clim_mod_pp) %>% 
  filter(effect == "fixed") %>%
  dplyr::select(term,estimate,std.error) %>%
  mutate(model = "Ponderosa pine")%>%
  relabel_predictors(c(z.DIA_C = "DBH",                       # relabel predictors
                       "I(z.DIA_C^2)" = "DBH^2",
                       z.CR = "Crown Ratio",
                       z.ppt_pJunSep = "Precipitation",
                       z.tmax_JunAug = "Temperature",
                       "z.ppt_pJunSep:z.tmax_JunAug" = "Climate Interaction",
                       z.BAL = "Competition",
                       z.CCF = "Competition", 
                       z.SDI = "Competition",
                       z.SICOND = "Site Index",
                       z.SLOPE = "Slope",
                       "I(z.SLOPE^2)" = "Slope^2",
                       z.sin = "sin(Aspect-0.7854)*Slope",
                       z.cos = "cos(Aspect-0.7854)*Slope",
                       z.solrad_MayAug = "Solar Radiation"))
red_clim_es <-  tidy(clim_mod2_es) %>% 
  filter(effect == "fixed") %>%
  dplyr::select(term,estimate,std.error) %>%
  mutate(model = "Engelmann spruce")%>%
  relabel_predictors(c(z.DIA_C = "DBH",                       # relabel predictors
                       "I(z.DIA_C^2)" = "DBH^2",
                       z.CR = "Crown Ratio",
                       "I(z.CR^2)" = "CR^2",
                       z.ppt_pJunSep = "Precipitation",
                       z.tmax_pAug = "Temperature",
                       "z.ppt_pJunSep:z.tmax_pAug" = "Climate Interaction",
                       z.BAL = "Competition",
                       z.CCF = "Competition", 
                       z.SICOND = "Site Index",
                       z.SLOPE = "Slope",
                       "I(z.SLOPE^2)" = "Slope^2",
                       z.sin = "sin(Aspect-0.7854)*Slope",
                       z.cos = "cos(Aspect-0.7854)*Slope"))
red_clim_mod <- full_join(red_clim_df,red_clim_pp) %>%
  full_join(.,red_clim_es)
dwplot(red_clim_mod, 
       vline = geom_vline(xintercept = 0, colour = "grey60", linetype = 2), # plot line at zero _behind_ coefs
       dot_args = list(aes(shape = model)),
       whisker_args = list(aes(linetype = model))) +
  theme_bw() + xlab("Coefficient Estimate") + ylab("") +
  ggtitle("Reduced: Tree-Rings + Climate") +
  theme(plot.title = element_text(face="bold"),
        legend.position = "right",
        legend.background = element_rect(colour="grey80"),
        legend.title.align = 0.5) +
  scale_colour_manual(values = c("Douglas fir" = "purple4", "Ponderosa pine" = "turquoise4",
                                 "Engelmann spruce" = "gold1"))

#vis comp
df_stats <- df_stats %>%
  mutate(Species = "Douglas fir") %>%
  dplyr:: select(Species, CR, CCF, BAL, SDI)
pp_stats <- pp_stats %>%
  mutate(Species = "Ponderosa pine") %>%
  dplyr:: select(Species, CR, CCF, BAL, SDI)
es_stats <- es_stats %>%
  mutate(Species = "Englemann spruce") %>%
  dplyr:: select(Species, CR, CCF, BAL, SDI)

sp_stats <- bind_rows(df_stats,)