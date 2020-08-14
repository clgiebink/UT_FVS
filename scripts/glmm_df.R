#Linear mixed model for Douglas fir
#Courtney Giebink
#clgiebink@gmail.com
#29 July 2019

#dataframe
#all data is provided in
load(file = '~/data/formatted/data_all_df.Rdata')

#create new dataframe with only variables needed for linear mixed model
#response: RW, dds
#covariates: SI, ASP, SL, DBH/DIA_C, BAL, CR, CCF, PCCF, climate
#random: TRE_CN, Year
#filter for last 30 years of growth
min(data_all_df$MEASYEAR) #1988 -> 1958

glmm_data_df <- data_all_df %>%
  ungroup() %>%
  dplyr::select(PLT_CN, FVS_LOC_CD, TRE_CN, RW, dds, Year, DIA_C, LAT,
         SICOND, tASPECT, SLOPE, BAL, SDI, CR, CR_fvs, PCCF, CCF, 
         cos, sin, solrad_an, solrad_JanApr, solrad_MayAug, solrad_SepDec,
         ppt_pOct, ppt_pDec, ppt_Jun, ppt_Jul,
         ppt_pJunAug, ppt_pAugOct, ppt_MayJul,
         ppt_pJunNov, ppt_FebJul, wateryr, ppt_pJunSep,
         tmin_Feb, tmax_Jul,
         tmax_JunAug, tmax_pJulSep, tmin_JanMar,
         tmax_JanJun,tmin_JanJun,tmax_FebJul) %>%
  filter(Year >= 1958)

#climate
#total ppt
#1 month: pOct,pDec, Jun, Jul
#3 month: pJun-pAug, pAug-pOct, May-Jul
#6 month + : pNov, Jul, wateryr

#average temp
#1 month: Feb tmin, Jul tmax
#3 month: tmax_Jun-Aug, tmax_pJul-pSep, tmin_Jan-Mar
#6 month + : Jun, Jul

save(glmm_data_df, file = "./data/formatted/glmm_data_df.Rdata")

#Exploration

#Missing data
sum(is.na(glmm_data_df)) #579
summary(glmm_data_df)
#tASPECT - 62
#SICOND - 70 
## two trees
#CR - 35
#ppt_pOct - 5
#ppt_pDec - 5
#ppt_pJunAug - 5
#ppt_pAugOct - 5
#ppt_pJunNov - 5
#wateryr - 5
#tmax_pJulSep - 5
miss_asp_df <- unique(glmm_data_df$TRE_CN[is.na(glmm_data_df$tASPECT)]) #2
# 2.881010e+12 2.881027e+12
asp_check_df <- glmm_data_df %>%
  filter(TRE_CN %in% miss_asp_df)

miss_clim_df <- unique(glmm_data_df$TRE_CN[is.na(glmm_data_df$ppt_pOct)])
#2.854250e+12 2.872497e+12 2.907837e+12 2.911563e+12 2.928494e+12
#climate NAs all same trees
#and years? maybe just the last year
unique(glmm_data_df$Year[is.na(glmm_data_df$ppt_pOct)])
#1958 1961 1965 1966
clim_check_df <- glmm_data_df %>%
  dplyr::select(TRE_CN,Year,ASPECT,ppt_pDec,ppt_pOct,
                ppt_pJunAug,ppt_pAugOct,ppt_pJunNov,wateryr,tmax_pJulSep) %>%
  filter(TRE_CN %in% miss_clim_df)
#missing climate data is earliest year of growth
#has to do with the way climate and increment data were joined
#to fix must go add another year to increment data and join?
#probably will just delete missing year.

#almost one tree per plot so don't need to have plot as random effect
length(unique(data_all_df$TRE_CN))
#136
length(unique(data_all_df$PLT_CN))
#118
tre_plt_df <- data_all_df %>% 
  select(PLT_CN,TRE_CN) %>%
  distinct() %>%
  group_by(PLT_CN) %>%
  summarise(n=n())
length(unique(tre_plt_df$PLT_CN))
#118
max(tre_plt_df$n) #2

#distribution of the response variable
hist(glmm_data_df$RW,breaks = 50, main = "Histogram of RW (mm)", xlab = "Increment")
hist(glmm_data_df$dds,breaks = 100, main = "Histogram of DDS (in)", xlab = "Increment") 

which(glmm_data_df$RW == 0) #262 (row number)
#1 missing ring
#replace with smallest rw of on same core
glmm_data_df <- glmm_data_df %>%
  group_by(TRE_CN) %>%
  mutate(dds = ifelse(RW != 0, dds, 
                      min(dds[dds > 0])))
which(is.na(glmm_data_df$RW)) #0
which(glmm_data_df$dds == 0) #0

#growth trends across time
library(ggplot2)

#understanding random effects: TRE_CN
growth_yr_plots_df <- glmm_data_df %>% 
  group_by(TRE_CN) %>%
  do(plots=ggplot(data=.) +
       aes(x=Year, y=RW) + geom_point() + ggtitle(unique(.$TRE_CN)))

for(i in nrow(growth_yr_plots_df)) {
  growth_yr_plots_df$plots[[i]]
}

#par(mfrow = c(2, 2)) doesn't work; why?
growth_yr_plots_df$plots[[1]]
growth_yr_plots_df$plots[[2]]
growth_yr_plots_df$plots[[3]]
growth_yr_plots_df$plots[[4]]
growth_yr_plots_df$plots[[5]]
growth_yr_plots_df$plots[[6]]
growth_yr_plots_df$plots[[7]]
growth_yr_plots_df$plots[[8]]
growth_yr_plots_df$plots[[9]]
growth_yr_plots_df$plots[[10]]
growth_yr_plots_df$plots[[11]]
growth_yr_plots_df$plots[[12]]
growth_yr_plots_df$plots[[13]]

#understanding random effects: Year
growth_wateryr_plots_df <- glmm_data_df %>% 
  group_by(Year) %>%
  do(plots=ggplot(data=.) +
       aes(x=wateryr, y=RW) + geom_point() +
       geom_smooth(method = "lm") + ggtitle(unique(.$Year)))

#par(mfrow = c(2, 2)) doesn't work
growth_wateryr_plots_df$plots[[1]]
growth_wateryr_plots_df$plots[[2]]
growth_wateryr_plots_df$plots[[3]]
growth_wateryr_plots_df$plots[[4]]
growth_wateryr_plots_df$plots[[5]]
growth_wateryr_plots_df$plots[[6]]
growth_wateryr_plots_df$plots[[7]]
growth_wateryr_plots_df$plots[[8]]
growth_wateryr_plots_df$plots[[9]]
growth_wateryr_plots_df$plots[[10]]
growth_wateryr_plots_df$plots[[11]]
growth_wateryr_plots_df$plots[[12]]
growth_wateryr_plots_df$plots[[13]]

growth_wateryr_plots_df$plots[[23]]
growth_wateryr_plots_df$plots[[24]]
growth_wateryr_plots_df$plots[[25]]
growth_wateryr_plots_df$plots[[26]]
growth_wateryr_plots_df$plots[[27]]
growth_wateryr_plots_df$plots[[28]]


#dbh as a proxy for age
#detrend
ggplot(data = glmm_data_df, aes(x = DIA_C, y = RW)) +
  geom_point(alpha=0.1) + geom_smooth(method = "lm")
#small skew on top of line

ggplot(data = glmm_data_df, aes(x = DIA_C, y = dds)) +
  geom_point(alpha=0.1) + geom_smooth(method = "lm")
#large skew on top of line

#log to reduce heteroscedasticity
ggplot(data = glmm_data_df, aes(x = log(DIA_C), y = log(dds))) +
  geom_point(alpha=0.1) + geom_smooth(method = "lm")
#reduces a little
#negative relationship

#relationship of response with site index (SI)
ggplot(data = glmm_data_df, aes(x = SICOND, y = log(dds))) +
  geom_point(alpha=0.1) + geom_smooth(method = "lm")
#constant variance; positive relationship

#relationship of response with slope (SLOPE)
ggplot(data = glmm_data_df, aes(x = SLOPE, y = log(dds))) +
  geom_point(alpha=0.1) + geom_smooth(method = "lm")
#constant variance; negative relationship

#relationship of response with BAL
ggplot(data = glmm_data_df, aes(x = BAL, y = log(dds))) +
  geom_point(alpha=0.1) + geom_smooth(method = "lm")
#non constant variance (decreases); negative relationship

#relationship of response with PCCF
ggplot(data = glmm_data_df, aes(x = PCCF, y = log(dds))) +
  geom_point(alpha=0.1) + geom_smooth(method = "lm")
#constant variance; negative relationship

#relationship of response with CCF
ggplot(data = glmm_data_df, aes(x = CCF, y = log(dds))) +
  geom_point(alpha=0.1) + geom_smooth(method = "lm")
#non constant variance (decreases); negative relationship

#relationship of response with crown ratio (CR,CR_fvs)
ggplot(data = glmm_data_df, aes(x = CR, y = log(dds))) +
  geom_point(alpha=0.1) + geom_smooth(method = "lm")
#variance increases (not bad); postive relationship

ggplot(data = glmm_data_df, aes(x = CR_fvs, y = log(dds))) +
  geom_point(alpha=0.1) + geom_smooth(method = "lm")
#constant variance; slight negative relationship


##Model Building
library(lme4)
library(lmerTest)
#library(nlme)
#library(glmmTMB)

#reminder- can't compare AIC values between a transformed and nontransformed model

#standardize to encourage convergence
library(MuMIn)
glmm_df_z <- stdize(as.data.frame(glmm_data_df),append=TRUE)
#remove missing data
glmm_df_z <- glmm_df_z %>%
  filter(!is.na(z.wateryr)) %>%
  filter(!is.na(z.SICOND))

#site index incorrect
load("./data/formatted/cal_si.Rdata")
#either reduce or calculate with height and age
glmm_df_z <- glmm_df_z %>%
  filter(!(TRE_CN %in% cal_si$TRE_CN))
length(unique(glmm_df_z$TRE_CN))
#136 -> 113
save(glmm_df_z,file = "./data/formatted/glmm_df_z.Rdata")

#collinearity
library(car)
#vif()

#AIC to compare climate variables
#total ppt
#1 month: pOct,pDec, Jun, Jul
#3 month: pJun-pAug, pAug-pOct, May-Jul
#6 month + : pNov, Jul, wateryr, pJunSep

fvs_clim_1 <- lmer(log(tdds)~z.SICOND+z.sin+
                     z.cos+z.SLOPE+I(z.SLOPE^2)+
                     z.DIA_C+z.BAL+z.CR_fvs+
                     I(z.DIA_C^2)+z.PCCF+z.CCF+
                     z.ppt_Jul+z.tmax_FebJul+
                     (1+z.DIA_C|TRE_CN)+(1|Year),
                   data = glmm_df_z)
fvs_clim_2 <- lmer(log(tdds)~z.SICOND+z.sin+
                     z.cos+z.SLOPE+I(z.SLOPE^2)+
                     z.DIA_C+I(BAL/100)+CR_fvs+
                     I(z.DIA_C^2)+z.PCCF+z.CCF+
                     z.ppt_pOct+z.tmax_FebJul+
                     (1+z.DIA_C|TRE_CN)+(1|Year),
                   data = glmm_df_z)
fvs_clim_3 <- lmer(log(tdds)~z.SICOND+z.sin+
                     z.cos+z.SLOPE+I(z.SLOPE^2)+
                     z.DIA_C+I(BAL/100)+CR_fvs+
                     I(z.DIA_C^2)+z.PCCF+z.CCF+
                     z.ppt_pDec+z.tmax_FebJul+
                     (1+z.DIA_C|TRE_CN)+(1|Year),
                   data = glmm_df_z)
fvs_clim_4 <- lmer(log(tdds)~z.SICOND+z.sin+
                     z.cos+z.SLOPE+I(z.SLOPE^2)+
                     z.DIA_C+I(BAL/100)+CR_fvs+
                     I(z.DIA_C^2)+z.PCCF+z.CCF+
                     z.ppt_Jun+z.tmax_FebJul+
                     (1+z.DIA_C|TRE_CN)+(1|Year),
                   data = glmm_df_z)
fvs_clim_5 <- lmer(log(tdds)~z.SICOND+z.sin+
                     z.cos+z.SLOPE+I(z.SLOPE^2)+
                     z.DIA_C+I(BAL/100)+CR_fvs+
                     I(z.DIA_C^2)+z.PCCF+z.CCF+
                     z.ppt_pJunAug+z.tmax_FebJul+
                     (1+z.DIA_C|TRE_CN)+(1|Year),
                   data = glmm_df_z)
fvs_clim_6 <- lmer(log(tdds)~z.SICOND+z.sin+
                     z.cos+z.SLOPE+I(z.SLOPE^2)+
                     z.DIA_C+I(BAL/100)+CR_fvs+
                     I(z.DIA_C^2)+z.PCCF+z.CCF+
                     z.ppt_pAugOct+z.tmax_FebJul+
                     (1+z.DIA_C|TRE_CN)+(1|Year),
                   data = glmm_df_z)
fvs_clim_7 <- lmer(log(tdds)~z.SICOND+z.sin+
                     z.cos+z.SLOPE+I(z.SLOPE^2)+
                     z.DIA_C+I(BAL/100)+CR_fvs+
                     I(z.DIA_C^2)+z.PCCF+z.CCF+
                     z.ppt_MayJul+z.tmax_FebJul+
                     (1+z.DIA_C|TRE_CN)+(1|Year),
                   data = glmm_df_z)
fvs_clim_8 <- lmer(log(tdds)~z.SICOND+z.sin+
                     z.cos+z.SLOPE+I(z.SLOPE^2)+
                     z.DIA_C+I(BAL/100)+CR_fvs+
                     I(z.DIA_C^2)+z.PCCF+z.CCF+
                     z.ppt_FebJul+z.tmax_FebJul+
                     (1+z.DIA_C|TRE_CN)+(1|Year),
                   data = glmm_df_z)
fvs_clim_9 <- lmer(log(tdds)~z.SICOND+z.sin+
                     z.cos+z.SLOPE+I(z.SLOPE^2)+
                     z.DIA_C+I(BAL/100)+CR_fvs+
                     I(z.DIA_C^2)+z.PCCF+z.CCF+
                     z.ppt_pJunNov+z.tmax_FebJul+
                     (1+z.DIA_C|TRE_CN)+(1|Year),
                   data = glmm_df_z)
fvs_clim_10 <- lmer(log(tdds)~z.SICOND+z.sin+
                      z.cos+z.SLOPE+I(z.SLOPE^2)+
                      z.DIA_C+I(BAL/100)+CR_fvs+
                      I(z.DIA_C^2)+z.PCCF+z.CCF+
                     z.wateryr+z.tmax_FebJul+
                     (1+z.DIA_C|TRE_CN)+(1|Year),
                   data = glmm_df_z)
fvs_clim_11 <- lmer(log(tdds)~z.SICOND+z.sin+
                      z.cos+z.SLOPE+I(z.SLOPE^2)+
                      z.DIA_C+I(BAL/100)+CR_fvs+
                      I(z.DIA_C^2)+z.PCCF+z.CCF+
                      z.ppt_pJunSep+z.tmax_FebJul+
                      (1+z.DIA_C|TRE_CN)+(1|Year),
                    data = glmm_df_z)
AIC(fvs_clim_1,fvs_clim_2,fvs_clim_3,fvs_clim_4,fvs_clim_5,
    fvs_clim_6,fvs_clim_7,fvs_clim_8,fvs_clim_9,fvs_clim_10,fvs_clim_11)
#          df      AIC
#fvs_clim_1  19 10215.65
#fvs_clim_2  19 10214.50
#fvs_clim_3  19 10213.60
#fvs_clim_4  19 10200.93*
#fvs_clim_5  19 10183.85**
#fvs_clim_6  19 10214.95
#fvs_clim_7  19 10205.72*
#fvs_clim_8  19 10216.34
#fvs_clim_9  19 10210.34
#fvs_clim_10 19 10211.36
#fvs_clim_11 19 10207.58*

#average temp
#1 month: Feb tmin, Jul tmax
#3 month: tmax_Jun-Aug, tmax_pJul-pSep, tmin_Jan-Mar
#6 month + : Jun, Jul
fvs_clim_1 <- lmer(log(dds)~SICOND+I(sin(ASPECT-0.7854)*SLOPE)+
                     I(cos(ASPECT-0.7854)*SLOPE)+SLOPE+
                     I(log(DIA_C))+I(BAL/100)+CR_fvs+
                     I(DIA_C^2)+PCCF+I(CCF/100)+
                     ppt_pJunAug+tmax_Jul+
                     (1+DIA_C|TRE_CN)+(1|Year),
                   data = glmm_df_z)
fvs_clim_2 <- lmer(log(dds)~SICOND+I(sin(ASPECT-0.7854)*SLOPE)+
                     I(cos(ASPECT-0.7854)*SLOPE)+SLOPE+
                     I(log(DIA_C))+I(BAL/100)+CR_fvs+
                     I(DIA_C^2)+PCCF+I(CCF/100)+
                     ppt_pJunAug+tmin_Feb+
                     (1+DIA_C|TRE_CN)+(1|Year),
                   data = glmm_df_z)
fvs_clim_3 <- lmer(log(dds)~SICOND+I(sin(ASPECT-0.7854)*SLOPE)+
                      I(cos(ASPECT-0.7854)*SLOPE)+SLOPE+
                      I(log(DIA_C))+I(BAL/100)+CR_fvs+
                      I(DIA_C^2)+PCCF+I(CCF/100)+
                      ppt_pJunAug+tmin_JanMar+
                      (1+DIA_C|TRE_CN)+(1|Year),
                    data = glmm_df_z)
fvs_clim_4 <- lmer(log(dds)~SICOND+I(sin(ASPECT-0.7854)*SLOPE)+
                     I(cos(ASPECT-0.7854)*SLOPE)+SLOPE+
                     I(log(DIA_C))+I(BAL/100)+CR_fvs+
                     I(DIA_C^2)+PCCF+I(CCF/100)+
                     ppt_pJunAug+tmax_pJulSep+
                     (1+DIA_C|TRE_CN)+(1|Year),
                   data = glmm_df_z)
fvs_clim_5 <- lmer(log(dds)~SICOND+I(sin(ASPECT-0.7854)*SLOPE)+
                      I(cos(ASPECT-0.7854)*SLOPE)+SLOPE+
                      I(log(DIA_C))+I(BAL/100)+CR_fvs+
                      I(DIA_C^2)+PCCF+I(CCF/100)+
                      ppt_pJunAug+tmax_JunAug+
                      (1+DIA_C|TRE_CN)+(1|Year),
                    data = glmm_df_z)
fvs_clim_6 <- lmer(log(dds)~SICOND+I(sin(ASPECT-0.7854)*SLOPE)+
                     I(cos(ASPECT-0.7854)*SLOPE)+SLOPE+
                     I(log(DIA_C))+I(BAL/100)+CR_fvs+
                     I(DIA_C^2)+PCCF+I(CCF/100)+
                     ppt_pJunAug+tmax_FebJul+
                     (1+DIA_C|TRE_CN)+(1|Year),
                   data = glmm_df_z)
fvs_clim_7 <- lmer(log(dds)~SICOND+I(sin(ASPECT-0.7854)*SLOPE)+
                     I(cos(ASPECT-0.7854)*SLOPE)+SLOPE+
                     I(log(DIA_C))+I(BAL/100)+CR_fvs+
                     I(DIA_C^2)+PCCF+I(CCF/100)+
                     ppt_pJunAug+tmin_JanJun+
                     (1+DIA_C|TRE_CN)+(1|Year),
                   data = glmm_df_z)
fvs_clim_8 <- lmer(log(dds)~SICOND+I(sin(ASPECT-0.7854)*SLOPE)+
                     I(cos(ASPECT-0.7854)*SLOPE)+SLOPE+
                     I(log(DIA_C))+I(BAL/100)+CR_fvs+
                     I(DIA_C^2)+PCCF+I(CCF/100)+
                     ppt_pJunAug+tmax_JanJun+
                     (1+DIA_C|TRE_CN)+(1|Year),
                   data = glmm_df_z)
AIC(fvs_clim_1,fvs_clim_2,fvs_clim_3,fvs_clim_4,
    fvs_clim_5,fvs_clim_6,fvs_clim_7,fvs_clim_8)
#          df      AIC
#fvs_clim_1 18 9348.695
#fvs_clim_2 18 9348.754
#fvs_clim_3 18 9349.264
#fvs_clim_4 18 9348.031
#fvs_clim_5 18 9336.595
#fvs_clim_6 18 9320.838**max_febjul
#fvs_clim_7 18 9343.432
#fvs_clim_8 18 9327.618

#glmm
#reminder- can't compare AIC values between a transformed and nontransformed model

#stefan's reasoning for using gamma distribution
#tree ring variability is proportional to mean growth rate
meanvsd <- glmm_data_df %>%
  group_by(TRE_CN)%>%
  summarise(mean = mean(RW),
            sd = sd(RW))
plot(meanvsd$mean,meanvsd$sd, title(main = "DF tree ring variability vs mean growth rate"))

#determining interaction between latitude and climate
#southern Utah seems to be dominated by monsoon rains
#northern Utah seems to be dominated by winter rains

#first visualize data by plotting it on a map
library(maps)
m = map_data('state', region = 'Utah')

ggplot() + 
  geom_polygon( data=m, aes(x=long, y=lat,group=group),colour="black", fill="white" )+
  geom_point(data=data_all,aes(x=LON,y=LAT,color= SPCD))+
  ggtitle("Distribution of Douglas fir")+
  xlab('Longitude')+
  ylab('Latitude')+
  coord_fixed()

#or
library(ggmap)

#test interaction
spatial_data_df <- data_all_df %>%
  select(TRE_CN, RW, dds, Year, DIA_C, LAT,
         SICOND, ASPECT, SLOPE, BAL, CR, CR_fvs, PCCF, CCF,
         ppt_pOct, ppt_pDec, ppt_Jun, ppt_Jul,
         ppt_pJunAug, ppt_pAugOct, ppt_MayJul,
         ppt_pJunNov, ppt_FebJul, wateryr,
         tmin_Feb, tmax_Jul,
         tmax_JunAug, tmax_pJulSep, tmin_JanMar,
         tmax_JanJun,tmin_JanJun,tmax_FebJul) %>%
  filter(Year >= 1958)

spat_wateryr <- lmer(log(dds) ~ DIA_C + wateryr + LAT + wateryr:LAT + (1+DIA_C|TRE_CN) + (1|Year), data = spatial_data_df)
summary(spat_wateryr)

library(sjPlot)
library(sjmisc)
library(ggplot2)

plot_model(spat_wateryr, type = "pred", terms = c("wateryr", "LAT"))

spat_explore <- lmer(log(dds) ~ DIA_C + ppt_Jun + LAT + ppt_Jun:LAT + (1+DIA_C|TRE_CN) + (1|Year), data = spatial_data_df)
summary(spat_explore)
