#Linear mixed model for Engelmann Spruce
#Courtney Giebink
#clgiebink@gmail.com
#29 July 2019

#dataframe
#all data is provided in
load(file = './data/formatted/data_all_es')

data_all_es <- data_all_es %>%
  mutate(radians = tASPECT * (pi/180)) %>%
  mutate(sin = sin(radians - 0.7854) * SLOPE,
         cos = cos(radians - 0.7854) * SLOPE)

#create new dataframe with only variables needed for linear mixed model
#response: RW, dds
#covariates: SI, ASP, SL, DBH/DIA_C, BAL, CR, CCF, PCCF, climate
#random: TRE_CN, Year
#filter for last 30 years of growth
min(data_all_es$MEASYEAR) #1992 -> 1962

glmm_data_es <- data_all_es %>%
  dplyr::select(PLT_CN, FVS_LOC_CD, TRE_CN, RW, dds, Year, DIA_C, 
         SICOND, tASPECT, SLOPE, BAL, SDI, CR, CR_weib, PCCF, CCF, 
         cos, sin, solrad_an, solrad_JanApr, solrad_MayAug, solrad_SepDec,
         ppt_Jul, ppt_Apr,
         ppt_pJunAug, ppt_JunAug, ppt_pJulSep, ppt_pOctDec,
         ppt_pJunNov, wateryr, ppt_pAugJul, ppt_pJunSep,
         tmax_pAug, tmin_Feb, tmin_Mar,
         tmin_FebApr, tmax_pJulSep, tmin_JanMar, tmax_FebApr,
         tmin_pNovApr, tmax_pNovApr) %>%
  filter(Year >= 1962)

#climate
#total ppt
#1 month: Jul, Apr
#3 month: pJun-pAug, Jun-Aug, pJul-pSep, pOct-pDec
#6 month + : pNov, wateryr

#average temp
#1 month: tmax_pAug, Feb, Mar
#3 month: tmin_Feb-Apr, tmax_pJul-pSep, tmin_Jan-Mar
#6 month + : tmin_Apr

save(glmm_data_es, file = "./data/formatted/glmm_data_es.Rdata")

#Exploration

#Missing data
sum(is.na(glmm_data_es)) #0
summary(glmm_data_es)

#almost one tree per plot so don't need to have plot as random effect
length(unique(data_all_es$TRE_CN))
#95
length(unique(data_all_es$PLT_CN))
#76
tre_plt_es <- data_all_es %>% 
  select(PLT_CN,TRE_CN) %>%
  distinct() %>%
  group_by(PLT_CN) %>%
  summarise(n=n())
length(unique(tre_plt_es$PLT_CN))#76
max(tre_plt_es$n) #2

which(glmm_data_es$RW == 0) #2611 (row number)
#1 missing ring
#replace with smallest rw of on same core
glmm_data_es <- glmm_data_es %>%
  group_by(TRE_CN) %>%
  mutate(dds = ifelse(RW != 0, dds, 
                      min(dds[dds > 0])))
which(is.na(glmm_data_es$RW)) #0
which(glmm_data_es$dds == 0) #0

#site index
#wrong site species
#fix
for(i in 1:nrow(glmm_data_es)){
  TRE_CN <- glmm_data_es$TRE_CN[i]
  if(TRE_CN %in% si_match$TRE_CN){
    glmm_data_es$SICOND[i] <- si_match$SICOND[si_match$TRE_CN == TRE_CN]
  }
}

#distribution of the response variable
hist(glmm_data_es$RW,breaks = 50, main = "Histogram of RW (mm)", xlab = "Increment")
hist(glmm_data_es$dds,breaks = 100, main = "Histogram of DDS (in)", xlab = "Increment") 


#growth trends across time
library(ggplot2)

#understanding random effects: TRE_CN
growth_yr_plots_es <- glmm_data_es %>% 
  group_by(TRE_CN) %>%
  do(plots=ggplot(data=.) +
       aes(x=Year, y=RW) + geom_point() + ggtitle(unique(.$TRE_CN)))

for(i in nrow(growth_yr_plots_es)) {
  growth_yr_plots_es$plots[[i]]
}

#par(mfrow = c(2, 2)) doesn't work; why?
growth_yr_plots_es$plots[[1]]
growth_yr_plots_es$plots[[2]]
growth_yr_plots_es$plots[[3]]
growth_yr_plots_es$plots[[4]]
growth_yr_plots_es$plots[[5]]
growth_yr_plots_es$plots[[6]]
growth_yr_plots_es$plots[[7]]
growth_yr_plots_es$plots[[8]]
growth_yr_plots_es$plots[[9]]
growth_yr_plots_es$plots[[10]]
growth_yr_plots_es$plots[[11]]
growth_yr_plots_es$plots[[12]]
growth_yr_plots_es$plots[[13]]

#understanding random effects: Year
growth_wateryr_plots_es <- glmm_data_es %>% 
  group_by(Year) %>%
  do(plots=ggplot(data=.) +
       aes(x=wateryr, y=RW) + geom_point() +
       geom_smooth(method = "lm") + ggtitle(unique(.$Year)))

#par(mfrow = c(2, 2)) doesn't work
growth_wateryr_plots_es$plots[[1]]
growth_wateryr_plots_es$plots[[2]]
growth_wateryr_plots_es$plots[[3]]
growth_wateryr_plots_es$plots[[4]]
growth_wateryr_plots_es$plots[[5]]
growth_wateryr_plots_es$plots[[6]]
growth_wateryr_plots_es$plots[[7]]
growth_wateryr_plots_es$plots[[8]]
growth_wateryr_plots_es$plots[[9]]
growth_wateryr_plots_es$plots[[10]]
growth_wateryr_plots_es$plots[[11]]
growth_wateryr_plots_es$plots[[12]]
growth_wateryr_plots_es$plots[[13]]

growth_wateryr_plots_es$plots[[23]]
growth_wateryr_plots_es$plots[[24]]
growth_wateryr_plots_es$plots[[25]]
growth_wateryr_plots_es$plots[[26]]
growth_wateryr_plots_es$plots[[27]]
growth_wateryr_plots_es$plots[[28]]


#dbh as a proxy for age
#detrend
ggplot(data = glmm_data_es, aes(x = DIA_C, y = RW)) +
  geom_point(alpha=0.1) + geom_smooth(method = "lm")
#small skew on top of line

ggplot(data = glmm_data_es, aes(x = DIA_C, y = dds)) +
  geom_point(alpha=0.1) + geom_smooth(method = "lm")
#large skew on top of line

ggplot(data = glmm_data_es, aes(x = DIA_C, y = log(dds))) +
  geom_point(alpha=0.1) + geom_smooth(method = "lm")

#log to reduce heteroscedasticity
ggplot(data = glmm_data_es, aes(x = log(DIA_C), y = log(dds))) +
  geom_point(alpha=0.1) + geom_smooth(method = "lm")
#reduces a little
#negative relationship

#relationship of response with site index (SI)
ggplot(data = glmm_data_es, aes(x = SICOND, y = log(dds))) +
  geom_point(alpha=0.1) + geom_smooth(method = "lm")
#constant variance; positive relationship

#relationship of response with slope (SLOPE)
ggplot(data = glmm_data_es, aes(x = SLOPE, y = log(dds))) +
  geom_point(alpha=0.1) + geom_smooth(method = "lm")
#constant variance; negative relationship

#relationship of response with BAL
ggplot(data = glmm_data_es, aes(x = BAL, y = log(dds))) +
  geom_point(alpha=0.1) + geom_smooth(method = "lm")
#non constant variance (decreases); negative relationship

#relationship of response with PCCF
ggplot(data = glmm_data_es, aes(x = PCCF, y = log(dds))) +
  geom_point(alpha=0.1) + geom_smooth(method = "lm")
#constant variance; negative relationship

#relationship of response with CCF
ggplot(data = glmm_data_es, aes(x = CCF, y = log(dds))) +
  geom_point(alpha=0.1) + geom_smooth(method = "lm")
#non constant variance (decreases); negative relationship

#relationship of response with crown ratio (CR,CR_weib)
ggplot(data = glmm_data_es, aes(x = CR, y = log(dds))) +
  geom_point(alpha=0.1) + geom_smooth(method = "lm")
#variance increases (not bad); postive relationship

ggplot(data = glmm_data_es, aes(x = CR_weib, y = log(dds))) +
  geom_point(alpha=0.1) + geom_smooth(method = "lm")
#constant variance; slight negative relationship

#FVS_loc_cd
glmm_es_z %>%
  select(TRE_CN,FVS_LOC_CD) %>%
  distinct() %>%
  group_by(FVS_LOC_CD) %>%
  summarise(n=n())
#LOC_CD    n
#   401     9
#   404     1
#   407    43
#   408     1
#   410    13
#   418     3
#   419     8

##Model Building
library(lme4)
library(lmerTest)
#library(nlme)
#library(glmmTMB)

#reminder- can't compare AIC values between a transformed and nontransformed model

#standardize to encourage convergence
library(MuMIn)
glmm_es_z <- stdize(as.data.frame(glmm_data_es),append=TRUE)

#site index incorrect
#either reduce or calculate with height and age
glmm_es_z <- glmm_es_z %>%
  filter(!(TRE_CN %in% cal_si$TRE_CN))
length(unique(glmm_es_z$TRE_CN))
#95 -> 85
save(glmm_es_z,file = "./data/formatted/glmm_es_z.Rdata")


#collinearity
library(car)
#vif()
#3 or below

#AIC to compare climate variables
#total ppt
#1 month: Jul, Apr
#3 month: pJun-pAug, Jun-Aug, pJul-pSep, pOct-pDec
#6 month + : pNov, wateryr, pJunSep

fvs_clim_1 <- lmer(log(tdds)~z.SICOND+z.sin+
                     z.cos+z.SLOPE+I(z.SLOPE^2)+
                     z.DIA_C+I(BAL/100)+CR_weib+
                     I(z.DIA_C^2)+z.PCCF+z.CCF+
                     z.ppt_Jul+z.tmax_pAug+
                     (1+z.DIA_C|TRE_CN)+(1|Year),
                   data = glmm_es_z)
fvs_clim_2 <- lmer(log(tdds)~z.SICOND+z.sin+
                     z.cos+z.SLOPE+I(z.SLOPE^2)+
                     z.DIA_C+I(BAL/100)+CR_weib+
                     I(z.DIA_C^2)+z.PCCF+z.CCF+
                     z.ppt_Apr+z.tmax_pAug+
                     (1+z.DIA_C|TRE_CN)+(1|Year),
                   data = glmm_es_z)
fvs_clim_3 <- lmer(log(tdds)~z.SICOND+z.sin+
                     z.cos+z.SLOPE+I(z.SLOPE^2)+
                     z.DIA_C+I(BAL/100)+CR_weib+
                     I(z.DIA_C^2)+z.PCCF+z.CCF+
                     z.ppt_pJunAug+z.tmax_pAug+
                     (1+z.DIA_C|TRE_CN)+(1|Year),
                   data = glmm_es_z)
fvs_clim_4 <- lmer(log(tdds)~z.SICOND+z.sin+
                     z.cos+z.SLOPE+I(z.SLOPE^2)+
                     z.DIA_C+I(BAL/100)+CR_weib+
                     I(z.DIA_C^2)+z.PCCF+z.CCF+
                     z.ppt_JunAug+z.tmax_pAug+
                     (1+z.DIA_C|TRE_CN)+(1|Year),
                   data = glmm_es_z)
fvs_clim_5 <- lmer(log(tdds)~z.SICOND+z.sin+
                     z.cos+z.SLOPE+I(z.SLOPE^2)+
                     z.DIA_C+I(BAL/100)+CR_weib+
                     I(z.DIA_C^2)+z.PCCF+z.CCF+
                     z.ppt_pJulSep+z.tmax_pAug+
                     (1+z.DIA_C|TRE_CN)+(1|Year),
                   data = glmm_es_z)
fvs_clim_6 <- lmer(log(tdds)~z.SICOND+z.sin+
                     z.cos+z.SLOPE+I(z.SLOPE^2)+
                     z.DIA_C+I(BAL/100)+CR_weib+
                     I(z.DIA_C^2)+z.PCCF+z.CCF+
                     z.ppt_pOctDec+z.tmax_pAug+
                     (1+z.DIA_C|TRE_CN)+(1|Year),
                   data = glmm_es_z)
fvs_clim_7 <- lmer(log(tdds)~z.SICOND+z.sin+
                     z.cos+z.SLOPE+I(z.SLOPE^2)+
                     z.DIA_C+I(BAL/100)+CR_weib+
                     I(z.DIA_C^2)+z.PCCF+z.CCF+
                     z.ppt_pJunNov+z.tmax_pAug+
                     (1+z.DIA_C|TRE_CN)+(1|Year),
                   data = glmm_es_z)
fvs_clim_8 <- lmer(log(tdds)~z.SICOND+z.sin+
                     z.cos+z.SLOPE+I(z.SLOPE^2)+
                     z.DIA_C+I(BAL/100)+CR_weib+
                     I(z.DIA_C^2)+z.PCCF+z.CCF+
                     z.wateryr+z.tmax_pAug+
                     (1+z.DIA_C|TRE_CN)+(1|Year),
                   data = glmm_es_z)
fvs_clim_9 <- lmer(log(tdds)~z.SICOND+z.sin+
                     z.cos+z.SLOPE+I(z.SLOPE^2)+
                     z.DIA_C+I(BAL/100)+CR_weib+
                     I(z.DIA_C^2)+z.PCCF+z.CCF+
                     z.ppt_pJunSep+z.tmax_pAug+
                     (1+z.DIA_C|TRE_CN)+(1|Year),
                   data = glmm_es_z)
AIC(fvs_clim_1,fvs_clim_2,fvs_clim_3,fvs_clim_4,fvs_clim_5,
    fvs_clim_6,fvs_clim_7,fvs_clim_8,fvs_clim_9)

#temperature
#1 month: tmax_pAug, Feb, Mar
#3 month: tmin_Feb-Apr, tmax_pJul-pSep, tmin_Jan-Mar
#6 month + : tmin_Apr
fvs_clim_1 <- lmer(log(tdds)~z.SICOND+z.sin+
                     z.cos+z.SLOPE+I(z.SLOPE^2)+
                     z.DIA_C+I(BAL/100)+CR_weib+
                     I(z.DIA_C^2)+z.PCCF+z.CCF+
                     z.wateryr+z.tmax_pAug+
                     (1+z.DIA_C|TRE_CN)+(1|Year),
                   data = glmm_es_z)
fvs_clim_2 <- lmer(log(tdds)~z.SICOND+z.sin+
                     z.cos+z.SLOPE+I(z.SLOPE^2)+
                     z.DIA_C+I(BAL/100)+CR_weib+
                     I(z.DIA_C^2)+z.PCCF+z.CCF+
                     z.wateryr+z.tmin_Feb+
                     (1+z.DIA_C|TRE_CN)+(1|Year),
                   data = glmm_es_z)
fvs_clim_21 <- lmer(log(tdds)~z.SICOND+z.sin+
                      z.cos+z.SLOPE+I(z.SLOPE^2)+
                      z.DIA_C+I(BAL/100)+CR_weib+
                      I(z.DIA_C^2)+z.PCCF+z.CCF+
                      z.wateryr+z.tmax_Feb+
                     (1+z.DIA_C|TRE_CN)+(1|Year),
                   data = glmm_es_z)
fvs_clim_3 <- lmer(log(tdds)~z.SICOND+z.sin+
                     z.cos+z.SLOPE+I(z.SLOPE^2)+
                     z.DIA_C+I(BAL/100)+CR_weib+
                     I(z.DIA_C^2)+z.PCCF+z.CCF+
                     z.wateryr+z.tmin_Mar+
                     (1+z.DIA_C|TRE_CN)+(1|Year),
                   data = glmm_es_z)
fvs_clim_31 <- lmer(log(tdds)~z.SICOND+z.sin+
                      z.cos+z.SLOPE+I(z.SLOPE^2)+
                      z.DIA_C+I(BAL/100)+CR_weib+
                      I(z.DIA_C^2)+z.PCCF+z.CCF+
                      z.wateryr+z.tmax_Mar+
                     (1+z.DIA_C|TRE_CN)+(1|Year),
                   data = glmm_es_z)
fvs_clim_4 <- lmer(log(tdds)~z.SICOND+z.sin+
                     z.cos+z.SLOPE+I(z.SLOPE^2)+
                     z.DIA_C+I(BAL/100)+CR_weib+
                     I(z.DIA_C^2)+z.PCCF+z.CCF+
                     z.wateryr+z.tmin_FebApr+
                     (1+z.DIA_C|TRE_CN)+(1|Year),
                   data = glmm_es_z)
fvs_clim_5 <- lmer(log(tdds)~z.SICOND+z.sin+
                     z.cos+z.SLOPE+I(z.SLOPE^2)+
                     z.DIA_C+I(BAL/100)+CR_weib+
                     I(z.DIA_C^2)+z.PCCF+z.CCF+
                     z.wateryr+z.tmax_pJulSep+
                     (1+z.DIA_C|TRE_CN)+(1|Year),
                   data = glmm_es_z)
fvs_clim_6 <- lmer(log(tdds)~z.SICOND+z.sin+
                     z.cos+z.SLOPE+I(z.SLOPE^2)+
                     z.DIA_C+I(BAL/100)+CR_weib+
                     I(z.DIA_C^2)+z.PCCF+z.CCF+
                     z.wateryr+z.tmin_JanMar+
                     (1+z.DIA_C|TRE_CN)+(1|Year),
                   data = glmm_es_z)
fvs_clim_7 <- lmer(log(tdds)~z.SICOND+z.sin+
                     z.cos+z.SLOPE+I(z.SLOPE^2)+
                     z.DIA_C+I(BAL/100)+CR_weib+
                     I(z.DIA_C^2)+z.PCCF+z.CCF+
                     z.wateryr+z.tmin_pNovApr+
                     (1+z.DIA_C|TRE_CN)+(1|Year),
                   data = glmm_es_z)
AIC(fvs_clim_1,fvs_clim_2,fvs_clim_3,
    fvs_clim_4,fvs_clim_5,fvs_clim_6,fvs_clim_7)

#glmm
#reminder- can't compare AIC values between a transformed and nontransformed model

#stefan's reasoning for using gamma distribution
#tree ring variability is proportional to mean growth rate
meanvsd <- glmm_data_es %>%
  group_by(TRE_CN)%>%
  summarise(mean = mean(RW),
            sd = sd(RW))
plot(meanvsd$mean,meanvsd$sd, title(main = "ES tree ring variability vs mean growth rate"))


#determining interaction between latitude and climate
#southern Utah seems to be dominated by monsoon rains
#northern Utah seems to be dominated by winter rains

#first visualize data by plotting it on a map
library(maps)
m = map_data('state', region = 'Utah')

ggplot() + 
  geom_polygon( data=m, aes(x=long, y=lat,group=group),colour="black", fill="white" )+
  geom_point(data=data_all_es,aes(x=LON,y=LAT),colour="red")+
  ggtitle("Distribution of Engelmann spruce")+
  xlab('Longitude')+
  ylab('Latitude')+
  coord_fixed()

#or
library(ggmap)


#test interaction
spatial_data_df <- data_all_es %>%
  select(TRE_CN, RW, dds, Year, DIA_C, LAT,
         SICOND, ASPECT, SLOPE, BAL, CR, CR_weib, PCCF, CCF,
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

