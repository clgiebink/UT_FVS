#Linear mixed model for Engelmann Spruce
#Courtney Giebink
#clgiebink@gmail.com
#29 July 2019

#dataframe
#all data is provided in
load(file = './data/formatted/data_all_es')

#create new dataframe with only variables needed for linear mixed model
#response: RW, dds
#covariates: SI, ASP, SL, DBH/DIA_C, BAL, CR, CCF, PCCF, climate
#random: TRE_CN, Year
#filter for last 30 years of growth
min(data_all_es$MEASYEAR) #1992 -> 1962

glmm_data_es <- data_all_es %>%
  dplyr::select(PLT_CN,TRE_CN, RW, dds, Year, DIA_C, 
         SICOND, ASPECT, SLOPE, BAL, CR, CR_weib, PCCF, CCF,
         ppt_Jul, ppt_Apr,
         ppt_pJunAug, ppt_JunAug, ppt_pJulSep, ppt_pOctDec,
         ppt_pJunNov, wateryr,
         tmax_pAug, tmin_Feb, tmin_Mar,
         tmin_FebApr, tmax_pJulSep, tmin_JanMar,
         tmin_pNovApr) %>%
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

save(glmm_data_es, file = "./data/formatted/glmm_data_es")

#Exploration

#Missing data
sum(is.na(glmm_data_es)) #66
summary(glmm_data_es)
#ASPECT
unique(glmm_data_es$TRE_CN[is.na(glmm_data_es$ASPECT)]) #2
glmm_data_es$ASPECT %>% replace_na(0)


#almost one tree per plot so don't need to have plot as random effect
length(unique(data_all_es$TRE_CN))
#50
length(unique(data_all_es$PLT_CN))
#45

#distribution of the response variable
hist(glmm_data_es$RW,breaks = 50, main = "Histogram of RW (mm)", xlab = "Increment")
hist(glmm_data_es$dds,breaks = 100, main = "Histogram of DDS (in)", xlab = "Increment") 

which(glmm_data_es$RW == 0) #1396
#1 missing ring
which(is.na(glmm_data_es$RW)) #0

#growth trends across time
library(ggplot2)
#library(bit64)
#as.integer64(.Machine$integer.max) + 1L
ggplot(data = glmm_data_es, aes(x = Year, y = RW)) +
  geom_line(colour = glmm_data_es$TRE_CN, group = glmm_data_es$TRE_CN) +
  xlab("Year of Growth") + ylab("Increment (in)")
#error


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

##Model Building
library(lme4)
library(lmerTest)
#library(nlme)
#library(glmmTMB)

#reminder- can't compare AIC values between a transformed and nontransformed model

#standardize to encourage convergence
library(MuMIn)
glmm_data_es <- as.data.frame(glmm_data_es)
glmm_es_z <- stdize(glmm_data_es,append=TRUE)
save(glmm_es_z,file = "./data/formatted/glmm_es_z")

#slope^2, dbh^2, and pccf are insignificant in current lm
old_fvs <- lm(log(dds+0.001)~SICOND+I(sin(ASPECT-0.7854)*SLOPE)+
               I(cos(ASPECT-0.7854)*SLOPE)+SLOPE+I(SLOPE^2)+
               I(log(DIA_C))+I(BAL/100)+CR_weib+I(CR_weib^2)+
               I(DIA_C^2)+PCCF+I(CCF/100),
             data = glmm_es_z)
summary(old_fvs)
#CR, dbh^2, CCF/100 insignificant

#add climate
fvs_clim_mod <- lmer(log(dds+0.001)~SICOND+I(sin(ASPECT-0.7854)*SLOPE)+
                I(cos(ASPECT-0.7854)*SLOPE)+SLOPE+I(SLOPE^2)+
                I(log(DIA_C))+I(BAL/100)+CR_weib+I(CR_weib^2)+
                I(DIA_C^2)+PCCF+I(CCF/100)+wateryr+tmin_FebApr+
                (1+DIA_C|TRE_CN)+(1|Year),
              data = glmm_es_z)
#center and scale
fvs_clim_mod <- glmer((dds+0.001)~z.SICOND+I(sin(z.ASPECT-0.7854)*z.SLOPE)+
                       I(cos(z.ASPECT-0.7854)*z.SLOPE)+z.SLOPE+I(z.SLOPE^2)+
                       I(log(DIA_C))+I(z.BAL/100)+z.CR_weib+I(z.CR_weib^2)+
                       I(z.DIA_C^2)+z.PCCF+I(z.CCF/100)+z.wateryr+z.tmin_FebApr+
                       (1+z.DIA_C|TRE_CN)+(1|Year),
                     family = Gamma(link = "log"),
                     data = glmm_es_z)
#log(DIA) error; can't take log of negative number

#collinearity
library(car)
vif(fvs_clim_mod)
#SLOPE and CR_weib

fvs_clim_red <- lmer(log(dds+0.001)~SICOND+I(sin(ASPECT-0.7854)*SLOPE)+
                       I(cos(ASPECT-0.7854)*SLOPE)+SLOPE+
                       I(log(DIA_C))+I(BAL/100)+CR_weib+
                       I(DIA_C^2)+PCCF+I(CCF/100)+
                       wateryr+tmin_FebApr+
                       (1+DIA_C|TRE_CN)+(1|Year),
                     data = glmm_es_z)
vif(fvs_clim_red)
#now 2 or below
summary(fvs_clim_red)
#signs make sense

#AIC to compare climate variables
#total ppt
#1 month: Jul, Apr
#3 month: pJun-pAug, Jun-Aug, pJul-pSep, pOct-pDec
#6 month + : pNov, wateryr

fvs_clim_1 <- lmer(log(dds+0.001)~SICOND+I(sin(ASPECT-0.7854)*SLOPE)+
                       I(cos(ASPECT-0.7854)*SLOPE)+SLOPE+
                       I(log(DIA_C))+I(BAL/100)+CR_weib+
                       I(DIA_C^2)+PCCF+I(CCF/100)+
                       ppt_Jul+tmax_pAug+
                       (1+DIA_C|TRE_CN)+(1|Year),
                     data = glmm_es_z)
fvs_clim_2 <- lmer(log(dds+0.001)~SICOND+I(sin(ASPECT-0.7854)*SLOPE)+
                       I(cos(ASPECT-0.7854)*SLOPE)+SLOPE+
                       I(log(DIA_C))+I(BAL/100)+CR_weib+
                       I(DIA_C^2)+PCCF+I(CCF/100)+
                       ppt_Apr+tmax_pAug+
                       (1+DIA_C|TRE_CN)+(1|Year),
                     data = glmm_es_z)
fvs_clim_3 <- lmer(log(dds+0.001)~SICOND+I(sin(ASPECT-0.7854)*SLOPE)+
                       I(cos(ASPECT-0.7854)*SLOPE)+SLOPE+
                       I(log(DIA_C))+I(BAL/100)+CR_weib+
                       I(DIA_C^2)+PCCF+I(CCF/100)+
                       ppt_pJunAug+tmax_pAug+
                       (1+DIA_C|TRE_CN)+(1|Year),
                     data = glmm_es_z)
fvs_clim_4 <- lmer(log(dds+0.001)~SICOND+I(sin(ASPECT-0.7854)*SLOPE)+
                     I(cos(ASPECT-0.7854)*SLOPE)+SLOPE+
                     I(log(DIA_C))+I(BAL/100)+CR_weib+
                     I(DIA_C^2)+PCCF+I(CCF/100)+
                     ppt_JunAug+tmax_pAug+
                     (1+DIA_C|TRE_CN)+(1|Year),
                   data = glmm_es_z)
fvs_clim_5 <- lmer(log(dds+0.001)~SICOND+I(sin(ASPECT-0.7854)*SLOPE)+
                     I(cos(ASPECT-0.7854)*SLOPE)+SLOPE+
                     I(log(DIA_C))+I(BAL/100)+CR_weib+
                     I(DIA_C^2)+PCCF+I(CCF/100)+
                     ppt_pJulSep+tmax_pAug+
                     (1+DIA_C|TRE_CN)+(1|Year),
                   data = glmm_es_z)
fvs_clim_6 <- lmer(log(dds+0.001)~SICOND+I(sin(ASPECT-0.7854)*SLOPE)+
                     I(cos(ASPECT-0.7854)*SLOPE)+SLOPE+
                     I(log(DIA_C))+I(BAL/100)+CR_weib+
                     I(DIA_C^2)+PCCF+I(CCF/100)+
                     ppt_pOctDec+tmax_pAug+
                     (1+DIA_C|TRE_CN)+(1|Year),
                   data = glmm_es_z)
fvs_clim_7 <- lmer(log(dds+0.001)~SICOND+I(sin(ASPECT-0.7854)*SLOPE)+
                     I(cos(ASPECT-0.7854)*SLOPE)+SLOPE+
                     I(log(DIA_C))+I(BAL/100)+CR_weib+
                     I(DIA_C^2)+PCCF+I(CCF/100)+
                     ppt_pJunNov+tmax_pAug+
                     (1+DIA_C|TRE_CN)+(1|Year),
                   data = glmm_es_z)
fvs_clim_8 <- lmer(log(dds+0.001)~SICOND+I(sin(ASPECT-0.7854)*SLOPE)+
                     I(cos(ASPECT-0.7854)*SLOPE)+SLOPE+
                     I(log(DIA_C))+I(BAL/100)+CR_weib+
                     I(DIA_C^2)+PCCF+I(CCF/100)+
                     wateryr+tmax_pAug+
                     (1+DIA_C|TRE_CN)+(1|Year),
                   data = glmm_es_z)
AIC(fvs_clim_1,fvs_clim_2,fvs_clim_3,fvs_clim_4,
           fvs_clim_5,fvs_clim_6,fvs_clim_7,fvs_clim_8)
#          df      AIC
#fvs_clim_1 18 1824.233
#fvs_clim_2 18 1824.192
#fvs_clim_3 18 1818.055**pJunAug
#fvs_clim_4 18 1824.221*JunAug after temp change
#fvs_clim_5 18 1820.753
#fvs_clim_6 18 1826.431
#fvs_clim_7 18 1824.770
#fvs_clim_8 18 1827.574

#temperature
#1 month: tmax_pAug, Feb, Mar
#3 month: tmin_Feb-Apr, tmax_pJul-pSep, tmin_Jan-Mar
#6 month + : tmin_Apr
fvs_clim_1 <- lmer(log(dds+0.001)~SICOND+I(sin(ASPECT-0.7854)*SLOPE)+
                     I(cos(ASPECT-0.7854)*SLOPE)+SLOPE+
                     I(log(DIA_C))+I(BAL/100)+CR_weib+
                     I(DIA_C^2)+PCCF+I(CCF/100)+
                     ppt_JunAug+tmax_pAug+
                     (1+DIA_C|TRE_CN)+(1|Year),
                   data = glmm_es_z)
fvs_clim_2 <- lmer(log(dds+0.001)~SICOND+I(sin(ASPECT-0.7854)*SLOPE)+
                     I(cos(ASPECT-0.7854)*SLOPE)+SLOPE+
                     I(log(DIA_C))+I(BAL/100)+CR_weib+
                     I(DIA_C^2)+PCCF+I(CCF/100)+
                     ppt_JunAug+tmin_Feb+
                     (1+DIA_C|TRE_CN)+(1|Year),
                   data = glmm_es_z)
fvs_clim_21 <- lmer(log(dds+0.001)~SICOND+I(sin(ASPECT-0.7854)*SLOPE)+
                     I(cos(ASPECT-0.7854)*SLOPE)+SLOPE+
                     I(log(DIA_C))+I(BAL/100)+CR_weib+
                     I(DIA_C^2)+PCCF+I(CCF/100)+
                     ppt_JunAug+tmax_Feb+
                     (1+DIA_C|TRE_CN)+(1|Year),
                   data = glmm_es_z)
fvs_clim_3 <- lmer(log(dds+0.001)~SICOND+I(sin(ASPECT-0.7854)*SLOPE)+
                     I(cos(ASPECT-0.7854)*SLOPE)+SLOPE+
                     I(log(DIA_C))+I(BAL/100)+CR_weib+
                     I(DIA_C^2)+PCCF+I(CCF/100)+
                     ppt_JunAug+tmin_Mar+
                     (1+DIA_C|TRE_CN)+(1|Year),
                   data = glmm_es_z)
fvs_clim_31 <- lmer(log(dds+0.001)~SICOND+I(sin(ASPECT-0.7854)*SLOPE)+
                     I(cos(ASPECT-0.7854)*SLOPE)+SLOPE+
                     I(log(DIA_C))+I(BAL/100)+CR_weib+
                     I(DIA_C^2)+PCCF+I(CCF/100)+
                     ppt_JunAug+tmax_Mar+
                     (1+DIA_C|TRE_CN)+(1|Year),
                   data = glmm_es_z)
fvs_clim_4 <- lmer(log(dds+0.001)~SICOND+I(sin(ASPECT-0.7854)*SLOPE)+
                     I(cos(ASPECT-0.7854)*SLOPE)+SLOPE+
                     I(log(DIA_C))+I(BAL/100)+CR_weib+
                     I(DIA_C^2)+PCCF+I(CCF/100)+
                     ppt_JunAug+tmin_FebApr+
                     (1+DIA_C|TRE_CN)+(1|Year),
                   data = glmm_es_z)
fvs_clim_5 <- lmer(log(dds+0.001)~SICOND+I(sin(ASPECT-0.7854)*SLOPE)+
                     I(cos(ASPECT-0.7854)*SLOPE)+SLOPE+
                     I(log(DIA_C))+I(BAL/100)+CR_weib+
                     I(DIA_C^2)+PCCF+I(CCF/100)+
                     ppt_JunAug+tmax_pJulSep+
                     (1+DIA_C|TRE_CN)+(1|Year),
                   data = glmm_es_z)
fvs_clim_6 <- lmer(log(dds+0.001)~SICOND+I(sin(ASPECT-0.7854)*SLOPE)+
                     I(cos(ASPECT-0.7854)*SLOPE)+SLOPE+
                     I(log(DIA_C))+I(BAL/100)+CR_weib+
                     I(DIA_C^2)+PCCF+I(CCF/100)+
                     ppt_JunAug+tmin_JanMar+
                     (1+DIA_C|TRE_CN)+(1|Year),
                   data = glmm_es_z)
fvs_clim_7 <- lmer(log(dds+0.001)~SICOND+I(sin(ASPECT-0.7854)*SLOPE)+
                     I(cos(ASPECT-0.7854)*SLOPE)+SLOPE+
                     I(log(DIA_C))+I(BAL/100)+CR_weib+
                     I(DIA_C^2)+PCCF+I(CCF/100)+
                     ppt_JunAug+tmin_pNovApr+
                     (1+DIA_C|TRE_CN)+(1|Year),
                   data = glmm_es_z)
AIC(fvs_clim_1,fvs_clim_2,fvs_clim_3,fvs_clim_4,
    fvs_clim_5,fvs_clim_6,fvs_clim_7)
#          df      AIC
#fvs_clim_1 18 1796.345**pAug 1794
#fvs_clim_2 18 1819.616
#fvs_clim_3 18 1817.436
#fvs_clim_4 18 1818.055
#fvs_clim_5 18 1797.422**pJulSep
#fvs_clim_6 18 1817.751
#fvs_clim_7 18 1817.683

summary(fvs_clim_1)
#remove based on significance
#CCF
fvs_clim_red <- lmer(log(dds+0.001)~SICOND+I(sin(ASPECT-0.7854)*SLOPE)+
                       I(cos(ASPECT-0.7854)*SLOPE)+SLOPE+
                       I(log(DIA_C))+I(BAL/100)+CR_weib+
                       I(DIA_C^2)+PCCF+
                       ppt_JunAug+tmax_pAug+
                       (1+DIA_C|TRE_CN)+(1|Year),
                     data = glmm_es_z)
AIC(fvs_clim_red)#1790
summary(fvs_clim_red)

#CR
fvs_clim_red <- lmer(log(dds+0.001)~SICOND+I(sin(ASPECT-0.7854)*SLOPE)+
                       I(cos(ASPECT-0.7854)*SLOPE)+SLOPE+
                       I(log(DIA_C))+I(BAL/100)+
                       I(DIA_C^2)+PCCF+
                       ppt_JunAug+tmax_pAug+
                       (1+DIA_C|TRE_CN)+(1|Year),
                     data = glmm_es_z)
AIC(fvs_clim_red)#1781
summary(fvs_clim_red)
#precipitation still isn't significant

#what about interactions?
fvs_clim_int <- lmer(log(dds+0.001)~SICOND+I(sin(ASPECT-0.7854)*SLOPE)+
                       I(cos(ASPECT-0.7854)*SLOPE)+SLOPE+
                       I(log(DIA_C))+I(BAL/100)+
                       I(DIA_C^2)+PCCF+
                       ppt_JunAug*tmax_pAug+
                       (1+DIA_C|TRE_CN)+(1|Year),
                     data = glmm_es_z)
AIC(fvs_clim_int)
summary(fvs_clim_int)
#interactions not significant

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


##Model Selection
