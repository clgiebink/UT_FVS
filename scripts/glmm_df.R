#Linear mixed model for Douglas fir
#Courtney Giebink
#clgiebink@gmail.com
#29 July 2019

#dataframe
#all data is provided in
load(file = '~/data/formatted/data_all_df.Rdata')

data_all_df <- data_all_df %>%
  mutate(tASPECT = ifelse(is.na(ASPECT),0,ASPECT)) %>%
  mutate(sin = sin(tASPECT - 0.7854) * SLOPE,
         cos = cos(tASPECT - 0.7854) * SLOPE)

#data_all_df <- data_all_df %>%
#  mutate(dds = (2*RW*0.0393701)^2) #mm to inches

#create new dataframe with only variables needed for linear mixed model
#response: RW, dds
#covariates: SI, ASP, SL, DBH/DIA_C, BAL, CR, CCF, PCCF, climate
#random: TRE_CN, Year
#filter for last 30 years of growth
min(data_all_df$MEASYEAR) #1988 -> 1958

glmm_data_df <- data_all_df %>%
  ungroup() %>%
  dplyr::select(PLT_CN, TRE_CN, RW, dds, Year, DIA_C, LAT,
         SICOND, ASPECT, tASPECT, SLOPE, BAL, CR, CR_weib, PCCF, CCF, cos, sin,
         ppt_pOct, ppt_pDec, ppt_Jun, ppt_Jul,
         ppt_pJunAug, ppt_pAugOct, ppt_MayJul,
         ppt_pJunNov, ppt_FebJul, wateryr,
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
sum(is.na(glmm_data_df)) #204
summary(glmm_data_df)
#ASPECT - 169
#ppt_pOct - 5
#ppt_pDec - 5
#ppt_pJunAug - 5
#ppt_pAugOct - 5
#ppt_pJunNov - 5
#wateryr - 5
#tmax_pJulSep - 5
miss_asp_df <- unique(glmm_data_df$TRE_CN[is.na(glmm_data_df$ASPECT)]) #5
# 2.876521e+12 2.880164e+12 2.881010e+12 2.881027e+12 2.872497e+12
asp_check_df <- per_cov %>%
  filter(TRE_CN %in% miss_asp_df)
glmm_data_df <- glmm_data_df %>%
  replace_na(ASPECT,0)

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
#131
length(unique(data_all_df$PLT_CN))
#114

#distribution of the response variable
hist(glmm_data_df$RW,breaks = 50, main = "Histogram of RW (mm)", xlab = "Increment")
hist(glmm_data_df$dds,breaks = 100, main = "Histogram of DDS (in)", xlab = "Increment") 

which(glmm_data_df$RW == 0) #0
#no missing rings
which(is.na(glmm_data_df$RW)) #0

#growth trends across time
library(ggplot2)
#library(bit64)
#as.integer64(.Machine$integer.max) + 1L
ggplot(data = glmm_data_df, aes(x = Year, y = RW)) +
  geom_line(colour = glmm_data_df$TRE_CN, group = glmm_data_df$TRE_CN) +
  xlab("Year of Growth") + ylab("Increment (in)")
#error


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

#relationship of response with crown ratio (CR,CR_weib)
ggplot(data = glmm_data_df, aes(x = CR, y = log(dds))) +
  geom_point(alpha=0.1) + geom_smooth(method = "lm")
#variance increases (not bad); postive relationship

ggplot(data = glmm_data_df, aes(x = CR_weib, y = log(dds))) +
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
save(glmm_df_z,file = "./data/formatted/glmm_df_z.Rdata")
#remove missing data
glmm_df_z <- glmm_df_z %>%
  filter(!is.na(z.wateryr))

#dbh^2 and ccf are insignificant in current lm
old_fvs <- lm(log(dds)~SICOND+I(sin(ASPECT-0.7854)*SLOPE)+
                I(cos(ASPECT-0.7854)*SLOPE)+SLOPE+I(SLOPE^2)+
                I(log(DIA_C))+I(BAL/100)+CR_weib+I(CR_weib^2)+
                I(DIA_C^2)+PCCF+I(CCF/100),
              data = glmm_df_z)
summary(old_fvs)
#slope insignificant


#add climate
fvs_clim_mod <- lmer(log(dds)~SICOND+I(sin(ASPECT-0.7854)*SLOPE)+
                       I(cos(ASPECT-0.7854)*SLOPE)+SLOPE+I(SLOPE^2)+
                       I(log(DIA_C))+I(BAL/100)+CR_weib+I(CR_weib^2)+
                       I(DIA_C^2)+PCCF+I(CCF/100)+wateryr+tmax_JanJun+
                       (1+DIA_C|TRE_CN)+(1|Year),
                     data = glmm_df_z)
#center and scale
fvs_clim_mod <- glmer((dds)~z.SICOND+I(sin(z.ASPECT-0.7854)*z.SLOPE)+
                        I(cos(z.ASPECT-0.7854)*z.SLOPE)+z.SLOPE+I(z.SLOPE^2)+
                        I(log(DIA_C))+I(z.BAL/100)+z.CR_weib+I(z.CR_weib^2)+
                        I(z.DIA_C^2)+z.PCCF+I(z.CCF/100)+z.wateryr+z.tmax_JanJun+
                        (1+z.DIA_C|TRE_CN)+(1|Year),
                      family = Gamma(link = "log"),
                      data = glmm_df_z)
#log(DIA) error; can't take log of negative number

#collinearity
library(car)
vif(fvs_clim_mod)
#SLOPE and CR_weib

fvs_clim_red <- lmer(log(dds)~SICOND+I(sin(ASPECT-0.7854)*SLOPE)+
                       I(cos(ASPECT-0.7854)*SLOPE)+SLOPE+
                       I(log(DIA_C))+I(BAL/100)+CR_weib+
                       I(DIA_C^2)+PCCF+I(CCF/100)+
                       wateryr+tmax_JanJun+
                       (1+DIA_C|TRE_CN)+(1|Year),
                     data = glmm_df_z)
vif(fvs_clim_red)
#now 2 or below
summary(fvs_clim_red)
#signs make sense

#AIC to compare climate variables
#total ppt
#1 month: pOct,pDec, Jun, Jul
#3 month: pJun-pAug, pAug-pOct, May-Jul
#6 month + : pNov, Jul, wateryr

fvs_clim_1 <- lmer(log(dds)~SICOND+I(sin(ASPECT-0.7854)*SLOPE)+
                     I(cos(ASPECT-0.7854)*SLOPE)+SLOPE+
                     I(log(DIA_C))+I(BAL/100)+CR_weib+
                     I(DIA_C^2)+PCCF+I(CCF/100)+
                     ppt_Jul+tmax_FebJul+
                     (1+DIA_C|TRE_CN)+(1|Year),
                   data = glmm_df_z)
fvs_clim_2 <- lmer(log(dds)~SICOND+I(sin(ASPECT-0.7854)*SLOPE)+
                     I(cos(ASPECT-0.7854)*SLOPE)+SLOPE+
                     I(log(DIA_C))+I(BAL/100)+CR_weib+
                     I(DIA_C^2)+PCCF+I(CCF/100)+
                     ppt_pOct+tmax_FebJul+
                     (1+DIA_C|TRE_CN)+(1|Year),
                   data = glmm_df_z)
fvs_clim_3 <- lmer(log(dds)~SICOND+I(sin(ASPECT-0.7854)*SLOPE)+
                     I(cos(ASPECT-0.7854)*SLOPE)+SLOPE+
                     I(log(DIA_C))+I(BAL/100)+CR_weib+
                     I(DIA_C^2)+PCCF+I(CCF/100)+
                     ppt_pDec+tmax_FebJul+
                     (1+DIA_C|TRE_CN)+(1|Year),
                   data = glmm_df_z)
fvs_clim_4 <- lmer(log(dds)~SICOND+I(sin(ASPECT-0.7854)*SLOPE)+
                     I(cos(ASPECT-0.7854)*SLOPE)+SLOPE+
                     I(log(DIA_C))+I(BAL/100)+CR_weib+
                     I(DIA_C^2)+PCCF+I(CCF/100)+
                     ppt_Jun+tmax_FebJul+
                     (1+DIA_C|TRE_CN)+(1|Year),
                   data = glmm_df_z)
fvs_clim_5 <- lmer(log(dds)~SICOND+I(sin(ASPECT-0.7854)*SLOPE)+
                     I(cos(ASPECT-0.7854)*SLOPE)+SLOPE+
                     I(log(DIA_C))+I(BAL/100)+CR_weib+
                     I(DIA_C^2)+PCCF+I(CCF/100)+
                     ppt_pJunAug+tmax_FebJul+
                     (1+DIA_C|TRE_CN)+(1|Year),
                   data = glmm_df_z)
fvs_clim_6 <- lmer(log(dds)~SICOND+I(sin(ASPECT-0.7854)*SLOPE)+
                     I(cos(ASPECT-0.7854)*SLOPE)+SLOPE+
                     I(log(DIA_C))+I(BAL/100)+CR_weib+
                     I(DIA_C^2)+PCCF+I(CCF/100)+
                     ppt_pAugOct+tmax_FebJul+
                     (1+DIA_C|TRE_CN)+(1|Year),
                   data = glmm_df_z)
fvs_clim_7 <- lmer(log(dds)~SICOND+I(sin(ASPECT-0.7854)*SLOPE)+
                     I(cos(ASPECT-0.7854)*SLOPE)+SLOPE+
                     I(log(DIA_C))+I(BAL/100)+CR_weib+
                     I(DIA_C^2)+PCCF+I(CCF/100)+
                     ppt_MayJul+tmax_FebJul+
                     (1+DIA_C|TRE_CN)+(1|Year),
                   data = glmm_df_z)
fvs_clim_8 <- lmer(log(dds)~SICOND+I(sin(ASPECT-0.7854)*SLOPE)+
                     I(cos(ASPECT-0.7854)*SLOPE)+SLOPE+
                     I(log(DIA_C))+I(BAL/100)+CR_weib+
                     I(DIA_C^2)+PCCF+I(CCF/100)+
                     ppt_FebJul+tmax_FebJul+
                     (1+DIA_C|TRE_CN)+(1|Year),
                   data = glmm_df_z)
fvs_clim_9 <- lmer(log(dds)~SICOND+I(sin(ASPECT-0.7854)*SLOPE)+
                     I(cos(ASPECT-0.7854)*SLOPE)+SLOPE+
                     I(log(DIA_C))+I(BAL/100)+CR_weib+
                     I(DIA_C^2)+PCCF+I(CCF/100)+
                     ppt_pJunNov+tmax_FebJul+
                     (1+DIA_C|TRE_CN)+(1|Year),
                   data = glmm_df_z)
fvs_clim_10 <- lmer(log(dds)~SICOND+I(sin(ASPECT-0.7854)*SLOPE)+
                     I(cos(ASPECT-0.7854)*SLOPE)+SLOPE+
                     I(log(DIA_C))+I(BAL/100)+CR_weib+
                     I(DIA_C^2)+PCCF+I(CCF/100)+
                     wateryr+tmax_FebJul+
                     (1+DIA_C|TRE_CN)+(1|Year),
                   data = glmm_df_z)
AIC(fvs_clim_1,fvs_clim_2,fvs_clim_3,fvs_clim_4,fvs_clim_5,
    fvs_clim_6,fvs_clim_7,fvs_clim_8,fvs_clim_9,fvs_clim_10)
#          df      AIC
#fvs_clim_1  18 9361.760
#fvs_clim_2  18 9359.135
#fvs_clim_3  18 9362.054
#fvs_clim_4  18 9338.122
#fvs_clim_5  18 9327.618**pJunAug
#fvs_clim_6  18 9360.595
#fvs_clim_7  18 9345.073
#fvs_clim_8  18 9359.454
#fvs_clim_9  18 9352.900
#fvs_clim_10 18 9354.475

#average temp
#1 month: Feb tmin, Jul tmax
#3 month: tmax_Jun-Aug, tmax_pJul-pSep, tmin_Jan-Mar
#6 month + : Jun, Jul
fvs_clim_1 <- lmer(log(dds)~SICOND+I(sin(ASPECT-0.7854)*SLOPE)+
                     I(cos(ASPECT-0.7854)*SLOPE)+SLOPE+
                     I(log(DIA_C))+I(BAL/100)+CR_weib+
                     I(DIA_C^2)+PCCF+I(CCF/100)+
                     ppt_pJunAug+tmax_Jul+
                     (1+DIA_C|TRE_CN)+(1|Year),
                   data = glmm_df_z)
fvs_clim_2 <- lmer(log(dds)~SICOND+I(sin(ASPECT-0.7854)*SLOPE)+
                     I(cos(ASPECT-0.7854)*SLOPE)+SLOPE+
                     I(log(DIA_C))+I(BAL/100)+CR_weib+
                     I(DIA_C^2)+PCCF+I(CCF/100)+
                     ppt_pJunAug+tmin_Feb+
                     (1+DIA_C|TRE_CN)+(1|Year),
                   data = glmm_df_z)
fvs_clim_3 <- lmer(log(dds)~SICOND+I(sin(ASPECT-0.7854)*SLOPE)+
                      I(cos(ASPECT-0.7854)*SLOPE)+SLOPE+
                      I(log(DIA_C))+I(BAL/100)+CR_weib+
                      I(DIA_C^2)+PCCF+I(CCF/100)+
                      ppt_pJunAug+tmin_JanMar+
                      (1+DIA_C|TRE_CN)+(1|Year),
                    data = glmm_df_z)
fvs_clim_4 <- lmer(log(dds)~SICOND+I(sin(ASPECT-0.7854)*SLOPE)+
                     I(cos(ASPECT-0.7854)*SLOPE)+SLOPE+
                     I(log(DIA_C))+I(BAL/100)+CR_weib+
                     I(DIA_C^2)+PCCF+I(CCF/100)+
                     ppt_pJunAug+tmax_pJulSep+
                     (1+DIA_C|TRE_CN)+(1|Year),
                   data = glmm_df_z)
fvs_clim_5 <- lmer(log(dds)~SICOND+I(sin(ASPECT-0.7854)*SLOPE)+
                      I(cos(ASPECT-0.7854)*SLOPE)+SLOPE+
                      I(log(DIA_C))+I(BAL/100)+CR_weib+
                      I(DIA_C^2)+PCCF+I(CCF/100)+
                      ppt_pJunAug+tmax_JunAug+
                      (1+DIA_C|TRE_CN)+(1|Year),
                    data = glmm_df_z)
fvs_clim_6 <- lmer(log(dds)~SICOND+I(sin(ASPECT-0.7854)*SLOPE)+
                     I(cos(ASPECT-0.7854)*SLOPE)+SLOPE+
                     I(log(DIA_C))+I(BAL/100)+CR_weib+
                     I(DIA_C^2)+PCCF+I(CCF/100)+
                     ppt_pJunAug+tmax_FebJul+
                     (1+DIA_C|TRE_CN)+(1|Year),
                   data = glmm_df_z)
fvs_clim_7 <- lmer(log(dds)~SICOND+I(sin(ASPECT-0.7854)*SLOPE)+
                     I(cos(ASPECT-0.7854)*SLOPE)+SLOPE+
                     I(log(DIA_C))+I(BAL/100)+CR_weib+
                     I(DIA_C^2)+PCCF+I(CCF/100)+
                     ppt_pJunAug+tmin_JanJun+
                     (1+DIA_C|TRE_CN)+(1|Year),
                   data = glmm_df_z)
fvs_clim_8 <- lmer(log(dds)~SICOND+I(sin(ASPECT-0.7854)*SLOPE)+
                     I(cos(ASPECT-0.7854)*SLOPE)+SLOPE+
                     I(log(DIA_C))+I(BAL/100)+CR_weib+
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

summary(fvs_clim_6)
#remove based on significance
#PCCF
fvs_clim_red <- lmer(log(dds)~SICOND+I(sin(ASPECT-0.7854)*SLOPE)+
                       I(cos(ASPECT-0.7854)*SLOPE)+SLOPE+
                       I(log(DIA_C))+I(BAL/100)+CR_weib+
                       I(DIA_C^2)+I(CCF/100)+
                       ppt_pJunAug+tmax_FebJul+
                       (1+DIA_C|TRE_CN)+(1|Year),
                     data = glmm_df_z)
AIC(fvs_clim_red)#9306
summary(fvs_clim_red)

#(sin(ASPECT-0.7854)*SLOPE)
fvs_clim_red <- lmer(log(dds)~SICOND+
                       I(cos(ASPECT-0.7854)*SLOPE)+SLOPE+
                       I(log(DIA_C))+I(BAL/100)+CR_weib+
                       I(DIA_C^2)+I(CCF/100)+
                       ppt_pJunAug+tmax_FebJul+
                       (1+DIA_C|TRE_CN)+(1|Year),
                     data = glmm_df_z)
AIC(fvs_clim_red)#9295
summary(fvs_clim_red)

#dbh^2
fvs_clim_red <- lmer(log(dds)~SICOND+
                       I(cos(ASPECT-0.7854)*SLOPE)+SLOPE+
                       I(log(DIA_C))+I(BAL/100)+CR_weib+
                       I(CCF/100)+
                       ppt_pJunAug+tmax_FebJul+
                       (1+DIA_C|TRE_CN)+(1|Year),
                     data = glmm_df_z)
AIC(fvs_clim_red)#9281
summary(fvs_clim_red)
#doesn't improve AIC to remove anything else

#what about interactions?
fvs_clim_int <- lmer(log(dds)~SICOND+
                       I(cos(ASPECT-0.7854)*SLOPE)+SLOPE+
                       I(log(DIA_C))+I(BAL/100)+CR_weib+
                       I(CCF/100)+
                       ppt_pJunAug*tmax_FebJul+
                       (1+DIA_C|TRE_CN)+(1|Year),
                     data = glmm_df_z)
AIC(fvs_clim_int)#9295
summary(fvs_clim_int)
#interactions significant

#glmm
#reminder- can't compare AIC values between a transformed and nontransformed model

#weird looking coef
clim_wy_red <- lmer(log(dds)~
                      #tree variables
                      z.DIA_C+I(z.DIA_C^2)+ #remove log due to standardization
                      z.CR_weib+I(z.CR_weib^2)+
                      #climate
                      z.wateryr+z.tmax_FebJul+
                      #competition/density
                      z.BAL+z.PCCF+z.CCF+ #remove /100 due to standardization
                      #site variables
                      z.SICOND+I(z.SLOPE^2)+
                      I(sin(z.ASPECT-0.7854)*z.SLOPE)+
                      I(cos(z.ASPECT-0.7854)*z.SLOPE)+
                      #random effects
                      (1+z.DIA_C|TRE_CN)+(1|Year),
                    data = glmm_df_z)
#compare CR
CR_wy_red <- lmer(log(dds)~
                      #tree variables
                      z.DIA_C+I(z.DIA_C^2)+ #remove log due to standardization
                      z.CR+I(z.CR^2)+
                      #climate
                      z.wateryr+z.tmax_FebJul+
                      #competition/density
                      z.BAL+z.PCCF+z.CCF+ #remove /100 due to standardization
                      #site variables
                      z.SICOND+I(z.SLOPE^2)+
                      I(sin(z.ASPECT-0.7854)*z.SLOPE)+
                      I(cos(z.ASPECT-0.7854)*z.SLOPE)+
                      #random effects
                      (1+z.DIA_C|TRE_CN)+(1|Year),
                    data = glmm_df_z)
#compare z.aspect
clim_z_red <- lmer(log(dds)~
                      #tree variables
                      z.DIA_C+I(z.DIA_C^2)+ #remove log due to standardization
                      z.CR+I(z.CR^2)+
                      #climate
                      z.wateryr+z.tmax_FebJul+
                      #competition/density
                      z.BAL+z.PCCF+z.CCF+ #remove /100 due to standardization
                      #site variables
                      z.SICOND+I(z.SLOPE^2)+
                      I(sin(ASPECT-0.7854)*SLOPE)+
                      I(cos(ASPECT-0.7854)*SLOPE)+
                      #random effects
                      (1+z.DIA_C|TRE_CN)+(1|Year),
                    data = glmm_df_z)


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
lmm1_df <- lmer(log(dds)~
                      #tree variables
                      z.DIA_C+I(z.DIA_C^2)+ #remove log due to standardization
                      z.CR_weib+I(z.CR_weib^2)+
                      #climate
                      z.wateryr+z.tmax_FebJul+
                      #competition/density
                      z.BAL+z.PCCF+z.CCF+ #remove /100 due to standardization
                      #site variables
                      z.SICOND+z.SLOPE+I(z.SLOPE^2)+
                      z.sin+z.cos+
                      #random effects
                      (1+z.DIA_C|TRE_CN)+(1|Year),
                    data = glmm_df_z)
vif(lmm1_df)
#CR and BAL are collinear
summary(lmm1_df)
AIC(lmm1_df)
#9641.618

#get rid of quadratic
lmm1_red2_df <- lmer(log(dds)~
                  #tree variables
                  z.DIA_C+I(z.DIA_C^2)+ #remove log due to standardization
                  z.CR_weib+
                  #climate
                  z.wateryr+z.tmax_FebJul+
                  #competition/density
                  z.BAL+z.PCCF+z.CCF+ #remove /100 due to standardization
                  #site variables
                  z.SICOND+z.SLOPE+
                  z.sin+z.cos+
                  #random effects
                  (1+z.DIA_C|TRE_CN)+(1|Year),
                data = glmm_df_z)
AIC(lmm1_red2_df)
#9631.994

#reduce significance
#PCCF,sin
lmm1_reds_df <- lmer(log(dds)~
                  #tree variables
                  z.DIA_C+I(z.DIA_C^2)+ #remove log due to standardization
                  z.CR_weib+I(z.CR_weib^2)+
                  #climate
                  z.wateryr+z.tmax_FebJul+
                  #competition/density
                  z.BAL+z.CCF+ #remove /100 due to standardization
                  #site variables
                  z.SICOND+z.SLOPE+I(z.SLOPE^2)+z.cos+
                  #random effects
                  (1+z.DIA_C|TRE_CN)+(1|Year),
                data = glmm_df_z)
#AIC - 9632.905

#both
#quadratic
#significance
lmm1_reds2_df <- lmer(log(dds)~
                       #tree variables
                       z.DIA_C+I(z.DIA_C^2)+ #remove log due to standardization
                       z.CR_weib+
                       #climate
                       z.wateryr+z.tmax_FebJul+
                       #competition/density
                       z.BAL+z.CCF+ #remove /100 due to standardization
                       #site variables
                       z.SICOND+z.SLOPE+z.cos+
                       #random effects
                       (1+z.DIA_C|TRE_CN)+(1|Year),
                      data = glmm_df_z)
AIC(lmm1_reds2_df)
#9623.195

#what happens if I take out cos?
lmm2_reds2_df <- lmer(log(dds)~
                        #tree variables
                        z.DIA_C+I(z.DIA_C^2)+ #remove log due to standardization
                        z.CR_weib+
                        #climate
                        z.wateryr+z.tmax_FebJul+
                        #competition/density
                        z.BAL+z.CCF+ #remove /100 due to standardization
                        #site variables
                        z.SICOND+z.SLOPE+
                        #random effects
                        (1+z.DIA_C|TRE_CN)+(1|Year),
                      data = glmm_df_z)
#AIC 9621.735

#try different competition factors
#with CR
lmm2_cr_df <- lmer(log(dds)~
                        #tree variables
                        z.DIA_C+I(z.DIA_C^2)+ #remove log due to standardization
                        z.CR_weib+
                        #climate
                        z.wateryr+z.tmax_FebJul+
                        #competition/density
                        #z.BAL+z.CCF+ #remove /100 due to standardization
                        #site variables
                        z.SICOND+z.SLOPE+
                        #random effects
                        (1+z.DIA_C|TRE_CN)+(1|Year),
                      data = glmm_df_z)
AIC(lmm2_cr_df)
#9633.186

#bal
lmm2_bal_df <- lmer(log(dds)~
                     #tree variables
                     z.DIA_C+I(z.DIA_C^2)+ #remove log due to standardization
                     #z.CR_weib+
                     #climate
                     z.wateryr+z.tmax_FebJul+
                     #competition/density
                     z.BAL+ #remove /100 due to standardization
                     #site variables
                     z.SICOND+z.SLOPE+
                     #random effects
                     (1+z.DIA_C|TRE_CN)+(1|Year),
                   data = glmm_df_z)
#warning - fail to converge
summary(lmm2_bal_df)
AIC(lmm2_bal_df)
#9639.324

#CCF
lmm2_ccf_df <- lmer(log(dds)~
                     #tree variables
                     z.DIA_C+I(z.DIA_C^2)+ #remove log due to standardization
                     #z.CR_weib+
                     #climate
                     z.wateryr+z.tmax_FebJul+
                     #competition/density
                     z.CCF+ #remove /100 due to standardization
                     #site variables
                     z.SICOND+z.SLOPE+
                     #random effects
                     (1+z.DIA_C|TRE_CN)+(1|Year),
                   data = glmm_df_z)
summary(lmm2_ccf_df)
AIC(lmm2_ccf_df)
#9625.844

#two
lmm2_crf_df <- lmer(log(dds)~
                      #tree variables
                      z.DIA_C+I(z.DIA_C^2)+ #remove log due to standardization
                      z.CR_weib+
                      #climate
                      z.wateryr+z.tmax_FebJul+
                      #competition/density
                      z.CCF+ #remove /100 due to standardization
                      #site variables
                      z.SICOND+z.SLOPE+
                      #random effects
                      (1+z.DIA_C|TRE_CN)+(1|Year),
                    data = glmm_df_z)
summary(lmm2_crf_df)
AIC(lmm2_crf_df)
#9626.656

lmm2_cfbl_df <- lmer(log(dds)~
                      #tree variables
                      z.DIA_C+I(z.DIA_C^2)+ #remove log due to standardization
                      #z.CR_weib+
                      #climate
                      z.wateryr+z.tmax_FebJul+
                      #competition/density
                      z.CCF+z.BAL+ #remove /100 due to standardization
                      #site variables
                      z.SICOND+z.SLOPE+
                      #random effects
                      (1+z.DIA_C|TRE_CN)+(1|Year),
                    data = glmm_df_z)
summary(lmm2_cfbl_df)
AIC(lmm2_cfbl_df)
#9631.762

#visualize
lmm_df <-  tidy(lmm2_reds2_df) %>% 
  filter(effect == "fixed") %>%
  select(term,estimate,std.error) %>%
  mutate(model = "LMM")%>%
  relabel_predictors(c(z.DIA_C = "DBH",                       # relabel predictors
                       "I(z.DIA_C^2)" = "DBH^2",
                       z.CR_weib = "Crown Ratio",
                       z.wateryr = "Precipitation",
                       z.tmax_FebJul = "Temperature",
                       z.BAL = "BAL",
                       z.CCF = "CCF", 
                       z.SICOND = "Site Index",
                       z.SLOPE = "Slope",
                       z.cos = "cos(Aspect-0.7854)*Slope"))

lmm_cr_df <-  tidy(lmm2_cr_df) %>% 
  filter(effect == "fixed") %>%
  select(term,estimate,std.error) %>%
  mutate(model = "LMM_cr")%>%
  relabel_predictors(c(z.DIA_C = "DBH",                       # relabel predictors
                       "I(z.DIA_C^2)" = "DBH^2",
                       z.CR_weib = "Crown Ratio",
                       z.wateryr = "Precipitation",
                       z.tmax_FebJul = "Temperature",
                       z.BAL = "BAL",
                       z.CCF = "CCF", 
                       z.SICOND = "Site Index",
                       z.SLOPE = "Slope",
                       z.cos = "cos(Aspect-0.7854)*Slope"))

lmm_bal_df <-  tidy(lmm2_bal_df) %>% 
  filter(effect == "fixed") %>%
  select(term,estimate,std.error) %>%
  mutate(model = "LMM_bal")%>%
  relabel_predictors(c(z.DIA_C = "DBH",                       # relabel predictors
                       "I(z.DIA_C^2)" = "DBH^2",
                       z.CR = "Crown Ratio",
                       z.wateryr = "Precipitation",
                       z.tmax_FebJul = "Temperature",
                       z.BAL = "BAL",
                       z.CCF = "CCF", 
                       z.SICOND = "Site Index",
                       z.SLOPE = "Slope",
                       z.cos = "cos(Aspect-0.7854)*Slope"))

lmm_ccf_df <-  tidy(lmm2_ccf_df) %>% 
  filter(effect == "fixed") %>%
  select(term,estimate,std.error) %>%
  mutate(model = "LMM_ccf")%>%
  relabel_predictors(c(z.DIA_C = "DBH",                       # relabel predictors
                       "I(z.DIA_C^2)" = "DBH^2",
                       z.CR = "Crown Ratio",
                       z.wateryr = "Precipitation",
                       z.tmax_FebJul = "Temperature",
                       z.BAL = "BAL",
                       z.CCF = "CCF", 
                       z.SICOND = "Site Index",
                       z.SLOPE = "Slope",
                       z.cos = "cos(Aspect-0.7854)*Slope"))

lmm_crf_df <-  tidy(lmm2_crf_df) %>% 
  filter(effect == "fixed") %>%
  select(term,estimate,std.error) %>%
  mutate(model = "LMM_crf")%>%
  relabel_predictors(c(z.DIA_C = "DBH",                       # relabel predictors
                       "I(z.DIA_C^2)" = "DBH^2",
                       z.CR_weib = "Crown Ratio",
                       z.wateryr = "Precipitation",
                       z.tmax_FebJul = "Temperature",
                       z.BAL = "BAL",
                       z.CCF = "CCF", 
                       z.SICOND = "Site Index",
                       z.SLOPE = "Slope",
                       z.cos = "cos(Aspect-0.7854)*Slope"))

models_check4 <- full_join(lmm_df,lmm_bal_df) %>%
  full_join(.,lmm_cr_df) %>%
  full_join(.,lmm_ccf_df) %>%
  full_join(.,lmm_crf_df)

dwplot(models_check4, 
       vline = geom_vline(xintercept = 0, colour = "grey60", linetype = 2), # plot line at zero _behind_ coefs
       dot_args = list(aes(shape = model)),
       whisker_args = list(aes(linetype = model))) +
  theme_bw() + xlab("Coefficient Estimate") + ylab("") +
  ggtitle("Growth for Douglas Fir") +
  theme(plot.title = element_text(face="bold"),
        legend.position = "right",
        legend.background = element_rect(colour="grey80"),
        legend.title.align = 0.5)
