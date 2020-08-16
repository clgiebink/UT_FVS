#Linear mixed model for Ponderosa Pine
#Courtney Giebink
#clgiebink@gmail.com
#29 July 2019

#dataframe
#all data is provided in
load(file = './data/formatted/data_all_pp')

#create new dataframe with only variables needed for linear mixed model
#response: RW, dds
#covariates: SI, ASP, SL, DBH/DIA_C, BAL, CR, CCF, PCCF, climate
#random: TRE_CN, Year
#filter for last 30 years of growth
min(data_all_pp$MEASYEAR) #1992 -> 1962

glmm_data_pp <- data_all_pp %>%
  dplyr::select(PLT_CN,FVS_LOC_CD,TRE_CN, RW, dds, Year, DIA_C, 
         SICOND, tASPECT, SLOPE, BAL, SDI, CR, CR_fvs, PCCF, CCF, 
         cos, sin, solrad_an, solrad_JanApr, solrad_MayAug, solrad_SepDec,
         ppt_pDec, ppt_Jun, ppt_Jul, ppt_pOct,
         ppt_pAugOct, ppt_pOctDec, ppt_pNovJan, ppt_MayJul,
         ppt_pAugJan, wateryr, ppt_pAugJul, ppt_pJunSep,
         tmax_Jun, tmax_JunAug) %>%
  filter(Year >= 1962)

#climate
#total ppt
#1 month: ,pDec, Jun, Jul, pOct
#3 month: pAug-pOct, pOct-pDec, pNov-Jan, May-Jul
#6 month + : Jan, wateryr

#average temp
#1 month: Jun tmax
#3 month: tmax_Jun-Aug

save(glmm_data_pp, file = "./data/formatted/glmm_data_pp.Rdata")

#Exploration

#Missing data
sum(is.na(glmm_data_pp)) #756
summary(glmm_data_pp)
#ppt_pDec - 5
#ppt_pOct - 5
#ppt_pAugOct - 5
#ppt_pOctDec - 5
#ppt_pNovJan - 5
#ppt_pAugJan - 5
#wateryr - 5
which(is.na(glmm_pp_z$ppt_pDec))
glmm_pp_z <- glmm_pp_z %>%
  filter(!is.na(ppt_pDec))
#missing climate data is earliest year of growth
#has to do with the way climate and increment data were joined
#to fix must go add another year to increment data and join?
#probably will just delete missing year.

which(glmm_data_pp$RW == 0) #0
#no missing rings

#distribution of the response variable
hist(glmm_data_pp$RW,breaks = 50, main = "Histogram of RW (mm)", xlab = "Increment")
hist(glmm_data_pp$dds,breaks = 100, main = "Histogram of DDS (in)", xlab = "Increment") 

#what probability distribution best matches?
#
library(fitdistrplus)
library(goft)
response <- glmm_data_pp$dds
#plot the empirical density and the histogram to gain insight of the data
plotdist(response, histo = TRUE, demp = TRUE)
descdist(response, discrete=FALSE, boot=500)
fit_ex  <- fitdist(response, "exp")
fit_gm  <- fitdist(response, "gamma")
fit_ig <- ig_fit(response)

par(mfrow=c(2,2))
plot.legend <- c("exponential","gamma","invgauss")
denscomp(list(fit_ex, fit_gm), legendtext = plot.legend)
cdfcomp (list(fit_ex, fit_gm), legendtext = plot.legend)
qqcomp  (list(fit_ex, fit_gm), legendtext = plot.legend)
ppcomp  (list(fit_ex, fit_gm), legendtext = plot.legend)


#growth trends across time
library(ggplot2)

#understanding random effects: TRE_CN
growth_yr_plots_pp <- glmm_data_pp %>% 
  group_by(TRE_CN) %>%
  do(plots=ggplot(data=.) +
       aes(x=Year, y=RW) + geom_point() + ggtitle(unique(.$TRE_CN)))

for(i in nrow(growth_yr_plots_pp)) {
  growth_yr_plots_pp$plots[[i]]
}

#par(mfrow = c(2, 2)) doesn't work; why?
growth_yr_plots_pp$plots[[1]]
growth_yr_plots_pp$plots[[2]]
growth_yr_plots_pp$plots[[3]]
growth_yr_plots_pp$plots[[4]]
growth_yr_plots_pp$plots[[5]]
growth_yr_plots_pp$plots[[6]]
growth_yr_plots_pp$plots[[7]]
growth_yr_plots_pp$plots[[8]]
growth_yr_plots_pp$plots[[9]]
growth_yr_plots_pp$plots[[10]]
growth_yr_plots_pp$plots[[11]]
growth_yr_plots_pp$plots[[12]]
growth_yr_plots_pp$plots[[13]]

#understanding random effects: Year
growth_wateryr_plots_pp <- glmm_data_pp %>% 
  group_by(Year) %>%
  do(plots=ggplot(data=.) +
       aes(x=wateryr, y=RW) + geom_point() +
       geom_smooth(method = "lm") + ggtitle(unique(.$Year)))

#par(mfrow = c(2, 2)) doesn't work
growth_wateryr_plots_pp$plots[[1]]
growth_wateryr_plots_pp$plots[[2]]
growth_wateryr_plots_pp$plots[[3]]
growth_wateryr_plots_pp$plots[[4]]
growth_wateryr_plots_pp$plots[[5]]
growth_wateryr_plots_pp$plots[[6]]
growth_wateryr_plots_pp$plots[[7]]
growth_wateryr_plots_pp$plots[[8]]
growth_wateryr_plots_pp$plots[[9]]
growth_wateryr_plots_pp$plots[[10]]
growth_wateryr_plots_pp$plots[[11]]
growth_wateryr_plots_pp$plots[[12]]
growth_wateryr_plots_pp$plots[[13]]

growth_wateryr_plots_pp$plots[[23]]
growth_wateryr_plots_pp$plots[[24]]
growth_wateryr_plots_pp$plots[[25]]
growth_wateryr_plots_pp$plots[[26]]
growth_wateryr_plots_pp$plots[[27]]
growth_wateryr_plots_pp$plots[[28]]


#dbh as a proxy for age
#detrend
ggplot(data = glmm_data_pp, aes(x = DIA_C, y = RW)) +
  geom_point(alpha=0.1) + geom_smooth(method = "lm")
#small skew on top of line

ggplot(data = glmm_data_pp, aes(x = DIA_C, y = dds)) +
  geom_point(alpha=0.1) + geom_smooth(method = "lm")
#large skew on top of line

ggplot(data = glmm_data_pp, aes(x = DIA_C, y = log(dds))) +
  geom_point(alpha=0.1) + geom_smooth(method = "lm")

#log to reduce heteroscedasticity
ggplot(data = glmm_data_pp, aes(x = log(DIA_C), y = log(dds))) +
  geom_point(alpha=0.1) + geom_smooth(method = "lm")
#reduces a little
#negative relationship

#relationship of response with site index (SI)
ggplot(data = glmm_data_pp, aes(x = SICOND, y = log(dds))) +
  geom_point(alpha=0.1) + geom_smooth(method = "lm")
#constant variance; positive relationship

#relationship of response with slope (SLOPE)
ggplot(data = glmm_data_pp, aes(x = SLOPE, y = log(dds))) +
  geom_point(alpha=0.1) + geom_smooth(method = "lm")
#constant variance; negative relationship

#relationship of response with BAL
ggplot(data = glmm_data_pp, aes(x = BAL, y = log(dds))) +
  geom_point(alpha=0.1) + geom_smooth(method = "lm")
#non constant variance (decreases); negative relationship

#relationship of response with PCCF
ggplot(data = glmm_data_pp, aes(x = PCCF, y = log(dds))) +
  geom_point(alpha=0.1) + geom_smooth(method = "lm")
#constant variance; negative relationship

#relationship of response with CCF
ggplot(data = glmm_data_pp, aes(x = CCF, y = log(dds))) +
  geom_point(alpha=0.1) + geom_smooth(method = "lm")
#non constant variance (decreases); negative relationship

#relationship of response with crown ratio (CR,CR_fvs)
ggplot(data = glmm_data_pp, aes(x = CR, y = log(dds))) +
  geom_point(alpha=0.1) + geom_smooth(method = "lm")
#variance increases (not bad); postive relationship

ggplot(data = glmm_data_pp, aes(x = CR_fvs, y = log(dds))) +
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
glmm_pp_z <- stdize(as.data.frame(glmm_data_pp),append=TRUE)
#remove missing data
glmm_pp_z <- glmm_pp_z %>%
  filter(!is.na(z.wateryr))

#site index incorrect
load("./data/formatted/cal_si.Rdata")
#either reduce or calculate with height and age
glmm_pp_z <- glmm_pp_z %>%
  filter(!(TRE_CN %in% cal_si$TRE_CN))
length(unique(glmm_pp_z$TRE_CN))
#87 -> 81
save(glmm_pp_z, file = "./data/formatted/glmm_pp_z.Rdata")


#log likelihood for random effects
#collinearity
library(car)
#vif()
#3 or below

#AIC to compare climate variables
#total ppt
#1 month: ,pDec, Jun, Jul, pOct
#3 month: pAug-pOct, pOct-pDec, pNov-Jan, May-Jul
#6 month + : Jan, wateryr, pJunSep

fvs_clim_1 <- lmer(log(dds)~z.SICOND+z.sin+
                     z.cos+z.SLOPE+I(z.SLOPE^2)+
                     z.DIA_C+I(BAL/100)+CR_fvs+
                     I(z.DIA_C^2)+z.PCCF+z.CCF+
                     z.ppt_Jul+z.tmax_JunAug+
                     (1+z.DIA_C|TRE_CN)+(1|Year),
                   data = glmm_pp_z)
fvs_clim_2 <- lmer(log(dds)~z.SICOND+z.sin+
                     z.cos+z.SLOPE+I(z.SLOPE^2)+
                     z.DIA_C+I(BAL/100)+CR_fvs+
                     I(z.DIA_C^2)+z.PCCF+z.CCF+
                     z.ppt_pOct+z.tmax_JunAug+
                     (1+z.DIA_C|TRE_CN)+(1|Year),
                   data = glmm_pp_z)
fvs_clim_3 <- lmer(log(dds)~z.SICOND+z.sin+
                     z.cos+z.SLOPE+I(z.SLOPE^2)+
                     z.DIA_C+I(BAL/100)+CR_fvs+
                     I(z.DIA_C^2)+z.PCCF+z.CCF+
                     z.ppt_pDec+z.tmax_JunAug+
                     (1+z.DIA_C|TRE_CN)+(1|Year),
                   data = glmm_pp_z)
fvs_clim_4 <- lmer(log(dds)~z.SICOND+z.sin+
                     z.cos+z.SLOPE+I(z.SLOPE^2)+
                     z.DIA_C+I(BAL/100)+CR_fvs+
                     I(z.DIA_C^2)+z.PCCF+z.CCF+
                     z.ppt_Jun+z.tmax_JunAug+
                     (1+DIA_C|TRE_CN)+(1|Year),
                   data = glmm_pp_z)
fvs_clim_5 <- lmer(log(dds)~z.SICOND+z.sin+
                     z.cos+z.SLOPE+I(z.SLOPE^2)+
                     z.DIA_C+I(BAL/100)+CR_fvs+
                     I(z.DIA_C^2)+z.PCCF+z.CCF+
                     z.ppt_pAugOct+z.tmax_JunAug+
                     (1+z.DIA_C|TRE_CN)+(1|Year),
                   data = glmm_pp_z)
fvs_clim_6 <- lmer(log(dds)~z.SICOND+z.sin+
                     z.cos+z.SLOPE+I(z.SLOPE^2)+
                     z.DIA_C+I(BAL/100)+CR_fvs+
                     I(z.DIA_C^2)+z.PCCF+z.CCF+
                     z.ppt_pOctDec+z.tmax_JunAug+
                     (1+DIA_C|TRE_CN)+(1|Year),
                   data = glmm_pp_z)
fvs_clim_7 <- lmer(log(dds)~z.SICOND+z.sin+
                     z.cos+z.SLOPE+I(z.SLOPE^2)+
                     z.DIA_C+I(BAL/100)+CR_fvs+
                     I(z.DIA_C^2)+z.PCCF+z.CCF+
                     z.ppt_MayJul+z.tmax_JunAug+
                     (1+z.DIA_C|TRE_CN)+(1|Year),
                   data = glmm_pp_z)
fvs_clim_8 <- lmer(log(dds)~z.SICOND+z.sin+
                     z.cos+z.SLOPE+I(z.SLOPE^2)+
                     z.DIA_C+I(BAL/100)+CR_fvs+
                     I(z.DIA_C^2)+z.PCCF+z.CCF+
                     z.ppt_pNovJan+z.tmax_JunAug+
                     (1+z.DIA_C|TRE_CN)+(1|Year),
                   data = glmm_pp_z)
fvs_clim_9 <- lmer(log(dds)~z.SICOND+z.sin+
                     z.cos+z.SLOPE+I(z.SLOPE^2)+
                     z.DIA_C+I(BAL/100)+CR_fvs+
                     I(z.DIA_C^2)+z.PCCF+z.CCF+
                     z.ppt_pAugJan+z.tmax_JunAug+
                     (1+z.DIA_C|TRE_CN)+(1|Year),
                   data = glmm_pp_z)
fvs_clim_10 <- lmer(log(dds)~z.SICOND+z.sin+
                      z.cos+z.SLOPE+I(z.SLOPE^2)+
                      z.DIA_C+I(BAL/100)+CR_fvs+
                      I(z.DIA_C^2)+z.PCCF+z.CCF+
                      z.wateryr+z.tmax_JunAug+
                      (1+z.DIA_C|TRE_CN)+(1|Year),
                    data = glmm_pp_z)
fvs_clim_11 <- lmer(log(dds)~z.SICOND+z.sin+
                      z.cos+z.SLOPE+I(z.SLOPE^2)+
                      z.DIA_C+I(BAL/100)+CR_fvs+
                      I(z.DIA_C^2)+z.PCCF+z.CCF+
                      z.ppt_pJunSep+z.tmax_JunAug+
                      (1+z.DIA_C|TRE_CN)+(1|Year),
                    data = glmm_pp_z)
AIC(fvs_clim_1,fvs_clim_2,fvs_clim_3,fvs_clim_4,fvs_clim_5,
    fvs_clim_6,fvs_clim_7,fvs_clim_8,fvs_clim_9,fvs_clim_10,fvs_clim_11)

#average temp
#1 month: Jun tmax
#3 month: tmax_Jun-Aug
fvs_clim_1 <- lmer(log(dds)~SICOND+I(sin(ASPECT-0.7854)*SLOPE)+
                     I(cos(ASPECT-0.7854)*SLOPE)+SLOPE+
                     I(log(DIA_C))+I(BAL/100)+CR_fvs+
                     I(DIA_C^2)+PCCF+I(CCF/100)+
                     ppt_pAugOct+tmax_Jun+
                     (1+DIA_C|TRE_CN)+(1|Year),
                   data = glmm_pp_z)
fvs_clim_2 <- lmer(log(dds)~SICOND+I(sin(ASPECT-0.7854)*SLOPE)+
                     I(cos(ASPECT-0.7854)*SLOPE)+SLOPE+
                     I(log(DIA_C))+I(BAL/100)+CR_fvs+
                     I(DIA_C^2)+PCCF+I(CCF/100)+
                     ppt_pAugOct+tmax_JunAug+
                     (1+DIA_C|TRE_CN)+(1|Year),
                   data = glmm_pp_z)
AIC(fvs_clim_1,fvs_clim_2)

summary(fvs_clim_2)
granef <- ranef(fvs_clim_2)
var <- VarCorr(fvs_clim_2)
#remove based on significance

#GLMM
#reminder- can't compare AIC values between a transformed and nontransformed model

meanvsd <- glmm_data_pp %>%
  group_by(TRE_CN)%>%
  summarise(mean = mean(RW),
            sd = sd(RW))
plot(meanvsd$mean,meanvsd$sd, title(main = "PP tree ring variability vs mean growth rate"))

glmm_g <- glmer(dds~z.SICOND+I(sin(z.ASPECT-0.7854)*z.SLOPE)+
                  I(cos(z.ASPECT-0.7854)*z.SLOPE)+z.SLOPE+
                  z.DIA_C+I(z.BAL/100)+z.CR_fvs+
                  I(z.DIA_C^2)+z.PCCF+I(z.CCF/100)+
                  z.ppt_pAugOct+z.tmax_JunAug+
                  (1+z.DIA_C|TRE_CN)+(1|Year),
                data = glmm_pp_z,
                family = Gamma(link = "log"),
                glmerControl(optimizer = "bobyqa", 
                             optCtrl = list(maxfun = 100000)))
AIC(glmm_g)#-12018
summary(glmm_g)


glmm_ig <- glmer(dds~z.SICOND+I(sin(z.ASPECT-0.7854)*z.SLOPE)+
                    I(cos(z.ASPECT-0.7854)*z.SLOPE)+z.SLOPE+
                    z.DIA_C+I(z.BAL/100)+z.CR_fvs+
                    I(z.DIA_C^2)+z.PCCF+I(z.CCF/100)+
                    z.ppt_pAugOct+z.tmax_JunAug+
                    (1+z.DIA_C|TRE_CN)+(1|Year),
                  data = glmm_pp_z,
                  family = inverse.gaussian(link = "log"),
                  glmerControl(optimizer = "bobyqa", 
                               optCtrl = list(maxfun = 100000)))
AIC(glmm_ig)#-7194.234
summary(glmm_ig)

glmm_ig_mu <- glmer(dds~z.SICOND+I(sin(z.ASPECT-0.7854)*z.SLOPE)+
                   I(cos(z.ASPECT-0.7854)*z.SLOPE)+z.SLOPE+
                   z.DIA_C+I(z.BAL/100)+z.CR_fvs+
                   I(z.DIA_C^2)+z.PCCF+I(z.CCF/100)+
                   z.ppt_pAugOct+z.tmax_JunAug+
                   (1+z.DIA_C|TRE_CN)+(1|Year),
                 data = glmm_pp_z,
                 family = inverse.gaussian(link = "1/mu^2"),
                 glmerControl(optimizer = "bobyqa", 
                              optCtrl = list(maxfun = 100000)))
AIC(glmm_ig_mu)#-7194.234
summary(glmm_ig_mu)


resid_glmm_n <- residuals(glmm_n_red,type="pearson",scaled=TRUE)
qqnorm(resid_glmm_n,main="QQ plot of LogN residuals")
qqline(resid_glmm_n)

#nlme ----
#temporal correlation
library(nlme)
#random effects
lmm_ranef <- lme(log(dds)~
                 #tree variables
                 z.DIA_C+I(z.DIA_C^2)+ #remove log due to standardization
                 I(z.CR_fvs^2)+
                 #climate
                 z.ppt_pAugOct+z.tmax_JunAug+ #interaction not significant
                 #competition/density
                 z.CCF+ #remove /100 due to standardization
                 #site variables
                 z.SICOND+
                 I(sin(z.ASPECT-0.7854)*z.SLOPE)+
                 I(cos(z.ASPECT-0.7854)*z.SLOPE),
               data = glmm_pp_z,
               #random effects
               random = ~1+z.DIA_C|TRE_CN,#not easy to implement 2 random intercepts
               na.action=na.exclude)
#add autoregressive correlation structure
lmm_ar1 <- lme(log(dds)~
                 #tree variables
                 z.DIA_C+I(z.DIA_C^2)+ #remove log due to standardization
                 I(z.CR_fvs^2)+
                 #climate
                 z.ppt_pAugOct+z.tmax_JunAug+ #interaction not significant
                 #competition/density
                 z.CCF+ #remove /100 due to standardization
                 #site variables
                 z.SICOND+
                 I(sin(z.ASPECT-0.7854)*z.SLOPE)+
                 I(cos(z.ASPECT-0.7854)*z.SLOPE),
               data = glmm_pp_z,
               #random effects
               random = ~1+z.DIA_C|TRE_CN,
               correlation = corAR1(), #on the intercept
               na.action=na.exclude)
#add weights
lmm_wght <- lme(log(dds)~
                 #tree variables
                 z.DIA_C+I(z.DIA_C^2)+ #remove log due to standardization
                 I(z.CR_fvs^2)+
                 #climate
                 z.ppt_pAugOct+z.tmax_JunAug+ #interaction not significant
                 #competition/density
                 z.CCF+ #remove /100 due to standardization
                 #site variables
                 z.SICOND+
                 I(sin(z.ASPECT-0.7854)*z.SLOPE)+
                 I(cos(z.ASPECT-0.7854)*z.SLOPE),
               data = glmm_pp_z,
               #random effects
               random = ~1+z.DIA_C|TRE_CN,
               correlation = corAR1(),#on the intercept,
               weights = varExp(form = ~z.ppt_pAugOct),
               na.action=na.exclude)

nlm_di <- lme(DI~
                  #tree variables
                  z.DIA_C+I(z.DIA_C^2)+ #remove log due to standardization
                  I(z.CR_fvs^2)+
                  #climate
                  z.ppt_pAugOct+z.tmax_JunAug+ #interaction not significant
                  #competition/density
                  z.CCF+ #remove /100 due to standardization
                  #site variables
                  z.SICOND+
                  I(sin(z.ASPECT-0.7854)*z.SLOPE)+
                  I(cos(z.ASPECT-0.7854)*z.SLOPE),
                data = glmm_pp_z,
                #random effects
                random = ~1+z.DIA_C|TRE_CN,
                correlation = corAR1(),#on the intercept,
                weights = varExp(form = ~z.ppt_pAugOct),
                na.action=na.exclude)

#assess
resid_nlme <- residuals(lmm_ar1,type="pearson",scaled=TRUE)
qqnorm(resid_nlme,main="QQ plot of LMM_AR1 residuals")
qqline(resid_nlme)

library(gplots)
pred_glm <- predict(fvs_glmm, type="response")
plotLowess(presid_glm~pred_glm, ylab="Residuals",xlab="Predicted", main="Gamma")

age <- glmm_pp_z[,] #z.dbh
plotLowess(presid_glm~age,data = glmm.data.trun,ylab="Residuals",main="Gamma")

#determining interaction between latitude and climate
#southern Utah seems to be dominated by monsoon rains
#northern Utah seems to be dominated by winter rains

#Spatial ----
#first visualize data by plotting it on a map
library(maps)
m = map_data('state', region = 'Utah')

ggplot() + 
  geom_polygon( data=m, aes(x=long, y=lat,group=group),colour="black", fill="white" )+
  geom_point(data=data_all_pp,aes(x=LON,y=LAT),colour="red")+
  ggtitle("Distribution of Ponderosa pine")+
  xlab('Longitude')+
  ylab('Latitude')+
  coord_fixed()

#or
library(ggmap)


#test interaction
spatial_data_df <- data_all_es %>%
  select(TRE_CN, RW, dds, Year, DIA_C, LAT,
         SICOND, ASPECT, SLOPE, BAL, CR, CR_fvs, PCCF, CCF,
         ppt_pOct, ppt_pDec, ppt_Jun, ppt_Jul,
         ppt_pJunAug, ppt_pAugOct, ppt_MayJul,
         ppt_pJunNov, ppt_FebJul, wateryr,
         tmin_Feb, tmax_Jul,
         tmax_JunAug, tmax_pJulSep, tmin_JanMar,
         tmax_JanJun,tmin_JanJun,tmax_JunAug) %>%
  filter(Year >= 1958)

spat_wateryr <- lmer(log(dds) ~ DIA_C + wateryr + LAT + wateryr:LAT + (1+DIA_C|TRE_CN) + (1|Year), data = spatial_data_df)
summary(spat_wateryr)

library(sjPlot)
library(sjmisc)

plot_model(spat_wateryr, type = "pred", terms = c("wateryr", "LAT"))

spat_explore <- lmer(log(dds) ~ DIA_C + ppt_Jun + LAT + ppt_Jun:LAT + (1+DIA_C|TRE_CN) + (1|Year), data = spatial_data_df)
summary(spat_explore)
