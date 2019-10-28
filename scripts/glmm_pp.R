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
  dplyr::select(PLT_CN,TRE_CN, RW, dds, Year, DIA_C, 
         SICOND, ASPECT, SLOPE, BAL, CR, CR_weib, PCCF, CCF,
         ppt_pDec, ppt_Jun, ppt_Jul, ppt_pOct,
         ppt_pAugOct, ppt_pOctDec, ppt_pNovJan, ppt_MayJul,
         ppt_pAugJan, wateryr,
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

save(glmm_data_pp, file = "./data/formatted/glmm_data_pp")

#Exploration

#Missing data
sum(is.na(glmm_data_pp)) #614
summary(glmm_data_pp)
#ASPECT - 579
#ppt_pDec - 5
#ppt_pOct - 5
#ppt_pAugOct - 5
#ppt_pOctDec - 5
#ppt_pNovJan - 5
#ppt_pAugJan - 5
#wateryr - 5
miss_asp_pp <- unique(glmm_data_pp$TRE_CN[is.na(glmm_data_pp$ASPECT)]) #19
# 2.858391e+12 2.858393e+12 2.873697e+12 2.875406e+12 2.876414e+12 2.876415e+12 2.876519e+12 2.879914e+12
# 2.880191e+12 2.880197e+12 2.880321e+12 2.880322e+12 2.907647e+12 2.907648e+12 2.907875e+12 2.908047e+12
# 2.930116e+12 2.907876e+12 2.875410e+12
asp_check_pp <- data_all_pp %>%
  filter(TRE_CN %in% miss_asp_pp)
asp_check_pp <- data_all_pp %>%
  filter(SLOPE <= 5)
unique(asp_check_pp$ASPECT) #NA
length(data_all_pp$ASPECT[data_all_pp$ASPECT == 0]) #1079;NAs?
glmm_data_pp$ASPECT %>% replace_na(0)

miss_clim_pp <- unique(glmm_data_pp$TRE_CN[is.na(glmm_data_pp$ppt_pDec)])
# 2.858378e+12 2.875469e+12 2.930116e+12 2.907876e+12 2.875410e+12
unique(glmm_data_pp$Year[is.na(glmm_data_pp$ppt_pDec)])
# 1962 1965 1993
clim_check_pp <- glmm_data_pp %>%
  dplyr::select(TRE_CN,Year,ASPECT,ppt_pDec,ppt_pOct,
                ppt_pAugOct,ppt_pOctDec,ppt_pNovJan,ppt_pAugJan,wateryr) %>%
  filter(TRE_CN %in% miss_clim_pp)
#missing climate data is earliest year of growth
#has to do with the way climate and increment data were joined
#to fix must go add another year to increment data and join?
#probably will just delete missing year.

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

which(glmm_data_pp$RW == 0) #0
#no missing rings
which(is.na(glmm_data_pp$RW)) #0

#growth trends across time
library(ggplot2)
#library(bit64)
#as.integer64(.Machine$integer.max) + 1L
ggplot(data = glmm_data_pp, aes(x = Year, y = RW)) +
  geom_line(colour = glmm_data_pp$TRE_CN, group = glmm_data_pp$TRE_CN) +
  xlab("Year of Growth") + ylab("Increment (in)")
#error


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

#relationship of response with crown ratio (CR,CR_weib)
ggplot(data = glmm_data_pp, aes(x = CR, y = log(dds))) +
  geom_point(alpha=0.1) + geom_smooth(method = "lm")
#variance increases (not bad); postive relationship

ggplot(data = glmm_data_pp, aes(x = CR_weib, y = log(dds))) +
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
glmm_data_pp <- as.data.frame(glmm_data_pp)
glmm_pp_z <- stdize(glmm_data_pp,append=TRUE)
save(glmm_pp_z, file = "./data/formatted/glmm_pp_z")


#ccf insignificant in current lm
old_fvs <- lm(log(dds)~SICOND+I(sin(ASPECT-0.7854)*SLOPE)+
                I(cos(ASPECT-0.7854)*SLOPE)+SLOPE+I(SLOPE^2)+
                I(log(DIA_C))+I(BAL/100)+CR_weib+I(CR_weib^2)+
                I(DIA_C^2)+PCCF+I(CCF/100),
              data = glmm_pp_z)
summary(old_fvs)
#(cos(ASPECT - 0.7854) * SLOPE), log(dia), ccf insignificant

#log likelihood for random effects
fvs_tre_int <- lmer(log(dds)~SICOND+I(sin(ASPECT-0.7854)*SLOPE)+
                   I(cos(ASPECT-0.7854)*SLOPE)+SLOPE+I(SLOPE^2)+
                   I(log(DIA_C))+I(BAL/100)+CR_weib+I(CR_weib^2)+
                   I(DIA_C^2)+PCCF+I(CCF/100)+(1|TRE_CN),
                 data = glmm_pp_z)
fvs_yr_int <- lmer(log(dds)~SICOND+I(sin(ASPECT-0.7854)*SLOPE)+
                      I(cos(ASPECT-0.7854)*SLOPE)+SLOPE+I(SLOPE^2)+
                      I(log(DIA_C))+I(BAL/100)+CR_weib+I(CR_weib^2)+
                      I(DIA_C^2)+PCCF+I(CCF/100)+(1|Year),
                    data = glmm_pp_z)
fvs_yrtre_int <- lmer(log(dds)~SICOND+I(sin(ASPECT-0.7854)*SLOPE)+
                      I(cos(ASPECT-0.7854)*SLOPE)+SLOPE+I(SLOPE^2)+
                      I(log(DIA_C))+I(BAL/100)+CR_weib+I(CR_weib^2)+
                      I(DIA_C^2)+PCCF+I(CCF/100)+(1|TRE_CN)+(1|Year),
                    data = glmm_pp_z)
fvs_tre_slint <- lmer(log(dds)~SICOND+I(sin(ASPECT-0.7854)*SLOPE)+
                      I(cos(ASPECT-0.7854)*SLOPE)+SLOPE+I(SLOPE^2)+
                      I(log(DIA_C))+I(BAL/100)+CR_weib+I(CR_weib^2)+
                      I(DIA_C^2)+PCCF+I(CCF/100)+(1+DIA_C|TRE_CN),
                    data = glmm_pp_z)
fvs_yrtre_slint <- lmer(log(dds)~SICOND+I(sin(ASPECT-0.7854)*SLOPE)+
                      I(cos(ASPECT-0.7854)*SLOPE)+SLOPE+I(SLOPE^2)+
                      I(log(DIA_C))+I(BAL/100)+CR_weib+I(CR_weib^2)+
                      I(DIA_C^2)+PCCF+I(CCF/100)+(1+DIA_C|TRE_CN)+(1|Year),
                    data = glmm_pp_z)
anova(fvs_tre_int,fvs_tre_slint,fvs_yr_int,fvs_yrtre_int,fvs_yrtre_slint)
#                Df    AIC    BIC  logLik deviance   Chisq Chi Df Pr(>Chisq)    
#fvs_tre_int     15 4462.2 4544.3 -2216.1   4432.2                              
#fvs_yr_int      15 5852.5 5934.6 -2911.3   5822.5    0.00      0          1    
#fvs_yrtre_int   16 3913.7 4001.2 -1940.8   3881.7 1940.86      1     <2e-16 ***
#fvs_tre_slint   17 4380.8 4473.9 -2173.4   4346.8    0.00      1          1    
#fvs_yrtre_slint 18 3716.5 3815.0 -1840.2   3680.5  666.36      1     <2e-16 ***
#tree and yr random intercept and random slope on DIA_C best
summary(fvs_yrtre_slint)


#add climate
fvs_clim_mod <- lmer(log(dds)~SICOND+I(sin(ASPECT-0.7854)*SLOPE)+
                       I(cos(ASPECT-0.7854)*SLOPE)+SLOPE+I(SLOPE^2)+
                       I(log(DIA_C))+I(BAL/100)+CR_weib+I(CR_weib^2)+
                       I(DIA_C^2)+PCCF+I(CCF/100)+wateryr+tmax_JunAug+
                       (1+DIA_C|TRE_CN)+(1|Year),
                     data = glmm_pp_z)
#center and scale
fvs_clim_mod <- glmer((dds)~z.SICOND+I(sin(z.ASPECT-0.7854)*z.SLOPE)+
                        I(cos(z.ASPECT-0.7854)*z.SLOPE)+z.SLOPE+I(z.SLOPE^2)+
                        I(log(DIA_C))+I(z.BAL/100)+z.CR_weib+I(z.CR_weib^2)+
                        I(z.DIA_C^2)+z.PCCF+I(z.CCF/100)+z.wateryr+z.tmax_JunAug+
                        (1+z.DIA_C|TRE_CN)+(1|Year),
                      family = Gamma(link = "log"),
                      data = glmm_pp_z)
#log(DIA) error; can't take log of negative number

#collinearity
library(car)
vif(fvs_clim_mod)
#SLOPE and CR_weib

fvs_clim_red <- lmer(log(dds)~SICOND+I(sin(ASPECT-0.7854)*SLOPE)+
                       I(cos(ASPECT-0.7854)*SLOPE)+SLOPE+
                       I(log(DIA_C))+I(BAL/100)+CR_weib+
                       I(DIA_C^2)+PCCF+I(CCF/100)+
                       wateryr+tmax_JunAug+
                       (1+DIA_C|TRE_CN)+(1|Year),
                     data = glmm_pp_z)
vif(fvs_clim_red)
#now 3 or below
summary(fvs_clim_red)
#signs make sense

#AIC to compare climate variables
#total ppt
#1 month: ,pDec, Jun, Jul, pOct
#3 month: pAug-pOct, pOct-pDec, pNov-Jan, May-Jul
#6 month + : Jan, wateryr

fvs_clim_1 <- lmer(log(dds)~SICOND+I(sin(ASPECT-0.7854)*SLOPE)+
                     I(cos(ASPECT-0.7854)*SLOPE)+SLOPE+
                     I(log(DIA_C))+I(BAL/100)+CR_weib+
                     I(DIA_C^2)+PCCF+I(CCF/100)+
                     ppt_Jul+tmax_JunAug+
                     (1+DIA_C|TRE_CN)+(1|Year),
                   data = glmm_pp_z)
fvs_clim_2 <- lmer(log(dds)~SICOND+I(sin(ASPECT-0.7854)*SLOPE)+
                     I(cos(ASPECT-0.7854)*SLOPE)+SLOPE+
                     I(log(DIA_C))+I(BAL/100)+CR_weib+
                     I(DIA_C^2)+PCCF+I(CCF/100)+
                     ppt_pOct+tmax_JunAug+
                     (1+DIA_C|TRE_CN)+(1|Year),
                   data = glmm_pp_z)
fvs_clim_3 <- lmer(log(dds)~SICOND+I(sin(ASPECT-0.7854)*SLOPE)+
                     I(cos(ASPECT-0.7854)*SLOPE)+SLOPE+
                     I(log(DIA_C))+I(BAL/100)+CR_weib+
                     I(DIA_C^2)+PCCF+I(CCF/100)+
                     ppt_pDec+tmax_JunAug+
                     (1+DIA_C|TRE_CN)+(1|Year),
                   data = glmm_pp_z)
fvs_clim_4 <- lmer(log(dds)~SICOND+I(sin(ASPECT-0.7854)*SLOPE)+
                     I(cos(ASPECT-0.7854)*SLOPE)+SLOPE+
                     I(log(DIA_C))+I(BAL/100)+CR_weib+
                     I(DIA_C^2)+PCCF+I(CCF/100)+
                     ppt_Jun+tmax_JunAug+
                     (1+DIA_C|TRE_CN)+(1|Year),
                   data = glmm_pp_z)
fvs_clim_5 <- lmer(log(dds)~SICOND+I(sin(ASPECT-0.7854)*SLOPE)+
                     I(cos(ASPECT-0.7854)*SLOPE)+SLOPE+
                     I(log(DIA_C))+I(BAL/100)+CR_weib+
                     I(DIA_C^2)+PCCF+I(CCF/100)+
                     ppt_pAugOct+tmax_JunAug+
                     (1+DIA_C|TRE_CN)+(1|Year),
                   data = glmm_pp_z)
fvs_clim_6 <- lmer(log(dds)~SICOND+I(sin(ASPECT-0.7854)*SLOPE)+
                     I(cos(ASPECT-0.7854)*SLOPE)+SLOPE+
                     I(log(DIA_C))+I(BAL/100)+CR_weib+
                     I(DIA_C^2)+PCCF+I(CCF/100)+
                     ppt_pOctDec+tmax_JunAug+
                     (1+DIA_C|TRE_CN)+(1|Year),
                   data = glmm_pp_z)
fvs_clim_7 <- lmer(log(dds)~SICOND+I(sin(ASPECT-0.7854)*SLOPE)+
                     I(cos(ASPECT-0.7854)*SLOPE)+SLOPE+
                     I(log(DIA_C))+I(BAL/100)+CR_weib+
                     I(DIA_C^2)+PCCF+I(CCF/100)+
                     ppt_MayJul+tmax_JunAug+
                     (1+DIA_C|TRE_CN)+(1|Year),
                   data = glmm_pp_z)
fvs_clim_8 <- lmer(log(dds)~SICOND+I(sin(ASPECT-0.7854)*SLOPE)+
                     I(cos(ASPECT-0.7854)*SLOPE)+SLOPE+
                     I(log(DIA_C))+I(BAL/100)+CR_weib+
                     I(DIA_C^2)+PCCF+I(CCF/100)+
                     ppt_pNovJan+tmax_JunAug+
                     (1+DIA_C|TRE_CN)+(1|Year),
                   data = glmm_pp_z)
fvs_clim_9 <- lmer(log(dds)~SICOND+I(sin(ASPECT-0.7854)*SLOPE)+
                     I(cos(ASPECT-0.7854)*SLOPE)+SLOPE+
                     I(log(DIA_C))+I(BAL/100)+CR_weib+
                     I(DIA_C^2)+PCCF+I(CCF/100)+
                     ppt_pAugJan+tmax_JunAug+
                     (1+DIA_C|TRE_CN)+(1|Year),
                   data = glmm_pp_z)
fvs_clim_10 <- lmer(log(dds)~SICOND+I(sin(ASPECT-0.7854)*SLOPE)+
                      I(cos(ASPECT-0.7854)*SLOPE)+SLOPE+
                      I(log(DIA_C))+I(BAL/100)+CR_weib+
                      I(DIA_C^2)+PCCF+I(CCF/100)+
                      wateryr+tmax_JunAug+
                      (1+DIA_C|TRE_CN)+(1|Year),
                    data = glmm_pp_z)
AIC(fvs_clim_1,fvs_clim_2,fvs_clim_3,fvs_clim_4,fvs_clim_5,
    fvs_clim_6,fvs_clim_7,fvs_clim_8,fvs_clim_9,fvs_clim_10)
#          df      AIC
#fvs_clim_1  18 3792.389
#fvs_clim_2  18 3787.475
#fvs_clim_3  18 3790.984
#fvs_clim_4  18 3793.753
#fvs_clim_5  18 3780.479**AugOct
#fvs_clim_6  18 3784.716
#fvs_clim_7  18 3790.709
#fvs_clim_8  18 3792.188
#fvs_clim_9  18 3784.560
#fvs_clim_10 18 3791.859

#average temp
#1 month: Jun tmax
#3 month: tmax_Jun-Aug
fvs_clim_1 <- lmer(log(dds)~SICOND+I(sin(ASPECT-0.7854)*SLOPE)+
                     I(cos(ASPECT-0.7854)*SLOPE)+SLOPE+
                     I(log(DIA_C))+I(BAL/100)+CR_weib+
                     I(DIA_C^2)+PCCF+I(CCF/100)+
                     ppt_pAugOct+tmax_Jun+
                     (1+DIA_C|TRE_CN)+(1|Year),
                   data = glmm_pp_z)
fvs_clim_2 <- lmer(log(dds)~SICOND+I(sin(ASPECT-0.7854)*SLOPE)+
                     I(cos(ASPECT-0.7854)*SLOPE)+SLOPE+
                     I(log(DIA_C))+I(BAL/100)+CR_weib+
                     I(DIA_C^2)+PCCF+I(CCF/100)+
                     ppt_pAugOct+tmax_JunAug+
                     (1+DIA_C|TRE_CN)+(1|Year),
                   data = glmm_pp_z)
AIC(fvs_clim_1,fvs_clim_2)
#          df      AIC
#fvs_clim_1 18 3781.912
#fvs_clim_2 18 3780.479**JunAug

summary(fvs_clim_2)
granef <- ranef(fvs_clim_2)
var <- VarCorr(fvs_clim_2)
#remove based on significance
#CR
fvs_clim_red <- lmer(log(dds)~SICOND+I(sin(ASPECT-0.7854)*SLOPE)+
                       I(cos(ASPECT-0.7854)*SLOPE)+SLOPE+
                       I(log(DIA_C))+I(BAL/100)+
                       I(DIA_C^2)+PCCF+I(CCF/100)+
                       ppt_pAugOct+tmax_JunAug+
                       (1+DIA_C|TRE_CN)+(1|Year),
                     data = glmm_pp_z)
AIC(fvs_clim_red)#3771
summary(fvs_clim_red)

#PCCF
fvs_clim_red <- lmer(log(dds)~SICOND+I(sin(ASPECT-0.7854)*SLOPE)+
                       I(cos(ASPECT-0.7854)*SLOPE)+SLOPE+
                       I(log(DIA_C))+I(BAL/100)+
                       I(DIA_C^2)+I(CCF/100)+
                       ppt_pAugOct+tmax_JunAug+
                       (1+DIA_C|TRE_CN)+(1|Year),
                     data = glmm_pp_z)
AIC(fvs_clim_red)#3757
summary(fvs_clim_red)

#(cos(ASPECT-0.7854)*SLOPE)
fvs_clim_red <- lmer(log(dds)~SICOND+I(sin(ASPECT-0.7854)*SLOPE)+
                       SLOPE+
                       I(log(DIA_C))+I(BAL/100)+
                       I(DIA_C^2)+I(CCF/100)+
                       ppt_pAugOct+tmax_JunAug+
                       (1+DIA_C|TRE_CN)+(1|Year),
                     data = glmm_pp_z)
AIC(fvs_clim_red)#3748
summary(fvs_clim_red)

#BAL
fvs_clim_red <- lmer(log(dds)~SICOND+I(sin(ASPECT-0.7854)*SLOPE)+
                       SLOPE+
                       I(log(DIA_C))+
                       I(DIA_C^2)+I(CCF/100)+
                       ppt_pAugOct+tmax_JunAug+
                       (1+DIA_C|TRE_CN)+(1|Year),
                     data = glmm_pp_z)
AIC(fvs_clim_red)#3745
summary(fvs_clim_red)

#slope
fvs_clim_red <- lmer(log(dds)~SICOND+I(sin(ASPECT-0.7854)*SLOPE)+
                       I(log(DIA_C))+
                       I(DIA_C^2)+I(CCF/100)+
                       ppt_pAugOct+tmax_JunAug+
                       (1+DIA_C|TRE_CN)+(1|Year),
                     data = glmm_pp_z)
AIC(fvs_clim_red)#3737
summary(fvs_clim_red)

#dbh^2
fvs_clim_red <- lmer(log(dds)~SICOND+I(sin(ASPECT-0.7854)*SLOPE)+
                       I(log(DIA_C))+
                       I(CCF/100)+
                       ppt_pAugOct+tmax_JunAug+
                       (1+DIA_C|TRE_CN)+(1|Year),
                     data = glmm_pp_z)
AIC(fvs_clim_red)#3727
summary(fvs_clim_red)
#doesn't improve AIC to remove anything else

#what about interactions?
fvs_clim_int <- lmer(log(dds)~SICOND+
                       I(cos(ASPECT-0.7854)*SLOPE)+SLOPE+
                       I(log(DIA_C))+
                       I(CCF/100)+
                       ppt_pAugOct*tmax_JunAug+
                       (1+DIA_C|TRE_CN)+(1|Year),
                     data = glmm_pp_z)
AIC(fvs_clim_int)#3753
summary(fvs_clim_int)
#interactions insignificant

#GLMM
#reminder- can't compare AIC values between a transformed and nontransformed model

meanvsd <- glmm_data_pp %>%
  group_by(TRE_CN)%>%
  summarise(mean = mean(RW),
            sd = sd(RW))
plot(meanvsd$mean,meanvsd$sd, title(main = "PP tree ring variability vs mean growth rate"))

glmm_g <- glmer(dds~z.SICOND+I(sin(z.ASPECT-0.7854)*z.SLOPE)+
                  I(cos(z.ASPECT-0.7854)*z.SLOPE)+z.SLOPE+
                  z.DIA_C+I(z.BAL/100)+z.CR_weib+
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
                    z.DIA_C+I(z.BAL/100)+z.CR_weib+
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
                   z.DIA_C+I(z.BAL/100)+z.CR_weib+
                   I(z.DIA_C^2)+z.PCCF+I(z.CCF/100)+
                   z.ppt_pAugOct+z.tmax_JunAug+
                   (1+z.DIA_C|TRE_CN)+(1|Year),
                 data = glmm_pp_z,
                 family = inverse.gaussian(link = "1/mu^2"),
                 glmerControl(optimizer = "bobyqa", 
                              optCtrl = list(maxfun = 100000)))
AIC(glmm_ig_mu)#-7194.234
summary(glmm_ig_mu)

#reduced
glmm_g_red <- glmer(dds~z.SICOND+I(sin(z.ASPECT-0.7854)*z.SLOPE)+
                       z.DIA_C+I(z.CCF/100)+
                       z.ppt_pAugOct+z.tmax_JunAug+
                       (1+z.DIA_C|TRE_CN)+(1|Year),
                     data = glmm_pp_z,
                     family = Gamma(link = "log"),
                     glmerControl(optimizer = "bobyqa", 
                                  optCtrl = list(maxfun = 100000)))
AIC(glmm_g_red)#-12016
summary(glmm_g_red)

glmm_ig_red <- glmer(dds~z.SICOND+I(sin(z.ASPECT-0.7854)*z.SLOPE)+
                       z.DIA_C+I(z.CCF/100)+
                       z.ppt_pAugOct+z.tmax_JunAug+
                       (1+z.DIA_C|TRE_CN)+(1|Year),
                     data = glmm_pp_z,
                     family = inverse.gaussian(link = "log"),
                     glmerControl(optimizer = "bobyqa", 
                                  optCtrl = list(maxfun = 100000)))
AIC(glmm_ig_red)#-5957.705
summary(glmm_ig_red)


#assess
presid_glm <- residuals(fvs_glmm,type="pearson",scaled=TRUE)
qqnorm(presid_glm,main="QQ plot of Gamma residuals")
qqline(presid_glm)

library(gplots)
pred_glm <- predict(fvs_glmm, type="response")
plotLowess(presid_glm~pred_glm, ylab="Residuals",xlab="Predicted", main="Gamma")

age <- glmm_pp_z[,] #z.dbh
plotLowess(presid_glm~age,data = glmm.data.trun,ylab="Residuals",main="Gamma")

#determining interaction between latitude and climate
#southern Utah seems to be dominated by monsoon rains
#northern Utah seems to be dominated by winter rains

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
         SICOND, ASPECT, SLOPE, BAL, CR, CR_weib, PCCF, CCF,
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
library(ggplot2)

plot_model(spat_wateryr, type = "pred", terms = c("wateryr", "LAT"))

spat_explore <- lmer(log(dds) ~ DIA_C + ppt_Jun + LAT + ppt_Jun:LAT + (1+DIA_C|TRE_CN) + (1|Year), data = spatial_data_df)
summary(spat_explore)


##Model Selection
