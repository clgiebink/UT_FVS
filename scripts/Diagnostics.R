#analysis
meanvsd <- glmm_data_pp %>%
  group_by(TRE_CN)%>%
  summarise(mean = mean(RW),
            sd = sd(RW))
plot(meanvsd$mean,meanvsd$sd, title(main = "PP tree ring variability vs mean growth rate"))


#p-values
library(lmerTest)
#anova for fixed effects
#ranova for random effects
## F-tests of 'single term deletions' for all marginal terms:
drop1(fm)
# Use the Kenward-Roger method
if(requireNamespace("pbkrtest", quietly = TRUE))
  anova(m, ddf="Kenward-Roger")
anova(model, ddf="Kenward-Roger")
library(pbkrtest)

library(reshape2)
data_wide <- dcast(glmm.data.trun, tree ~ year, value.var="incr")
data_wide <- data_wide[,2:32]
matrix.w <- as.matrix(data_wide)
data.cov <- cov(matrix.w,use = 'pairwise')
head(data.cov)

#variance covariance matrix
data.cor <- cor(matrix.w,use = 'pairwise')

Z <- as.matrix(getME(lmer.intslp,"Z"))
Z1 <- Z[(Z[,1]==1),c(1,2)] #to get one tree
tZ1 <- t(Z1) #transpose
stdv <- VarCorr(lmer.intslp)
as.data.frame(print(stdv,comp=c("Variance")))

G <- matrix(c(0.0216500,0.001814682,0.009619527,0.001814682), nrow = 2, ncol = 2, byrow = TRUE)
sigma_sq1 <- 0.0117915
R1 <- diag(sigma_sq1,nrow = 30)
cov1 <- Z1 %*% G %*% tZ1 + R1
head(cov1) #random intercept and slope covariance structure
head(data.cov) #sample covariance

#residuals
par(mfrow=c(2,2))
#clim_1_red
resid_clim1<- residuals(clim_1_red,type="pearson",scaled=TRUE)
qqnorm(resid_clim1,main="QQ plot of LMM1")
qqline(resid_clim1)
hist(resid_clim1)

#clim_wy_red
resid_climwy <- residuals(clim_wy_red,type="pearson",scaled=TRUE)
qqnorm(resid_climwy,main="QQ plot of LMMwy")
qqline(resid_climwy)


#To test if my transformed residuals have a constant variance with a mean of zero, 
#I will plot transformed residuals vs predicted values.
library(gplots)
par(mfrow=c(1,2))
pred_lmm <- predict(clim_1_red, type="response")
plotLowess(resid_clim1~pred_lmm, ylab="Residuals",xlab="Predicted", main="LMM")
#same?
fit_lmm <- fitted(clim_1_red)
plotLowess(resid_clim1~fit_lmm, ylab="Residuals",xlab="Fitted", main="LMM")

plotLowess(resid_clim1~z.DIA_C,data = glmm_pp_z,ylab="Residuals",main="LMM")
plotLowess(resid_clim1~z.CR_weib,data = glmm_pp_z,ylab="Residuals",main="LMM")
plotLowess(resid_clim1~z.ppt_pAugOct,data = glmm_pp_z,ylab="Residuals",main="LMM") #heteroskedasticity
plotLowess(resid_clim1~z.tmax_JunAug,data = glmm_pp_z,ylab="Residuals",main="LMM")
plotLowess(resid_clim1~z.CCF,data = glmm_pp_z,ylab="Residuals",main="LMM")
plotLowess(resid_clim1~z.SICOND,data = glmm_pp_z,ylab="Residuals",main="LMM")
plotLowess(resid_clim1~z.SLOPE,data = glmm_pp_z,ylab="Residuals",main="LMM")

#wateyear
resid_climwy<- residuals(clim_wy_red,type="pearson",scaled=TRUE)
qqnorm(resid_climwy,main="QQ plot of LMMwy")
qqline(resid_climwy)
hist(resid_climwy)
pred_lmm <- predict(clim_wy_red, type="response")
plotLowess(resid_climwy~pred_lmm, ylab="Residuals",xlab="Predicted", main="LMM")

#effect size
plot(glmm_pp_z$DIA_C,pred_lmm)

plotLowess(resid_climwy~z.DIA_C,data = glmm_pp_z,ylab="Residuals",main="LMM")
plotLowess(resid_climwy~z.CR_weib,data = glmm_pp_z,ylab="Residuals",main="LMM")
plotLowess(resid_climwy~z.wateryr,data = glmm_pp_z,ylab="Residuals",main="LMM") #still heteroskedasticity
plotLowess(resid_climwy~z.tmax_JunAug,data = glmm_pp_z,ylab="Residuals",main="LMM")
plotLowess(resid_climwy~z.CCF,data = glmm_pp_z,ylab="Residuals",main="LMM")
plotLowess(resid_climwy~z.SICOND,data = glmm_pp_z,ylab="Residuals",main="LMM")
plotLowess(resid_climwy~z.SLOPE,data = glmm_pp_z,ylab="Residuals",main="LMM")

resid_glmm1 <- residuals(glmm_1_red,type="pearson",scaled=TRUE)
qqnorm(resid_glmm1,main="QQ plot of GLMM1")
qqline(resid_glmm1)
pred_glmm <- predict(glmm_1_red, type="response")
plotLowess(resid_glmm1~pred_glmm, ylab="Residuals",xlab="Predicted", main="GLMM")
plotLowess(resid_glmm1~z.DIA_C,data = glmm_pp_z,ylab="Residuals",main="GLMM")
plotLowess(resid_glmm1~z.CR_weib,data = glmm_pp_z,ylab="Residuals",main="GLMM")
plotLowess(resid_glmm1~z.ppt_pAugOct,data = glmm_pp_z,ylab="Residuals",main="GLMM") #still heteroskedasticity
plotLowess(resid_glmm1~z.tmax_JunAug,data = glmm_pp_z,ylab="Residuals",main="GLMM")
plotLowess(resid_glmm1~z.CCF,data = glmm_pp_z,ylab="Residuals",main="GLMM")
plotLowess(resid_glmm1~z.SICOND,data = glmm_pp_z,ylab="Residuals",main="GLMM")
plotLowess(resid_glmm1~z.SLOPE,data = glmm_pp_z,ylab="Residuals",main="GLMM")
#pearson not correct for GLMM
library(DHARMa)
res <- simulateResiduals(fittedModel = glmm_1_red)
plot(res)
res_tre <- recalculateResiduals(res, group = glmm_pp_z$TRE_CN)
plot(res_tre, quantreg = FALSE)
testTemporalAutocorrelation(res_tre, time = glmm_pp_z$Year)
testTemporalAutocorrelation(res_tre)
#grouping not working
testUniformity(res)
res_glmm1 <- res$scaledResiduals
hist(res_glmm1)

resid_glmm2 <- residuals(glmm_2_red,type="pearson",scaled=TRUE)
qqnorm(resid_glmm2,main="QQ plot of GLMM1")
qqline(resid_glmm2)
pred_glmm2 <- predict(glmm_2_red, type="response")
plotLowess(resid_glmm2~pred_glmm2, ylab="Residuals",xlab="Predicted", main="GLMM")
plotLowess(resid_glmm2~z.DIA_C,data = glmm_pp_z,ylab="Residuals",main="GLMM")
plotLowess(resid_glmm2~z.CR_weib,data = glmm_pp_z,ylab="Residuals",main="GLMM")
plotLowess(resid_glmm2~z.ppt_pAugOct,data = glmm_pp_z,ylab="Residuals",main="GLMM") #still heteroskedasticity
plotLowess(resid_glmm2~z.tmax_JunAug,data = glmm_pp_z,ylab="Residuals",main="GLMM")
plotLowess(resid_glmm2~z.CCF,data = glmm_pp_z,ylab="Residuals",main="GLMM")
plotLowess(resid_glmm2~z.SICOND,data = glmm_pp_z,ylab="Residuals",main="GLMM")
plotLowess(resid_glmm2~z.SLOPE,data = glmm_pp_z,ylab="Residuals",main="GLMM")
#pearson not correct for GLMM
res2 <- simulateResiduals(fittedModel = glmm_2_red)
plot(res2)
res2_tre <- recalculateResiduals(res2, group = glmm_pp_z$TRE_CN)
plot(res2_tre, quantreg = FALSE)
testTemporalAutocorrelation(res2_tre, time = glmm_pp_z$Year)
testTemporalAutocorrelation(res2_tre)
#grouping not working
testUniformity(res2)
res_glmm2 <- res$scaledResiduals
hist(res_glmm2)

#nlme
resid_nlme1 <- residuals(lmm_ar1,type="pearson",scaled=TRUE)
qqnorm(resid_nlme1,main="QQ plot of LMM_AR1")
qqline(resid_nlme1)

resid_nlme2 <- residuals(lmm_wght,type="pearson",scaled=TRUE)
qqnorm(resid_nlme2,main="QQ plot of LMM_weight")
qqline(resid_nlme2)
hist(resid_nlme2)
pred_lmm_w <- predict(lmm_wght, type="response")
plotLowess(resid_nlme2~pred_lmm_w, ylab="Residuals",xlab="Predicted", main="LMM_w")

plotLowess(resid_nlme2~z.DIA_C,data = glmm_pp_z,ylab="Residuals",main="LMM")
plotLowess(resid_nlme2~z.CR_weib,data = glmm_pp_z,ylab="Residuals",main="LMM")
plotLowess(resid_nlme2~z.ppt_pAugOct,data = glmm_pp_z,ylab="Residuals",main="LMM") #still heteroskedasticity
plotLowess(resid_nlme2~z.tmax_JunAug,data = glmm_pp_z,ylab="Residuals",main="LMM")
plotLowess(resid_nlme2~z.CCF,data = glmm_pp_z,ylab="Residuals",main="LMM")
plotLowess(resid_nlme2~z.SICOND,data = glmm_pp_z,ylab="Residuals",main="LMM")
plotLowess(resid_nlme2~z.SLOPE,data = glmm_pp_z,ylab="Residuals",main="LMM")

#di response
resid_nlme3 <- residuals(nlm_di,type="pearson",scaled=TRUE)
qqnorm(resid_nlme3,main="QQ plot of LMM_di")
qqline(resid_nlme3)
hist(resid_nlme3,breaks=50)
pred_nlm_di <- predict(nlm_di, type="response")
plotLowess(resid_nlme3~pred_nlm_di, ylab="Residuals",xlab="Predicted", main="LMM_w")

plotLowess(resid_nlme3~z.DIA_C,data = glmm_pp_z,ylab="Residuals",main="LMM")
plotLowess(resid_nlme3~z.CR_weib,data = glmm_pp_z,ylab="Residuals",main="LMM")
plotLowess(resid_nlme3~z.ppt_pAugOct,data = glmm_pp_z,ylab="Residuals",main="LMM") #still heteroskedasticity
plotLowess(resid_nlme3~z.tmax_JunAug,data = glmm_pp_z,ylab="Residuals",main="LMM")
plotLowess(resid_nlme3~z.CCF,data = glmm_pp_z,ylab="Residuals",main="LMM")
plotLowess(resid_nlme3~z.SICOND,data = glmm_pp_z,ylab="Residuals",main="LMM")
plotLowess(resid_nlme3~z.SLOPE,data = glmm_pp_z,ylab="Residuals",main="LMM")

age <- glmm_pp_z[,] #z.dbh
plotLowess(presid_glm~age,data = glmm.data.trun,ylab="Residuals",main="Gamma")

#for individual effects
library(effects)
plot(effect("z.sdi",glm.intslp))

#mark castle suggestions
#effect size of DBH
#why insignificant?
#pp
pred_lmm_pp <- predict(clim_wy_red, type = "response")
plot(pred_lmm_pp,glmm_pp_z$z.DIA_C, main = "Effect size of DBH for PP")
plot(exp(1)^pred_lmm_pp,glmm_pp_z$z.DIA_C, main = "Effect size of DBH for PP")

pred_glmm_pp <- predict(glmm_wy_red, type = "response")
plot(pred_glmm_pp,glmm_pp_z$z.DIA_C, main = "Effect size of DBH for PP")
plot(pred_glmm_pp,glmm_pp_z$z.BAL, main = "Effect of BAL for PP")

#df
pred_lmm_df <- predict(clim_wy_red, type = "response")
plot(pred_lmm_df,glmm_df_z$z.DIA_C, main = "Effect size of DBH for DF")
plot(exp(1)^pred_lmm_df,glmm_df_z$z.DIA_C, main = "Effect size of DBH for DF")
plot(exp(1)^pred_lmm_df,glmm_df_z$z.BAL, main = "Effect of BAL for DF")

pred_glmm_df <- predict(glmm_red, type = "response")
plot(pred_glmm_df,glmm_df_z$z.DIA_C, main = "Effect size of DBH for DF")
plot(pred_glmm_df,glmm_df_z$z.BAL, main = "Effect of BAL for DF")

#es
pred_lmm_es <- predict(clim_1_red, type = "response")
plot(pred_lmm_es,glmm_es_z$z.DIA_C, main = "Effect size of DBH for ES")
plot(exp(1)^pred_lmm_es,glmm_es_z$z.DIA_C, main = "Effect size of DBH for ES")

pred_glmm_es <- predict(glmm_red, type = "response")
plot(pred_glmm_es,glmm_es_z$z.DIA_C, main = "Effect size of DBH for ES")
plot(pred_glmm_es,glmm_es_z$z.BAL, main = "Effect of BAL for ES")

#
library(sjPlot)
library(sjmisc)


#visualize effects - coefficients
ggcoef()


