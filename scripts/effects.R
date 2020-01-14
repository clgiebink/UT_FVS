#effects plots
#mark castle

#Produce dataframe with range of DBH values and all other covariates held at a constant value (the mean)

Preds<-expand.grid(z.DIA_C = seq(from = 1, to = 50, by = 1),
                   z.CR_weib = mean(glmm_df_z$z.CR_weib),
                   z.wateryr = mean(glmm_df_z$z.wateryr),
                   z.tmax_FebJul =  mean(glmm_df_z$z.tmax_FebJul),
                   z.BAL = mean(glmm_df_z$z.BAL),
                   z.CCF = mean(glmm_df_z$z.CCF),
                   z.SICOND = mean(glmm_df_z$z.SICOND),
                   z.SLOPE = mean(glmm_df_z$z.SLOPE),
                   z.ASPECT = mean(glmm_df_z$z.ASPECT))

#Generate predictions of diameter growth using fixed effects only
#Choose which ever equation you want to use - I just selected one for this example
Preds$PDDS<-predict(clim_1_red, newdata = Preds, re.form = 0)

#Exponentiate DDS predictions and plot
Preds$PDDS<-exp(Preds$PDDS)
plot(Preds$DBH, Preds$PDDS)

#same for BAL

#using effects package
library(effects)

#df
plot(effect("z.DIA_C",lmm2_crf_df),xlab = "DBH",main = "DBH")
plot(effect("z.CR_weib",lmm2_crf_df),xlab = "Crown Ratio",main = "Crown Ratio")
plot(effect("z.CCF",lmm2_crf_df),xlab = "CCF",main = "CCF")
plot(effect("z.SICOND",lmm2_crf_df),xlab = "Site Index",main = "Site Index")
plot(effect("z.SLOPE",lmm2_crf_df),xlab = "Slope",main = "Slope")
plot(effect("z.wateryr",lmm2_crf_df),xlab = "Precipitation",main = "Precipitation")
plot(effect("z.tmax_FebJul",lmm2_crf_df),xlab = "Temperature",main = "Temperature")

#pp   
plot(effect("z.DIA_C",lmm2_crf_pp),xlab = "DBH",main = "DBH")
plot(effect("z.CR_weib",lmm3_crf_pp),xlab = "Crown Ratio",main = "Crown Ratio")
plot(effect("z.CCF",lmm2_crf_pp),xlab = "CCF",main = "CCF")
plot(effect("z.SICOND",lmm2_crf_pp),xlab = "Site Index",main = "Site Index")
plot(effect("z.SLOPE",lmm2_crf_pp),xlab = "Slope",main = "Slope")
plot(effect("z.ppt_pAugJul",lmm2_crf_pp),xlab = "Precipitation",main = "Precipitation")
plot(effect("z.tmax_JunAug",lmm2_crf_pp),xlab = "Temperature",main = "Temperature")
