#Calculate CCF from annualized DBH
#Courtney Giebink
#clgiebink@email.arizona.edu
#4-24-19

#load data
load(file = "./data/formatted/glmm.data.imputed")

#Crown Competition Factor
#measure of stand density
#equations taken from Utah variant guide
##If DBH greater than or equal to 1” CCFt= R1 + (R2 * DBH) + (R3 * DBH2)
##If DBH less than 1” but greater than 0.1” CCFt = R4 * DBHR5
##If DBH less than 0.1” CCFt = 0.001


#dataframe of species specific coefficients for ccf calculation
ccf_df <- data.frame(species=c(93,202,122),
                     #93=ES, 202=DF, 122=PP
                     r1 = c(0.03,0.11,0.03),
                     r2 = c(0.0173,0.0333,0.0180),
                     r3 = c(0.00259,0.00259,0.00281),
                     r4 = c(0.007875,0.017299,0.007813),
                     r5 = c(1.7360,1.5571,1.7680)) #can add more species later 

#empty vector to hold calculated crown competition factor
CCF_t <- vector(mode="numeric", length=nrow(glmm.data.imputed))
for(i in 1:nrow(glmm.data.imputed)){
  Species <- glmm.data.imputed$SPCD[i]
  r1 <- ccf_df$r1[ccf_df$species == Species]
  r2 <- ccf_df$r2[ccf_df$species == Species]
  r3 <- ccf_df$r3[ccf_df$species == Species]
  r4 <- ccf_df$r4[ccf_df$species == Species]
  r5 <- ccf_df$r5[ccf_df$species == Species]
  ifelse(is.na(glmm.data.imputed$DIA_C[i]), 
         CCF_t[i] <- NA,
         ifelse(glmm.data.imputed$DIA_C[i] <= 0.1, 
                CCF_t[i] <- 0.0001,
                ifelse(glmm.data.imputed$DIA_C[i] < 1.0, 
                       CCF_t[i] <- r4 * (glmm.data.imputed$DIA_C[i]^r5),
                       CCF_t[i] <- r1 + (r2 * glmm.data.imputed$DIA_C[i]) + (r3 * glmm.data.imputed$DIA_C[i]^2))))
  glmm.data.imputed$CCF_t[i] <- CCF_t[i]
}

#PCCF is the crown competition factor on the inventory point where the tree is established
glmm.data.imputed$PCCF <- glmm.data.imputed$CCF_t*glmm.data.imputed$TPA_UNADJ

#stand CCF = sum(CCFt on a plot)
CCF <- vector(mode="numeric", length=nrow(glmm.data.imputed))
for(i in 1:nrow(glmm.data.imputed)){
  plot_cn <- glmm.data.imputed$PLT_CN.y[i]
  year <- glmm.data.imputed$Year[i]
  pccf_df <- glmm.data.imputed[glmm.data.imputed$Year == year & glmm.data.imputed$PLT_CN.y == plot_cn,]
  glmm.data.imputed$CCF[i] <- sum(pccf_df$PCCF)
}
#above will not include other species on the plot besides ES, DF, and PP
#in AnnualizeDBH.R
#might have to include other species

save(glmm.data.imputed,file = "./data/formatted/glmm.data.imputed")