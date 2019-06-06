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
ccf_df <- data.frame(species=c(93,202,122,15,19,65,96,106,108,133,321),
                     #93=ES,202=DF,122=PP,15=WF,19=AF,65=UJ,96=BS,106=PI,108=LP,133=PM,321=OH
                     r1 = c(0.03,0.11,0.03,0.04,0.03,0.01925,0.03,0.01925,0.01925,0.01925,0.03),
                     r2 = c(0.0173,0.0333,0.0180,0.0270,0.0216,0.01676,0.0173,0.01676,0.01676,0.01676,0.0215),
                     r3 = c(0.00259,0.00259,0.00281,0.00405,0.00405,0.00365,0.00259,0.00365,0.00365,0.00365,0.00363),
                     r4 = c(0.007875,0.017299,0.007813,0.015248,0.011402,0.009187,0.007875,0.009187,0.009187,0.009187,0.011109),
                     r5 = c(1.7360,1.5571,1.7680,1.7333,1.7560,1.76,1.736,1.76,1.76,1.76,1.7250)) #can add more species later 

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

#some of the CCF values are NA while PCCF is >0
#error will have to be checked
#for a lot of trees stand CCF == PCCF
#need to get more trees

save(glmm.data.imputed,file = "./data/formatted/glmm.data.imputed")
