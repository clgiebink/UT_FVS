#Calculate Crown Ratio from annualized DBH
#Courtney Giebink
#clgiebink@email.arizona.edu
#4-25-19

#Calculations modified from Mark Castle
#email: markcastle@fs.fed.us

#First need to calcuate SDI
#Calculations from John Shaw (2000; Stage 1968)
#SDI = sum((DIA_t/10)^2)

SDI <- vector(mode="numeric", length=nrow(glmm.data.imputed))
for(i in 1:nrow(glmm.data.imputed)){
  plot_cn <- glmm.data.imputed$PLT_CN.y[i]
  year <- glmm.data.imputed$Year[i]
  DIA_tpa <- glmm.data.imputed$DIA_C[i]*glmm.data.imputed$TPA_UNADJ[i]
  glmm.data.imputed$DIA_tpa_cal[i] <- (DIA_tpa/10)^2
  SDI_df <- glmm.data.imputed[glmm.data.imputed$Year == year & glmm.data.imputed$PLT_CN.y == plot_cn,]
  glmm.data.imputed$SDI[i] <- sum(SDI_df$DIA_tpa_cal)
}

##UT variant guide instructions below:

#dbh>1 (excludes common pinyon)
#average stand crown ratio estimated from stand density
#ACR = do + d1 * RELSDI * 100 , where RELSDI = SDIstand/SDImax

#weibull parameters estimated from ACR
#A = ao
#B = bo + b1 * ACR (B>=1)
#C = co + c1 * ACR (C>=2)

#Weibull distribution
#Y = 1-exp(-((X-A)/B))^C

#Scale = 1 - 0.00167 * (CCF - 100)

#Original code by Mark Castle
#markcastle@fs.fed.us
#Refer to UT variant overview as you go through the steps in this script

#Read in data
load('glmm.data.imputed')

#####################################
#Step 1:
#Calculate stand density index (RSDI)
#and crown competition factor (CCF)
#####################################

#Instead of Reineke SDI (for stand) use
#Calculations from John Shaw (2000; Stage 1968)
#SDI = sum((DIA_t/10)^2)

SDI <- vector(mode="numeric", length=nrow(glmm.data.imputed))
for(i in 1:nrow(glmm.data.imputed)){
  plot_cn <- glmm.data.imputed$PLT_CN.y[i]
  year <- glmm.data.imputed$Year[i]
  DIA_tpa <- glmm.data.imputed$DIA_C[i]*glmm.data.imputed$TPA_UNADJ[i]
  glmm.data.imputed$DIA_tpa_cal[i] <- (DIA_tpa/10)^2
  SDI_df <- glmm.data.imputed[glmm.data.imputed$Year == year & glmm.data.imputed$PLT_CN.y == plot_cn,]
  glmm.data.imputed$SDI[i] <- sum(SDI_df$DIA_tpa_cal)
}

#Crown Competition Factor (CCF - for stand)
#Refer to variant overview for how to calculate CCF
#calculated in CCF.R script

head(glmm.data.imputed)

#####################################
#Step 2:
#Calculate relative density (RD)
#Step 3:
#Calculate average stand crown ratio
#by species (ACR)
#Step 4:
#Determine Weibull parameters to use
#for crown ratio calculation
#Step 5:
#Calculate crown ratio for tree
#records
#####################################

#Function for calculating relative density for each species in a stand
#Maximum stand density index at stand level is weighted average,
#by basal area proportion of individual species SDI max
##for now use individual SDI max
##from UT variant guide

CR_WEIB_df <- data.frame(species=c(93,202,122,15,19,96,108),
                         #93=ES,202=DF,122=PP,15=WF,19=AF,65=UJ,96=BS,106=PI,108=LP,133=PM,321=OH
                         SDIMAX = c(625,440,400,560,625,625,540),
                         a0 = c(1,1,1,1,1,1,0),
                         b0 = c(-0.90648,-0.24217,-0.82631,-0.89553,-0.89553,-0.90648,0.17162),
                         b1 = c(1.08122,0.96529,1.06217,1.07728,1.07728,1.08122,1.07338),
                         c0 = c(3.48889,-7.94832,-1.02873,1.74621,1.74621,3.48889,3.15000),
                         c1 = c(0,1.93832,0.80143,0.29052,0.29052,0,0),
                         d0 = c(6.81087,7.46296,6.19911,7.65751,7.65751,6.81087,6.00567),
                         d1 = c(-0.01037,-0.02944,-0.02216,-0.03513,-0.03513,-0.01037,-0.03520))

#some species use height
#PI,WJ,GO,PM,UJ,OH
#CL = lm(HT)
#CR = CL/HT

CR_weib <- vector(mode="numeric", length=nrow(glmm.data.imputed))
for(i in 1:nrow(glmm.data.imputed)){
  #Function arguments:
  #SPCD - is number code of species of tree record
  #SDI - is SDI of stand (Stage 1968)
  Species <- glmm.data.imputed$SPCD[i]
  #SDI max values for each species were pulled from UT Variant overview
  SDIMAX <- CR_WEIB_df$SDIMAX[CR_WEIB_df$species == Species]
  #Calculate relative density
  RD <- glmm.data.imputed$SDI[i]/SDIMAX
  
  #Calculate average stand crown ratio (ACR) for each species in the stand
  d0 <- CR_WEIB_df$d0[CR_WEIB_df$species == Species]
  d1 <- CR_WEIB_df$d1[CR_WEIB_df$species == Species]
  ACR <- d0 + d1 * RD * 100
  
  #Parameters of Weibull distribution: A,B,C
  a0 <- CR_WEIB_df$a0[CR_WEIB_df$species == Species]
  b0 <- CR_WEIB_df$b0[CR_WEIB_df$species == Species]
  b1 <- CR_WEIB_df$b1[CR_WEIB_df$species == Species]
  c0 <- CR_WEIB_df$c0[CR_WEIB_df$species == Species]
  c1 <- CR_WEIB_df$c1[CR_WEIB_df$species == Species]
  
  #A parameter
  WEIBA <-a0
  #B parameter
  WEIBB <- b0 + b1*ACR
  #C parameter
  WEIBC <- c0 + c1*ACR
  
  #Function arguments:
  
  #CCF - crown competition factor of stand
  #rank_pltyr - tree's rank in diameter distribution by plot by year
  #N  - number of records in the stand by year
  
  #Calculate scale parameter
  SCALE = (1.0 - .00167 * (glmm.data.imputed$CCF[i]-100.0))
  
  plot_cn <- glmm.data.imputed$PLT_CN.y[i]
  year <- glmm.data.imputed$Year[i]
  rank_df <- glmm.data.imputed[glmm.data.imputed$Year == year 
                               & glmm.data.imputed$PLT_CN.y == plot_cn,]
  N <- length(rank_df$TRE_CN)
  #X is tree's rank in diameter distribution
  #Multiply tree's rank in diameter distribution (trees position relative to tree with largest diameter in the stand) by scale parameter
  X <- glmm.data.imputed$rank_pltyr[i]/N * SCALE
  
  #Constrain X between 0.05 and 0.95 - crown ratio predictions in FVS are bound between these two values
  if(X < 0.05){X = 0.05}
  if(X > 0.95){X = 0.95}
  
  #Calculate crown ratio (this corresponds to variable Y in UTAH variant overview)
  glmm.data.imputed$CR_weib[i] <- WEIBA + WEIBB*(-1*log(1-X))^(1/WEIBC)
}


  #If a tree has a DBH less than 1" or is dead, assign a value of zero to CR
  #Use the equation you were using before to impute crown ratio values for these records.
  #Your live and dead codes for tree records will likely be different than the FVS codes
  #if(DBH < 1 | HISTORY == 6 | HISTORY == 9)
  #{
  #  CR = 0
  #}


#Subset out live trees with DBH >= 1 to check crown ratio values predicted by weibull function
cr.check<-subset(cr.data, History == 1 & DBH >= 1);cr.check

#Compare CR_PRED values against predicted crown ratios in treelist file (CR column) output by UTAH_CR.key for year 2014 ONLY
#They should be within 1-2 percent. The difference I think is due to rounding error between the FVS FORTRAN code and R. 

#Extract neccesary columns
cr.check<-cr.check[,c("Stand_CN", "Stand_ID", "StandPlot_CN", "StandPlot_ID", "Plot_ID", "Tree_Count",
                      "History", "Species", "DBH", "DG", "Ht", "HTG", "HtTopK", "TREE_INDEX", "Tree_ID", "CR_PRED")]
cr.check

#Also FVS does have logic for adjusting a tree's crown ratio for top-kill 
#I did not include this in the script.