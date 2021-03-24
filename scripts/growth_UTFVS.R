#Large Tree Diameter Growth Model
#UT variant FVS
#Courtney Giebink
#clgiebink@gmail.com
#Feb 2020

b1_df <- data.frame(species = c(202, 93, 122),
                    loc_401 = c(0.192136,0.011943,-0.13235), #Ashley
                    loc_407 = c(-0.064516,0.011943,-0.460129), #Dixie
                    loc_408 = c(-0.064516,0.011943,-0.460129), #Fishlake
                    loc_409 = c(-0.064516,0.011943,-0.460129), #Humbolt
                    loc_417 = c(-0.064516,0.011943,-0.460129), #Toiyabe
                    loc_410 = c(-0.064516,0.265071,-0.302309), #MantiLaSal
                    loc_418 = c(0.477698,-0.094861,-0.302309), #Uinta
                    loc_419 = c(0.589169,0.796948,-0.302309), #Wasatch
                    loc_404 = c(0.589169,0.796948,-0.302309)) #Cache

#b2 is species specific site index
#left column is based off of what species has the greatest basal area of the stand

#site species coefficient
b2_df <- data.frame(species = c(202, 93, 122),
                    ss_rest = c(0.010968,0.015133,0.019282), #WB, LM, LP , PI, WJ, GO, PM, RM, UJ, GB, NC, FC, MC, BI, BE, OH 
                    ss_202 = c(0.006827,0.021085,0.019282), #DF
                    ss_19 = c(0.010968,0.021085,0.019282), #AF
                    ss_93 = c(0.006827,0.021085,0.019282), #ES, BS
                    ss_746 = c(0.010968,0.021085,0.019282), #AS
                    ss_15 = c(0.010968,0.021085,0.049804), #WF
                    ss_122 = c(0.010968,0.021085,0.02943)) #PP, OS

b_all_df <- data.frame(species = c(202,93,122),
                       b3 = c(0.022753,-0.122483,-0.287654),
                       b4 = c(0.015235,-0.198194,-0.411292),
                       b5 = c(-0.532905,0.240433,0.016965),
                       b6 = c(-0.086518,0,2.282665),
                       b7 = c(0.479631,0.587579,0.733305),
                       b8 = c(-0.707380,-0.399357,-0.320124),
                       b9 = c(3.182592,0.331129,1.315804),
                       b10 = c(-1.310144,0.816301,0.238917),
                       b11 = c(0,0,-0.0005345),
                       b12 = c(-0.001613,0,-0.002576),
                       b13 = c(0,-0.043414,0))

#dataframe of coefficients for bark ratio calculation
#from Utah variant guide
bratio_df <- data.frame(species=c(93,202,122,15,19,65,96,106,108,133,321),
                        #93=ES,202=DF,122=PP,15=WF,19=AF,65=UJ,96=BS,106=PI,108=LP,133=PM,321=OH
                        b1 = c(0.9502,0.867,0.8967,0.890,0.890,0.9002,0.9502,0.9002,0.9625,0.9002,0.93789),
                        b2 = c(-0.2528, 0, -0.4448,0,0,-0.3089,-0.2528,-0.3089,-0.1141,-0.3089,-0.24096),
                        exp = c(1,0,1,0,0,1,1,1,1,1,1)) #can add more species later 
#DBH0 = DBH - k * DG , where k = 1/BRATIO
# so DBH0 + k*DG = DBH
# DG   = periodic increment in inside bark diameter 
#see page 53 of prognosis model
#DG = sqrt(dib^2 + dds) - dib
#dib  = inside bark diameter at the beginning of the growth period
#DG = sqrt((DBH/k)^2 + dds) - (DBH/k)
#see UT variant guide
#BRATIO = (b1 + b2)/(DBH^exp)

for(i in 1:nrow(val_dset)){
  Species <- val_dset$SPCD[i]
  loc_cd <- val_dset$FVS_LOC_CD[i] #forest code
  site_sp <- val_dset$site_sp[i] #species with the largest BA on a plot
  #model coefficients
  b1 <- b1_df[b1_df$species == Species, grepl(loc_cd,names(b1_df))] #grabs column with matching species code
  ifelse(site_sp %in% c(202,19,93,746,15,122), #need to add BS and OS
         b2 <- b2_df[b2_df$species == Species, grepl(site_sp,names(b2_df))],
         b2 <- b2_df[b2_df$species == Species, 2])
  b3 <- b_all_df$b3[b_all_df$species == Species]
  b4 <- b_all_df$b4[b_all_df$species == Species]
  b5 <- b_all_df$b5[b_all_df$species == Species]
  b6 <- b_all_df$b6[b_all_df$species == Species]
  b7 <- b_all_df$b7[b_all_df$species == Species]
  b8 <- b_all_df$b8[b_all_df$species == Species]
  b9 <- b_all_df$b9[b_all_df$species == Species]
  b10 <- b_all_df$b10[b_all_df$species == Species]
  b11 <- b_all_df$b11[b_all_df$species == Species]
  b12 <- b_all_df$b12[b_all_df$species == Species]
  b13 <- b_all_df$b13[b_all_df$species == Species]
  SICOND <- val_dset$SICOND[i] #site index
  ASPECT <- val_dset$ASPECT[i] #aspect of slope
  SLOPE <- val_dset$SLOPE[i]/100 #inclination expressed as percent
  DIA <- val_dset$DIA[i] #DBH
  BAL <- val_dset$BAL[i] #basal area of trees larger than the subject tree
  CR <- val_dset$UNCRCD[i]/100 #uncompacted crown ratio expressed as a proportion
  PCCF <- val_dset$PCCF[i] #crown competition factor on the subplot
  CCF <- val_dset$CCF[i] #stand crown competition factor
  #large tree diameter growth model
  val_dset$log_dds[i] <- b1 + (b2*SICOND) + (b3*sin(ASPECT-0.7854)*SLOPE) + (b4*cos(ASPECT-0.7854)*SLOPE) +
    (b5*SLOPE) + (b6*I(SLOPE^2)) + (b7*I(log(DIA))) + (b8*I(BAL/100)) + (b9*CR) +
    (b10*I(CR^2)) + (b11*I(DIA^2)) + (b12*PCCF) + (b13*I(CCF/100))
  #from log(dds) to DBHt+1
  #scale to remeasurement interval
  dds_adj <- ((val_dset$fMEASYEAR[i]-val_dset$MEASYEAR[i])/10)*exp(val_dset$log_dds[i])
  #BRATIO = (b1+b2)/(DBH^exp)
  #k = 1/BRATIO
  b1 <- bratio_df$b1[bratio_df$species == Species]
  b2 <- bratio_df$b2[bratio_df$species == Species]
  exp <- bratio_df$exp[bratio_df$species == Species]
  k <- 1/((b1+b2)/(DIA^exp)) #exp determines if use equation 4.2.1/3 or 4.2.2 in UT variant guide
  #DG = sqrt((DBH/k)^2 + dds - (DBH/k))
  DG <- sqrt((DIA/k)^2 + dds_adj) - (DIA/k)
  #so DBH0 + k*DG = DBH
  val_dset$eDIA[i] <- DIA + (k*DG)
}

