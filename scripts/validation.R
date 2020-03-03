#Validation
#grow trees forward with model object
#Courtney Giebink
#clgiebink@gmail.com
#February 2020


# FIA ----

#data from FIADB
plot <- read.csv("./data/raw/UT_PLOT.csv",header=T)
tree <- read.csv("./data/raw/UT_TREE.csv",header = T)
cond <- read.csv("./data/raw/UT_COND.csv",header = T)

# Get trees
# tree: "TRE_CN","PLT_CN","SUBP","PREV_TRE_CN","DIA","UNCRCD","SITREE","TPA_UNADJ"
##also grab previous tree CN?
# cond: CONDID, SLOPE, ASPECT, SDIMAX_RMRS, SICOND, BALIVE, DSTRBCD1
# plot: LAT, LON, ELEV, MEASYEAR, DESIGNCD, SUBP_EXAMINE_CD, PREV_MEASYEAR

#select relevant tree table attributes
val_dset <- tree %>%
  select(CN,PLT_CN,STATUSCD,SPCD,SUBP,PREV_TRE_CN,DIA,UNCRCD,TPA_UNADJ)
#get DIA at remeasurement
val_dset$fDIA <- tree$DIA[match(val_dset$CN,tree$PREV_TRE_CN)]
#get status (dead or alive) of trees at remeasurement
val_dset$fSTATUSCD <- tree$STATUSCD[match(val_dset$CN,tree$PREV_TRE_CN)]
#filter for trees that were remeasured
#trees can have a PREV_TRE_CN of NA if first measurement
val_dset <- val_dset %>%
  filter(CN %in% PREV_TRE_CN &
           SPCD %in% c(93,122,202)) %>% #filter for focal species
  #filter for alive trees at both remeasurement
  filter(STATUSCD == 1 & fSTATUSCD == 1) %>%
  #filter for trees that that are larger at remeasurement/grew
  filter(DIA <= fDIA)

val_check <- val_dset %>%
  group_by(SPCD) %>%
  summarise(null = length(which(is.na(UNCRCD))),
            full = length(which(!is.na(UNCRCD))),
            n = n())
#filter for UNCRCD?
val_crtest <- val_dset %>%
  filter(!is.na(UNCRCD))
length(unique(val_crtest$TRE_CN)) #1082
save(val_crtest,file = './data/formatted/val_crtest.Rdata')
#filter for SICOND?
val_sitest <- val_dset %>%
  filter(!is.na(SICOND))
val_check <- val_sitest %>%
  group_by(SPCD) %>%
  summarise(n = n())

colnames(val_dset)[colnames(val_dset)=="CN"] <- "TRE_CN"

val_dset$CONDID <- cond$CONDID[match(val_dset$PLT_CN, cond$PLT_CN)]
val_dset$ASPECT <- cond$ASPECT[match(val_dset$PLT_CN, cond$PLT_CN)]
val_dset$SLOPE <- cond$SLOPE[match(val_dset$PLT_CN, cond$PLT_CN)]
val_dset$SDI <- cond$SDI_RMRS[match(val_dset$PLT_CN, cond$PLT_CN)]
#all SDI are NA, need to calculate SDI
val_dset$SDImax <- cond$SDIMAX_RMRS[match(val_dset$PLT_CN, cond$PLT_CN)]
val_dset$SICOND <- cond$SICOND[match(val_dset$PLT_CN, cond$PLT_CN)]
val_dset$DSTRBCD1 <- cond$DSTRBCD1[match(val_dset$PLT_CN, cond$PLT_CN)]

#BALIVE 
#val_dset$BALIVE <- apply(X = grData_remeas[, c("PREV_PLT_CN", "PREV_CONDID")], 
#                              MARGIN = 1, # applies function to each row in grData_remeas
#                              FUN = function(x, conds.df) {
#                                conds.df$BALIVE[conds.df$PLT_CN %in% x["PREV_PLT_CN"] &
#                                                  conds.df$CONDID %in% x["PREV_CONDID"]]
#                              },
#                              conds.df = conds)
#grData_remeas[is.nan(grData_remeas$BALIVE), "BALIVE"] <- NA
val_dset$BALIVE <- cond$BALIVE[match(val_dset$PLT_CN, cond$PLT_CN)]

val_dset$LAT <- plot$LAT[match(val_dset$PLT_CN, plot$CN)]
val_dset$LON <- plot$LON[match(val_dset$PLT_CN, plot$CN)]
val_dset$ELEV <- plot$ELEV[match(val_dset$PLT_CN, plot$CN)]
val_dset$MEASYEAR <- plot$MEASYEAR[match(val_dset$PLT_CN, plot$CN)]
val_dset$DESIGNCD <- plot$DESIGNCD[match(val_dset$PLT_CN, plot$CN)]
val_dset$SUBP_EXAM <- plot$SUBP_EXAMINE_CD[match(val_dset$PLT_CN, plot$CN)]
val_dset$fMEASYEAR <- plot$MEASYEAR[match(val_dset$PLT_CN, plot$PREV_PLT_CN)]

val_measyr <- val_dset %>%
  select(MEASYEAR,fMEASYEAR) %>%
  distinct()

#filter no disturbance
val_dset <- val_dset %>%
  filter(DSTRBCD1 == 0) %>%
  filter(!is.na(UNCRCD)) %>%
  filter(!is.na(SICOND))

# explore?
# Calculate census interval

#all available trees
save(val_dset,file = "./data/formatted/val_dset.Rdata")

# Val trees ----
#choose based on years of growth? climate conditions
UT_clim_an <- read.csv("./data/raw/climate/UTanPRISM_00_18.csv",header =T)
#randomize  

# Density ----
#for current year
#fetch trees from same plot in FIADB
#with associated covariate data (above)
#will include other species
##use current FVS model?
#could include small trees
##project with FVS small tree growth equations?


## CCF ----


## BAL ----


## CR_weib ----


# Current FVS ----

#large tree

#covariates
#density
#ccf - calculate
#bal - 
#cr - given

#get trees from same plot
plot_val <- unique(val_dset$PLT_CN)
tree_val <- unique(val_dset$TRE_CN)
density_val <- tree[(tree$PLT_CN %in% plot_val),c("CN","PLT_CN","SUBP","SPCD","DIA","TPA_UNADJ","UNCRCD")]
#make sure trees are on the same plot b/c calculating stand variables
#make sure I'm not including validation trees
colnames(density_val)[colnames(density_val)=="CN"] <- "TRE_CN"

density_val$MEASYEAR <- plot$MEASYEAR[match(density_val$PLT_CN, plot$CN)]
density_val$DESIGNCD <- plot$DESIGNCD[match(density_val$PLT_CN, plot$CN)]
density_val$CONDID <- cond$CONDID[match(density_val$PLT_CN, cond$PLT_CN)]
density_val$SDImax <- cond$SDIMAX_RMRS[match(density_val$PLT_CN, cond$PLT_CN)]

#check
length(plot_val) #129
length(unique(density_val$PLT_CN)) #129


#calculate density metrics
#ccf
#equations taken from Utah variant guide
##If DBH greater than or equal to 1” CCFt= R1 + (R2 * DBH) + (R3 * DBH2)
##If DBH less than 1” but greater than 0.1” CCFt = R4 * DBHR5
##If DBH less than 0.1” CCFt = 0.001


#dataframe of species specific coefficients for ccf calculation
ccf_df <- data.frame(species=c(93,202,122,15,19,65,96,106,108,133,321,66,475,113,746,814,102),
                     #93=ES,202=DF,122=PP,15=WF,19=AF,65=UJ,66=RM,96=BS,106=PI,108=LP,113=LM,133=PM,321=OH,475=MC,746=AS,814=GO,102=bristlecone pine
                     r1 = c(0.03,0.11,0.03,0.04,0.03,0.01925,0.03,0.01925,0.01925,0.01925,0.03,0.01925,0.0204,0.01925,0.03,0.03,0.01925),
                     r2 = c(0.0173,0.0333,0.0180,0.0270,0.0216,0.01676,0.0173,0.01676,0.01676,0.01676,0.0215,0.01676,0.0246,0.01676,0.0238,0.0215,0.01676),
                     r3 = c(0.00259,0.00259,0.00281,0.00405,0.00405,0.00365,0.00259,0.00365,0.00365,0.00365,0.00363,0.00365,0.0074,0.00365,0.00490,0.00363,0.00365),
                     r4 = c(0.007875,0.017299,0.007813,0.015248,0.011402,0.009187,0.007875,0.009187,0.009187,0.009187,0.011109,0.009187,0,0.009187,0.008915,0.011109,0.009187),
                     r5 = c(1.7360,1.5571,1.7680,1.7333,1.7560,1.76,1.736,1.76,1.76,1.76,1.7250,1.76,0,1.76,1.78,1.725,1.76),
                     dbrk = c(1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,10)) #can add more species later 

#empty vector to hold calculated crown competition factor
CCF_t <- vector(mode="numeric", length=nrow(density_val))
for(i in 1:nrow(density_val)){
  Species <- density_val$SPCD[i]
  r1 <- ccf_df$r1[ccf_df$species == Species]
  r2 <- ccf_df$r2[ccf_df$species == Species]
  r3 <- ccf_df$r3[ccf_df$species == Species]
  r4 <- ccf_df$r4[ccf_df$species == Species]
  r5 <- ccf_df$r5[ccf_df$species == Species]
  dbrk <- ccf_df$dbrk[ccf_df$species == Species]
  if(Species == 475){
    ifelse(density_val$DIA[i] < dbrk,
           CCF_t <- density_val$DIA[i]*(r1+r2+r3),
           CCF_t <- r1 + (r2 * density_val$DIA[i]) + (r3 * density_val$DIA[i]^2))
  }
  if(Species != 475){
    ifelse(is.na(density_val$DIA[i]), 
           CCF_t <- NA,
           ifelse(density_val$DIA[i] <= 0.1, 
                  CCF_t <- 0.0001,
                  ifelse(density_val$DIA[i] < dbrk, 
                         CCF_t <- r4 * (density_val$DIA[i]^r5),
                         CCF_t <- r1 + (r2 * density_val$DIA[i]) + (r3 * density_val$DIA[i]^2))))
  }
  density_val$CCF_t[i] <- CCF_t
}

#PCCF is the crown competition factor on the inventory point where the tree is established
#pCCF = the sum of CCF_t on a subplot on a per acre basis
#subplot given by SUBP
#TPA is measured on a stand level, convert to subplot by multiplying by number of subplots

density_val <- density_val %>%
  group_by(PLT_CN,SUBP) %>%
  mutate(PCCF = sum(CCF_t * (TPA_UNADJ * 4), na.rm = TRUE))

#stand CCF = sum(CCFt on a plot) on a per acre basis
#plot given by PLT_CN
#TPA measured on a plot/stand level
density_val <- density_val %>%
  group_by(PLT_CN) %>%
  mutate(CCF = sum(CCF_t * TPA_UNADJ,na.rm = TRUE))

#bal
#basal area
# = dbh^2 * 0.005454 ; converts dbh in inches to squared feet
#basal area per acre
#BA*tpa
density_val <- density_val %>%
  mutate(BA_pa = ((DIA^2) * 0.005454) * TPA_UNADJ)

#BAL
#sum BApa of trees larger on the same plot in same year
density_val <- density_val %>%
  group_by(PLT_CN) %>%
  #min assigns lowest value to ties (1,2,3,3,5,6)
  mutate(BAL = map_dbl(DIA,~sum(BA_pa[DIA>.x],na.rm = TRUE)))

#site species
site_sum <- density_val %>%
  group_by(PLT_CN,SPCD) %>%
  summarise(BA_sp = sum(BA_pa,na.rm = T)) %>%
  ungroup() %>%
  group_by(PLT_CN) %>%
  filter(BA_sp == max(BA_sp))
density_val$site_sp <- site_sum$SPCD[match(density_val$PLT_CN, site_sum$PLT_CN)]
unique(density_val$site_sp)
#[1] 108  93  65 122 202  15  19 814 749  66 113 106 746 475
#93=ES,202=DF,122=PP,15=WF,19=AF,65=UJ,66=RM,106=PI,108=LP,113=LM,
#475=MC,746=AS,749=NC:narrowleaf cottonwood,814=GO:Gambel oak


#filter for focal trees
val_dset <- density_val %>%
  select(TRE_CN,CCF,PCCF,BAL,site_sp) %>%
  right_join(.,val_dset)

#grow focal trees
#coefficients

#b1 based on location
#location codes found in FIADB
#(FVS_LOC_CD) in PLOTGEOM table
library(dbplyr)
library(RSQLite)

UT_FIA <- DBI::dbConnect(RSQLite::SQLite(), "./data/raw/FS_FIADB_STATECD_49.db")
PLOTGEOM <- tbl(UT_FIA, sql("SELECT CN, FVS_LOC_CD FROM PLOTGEOM")) %>%
  collect()
val_dset$FVS_LOC_CD <- PLOTGEOM$FVS_LOC_CD[match(val_dset$PLT_CN, PLOTGEOM$CN)]

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
#BRATIO = b1 + b2/(DBH^exp)

for(i in 1:nrow(val_dset)){
  Species <- val_dset$SPCD[i]
  loc_cd <- val_dset$FVS_LOC_CD[i] #forest code
  site_sp <- val_dset$site_sp[i] #species with the largest BA on a plot
  #model coefficients
  b1 <- b1_df[b1_df$species == Species, grepl(loc_cd,names(b1_df))] #grabs column with species code in the name
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
  #parameters
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
  #BRATIO = b1+b2/(DBH^exp)
  b1 <- bratio_df$b1[bratio_df$species == Species]
  b2 <- bratio_df$b2[bratio_df$species == Species]
  exp <- bratio_df$exp[bratio_df$species == Species]
  BRATIO <- b1 + b2/(DIA^exp) #exp determines if use equation 4.2.1/3 or 4.2.2 in UT variant guide
  #k = 1/BRATIO
  k <- 1/BRATIO 
  #DG = sqrt((DBH/k)^2 + dds - (DBH/k))
  DG <- sqrt((DIA/k)^2 + dds_adj) - (DIA/k)
  #so DBH0 + k*DG = DBH
  val_dset$eDIA[i] <- DIA + (k*DG)
}

#expected vs predicted
plot(val_dset$eDIA,val_dset$fDIA,
     xlab = "predicted", ylab = "observed",
     main = "Current FVS Growth Model")

  #check difference between measyear and fmeasyear
#might need to express output as a proportion


#Tree-rings ----

#consider bark ratio
#use predict function for lmermod (lme4)


# Climate ----
#Precipitation
#Tmax
#for only large trees

#new trees w/ LAT & LON
val_trees <- val_dset %>%
  select(TRE_CN,LON,LAT)

# Make lat, lon data spatial
val_tree_spat <- SpatialPointsDataFrame(coords = cbind(val_trees$LON, val_trees$LAT), 
                                        data = val_trees, 
                                        proj4string = CRS("+proj=longlat +datum=NAD83"))

# Read in PRISM climate stacks
clim.path <-  "./data/formatted/"
ppt <- stack(paste(clim.path,"pptStack.tif",sep=''))
tmax <- stack(paste(clim.path,"tmaxStack.tif",sep=''))
tmin <- stack(paste(clim.path,"tminStack.tif",sep=''))

# raster::extract PRISM data
ppt.extr <- raster::extract(ppt, val_tree_spat) # this step takes about 8 minutes each (laptop)
tmin.extr <- raster::extract(tmin, val_tree_spat)
tmax.extr <- raster::extract(tmax, val_tree_spat)

# Dec 2000 - 
# Remove data after Oct, 2016 (because of different CRS Nov, 2016 vpdmax .bil)
# note that the work-around for this problem is to assign the CRS of another layer to Nov and Dec of 2016
# crs(vpdNov2016_raster) <- crs(vpdOct2016_raster)
ppt.extr <- ppt.extr[, 1:1272] #1895 - 2000
tmin.extr <- tmin.extr[, 1:1272]
tmax.extr <- tmax.extr[, 1:1272]

# Add sensible column names for raster::extracted climate data
ppt.extr <- as.data.frame(ppt.extr)
tmin.extr <- as.data.frame(tmin.extr)
tmax.extr <- as.data.frame(tmax.extr)
PRISM.path <-  "./data/raw/climate/PRISM/"
pptFiles <- list.files(path = PRISM.path, pattern = glob2rx("*ppt*.bil"), full.names = TRUE)
pptFiles <- pptFiles[1:1272] # (hack to deal with CRS incompatibility, vpd .bil file Nov, 2016)
#tmpFiles <- list.files(path = PRISM.path, pattern = glob2rx("*tmean*.bil"), full.names = TRUE)
#vpdFiles <- list.files(path = PRISM.path, pattern = glob2rx("*vpdmin*.bil"), full.names = TRUE)
colNames <- lapply(strsplit(pptFiles, "4kmM._"), function (x) x[2])
colNames <- unlist(colNames)
colNames <- lapply(strsplit(colNames, "_"), function (x) x[1])
colNames <- unlist(colNames)
colnames(ppt.extr) <- paste0("ppt_", colNames)
colnames(tmin.extr) <- paste0("tmin_", colNames)
colnames(tmax.extr) <- paste0("tmax_", colNames)

# Export climate data
processed.path <- "./data/formatted/"
write.csv(ppt.extr, paste0(processed.path,"ppt_extr.csv"), row.names = F)
write.csv(tmin.extr, paste0(processed.path,"tmin_extr.csv"), row.names = F)
write.csv(tmax.extr, paste0(processed.path,"tmax_extr.csv"), row.names = F)


# Forecast ----

#what to do with non focal trees
#interpolate
#change in DBH per year = (fDBH-DBH/census interval)

#while loop
#seq(MEASYEAR:fMEASYEAR, by 1)
#while year is <= fMEASYEAR


#function
#insert model object, focal trees, nonfocal trees, climate (default null)
## with non focal trees
### interpolate dbh
#require tidyverse?


## for a given year focal trees has
### DBH, CR, BAL, CCF, SI, SLOPE, rad
### DBH needs to be added continuously
### CR_weib, BAL, CCF need to be recalculated each year
### climate is pulled down (PRISM)
### SI, SLOPE, radiation copy




#for loop? 
#Every row is a tree
#