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

library(dbplyr)
library(RSQLite)
UT_FIA <- DBI::dbConnect(RSQLite::SQLite(), "./data/raw/FIADB.db")
TREE <- tbl(UT_FIA, sql("SELECT CN, PLT_CN, PLOT, COUNTYCD, SUBP, TREE, SPCD, STATUSCD, PREV_TRE_CN, DIA, CR, TPA_UNADJ, INVYR FROM TREE")) %>%
  collect()
PLOT <- tbl(UT_FIA, sql("SELECT CN, PREV_PLT_CN, MEASYEAR, DESIGNCD_P2A FROM PLOT")) %>%
  collect()
colnames(PLOT)[colnames(PLOT)=="CN"] <- "PLT_CN"
COND <- tbl(UT_FIA, sql("SELECT PLT_CN, SICOND, SIBASE, SISP FROM COND")) %>%
  collect()

plt_tre <- left_join(TREE,PLOT)


# Get trees
# tree: "TRE_CN","PLT_CN","SUBP","PREV_TRE_CN","DIA","UNCRCD","SITREE","TPA_UNADJ"
##also grab previous tree CN?
# cond: CONDID, SLOPE, ASPECT, SDIMAX_RMRS, SICOND, BALIVE, DSTRBCD1
# plot: LAT, LON, ELEV, MEASYEAR, DESIGNCD, SUBP_EXAMINE_CD, PREV_MEASYEAR

#select relevant tree table attributes
val_dset <- tree %>%
  select(CN,PLT_CN,STATUSCD,SPCD,SUBP,PREV_TRE_CN,DIA,UNCRCD,TPA_UNADJ)
val_dset$CR <- TREE$CR[match(val_dset$TRE_CN,TREE$CN)]
#get DIA at remeasurement
val_dset$fDIA <- tree$DIA[match(val_dset$CN,tree$PREV_TRE_CN)]
#get status (dead or alive) of trees at remeasurement
val_dset$fSTATUSCD <- tree$STATUSCD[match(val_dset$CN,tree$PREV_TRE_CN)]
#get future crown ratio to interpolate later
val_dset$fCR <- TREE$UNCRCD[match(val_dset$TRE_CN,TREE$PREV_TRE_CN)]
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
val_sicheck <- val_dset %>%
  select(PLT_CN,SUBP,TRE_CN,SPCD,SICOND)
cond_si <- cond %>%
  select(PLT_CN,SICOND,SISP,SIBASE) %>%
  filter(PLT_CN %in% plt_val) #102
length(unique(cond_si$PLT_CN)) #85
val_sicheck <- left_join(val_sicheck,cond_si)
library(dbplyr)
library(RSQLite)
UT_FIA <- DBI::dbConnect(RSQLite::SQLite(), "./data/raw/FS_FIADB_STATECD_49.db")
SITREE <- tbl(UT_FIA, sql("SELECT PLT_CN, SUBP, SPCD, SITREE FROM SITETREE")) %>%
  collect()
#data_all$SITREE <- SITREE$SITREE[match(data_all$TRE_CN, SITREE$CN)]
SITREE$PLT_CN <- as.numeric(SITREE$PLT_CN)
val_si_test <- left_join(val_sicheck, SITREE, by = c("PLT_CN","SPCD")) %>%
  distinct()
#get rid of trees where sitesp is the same as spcd
val_si_plt <- val_si_test %>%
  filter(SPCD != SISP)
#calculate average site index per species per plot
val_si_plt <- val_si_plt %>%
  dplyr::select(PLT_CN,SPCD,SITREE) %>%
  group_by(PLT_CN,SPCD) %>%
  summarise(mean = mean(SITREE,na.rm=TRUE))
#replace in validation dataset
val_dset <- val_dset %>%
  mutate(SICOND_c = ifelse(SPCD == SISP,
                           SICOND,
                           val_si_plt$mean[val_si_plt$PLT_CN == PLT_CN &
                                               val_si_plt$SPCD == SPCD])) %>%
  filter(!is.na(SICOND_c))

colnames(val_dset)[colnames(val_dset)=="CN"] <- "TRE_CN"

val_dset$CONDID <- cond$CONDID[match(val_dset$PLT_CN, cond$PLT_CN)]
val_dset$SISP <- cond$SISP[match(val_dset$PLT_CN, cond$PLT_CN)]
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

#filter no disturbance at start of projection cycle
#TODO filter      no disturbance at end?
val_dset <- val_dset %>%
  filter(DSTRBCD1 == 0) %>%
  filter(!is.na(UNCRCD)) %>%
  filter(!is.na(SICOND))

# explore?
unique(val_dset$CONDID)
# 1
val_dset$tCONDID <- tree$CONDID[match(val_dset$TRE_CN, tree$CN)]
unique(val_dset$tCONDID)
# 1 2
#status code
cond_red <- cond %>%
  select(PLT_CN,CONDID,COND_STATUS_CD,DSTRBCD1,DSTRBCD2,TRTCD1,TRTCD2) %>%
  filter(PLT_CN %in% plt_val) %>%
  distinct()

library(dbplyr)
library(RSQLite)
  
UT_FIA <- DBI::dbConnect(RSQLite::SQLite(), "./data/raw/FS_FIADB_STATECD_49.db")
subplt <- tbl(UT_FIA, sql("SELECT PLT_CN, SUBP, SUBP_STATUS_CD, MICRCOND, SUBPCOND, MACRCOND FROM SUBPLOT")) %>%
  collect()
subplt$PLT_CN <- as.numeric(subplt$PLT_CN)
cond_red <- left_join(cond_red,subplt, by = "PLT_CN")
sub_cond <- tbl(UT_FIA, sql("SELECT PLT_CN, SUBP, CONDID, MICRCOND_PROP, SUBPCOND_PROP,
                            MACRCOND_PROP, NONFR_INCL_PCT_SUBP, NONFR_INCL_PCT_MACRO FROM SUBP_COND")) %>%
  collect()
colnames(sub_cond)[colnames(sub_cond)=="CONDID"] <- "CONDID2"
sub_cond$PLT_CN <- as.numeric(sub_cond$PLT_CN)
cond_red <- left_join(cond_red,sub_cond, by = c("PLT_CN","SUBP"))

condid1 <- cond_red$PLT_CN[cond_red$CONDID!=1]

tre_cond <- val_dset %>%
  select(PLT_CN,TRE_CN,SUBP,CONDID,tCONDID) %>%
  filter(tCONDID != 1)
tre_cd_jn <- cond_red %>%
  select(PLT_CN,SUBP,SUBP_STATUS_CD,MICRCOND,MICRCOND_PROP,SUBPCOND,SUBPCOND_PROP)
tre_cond2 <- left_join(tre_cond,tre_cd_jn, by = c("PLT_CN","SUBP"))

plt_red <- unique(tre_cond$PLT_CN)
plt_cond <- tree[(tree$PLT_CN %in% plt_red),c("CN","PLT_CN","SUBP","SPCD","TPA_UNADJ","STATUSCD","CONDID")]
plt_cond$COND_STATUS_CD <- cond$COND_STATUS_CD[match(plt_cond$PLT_CN, cond$PLT_CN)]
colnames(plt_cond)[colnames(plt_cond)=="CN"] <- "TRE_CN"
plt_cond2 <- left_join(plt_cond,tre_cd_jn, by = c("PLT_CN","SUBP")) %>%
  distinct()

plt_cond_ex <- plt_cond2 %>%
  filter(PLT_CN == plt_red[1])
write.csv(plt_cond_ex,file = "./data/formatted/plt_cond_ex.csv")

#filter for condid = 1 on a plot
val_dset <- val_dset %>%
  filter(!PLT_CN %in% condid1)

# Calculate census interval

#all available trees
save(val_dset,file = "./data/formatted/val_dset.Rdata")

# Val trees ----
#choose based on years of growth? climate conditions
UT_clim_an <- read.csv("./data/raw/climate/UTanPRISM_00_18.csv",header =T)
#randomize  

#multiple years of measurement?
TREE <- tbl(UT_FIA, sql("SELECT CN, PLT_CN, PLOT, SUBP, TREE, SPCD, STATUSCD, PREV_TRE_CN, DIA, CR, TPA_UNADJ FROM TREE")) %>%
  collect()
PLOT <- tbl(UT_FIA, sql("SELECT * FROM PLOT")) %>%
  collect()
val_ex <- TREE %>%
  filter(CN %in% PREV_TRE_CN &
           SPCD %in% c(93,122,202))
val_ex$MEASYEAR <- PLOT$MEASYEAR[match(val_ex$PLT_CN, PLOT$CN)]

# Density ----
#for current year
#fetch trees from same plot in FIADB
#with associated covariate data (above)
#will include other species

#covariates
#density
#ccf - calculate
#bal - 
#cr - given

#get trees from same plot
plot_val <- unique(val_dset$PLT_CN)
tree_val <- unique(val_dset$TRE_CN)
density_val <- TREE[(TREE$PLT_CN %in% plot_val),]
#make sure trees are on the same plot b/c calculating stand variables
#make sure I'm not including validation trees
colnames(density_val)[colnames(density_val)=="CN"] <- "TRE_CN"

#remove trees that are dead
density_val <- density_val %>%
  filter(STATUSCD == 1)

#get DIA at remeasurement
density_val$fDIA <- tree$DIA[match(density_val$TRE_CN,tree$PREV_TRE_CN)]

density_val$fSTATUSCD <- tree$STATUSCD[match(density_val$TRE_CN,tree$PREV_TRE_CN)]

density_val$MEASYEAR <- plot$MEASYEAR[match(density_val$PLT_CN, plot$CN)]
density_val$fMEASYEAR <- plot$MEASYEAR[match(density_val$PLT_CN, plot$PREV_PLT_CN)]
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
#on a subplot
density_val <- density_val %>%
  group_by(PLT_CN,SUBP) %>%
  #min assigns lowest value to ties (1,2,3,3,5,6)
  mutate(BAL_s = map_dbl(DIA,~sum(BA_pa[DIA>.x],na.rm = TRUE)))
#scaled by subplot number
density_val <- density_val %>%
  group_by(PLT_CN,SUBP) %>%
  #min assigns lowest value to ties (1,2,3,3,5,6)
  mutate(BAL_sc = BAL_s * 4)


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

# Current FVS ----
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

#from FVS online

fvs_treelist <- read_csv(file = "./data/raw/FVS/fvs_runs.csv")
length(unique(fvs_treelist$StandID)) #63
fvs_red <- fvs_treelist %>%
  select(StandID,Year,PrdLen,TreeId,SpeciesFIA,DBH,DG,PctCr,PtBAL) %>%
  filter(SpeciesFIA %in% c(93,122,202)) %>%
  filter(DBH >= 1)
length(unique(fvs_red$StandID)) #40

library(RSQLite)
UT_fvsred_db <- dbConnect(RSQLite::SQLite(), "./New_Utah/FVS_Data.db")
#Extract FVS_StandInit_Plot table
fvsStandInitPlot<-dbReadTable(UT_fvsred_db, 'FVS_STANDINIT_PLOT');head(fvsStandInitPlot)
std2plt <- fvsStandInitPlot %>%
  select(STAND_CN,STAND_ID)
length(unique(std2plt$STAND_ID)) #63
length(unique(std2plt$STAND_CN)) #63

fvs_red$PLT_CN <- std2plt$STAND_CN[match(fvs_red$StandID,std2plt$STAND_ID)]
length(unique(fvs_red$StandID)) #40
length(unique(fvs_red$PLT_CN)) #40

length(unique(fvs_red$StandID))
#43
#should be 63 - what is up?
fvs_stid <- read.csv(file = "./data/raw/FVS/StandID.csv", header = T)
head(fvs_stid)
length(unique(fvs_stid$StandID)) #43
stid_red <- fvs_stid %>%
  filter(!(StandID %in% fvs_red$StandID))



# Climate ----
#Precipitation
#Tmax
#for only large trees

#new trees w/ LAT & LON
val_trees <- val_dset %>%
  select(TRE_CN,LON,LAT)

library(raster)
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

  # Jan 1999 - 
# Remove data after Oct, 2016 (because of different CRS Nov, 2016 vpdmax .bil)
# note that the work-around for this problem is to assign the CRS of another layer to Nov and Dec of 2016
# crs(vpdNov2016_raster) <- crs(vpdOct2016_raster)
ppt.extr <- ppt.extr[, 1249:1476] 
tmin.extr <- tmin.extr[, 1249:1476]
tmax.extr <- tmax.extr[, 1249:1476]

# Add sensible column names for raster::extracted climate data
ppt.extr <- as.data.frame(ppt.extr)
tmin.extr <- as.data.frame(tmin.extr)
tmax.extr <- as.data.frame(tmax.extr)
PRISM.path <-  "./data/raw/climate/PRISM/"
pptFiles <- list.files(path = PRISM.path, pattern = glob2rx("*ppt*.bil"), full.names = TRUE)
pptFiles <- pptFiles[1249:1476] # (hack to deal with CRS incompatibility, vpd .bil file Nov, 2016)
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
write.csv(ppt.extr, paste0(processed.path,"ppt_val_extr.csv"), row.names = F)
write.csv(tmin.extr, paste0(processed.path,"tmin_val_extr.csv"), row.names = F)
write.csv(tmax.extr, paste0(processed.path,"tmax_val_extr.csv"), row.names = F)

#already have some climate information
processed.path <- "./data/formatted/"
ppt.extr <- read.csv(paste(processed.path,"ppt_val_extr.csv",sep=''), header = T)
tmin.extr <- read.csv(paste(processed.path,"tmin_val_extr.csv",sep=''), header = T)
tmax.extr <- read.csv(paste(processed.path,"tmax_val_extr.csv",sep=''), header = T)

#for all trees
#same dataset LAT/LONG used to extract climate
val_tre <- unique(val_trees$TRE_CN)

clim_col <- function(PRISM,clim_var,TRE_CN){
  #make a list
  #each item is a year
  #each item has 1-13 columns (TRE_CN + 12 months)
  
  #ppt
  #empty list
  climate_list <- list()
  start_col <- 1 #Jan
  end_col <- 12 #Dec
  n <- ncol(PRISM)/12
  for(i in 1:n){ #number of years (1895:2000)
    climate_list[[i]] <- PRISM[,start_col:end_col]
    start_col <- start_col + 12
    end_col <- end_col + 12
  }
  
  prism_id <- colnames(ppt.extr)
  
  months <- c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")
  #clim_var <- str_sub(prism_id,1,4) #1:5 for tmin_ and tmax_
  climate_list <- lapply(climate_list, setNames, nm = paste0(clim_var,months))
  
  #years <- seq(from=start_yr,to=end_yr,by=1)
  prism_yr <- unique(sub('\\D*(\\d{4}).*', '\\1', prism_id))
  names(climate_list) <- as.integer(prism_yr)
  
  climate_stack <- bind_rows(climate_list, .id = "Year")
  climate_stack$TRE_CN <- rep(TRE_CN,times=n)
  return(climate_stack)
}
#could make function for all climate variables
#find way to extract start_yr and end_yr
#lubridate?

val_ppt <- clim_col(ppt.extr,clim_var = "ppt_",TRE_CN = val_tre)
val_tmin <- clim_col(tmin.extr,clim_var = "tmin_",TRE_CN = val_tre)
val_tmax <- clim_col(tmax.extr,clim_var = "tmax_",TRE_CN = val_tre)

val_clim <- full_join(val_ppt,val_tmin, by = c("TRE_CN","Year")) %>%
  full_join(.,val_tmax,by = c("TRE_CN","Year"))

val_clim$Year <- as.integer(val_clim$Year)

#seasonal calculations
val_clim <- val_clim %>%
  group_by(TRE_CN) %>%
  arrange(Year) %>%
  mutate(ppt_pJunSep = lag(ppt_Jun) + lag(ppt_Jul) + lag(ppt_Aug) + lag(ppt_Sep) + lag(ppt_Oct) +
           lag(ppt_Nov)+ lag(ppt_Dec)+ ppt_Jan+ ppt_Feb + ppt_Mar + ppt_Apr +
           ppt_May + ppt_Jun + ppt_Jul + ppt_Aug + ppt_Sep,
         tmax_FebJul = (tmax_Feb + tmax_Mar + tmax_Apr + tmax_May + tmax_Jun + tmax_Jul)/6, #temp parameter for DF
         tmax_JunAug = (tmax_Jun + tmax_Jul + tmax_Aug)/3, #temp parameter for PP
         tmax_pAug = lag(tmax_Aug)) #temp parameter for ES
save(val_clim,file = "./data/formatted/val_clim.Rdata")

#get climate-fvs ready data
#https://www.fs.fed.us/nrm/fsveg/index.shtml
clim_fvs <- val_dset %>%
  ungroup() %>%
  select(TRE_CN,LAT,LON,ELEV) %>%
  rename_at("ELEV",~"ELE")

write.table(clim_fvs, file = "./data/formatted/clim_fvs.txt", sep = " ",
            row.names = FALSE)

# Forecast ----
library(tidyverse)

#first function for nonfocal trees
tree_val <- unique(val_dset$TRE_CN) #905
non_focal <- density_val %>%
  dplyr::select(PLT_CN,TRE_CN,SUBP,SPCD,DIA,TPA_UNADJ,DESIGNCD,MEASYEAR,fMEASYEAR,fDIA,STATUSCD,fSTATUSCD) %>%
  filter(!(TRE_CN %in% tree_val)) %>% #3707
  filter(STATUSCD == 1) %>% #filter for live trees at start
  filter(fSTATUSCD != 0) %>%
  ungroup()
#check
#905+3707
#[1] 4612
  
#add mortality year (estimated)
non_focal$fMORTYR <- tree$MORTYR[match(non_focal$TRE_CN,tree$PREV_TRE_CN)]
non_focal$CONDID <- tree$CONDID[match(non_focal$TRE_CN,tree$CN)]

unique(non_focal$CONDID)
#[1] 1 2 3

#explore
non_focal_stat <- non_focal %>%
  dplyr::select(STATUSCD,fSTATUSCD) %>%
  group_by(STATUSCD,fSTATUSCD) %>%
  tally()
# STATUSCD fSTATUSCD
#       1         0   #40 #? 0 = no status; tree is not presently in sample
#       1         1   #2415; keep
#       1         2   #603; 2 = dead
#       1         3   #8;  3 = cut
#       2         0   #10
#       2         2   #622
#       2         NA  #9

#filter trees that died or were cut
#do they have a year of death? MORTYR?
non_focal_mort <- non_focal %>%
  filter(fSTATUSCD %in% c(2,3))


#change in BAL
non_foc_bal <- non_focal %>%
  group_by(PLT_CN) %>%
  mutate(BA1 = sum(((DIA^2) * 0.005454) * TPA_UNADJ),
         BA2 = sum(((fDIA[fSTATUSCD == 1]^2) * 0.005454) * TPA_UNADJ))

non_foc_hist <- non_foc_bal %>%
  select(PLT_CN, BA1, BA2) %>%
  distinct() %>%
  mutate(BA_diff = BA2 - BA1,
         BA_ratio = BA2/BA1)

hist(non_foc_hist$BA_diff,breaks = 50, xlab = "BA2 - BA1", main = "Difference in basal area")
hist(non_foc_hist$BA_ratio,breaks = 50, xlab = "BA2 / BA1", main = "Ratio of basal area")


#interpolate dbh
#change in DBH per year = (fDBH-DBH/census interval)
int_tre <- function(data){
  growth_df <- data %>%
    filter(!is.na(av_growth))
  for(i in 1:nrow(data)){
    Species <- data$SPCD[i]
    data$growth_adj[i] <- ifelse(is.na(data$av_growth[i]),
                                 round(mean(growth_df$av_growth[growth_df$SPCD == Species]),digits = 5),
                                 data$av_growth[i])
    tre_df <- data[i,] %>%
      slice(rep(1:n(), each = (data$census_int[i] +1))) %>%
      mutate(Year =  ifelse(is.na(fMORTYR),
                            c(MEASYEAR[1]:fMEASYEAR[1]),
                              c(MEASYEAR[1]:fMORTYR[1])),
             DIA_int = ifelse(DIA == fDIA,
                              DIA,
                              NA))
    N <- which(tre_df$Year == tre_df$MEASYEAR[1])
    tre_df$DIA_int[N] <- tre_df$DIA[N] #dbh when year and measure year are equal
    Curr_row <- N+1 #each time through subtract 1 and move down one row
    while(Curr_row <= (data$census_int[i] +1)){
      DIA_1 <- tre_df$DIA_int[Curr_row-1]
      tre_df$DIA_int[Curr_row] <- DIA_1 + tre_df$growth_adj[Curr_row]
      #continue loop for next row until curr_row>0
      Curr_row = Curr_row + 1 
    }
    data <- full_join(data,tre_df)
  }
  return(data)
}

non_focal_test <- non_focal %>%
  mutate(census_int = ifelse(is.na(fMORTYR),
                              fMEASYEAR - MEASYEAR,
                              fMORTYR - MEASYEAR),
         av_growth = (fDIA-DIA)/census_int) %>%
  # some will be NA where fDIA is NA and some will be negative where fDIA<DIA
  int_tre(.)

non_focal_test <- non_focal_test %>%
  filter(!is.na(Year)) %>%
  filter(!is.infinite(growth_adj))

#function
#insert model object, focal trees, nonfocal trees, climate (default null)
#N is the row where measure year and ring width year are the same

#standardize new data variables
# get mean and standard deviation from training set (tree ring data set) for each variable
#variables: SICOND, tASPECT, SLOPE, BAL, SDI, CR_weib, PCCF, CCF, cos, sin, solrad_MayAug
# precip: ppt_pJunSep
# temp: different for each species
get_stats <- function(x) c(mean=mean(x), sd=sd(x))
df_stats <- glmm_df_z %>%
  ungroup() %>%
  dplyr::select(DIA_C,SICOND,tASPECT,SLOPE,BAL,SDI,CR_weib,PCCF,CCF,
         cos,sin,solrad_MayAug,ppt_pJunSep,tmax_FebJul) %>%
  sapply(.,function(x) c(mean=mean(x,na.rm = T),
                         sd=sd(x,na.rm = T)))
pp_stats <- glmm_pp_z %>%
  ungroup() %>%
  dplyr::select(DIA_C,SICOND,tASPECT,SLOPE,BAL,SDI,CR_weib,PCCF,CCF,
         cos,sin,solrad_MayAug,ppt_pJunSep,tmax_JunAug) %>%
  sapply(.,function(x) c(mean=mean(x,na.rm = T),
                         sd=sd(x,na.rm = T)))
es_stats <- glmm_es_z %>%
  ungroup() %>%
  dplyr::select(DIA_C,SICOND,tASPECT,SLOPE,BAL,SDI,CR_weib,PCCF,CCF,
         cos,sin,solrad_MayAug,ppt_pJunSep,tmax_pAug) %>%
  sapply(.,function(x) c(mean=mean(x,na.rm = T),
                         sd=sd(x,na.rm = T)))
sp_stats <- list(sp_202 = df_stats, sp_122 = pp_stats,sp_93 = es_stats)
#use to standardize newdata covariates

val_an <- function(newdata,mod_df,mod_pp,mod_es,sp_stats,nonfocal,bratio,ccf_df,CR_WEIB_df) {
  #parameters:
  #newdata - validataion trees from FIADB
  newdata_rep <- newdata %>%
    ungroup() %>%
    dplyr::select(PLT_CN, TRE_CN, SPCD, SUBP, MEASYEAR, 
                  TPA_UNADJ, DESIGNCD, tCONDID, census_int,
                  ASPECT,SLOPE,sin,cos,LAT,LON,ELEV,
                  FVS_LOC_CD,SDImax,SICOND_c,solrad_MayAug, fMEASYEAR, fDIA) %>%
    mutate(Year = NA,
           DIA = NA,
           CCF = NA,
           PCCF = NA,
           BAL = NA,
           SDI = NA,
           CR_weib = NA)
  newdata_rep <- newdata_rep %>% 
    group_by(TRE_CN) %>%
    slice(rep(1:n(), each = census_int+1)) %>%
    mutate(Year = (MEASYEAR[1]:fMEASYEAR[1])) %>% #repeated data frame ready  to fill in
    ungroup()
  ##density for new data in MEASYEAR will already be calculated
  #fill in DIA,CCF, PCCF, BAL, SDI, CR_weib, where year = measyear
  for(i in 1:nrow(newdata_rep)){
    TRE_CN <- newdata_rep$TRE_CN[i]
    if(newdata_rep$Year[i] == newdata_rep$MEASYEAR[i]){
      newdata_rep$DIA[i] <- newdata$DIA[newdata$TRE_CN == TRE_CN]
      newdata_rep$CCF[i] <- newdata$CCF[newdata$TRE_CN == TRE_CN]
      newdata_rep$PCCF[i] <- newdata$PCCF[newdata$TRE_CN == TRE_CN]
      newdata_rep$BAL[i] <- newdata$BAL[newdata$TRE_CN == TRE_CN]
      newdata_rep$SDI[i] <- newdata$SDI[newdata$TRE_CN == TRE_CN]
      newdata_rep$CR_weib[i] <- newdata$CR_weib[newdata$TRE_CN == TRE_CN]
    }
  }
  #mod_obj - list of model objects for each species
  #sp_stats - list of species statistics (mean and standard deviation) for each covariate
  ##will be used to standardize newdata covariates
  #nonfocal - trees on the same plot as validation trees
  nonfocal <- nonfocal %>%
    ungroup()
  ##will be used to compute density parameters
  #bratio - dataframe of bark ratio constants for equation
  #ccf - dataframe of crown competiton factor constants for equation
  #CR_weib_df - dataframe of crown ratio constants for equation
  
  #empty dataframe to add results
  pred_df <- newdata_rep[FALSE,]
  
  # loop over plot
  # to calculate density after MEASYR
  for(i in unique(newdata_rep$PLT_CN)) {
    #create plot dataframe to predict growth
    plt_df <- newdata_rep[newdata_rep$PLT_CN == i,]
    #assign projection start year
    growthyr <- plt_df$MEASYEAR[1] # assumes all trees on a plot have same MEASYEAR
    while (growthyr <= (plt_df$fMEASYEAR[1]-1)){ #stops the year before fMEASYEAR
      #filter dataframe for year of projection
      plt_yr_df <- plt_df %>%
        filter(Year == growthyr)
      
      for(i in 1:nrow(plt_yr_df)) {#for each tree on plot in given year
        Species <- plt_yr_df$SPCD[i]
        TRE_CN <- plt_yr_df$TRE_CN[i]
        
        #select model to predict growth
        #models are species specific
        if(Species == 202){mod_obj <- mod_df} 
        if(Species == 122){mod_obj <- mod_pp} 
        if(Species == 93){mod_obj <- mod_es} 
        
        #standardize covariates
        #get mean/sd for each species
        if(Species == 202){para_std <- sp_stats$sp_202}
        if(Species == 122){para_std <- sp_stats$sp_122}
        if(Species == 93) {para_std <- sp_stats$sp_93}
        
        #first row is mean, second row is sd
        #the inputs of the equation are from a list so output needs to be unlisted
        plt_yr_df$z.DIA_C[i] = unlist((plt_yr_df[i,"DIA"] - para_std[1,"DIA_C"]) / para_std[2,"DIA_C"])
        plt_yr_df$z.CR_weib[i] = unlist((plt_yr_df[i,"CR_weib"] - para_std[1,"CR_weib"]) / para_std[2,"CR_weib"])
        plt_yr_df$z.BAL[i] = unlist((plt_yr_df[i,"BAL"] - para_std[1,"BAL"]) / para_std[2,"BAL"])
        plt_yr_df$z.CCF[i] = unlist((plt_yr_df[i,"CCF"] - para_std[1,"CCF"]) / para_std[2,"CCF"])
        plt_yr_df$z.SDI[i] = unlist((plt_yr_df[i,"SDI"] - para_std[1,"SDI"]) / para_std[2,"SDI"])
        plt_yr_df$z.SICOND[i] = unlist((plt_yr_df[i,"SICOND_c"] - para_std[1,"SICOND"]) / para_std[2,"SICOND"])
        plt_yr_df$z.SLOPE[i] = unlist((plt_yr_df[i,"SLOPE"] - para_std[1,"SLOPE"]) / para_std[2,"SLOPE"])
        plt_yr_df$z.sin[i] = unlist((plt_yr_df[i,"sin"] - para_std[1,"sin"]) / para_std[2,"sin"])
        plt_yr_df$z.cos[i] = unlist((plt_yr_df[i,"cos"] - para_std[1,"cos"]) / para_std[2,"cos"])
        plt_yr_df$z.solrad_MayAug[i] = unlist((plt_yr_df[i,"solrad_MayAug"] - para_std[1,"solrad_MayAug"]) / para_std[2,"solrad_MayAug"])
        #TODO there must be a better way to do this? for loop?
        
        #predict
        # modobj will change with different species
        plt_yr_df$log_dds[i] <- predict(object = mod_obj, newdata = plt_yr_df[i,], 
                                        re.form = NA, type = "response")
        #re.form specify random effects to include
        ##NA include none
        
        #see page 53 of prognosis model
        #DG = sqrt((DBH/k)^2 + dds) - (DBH/k)
        # k = 1/BRATIO
        #see UT variant guide
        #BRATIO = b1+b2/(DBH^exp)
        b1 <- bratio$b1[bratio$species == Species]
        b2 <- bratio$b2[bratio$species == Species]
        exp <- bratio$exp[bratio$species == Species]
        BRATIO <- b1 + b2/(plt_yr_df$DIA[i]^exp) 
        #exp determines if use equation 4.2.1/3 or 4.2.2 in UT variant guide
        k <- 1/BRATIO 
        #DG = sqrt((DBH/k)^2 + dds - (DBH/k))
        plt_yr_df$DG[i] <- k * (sqrt((plt_yr_df$DIA[i]/k)^2 + exp(plt_yr_df$log_dds[i])) -
                                  (plt_yr_df$DIA[i]/k))
      }
      
      #so DBH0 + k*DG = DBH
      plt_yr2_df <- plt_df %>%
        filter(Year == (growthyr+1))%>%
        mutate(DIA = plt_yr_df$DIA + plt_yr_df$DG)
      
      #join with nonfocal
      #fitler for trees on plots in validation set in same year
      non_foc_filt <- nonfocal %>%
        dplyr::select(PLT_CN, TRE_CN, SUBP, SPCD,TPA_UNADJ,Year,DIA_int) %>%
        mutate(DIA = DIA_int) %>%
        filter(PLT_CN == plt_yr2_df$PLT_CN[1],
               Year == plt_yr2_df$Year[1])
      #join
      density_cal <- bind_rows(plt_yr2_df,non_foc_filt)
      
      #compute density parameters
      #ccf
      for(i in 1:nrow(density_cal)){
        Species <- density_cal$SPCD[i]
        r1 <- ccf_df$r1[ccf_df$species == Species]
        r2 <- ccf_df$r2[ccf_df$species == Species]
        r3 <- ccf_df$r3[ccf_df$species == Species]
        r4 <- ccf_df$r4[ccf_df$species == Species]
        r5 <- ccf_df$r5[ccf_df$species == Species]
        dbrk <- ccf_df$dbrk[ccf_df$species == Species]
        if(Species == 475){
          ifelse(density_cal$DIA[i] < dbrk,
                 CCF_t <- density_cal$DIA[i]*(r1+r2+r3),
                 CCF_t <- r1 + (r2 * density_cal$DIA[i]) + (r3 * density_cal$DIA[i]^2))
        }
        if(Species != 475){
          ifelse(is.na(density_cal$DIA[i]), 
                 CCF_t <- NA,
                 ifelse(density_cal$DIA[i] <= 0.1, 
                        CCF_t <- 0.0001,
                        ifelse(density_cal$DIA[i] < dbrk, 
                               CCF_t <- r4 * (density_cal$DIA[i]^r5),
                               CCF_t <- r1 + (r2 * density_cal$DIA[i]) + (r3 * density_cal$DIA[i]^2))))
        }
        density_cal$CCF_t[i] <- CCF_t
      }
      
      density_cal <- density_cal %>%
        group_by(PLT_CN,SUBP,Year) %>%
        mutate(PCCF = sum(CCF_t * (TPA_UNADJ * 4), na.rm = TRUE))
      
      #stand CCF = sum(CCFt on a plot) on a per acre basis
      #TPA measured on a plot/stand level
      density_cal <- density_cal %>%
        group_by(PLT_CN,Year) %>%
        mutate(CCF = sum(CCF_t * TPA_UNADJ,na.rm = TRUE))
      
      #bal
      density_cal <- density_cal %>%
        mutate(BA_pa = ((DIA^2) * 0.005454) * TPA_UNADJ)
      
      #rank trees per year per plot
      #sum BApa of trees larger on the same plot in same year
      density_cal <- density_cal %>%
        group_by(PLT_CN,Year) %>%
        mutate(rank_pltyr = rank(DIA, na.last = TRUE, ties.method = "min")) %>%
        #min assigns lowest value to ties (1,2,3,3,5,6)
        mutate(BAL = map_dbl(DIA,~sum(BA_pa[DIA>.x],na.rm = TRUE)))
      
      density_cal <- density_cal %>%
        group_by(PLT_CN,Year) %>%
        mutate(SDI = sum(TPA_UNADJ*(DIA/10)^1.6), #stage
               num_t = length(unique(TRE_CN)))
      
      #filter for focal trees
      plt_yr2_df <- density_cal %>%
        filter(TRE_CN %in% plt_yr_df$TRE_CN)
      
      #cr
      for(i in 1:nrow(plt_yr2_df)){
        #Function arguments:
        #SPCD - is number code of species of tree record
        #SDI - is SDI of stand (Stage 1968)
        Species <- plt_yr2_df$SPCD[i]
        #SDI max values for each species were pulled from UT Variant overview
        SDIMAX <- ifelse(is.na(plt_yr2_df$SDImax[i]),
                         CR_WEIB_df$SDIMAX[CR_WEIB_df$species == Species],
                         plt_yr2_df$SDImax[i])
        #Calculate relative density
        RD <- plt_yr2_df$SDI[i]/SDIMAX
        
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
        SCALE = (1.0 - .00167 * (plt_yr2_df$CCF[i]-100.0))
        if(SCALE < 0.3){SCALE = 0.3}
        if(SCALE > 1.0){SCALE = 1.0}
        
        N <- plt_yr2_df$num_t[i]
        #X is tree's rank in diameter distribution
        #Multiply tree's rank in diameter distribution (trees position relative to tree with largest diameter in the stand) by scale parameter
        Y <- plt_yr2_df$rank_pltyr[i]/N * SCALE
        if(Y < 0.05){Y = 0.05}
        if(Y > 0.95){Y = 0.95}
        #Constrain Y between 0.05 and 0.95 - crown ratio predictions in FVS are bound between these two values
        
        #Calculate crown ratio (this corresponds to variable X in UTAH variant overview)
        X <- WEIBA + WEIBB*((-1*log(1-Y))^(1/WEIBC))
        #X = a tree’s crown ratio expressed as a percent / 10
        plt_yr2_df$CR_weib[i] <- X * 10
      }
      
      #join with plt_df
      for(i in 1:nrow(plt_df)){
        TRE_CN <- plt_df$TRE_CN[i]
        if(plt_df$Year[i] == (growthyr + 1)){
          plt_df$DIA[i] <- plt_yr2_df$DIA[plt_yr2_df$TRE_CN == TRE_CN]
          plt_df$CCF[i] <- plt_yr2_df$CCF[plt_yr2_df$TRE_CN == TRE_CN]
          plt_df$PCCF[i] <- plt_yr2_df$PCCF[plt_yr2_df$TRE_CN == TRE_CN]
          plt_df$BAL[i] <- plt_yr2_df$BAL[plt_yr2_df$TRE_CN == TRE_CN]
          plt_df$SDI[i] <- plt_yr2_df$SDI[plt_yr2_df$TRE_CN == TRE_CN]
          plt_df$CR_weib[i] <- plt_yr2_df$CR_weib[plt_yr2_df$TRE_CN == TRE_CN]
        }
      }
      #add density metrics in
      
      
      #loop over next year
      growthyr = growthyr + 1
    }
    #join plt_df with empty data frame
    #output
    pred_df <- bind_rows(pred_df,plt_df)
    
    #loop over next plot
  }
  
  return(pred_df)
}


val_clim <- function(newdata,mod_df,mod_pp,mod_es,sp_stats,nonfocal,bratio,ccf_df,CR_WEIB_df,climate) {
  #parameters:
  #newdata - validataion trees from FIADB
  newdata_rep <- newdata %>%
    ungroup() %>%
    dplyr::select(PLT_CN, TRE_CN, SPCD, SUBP, MEASYEAR, 
                  TPA_UNADJ, DESIGNCD, tCONDID, census_int,
                  ASPECT,SLOPE,sin,cos,LAT,LON,ELEV,
                  FVS_LOC_CD,SDImax,SICOND_c,solrad_MayAug, fMEASYEAR, fDIA) %>%
    mutate(Year = NA,
           DIA = NA,
           CCF = NA,
           PCCF = NA,
           BAL = NA,
           SDI = NA,
           CR_weib = NA)
  newdata_rep <- newdata_rep %>% 
    group_by(TRE_CN) %>%
    slice(rep(1:n(), each = census_int+1)) %>%
    mutate(Year = (MEASYEAR[1]:fMEASYEAR[1])) %>% #repeated data frame ready  to fill in
    ungroup()
  ##density for new data in MEASYEAR will already be calculated
  #fill in DIA,CCF, PCCF, BAL, SDI, CR_weib, where year = measyear
  for(i in 1:nrow(newdata_rep)){
    TRE_CN <- newdata_rep$TRE_CN[i]
    if(newdata_rep$Year[i] == newdata_rep$MEASYEAR[i]){
      newdata_rep$DIA[i] <- newdata$DIA[newdata$TRE_CN == TRE_CN]
      newdata_rep$CCF[i] <- newdata$CCF[newdata$TRE_CN == TRE_CN]
      newdata_rep$PCCF[i] <- newdata$PCCF[newdata$TRE_CN == TRE_CN]
      newdata_rep$BAL[i] <- newdata$BAL[newdata$TRE_CN == TRE_CN]
      newdata_rep$SDI[i] <- newdata$SDI[newdata$TRE_CN == TRE_CN]
      newdata_rep$CR_weib[i] <- newdata$CR_weib[newdata$TRE_CN == TRE_CN]
    }
    #add climate
    #match over tree and year
    newdata_rep$ppt_pJunSep[i] <- climate$ppt_pJunSep[climate$TRE_CN == TRE_CN &
                                                        climate$Year == newdata_rep$Year[i]]
    newdata_rep$tmax_JunAug[i] <- climate$tmax_JunAug[climate$TRE_CN == TRE_CN &
                                                        climate$Year == newdata_rep$Year[i]]
    newdata_rep$tmax_FebJul[i] <- climate$tmax_FebJul[climate$TRE_CN == TRE_CN &
                                                        climate$Year == newdata_rep$Year[i]]
    newdata_rep$tmax_pAug[i] <- climate$tmax_pAug[climate$TRE_CN == TRE_CN &
                                                    climate$Year == newdata_rep$Year[i]]
  }
  
  #mod_obj - list of model objects for each species
  #sp_stats - list of species statistics (mean and standard deviation) for each covariate
  ##will be used to standardize newdata covariates
  #nonfocal - trees on the same plot as validation trees
  nonfocal <- nonfocal %>%
    ungroup()
  ##will be used to compute density parameters
  #bratio - dataframe of bark ratio constants for equation
  #ccf - dataframe of crown competiton factor constants for equation
  #CR_weib_df - dataframe of crown ratio constants for equation
  #climate - dataframe of climate variables (ppt & tmax) for each tree
  
  #empty dataframe to add results
  pred_df <- newdata_rep[FALSE,]
  
  # loop over plot
  # to calculate density after MEASYR
  for(i in unique(newdata_rep$PLT_CN)) {
    #create plot dataframe to predict growth
    plt_df <- newdata_rep[newdata_rep$PLT_CN == i,]
    #assign projection start year
    growthyr <- plt_df$MEASYEAR[1] # assumes all trees on a plot have same MEASYEAR
    while (growthyr <= (plt_df$fMEASYEAR[1]-1)){ #stops the year before fMEASYEAR
      #filter dataframe for year of projection
      plt_yr_df <- plt_df %>%
        filter(Year == growthyr)
      
      for(i in 1:nrow(plt_yr_df)) {#for each tree on plot in given year
        Species <- plt_yr_df$SPCD[i]
        TRE_CN <- plt_yr_df$TRE_CN[i]
        
        #select model to predict growth
        #models are species specific
        if(Species == 202){mod_obj <- mod_df} 
        if(Species == 122){mod_obj <- mod_pp} 
        if(Species == 93){mod_obj <- mod_es} 
        
        #standardize covariates
        #get mean/sd for each species
        if(Species == 202){para_std <- sp_stats$sp_202}
        if(Species == 122){para_std <- sp_stats$sp_122}
        if(Species == 93) {para_std <- sp_stats$sp_93}
        
        #first row is mean, second row is sd
        #the inputs of the equation are from a list so output needs to be unlisted
        plt_yr_df$z.DIA_C[i] = unlist((plt_yr_df[i,"DIA"] - para_std[1,"DIA_C"]) / para_std[2,"DIA_C"])
        plt_yr_df$z.CR_weib[i] = unlist((plt_yr_df[i,"CR_weib"] - para_std[1,"CR_weib"]) / para_std[2,"CR_weib"])
        plt_yr_df$z.BAL[i] = unlist((plt_yr_df[i,"BAL"] - para_std[1,"BAL"]) / para_std[2,"BAL"])
        plt_yr_df$z.CCF[i] = unlist((plt_yr_df[i,"CCF"] - para_std[1,"CCF"]) / para_std[2,"CCF"])
        plt_yr_df$z.SDI[i] = unlist((plt_yr_df[i,"SDI"] - para_std[1,"SDI"]) / para_std[2,"SDI"])
        plt_yr_df$z.SICOND[i] = unlist((plt_yr_df[i,"SICOND_c"] - para_std[1,"SICOND"]) / para_std[2,"SICOND"])
        plt_yr_df$z.SLOPE[i] = unlist((plt_yr_df[i,"SLOPE"] - para_std[1,"SLOPE"]) / para_std[2,"SLOPE"])
        plt_yr_df$z.sin[i] = unlist((plt_yr_df[i,"sin"] - para_std[1,"sin"]) / para_std[2,"sin"])
        plt_yr_df$z.cos[i] = unlist((plt_yr_df[i,"cos"] - para_std[1,"cos"]) / para_std[2,"cos"])
        plt_yr_df$z.solrad_MayAug[i] = unlist((plt_yr_df[i,"solrad_MayAug"] - para_std[1,"solrad_MayAug"]) / para_std[2,"solrad_MayAug"])
        plt_yr_df$z.ppt_pJunSep[i] = unlist((plt_yr_df[i,"ppt_pJunSep"] - 
                                               para_std[1,"ppt_pJunSep"]) / para_std[2,"ppt_pJunSep"])
        #temperature different for each species
        plt_yr_df$z.tmax_FebJul[i] = unlist((plt_yr_df[i,"tmax_FebJul"] -
                                               sp_stats$sp_202[1,"tmax_FebJul"]) /
                                              sp_stats$sp_202[2,"tmax_FebJul"])
        plt_yr_df$z.tmax_JunAug[i] = unlist((plt_yr_df[i,"tmax_JunAug"] -
                                               sp_stats$sp_122[1,"tmax_JunAug"]) /
                                              sp_stats$sp_122[2,"tmax_JunAug"])
        plt_yr_df$z.tmax_pAug[i] = unlist((plt_yr_df[i,"tmax_pAug"] -
                                             sp_stats$sp_93[1,"tmax_pAug"]) / 
                                            sp_stats$sp_93[2,"tmax_pAug"])
        #TODO there must be a better way to do this? for loop?
        
        #predict
        # modobj will change with different species
        plt_yr_df$log_dds[i] <- predict(object = mod_obj, newdata = plt_yr_df[i,], 
                                        re.form = NA, type = "response")
        #re.form specify random effects to include
        ##NA include none
        
        #see page 53 of prognosis model
        #DG = sqrt((DBH/k)^2 + dds) - (DBH/k)
        # k = 1/BRATIO
        #see UT variant guide
        #BRATIO = b1+b2/(DBH^exp)
        b1 <- bratio$b1[bratio$species == Species]
        b2 <- bratio$b2[bratio$species == Species]
        exp <- bratio$exp[bratio$species == Species]
        BRATIO <- b1 + b2/(plt_yr_df$DIA[i]^exp) 
        #exp determines if use equation 4.2.1/3 or 4.2.2 in UT variant guide
        k <- 1/BRATIO 
        #DG = sqrt((DBH/k)^2 + dds - (DBH/k))
        plt_yr_df$DG[i] <- k * (sqrt((plt_yr_df$DIA[i]/k)^2 + exp(plt_yr_df$log_dds[i])) -
                                  (plt_yr_df$DIA[i]/k))
      }
      
      #so DBH0 + k*DG = DBH
      plt_yr2_df <- plt_df %>%
        filter(Year == (growthyr+1))%>%
        mutate(DIA = plt_yr_df$DIA + plt_yr_df$DG)
      
      #join with nonfocal
      #fitler for trees on plots in validation set in same year
      non_foc_filt <- nonfocal %>%
        dplyr::select(PLT_CN, TRE_CN, SUBP, SPCD,TPA_UNADJ,Year,DIA_int) %>%
        mutate(DIA = DIA_int) %>%
        filter(PLT_CN == plt_yr2_df$PLT_CN[1],
               Year == plt_yr2_df$Year[1])
      #join
      density_cal <- bind_rows(plt_yr2_df,non_foc_filt)
      
      #compute density parameters
      #ccf
      for(i in 1:nrow(density_cal)){
        Species <- density_cal$SPCD[i]
        r1 <- ccf_df$r1[ccf_df$species == Species]
        r2 <- ccf_df$r2[ccf_df$species == Species]
        r3 <- ccf_df$r3[ccf_df$species == Species]
        r4 <- ccf_df$r4[ccf_df$species == Species]
        r5 <- ccf_df$r5[ccf_df$species == Species]
        dbrk <- ccf_df$dbrk[ccf_df$species == Species]
        if(Species == 475){
          ifelse(density_cal$DIA[i] < dbrk,
                 CCF_t <- density_cal$DIA[i]*(r1+r2+r3),
                 CCF_t <- r1 + (r2 * density_cal$DIA[i]) + (r3 * density_cal$DIA[i]^2))
        }
        if(Species != 475){
          ifelse(is.na(density_cal$DIA[i]), 
                 CCF_t <- NA,
                 ifelse(density_cal$DIA[i] <= 0.1, 
                        CCF_t <- 0.0001,
                        ifelse(density_cal$DIA[i] < dbrk, 
                               CCF_t <- r4 * (density_cal$DIA[i]^r5),
                               CCF_t <- r1 + (r2 * density_cal$DIA[i]) + (r3 * density_cal$DIA[i]^2))))
        }
        density_cal$CCF_t[i] <- CCF_t
      }
      
      density_cal <- density_cal %>%
        group_by(PLT_CN,SUBP,Year) %>%
        mutate(PCCF = sum(CCF_t * (TPA_UNADJ * 4), na.rm = TRUE))
      
      #stand CCF = sum(CCFt on a plot) on a per acre basis
      #TPA measured on a plot/stand level
      density_cal <- density_cal %>%
        group_by(PLT_CN,Year) %>%
        mutate(CCF = sum(CCF_t * TPA_UNADJ,na.rm = TRUE))
      
      #bal
      density_cal <- density_cal %>%
        mutate(BA_pa = ((DIA^2) * 0.005454) * TPA_UNADJ)
      
      #rank trees per year per plot
      #sum BApa of trees larger on the same plot in same year
      density_cal <- density_cal %>%
        group_by(PLT_CN,Year) %>%
        mutate(rank_pltyr = rank(DIA, na.last = TRUE, ties.method = "min")) %>%
        #min assigns lowest value to ties (1,2,3,3,5,6)
        mutate(BAL = map_dbl(DIA,~sum(BA_pa[DIA>.x],na.rm = TRUE)))
      
      density_cal <- density_cal %>%
        group_by(PLT_CN,Year) %>%
        mutate(SDI = sum(TPA_UNADJ*(DIA/10)^1.6), #stage
               num_t = length(unique(TRE_CN)))
      
      #filter for focal trees
      plt_yr2_df <- density_cal %>%
        filter(TRE_CN %in% plt_yr_df$TRE_CN)
      
      #cr
      for(i in 1:nrow(plt_yr2_df)){
        #Function arguments:
        #SPCD - is number code of species of tree record
        #SDI - is SDI of stand (Stage 1968)
        Species <- plt_yr2_df$SPCD[i]
        #SDI max values for each species were pulled from UT Variant overview
        SDIMAX <- ifelse(is.na(plt_yr2_df$SDImax[i]),
                         CR_WEIB_df$SDIMAX[CR_WEIB_df$species == Species],
                         plt_yr2_df$SDImax[i])
        #Calculate relative density
        RD <- plt_yr2_df$SDI[i]/SDIMAX
        
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
        SCALE = (1.0 - .00167 * (plt_yr2_df$CCF[i]-100.0))
        if(SCALE < 0.3){SCALE = 0.3}
        if(SCALE > 1.0){SCALE = 1.0}
        
        N <- plt_yr2_df$num_t[i]
        #X is tree's rank in diameter distribution
        #Multiply tree's rank in diameter distribution (trees position relative to tree with largest diameter in the stand) by scale parameter
        Y <- plt_yr2_df$rank_pltyr[i]/N * SCALE
        if(Y < 0.05){Y = 0.05}
        if(Y > 0.95){Y = 0.95}
        #Constrain Y between 0.05 and 0.95 - crown ratio predictions in FVS are bound between these two values
        
        #Calculate crown ratio (this corresponds to variable X in UTAH variant overview)
        X <- WEIBA + WEIBB*((-1*log(1-Y))^(1/WEIBC))
        #X = a tree’s crown ratio expressed as a percent / 10
        plt_yr2_df$CR_weib[i] <- X * 10
      }
      
      #join with plt_df
      for(i in 1:nrow(plt_df)){
        TRE_CN <- plt_df$TRE_CN[i]
        if(plt_df$Year[i] == (growthyr + 1)){
          plt_df$DIA[i] <- plt_yr2_df$DIA[plt_yr2_df$TRE_CN == TRE_CN]
          plt_df$CCF[i] <- plt_yr2_df$CCF[plt_yr2_df$TRE_CN == TRE_CN]
          plt_df$PCCF[i] <- plt_yr2_df$PCCF[plt_yr2_df$TRE_CN == TRE_CN]
          plt_df$BAL[i] <- plt_yr2_df$BAL[plt_yr2_df$TRE_CN == TRE_CN]
          plt_df$SDI[i] <- plt_yr2_df$SDI[plt_yr2_df$TRE_CN == TRE_CN]
          plt_df$CR_weib[i] <- plt_yr2_df$CR_weib[plt_yr2_df$TRE_CN == TRE_CN]
        }
      }
      #add density metrics in
      
      #loop over next year
      growthyr = growthyr + 1
    }
    #join plt_df with empty data frame
    #output
    pred_df <- bind_rows(pred_df,plt_df)
    
    #loop over next plot
  }
  
  return(pred_df)
}
#clt+enter through code works
#running function error: object of type 'closure' is not subsettable

