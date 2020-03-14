      ##Making Utah data frame
##Tables from Justing DeRose and FIA database
#original code from Margaret Evans
#margaret.ekevans@gmail.com


UT_per <- read.csv("./data/raw/T_Utah_periodic_metadata.csv", header = T)
UT_rw <- read.csv("./data/raw/Q_Utah_Courtney_ringwidth.csv",header = T)

library(tidyverse)

UT_plot <- read.csv("./data/raw/UT_PLOT.csv",header=T)
UT_tree <- read.csv("./data/raw/UT_TREE.csv",header = T)
UT_cond <- read.csv("./data/raw/UT_COND.csv",header = T)

#make tables generic
plot <- UT_plot
tree <- UT_tree
cond <- UT_cond

#UT_cond - Stand density index for the condition, 
##FLDTYPCD_30, Site index for the condition, SIBASE Site index base age, SISP Site index species code

#grab covariates for model
covariates <- tree[,c("CN","PLT_CN","SUBP","PREV_TRE_CN","DIA","CR","UNCRCD","SITREE","TPA_UNADJ")]

colnames(covariates)[colnames(covariates)=="CN"] <- "TRE_CN"
colnames(covariates)[colnames(covariates)=="DIA"] <- "DIA_t"
colnames(covariates)[colnames(covariates)=="SUBP"] <- "SUBP_t"

covariates$CONDID <- cond$CONDID[match(covariates$PLT_CN, cond$PLT_CN)]
covariates$ASPECT <- cond$ASPECT[match(covariates$PLT_CN, cond$PLT_CN)]
covariates$SLOPE <- cond$SLOPE[match(covariates$PLT_CN, cond$PLT_CN)]
#covariates$SDI <- conds$SDI_RMRS[match(covariates$PLT_CN, conds$PLT_CN)]
#all SDI are NA, need to calculate SDI
covariates$SDImax <- cond$SDIMAX_RMRS[match(covariates$PLT_CN, cond$PLT_CN)]
covariates$SICOND <- cond$SICOND[match(covariates$PLT_CN, cond$PLT_CN)]

#BALIVE 
#covariates$BALIVE <- apply(X = grData_remeas[, c("PREV_PLT_CN", "PREV_CONDID")], 
#                              MARGIN = 1, # applies function to each row in grData_remeas
#                              FUN = function(x, conds.df) {
#                                conds.df$BALIVE[conds.df$PLT_CN %in% x["PREV_PLT_CN"] &
#                                                  conds.df$CONDID %in% x["PREV_CONDID"]]
#                              },
#                              conds.df = conds)
#grData_remeas[is.nan(grData_remeas$BALIVE), "BALIVE"] <- NA
covariates$BALIVE <- cond$BALIVE[match(covariates$PLT_CN, cond$PLT_CN)]

covariates$LAT <- plot$LAT[match(covariates$PLT_CN, plot$CN)]
covariates$LON <- plot$LON[match(covariates$PLT_CN, plot$CN)]
covariates$ELEV <- plot$ELEV[match(covariates$PLT_CN, plot$CN)]
covariates$MEASYEAR <- plot$MEASYEAR[match(covariates$PLT_CN, plot$CN)]
covariates$DESIGNCD <- plot$DESIGNCD[match(covariates$PLT_CN, plot$CN)]
covariates$SUBP_EXAM <- plot$SUBP_EXAMINE_CD[match(covariates$PLT_CN, plot$CN)]
#covariates$PREV_MEASYEAR <- plots$MEASYEAR[match(covariates$PREV_PLT_CN, plots$CN)]

#location codes found in FIADB
#(FVS_LOC_CD) in PLOTGEOM table
library(dbplyr)
library(RSQLite)

UT_FIA <- DBI::dbConnect(RSQLite::SQLite(), "./data/raw/FS_FIADB_STATECD_49.db")
PLOTGEOM <- tbl(UT_FIA, sql("SELECT CN, FVS_LOC_CD FROM PLOTGEOM")) %>%
  collect()
covariates$FVS_LOC_CD <- PLOTGEOM$FVS_LOC_CD[match(covariates$PLT_CN, PLOTGEOM$CN)]

#data for glmm - ring widths
per_cov <- left_join(UT_per,covariates)
per_cov[duplicated(per_cov$TRE_CN),] #are there any dublicated trees?
#0

incr_percov <- left_join(UT_rw,per_cov) #by CN


#check species
length(per_cov$TRE_CN[per_cov$SPCD == 202]) #166
length(per_cov$TRE_CN[per_cov$SPCD == 122]) #80
length(per_cov$TRE_CN[per_cov$SPCD == 93])  #52

#is the dataset correct?
sum(is.na(incr_percov$RW)) #0
sum(is.na(UT_rw$RW)) #0
hist((incr_percov$DIA - incr_percov$DIA_t),breaks=50,
     main = "Histogram of core DIA - FIADB DIA") #remove outliers?
length(unique(incr_percov$TRE_CN[incr_percov$DIA == incr_percov$DIA_t]))
#[1] 572

#find rows where measure/inventory year matches year of ring
start.year <- incr_percov[incr_percov$MEASYEAR==incr_percov$Year,] #373
duplicated(start.year$TRE_CN) #FALSE
length(unique(start.year$TRE_CN)) #373

#Max year by CN - is it within 1 year of Measure year?
yr_cored <- aggregate(incr_percov$Year, by = list(incr_percov$TRE_CN), max)
colnames(yr_cored) <- c("TRE_CN","Year")
yr_cored$MEASYEAR <- UT_per$MEASYEAR[match(yr_cored$TRE_CN, UT_per$TRE_CN)]
yr_cored$diff <- yr_cored$Year - yr_cored$MEASYEAR
sum(yr_cored$diff < -1) #96; what???

save(per_cov,file = "./data/formatted/per_cov.Rdata")
save(incr_percov,file = "./data/formatted/incr_percov.Rdata")
  
