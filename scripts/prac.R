##Making Utah data frame
##Tables from Justing DeRose and FIA database
#original code from Margaret Evans
#margaret.ekevans@gmail.com
#updated by Courtney Giebink
#clgiebink@gmail.com

library(tidyverse)
#read in data from Justin
UT_per <- read.csv("./data/raw/T_Utah_periodic_metadata.csv", header = T, stringsAsFactors = F)
UT_rw <- read.csv("./data/raw/Q_Utah_Courtney_ringwidth.csv", header = T, stringsAsFactors = F)

#connect to SQLite database to get metadata
library(dbplyr)
library(RSQLite)
UT_FIA <- DBI::dbConnect(RSQLite::SQLite(), "./data/raw/FIADB.db")
tree <- tbl(UT_FIA, sql("SELECT * FROM TREE")) %>%
  collect()
plot <- tbl(UT_FIA, sql("SELECT * FROM PLOT")) %>%
  collect()
colnames(plot)[colnames(plot)=="CN"] <- "PLT_CN"
cond <- tbl(UT_FIA, sql("SELECT * FROM COND")) %>%
  collect()

#grab covariates for model
tree_red <- tree[,c("CN","PLT_CN","SUBP","PREV_TRE_CN","DIA","CR","SITREE","TPA_UNADJ")]
colnames(tree_red)[colnames(tree_red)=="CN"] <- "TRE_CN"
colnames(tree_red)[colnames(tree_red)=="DIA"] <- "DIA_t"
colnames(tree_red)[colnames(tree_red)=="SUBP"] <- "SUBP_t"

cond_red <- cond[,c("PLT_CN","CONDID","ASPECT","SLOPE","SDIMAX_RMRS","SICOND","BALIVE")]
#all SDI_RMRS are NA, need to calculate SDI
colnames(cond_red)[colnames(cond_red)=="SDIMAX_RMRS"] <- "SDImax"

plot_red <- plot[,c("PLT_CN","LAT","LON","ELEV","MEASYEAR","DESIGNCD","SUBP_EXAMINE_CD")]
colnames(plot_red)[colnames(plot_red)=="SUBP_EXAMINE_CD"] <- "SUBP_EXAM"

covariates <- full_join(tree_red,cond_red, by="PLT_CN")
covariates <- full_join(covariates,plot_red, by="PLT_CN")

#location codes found in FIADB
#(FVS_LOC_CD) in PLOTGEOM table
PLOTGEOM <- tbl(UT_FIA, sql("SELECT CN, FVS_LOC_CD FROM PLOTGEOM")) %>%
  collect()
covariates$FVS_LOC_CD <- PLOTGEOM$FVS_LOC_CD[match(covariates$PLT_CN, PLOTGEOM$CN)]

#data for glmm - ring widths
#merge justin's trees with FIADB metadata
covariates$TRE_CN <- as.numeric(covariates$TRE_CN)
covariates$PLT_CN <- as.numeric(covariates$PLT_CN)
per_cov <- left_join(UT_per,covariates)
per_cov[duplicated(per_cov$TRE_CN),] #are there any dublicated trees?
#0

#merge with ring width data
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
#DIA from FIADB (DIA_t) is correct

#Does ring width data overlap with DBH measurement?
yr_cored <- aggregate(incr_percov$Year, by = list(incr_percov$TRE_CN), max)
colnames(yr_cored) <- c("TRE_CN","Year")
yr_cored$MEASYEAR <- UT_per$MEASYEAR[match(yr_cored$TRE_CN, UT_per$TRE_CN)]
yr_cored$diff <- yr_cored$Year - yr_cored$MEASYEAR
#positive - yes; negative - no

save(per_cov,file = "./data/formatted/per_cov.Rdata")
save(incr_percov,file = "./data/formatted/incr_percov.Rdata")

dbDisconnect(UT_FIA)

#check
#condid


#site index
# first site tree?
cond_si <- cond[,c("PLT_CN","SICOND","SISP","SIBASE")]
cond_si$PLT_CN <- as.numeric(cond_si$PLT_CN)
library(dbplyr)
library(RSQLite)
UT_FIA <- DBI::dbConnect(RSQLite::SQLite(), "./data/raw/FS_FIADB_STATECD_49.db")
sitetree <- tbl(UT_FIA, sql("SELECT PLT_CN, SUBP, SPCD, SITREE FROM SITETREE")) %>%
  collect()
sitetree$PLT_CN <- as.numeric(sitetree$PLT_CN)

#does site species associated with sicond match spcd?
#is base age (sibase) consistent?
per_cov_si <- per_cov %>%
  dplyr::select(PLT_CN,TRE_CN,SUBP,SPCD,SICOND) %>%
  left_join(.,cond_si) %>%
  filter(SPCD %in% c(93,122,202)) %>%
  filter(SPCD != SISP)

#can use SITREE to fill missing?
per_cov_si <- left_join(per_cov_si,SITREE)
#no match - all NAs

# send to john to find match
cal_si <- per_cov_si
write.csv(cal_si, file = "./data/formatted/cal_si.csv")

#after sending to John
#load data with matched SI
si_match <- read_csv(file = "./data/raw/Supplemental_SI.csv")
si_match <- si_match %>%
  select(TRE_CN,SITREE) %>%
  group_by(TRE_CN) %>%
  summarise(SICOND = mean(SITREE, na.rm =T))

fix_si <- function(data,new_si){
  for(i in 1:nrow(data)){
    TRE_CN <- data$TRE_CN[i]
    if(TRE_CN %in% new_si$TRE_CN){
      data$SICOND[i] <- new_si$SICOND[new_si$TRE_CN == TRE_CN]
    }
  }
}

incr_percov <- fix_si(data = incr_percov, new_si = si_match)
#fixed 28 trees

#for filtering later
#take out trees where SI is not fixed
# cal_si <- cal_si %>%
#   filter(!(TRE_CN %in% si_match$TRE_CN))

save(incr_percov,file = "./data/formatted/incr_percov.Rdata")