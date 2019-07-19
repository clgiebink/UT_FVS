##Making Utah data frame
##Tables from Justing DeRose and FIA database
#original code from Margaret Evans
#margaret.ekevans@gmail.com


UT_per <- read.csv("./data/raw/T_Utah_periodic_metadata.csv", header = T)
UT_rw <- read.csv("./data/raw/Q_Utah_Courtney_ringwidth.csv",header = T)

library(tidyverse)

UT_plot <- read.csv("./data/raw/PLOT.txt",header=T)
UT_tree <- read.csv("./data/raw/TREE.txt",header = T)
UT_cond <- read.csv("./data/raw/COND.txt",header = T)

#make tables generic
plot <- UT_plot
tree <- UT_tree
cond <- UT_cond

#UT_cond - Stand density index for the condition, 
##FLDTYPCD_30, Site index for the condition, SIBASE Site index base age, SISP Site index species code

#grab covariates for model
covariates <- UT_tree[,c("CN","PLT_CN","PREV_TRE_CN","DIA","CR","SITREE","TPA_UNADJ")]

colnames(covariates)[colnames(covariates)=="CN"] <- "TRE_CN"
colnames(covariates)[colnames(covariates)=="DIA"] <- "DIA_t"

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
#covariates$PREV_MEASYEAR <- plots$MEASYEAR[match(covariates$PREV_PLT_CN, plots$CN)]

#data for glmm - ring widths
per_cov <- merge(UT_per,covariates,by = "TRE_CN",all.x = TRUE,sort = FALSE)

incr_percov <- merge(per_cov,UT_rw, by= "CN", all=TRUE, sort = FALSE)

#is the dataset correct?
sum(is.na(incr_percov$RW)) #0
sum(is.na(UT_rw$RW)) #0
hist((incr_percov$DIA - incr_percov$DIA_t),breaks=50) #remove outliers?
length(unique(incr_percov$TRE_CN[incr_percov$DIA == incr_percov$DIA_t]))
#[1] 575

#filter large differences in FIADB DIA and core mount DIA
incr_percov <- incr_percov[abs(incr_percov$DIA - incr_percov$DIA_t) <= 0.5,] #half an inch margin of error
hist((incr_percov$DIA - incr_percov$DIA_t),breaks=50) #check
length(unique(incr_percov$TRE_CN))
#[1] 582

save(incr_percov,file = "./data/formatted/incr_percov")

#find rows where measure/inventory year matches year of ring
start.year <- incr_percov[incr_percov$MEASYEAR.x==incr_percov$Year,] #373

#Max year by CN - is it within 1 year of Measure year?
yr_cored <- aggregate(incr_percov$Year, by = list(incr_percov$TRE_CN), max)
colnames(yr_cored) <- c("TRE_CN","Year")
yr_cored$MEASYEAR <- UT_per$MEASYEAR[match(yr_cored$TRE_CN, UT_per$TRE_CN)]
yr_cored$diff <- yr_cored$Year - yr_cored$MEASYEAR
sum(yr_cored$diff < -1) #96; what???

