##Making Utah data frame
##Tables from Justing DeRose and FIA database
#original code from Margaret Evans
#margaret.ekevans@gmail.com


UT_per <- read.csv("T_Utah_periodic_metadata.csv", header = T)
UT_rw <- read.csv("Q_Utah_Courtney_ringwidth.csv",header = T)

library(tidyr)

UT_plot <- read.csv("PLOT.txt",header=T)
UT_tree <- read.csv("TREE.txt",header = T)
UT_cond <- read.csv("COND.txt",header = T)

#make tables generic
plots <- UT_plot
tree <- UT_tree
conds <- UT_cond

#UT_cond - Stand density index for the condition, 
##FLDTYPCD_30, Site index for the condition, SIBASE Site index base age, SISP Site index species code


covariates <- UT_tree[,c("CN","PLT_CN","PREV_TRE_CN","DIA","CR","SITREE","TPA_UNADJ","TPAGROW_UNADJ")]

colnames(covariates)[colnames(covariates)=="CN"] <- "TRE_CN"
colnames(covariates)[colnames(covariates)=="DIA"] <- "DIA_t"

covariates$ASPECT <- conds$ASPECT[match(covariates$PLT_CN, conds$PLT_CN)]
covariates$SLOPE <- conds$SLOPE[match(covariates$PLT_CN, conds$PLT_CN)]
#covariates$SDI <- conds$SDI_RMRS[match(covariates$PLT_CN, conds$PLT_CN)]
#all SDI are NA, need to calculate SDI
covariates$SDImax <- conds$SDIMAX_RMRS[match(covariates$PLT_CN, conds$PLT_CN)]
covariates$SICOND <- conds$SICOND[match(covariates$PLT_CN, conds$PLT_CN)]

#BALIVE 
covariates$BALIVE <- conds$BALIVE[match(covariates$PLT_CN, conds$PLT_CN)]
#grData_remeas$BALIVE <- apply(X = grData_remeas[, c("PREV_PLT_CN", "PREV_CONDID")], 
# MARGIN = 1, # applies function to each row in grData_remeas
# FUN = function(x, conds.df) {
#   conds.df$BALIVE[conds.df$PLT_CN %in% x["PREV_PLT_CN"] &
#                     conds.df$CONDID %in% x["PREV_CONDID"]]
# },
# conds.df = conds)
#grData_remeas[is.nan(grData_remeas$BALIVE), "BALIVE"] <- NA
#grData_remeas <- subset(grData_remeas, !is.na(BALIVE))

covariates$LAT <- plots$LAT[match(covariates$PLT_CN, plots$CN)]
covariates$LON <- plots$LON[match(covariates$PLT_CN, plots$CN)]
covariates$ELEV <- plots$ELEV[match(covariates$PLT_CN, plots$CN)]
covariates$MEASYEAR <- plots$MEASYEAR[match(covariates$PLT_CN, plots$CN)]
#covariates$PREV_MEASYEAR <- plots$MEASYEAR[match(covariates$PREV_PLT_CN, plots$CN)]
#?per_cov$PREV_PLT_CN <- per_cov$PLT_CN[match($PREV_TRE_CN, grData$CN)]

#data for glmm - ring widths
per_cov <- merge(UT_per,covariates,by = "TRE_CN",all.x = TRUE,sort = FALSE)

glmm.data <- merge(per_cov,UT_rw, by= "CN", all=TRUE, sort = FALSE)

#is the dataset correct?
sum(is.na(glmm.data$RW)) #0
sum(is.na(UT_rw$RW)) #0
hist((glmm.data$DIA - glmm.data$DIA_t),breaks=50) #remove outliers?

#find rows where measure/inventory year matches year of ring
start.year <- glmm.data[glmm.data$MEASYEAR.x==glmm.data$Year,] #373

#Max year by CN - is it within 1 year of Measure year?
yr_cored <- aggregate(glmm.data$Year, by = list(glmm.data$TRE_CN), max)
colnames(yr_cored) <- c("TRE_CN","Year")
yr_cored$MEASYEAR <- UT_per$MEASYEAR[match(yr_cored$TRE_CN, UT_per$TRE_CN)]
yr_cored$diff <- yr_cored$Year - yr_cored$MEASYEAR
sum(yr_cored$diff < -1) #96; what???

