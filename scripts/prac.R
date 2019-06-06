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

#Margaret's method of comma delimited ring width values
#can easily create wide dataframe
#annualized calculation too hard
#long format preferred
#refer to annualizeDBH.R script

library(plyr)
UT_RWcd <- as.data.frame(ddply(UT_rw, .(CN), summarize, 
                               RW=paste(RW, collapse=","),
                               DateFirst=min(Year),
                               DateEnd=max(Year)))
glmm.data.cd <- merge(per_cov,UT_RWcd, by= "CN", all=TRUE, sort = FALSE)
save(glmm.data.cd,file = "FIA_RW.data")

temp1 <- glmm.data.cd[glmm.data.cd$MEASYEAR.y-glmm.data.cd$DateEnd<2,]
#temp2 <- temp1[temp1$MEASYEAR.y-temp1$DateEnd>-1,]

temp1$RW <- as.character(temp1$RW)
first.start.yr <- min(temp1$DateFirst, na.rm=T)
last.DBH.yr.1 <- max(temp1$MEASYEAR.y, na.rm=T)#1995
last.DBH.yr.2 <- max(temp1$DateEnd, na.rm = T) #1997
last.meas.yr <- max(last.DBH.yr.1, last.DBH.yr.2)
years <- seq(first.start.yr, last.meas.yr)
y.matrix <- matrix(data=NA, nrow=nrow(temp1), ncol=length(years))
colnames(y.matrix) <- years
for (t in 1:nrow(temp1)) {
  width.string <- temp1$RW[t]
  width.vector <- as.numeric(unlist(strsplit(x = width.string, split = ",")))
  start.column <- which(years == temp1$DateFirst[t])
  end.column <- which(years == temp1$DateEnd[t])
  width.subset <- (end.column - start.column) + 1
  if(length(width.subset)==0){
    cat("row=",t,"\n")
    cat("\tend.column=",end.column,", start.column=", start.column,"\n")
    cat("\tdate.end=",temp1$DateEnd[t],", date.first=",temp1$DateFirst[t],"\n")
  }
  width.vector <- width.vector[1:width.subset] 
  width.vector <- width.vector*0.1*2*(1/2.54) # convert to cm, then to inches
  #and multiply by 2 to turn radial increment into diameter increment
  y.matrix[t, c(start.column:end.column)] <- width.vector 
  # put vector in y.matrix at the right start year:end year
}


trunc.yr = 1954
index.last.start <- which(years==trunc.yr) 
y.small <- y.matrix[,index.last.start:ncol(y.matrix)]
years.small <- years[index.last.start:ncol(y.matrix)]
z0 <- matrix(data=NA, nrow=nrow(y.small), ncol=ncol(y.small))

DIA.T1 <- vector(mode="numeric", length=nrow(temp1))
#ave.ring <- vector(mode="numeric", length=nrow(temp1))
for (t in 1:nrow(temp1)) {
  ### shrink tree backward: subtract the cumulative tree-ring-derived diameter increments (in y.matrix) from DIA
  ifelse(!is.na(temp1$DIA_t[t]), DIA.T1[t]<-temp1$DIA_t[t], DIA.T1[t]<-NA) # extract time 1 DBH (in some cases, the only DBH measurement)
  # extract tree-ring data from trunc.yr:end series
  end.col <- which(years== min(temp1$MEASYEAR.y[t],temp1$DateEnd[t]))
  temp.growth <- y.matrix[t,(index.last.start+1):end.col] # which(years==1966) # returns 248
  # add rep(ave.ring) to any NA's at the beginning of the tree-ring time series
  #ave.ring[t] <- mean(temp.growth, na.rm=T)
  #temp.growth[is.na(temp.growth)]<-ave.ring[t]
  temp.growth2 <- c(-rev(cumsum(rev(temp.growth))),0) # add a zero at the end of this so that z0 in MEASYR = DIA
  z0[t,1:length(temp.growth2)] <- DIA.T1[t] + temp.growth2 # note that this is one year off where DateEnd = MEASYEAR-1
  
  ### grow tree forward: add cumulative from DIA
  end.rw <- which(years==temp1$DateEnd[t]) #year of last ring width value
  #only want trees that have more ring width values after the plot was inventoried
  if(end.col < end.rw){
    growth.for <- y.matrix[t, end.col:end.rw]
    growth.for2 <- c(0,cumsum(temp.growth))
    z0[t, length(temp.growth2):length(years.small)] <- DIA.T1[t] + cumsum(growth.for2)
  }
}



