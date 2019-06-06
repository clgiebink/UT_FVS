#Climate-Growth Relationships
#to determine significant climate variables to include in model
#Courtney Giebink
#clgiebink@gmail.com
#June 2019

library(tidyverse)
library(dplR)
library(treeclim)

#read in ring width data
load("glmm.data.imputed")

#create ring width dataframe (rwl) for dplR
rw_sub <- glmm.data.imputed[,c("TRE_CN","Year","RW")]
rw_data <- spread(rw_sub,key = "TRE_CN",value = RW,fill = NA,drop = FALSE)
rw_data <- as.data.frame(rw_data)
years <- rw_data$Year
rw_data <- rw_data[,-1]
rownames(rw_data) <- years
colnames(rw_data) <- paste("SER", colnames(rw_data), sep = "_")

#explore dataframe in dplR
rwl.report(rw_data) #descriptive statistics

rw_stat <- rwl.stats(rw_data)
head(rw_stat)


#Read in PRISM climate data
#each row is a tree, each column is a year&month
ppt.extr <- read.csv(paste(processed.path,"ppt_extr.csv",sep=''), header = T)
tmin.extr <- read.csv(paste(processed.path,"tmin_extr.csv",sep=''), header = T)
tmax.extr <- read.csv(paste(processed.path,"tmax_extr.csv",sep=''), header = T)

#build dataframe for treeclim
temp_mxn <- data.frame("tmin" = colMeans(tmin.extr,na.rm = TRUE),
                       "tmax" = colMeans(tmax.extr,na.rm = TRUE))
clim_crn <- data.frame("year" = rep(1895:2000,each=12),
                       "month" = rep(1:12,106),
                       "prec" = colMeans(ppt.extr, na.rm = TRUE),
                       "temp" = rowMeans(temp_mxn, na.rm = TRUE))


#subset ring width data to correlate with climate
rw_data_sub <- subset(rw_data, rownames(rw_data) > 1895) 
max(rownames(rw_data_sub)) #last year will subset climate data

#subset climate data to correlate with ring width
clim_sub <- clim_crn[clim_crn$year <= 1997,]

#detrend and build chronology
rwi_nex <- detrend(rwl = rw_data_sub, 
                   method = "ModNegExp")
rwi.stats(rwi_nex)
rw_crn <- chron(rwi_nex, prefix = "CAM")

#growth-climate relationships
dc1 <- dcc(rw_crn, clim_sub) #dcc = dendroclimatic calibration
plot(dc1)

sc1 <- seascorr(rw_crn,clim_sub, 
                primary = "prec", secondary = "temp")
plot(sc1)

dc2 <- dcc(rw_crn, clim_sub, selection = 1:9, 
           dynamic = "moving", boot = "std", sb = FALSE)
plot(dc2)
