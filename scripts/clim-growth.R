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

#all species
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

#all
#growth-climate relationships
dc1_all <- dcc(rw_crn, clim_sub) #dcc = dendroclimatic calibration
plot(dc1_all)

sc1_all <- seascorr(rw_crn,clim_sub, 
                primary = "prec", secondary = "temp")
plot(sc1_all)

dc2_all <- dcc(rw_crn, clim_sub, selection = 1:9, 
           dynamic = "moving", boot = "std", sb = FALSE)
plot(dc2_all)


#DF
#create ring width dataframe (rwl) for dplR
incr_data_DF <- incr_percov %>%
  filter(SPCD == 202)
rw_sub_DF <- incr_data_DF[,c("TRE_CN","Year","RW")]
rw_data_DF <- spread(rw_sub_DF,key = "TRE_CN",value = RW,fill = NA,drop = FALSE)
rw_data_DF <- as.data.frame(rw_data_DF)
years <- rw_data_DF$Year
rw_data_DF <- rw_data_DF[,-1]
rownames(rw_data_DF) <- years
colnames(rw_data_DF) <- paste("SER", colnames(rw_data_DF), sep = "_")

#explore dataframe in dplR
rwl.report(rw_data_DF) #descriptive statistics

rw_stat_DF <- rwl.stats(rw_data_DF)
head(rw_stat_DF)


#Read in PRISM climate data
#each row is a tree, each column is a year&month
ppt.extr_DF <- read.csv(paste(processed.path,"ppt_extr_DF.csv",sep=''), header = T)
tmin.extr_DF <- read.csv(paste(processed.path,"tmin_extr_DF.csv",sep=''), header = T)
tmax.extr_DF <- read.csv(paste(processed.path,"tmax_extr_DF.csv",sep=''), header = T)

#build dataframe for treeclim
temp_mxn_DF <- data.frame("tmin" = colMeans(tmin.extr_DF,na.rm = TRUE),
                       "tmax" = colMeans(tmax.extr_DF,na.rm = TRUE))
clim_crn_DF <- data.frame("year" = rep(1895:2000,each=12),
                       "month" = rep(1:12,106),
                       "prec" = colMeans(ppt.extr_DF, na.rm = TRUE),
                       "temp" = rowMeans(temp_mxn_DF, na.rm = TRUE))


#subset ring width data to correlate with climate
rw_data_DF_sub <- subset(rw_data_DF, rownames(rw_data_DF) > 1895) 
max(rownames(rw_data_DF_sub)) #last year will subset climate data; 1997

#subset climate data to correlate with ring width
clim_DF_sub <- clim_crn_DF[clim_crn_DF$year <= 1997,]

#detrend and build chronology
rwi_nex_DF <- detrend(rwl = rw_data_DF_sub, 
                   method = "ModNegExp")
rwi.stats(rwi_nex_DF)
rw_crn_DF <- chron(rwi_nex_DF, prefix = "CAM")


#growth-climate relationships
dc1_DF <- dcc(rw_crn_DF, clim_DF_sub) #dcc = dendroclimatic calibration
plot(dc1_DF)

sc1_DF <- seascorr(rw_crn_DF,clim_DF_sub, 
                primary = "prec", secondary = "temp")
plot(sc1_DF)

dc2_DF <- dcc(rw_crn_DF, clim_DF_sub, selection = 1:9, 
           dynamic = "moving", boot = "std", sb = FALSE)
plot(dc2_DF)

#PP
#create ring width dataframe (rwl) for dplR
incr_data_PP <- incr_percov %>%
  filter(SPCD == 122)
rw_sub_PP <- incr_data_PP[,c("TRE_CN","Year","RW")]
rw_data_PP <- spread(rw_sub_PP,key = "TRE_CN",value = RW,fill = NA,drop = FALSE)
rw_data_PP <- as.data.frame(rw_data_PP)
years <- rw_data_PP$Year
rw_data_PP <- rw_data_PP[,-1]
rownames(rw_data_PP) <- years
colnames(rw_data_PP) <- paste("SER", colnames(rw_data_PP), sep = "_")

#explore dataframe in dplR
rwl.report(rw_data_PP) #descriptive statistics

rw_stat_PP <- rwl.stats(rw_data_PP)
head(rw_stat_PP)


#Read in PRISM climate data
#each row is a tree, each column is a year&month
ppt.extr_PP <- read.csv(paste(processed.path,"ppt_extr_PP.csv",sep=''), header = T)
tmin.extr_PP <- read.csv(paste(processed.path,"tmin_extr_PP.csv",sep=''), header = T)
tmax.extr_PP <- read.csv(paste(processed.path,"tmax_extr_PP.csv",sep=''), header = T)

#build dataframe for treeclim
temp_mxn_PP <- data.frame("tmin" = colMeans(tmin.extr_PP,na.rm = TRUE),
                          "tmax" = colMeans(tmax.extr_PP,na.rm = TRUE))
clim_crn_PP <- data.frame("year" = rep(1895:2000,each=12),
                          "month" = rep(1:12,106),
                          "prec" = colMeans(ppt.extr_PP, na.rm = TRUE),
                          "temp" = rowMeans(temp_mxn_PP, na.rm = TRUE))


#subset ring width data to correlate with climate
rw_data_PP_sub <- subset(rw_data_PP, rownames(rw_data_PP) > 1895) 
max(rownames(rw_data_PP_sub)) #last year will subset climate data; 1997

#subset climate data to correlate with ring width
clim_PP_sub <- clim_crn_PP[clim_crn_PP$year <= 1997,]

#detrend and build chronology
rwi_nex_PP <- detrend(rwl = rw_data_PP_sub, 
                      method = "ModNegExp")
rwi.stats(rwi_nex_PP)
rw_crn_PP <- chron(rwi_nex_PP, prefix = "CAM")


#growth-climate relationships
dc1_PP <- dcc(rw_crn_PP, clim_PP_sub) #dcc = dendroclimatic calibration
plot(dc1_PP)

sc1_PP <- seascorr(rw_crn_PP,clim_PP_sub, 
                   primary = "prec", secondary = "temp")
plot(sc1_PP)

dc2_PP <- dcc(rw_crn_PP, clim_PP_sub, selection = 1:9, 
              dynamic = "moving", boot = "std", sb = FALSE)
plot(dc2_PP)

#ES
#create ring width dataframe (rwl) for dplR
incr_data_ES <- incr_percov %>%
  filter(SPCD == 93)
rw_sub_ES <- incr_data_ES[,c("TRE_CN","Year","RW")]
rw_data_ES <- spread(rw_sub_ES,key = "TRE_CN",value = RW,fill = NA,drop = FALSE)
rw_data_ES <- as.data.frame(rw_data_ES)
years <- rw_data_ES$Year
rw_data_ES <- rw_data_ES[,-1]
rownames(rw_data_ES) <- years
colnames(rw_data_ES) <- paste("SER", colnames(rw_data_ES), sep = "_")

#explore dataframe in dplR
rwl.report(rw_data_ES) #descriptive statistics

rw_stat_ES <- rwl.stats(rw_data_ES)
head(rw_stat_ES)

#Read in PRISM climate data
#each row is a tree, each column is a year&month
ppt.extr_ES <- read.csv(paste(processed.path,"ppt_extr_ES.csv",sep=''), header = T)
tmin.extr_ES <- read.csv(paste(processed.path,"tmin_extr_ES.csv",sep=''), header = T)
tmax.extr_ES <- read.csv(paste(processed.path,"tmax_extr_ES.csv",sep=''), header = T)

#build dataframe for treeclim
temp_mxn_ES <- data.frame("tmin" = colMeans(tmin.extr_ES,na.rm = TRUE),
                          "tmax" = colMeans(tmax.extr_ES,na.rm = TRUE))
clim_crn_ES <- data.frame("year" = rep(1895:2000,each=12),
                          "month" = rep(1:12,106),
                          "prec" = colMeans(ppt.extr_ES, na.rm = TRUE),
                          "temp" = rowMeans(temp_mxn_ES, na.rm = TRUE))


#subset ring width data to correlate with climate
rw_data_ES_sub <- subset(rw_data_ES, rownames(rw_data_ES) > 1895) 
max(rownames(rw_data_ES_sub)) #last year will subset climate data; 1994

#subset climate data to correlate with ring width
clim_ES_sub <- clim_crn_ES[clim_crn_ES$year <= 1994,]

#detrend and build chronology
rwi_nex_ES <- detrend(rwl = rw_data_ES_sub, 
                      method = "ModNegExp")
rwi.stats(rwi_nex_ES)
rw_crn_ES <- chron(rwi_nex_ES, prefix = "CAM")


#growth-climate relationships
dc1_ES <- dcc(rw_crn_ES, clim_ES_sub) #dcc = dendroclimatic calibration
plot(dc1_ES)

sc1_ES <- seascorr(rw_crn_ES,clim_ES_sub, 
                   primary = "prec", secondary = "temp")
plot(sc1_ES)

dc2_ES <- dcc(rw_crn_ES, clim_ES_sub, selection = 1:9, 
              dynamic = "moving", boot = "std", sb = FALSE)
plot(dc2_ES)

