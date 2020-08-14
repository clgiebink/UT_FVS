#Climate-Growth Relationships
#to determine significant climate variables to include in model
#Courtney Giebink
#clgiebink@gmail.com
#June 2019

library(tidyverse)
library(dplR)
library(treeclim)

#read in ring width data
load("./data/formatted/data_all.Rdata")
#568 trees
#climate data
load("./data/formatted/clim_all.R")
#568 trees
#give species info to trees to filter later
clim_all$SPCD <- data_all$SPCD[match(clim_all$TRE_CN,data_all$TRE_CN)]
#reduce the years to reflect PRISM data
clim_all <- clim_all %>%
  filter(Year >= 1895)


#DF
#create ring width dataframe (rwl) for dplR
rw_data_DF <- data_all %>%
  filter(SPCD == 202) %>%
  ungroup() %>%
  dplyr::select(TRE_CN, Year, RW)
rw_data_DF <- spread(rw_data_DF,key = "TRE_CN",value = RW,fill = NA,drop = FALSE) #pivot_wider?
rw_data_DF <- as.data.frame(rw_data_DF)
years <- rw_data_DF$Year
rw_data_DF <- rw_data_DF %>%
  dplyr::select(-Year)
rownames(rw_data_DF) <- years
colnames(rw_data_DF) <- paste("SER", colnames(rw_data_DF), sep = "_")

#explore dataframe in dplR
rwl.report(rw_data_DF) #descriptive statistics
rw_stat_DF <- rwl.stats(rw_data_DF)
head(rw_stat_DF)

#Get climate data
#treeclim needs it to be a specific format
clim_df <- clim_all %>%
  filter(SPCD == 202) %>%
  ungroup() %>%
  dplyr::select(-c(PLT_CN,TRE_CN,SPCD))%>%
  group_by(Year) %>%
  summarise_all(mean)

tmax_df <- clim_df %>%
  dplyr::select(Year, contains("tmax")) %>%
  pivot_longer(-Year,names_to = "var1",values_to = "tmax")

tav_df <- clim_df %>%
  dplyr::select(Year, contains("tmin")) %>%
  pivot_longer(-Year,names_to = "var2",values_to = "tmin")%>%
  mutate(tmax = tmax_df$tmax) %>%
  rowwise() %>%
  mutate(temp = mean(c(tmax,tmin)))

ppt_df <- clim_df %>%
  dplyr::select(Year, contains("ppt")) %>%
  pivot_longer(-Year,names_to = "var",values_to = "prec") %>%
  mutate(month = rep(1:12,times = 106)) %>% #1895-2000
  dplyr::select(Year,month,prec)

clim_df <- ppt_df %>%
  mutate(temp = tav_df$temp)

#could also use prism extract output (ppt.extr) - check history for code
#build dataframe for treeclim
# temp_mxn_DF <- data.frame("tmin" = colMeans(tmin.extr_DF,na.rm = TRUE),
#                        "tmax" = colMeans(tmax.extr_DF,na.rm = TRUE))
# clim_df <- data.frame("year" = rep(1895:2000,each=12),
#                        "month" = rep(1:12,106),
#                        "prec" = colMeans(ppt.extr_DF, na.rm = TRUE),
#                        "temp" = rowMeans(temp_mxn_DF, na.rm = TRUE))


#subset ring width data to correlate with climate
rw_data_DF <- subset(rw_data_DF, rownames(rw_data_DF) >= 1895) 
max(rownames(rw_data_DF)) #last year will subset climate data; 1995

#subset climate data to correlate with ring width
clim_df <- as.data.frame(clim_df[clim_df$Year <= 1995,])

#detrend and build chronology
rwi_nex_DF <- detrend(rwl = rw_data_DF, 
                   method = "ModNegExp")
rwi.stats(rwi_nex_DF)
rw_crn_DF <- chron(rwi_nex_DF, prefix = "CAM")


#growth-climate relationships
dc1_DF <- dcc(chrono = rw_crn_DF, climate = clim_df) #dcc = dendroclimatic calibration
plot(dc1_DF)

sc1_DF <- seascorr(rw_crn_DF,clim_df, 
                primary = "prec", secondary = "temp")
plot(sc1_DF)

dc2_DF <- dcc(rw_crn_DF, clim_df, selection = 1:9, 
           dynamic = "moving", boot = "std", sb = FALSE)
plot(dc2_DF)


#PP
#create ring width dataframe (rwl) for dplR
rw_data_PP <- incr_percov %>%
  filter(SPCD == 122) %>%
  ungroup() %>%
  dplyr::select(TRE_CN, Year, RW)
rw_data_PP <- spread(rw_data_PP,key = "TRE_CN",value = RW,fill = NA,drop = FALSE)
rw_data_PP <- as.data.frame(rw_data_PP)
years <- rw_data_PP$Year
rw_data_PP <- rw_data_PP %>%
  dplyr::select(-Year)
rownames(rw_data_PP) <- years
colnames(rw_data_PP) <- paste("SER", colnames(rw_data_PP), sep = "_")

#explore dataframe in dplR
rwl.report(rw_data_PP) #descriptive statistics
rw_stat_PP <- rwl.stats(rw_data_PP)
head(rw_stat_PP)

#Get climate data
#treeclim needs it to be a specific format
clim_pp <- clim_all %>%
  filter(SPCD == 122) %>%
  ungroup() %>%
  dplyr::select(-c(PLT_CN,TRE_CN,SPCD))%>%
  group_by(Year) %>%
  summarise_all(mean)

tmax_pp <- clim_pp %>%
  dplyr::select(Year, contains("tmax")) %>%
  pivot_longer(-Year,names_to = "var1",values_to = "tmax")

tav_pp <- clim_pp %>%
  dplyr::select(Year, contains("tmin")) %>%
  pivot_longer(-Year,names_to = "var2",values_to = "tmin")%>%
  mutate(tmax = tmax_df$tmax) %>%
  rowwise() %>%
  mutate(temp = mean(c(tmax,tmin)))

ppt_pp <- clim_pp %>%
  dplyr::select(Year, contains("ppt")) %>%
  pivot_longer(-Year,names_to = "var",values_to = "prec") %>%
  mutate(month = rep(1:12,times = 106)) %>% #1895-2000
  dplyr::select(Year,month,prec)

clim_pp <- ppt_pp %>%
  mutate(temp = tav_df$temp)

#subset ring width data to correlate with climate
rw_data_PP <- subset(rw_data_PP, rownames(rw_data_PP) > 1895) 
max(rownames(rw_data_PP)) #last year will subset climate data; 1997

#subset climate data to correlate with ring width
clim_pp <- as.data.frame(clim_pp[clim_pp$Year <= 1997,])


#detrend and build chronology
rwi_nex_PP <- detrend(rwl = rw_data_PP, 
                      method = "ModNegExp")
rwi.stats(rwi_nex_PP)
rw_crn_PP <- chron(rwi_nex_PP, prefix = "CAM")


#growth-climate relationships
dc1_PP <- dcc(rw_crn_PP, clim_pp) #dcc = dendroclimatic calibration
plot(dc1_PP)

sc1_PP <- seascorr(rw_crn_PP,clim_pp, 
                   primary = "prec", secondary = "temp")
plot(sc1_PP)

dc2_PP <- dcc(rw_crn_PP, clim_pp, selection = 1:9, 
              dynamic = "moving", boot = "std", sb = FALSE)
plot(dc2_PP)


#ES
#create ring width dataframe (rwl) for dplR
rw_data_ES <- incr_percov %>%
  filter(SPCD == 93) %>%
  ungroup() %>%
  dplyr::select(TRE_CN, Year, RW)
rw_data_ES <- spread(rw_data_ES,key = "TRE_CN",value = RW,fill = NA,drop = FALSE)
rw_data_ES <- as.data.frame(rw_data_ES)
years <- rw_data_ES$Year
rw_data_ES <- rw_data_ES %>%
  dplyr::select(-Year)
rownames(rw_data_ES) <- years
colnames(rw_data_ES) <- paste("SER", colnames(rw_data_ES), sep = "_")

#explore dataframe in dplR
rwl.report(rw_data_ES) #descriptive statistics
rw_stat_ES <- rwl.stats(rw_data_ES)
head(rw_stat_ES)

#Get climate data
#treeclim needs it to be a specific format
clim_es <- clim_all %>%
  filter(SPCD == 93) %>%
  ungroup() %>%
  dplyr::select(-c(PLT_CN,TRE_CN,SPCD))%>%
  group_by(Year) %>%
  summarise_all(mean)

tmax_es <- clim_es %>%
  dplyr::select(Year, contains("tmax")) %>%
  pivot_longer(-Year,names_to = "var1",values_to = "tmax")

tav_es <- clim_es %>%
  dplyr::select(Year, contains("tmin")) %>%
  pivot_longer(-Year,names_to = "var2",values_to = "tmin")%>%
  mutate(tmax = tmax_df$tmax) %>%
  rowwise() %>%
  mutate(temp = mean(c(tmax,tmin)))

ppt_es <- clim_es %>%
  dplyr::select(Year, contains("ppt")) %>%
  pivot_longer(-Year,names_to = "var",values_to = "prec") %>%
  mutate(month = rep(1:12,times = 106)) %>% #1895-2000
  dplyr::select(Year,month,prec)

clim_es <- ppt_es %>%
  mutate(temp = tav_df$temp)

#subset ring width data to correlate with climate
rw_data_ES <- subset(rw_data_ES, rownames(rw_data_ES) > 1895) 
max(rownames(rw_data_ES)) #last year will subset climate data; 1994

#subset climate data to correlate with ring width
clim_es <- as.data.frame(clim_es[clim_es$year <= 1994,])

#detrend and build chronology
rwi_nex_ES <- detrend(rwl = rw_data_ES, 
                      method = "ModNegExp")
rwi.stats(rwi_nex_ES)
rw_crn_ES <- chron(rwi_nex_ES, prefix = "CAM")


#growth-climate relationships
dc1_ES <- dcc(rw_crn_ES, clim_es) #dcc = dendroclimatic calibration
plot(dc1_ES)

sc1_ES <- seascorr(rw_crn_ES,clim_es, 
                   primary = "prec", secondary = "temp")
plot(sc1_ES)

dc2_ES <- dcc(rw_crn_ES, clim_es, selection = 1:9, 
              dynamic = "moving", boot = "std", sb = FALSE)
plot(dc2_ES)

