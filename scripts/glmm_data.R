#finish off dataframe
#Courtney Giebink
#clgiebink@gmail.com
#July 19, 2019

library(tidyverse)

#load
#data after tree and stand covariates calculated and filtered
load(file = "./data/formatted/incr_calcov")

#forest service predicts change in squared inside bark diameter
#TODO convert ring width from mm to inches
#RW = RW * 0.0393701
incr_calcov <- incr_calcov %>%
  mutate(dds = (2*RW*0.0393701)^2)

#add climate variables
#and make sure it is reproducible in case more are needed

#already have some climate information
processed.path <- "./data/formatted/"
ppt.extr <- read.csv(paste(processed.path,"ppt_extr.csv",sep=''), header = T)
tmin.extr <- read.csv(paste(processed.path,"tmin_extr.csv",sep=''), header = T)
tmax.extr <- read.csv(paste(processed.path,"tmax_extr.csv",sep=''), header = T)

#for all trees
#same dataset LAT/LONG used to extract climate
all_trees <- unique(final_trees$TRE_CN)

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

all_ppt <- clim_col(ppt.extr,clim_var = "ppt_",TRE_CN = all_trees)
all_tmin <- clim_col(tmin.extr,clim_var = "tmin_",TRE_CN = all_trees)
all_tmax <- clim_col(tmax.extr,clim_var = "tmax_",TRE_CN = all_trees)

all_clim <- full_join(all_ppt,all_tmin, by = c("TRE_CN","Year")) %>%
  full_join(.,all_tmax,by = c("TRE_CN","Year"))

all_clim$Year <- as.integer(all_clim$Year)
data_all <- full_join(incr_calcov,all_clim, by = c("TRE_CN","Year"))

save(data_all, file = "./data/formatted/data_all.Rdata")

#check
length(unique(data_all$TRE_CN)) #504

#make seasonal climate variables
#refer to climate-growth analysis
#first filter by species

#DF
data_all_df <- data_all %>%
  filter(SPCD == 202)
length(unique(data_all_df$TRE_CN)) #131

#total ppt
#1 month: pOct,pDec, Jun, Jul
data_all_df <- data_all_df %>%
  group_by(TRE_CN) %>%
  arrange(Year) %>%
  mutate(ppt_pOct = lag(ppt_Oct),
         ppt_pDec = lag(ppt_Dec))

#3 month: pJun-pAug, pAug-pOct, May-Jul
data_all_df <- data_all_df %>%
  group_by(TRE_CN) %>%
  arrange(Year) %>%
  mutate(ppt_pJunAug = lag(ppt_Jun) + lag(ppt_Jul) + lag(ppt_Aug),
         ppt_pAugOct = lag(ppt_Aug) + lag(ppt_Sep) + lag(ppt_Oct),
         ppt_MayJul = ppt_May + ppt_Jun + ppt_Jul)

#6 month + : pNov, Jul, wateryr
data_all_df <- data_all_df %>%
  group_by(TRE_CN) %>%
  arrange(Year) %>%
  mutate(ppt_pJunNov = lag(ppt_Jun) + lag(ppt_Jul) + lag(ppt_Aug) + lag(ppt_Sep) + lag(ppt_Oct) + lag(ppt_Nov),
         ppt_FebJul = ppt_Feb + ppt_Mar + ppt_Apr + ppt_May + ppt_Jun + ppt_Jul,
         wateryr = lag(ppt_Oct) + lag(ppt_Nov)+ lag(ppt_Dec)+ ppt_Jan+ ppt_Feb + ppt_Mar + ppt_Apr + ppt_May + ppt_Jun + ppt_Jul + ppt_Aug + ppt_Sep)

#average temp
#1 month: Feb, Jul
#Feb tmin
#Jul tmax

#3 month: tmax_Jun-Aug, tmax_pJul-pSep, tmin_Jan-Mar
data_all_df <- data_all_df %>%
  group_by(TRE_CN) %>%
  arrange(Year) %>%
  mutate(tmax_pJulSep = (lag(tmax_Jul) + lag(tmax_Aug) + lag(tmax_Sep))/3,
         tmax_JunAug = (tmax_Jun + tmax_Jul + tmax_Aug)/3,
         tmin_JanMar = (tmin_Jan + tmin_Feb + tmin_Mar)/3,
         tmax_JanMar = (tmax_Jan + tmax_Feb + tmax_Mar)/3)

#6 month + : Jun, Jul
data_all_df <- data_all_df %>%
  group_by(TRE_CN) %>%
  arrange(Year) %>%
  mutate(tmax_JanJun = (tmax_Jan + tmax_Feb + tmax_Mar + tmax_Apr + tmax_May + tmax_Jun)/6,
         tmax_FebJul = (tmax_Feb + tmax_Mar + tmax_Apr + tmax_May + tmax_Jun + tmax_Jul)/6,
         tmin_JanJun = (tmin_Jan + tmin_Feb + tmin_Mar + tmin_Apr + tmin_May + tmin_Jun)/6)

save(data_all_df, file = "./data/formatted/data_all_df.Rdata")

##PP
data_all_pp <- data_all %>%
  filter(SPCD == 122)
length(unique(data_all_pp$TRE_CN)) #73

#total ppt
#1 month: ,pDec, Jun, Jul, pOct
data_all_pp <- data_all_pp %>%
  group_by(TRE_CN) %>%
  arrange(Year) %>%
  mutate(ppt_pOct = lag(ppt_Oct),
         ppt_pDec = lag(ppt_Dec))

#3 month: pAug-pOct, pOct-pDec, pNov-Jan, May-Jul
data_all_pp <- data_all_pp %>%
  group_by(TRE_CN) %>%
  arrange(Year) %>%
  mutate(ppt_pOctDec = lag(ppt_Oct) + lag(ppt_Nov) + lag(ppt_Dec),
         ppt_pAugOct = lag(ppt_Aug) + lag(ppt_Sep) + lag(ppt_Oct),
         ppt_pNovJan = lag(ppt_Nov) + lag(ppt_Dec) + ppt_Jan,
         ppt_MayJul = ppt_May + ppt_Jun + ppt_Jul)

#6 month + : Jan, wateryr
data_all_pp <- data_all_pp %>%
  group_by(TRE_CN) %>%
  arrange(Year) %>%
  mutate(ppt_pAugJan = lag(ppt_Aug) + lag(ppt_Sep) + lag(ppt_Oct) + lag(ppt_Nov) + lag(ppt_Dec) + ppt_Jan,
         wateryr = lag(ppt_Oct) + lag(ppt_Nov)+ lag(ppt_Dec)+ ppt_Jan+ ppt_Feb + ppt_Mar + ppt_Apr + ppt_May + ppt_Jun + ppt_Jul + ppt_Aug + ppt_Sep)

#average temp
#1 month: Jun
#Jun tmax

#3 month: tmax_Jun-Aug
data_all_pp <- data_all_pp %>%
  group_by(TRE_CN) %>%
  arrange(Year) %>%
  mutate(tmax_JunAug = (tmax_Jun + tmax_Jul + tmax_Aug)/3)

save(data_all_pp, file = "./data/formatted/data_all_pp.Rdata")

#ES
data_all_es <- data_all %>%
  filter(SPCD == 93)
length(unique(data_all_es$TRE_CN)) #50

#total ppt
#1 month: Jul, Apr

#3 month: pJun-pAug, Jun-Aug, pJul-pSep, pOct-pDec
data_all_es <- data_all_es %>%
  group_by(TRE_CN) %>%
  arrange(Year) %>%
  mutate(ppt_pJunAug = lag(ppt_Jun) + lag(ppt_Jul) + lag(ppt_Aug),
         ppt_pJulSep = lag(ppt_Jul) + lag(ppt_Aug) + lag(ppt_Sep),
         ppt_JunAug = ppt_Jun + ppt_Jul+ ppt_Aug,
         ppt_pOctDec = lag(ppt_Oct) + lag(ppt_Nov) + lag(ppt_Dec))

#6 month + : pNov, wateryr
data_all_es <- data_all_es %>%
  group_by(TRE_CN) %>%
  arrange(Year) %>%
  mutate(ppt_pJunNov = lag(ppt_Jun) + lag(ppt_Jul) + lag(ppt_Aug) + lag(ppt_Sep) + lag(ppt_Oct) + lag(ppt_Nov),
         wateryr = lag(ppt_Oct) + lag(ppt_Nov)+ lag(ppt_Dec)+ ppt_Jan+ ppt_Feb + ppt_Mar + ppt_Apr + ppt_May + ppt_Jun + ppt_Jul + ppt_Aug + ppt_Sep)

#average temp
#1 month: tmax_pAug, Feb, Mar
#Feb tmin
#Mar tmax
data_all_es <- data_all_es %>%
  group_by(TRE_CN) %>%
  arrange(Year) %>%
  mutate(tmax_pAug = lag(tmax_Aug))

#3 month: tmin_Feb-Apr, tmax_pJul-pSep, tmin_Jan-Mar
data_all_es <- data_all_es %>%
  group_by(TRE_CN) %>%
  arrange(Year) %>%
  mutate(tmax_pJulSep = (lag(tmax_Jul) + lag(tmax_Aug) + lag(tmax_Sep))/3,
         tmin_FebApr = (tmin_Feb + tmin_Mar + tmin_Apr)/3,
         tmax_FebApr = (tmax_Feb + tmax_Mar + tmax_Apr)/3,
         tmin_JanMar = (tmin_Jan + tmin_Feb + tmin_Mar)/3)

#6 month + : tmin_Apr
data_all_es <- data_all_es %>%
  group_by(TRE_CN) %>%
  arrange(Year) %>%
  mutate(tmin_pNovApr = (lag(tmin_Nov) + lag(tmin_Dec) + tmin_Jan + tmin_Feb + tmin_Mar + tmin_Apr)/6,
         tmax_pNovApr = (lag(tmax_Nov) + lag(tmax_Dec) + tmax_Jan + tmax_Feb + tmax_Mar + tmax_Apr)/6)

save(data_all_es, file = "./data/formatted/data_all_es.Rdata")

#filtering
#by species
#only going back 30 yrs - 1958
#need response: RW, dds
#fixed effects: DBH (DIA), CR/CR_weib, PCCF, CCF, BAL, Climate
#random effect: TRE_CN, Year
