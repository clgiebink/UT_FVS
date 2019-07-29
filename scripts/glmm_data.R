#finish off dataframe
#Courtney Giebink
#clgiebink@gmail.com
#July 19, 2019

library(tidyverse)

#load
#data after tree and stand covariates calculated and filtered
load(file = "./data/formatted/incr_calcov")

#forest service predicts change in squared inside bark diameter
incr_calcov <- incr_calcov %>%
  mutate(dds = (2*RW)^2)

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
#probably should be:
all_trees <- unique(incr_calcov$TRE_CN)

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



#make seasonal climate variables
#refer to climate-growth analysis
#first filter by species

#DF
data_all_df <- data_all %>%
  filter(SPCD == 202)

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
         tmin_JanMar = (tmin_Jan + tmin_Feb + tmin_Mar)/3)

#6 month + : Jun, Jul
data_all_df <- data_all_df %>%
  group_by(TRE_CN) %>%
  arrange(Year) %>%
  mutate(tmax_JanJun = (tmax_Jan + tmax_Feb + tmax_Mar + tmax_Apr + tmax_May + tmax_Jun)/6,
         tmax_FebJul = (tmax_Feb + tmax_Mar + tmax_Apr + tmax_May + tmax_Jun + tmax_Jul)/6,
         tmin_JanJun = (tmin_Jan + tmin_Feb + tmin_Mar + tmin_Apr + tmin_May + tmin_Jun)/6)

##PP
data_all_pp <- data_all %>%
  filter(SPCD == 122)

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
  mutate(ppt_pJunJan = lag(ppt_Aug) + lag(ppt_Sep) + lag(ppt_Oct) + lag(ppt_Nov) + lag(ppt_Dec) + ppt_Jan,
         wateryr = lag(ppt_Oct) + lag(ppt_Nov)+ lag(ppt_Dec)+ ppt_Jan+ ppt_Feb + ppt_Mar + ppt_Apr + ppt_May + ppt_Jun + ppt_Jul + ppt_Aug + ppt_Sep)

#average temp
#1 month: Jun
#Jun tmax

#3 month: tmax_Jun-Aug
data_all_pp <- data_all_pp %>%
  group_by(TRE_CN) %>%
  arrange(Year) %>%
  mutate(tmax_JunAug = (tmax_Jun + tmax_Jul + tmax_Aug)/3)


#ES
data_all_es <- data_all %>%
  filter(SPCD == 93)

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
         tmin_JanMar = (tmin_Jan + tmin_Feb + tmin_Mar)/3)

#6 month + : tmin_Apr
data_all_es <- data_all_es %>%
  group_by(TRE_CN) %>%
  arrange(Year) %>%
  mutate(tmin_pNovApr = (lag(tmin_Nov) + lag(tmin_Dec) + tmin_Jan + tmin_Feb + tmin_Mar + tmin_Apr)/6)

#filtering
#by species
#only going back 30 yrs - 1958
#need response: RW, dds
#fixed effects: DBH (DIA), CR/CR_weib, PCCF, CCF, BAL, Climate
#random effect: TRE_CN, Year


#for each focal species 202=DF,122=PP,93=ES
#might be unnecessary
##Douglas fir
#PRISM data
ppt.extr_DF <- read.csv(paste(processed.path,"ppt_extr_DF.csv",sep=''), header = T)
tmin.extr_DF <- read.csv(paste(processed.path,"tmin_extr_DF.csv",sep=''), header = T)
tmax.extr_DF <- read.csv(paste(processed.path,"tmax_extr_DF.csv",sep=''), header = T)

df_trees <- unique(final_trees_DF$TRE_CN)

DF_ppt <- clim_col(ppt.extr_DF,clim_var = "ppt_",start_yr = 1895,end_yr = 2000,TRE_CN = df_trees)
DF_tmin <- clim_col(tmin.extr_DF,clim_var = "tmin_",start_yr = 1895,end_yr = 2000,TRE_CN = df_trees)
DF_tmax <- clim_col(tmax.extr_DF,clim_var = "tmax_",start_yr = 1895,end_yr = 2000,TRE_CN = df_trees)

df_clim <- full_join(DF_ppt,DF_tmin, by = c("TRE_CN","Year")) %>%
  full_join(.,DF_tmax,by = c("TRE_CN","Year"))

data_DF <- data[data$SPCD == 202,]

data_DF <- left_join(data_DF,df_clim, by = c("TRE_CN","Year"))


##Ponderosa pine
ppt.extr_PP <- read.csv(paste(processed.path,"ppt_extr_PP.csv",sep=''), header = T)
tmin.extr_PP <- read.csv(paste(processed.path,"tmin_extr_PP.csv",sep=''), header = T)
tmax.extr_PP <- read.csv(paste(processed.path,"tmax_extr_PP.csv",sep=''), header = T)

pp_trees <- unique(final_trees_PP$TRE_CN)

PP_ppt <- clim_col(ppt.extr_PP,clim_var = "ppt_",start_yr = 1895,end_yr = 2000,TRE_CN = pp_trees)
PP_tmin <- clim_col(tmin.extr_PP,clim_var = "tmin_",start_yr = 1895,end_yr = 2000,TRE_CN = pp_trees)
PP_tmax <- clim_col(tmax.extr_PP,clim_var = "tmax_",start_yr = 1895,end_yr = 2000,TRE_CN = pp_trees)

pp_clim <- full_join(PP_ppt,PP_tmin, by = c("TRE_CN","Year")) %>%
  full_join(.,PP_tmax,by = c("TRE_CN","Year"))

data_PP <- data[data$SPCD == 122,]

data_PP <- left_join(data_PP,pp_clim, by = c("TRE_CN","Year"))


##Engelman spruce
ppt.extr_ES <- read.csv(paste(processed.path,"ppt_extr_ES.csv",sep=''), header = T)
tmin.extr_ES <- read.csv(paste(processed.path,"tmin_extr_ES.csv",sep=''), header = T)
tmax.extr_ES <- read.csv(paste(processed.path,"tmax_extr_ES.csv",sep=''), header = T)

es_trees <- unique(final_trees_ES$TRE_CN)

ES_ppt <- clim_col(ppt.extr_ES,clim_var = "ppt_",start_yr = 1895,end_yr = 2000,TRE_CN = es_trees)
ES_tmin <- clim_col(tmin.extr_ES,clim_var = "tmin_",start_yr = 1895,end_yr = 2000,TRE_CN = es_trees)
ES_tmax <- clim_col(tmax.extr_ES,clim_var = "tmax_",start_yr = 1895,end_yr = 2000,TRE_CN = es_trees)

es_clim <- full_join(ES_ppt,ES_tmin, by = c("TRE_CN","Year")) %>%
  full_join(.,ES_tmax,by = c("TRE_CN","Year"))

data_ES <- data[data$SPCD == 93,]

data_ES <- left_join(data_ES,df_clim, by = c("TRE_CN","Year"))

