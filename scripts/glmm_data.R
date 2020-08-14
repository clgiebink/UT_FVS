#finish off dataframe
#Courtney Giebink
#clgiebink@gmail.com
#July 19, 2019

library(tidyverse)

#load
#data after tree and stand covariates calculated and filtered
load(file = "./data/formatted/incr_calcov")

#forest service predicts change in squared inside bark diameter
#convert ring width from mm to inches
#RW = RW * 0.0393701
#DG = 2*RW
#dds = (dib + (2*RW))^2 - dib^2
#dib = DBH/k
#k = 1/BRATIO
#BRATIO = b1 + b2/(DBH^exp)
bratio_df <- data.frame(species=c(93,202,122,15,19,65,96,106,108,133,321),
                        #93=ES,202=DF,122=PP,15=WF,19=AF,65=UJ,96=BS,106=PI,108=LP,133=PM,321=OH
                        b1 = c(0.9502,0.867,0.8967,0.890,0.890,0.9002,0.9502,0.9002,0.9625,0.9002,0.93789),
                        b2 = c(-0.2528, 0, -0.4448,0,0,-0.3089,-0.2528,-0.3089,-0.1141,-0.3089,-0.24096),
                        exp = c(1,0,1,0,0,1,1,1,1,1,1)) #can add more species later 
for(i in 1:nrow(incr_calcov)){
  Species <- incr_calcov$SPCD[i]
  #BRATIO = b1+b2/(DBH^exp)
  b1 <- bratio_df$b1[bratio_df$species == Species]
  b2 <- bratio_df$b2[bratio_df$species == Species]
  exp <- bratio_df$exp[bratio_df$species == Species]
  BRATIO <- b1 + b2/(incr_calcov$DIA_C[i]^exp) #exp determines if use equation 4.2.1/3 or 4.2.2 in UT variant guide
  #k = 1/BRATIO
  k <- 1/BRATIO
  dib <- incr_calcov$DIA_C[i]/k
  #don't forget mm to inches
  incr_calcov$dds[i] <- (dib + (2*incr_calcov$RW[i]*0.0393701))^2 - dib^2
}

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


#tranform aspect where appropriate
#Na-> 0 when slope is less than or equal to 5
data_all <- data_all %>%
  mutate(tASPECT = ifelse(is.na(ASPECT) & SLOPE <= 5,0,ASPECT)) %>%
  #NA values will be removed from calibration
  #APSECT converted to radians for sin and cos transformation
  mutate(radians = tASPECT * (pi/180)) %>%
  mutate(sin = sin(radians - 0.7854) * SLOPE,
         cos = cos(radians - 0.7854) * SLOPE)

save(data_all, file = "./data/formatted/data_all.Rdata")

#check
length(unique(data_all$TRE_CN)) #568

#replace site index where fixed
#see prac.R (site index check)
load("./data/formatted/cal_si.csv")
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

data_all <- fix_si(data = data_all, new_si = si_match)
#fixed 28 trees

#filter later
#take out trees where SI is not fixed
cal_si <- cal_si %>%
  filter(!(TRE_CN %in% si_match$TRE_CN))
save(cal_si,file = "./data/formatted/cal_si.Rdata")
save(data_all,file = "./data/formatted/data_all.Rdata")

#make seasonal climate variables
#refer to climate-growth analysis
#first filter by species

#DF
data_all_df <- data_all %>%
  filter(SPCD == 202)
length(unique(data_all_df$TRE_CN)) #136

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

#16 months: previous June to current Sept
data_all_df <- data_all_df %>%
  group_by(TRE_CN) %>%
  arrange(Year) %>%
  mutate(ppt_pJunSep = lag(ppt_Jun) + lag(ppt_Jul) + lag(ppt_Aug) + lag(ppt_Sep) + lag(ppt_Oct) +
           lag(ppt_Nov)+ lag(ppt_Dec)+ ppt_Jan+ ppt_Feb + ppt_Mar + ppt_Apr +
           ppt_May + ppt_Jun + ppt_Jul + ppt_Aug + ppt_Sep)

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
length(unique(data_all_pp$TRE_CN)) #87

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
         wateryr = lag(ppt_Oct) + lag(ppt_Nov)+ lag(ppt_Dec)+ ppt_Jan+ ppt_Feb + ppt_Mar + ppt_Apr + ppt_May + ppt_Jun + ppt_Jul + ppt_Aug + ppt_Sep,
         ppt_pAugJul = lag(ppt_Aug) + lag(ppt_Sep) + lag(ppt_Oct) + lag(ppt_Nov)+ lag(ppt_Dec)+ ppt_Jan+ ppt_Feb + ppt_Mar + ppt_Apr + ppt_May + ppt_Jun + ppt_Jul)

#16 months: previous June to current Sept
data_all_pp <- data_all_pp %>%
  group_by(TRE_CN) %>%
  arrange(Year) %>%
  mutate(ppt_pJunSep = lag(ppt_Jun) + lag(ppt_Jul) + lag(ppt_Aug) + lag(ppt_Sep) + lag(ppt_Oct) +
           lag(ppt_Nov)+ lag(ppt_Dec)+ ppt_Jan+ ppt_Feb + ppt_Mar + ppt_Apr +
           ppt_May + ppt_Jun + ppt_Jul + ppt_Aug + ppt_Sep)

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
length(unique(data_all_es$TRE_CN)) #95

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
         wateryr = lag(ppt_Oct) + lag(ppt_Nov)+ lag(ppt_Dec)+ ppt_Jan+ ppt_Feb + ppt_Mar + ppt_Apr + ppt_May + ppt_Jun + ppt_Jul + ppt_Aug + ppt_Sep,
         ppt_pAugJul = lag(ppt_Aug) + lag(ppt_Sep) + lag(ppt_Oct) + lag(ppt_Nov)+ lag(ppt_Dec)+ ppt_Jan+ ppt_Feb + ppt_Mar + ppt_Apr + ppt_May + ppt_Jun + ppt_Jul + ppt_Aug + ppt_Sep)

#16 months: previous June to current Sept
data_all_es <- data_all_es %>%
  group_by(TRE_CN) %>%
  arrange(Year) %>%
  mutate(ppt_pJunSep = lag(ppt_Jun) + lag(ppt_Jul) + lag(ppt_Aug) + lag(ppt_Sep) + lag(ppt_Oct) +
           lag(ppt_Nov)+ lag(ppt_Dec)+ ppt_Jan+ ppt_Feb + ppt_Mar + ppt_Apr +
           ppt_May + ppt_Jun + ppt_Jul + ppt_Aug + ppt_Sep)

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
