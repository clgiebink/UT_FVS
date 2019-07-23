#finish off dataframe
#Courtney Giebink
#clgiebink@gmail.com
#July 19, 2019

library(tidyverse)

#load
#data after tree and stand covariates calculated and filtered
load(file = "./data/formatted/data")

#add climate variables
#and make sure it is reproducible in case more are needed

#already have some climate information
processed.path <- "./data/formatted/"
ppt.extr <- read.csv(paste(processed.path,"ppt_extr.csv",sep=''), header = T)
tmin.extr <- read.csv(paste(processed.path,"tmin_extr.csv",sep=''), header = T)
tmax.extr <- read.csv(paste(processed.path,"tmax_extr.csv",sep=''), header = T)

#for all trees
all_trees <- unique(data$TRE_CN)

clim_col <- function(PRISM,clim_var,start_yr,end_yr,TRE_CN){
  #make a list
  #each item is a year
  #each item has 1-13 columns (TRE_CN + 12 months)
  
  #ppt
  #empty list
  climate_list <- list()
  start_col <- 1
  end_col <- 12
  n <- ncol(data)/12
  for(i in 1:n){ #number of years (1895:2000)
    climate_list[[i]] <- ppt.extr_test[,start_col:end_col]
    start_col <- start_col + 12
    end_col <- end_col + 12
  }
  
  months <- c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")
  climate_list <- lapply(climate_list, setNames, nm = paste0(clim_var,months))
  
  climate_list <- mapply(cbind, climate_list, "TRE_CN"=TRE_CN, SIMPLIFY=F)
  
  years <- seq(from=start_yr,to=end_yr,by=1)
  names(climate_list) <- years
  
  climate_stack <- bind_rows(climate_list, .id = "Year")
  return(climate_stack)
}

all_ppt <- clim_col(ppt.extr,clim_var = "ppt_",start_yr = 1895,end_yr = 2000,TRE_CN = all_trees)
all_tmin <- clim_col(tmin.extr,clim_var = "tmin_",start_yr = 1895,end_yr = 2000,TRE_CN = df_trees)
all_tmax <- clim_col(tmax.extr,clim_var = "tmax_",start_yr = 1895,end_yr = 2000,TRE_CN = df_trees)

all_clim <- full_join(all_ppt,all_tmin, by = c("TRE_CN","Year")) %>%
  full_join(.,all_tmax,by = c("TRE_CN","Year"))

data_all <- left_join(data,all_clim, by = c("TRE_CN","Year"))


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

