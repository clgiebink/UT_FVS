#Growth projection
#under climate change
#Courtney Giebink
#clgiebink@gmail.com
#August 2020

#climate ----
#climateNA: 
#or
#climate explorer: 
#https://climexp.knmi.nl/selectfield_cmip5.cgi?id=someone@somewhere
#can be downloaded as ASCII or NetCDF files

#climate_kah5
#script originally from kelly
#updated by Courtney Giebink
# script to readin in the downscaled climate model projections from https://gdo-dcp.ucllnl.org/downscaled_cmip_projections/#Projections:%20Complete%20Archives
# Under Subset request, I selected downscaled projections for Jan 2018 - Dec 2099 and highlighted the region of AZ for the domain
# I used the projection set: BCSD-CMIP5-Hydrology-monthly and selected maximum temperature and precipiation
# then for all the rcps, I selected "all"
# then I selected "no analysis" and "netcdf" on the last page...it took less than an hour for them to email me with a link to download the zipped data
# #######################more information from the product:
# Product:               Bias-corrected statistically downscaled GCM data
# Variables:             tasmax    
# NetCDF type:          float     
# NetCDF missing value:  1e+20     
# Period:                2010Jan through 2099Dec
# Resolution:            1/8 degree
# Latitude bounds:       (29.875, 38.125)
# Longitude bounds:      (-115.125, -108.0)
# Area within bounds:    602058 km^2 (approx)
# Dimensions:         
#   Times:                984
# Latitudes:            66
# Longitudes:           57
# Projections:          97
# 
# 
# Global attributes
#   Conventions:           GDT 1.2
# authors:               Bridget Thrasher, Ed Maurer
# description:           Bias-corrected and downscaled GCM data
# creation_date:         2012
# institution:           Climate Analytics Group, Santa Clara U.
# SurfSgnConvention:     Traditional


# Selected 
# overview:
# 1. Read in the lat long data we need to extract climate data over
# 2. create function to open netcdf, generate a raster stack of all the projections, then extract by our lat long data
# 3. output & repeat for the next climate variable

library(raster)
library(sp)
library(rgeos)
library(ggplot2)
library(reshape2)
library(ncdf4) # a must have package for opening netcdfs
library(lubridate)
library(tidync)

# 1. read in the data set that has the lat long of the plots/cores we want to extract projections from
load("./data/formatted/val_dset.Rdata")

#validation trees w/ LAT & LON
val_trees <- val_red2 %>%
  dplyr::select(LON,LAT) %>%
  distinct()

# Make lat, lon data spatial
val_tree_spat <- SpatialPointsDataFrame(coords = cbind(val_trees$LON, val_trees$LAT), 
                                        data = val_trees, 
                                        proj4string = CRS("+init=epsg:4326"))
plot(val_tree_spat)

# dont need this, but this would be the way to transform to a new projection (needed for climat NA)
#cov.data.en <- spTransform(val_tree_spat, CRSobj = CRS("+proj=lcc +lat_1=49.0 +lat_2=77.0 +lat_0=0.0 +lon_0=-95.0 +x_0=0.0 +y_0=0.0 +ellps=WGS84 + datum=WGS84 +units=m +no_defs"))



# 2. create function to open netcdf, generate a raster stack of all the projections, then extract by our lat long data

# for the precipitation:
# open the netcdf
pr_nc <- nc_open("./data/raw/climate/projections/DNSC CMIP5/hydro5/Extraction_pr.nc")
variableofinterest <- names(pr_nc$var) # get the variable of interest
ppt <- ncvar_get(pr_nc,variableofinterest) # this extracts a 4 dimensional array of data
# 4 Dimensional array:
# dim 1: long
# dim 2: lat
# dim 3: time in months (jan 2018 - Dec 2099)
# dim 4: each climate model/ensemble member
lat <- ncvar_get(pr_nc,"latitude") # get lat
lon <- ncvar_get(pr_nc, "longitude") # get long
#projection <- ncvar_get(pr_nc, "projection") # cant get the dimvar, but metadata has info on projections

dim(ppt)# look at the dimensions
nmonths <- dim(ppt)[3] # 3rd dimension is the number of months in the downscaled projections
nc_close(pr_nc) # close the netcdf file when you are done extracting


# this function takes a given projection, makes a raster stack where each raster is a month for a given climate model run projection
# and extracts the monthly time series for each lat long point of interest, then summs across year to get a data frame of
#  columns: lat   lon climate year  year.ppt
# inputs: proj = a number of which climate ensemble you want 
# ppt = the 4 D array 
# val_tree_spat = the spatial object to extract by
# nmonths = # months from the 4D array
proj <- seq(1:97) #97 ensembles
rlist <- list()
# apply funcation across all 97 projections downloaded in the netcdf
all.future.ppt <- list()
extract.yearly.ppt  <- function(proj, ppt, val_tree_spat, nmonths){ 
  for(p in 1:length(proj)){
    #all.future.ppt <- list()
    for(i in 1:nmonths){ 
      #rlist() <- list
      # make a raster for each month
      rlist[[i]] <- raster(as.matrix(ppt[,,i,p]), xmn = min(lon), xmx = max(lon), 
                           ymn = min(lat) , ymx = max(lat), 
                           crs = CRS('+init=epsg:4269'))
    }
    rast.stack <- stack(rlist)
    #plot(rast.stack[[9]]) # can plot for sanity
    #plot(val_tree_spat, add = TRUE)
    
    #tmax.rast.ll <- projectRaster(tmax.rast, crs =CRS("+init=epsg:4326") ) # dont recommend trying to change projections of the rasters, it will take much much longer to run this
    
    extracted.pts <- data.frame(raster::extract(rast.stack, val_tree_spat))
    ll.data <- as.data.frame(val_tree_spat)
    extracted.pts$lat <-ll.data$LAT # get the lat and long
    extracted.pts$lon <-ll.data$LON
    
    months <- c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")
    colnames(extracted.pts)[1:nmonths] <- paste0("ppt_", rep(2010:2099, each = 12), "_", rep(months, 90)) # note may need to change this to make more customizable
    extracted.pts.m <- extracted.pts %>%
      pivot_longer(cols = -c("lat","lon"), 
                   names_to = "year",
                   names_pattern =  ".*[_](\\d+)[_].*", #takes year out of column name
                   values_to = paste0("ppt_",months)) 
   extracted.pts.m[, 4:15][extracted.pts.m[, 4:15] >= 1e+20] <- NA #replace values with NA
   yearly.ppt <- extracted.pts.m %>%
      group_by(lat,lon) %>%
      arrange(year) %>%
      mutate(ppt_pJunSep = lag(ppt_Jun) + lag(ppt_Jul) + lag(ppt_Aug) + lag(ppt_Sep) + lag(ppt_Oct) +
               lag(ppt_Nov)+ lag(ppt_Dec)+ ppt_Jan+ ppt_Feb + ppt_Mar + ppt_Apr +
               ppt_May + ppt_Jun + ppt_Jul + ppt_Aug + ppt_Sep)
    all.future.ppt[[p]] <- yearly.ppt
  }
  all.future.ppt
}

# for loop is slightly faster, so we will use that, but this takes a bit
# extracts for all 97 projections 
all.future.ppt <- extract.yearly.ppt(proj = proj, ppt = ppt, val_tree_spat = val_tree_spat, nmonths = nmonths)


# for temperature
# open the netcdf
tmx_nc <- nc_open("./data/raw/climate/projections/DNSC CMIP5/hydro5/Extraction_tasmax.nc")
variableofinterest <- names(tmx_nc$var)
Tmax <- ncvar_get(tmx_nc,variableofinterest)
# 4 Dimensional array:
# dim 1: long
# dim 2: lat
# dim 3: time in months (jan 2010 - Dec 2099)
# dim 4: each climate model + ensemble member
lat <- ncvar_get(tmx_nc,"latitude")
lon <- ncvar_get(tmx_nc, "longitude")
#projection <- ncvar_get(tmx_nc, "projection") # cant get the dimvar

dim(Tmax)
nmonths <- dim(Tmax)[3] # 3rd dimension is the number of months in the downscaled projections
nc_close(tmx_nc)


# open all the ncs
rlist <- list()
all.future.tmax <- list()
extract.yearly.tmax  <- function(proj, Tmax, val_tree_spat, nmonths){ 
  for(p in 1:length(proj)){
    #all.future.tmax <- list()
    for(i in 1:nmonths){
      #rlist() <- list
      # make a raster for each month
      rlist[[i]] <- raster(as.matrix(Tmax[,,i,p]), xmn = min(lon), xmx = max(lon), 
                           ymn = min(lat) , ymx = max(lat), 
                           crs = CRS('+init=epsg:4269'))
    }
    
    rast.stack <- stack(rlist)
    #plot(rast.stack[[9]])
    #plot(val_tree_spat, add = TRUE)
    
    #tmax.rast.ll <- projectRaster(tmax.rast, crs =CRS("+init=epsg:4326") )
    # extracted.pts <- list()
    extracted.pts<- data.frame(raster::extract(rast.stack, val_tree_spat))
    
    ll.data <- as.data.frame(val_tree_spat)
    extracted.pts$lat <-ll.data$LAT # get the lat and long
    extracted.pts$lon <-ll.data$LON
    
    months <- c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")
    colnames(extracted.pts)[1:nmonths] <- paste0("tmax_", rep(2010:2099, each = 12), "_", rep(months, 90)) # note may need to change this to make more customizable
    extracted.pts.m <- extracted.pts %>%
      pivot_longer(cols = -c("lat","lon"), 
                   names_to = "year",
                   names_pattern =  ".*[_](\\d+)[_].*", #takes year out of column name
                   values_to = paste0("tmax_",months)) 
    extracted.pts.m[, 4:15][extracted.pts.m[, 4:15] >= 1e+20] <- NA #replace values with NA
    #tmax_JunAug
    #tmax_FebJul
    #tmax_pAug
    yearly.tmax <- extracted.pts.m %>%
      group_by(lat,lon) %>%
      arrange(year) %>%
      mutate(tmax_JunAug = tmax_Jun + tmax_Jul + tmax_Aug,
             tmax_FebJul = (tmax_Feb + tmax_Mar + tmax_Apr + tmax_May + tmax_Jun + tmax_Jul)/6,
             tmax_pAug = lag(tmax_Aug))
    all.future.tmax[[p]] <- yearly.tmax
  }
  all.future.tmax
}

#run
all.future.tmax <- extract.yearly.tmax(proj = proj, Tmax = Tmax, val_tree_spat = val_tree_spat, nmonths = nmonths)

# convert to df
all.tmax.df <- do.call(rbind, all.future.tmax)
all.ppt.df <- do.call(rbind, all.future.ppt)

# because the projection labels were not working for this, I need to read in a text file with all the projection names:
proj <- read.delim("./data/raw/climate/projections/DNSC CMIP5/hydro5/Projections5.txt", header = FALSE)

#add the projection names to the tmax and ppt data frames
all.tmax.df$proj <- rep(proj$V1, sapply(all.future.tmax , nrow))
all.ppt.df$proj <- rep(proj$V1, sapply(all.future.ppt , nrow))

ppt.models <- all.ppt.df %>% tidyr::separate(proj, sep = -5, into = c("modelrun", "rcp")) #%>% 
#tidyr::separate(modelrun, sep = "-", into = c("model", "run"))
tmax.models <- all.tmax.df %>% tidyr::separate(proj, sep = -5, into = c("modelrun", "rcp")) 
summary.tas <- tmax.models %>% dplyr::group_by(lat, lon, year, rcp) %>% dplyr::summarise(sd = sd(tmax.fall.spr, na.rm = TRUE), 
                                                                                         mean = mean(tmax.fall.spr, na.rm = TRUE))

summary.ppt <- ppt.models %>% dplyr::group_by(lat, lon, year, rcp) %>% dplyr::summarise(sd = sd(ppt, na.rm = TRUE), 
                                                                                        mean = mean(ppt, na.rm = TRUE))

# okay lets merge these together:
future_clim <- merge(ppt.models, tmax.models, by = c("lat", "lon", "year", "modelrun","rcp"))

save(future_clim,file = "./data/formatted/future_clim.Rdata")

#maybe merge

#project growth ----

#load trees
load('./data/formatted/val_dset.Rdata')
#or select
#alive
#no missing data
#

#load/get nonfocal trees

#calculate density

#calculate solar radiation


project_clim <- function(data,mod_df,mod_pp,mod_es,sp_stats,nonfocal,bratio,ccf_df,CR_WEIB_df,climate) {
  #parameters:
  #data - trees from FIADB
  data_rep <- data %>%
    ungroup() %>%
    dplyr::select(PLT_CN, TRE_CN, SPCD, SUBP, MEASYEAR, 
                  TPA_UNADJ, DESIGNCD,
                  ASPECT,SLOPE,sin,cos,LAT,LON,ELEV,
                  FVS_LOC_CD,SDImax,SICOND_c,solrad_MayAug) %>%
    mutate(Year = NA,
           DIA = NA,
           CCF = NA,
           PCCF = NA,
           BAL = NA,
           SDI = NA,
           CR_fvs = NA)
  data_rep <- data_rep %>% 
    group_by(TRE_CN) %>%
    slice(rep(1:n(), each = 20)) %>% #grow 20 years into the future?
    mutate(Year = (MEASYEAR[1]:(MEASYEAR[1]+19))) %>% #repeated data frame ready  to fill in
    ungroup()
  ##density for new data in MEASYEAR will already be calculated
  #fill in DIA,CCF, PCCF, BAL, SDI, CR_weib, where year = measyear
  #climate projections
  #TODO CMIP - 5? or 6?
  #fill climate
  climate <- climate %>%
    ungroup()
  for(i in 1:nrow(data_rep)){
    TRE_CN <- data_rep$TRE_CN[i]
    if(data_rep$Year[i] == data_rep$MEASYEAR[i]){
      data_rep$DIA[i] <- data$DIA[data$TRE_CN == TRE_CN]
      data_rep$CCF[i] <- data$CCF[data$TRE_CN == TRE_CN]
      data_rep$PCCF[i] <- data$PCCF[data$TRE_CN == TRE_CN]
      data_rep$BAL[i] <- data$BAL[data$TRE_CN == TRE_CN]
      data_rep$SDI[i] <- data$SDI[data$TRE_CN == TRE_CN]
      data_rep$CR_fvs[i] <- data$CR[data$TRE_CN == TRE_CN]
    }
    #add climate
    #match over tree and year
    data_rep$ppt_pJunSep[i] <- climate$ppt_pJunSep[climate$TRE_CN == TRE_CN &
                                                        climate$Year == data_rep$Year[i]]
    data_rep$tmax_JunAug[i] <- climate$tmax_JunAug[climate$TRE_CN == TRE_CN &
                                                        climate$Year == data_rep$Year[i]]
    data_rep$tmax_FebJul[i] <- climate$tmax_FebJul[climate$TRE_CN == TRE_CN &
                                                        climate$Year == data_rep$Year[i]]
    data_rep$tmax_pAug[i] <- climate$tmax_pAug[climate$TRE_CN == TRE_CN &
                                                    climate$Year == data_rep$Year[i]]
  }
  
  #mod_obj - list of model objects for each species
  #sp_stats - list of species statistics (mean and standard deviation) for each covariate
  ##will be used to standardize data covariates
  #nonfocal - trees on the same plot as focal trees
  nonfocal <- nonfocal %>%
    ungroup()
  #TODO expand nonfocal -> will fill in...how??
  ##will be used to compute density parameters
  #bratio - dataframe of bark ratio constants for equation
  #ccf - dataframe of crown competiton factor constants for equation
  #CR_weib_df - dataframe of crown ratio constants for equation
  #climate - dataframe of climate variables (ppt & tmax) for each tree
  
  #empty dataframe to add results
  pred_df <- data_rep[FALSE,]
  
  # loop over plot
  # to calculate density after MEASYR
  for(i in unique(data_rep$PLT_CN)) {
    #create plot dataframe to predict growth
    plt_df <- data_rep[data_rep$PLT_CN == i,]
    #assign projection start year
    growthyr <- min(plt_df$Year) # assumes all trees on a plot have same MEASYEAR
    while (growthyr <= max(plt_df$Year)-1){ #stops the year before last projection year
      #filter dataframe for year of projection
      plt_yr_df <- plt_df %>%
        filter(Year == growthyr)
      
      for(i in 1:nrow(plt_yr_df)) {#for each tree on plot in given year
        Species <- plt_yr_df$SPCD[i]
        TRE_CN <- plt_yr_df$TRE_CN[i]
        
        #select model to predict growth
        #models are species specific
        if(Species == 202){mod_obj <- mod_df} 
        if(Species == 122){mod_obj <- mod_pp} 
        if(Species == 93){mod_obj <- mod_es} 
        
        #standardize covariates
        #get mean/sd for each species
        if(Species == 202){para_std <- sp_stats$sp_202}
        if(Species == 122){para_std <- sp_stats$sp_122}
        if(Species == 93) {para_std <- sp_stats$sp_93}
        
        #first row is mean, second row is sd
        #the inputs of the equation are from a list so output needs to be unlisted
        plt_yr_df$z.DIA_C[i] = unlist((plt_yr_df[i,"DIA"] - para_std[1,"DIA_C"]) / para_std[2,"DIA_C"])
        plt_yr_df$z.CR_fvs[i] = unlist((plt_yr_df[i,"CR_fvs"] - para_std[1,"CR_fvs"]) / para_std[2,"CR_fvs"])
        plt_yr_df$z.BAL[i] = unlist((plt_yr_df[i,"BAL"] - para_std[1,"BAL"]) / para_std[2,"BAL"])
        plt_yr_df$z.CCF[i] = unlist((plt_yr_df[i,"CCF"] - para_std[1,"CCF"]) / para_std[2,"CCF"])
        plt_yr_df$z.PCCF[i] = unlist((plt_yr_df[i,"PCCF"] - para_std[1,"PCCF"]) / para_std[2,"PCCF"])
        plt_yr_df$z.SDI[i] = unlist((plt_yr_df[i,"SDI"] - para_std[1,"SDI"]) / para_std[2,"SDI"])
        plt_yr_df$z.SICOND[i] = unlist((plt_yr_df[i,"SICOND_c"] - para_std[1,"SICOND"]) / para_std[2,"SICOND"])
        plt_yr_df$z.SLOPE[i] = unlist((plt_yr_df[i,"SLOPE"] - para_std[1,"SLOPE"]) / para_std[2,"SLOPE"])
        plt_yr_df$z.sin[i] = unlist((plt_yr_df[i,"sin"] - para_std[1,"sin"]) / para_std[2,"sin"])
        plt_yr_df$z.cos[i] = unlist((plt_yr_df[i,"cos"] - para_std[1,"cos"]) / para_std[2,"cos"])
        plt_yr_df$z.solrad_MayAug[i] = unlist((plt_yr_df[i,"solrad_MayAug"] - para_std[1,"solrad_MayAug"]) / para_std[2,"solrad_MayAug"])
        plt_yr_df$z.ppt_pJunSep[i] = unlist((plt_yr_df[i,"ppt_pJunSep"] - 
                                               para_std[1,"ppt_pJunSep"]) / para_std[2,"ppt_pJunSep"])
        #temperature different for each species
        sp_202 <- sp_stats$sp_202
        sp_122 <- sp_stats$sp_122
        sp_93 <- sp_stats$sp_93
        plt_yr_df$z.tmax_FebJul[i] = unlist((plt_yr_df[i,"tmax_FebJul"] -
                                               sp_202[1,"tmax_FebJul"]) /
                                              sp_202[2,"tmax_FebJul"])
        plt_yr_df$z.tmax_JunAug[i] = unlist((plt_yr_df[i,"tmax_JunAug"] -
                                               sp_122[1,"tmax_JunAug"]) /
                                              sp_122[2,"tmax_JunAug"])
        plt_yr_df$z.tmax_pAug[i] = unlist((plt_yr_df[i,"tmax_pAug"] -
                                             sp_93[1,"tmax_pAug"]) / 
                                            sp_93[2,"tmax_pAug"])
        #TODO there must be a better way to do this? for loop?
        
        #predict
        # modobj will change with different species
        plt_yr_df$log_dds[i] <- predict(object = mod_obj, data = plt_yr_df[i,], 
                                        re.form = NA, type = "response")
        #re.form specify random effects to include
        ##NA include none
        
        #see page 53 of prognosis model
        #DG = sqrt((DBH/k)^2 + dds) - (DBH/k)
        # k = 1/BRATIO
        #see UT variant guide
        #BRATIO = b1+b2/(DBH^exp)
        b1 <- bratio$b1[bratio$species == Species]
        b2 <- bratio$b2[bratio$species == Species]
        exp <- bratio$exp[bratio$species == Species]
        BRATIO <- b1 + b2/(plt_yr_df$DIA[i]^exp) 
        #exp determines if use equation 4.2.1/3 or 4.2.2 in UT variant guide
        k <- 1/BRATIO 
        #DG = sqrt((DBH/k)^2 + dds - (DBH/k))
        plt_yr_df$DG[i] <- k * (sqrt((plt_yr_df$DIA[i]/k)^2 + exp(plt_yr_df$log_dds[i])) -
                                  (plt_yr_df$DIA[i]/k))
      }
      
      #so DBH0 + k*DG = DBH
      plt_yr2_df <- plt_df %>%
        filter(Year == (growthyr+1))%>%
        mutate(DIA = plt_yr_df$DIA + plt_yr_df$DG)
      
      #TODO grow nonfocal trees
      
      #join with nonfocal
      #fitler for trees on plots in validation set in same year
      non_foc_filt <- nonfocal %>%
        dplyr::select(PLT_CN, TRE_CN, SUBP, SPCD,TPA_UNADJ,Year,DIA_int) %>%
        mutate(DIA = DIA_int) %>%
        filter(PLT_CN == plt_yr2_df$PLT_CN[1],
               Year == plt_yr2_df$Year[1])
      #join
      density_cal <- bind_rows(plt_yr2_df,non_foc_filt)
      
      #compute density parameters
      #ccf
      for(i in 1:nrow(density_cal)){
        Species <- density_cal$SPCD[i]
        r1 <- ccf_df$r1[ccf_df$species == Species]
        r2 <- ccf_df$r2[ccf_df$species == Species]
        r3 <- ccf_df$r3[ccf_df$species == Species]
        r4 <- ccf_df$r4[ccf_df$species == Species]
        r5 <- ccf_df$r5[ccf_df$species == Species]
        dbrk <- ccf_df$dbrk[ccf_df$species == Species]
        if(Species == 475){
          ifelse(density_cal$DIA[i] < dbrk,
                 CCF_t <- density_cal$DIA[i]*(r1+r2+r3),
                 CCF_t <- r1 + (r2 * density_cal$DIA[i]) + (r3 * density_cal$DIA[i]^2))
        }
        if(Species != 475){
          ifelse(is.na(density_cal$DIA[i]), 
                 CCF_t <- NA,
                 ifelse(density_cal$DIA[i] <= 0.1, 
                        CCF_t <- 0.0001,
                        ifelse(density_cal$DIA[i] < dbrk, 
                               CCF_t <- r4 * (density_cal$DIA[i]^r5),
                               CCF_t <- r1 + (r2 * density_cal$DIA[i]) + (r3 * density_cal$DIA[i]^2))))
        }
        density_cal$CCF_t[i] <- CCF_t
      }
      
      density_cal <- density_cal %>%
        group_by(PLT_CN,SUBP,Year) %>%
        mutate(PCCF = sum(CCF_t * (TPA_UNADJ * 4), na.rm = TRUE))
      
      #stand CCF = sum(CCFt on a plot) on a per acre basis
      #TPA measured on a plot/stand level
      density_cal <- density_cal %>%
        group_by(PLT_CN,Year) %>%
        mutate(CCF = sum(CCF_t * TPA_UNADJ,na.rm = TRUE))
      
      #bal
      density_cal <- density_cal %>%
        mutate(BA_pa = ((DIA^2) * 0.005454) * TPA_UNADJ)
      
      #rank trees per year per plot
      #sum BApa of trees larger on the same plot in same year
      density_cal <- density_cal %>%
        group_by(PLT_CN,Year) %>%
        mutate(rank_pltyr = rank(DIA, na.last = TRUE, ties.method = "min")) %>%
        #min assigns lowest value to ties (1,2,3,3,5,6)
        mutate(BAL = map_dbl(DIA,~sum(BA_pa[DIA>.x],na.rm = TRUE)))
      
      density_cal <- density_cal %>%
        group_by(PLT_CN,Year) %>%
        mutate(SDI = sum(TPA_UNADJ*(DIA/10)^1.6), #stage
               num_t = length(unique(TRE_CN)))
      
      #filter for focal trees
      plt_yr2_df <- density_cal %>%
        filter(TRE_CN %in% plt_yr_df$TRE_CN)
      
      #cr
      for(i in 1:nrow(plt_yr2_df)){
        #Function arguments:
        #SPCD - is number code of species of tree record
        #SDI - is SDI of stand (Stage 1968)
        Species <- plt_yr2_df$SPCD[i]
        #SDI max values for each species were pulled from UT Variant overview
        SDIMAX <- ifelse(is.na(plt_yr2_df$SDImax[i]),
                         CR_WEIB_df$SDIMAX[CR_WEIB_df$species == Species],
                         plt_yr2_df$SDImax[i])
        #Calculate relative density
        RD <- plt_yr2_df$SDI[i]/SDIMAX
        
        #Calculate average stand crown ratio (ACR) for each species in the stand
        d0 <- CR_WEIB_df$d0[CR_WEIB_df$species == Species]
        d1 <- CR_WEIB_df$d1[CR_WEIB_df$species == Species]
        ACR <- d0 + d1 * RD * 100
        
        #Parameters of Weibull distribution: A,B,C
        a0 <- CR_WEIB_df$a0[CR_WEIB_df$species == Species]
        b0 <- CR_WEIB_df$b0[CR_WEIB_df$species == Species]
        b1 <- CR_WEIB_df$b1[CR_WEIB_df$species == Species]
        c0 <- CR_WEIB_df$c0[CR_WEIB_df$species == Species]
        c1 <- CR_WEIB_df$c1[CR_WEIB_df$species == Species]
        
        #A parameter
        WEIBA <-a0
        #B parameter
        WEIBB <- b0 + b1*ACR
        #C parameter
        WEIBC <- c0 + c1*ACR
        
        #Function arguments:
        
        #CCF - crown competition factor of stand
        #rank_pltyr - tree's rank in diameter distribution by plot by year
        #N  - number of records in the stand by year
        
        #Calculate scale parameter
        SCALE = (1.0 - .00167 * (plt_yr2_df$CCF[i]-100.0))
        if(SCALE < 0.3){SCALE = 0.3}
        if(SCALE > 1.0){SCALE = 1.0}
        
        N <- plt_yr2_df$num_t[i]
        #X is tree's rank in diameter distribution
        #Multiply tree's rank in diameter distribution (trees position relative to tree with largest diameter in the stand) by scale parameter
        Y <- plt_yr2_df$rank_pltyr[i]/N * SCALE
        if(Y < 0.05){Y = 0.05}
        if(Y > 0.95){Y = 0.95}
        #Constrain Y between 0.05 and 0.95 - crown ratio predictions in FVS are bound between these two values
        
        #Calculate crown ratio (this corresponds to variable X in UTAH variant overview)
        X <- WEIBA + WEIBB*((-1*log(1-Y))^(1/WEIBC))
        #X = a tree’s crown ratio expressed as a percent / 10
        CR_weib <- X * 10
        
        #get CR the year before
        TRE_CN <- plt_yr2_df$TRE_CN[i]
        #growthyr is still previous year
        CR_1 <- plt_df$CR_fvs[plt_df$TRE_CN == TRE_CN & 
                                plt_df$Year == growthyr] #or CR_fvs[N] for the first round
        #bound to 1% change per year
        cr_bound1 <- CR_1 * .01
        plt_yr2_df$CR_fvs[i] <- ifelse(CR_1 > CR_weib, 
                                       CR_1 - cr_bound1,
                                       CR_1 + cr_bound1)
      }
      
      #join with plt_df
      for(i in 1:nrow(plt_df)){
        TRE_CN <- plt_df$TRE_CN[i]
        if(plt_df$Year[i] == (growthyr + 1)){
          plt_df$DIA[i] <- plt_yr2_df$DIA[plt_yr2_df$TRE_CN == TRE_CN]
          plt_df$CCF[i] <- plt_yr2_df$CCF[plt_yr2_df$TRE_CN == TRE_CN]
          plt_df$PCCF[i] <- plt_yr2_df$PCCF[plt_yr2_df$TRE_CN == TRE_CN]
          plt_df$BAL[i] <- plt_yr2_df$BAL[plt_yr2_df$TRE_CN == TRE_CN]
          plt_df$SDI[i] <- plt_yr2_df$SDI[plt_yr2_df$TRE_CN == TRE_CN]
          plt_df$CR_fvs[i] <- plt_yr2_df$CR_fvs[plt_yr2_df$TRE_CN == TRE_CN]
        }
      }
      #add density metrics in
      
      #loop over next year
      growthyr = growthyr + 1
    }
    #join plt_df with empty data frame
    #output
    pred_df <- bind_rows(pred_df,plt_df)
    
    #loop over next plot
  }
  
  return(pred_df)
}

