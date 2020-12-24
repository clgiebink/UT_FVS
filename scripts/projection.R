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

#trees w/ LAT & LON
val_trees <- val_dset %>%
  dplyr::select(LON,LAT) %>%
  distinct()
proj_trees <- proj_tst %>%
  dplyr::select(lon,lat) %>%
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

#FIA ----
#get trees for projection
#skip if already selected
#criteria: alive, no missing data
#connect to database
library(dbplyr)
library(RSQLite)
UT_FIA <- DBI::dbConnect(RSQLite::SQLite(), "./data/raw/FIADB.db")
tree <- tbl(UT_FIA, sql("SELECT CN, PLT_CN, PLOT, COUNTYCD, SUBP, TREE, SPCD, STATUSCD, PREV_TRE_CN, DIA, CR, TPA_UNADJ, INVYR FROM TREE")) %>%
  collect()
colnames(tree)[colnames(tree)=="CN"] <- "TRE_CN"
plot <- tbl(UT_FIA, sql("SELECT CN, MEASYEAR, LAT, LON, ELEV, DESIGNCD, SUBP_EXAMINE_CD, PREV_PLT_CN FROM PLOT")) %>%
  collect()
colnames(plot)[colnames(plot)=="CN"] <- "PLT_CN"
cond <- tbl(UT_FIA, sql("SELECT PLT_CN, CONDID, COND_STATUS_CD, SLOPE, ASPECT,SDI_RMRS, SDIMAX_RMRS, SICOND, BALIVE, DSTRBCD1, SIBASE, SISP FROM COND")) %>%
  collect()

#merge tables to make dataset
#each row is a tree
plt_tre <- left_join(tree,plot, by = "PLT_CN")
all_fia <- left_join(plt_tre,cond, by = "PLT_CN")

#filter for trees to be used for projection
#filter for trees that were measured last
proj_red <- all_fia %>%
  filter(MEASYEAR >= 2010 &
           SPCD %in% c(93,122,202)) %>% #filter for focal species
  #filter for alive trees
  filter(STATUSCD == 1) %>%
  #filter for trees that are greater than 3 inch (large tree growth model threshold)
  filter(DIA >= 3)

#summarize dataset
#find NAs, etc
summary(proj_red)
#site index has NA

#filter for SICOND
#Species associated with SICOND (aka SISP) doesn't always match SPCD
SITREE <- tbl(UT_FIA, sql("SELECT PLT_CN, SUBP, SPCD, SITREE FROM SITETREE")) %>%
  collect()
#filter trees for SI
proj_si_check <- proj_red %>%
  select(PLT_CN,SUBP,TRE_CN,SPCD,SICOND,SISP,SIBASE) %>%
  distinct() #for some reason there are duplicate trees - more than one condition?
#join with SITREE table
proj_si_check <- left_join(proj_si_check, SITREE, by = c("PLT_CN","SPCD")) %>%
  distinct()
#calculate average site index per species per plot
proj_si_plt <- proj_si_check %>%
  dplyr::select(PLT_CN,SPCD,SITREE) %>%
  group_by(PLT_CN,SPCD) %>%
  summarise(mean = mean(SITREE,na.rm=TRUE))
#replace in data set
proj_red <- proj_red %>%
  mutate(SICOND_c = ifelse(SPCD == SISP,
                           SICOND,
                           proj_si_plt$mean[val_si_plt$PLT_CN == PLT_CN &
                                             val_si_plt$SPCD == SPCD])) %>%
  filter(!is.na(SICOND_c))
length(unique(proj_red$TRE_CN))

#Condition
#CONDID is the unique number assigned to each condition on a plot (1,2,etc)
#CON_STATUS_CD is the number associated with the type of land
##1 = forest land
##2 = nonforested land
##3 = noncensus water
##4 = census water
##5 = non sampled forest land

#find plots with multiple conditions
plt_proj <- proj_red$PLT_CN
cond_red <- tbl(UT_FIA, sql("SELECT PLT_CN, CONDID, COND_STATUS_CD FROM COND")) %>%
  collect() %>%
  filter(PLT_CN %in% plt_proj)
#summarize to find how many unique conditions on each plot
proj_cond_plt <- cond_red %>%
  group_by(PLT_CN) %>%
  summarise(n_cond = length(unique(CONDID)), #number of conditions
            n_stat = length(unique(COND_STATUS_CD)), 
            max_stat = max(COND_STATUS_CD)) #maximum condition status code on the plot

# filter with only 1 condition on plot, which is forest land
proj_cond_plt <- proj_cond_plt %>%
  filter(n_cond == 1 & max_stat ==1)
proj_red <- proj_red %>%
  filter(PLT_CN %in% proj_cond_plt$PLT_CN)
length(unique(proj_red$TRE_CN)) 

#disturbance
# proj_dist <- proj_red %>%
#   select(TRE_CN,DSTRBCD1) %>%
#   group_by(DSTRBCD1) %>%
#   summarise(n_tre = length(unique(TRE_CN)))

#keep disturbance
#calibration data set has disturbance codes:
# 10, 20, 30, 50, 52, 60, 80

proj_dset <- proj_red
proj_dset %>%
  dplyr::select(TRE_CN,SPCD) %>%
  group_by(SPCD) %>%
  summarise(n_tre = length(unique(TRE_CN)))

PLOTGEOM <- tbl(UT_FIA, sql("SELECT CN, FVS_LOC_CD FROM PLOTGEOM")) %>%
  collect()
proj_dset$FVS_LOC_CD <- PLOTGEOM$FVS_LOC_CD[match(proj_dset$PLT_CN, PLOTGEOM$CN)]

# Disconnect from the database
dbDisconnect(UT_FIA)

#all available trees
save(proj_dset,file = "./data/formatted/proj_dset.Rdata")

#rm(list=ls())

#load trees if have already selected
load('./data/formatted/proj_dset.Rdata')

#Density ----
#load/get nonfocal trees
#get trees from same plot
plot_proj <- unique(proj_dset$PLT_CN)
tree_proj <- unique(proj_dset$TRE_CN)
density_proj <- tree[(tree$PLT_CN %in% plot_proj),]
#make sure trees are on the same plot b/c calculating stand variables

#remove trees that are dead
density_proj <- density_proj %>%
  filter(STATUSCD == 1)

#plot information
density_proj <- left_join(density_proj,plot)

#check
length(plot_proj)
length(unique(density_proj$PLT_CN))
#also check - no duplicate trees - can happen with multiple CONDID

#calculate density metrics
#ccf
#equations taken from Utah variant guide
##If DBH greater than or equal to 1” CCFt= R1 + (R2 * DBH) + (R3 * DBH2)
##If DBH less than 1” but greater than 0.1” CCFt = R4 * DBHR5
##If DBH less than 0.1” CCFt = 0.001


#dataframe of species specific coefficients for ccf calculation
ccf_df <- data.frame(species=c(93,202,122,15,19,65,96,106,108,133,321,66,475,113,746,814,102),
                     #93=ES,202=DF,122=PP,15=WF,19=AF,65=UJ,66=RM,96=BS,106=PI,108=LP,113=LM,133=PM,321=OH,475=MC,746=AS,814=GO,102=bristlecone pine
                     r1 = c(0.03,0.11,0.03,0.04,0.03,0.01925,0.03,0.01925,0.01925,0.01925,0.03,0.01925,0.0204,0.01925,0.03,0.03,0.01925),
                     r2 = c(0.0173,0.0333,0.0180,0.0270,0.0216,0.01676,0.0173,0.01676,0.01676,0.01676,0.0215,0.01676,0.0246,0.01676,0.0238,0.0215,0.01676),
                     r3 = c(0.00259,0.00259,0.00281,0.00405,0.00405,0.00365,0.00259,0.00365,0.00365,0.00365,0.00363,0.00365,0.0074,0.00365,0.00490,0.00363,0.00365),
                     r4 = c(0.007875,0.017299,0.007813,0.015248,0.011402,0.009187,0.007875,0.009187,0.009187,0.009187,0.011109,0.009187,0,0.009187,0.008915,0.011109,0.009187),
                     r5 = c(1.7360,1.5571,1.7680,1.7333,1.7560,1.76,1.736,1.76,1.76,1.76,1.7250,1.76,0,1.76,1.78,1.725,1.76),
                     dbrk = c(1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,10)) #can add more species later 

#empty vector to hold calculated crown competition factor
density_proj$CCF_t <- NA
for(i in 1:nrow(density_proj)){
  Species <- density_proj$SPCD[i]
  r1 <- ccf_df$r1[ccf_df$species == Species]
  r2 <- ccf_df$r2[ccf_df$species == Species]
  r3 <- ccf_df$r3[ccf_df$species == Species]
  r4 <- ccf_df$r4[ccf_df$species == Species]
  r5 <- ccf_df$r5[ccf_df$species == Species]
  dbrk <- ccf_df$dbrk[ccf_df$species == Species]
  if(Species == 475){
    ifelse(density_proj$DIA[i] < dbrk,
           CCF_t <- density_proj$DIA[i]*(r1+r2+r3),
           CCF_t <- r1 + (r2 * density_proj$DIA[i]) + (r3 * density_proj$DIA[i]^2))
  }
  if(Species != 475){
    ifelse(is.na(density_proj$DIA[i]), 
           CCF_t <- NA,
           ifelse(density_proj$DIA[i] <= 0.1, 
                  CCF_t <- 0.0001,
                  ifelse(density_proj$DIA[i] < dbrk, 
                         CCF_t <- r4 * (density_proj$DIA[i]^r5),
                         CCF_t <- r1 + (r2 * density_proj$DIA[i]) + (r3 * density_proj$DIA[i]^2))))
  }
  density_proj$CCF_t[i] <- CCF_t
}

#PCCF is the crown competition factor on the inventory point where the tree is established
#pCCF = the sum of CCF_t on a subplot on a per acre basis
#subplot given by SUBP
#TPA is measured on a stand level, convert to subplot by multiplying by number of subplots

density_proj <- density_proj %>%
  group_by(PLT_CN,SUBP) %>%
  mutate(PCCF = sum(CCF_t * (TPA_UNADJ * 4), na.rm = TRUE))

#stand CCF = sum(CCFt on a plot) on a per acre basis
#plot given by PLT_CN
#TPA measured on a plot/stand level
density_proj <- density_proj %>%
  group_by(PLT_CN) %>%
  mutate(CCF = sum(CCF_t * TPA_UNADJ,na.rm = TRUE))

#bal
#basal area
# = dbh^2 * 0.005454 ; converts dbh in inches to squared feet
#basal area per acre
#BA*tpa
density_proj <- density_proj %>%
  mutate(BA_pa = ((DIA^2) * 0.005454) * TPA_UNADJ)

#BAL
#sum BApa of trees larger on the same plot in same year
density_proj <- density_proj %>%
  group_by(PLT_CN) %>%
  #min assigns lowest value to ties (1,2,3,3,5,6)
  mutate(BAL = map_dbl(DIA,~sum(BA_pa[DIA>.x],na.rm = TRUE)))

#CR
#see CR.R
#filter for focal trees
proj_dset <- density_proj %>%
  select(TRE_CN,CCF,PCCF,BAL) %>%
  right_join(.,proj_dset)

#First need to calcuate SDI
#Calculations from John Shaw (2000; Stage 1968)
#SDI = sum((DIA_t/10)^1.6)
#then number of trees on a plot
#rank in the diameter distribution
# val_dset <- val_dset %>%
#   group_by(PLT_CN) %>%
#   mutate(SDI = sum(TPA_UNADJ*(DIA/10)^1.6),
#          num_t = length(unique(TRE_CN)),
#          rank_pltyr = rank(DIA, na.last = TRUE, ties.method = "min"))

#will only need cr function for crown ratio change
crw_bound <- function(data,CR_WEIB_df){
  #by tree
  data$CR_fvs <- NA
  data_empty <- data[F,]
  for(t in unique(data$TRE_CN)){
    #make dataframe
    tre_df <- data %>%
      filter(TRE_CN == t) %>%
      arrange(Year)
    Species <- tre_df$SPCD[1]
    
    #only focal species
    if(Species %in% CR_WEIB_df$species){
      
      #SDI max values for each species were pulled from UT Variant overview
      ifelse(is.na(tre_df$SDIMAX_RMRS[1]),
             SDIMAX <- CR_WEIB_df$SDIMAX[CR_WEIB_df$species == Species],
             SDIMAX <- tre_df$SDIMAX_RMRS[1])
      #parameters true for same species
      d0 <- CR_WEIB_df$d0[CR_WEIB_df$species == Species]
      d1 <- CR_WEIB_df$d1[CR_WEIB_df$species == Species]
      #Parameters of Weibull distribution: A,B,C
      a0 <- CR_WEIB_df$a0[CR_WEIB_df$species == Species]
      b0 <- CR_WEIB_df$b0[CR_WEIB_df$species == Species]
      b1 <- CR_WEIB_df$b1[CR_WEIB_df$species == Species]
      c0 <- CR_WEIB_df$c0[CR_WEIB_df$species == Species]
      c1 <- CR_WEIB_df$c1[CR_WEIB_df$species == Species]
      
      N <- which(tre_df$Year == tre_df$MEASYEAR[1]) #next step is to allow N to be ring width year -1
      if(length(N) == 0){
        N <- which(tre_df$Year + 1 == tre_df$MEASYEAR[1])
      }
      if(length(N) > 0){
        Curr_row <- N-1 #each time through subtract 1 and move down one row
        tre_df$CR_fvs[N] <- tre_df$CR[N] #dbh when year of ring width and measure year are equal
        while (Curr_row > 0) { #loop will stop when it gets to the end of data for that tree
          #Calculate relative density
          #SDI - is SDI of stand (Stage 1968)
          RD <- tre_df$SDI[Curr_row]/SDIMAX
          #Calculate average stand crown ratio (ACR) for each species in the stand
          ACR <- d0 + d1 * RD * 100
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
          SCALE = (1.0 - .00167 * (tre_df$CCF[Curr_row]-100.0))
          if(SCALE < 0.3){SCALE = 0.3}
          if(SCALE > 1.0){SCALE = 1.0}
          
          N <- tre_df$num_t[Curr_row]
          #X is tree's rank in diameter distribution
          #Multiply tree's rank in diameter distribution (trees position relative to tree with largest diameter in the stand) by scale parameter
          Y <- tre_df$rank_pltyr[Curr_row]/N * SCALE
          if(Y < 0.05){Y = 0.05}
          if(Y > 0.95){Y = 0.95}
          #Constrain Y between 0.05 and 0.95 - crown ratio predictions in FVS are bound between these two values
          
          #Calculate crown ratio (this corresponds to variable X in UTAH variant overview)
          X <- WEIBA + WEIBB*((-1*log(1-Y))^(1/WEIBC))
          #X = a tree’s crown ratio expressed as a percent / 10
          CR_weib <- X * 10
          
          CR_1 <- tre_df$CR_fvs[Curr_row+1] #or CR_fvs[N] for the first round
          #bound to 1% change per year
          cr_bound1 <- tre_df$CR_fvs[Curr_row+1] * .01
          tre_df$CR_fvs[Curr_row] <- ifelse(CR_1 > CR_weib, 
                                            CR_1 - cr_bound1,
                                            CR_1 + cr_bound1)
          
          #loop will stop when it gets to the end of data for that tree
          #continue loop for next row until curr_row>0
          Curr_row = Curr_row - 1 
        }
      }
      data_empty <- bind_rows(data_empty,tre_df)
    }
    else {
      data_empty <- bind_rows(data_empty,tre_df)
    }
  }
  return(data_empty)
}

save(density_proj, file = "./data/formatted/density_proj.Rdata")
save(proj_dset,file = "./data/formatted/proj_dset.Rdata")

#calculate solar radiation
library(solrad)

#function applied to all trees.
seas_dirad <- function(begin, end, Lat, Lon, Elevation, Slope, Aspect) {
  DOY <- seq(1,365,1)
  aspect_s <- ifelse(Aspect[1] <= 180, Aspect[1] + 180, Aspect[1] - 180)
  yr_dirad <- DirectRadiation(DOY = DOY, Lat = abs(Lat[1]), Lon = abs(Lon[1]), #lat & lon in degrees
                              SLon = -105, DS = 60, #Slon and DS for UT; SLon = -7*15, DS = 60 minutes
                              Elevation = Elevation[1]/3.281, #from ft to meters
                              Slope[1], Aspect = aspect_s) #Aspect
  sum_rad = sum(yr_dirad[begin:end])
  return(sum_rad) #W/m2
}

proj_dset <- proj_dset %>%
  mutate(sin = sin((ASPECT * (pi/180)) - 0.7854) * SLOPE,
         cos = cos((ASPECT * (pi/180)) - 0.7854) * SLOPE)

#function applied to all trees.
#seas_dirad
#JanApr = (1:120)
#MayAug = (121:243)
#SepDec = (244:365)

proj_dset <- proj_dset %>%
  group_by(TRE_CN) %>%
  mutate(solrad_an = seas_dirad(begin = 1, end = 365, Lat = LAT, Lon = LON, 
                                Elevation = ELEV, Slope = SLOPE, Aspect = ASPECT),
         solrad_MayAug = seas_dirad(begin = 121, end = 243, Lat = LAT, Lon = LON, 
                                    Elevation = ELEV, Slope = SLOPE, Aspect = ASPECT))

save(proj_dset,file = "./data/formatted/proj_dset.Rdata")

#recent climate
#Precipitation
#Tmax
#for only large trees

#new plots w/ LAT & LON
proj_plt <- proj_dset %>%
  ungroup() %>%
  dplyr::select(PLT_CN,LON,LAT) %>%
  distinct()

library(raster)
# Make lat, lon data spatial
proj_plt_spat <- SpatialPointsDataFrame(coords = cbind(proj_plt$LON, proj_plt$LAT), 
                                       data = proj_plt, 
                                       proj4string = CRS("+proj=longlat +datum=NAD83"))

# Read in PRISM climate stacks
clim.path <-  "./data/formatted/"
ppt <- stack(paste(clim.path,"pptStack.tif",sep=''))
tmax <- stack(paste(clim.path,"tmaxStack.tif",sep=''))
#tmin <- stack(paste(clim.path,"tminStack.tif",sep=''))

# raster::extract PRISM data
ppt.extr <- raster::extract(ppt, proj_plt_spat) # this step takes about 8 minutes each (laptop)
tmax.extr <- raster::extract(tmax, proj_plt_spat)
#tmin.extr <- raster::extract(tmin, val_tree_spat)

# Jan 1999 - Dec 2019
ppt.extr <- ppt.extr[, 1249:1488] 
tmax.extr <- tmax.extr[, 1249:1488]
#tmin.extr <- tmin.extr[, 1249:1476]

# Add sensible column names for raster::extracted climate data
ppt.extr <- as.data.frame(ppt.extr)
tmax.extr <- as.data.frame(tmax.extr)
#tmin.extr <- as.data.frame(tmin.extr)
PRISM.path <-  "./data/raw/climate/PRISM/"
pptFiles <- list.files(path = PRISM.path, pattern = glob2rx("*ppt*.bil"), full.names = TRUE)
pptFiles <- pptFiles[1249:1488] 
#tmpFiles <- list.files(path = PRISM.path, pattern = glob2rx("*tmean*.bil"), full.names = TRUE)
#vpdFiles <- list.files(path = PRISM.path, pattern = glob2rx("*vpdmin*.bil"), full.names = TRUE)
colNames <- lapply(strsplit(pptFiles, "4kmM._"), function (x) x[2])
colNames <- unlist(colNames)
colNames <- lapply(strsplit(colNames, "_"), function (x) x[1])
colNames <- unlist(colNames)
colnames(ppt.extr) <- paste0("ppt_", colNames)
colnames(tmax.extr) <- paste0("tmax_", colNames)
#colnames(tmin.extr) <- paste0("tmin_", colNames)


# Export climate data
processed.path <- "./data/formatted/"
write.csv(ppt.extr, paste0(processed.path,"ppt_proj_extr.csv"), row.names = F)
write.csv(tmax.extr, paste0(processed.path,"tmax_proj_extr.csv"), row.names = F)
#write.csv(tmin.extr, paste0(processed.path,"tmin_val_extr.csv"), row.names = F)

#already have some climate information
processed.path <- "./data/formatted/"
ppt.extr <- read.csv(paste(processed.path,"ppt_proj_extr.csv",sep=''), header = T)
tmax.extr <- read.csv(paste(processed.path,"tmax_proj_extr.csv",sep=''), header = T)
#tmin.extr <- read.csv(paste(processed.path,"tmin_val_extr.csv",sep=''), header = T)

#for all plots
#same dataset LAT/LONG used to extract climate
proj_plots <- unique(proj_plt$PLT_CN)

clim_col <- function(PRISM,clim_var,PLT_CN){
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
  climate_stack$PLT_CN <- rep(PLT_CN,times=n)
  return(climate_stack)
}
#could make function for all climate variables
#find way to extract start_yr and end_yr
#lubridate?

proj_ppt <- clim_col(ppt.extr,clim_var = "ppt_",PLT_CN = proj_plots)
proj_tmax <- clim_col(tmax.extr,clim_var = "tmax_",PLT_CN = proj_plots)
#val_tmin <- clim_col(tmin.extr,clim_var = "tmin_",PLT_CN = val_plots)

proj_clim <- full_join(proj_ppt,proj_tmax, by = c("PLT_CN","Year"))
proj_clim$Year <- as.integer(proj_clim$Year)

#seasonal calculations
proj_clim <- proj_clim %>%
  group_by(PLT_CN) %>%
  arrange(Year) %>%
  mutate(ppt_pJunSep = lag(ppt_Jun) + lag(ppt_Jul) + lag(ppt_Aug) + lag(ppt_Sep) + lag(ppt_Oct) +
           lag(ppt_Nov)+ lag(ppt_Dec)+ ppt_Jan+ ppt_Feb + ppt_Mar + ppt_Apr +
           ppt_May + ppt_Jun + ppt_Jul + ppt_Aug + ppt_Sep,
         tmax_FebJul = (tmax_Feb + tmax_Mar + tmax_Apr + tmax_May + tmax_Jun + tmax_Jul)/6, #temp parameter for DF
         tmax_JunAug = (tmax_Jun + tmax_Jul + tmax_Aug)/3, #temp parameter for PP
         tmax_pAug = lag(tmax_Aug)) #temp parameter for ES
save(proj_clim,file = "./data/formatted/proj_clim.Rdata")

#future climate
load("./data/formatted/future_clim.Rdata")
sim <- future_clim %>% dplyr::select(modelrun, rcp) %>% distinct() %>% filter(rcp %in% c("rcp26","rcp85"))

#one ensemble for each rcp and model run

had_rcp26 <- future_clim %>%
  filter(modelrun == "hadgem2-es.1.") %>%
  filter(rcp == "rcp26" & year <= 2050)
save(had_rcp26, file = "./data/formatted/fut_clim/had_rcp26.Rdata")

had_rcp45 <- future_clim %>%
  filter(modelrun == "hadgem2-es.1.") %>%
  filter(rcp == "rcp45" & year <= 2050)
save(had_rcp45, file = "./data/formatted/fut_clim/had_rcp45.Rdata")

had_rcp60 <- future_clim %>%
  filter(modelrun == "hadgem2-es.1.") %>%
  filter(rcp == "rcp60" & year <= 2050)
save(had_rcp60, file = "./data/formatted/fut_clim/had_rcp60.Rdata")

had_rcp85 <- future_clim %>%
  filter(modelrun == "hadgem2-es.1.") %>%
  filter(rcp == "rcp85" & year <= 2050)
save(had_rcp85, file = "./data/formatted/fut_clim/had_rcp85.Rdata")

#for climate
proj_tst <- proj_dset %>%
  mutate(lat = LAT, lon = LON) %>%
  ungroup() %>%
  dplyr::select(lat, lon) %>%
  distinct() %>%
  left_join(.,rcp_red) %>%
  filter(is.na(ppt_Jan))


# Density ----


##BAR method ----

#get non focal data
tree_proj <- unique(proj_dset$TRE_CN)
non_foc_proj <- density_proj %>%
  ungroup() %>%
  filter(!(TRE_CN %in% tree_proj))
#match bar to species or average over species
sp_bar <- unique(bar_df$SPCD)
non_foc_proj <- non_foc_proj %>%
  mutate(BAR_av = ifelse(SPCD %in% sp_bar,
                         bar_df$BAR_Avg[match(non_foc_proj$SPCD,bar_df$SPCD)], #mean species specific
                         mean(bar_df$BAR_Avg))) #mean across all species

#create nonfocal dataframe with average bar (missingDBH.R script)
#From User's Guide to the Stand Prognosis Model
#Wykoff, Crookston, Stage
#pg 48
#DBH_0 = sqrt(BAR * DBH_1^2) -- back calculating
#DBH_1 = sqrt((DBH_0^2)/BAR_av) -- projecting
#Optional: BAR by size (DBH)

#adapted from function in missingDBH.R
#take some code from validation function int_tre

DIA_BAR <- function(data){
  for(i in 1:nrow(data)){
    #bar_av has already been matched
    #otherwise could use this space to match (bar_df)
    tre_df <- data[i,] %>%
      slice(rep(1:n(), each = ((2050-MEASYEAR) +1))) %>%
      mutate(Year = c(MEASYEAR:2050),
             DIA_int = NA)
      N <- which(tre_df$Year == tre_df$MEASYEAR[1])
      tre_df$DIA_int[N] <- tre_df$DIA[N] #dbh when year and measure year are equal
      Curr_row <- N+1 #each time through subtract 1 and move down one row
      while(Curr_row <= ((2050 - data$MEASYEAR[i]) +1)){
        DIA_1 <- tre_df$DIA_int[Curr_row-1]
        tre_df$DIA_int[Curr_row] <- sqrt((DIA_1^2)/tre_df$BAR_av[Curr_row])
        #continue loop for next row until curr_row>0
        Curr_row = Curr_row + 1 
      }
    data <- full_join(data,tre_df)
  }
  data <- data %>%
    filter(!is.na(Year))
  #there is probably a better way to do this
  #maybe dplyr method like missing- and annualizeDBH - group by tree
  return(data)
}

nonfoc_proj_exp <- non_foc_proj %>%
  dplyr::select(TRE_CN,PLT_CN,SUBP,SPCD,DIA,TPA_UNADJ,MEASYEAR,
                LAT,LON,ELEV,DESIGNCD,BAR_av) %>%
  DIA_BAR(.)
save(nonfoc_proj_exp, file = "./data/formatted/nonfoc_proj_exp.Rdata")

## FVS ----
#optional - use fvs online to grow stands
#impute dbh and drop trees that die

View(proj_dset %>% 
       ungroup() %>% 
       dplyr::select(TRE_CN,MEASYEAR) %>% 
       group_by(MEASYEAR) %>% 
       summarise(n_tre = length(unique(TRE_CN))))

#FVS ready data
plt_measyr <- proj_dset %>%
  ungroup() %>%
  select(PLT_CN,MEASYEAR) %>%
  distinct()

#original code Mark castle
#Read in CSV containing validation plots and years
projPlots<-plt_measyr

#Remove row names column
projPlots$X<-NULL;head(projPlots)

#Open DB connection
con <- dbConnect(SQLite(), "./data/raw/FIADB.db")

#Extract FVS_GroupAddFilesAndKeywords table
fvsAddKey<-dbReadTable(con, 'FVS_GROUPADDFILESANDKEYWORDS')

#Extract FVS_PlotInit_Plot table
fvsPlotInitPlot<-dbReadTable(con, 'FVS_PLOTINIT_PLOT')

#Extract FVS_StandInit_Cond table
fvsStandInitPlot<-dbReadTable(con, 'FVS_STANDINIT_PLOT')

#Extract FVS_StandInit_Cond table
fvsStandInitCond<-dbReadTable(con, 'FVS_STANDINIT_COND')

#Extract FVS_StandInit_Cond table
fvsStandInitPlot<-dbReadTable(con, 'FVS_STANDINIT_PLOT')

#Extract FVS_StandInit_Cond table
fvsTreeInitPlot<-dbReadTable(con, 'FVS_TREEINIT_PLOT')

#Extract FVS_StandInit_Cond table
fvsTreeInitCond<-dbReadTable(con, 'FVS_TREEINIT_COND')

#Disconnect from input database
dbDisconnect(con)

#Rename  PLT_CN header in valPlots
names(projPlots)[names(projPlots)=="PLT_CN"]<-"STAND_CN"

#Subset FIA utah data based on the plots in valPlots
fvsPlotInitPlot<-fvsPlotInitPlot[fvsPlotInitPlot$STAND_CN %in% projPlots$STAND_CN,]
fvsStandInitPlot<-fvsStandInitPlot[fvsStandInitPlot$STAND_CN %in% projPlots$STAND_CN,]
fvsTreeInitPlot<-fvsTreeInitPlot[fvsTreeInitPlot$STAND_CN %in% projPlots$STAND_CN,]

#Merge inventory years to FVSPlotInitPlot and FVSStandInitPlot
fvsPlotInitPlot<-merge(fvsPlotInitPlot, projPlots, by="STAND_CN", all.x = T)
fvsStandInitPlot<-merge(fvsStandInitPlot, projPlots, by="STAND_CN", all.x = T)

#Create group label based on inventory years
fvsPlotInitPlot$NewGroup<-paste(fvsPlotInitPlot$MEASYEAR);head(fvsPlotInitPlot)
fvsStandInitPlot$NewGroup<-paste(fvsStandInitPlot$MEASYEAR);head(fvsStandInitPlot)

#Add NewGroup to GROUPS column
fvsPlotInitPlot$GROUPS<-paste(fvsPlotInitPlot$GROUPS, fvsPlotInitPlot$NewGroup, sep = " ");head(fvsPlotInitPlot$GROUPS)
fvsStandInitPlot$GROUPS<-paste(fvsStandInitPlot$GROUPS, fvsStandInitPlot$NewGroup, sep = " ");head(fvsStandInitPlot$GROUPS)

#Create new UT DB
conn <- dbConnect(RSQLite::SQLite(), "./data/raw/FVS/FVS_proj.db")

#Write each of the neccesary FVS tables to DB
dbWriteTable(conn, "FVS_GROUPADDFILESANDKEYWORDS", fvsAddKey)
dbWriteTable(conn, "FVS_PLOTINIT_PLOT", fvsPlotInitPlot)
dbWriteTable(conn, "FVS_STANDINIT_COND", fvsStandInitCond)
dbWriteTable(conn, "FVS_STANDINIT_PLOT", fvsStandInitPlot)
dbWriteTable(conn, "FVS_TREEINIT_COND", fvsTreeInitCond)
dbWriteTable(conn, "FVS_TREEINIT_PLOT", fvsTreeInitPlot)


#from FVS online
#https://forest.moscowfsl.wsu.edu/FVSOnline/

#get output from fvs runs
#tree list table
fvs_treelist <- read_csv(file = "./data/raw/FVS/fvs_proj.csv")
length(unique(fvs_treelist$StandID))
#mostly to convert 093 -> 93
fvs_treelist$SpeciesFIA <- as.numeric(fvs_treelist$SpeciesFIA)

#connect to fvs ready database for validation data
library(dbplyr)
library(RSQLite)
fvs_proj_db <- dbConnect(RSQLite::SQLite(), "./data/raw/FVS/FVS_proj.db")
#Extract FVS_StandInit_Plot table
#has the link between stand_cn (aka plt_cn) and stand_id
fvsStandInitPlot<-dbReadTable(fvs_proj_db, 'FVS_STANDINIT_PLOT');head(fvsStandInitPlot)
# Disconnect from the database
dbDisconnect(fvs_proj_db)
std2plt <- fvsStandInitPlot %>%
  dplyr::select(STAND_CN,STAND_ID)
length(unique(std2plt$STAND_ID))
#stand_cn = plt_cn
fvs_treelist$PLT_CN <- std2plt$STAND_CN[match(fvs_treelist$StandID,std2plt$STAND_ID)]
length(unique(fvs_treelist$PLT_CN))

#get tree cn
UT_FIA <- DBI::dbConnect(RSQLite::SQLite(), "./data/raw/FIADB.db")
TREE_red <- tbl(UT_FIA, sql("SELECT CN, PLT_CN, SPCD, SUBP, TREE, DIA, PREV_TRE_CN, STATUSCD FROM TREE")) %>%
  collect()
#prep for merge
fvs_proj_red <- fvs_treelist %>%
  dplyr::select(PLT_CN, Year, TreeId, ActPt, SpeciesFIA, TPA, MortPA, DBH, DG, PctCr, PtBAL)
colnames(TREE_red)[colnames(TREE_red)=="TREE"] <- "TreeId"
colnames(fvs_proj_red)[colnames(fvs_proj_red)=="SpeciesFIA"] <- "SPCD"
colnames(fvs_proj_red)[colnames(fvs_proj_red)=="ActPt"] <- "SUBP"
# Disconnect from the database
dbDisconnect(UT_FIA)

#merge fvs runs and fia observations
fvs_proj <- left_join(fvs_proj_red,TREE_red)
length(unique(fvs_proj$CN))
save(fvs_proj, file = "./data/formatted/fvs_proj.Rdata")

#filter for nonfocal trees
fvs_proj$CN <- as.numeric(fvs_proj$CN)
n_tre <- fvs_proj %>%
  dplyr::select(PLT_CN,TreeId,SUBP,SPCD) %>%
  distinct() #8473
fvs_proj_red <- fvs_proj %>%
  mutate(TRE_CN = as.numeric(CN)) %>%
  filter(TPA > 0) %>% #remove dead (2) trees; comment out to check line below
  filter(!(TRE_CN %in% proj_dset$TRE_CN)) %>% #remove focal trees
  filter(Year < 2060) #keep projection short
n_tre <- fvs_proj_red %>%
  dplyr::select(PLT_CN,TreeId,SUBP,SPCD) %>%
  distinct() #5678; reduced 2795 trees
length(unique(proj_dset$TRE_CN)) #so 176 not reduced

#remove plots were trees do not match
#trees not removed
nrm_tre <- proj_dset %>% 
  dplyr::select(PLT_CN, TRE_CN) %>% 
  distinct() %>% 
  filter(!(TRE_CN %in% fvs_proj$CN))
length(unique(nrm_tre$TRE_CN)) #7?

proj_dset <- proj_dset %>%
  filter(!(PLT_CN %in% nrm_tre$PLT_CN))
length(unique(proj_dset$TRE_CN)) #2795
save(proj_dset, file = './data/formatted/proj_dset.Rdata')

fvs_proj_red <- fvs_proj_red %>%
  filter(!(PLT_CN %in% nrm_tre$PLT_CN))


#get difference in diameter
fvs_proj_red <- fvs_proj_red %>%
  group_by(PLT_CN,TreeId,SUBP,SPCD) %>%
  arrange(desc(Year)) %>% 
  mutate(DG_int = (lag(DBH) - DBH)/10,
         mort_int = lag(MortPA)/10)

#interpolate dbh
#mortality is applied over trees per acre so interpolate tpa

#interpolate dbh
int_fvs <- function(data){
  end <- data %>% filter(Year >= 2050) %>%
    mutate(Year_int = Year,
           DIA_int = DBH,
           TPA_UNADJ = TPA)
  data_red <- data %>% filter(Year < 2050) %>%
    arrange(Year)
  for(i in 1:nrow(data_red)){
    tre_yr_df <- data_red[i,] %>%
      slice(rep(1:n(), each = 10)) %>%
      mutate(Year_int =  Year[1]:(Year[1]+9),
             DIA_int = NA,
             TPA_UNADJ = NA)
    N <- which(tre_yr_df$Year_int == tre_yr_df$Year[1])
    tre_yr_df$DIA_int[N] <- tre_yr_df$DBH[N] #dbh when year and measure year are equal
    tre_yr_df$TPA_UNADJ[N] <- tre_yr_df$TPA[N]
    Curr_row <- N+1 #each time through subtract 1 and move down one row
    while(Curr_row <= 10){
      DIA_1 <- tre_yr_df$DIA_int[Curr_row-1]
      tre_yr_df$DIA_int[Curr_row] <- DIA_1 + tre_yr_df$DG_int[Curr_row]
      TPA_1 <- tre_yr_df$TPA[Curr_row-1]
      tre_yr_df$TPA_UNADJ[Curr_row] <- TPA_1 - tre_yr_df$mort_int[Curr_row]
      #continue loop for next row until curr_row>0
      Curr_row = Curr_row + 1 
    }
    end <- bind_rows(end,tre_yr_df) #%>%
      #filter(!is.na(Year_int))
  }
  #data <- data %>%
  #  filter(!is.na(Year))
  return(end)
}

#TODO end should have DIA_int filled in

proj_nonf <- int_fvs(data = fvs_proj_red)
save(proj_nonf, file = './data/formatted/proj_nonf.Rdata')


# Function ----

project_fun <- function(data,mod_df,mod_pp,mod_es,sp_stats,nonfocal,bratio,ccf_df,cur_clim,fut_clim) {
  #parameters:
  #data - trees from FIADB
  data_rep <- data %>%
    ungroup() %>%
    dplyr::select(PLT_CN, TRE_CN, SPCD, SUBP, MEASYEAR, 
                  TPA_UNADJ, DESIGNCD, CR,
                  ASPECT,SLOPE,sin,cos,LAT,LON,ELEV,
                  FVS_LOC_CD,SDIMAX_RMRS,SICOND_c,solrad_MayAug) %>%
    mutate(Year = NA,
           DIA = NA,
           CCF = NA,
           PCCF = NA,
           BAL = NA,
           SDI = NA)
  data_rep <- data_rep %>% 
    group_by(TRE_CN) %>%
    slice(rep(1:n(), each = 30)) %>% #grow 30 years into the future?
    mutate(Year = (MEASYEAR[1]:(MEASYEAR[1]+29))) %>% #repeated data frame ready  to fill in
    ungroup()
  ##density for new data in MEASYEAR will already be calculated
  #fill in DIA,CCF, PCCF, BAL, SDI, CR_weib, where year = measyear
  #climate projections
  #fill climate
  climate <- cur_clim %>%
    ungroup()
  for(i in 1:nrow(data_rep)){
    TRE_CN <- data_rep$TRE_CN[i]
    if(data_rep$Year[i] == data_rep$MEASYEAR[i]){
      data_rep$DIA[i] <- data$DIA[data$TRE_CN == TRE_CN]
      data_rep$CCF[i] <- data$CCF[data$TRE_CN == TRE_CN]
      data_rep$PCCF[i] <- data$PCCF[data$TRE_CN == TRE_CN]
      data_rep$BAL[i] <- data$BAL[data$TRE_CN == TRE_CN]
      data_rep$SDI[i] <- data$SDI_RMRS[data$TRE_CN == TRE_CN]
    }
    #add known climate
    if(data_rep$Year[i] <= 2018){ #data set only goes up to 2018
      #match over tree and year
      data_rep$ppt_pJunSep[i] <- cur_clim$ppt_pJunSep[cur_clim$PLT_CN == PLT_CN &
                                                       cur_clim$Year == data_rep$Year[i]]
      data_rep$tmax_JunAug[i] <- cur_clim$tmax_JunAug[cur_clim$PLT_CN == PLT_CN &
                                                       cur_clim$Year == data_rep$Year[i]]
      data_rep$tmax_FebJul[i] <- cur_clim$tmax_FebJul[cur_clim$PLT_CN == PLT_CN &
                                                       cur_clim$Year == data_rep$Year[i]]
      data_rep$tmax_pAug[i] <- cur_clim$tmax_pAug[cur_clim$PLT_CN == PLT_CN &
                                                   cur_clim$Year == data_rep$Year[i]]
    }
    else{ #use future climate
      data_rep$ppt_pJunSep[i] <- fut_clim$ppt_pJunSep[fut_clim$PLT_CN == PLT_CN &
                                                       fut_clim$Year == data_rep$Year[i]]
      data_rep$tmax_JunAug[i] <- fut_clim$tmax_JunAug[fut_clim$PLT_CN == PLT_CN &
                                                       fut_clim$Year == data_rep$Year[i]]
      data_rep$tmax_FebJul[i] <- fut_clim$tmax_FebJul[fut_clim$PLT_CN == PLT_CN &
                                                       fut_clim$Year == data_rep$Year[i]]
      data_rep$tmax_pAug[i] <- fut_clim$tmax_pAug[fut_clim$PLT_CN == PLT_CN &
                                                   fut_clim$Year == data_rep$Year[i]]
    }
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
        #plt_yr_df$z.CR_fvs[i] = unlist((plt_yr_df[i,"CR_fvs"] - para_std[1,"CR_fvs"]) / para_std[2,"CR_fvs"])
        plt_yr_df$z.BAL[i] = unlist((plt_yr_df[i,"BAL"] - para_std[1,"BAL"]) / para_std[2,"BAL"])
        plt_yr_df$z.CCF[i] = unlist((plt_yr_df[i,"CCF"] - para_std[1,"CCF"]) / para_std[2,"CCF"])
        #plt_yr_df$z.PCCF[i] = unlist((plt_yr_df[i,"PCCF"] - para_std[1,"PCCF"]) / para_std[2,"PCCF"])
        plt_yr_df$z.SDI[i] = unlist((plt_yr_df[i,"SDI"] - para_std[1,"SDI"]) / para_std[2,"SDI"])
        plt_yr_df$z.SICOND[i] = unlist((plt_yr_df[i,"SICOND_c"] - para_std[1,"SICOND"]) / para_std[2,"SICOND"])
        plt_yr_df$z.SLOPE[i] = unlist((plt_yr_df[i,"SLOPE"] - para_std[1,"SLOPE"]) / para_std[2,"SLOPE"])
        #plt_yr_df$z.sin[i] = unlist((plt_yr_df[i,"sin"] - para_std[1,"sin"]) / para_std[2,"sin"])
        #plt_yr_df$z.cos[i] = unlist((plt_yr_df[i,"cos"] - para_std[1,"cos"]) / para_std[2,"cos"])
        #plt_yr_df$z.solrad_MayAug[i] = unlist((plt_yr_df[i,"solrad_MayAug"] - para_std[1,"solrad_MayAug"]) / para_std[2,"solrad_MayAug"])
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


