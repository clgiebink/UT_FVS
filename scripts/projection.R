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
proj_trees <- proj_dset %>%
  ungroup() %>%
  dplyr::select(LON,LAT) %>%
  distinct()

# Make lat, lon data spatial
proj_tree_spat <- SpatialPointsDataFrame(coords = cbind(proj_trees$LON, proj_trees$LAT), 
                                        data = proj_trees, 
                                        proj4string = CRS("+init=epsg:4326"))
plot(proj_tree_spat)

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
extract.yearly.ppt  <- function(proj, ppt, proj_tree_spat, nmonths){ 
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
    
    extracted.pts <- data.frame(raster::extract(rast.stack, proj_tree_spat))
    ll.data <- as.data.frame(proj_tree_spat)
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
all.future.ppt <- extract.yearly.ppt(proj = proj, ppt = ppt, proj_tree_spat = proj_tree_spat, nmonths = nmonths)


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
extract.yearly.tmax  <- function(proj, Tmax, proj_tree_spat, nmonths){ 
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
    extracted.pts<- data.frame(raster::extract(rast.stack, proj_tree_spat))
    
    ll.data <- as.data.frame(proj_tree_spat)
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
      mutate(tmax_JunAug = (tmax_Jun + tmax_Jul + tmax_Aug)/3,
             tmax_FebJul = (tmax_Feb + tmax_Mar + tmax_Apr + tmax_May + tmax_Jun + tmax_Jul)/6,
             tmax_pAug = lag(tmax_Aug))
    all.future.tmax[[p]] <- yearly.tmax
  }
  all.future.tmax
}

#run
all.future.tmax <- extract.yearly.tmax(proj = proj, Tmax = Tmax, proj_tree_spat = proj_tree_spat, nmonths = nmonths)

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

#add plot number from lat and lon
plot <- tbl(UT_FIA, sql("SELECT CN, LAT, LON FROM PLOT")) %>%
  collect() %>%
  mutate(PLT_CN = CN, lat = LAT, lon = LON) %>%
  dplyr::select(PLT_CN,lat,lon)
future_clim <- left_join(future_clim,plot, by = c("lat","lon"))

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

# Search for PRISM files
PRISM.path <-  "./data/raw/climate/PRISM_rec/"
pptFiles <- list.files(path = PRISM.path, pattern = glob2rx("*ppt*.bil"), full.names = TRUE)
tmaxFiles <- list.files(path = PRISM.path, pattern = glob2rx("*tmax*.bil"), full.names = TRUE)

# Stack monthly data
pptStack <- stack()
for (i in pptFiles) {
  print(i)
  pptStack <- stack(pptStack, raster(i))
}

tmaxStack <- stack()
for (i in tmaxFiles) {
  print(i)
  tmaxStack <- stack(tmaxStack, raster(i))
}

# Crop climate to extent of Utah
library(maps)
m <- ggplot2::map_data('state', region = 'Utah')

ut_spat <- SpatialPointsDataFrame(coords = cbind(m$long, m$lat), 
                                  data = m, 
                                  proj4string = CRS("+proj=longlat +datum=NAD83"))
cropUT <- extent(ut_spat)
pptStackCropped <- crop(pptStack, cropUT)
tmaxStackCropped <- crop(tmaxStack, cropUT)

# Export rasters
clim.path <-  "./data/formatted/"
writeRaster(pptStackCropped, paste0(clim.path, "pptStack_proj.tif"), overwrite = T)
writeRaster(tmaxStackCropped, paste0(clim.path, "tmaxStack_proj.tif"), overwrite = T)

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
ppt <- stack(paste(clim.path,"pptStack_proj.tif",sep=''))
tmax <- stack(paste(clim.path,"tmaxStack_proj.tif",sep=''))

# raster::extract PRISM data
ppt.extr <- raster::extract(ppt, proj_plt_spat)
tmax.extr <- raster::extract(tmax, proj_plt_spat)

# Jan 2009 - Dec 2019 (or May 2020)
#ppt.extr <- ppt.extr[, 1] 
#tmax.extr <- tmax.extr[, 1]

# Add sensible column names for raster::extracted climate data
ppt.extr <- as.data.frame(ppt.extr)
tmax.extr <- as.data.frame(tmax.extr)
#tmin.extr <- as.data.frame(tmin.extr)
PRISM.path <-  "./data/raw/climate/PRISM_rec/"
pptFiles <- list.files(path = PRISM.path, pattern = glob2rx("*ppt*.bil"), full.names = TRUE)
pptFiles <- list.files(path = PRISM.path, pattern = glob2rx("*tmax*.bil"), full.names = TRUE)
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

#since not full years (12 months), add columns
#june - dec
#add directly to csv file

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

#normals
#calibration trees w/ LAT & LON - proj_plt
# Make lat, lon data spatial _proj_plt_spat
#see above

# Read in PRISM climate stacks
PRISM.norm.path <-  "./data/raw/climate/normals"
ppt.norm <- stack(paste(PRISM.norm.path,"pptNormals.tif",sep=''))
#tmx.norm <- stack(paste(PRISM.norm.path,"tmxNormals.tif",sep=''))
tmp.norm <- stack(paste(PRISM.norm.path,"tmpNormals.tif",sep=''))

# raster::extract PRISM data
n.ppt.extr <- raster::extract(ppt.norm, proj_plt_spat)
n.tmp.extr <- raster::extract(tmp.norm, proj_plt_spat)
#n.tmx.extr <- raster::extract(tmx.norm, proj_plt_spat)

# Add tre_cn column to link to other dataframes
n.ppt.extr <- as.data.frame(n.ppt.extr)
n.ppt.extr$PLT_CN <- proj_plt$PLT_CN
n.tmp.extr <- as.data.frame(n.tmp.extr)
n.tmp.extr$PLT_CN <- proj_plt$PLT_CN

#join with dataset
proj_dset$n_ppt <- n.ppt.extr$normalspptNormals[match(proj_dset$PLT_CN,n.ppt.extr$PLT_CN)]
proj_dset$n_tmp <- n.tmp.extr$normalstmpNormals[match(proj_dset$PLT_CN,n.tmp.extr$PLT_CN)]
save(proj_dset, file = './data/formatted/proj_dset.Rdata')

#future climate
load("./data/formatted/future_clim.Rdata")
sim <- future_clim %>% dplyr::select(modelrun, rcp) %>% distinct() %>% filter(rcp %in% c("rcp26","rcp85"))

#one ensemble for each rcp and model run

acc_rcp85 <- future_clim %>%
  filter(modelrun == "access1-0.1.") %>%
  filter(rcp == "rcp85" & year <= 2060)
save(acc_rcp85, file = "./data/formatted/fut_clim/acc_rcp85.Rdata")

bccm_rcp85 <- future_clim %>%
  filter(modelrun == "bcc-csm1-1-m.1.") %>%
  filter(rcp == "rcp85" & year <= 2060)
save(bccm_rcp85, file = "./data/formatted/fut_clim/bccm_rcp85.Rdata")

bcc_rcp26 <- future_clim %>%
  filter(modelrun == "bcc-csm1-1.1.") %>%
  filter(rcp == "rcp26" & year <= 2060)
save(bcc_rcp26, file = "./data/formatted/fut_clim/bcc_rcp26.Rdata")

bcc_rcp85 <- future_clim %>%
  filter(modelrun == "bcc-csm1-1.1.") %>%
  filter(rcp == "rcp85" & year <= 2060)
save(bcc_rcp85, file = "./data/formatted/fut_clim/bcc_rcp85.Rdata")

can_rcp26 <- future_clim %>%
  filter(modelrun == "canesm2.1.") %>%
  filter(rcp == "rcp26" & year <= 2060)
save(can_rcp26, file = "./data/formatted/fut_clim/can_rcp26.Rdata")

can_rcp85 <- future_clim %>%
  filter(modelrun == "canesm2.1.") %>%
  filter(rcp == "rcp85" & year <= 2060)
save(can_rcp85, file = "./data/formatted/fut_clim/can_rcp85.Rdata")

ccs_rcp26 <- future_clim %>%
  filter(modelrun == "ccsm4.1.") %>%
  filter(rcp == "rcp26" & year <= 2060)
save(ccs_rcp26, file = "./data/formatted/fut_clim/ccs_rcp26.Rdata")

ccs_rcp85 <- future_clim %>%
  filter(modelrun == "ccsm4.1.") %>%
  filter(rcp == "rcp85" & year <= 2060)
save(ccs_rcp85, file = "./data/formatted/fut_clim/ccs_rcp85.Rdata")

cesb_rcp85 <- future_clim %>%
  filter(modelrun == "cesm1-bgc.1.") %>%
  filter(rcp == "rcp85" & year <= 2060)
save(cesb_rcp85, file = "./data/formatted/fut_clim/cesb_rcp85.Rdata")

ces_rcp26 <- future_clim %>%
  filter(modelrun == "cesm1-cam5.1.") %>%
  filter(rcp == "rcp26" & year <= 2060)
save(ces_rcp26, file = "./data/formatted/fut_clim/ces_rcp26.Rdata")

ces_rcp85 <- future_clim %>%
  filter(modelrun == "cesm1-cam5.1.") %>%
  filter(rcp == "rcp85" & year <= 2060)
save(ces_rcp85, file = "./data/formatted/fut_clim/ces_rcp85.Rdata")

cmc_rcp85 <- future_clim %>%
  filter(modelrun == "cmcc-cm.1.") %>%
  filter(rcp == "rcp85" & year <= 2060)
save(cmc_rcp85, file = "./data/formatted/fut_clim/cmc_rcp85.Rdata")

cnm_rcp85 <- future_clim %>%
  filter(modelrun == "cnrm-cm5.1.") %>%
  filter(rcp == "rcp85" & year <= 2060)
save(cnm_rcp85, file = "./data/formatted/fut_clim/cnm_rcp85.Rdata")

csr_rcp26 <- future_clim %>%
  filter(modelrun == "csiro-mk3-6-0.1.") %>%
  filter(rcp == "rcp26" & year <= 2060)
save(csr_rcp26, file = "./data/formatted/fut_clim/csr_rcp26.Rdata")

csr_rcp85 <- future_clim %>%
  filter(modelrun == "csiro-mk3-6-0.1.") %>%
  filter(rcp == "rcp85" & year <= 2060)
save(csr_rcp85, file = "./data/formatted/fut_clim/csr_rcp85.Rdata")

fgl_rcp26 <- future_clim %>%
  filter(modelrun == "fgoals-g2.1.") %>%
  filter(rcp == "rcp26" & year <= 2060)
save(fgl_rcp26, file = "./data/formatted/fut_clim/fgl_rcp26.Rdata")

fgl_rcp85 <- future_clim %>%
  filter(modelrun == "fgoals-g2.1.") %>%
  filter(rcp == "rcp85" & year <= 2060)
save(fgl_rcp85, file = "./data/formatted/fut_clim/fgl_rcp85.Rdata")

fio_rcp26 <- future_clim %>%
  filter(modelrun == "fio-esm.1.") %>%
  filter(rcp == "rcp26" & year <= 2060)
save(fio_rcp26, file = "./data/formatted/fut_clim/fio_rcp26.Rdata")

fio_rcp85 <- future_clim %>%
  filter(modelrun == "fio-esm.1.") %>%
  filter(rcp == "rcp85" & year <= 2060)
save(fio_rcp85, file = "./data/formatted/fut_clim/fio_rcp85.Rdata")

gfc_rcp26 <- future_clim %>%
  filter(modelrun == "gfdl-cm3.1.") %>%
  filter(rcp == "rcp26" & year <= 2060)
save(gfc_rcp26, file = "./data/formatted/fut_clim/gfc_rcp26.Rdata")

gfc_rcp85 <- future_clim %>%
  filter(modelrun == "gfdl-cm3.1.") %>%
  filter(rcp == "rcp85" & year <= 2060)
save(gfc_rcp85, file = "./data/formatted/fut_clim/gfc_rcp85.Rdata")

gfg_rcp26 <- future_clim %>%
  filter(modelrun == "gfdl-esm2g.1.") %>%
  filter(rcp == "rcp26" & year <= 2060)
save(gfg_rcp26, file = "./data/formatted/fut_clim/gfg_rcp26.Rdata")

gfg_rcp85 <- future_clim %>%
  filter(modelrun == "gfdl-esm2g.1.") %>%
  filter(rcp == "rcp85" & year <= 2060)
save(gfg_rcp85, file = "./data/formatted/fut_clim/gfg_rcp85.Rdata")

gfm_rcp26 <- future_clim %>%
  filter(modelrun == "gfdl-esm2m.1.") %>%
  filter(rcp == "rcp26" & year <= 2060)
save(gfm_rcp26, file = "./data/formatted/fut_clim/gfm_rcp26.Rdata")

gfm_rcp85 <- future_clim %>%
  filter(modelrun == "gfdl-esm2m.1.") %>%
  filter(rcp == "rcp85" & year <= 2060)
save(gfm_rcp85, file = "./data/formatted/fut_clim/gfm_rcp85.Rdata")

gis_rcp26 <- future_clim %>%
  filter(modelrun == "giss-e2-r.1.") %>%
  filter(rcp == "rcp26" & year <= 2060)
save(gis_rcp26, file = "./data/formatted/fut_clim/gis_rcp26.Rdata")

gis_rcp85 <- future_clim %>%
  filter(modelrun == "giss-e2-r.1.") %>%
  filter(rcp == "rcp85" & year <= 2060)
save(gis_rcp85, file = "./data/formatted/fut_clim/gis_rcp85.Rdata")

hada_rcp26 <- future_clim %>%
  filter(modelrun == "hadgem2-ao.1.") %>%
  filter(rcp == "rcp26" & year <= 2060)
save(hada_rcp26, file = "./data/formatted/fut_clim/hada_rcp26.Rdata")

hada_rcp85 <- future_clim %>%
  filter(modelrun == "hadgem2-es.1.") %>%
  filter(rcp == "rcp85" & year <= 2060)
save(hada_rcp85, file = "./data/formatted/fut_clim/hada_rcp85.Rdata")

hadc_rcp85 <- future_clim %>%
  filter(modelrun == "hadgem2-cc.1.") %>%
  filter(rcp == "rcp85" & year <= 2060)
save(hadc_rcp85, file = "./data/formatted/fut_clim/hadc_rcp85.Rdata")

had_rcp26 <- future_clim %>%
  filter(modelrun == "hadgem2-es.1.") %>%
  filter(rcp == "rcp26" & year <= 2060)
save(had_rcp26, file = "./data/formatted/fut_clim/had_rcp26.Rdata")

had_rcp85 <- future_clim %>%
  filter(modelrun == "hadgem2-es.1.") %>%
  filter(rcp == "rcp85" & year <= 2060)
save(had_rcp85, file = "./data/formatted/fut_clim/had_rcp85.Rdata")

inm_rcp85 <- future_clim %>%
  filter(modelrun == "inmcm4.1.") %>%
  filter(rcp == "rcp85" & year <= 2060)
save(inm_rcp85, file = "./data/formatted/fut_clim/inm_rcp85.Rdata")

ipm_rcp26 <- future_clim %>%
  filter(modelrun == "ipsl-cm5a-mr.1.") %>%
  filter(rcp == "rcp26" & year <= 2060)
save(ipm_rcp26, file = "./data/formatted/fut_clim/ipm_rcp26.Rdata")

ipm_rcp85 <- future_clim %>%
  filter(modelrun == "ipsl-cm5a-mr.1.") %>%
  filter(rcp == "rcp85" & year <= 2060)
save(ipm_rcp85, file = "./data/formatted/fut_clim/ipm_rcp85.Rdata")

ipl_rcp85 <- future_clim %>%
  filter(modelrun == "ipsl-cm5b-lr.1.") %>%
  filter(rcp == "rcp85" & year <= 2060)
save(ipl_rcp85, file = "./data/formatted/fut_clim/ipl_rcp85.Rdata")

mrc_rcp26 <- future_clim %>%
  filter(modelrun == "miroc-esm-chem.1.") %>%
  filter(rcp == "rcp26" & year <= 2060)
save(mrc_rcp26, file = "./data/formatted/fut_clim/mrc_rcp26.Rdata")

mrc_rcp85 <- future_clim %>%
  filter(modelrun == "miroc-esm-chem.1.") %>%
  filter(rcp == "rcp85" & year <= 2060)
save(mrc_rcp85, file = "./data/formatted/fut_clim/mrc_rcp85.Rdata")

mre_rcp26 <- future_clim %>%
  filter(modelrun == "miroc-esm.1.") %>%
  filter(rcp == "rcp26" & year <= 2060)
save(mre_rcp26, file = "./data/formatted/fut_clim/mre_rcp26.Rdata")

mre_rcp85 <- future_clim %>%
  filter(modelrun == "miroc-esm.1.") %>%
  filter(rcp == "rcp85" & year <= 2060)
save(mre_rcp85, file = "./data/formatted/fut_clim/mre_rcp85.Rdata")

mir_rcp26 <- future_clim %>%
  filter(modelrun == "miroc5.1.") %>%
  filter(rcp == "rcp26" & year <= 2060)
save(mir_rcp26, file = "./data/formatted/fut_clim/mir_rcp26.Rdata")

mir_rcp85 <- future_clim %>%
  filter(modelrun == "miroc5.1.") %>%
  filter(rcp == "rcp85" & year <= 2060)
save(mir_rcp85, file = "./data/formatted/fut_clim/mir_rcp85.Rdata")

mpl_rcp26 <- future_clim %>%
  filter(modelrun == "mpi-esm-lr.1.") %>%
  filter(rcp == "rcp26" & year <= 2060)
save(mpl_rcp26, file = "./data/formatted/fut_clim/mpl_rcp26.Rdata")

mpl_rcp85 <- future_clim %>%
  filter(modelrun == "mpi-esm-lr.1.") %>%
  filter(rcp == "rcp85" & year <= 2060)
save(mpl_rcp85, file = "./data/formatted/fut_clim/mpl_rcp85.Rdata")

mpm_rcp26 <- future_clim %>%
  filter(modelrun == "mpi-esm-mr.1.") %>%
  filter(rcp == "rcp26" & year <= 2060)
save(mpm_rcp26, file = "./data/formatted/fut_clim/mpm_rcp26.Rdata")

mpm_rcp85 <- future_clim %>%
  filter(modelrun == "mpi-esm-mr.1.") %>%
  filter(rcp == "rcp85" & year <= 2060)
save(mpm_rcp85, file = "./data/formatted/fut_clim/mpm_rcp85.Rdata")

mri_rcp26 <- future_clim %>%
  filter(modelrun == "mri-cgcm3.1.") %>%
  filter(rcp == "rcp26" & year <= 2060)
save(mri_rcp26, file = "./data/formatted/fut_clim/mri_rcp26.Rdata")

mri_rcp85 <- future_clim %>%
  filter(modelrun == "mri-cgcm3.1.") %>%
  filter(rcp == "rcp85" & year <= 2060)
save(mri_rcp85, file = "./data/formatted/fut_clim/mri_rcp85.Rdata")

nor_rcp26 <- future_clim %>%
  filter(modelrun == "noresm1-m.1.") %>%
  filter(rcp == "rcp26" & year <= 2060)
save(nor_rcp26, file = "./data/formatted/fut_clim/nor_rcp26.Rdata")

nor_rcp85 <- future_clim %>%
  filter(modelrun == "noresm1-m.1.") %>%
  filter(rcp == "rcp85" & year <= 2060)
save(nor_rcp85, file = "./data/formatted/fut_clim/nor_rcp85.Rdata")

#for climate
proj_tst <- proj_dset %>%
  mutate(lat = LAT, lon = LON) %>%
  ungroup() %>%
  dplyr::select(lat, lon) %>%
  distinct() %>%
  left_join(.,rcp_red) %>%
  filter(is.na(ppt_Jan))


fvs# Density ----


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
  dplyr::select(PLT_CN, Year, TreeId, ActPt, SpeciesFIA, TPA, MortPA, DBH, DG)
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
  filter(!(TRE_CN %in% proj_dset$TRE_CN)) #%>% #remove focal trees
  #filter(Year < 2060) #keep projection short
n_tre <- fvs_proj_red %>%
  dplyr::select(PLT_CN,TreeId,SUBP,SPCD) %>%
  distinct() #5678; reduced 2795 trees
length(unique(proj_dset$TRE_CN)) #so 176 not reduced

#get data set with focal trees for visualization as well
proj_foc <- fvs_proj %>%
  mutate(TRE_CN = as.numeric(CN)) %>%
  filter(TRE_CN %in% proj_dset$TRE_CN) #keep focal trees
save(proj_foc, file = "./data/formatted/projection/proj_foc.Rdata")

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
  mutate(n_yr = lag(Year) - Year,
         DG_int = (lag(DBH) - DBH)/n_yr,
         mort_int = lag(MortPA)/n_yr)

#interpolate dbh
#mortality is applied over trees per acre so interpolate tpa


#interpolate dbh
int_fvs <- function(data){
  end <- data %>% filter(Year == 2100) %>%
    mutate(Year_int = Year,
           DIA_int = DBH,
           TPA_UNADJ = TPA)
  data_red <- data %>% filter(Year < 2100) %>%
    arrange(Year)
  for(i in 1:nrow(data_red)){
    n_g_yr <- data_red$n_yr[i]
    tre_yr_df <- data_red[i,] %>%
      slice(rep(1:n(), each = n_g_yr)) %>%
      mutate(Year_int =  Year[1]:(Year[1]+(n_g_yr-1)),
             DIA_int = NA,
             TPA_UNADJ = NA)
    N <- which(tre_yr_df$Year_int == tre_yr_df$Year[1])
    tre_yr_df$DIA_int[N] <- tre_yr_df$DBH[N] #dbh when year and measure year are equal
    tre_yr_df$TPA_UNADJ[N] <- tre_yr_df$TPA[N]
    Curr_row <- N+1 #each time through subtract 1 and move down one row
    while(Curr_row <= n_g_yr){
      DIA_1 <- tre_yr_df$DIA_int[Curr_row-1]
      tre_yr_df$DIA_int[Curr_row] <- DIA_1 + tre_yr_df$DG_int[Curr_row]
      TPA_1 <- tre_yr_df$TPA_UNADJ[Curr_row-1]
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

proj_nonf <- int_fvs(data = fvs_proj_red)
save(proj_nonf, file = './data/formatted/proj_nonf.Rdata')

#get interpolated tpa for focal trees
fvs_tpa_foc <- fvs_proj %>%
  mutate(TRE_CN = as.numeric(CN)) %>%
  filter(TRE_CN %in% proj_dset$TRE_CN) %>% #keep focal trees
  group_by(PLT_CN,TRE_CN) %>%
  arrange(desc(Year)) %>% 
  mutate(n_yr = lag(Year) - Year,
         mort_int = lag(MortPA)/n_yr)
#function to interpolate just tpa
int_tpa_fvs <- function(data){
  end <- data %>% filter(Year == 2100) %>%
    mutate(Year_int = Year,
           TPA_UNADJ = TPA) %>%
    dplyr::select(PLT_CN,TRE_CN,Year_int,TPA_UNADJ)
  data_red <- data %>% filter(Year < 2100) %>%
    arrange(Year)
  for(i in 1:nrow(data_red)){
    n_g_yr <- data_red$n_yr[i]
    tre_yr_df <- data_red[i,] %>%
      slice(rep(1:n(), each = n_g_yr)) %>%
       mutate(Year_int =  Year[1]:(Year[1]+(n_g_yr-1)),
             TPA_UNADJ = NA)
    mort_c <- tre_yr_df$mort_int[1]
    N <- which(tre_yr_df$Year_int == tre_yr_df$Year[1])
    tre_yr_df$TPA_UNADJ[N] <- tre_yr_df$TPA[N] #tpa when year and measure year are equal
    Curr_row <- N+1 #each time through subtract 1 and move down one row
    while(Curr_row <= n_g_yr){
      TPA_1 <- tre_yr_df$TPA_UNADJ[Curr_row-1]
      tre_yr_df$TPA_UNADJ[Curr_row] <- TPA_1 - mort_c
      #continue loop for next row until curr_row>0
      Curr_row = Curr_row + 1 
    }
    tre_yr_df <- tre_yr_df %>%
      dplyr::select(PLT_CN,TRE_CN,Year_int,mort_int,TPA_UNADJ)
    end <- bind_rows(end,tre_yr_df) 
  }
  return(end)
}
#expand df
proj_tpa_foc <- int_tpa_fvs(fvs_tpa_foc)
#good
proj_tpa_foc <- proj_tpa_foc %>%
  mutate(Year = Year_int) %>%
  dplyr::select(PLT_CN,TRE_CN,Year,TPA_UNADJ)
save(proj_tpa_foc, file = './data/formatted/proj_tpa_foc.Rdata')

#join tpa
proj_dtst <- proj_dset %>%
  ungroup() %>%
  mutate(PLT_CN = as.numeric(PLT_CN),
         TRE_CN = as.numeric(TRE_CN)) %>%
  dplyr::select(PLT_CN, TRE_CN, SPCD, SUBP, MEASYEAR, 
                DESIGNCD, CR,
                ASPECT,SLOPE,sin,cos,LAT,LON,ELEV,
                FVS_LOC_CD,SDIMAX_RMRS,SICOND_c,solrad_MayAug) %>%
  right_join(.,proj_tpa_foc) %>%
  mutate(DIA = NA,
         CCF = NA,
         PCCF = NA,
         BAL = NA,
         SDI = NA)

# Climate-FVS ----

clim_fvs_proj <- proj_dset %>%
  mutate(Ele = ELEV/3.28) %>%
  dplyr::select(PLT_CN,LON,LAT,Ele) %>%
  distinct()
write.table(clim_fvs_proj, file = "./data/formatted/clim_fvs_proj.txt", append = FALSE, sep = " ", dec = ".",
            row.names = FALSE, col.names = TRUE)

#climate-ready fvs data
#http://charcoal.cnre.vt.edu/climate/customData/fvs_data.php

#ensemble of 17 AR5 model predictions, rcp85
#get output from fvs runs
#tree list table
fvs_treelist <- read_csv(file = "./data/raw/FVS/fvs_85.csv")
length(unique(fvs_treelist$StandID))
#mostly to convert 093 -> 93
fvs_treelist$SpeciesFIA <- as.numeric(fvs_treelist$SpeciesFIA)
fvs_red <- fvs_treelist %>%
  dplyr::select(StandID,Year,PrdLen,TreeId,TreeIndex,SpeciesFIA,DBH,DG,PctCr,ActPt) %>%
  filter(SpeciesFIA %in% c(93,122,202)) %>%
  filter(DBH >= 3) #growth model threshold
length(unique(fvs_red$StandID)) #should match above

#connect to fvs ready database for validation data
library(dbplyr)
library(RSQLite)
fvs_val_db <- dbConnect(RSQLite::SQLite(), "./data/raw/FVS/FVS_val.db")
#Extract FVS_StandInit_Plot table
#has the link between stand_cn (aka plt_cn) and stand_id
fvsStandInitPlot<-dbReadTable(fvs_val_db, 'FVS_STANDINIT_PLOT')
# Disconnect from the database
dbDisconnect(fvs_val_db)
std2plt <- fvsStandInitPlot %>%
  dplyr::select(STAND_CN,STAND_ID)
length(unique(std2plt$STAND_ID))
#stand_cn = plt_cn
fvs_red$PLT_CN <- std2plt$STAND_CN[match(fvs_red$StandID,std2plt$STAND_ID)]
length(unique(fvs_red$PLT_CN))

#separate observations from predictions
fvs_obs <- fvs_red %>%
  filter(DG == 0) %>%
  select(-DG)
dim(fvs_obs) 
length(unique(fvs_obs$PLT_CN))

fvs_pred <- fvs_red %>%
  filter(DG != 0) %>%
  select(-c("PrdLen"))
dim(fvs_pred) 
length(unique(fvs_pred$PLT_CN))

#rename columns to be able to merge
colnames(fvs_obs)[colnames(fvs_obs)=="Year"] <- "MEASYEAR"
colnames(fvs_pred)[colnames(fvs_pred)=="Year"] <- "fMEASYEAR"
colnames(fvs_pred)[colnames(fvs_pred)=="DBH"] <- "eDBH"
colnames(fvs_pred)[colnames(fvs_pred)=="PctCr"] <- "CR2"

#merge 
fvs_obs_pred <- left_join(fvs_obs,fvs_pred)
#tree index > 2000 means trees died in projection?

#validation dataset for future observations
UT_FIA <- DBI::dbConnect(RSQLite::SQLite(), "./data/raw/FIADB.db")
TREE <- dbReadTable(UT_FIA, 'TREE')
TREE_red <- tbl(UT_FIA, sql("SELECT CN, PLT_CN, SPCD, SUBP, TREE, DIA, PREV_TRE_CN, STATUSCD FROM TREE")) %>%
  collect()
TREE_red$fDIA <- TREE$DIA[match(TREE_red$CN,TREE$PREV_TRE_CN)]
#get status (dead or alive) of trees at remeasurement
TREE_red$fSTATUSCD <- TREE$STATUSCD[match(TREE_red$CN,TREE$PREV_TRE_CN)]
#prep for merge
colnames(TREE_red)[colnames(TREE_red)=="TREE"] <- "TreeId"
colnames(fvs_obs_pred)[colnames(fvs_obs_pred)=="SpeciesFIA"] <- "SPCD"
colnames(fvs_obs_pred)[colnames(fvs_obs_pred)=="ActPt"] <- "SUBP"
# Disconnect from the database
dbDisconnect(UT_FIA)

#merge fvs runs and fia observations
fvs_check <- left_join(fvs_obs_pred,TREE_red)
length(unique(fvs_check$CN))
#filter for trees that are alive at remeasurement
fvs_check <- fvs_check %>%
  filter(fSTATUSCD == 1)
length(unique(fvs_check$CN))
fvs_clim_red <- fvs_check %>%
  mutate(TRE_CN = as.numeric(CN)) %>%
  filter(TRE_CN %in% val_dset$TRE_CN)
length(unique(fvs_check_red$CN))

#are climate projections different from regular projections?
fvs_clim_check <- fvs_check_red %>%
  dplyr::select(TRE_CN,DIA,fDIA,eDBH)
fvs_clim_check$eDBH_clim <- fvs_clim_red$eDBH[match(fvs_clim_check$TRE_CN,fvs_clim_red$TRE_CN)]
fvs_clim_check <- fvs_clim_check %>%
  mutate(diff_clim = eDBH - eDBH_clim) #0 no difference


# Function ----

proj_an <- function(data,tpa_df,mod_df,mod_pp,mod_es,sp_stats,nonfocal,bratio,ccf_df,CR_fvs_df) {
  #parameters:
  data <- data %>%
    ungroup() %>%
    mutate(PLT_CN = as.numeric(PLT_CN),
           TRE_CN = as.numeric(TRE_CN))
  tpa_df <- tpa_df %>%
    ungroup() %>%
    mutate(PLT_CN = as.numeric(PLT_CN))
  #tpa_df - updated tpa from fvs includes mortality
  data_rep <- data %>%
    dplyr::select(PLT_CN, TRE_CN, SPCD, SUBP, MEASYEAR, 
                  DESIGNCD, CR,
                  ASPECT,SLOPE,sin,cos,LAT,LON,ELEV,
                  FVS_LOC_CD,SDIMAX_RMRS,SICOND_c,solrad_MayAug) %>%
    right_join(.,tpa_df) %>%
    mutate(DIA = NA,
           CCF = NA,
           PCCF = NA,
           BAL = NA,
           SDI = NA,
           CR_fvs = NA)
  ##density for new data in MEASYEAR will already be calculated
  #fill in DIA,CCF, PCCF, BAL, SDI, CR_weib, where year = measyear
  for(i in 1:nrow(data_rep)){
    TRE_CN <- data_rep$TRE_CN[i]
    if(data_rep$Year[i] == data_rep$MEASYEAR[i]){
      data_rep$DIA[i] <- data$DIA[data$TRE_CN == TRE_CN]
      data_rep$CCF[i] <- data$CCF[data$TRE_CN == TRE_CN]
      data_rep$PCCF[i] <- data$PCCF[data$TRE_CN == TRE_CN]
      data_rep$BAL[i] <- data$BAL[data$TRE_CN == TRE_CN]
      data_rep$SDI[i] <- data$SDI_RMRS[data$TRE_CN == TRE_CN]
      data_rep$CR_fvs[i] <- data$CR[data$TRE_CN == TRE_CN]
    }
  }
  
  #mod_obj - list of model objects for each species
  #sp_stats - list of species statistics (mean and standard deviation) for each covariate
  ##will be used to standardize data covariates
  #nonfocal - trees on the same plot as focal trees
  nonfocal <- nonfocal %>%
    ungroup() %>%
    mutate(PLT_CN = as.numeric(PLT_CN),
           TRE_CN = as.numeric(TRE_CN),
           DIA = DIA_int,
           Year = Year_int)
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
        
        #standardize covariates
        #get mean/sd for each species
        if(Species == 202){para_std <- sp_stats$sp_202}
        if(Species == 122){para_std <- sp_stats$sp_122}
        if(Species == 93) {para_std <- sp_stats$sp_93}
        
        #first row is mean, second row is sd
        #the inputs of the equation are from a list so output needs to be unlisted
        plt_yr_df$z.DIA_C[i] = unlist((plt_yr_df[i,"DIA"] - para_std[1,"DIA_C"]) / para_std[2,"DIA_C"])
        plt_yr_df$z.CR_fvs[i] = unlist((plt_yr_df[i,"CR_fvs"] - para_std[1,"CR_fvs"]) / para_std[2,"CR_fvs"])
        plt_yr_df$z.CR[i] = unlist((plt_yr_df[i,"CR"] - para_std[1,"CR"]) / para_std[2,"CR"])
        plt_yr_df$z.BAL[i] = unlist((plt_yr_df[i,"BAL"] - para_std[1,"BAL"]) / para_std[2,"BAL"])
        plt_yr_df$z.CCF[i] = unlist((plt_yr_df[i,"CCF"] - para_std[1,"CCF"]) / para_std[2,"CCF"])
        plt_yr_df$z.PCCF[i] = unlist((plt_yr_df[i,"PCCF"] - para_std[1,"PCCF"]) / para_std[2,"PCCF"])
        plt_yr_df$z.SDI[i] = unlist((plt_yr_df[i,"SDI"] - para_std[1,"SDI"]) / para_std[2,"SDI"])
        plt_yr_df$z.SICOND[i] = unlist((plt_yr_df[i,"SICOND_c"] - para_std[1,"SICOND"]) / para_std[2,"SICOND"])
        plt_yr_df$z.SLOPE[i] = unlist((plt_yr_df[i,"SLOPE"] - para_std[1,"SLOPE"]) / para_std[2,"SLOPE"])
        plt_yr_df$z.sin[i] = unlist((plt_yr_df[i,"sin"] - para_std[1,"sin"]) / para_std[2,"sin"])
        plt_yr_df$z.cos[i] = unlist((plt_yr_df[i,"cos"] - para_std[1,"cos"]) / para_std[2,"cos"])
        plt_yr_df$z.solrad_MayAug[i] = unlist((plt_yr_df[i,"solrad_MayAug"] - para_std[1,"solrad_MayAug"]) / para_std[2,"solrad_MayAug"])
      }
      
      #predict
      plt_yr_df$dds <- NA
      
      #but first separate by species
      # modobj will change with different species
      #DF
      plt_yr_df_df <- plt_yr_df %>% filter(SPCD == 202)
      if(dim(plt_yr_df_df)[1] > 0){
        plt_yr_df_df$dds <- predict(object = mod_df, newdata = plt_yr_df_df,
                                    re.form = NA, type = "response")
      }
      #PP
      plt_yr_df_pp <- plt_yr_df %>% filter(SPCD == 122)
      if(dim(plt_yr_df_pp)[1] > 0){
        plt_yr_df_pp$dds <- predict(object = mod_pp, newdata = plt_yr_df_pp,
                                    re.form = NA, type = "response")
      }
      #ES
      plt_yr_df_es <- plt_yr_df %>% filter(SPCD == 93)
      if(dim(plt_yr_df_es)[1] > 0){
        plt_yr_df_es$dds <- predict(object = mod_es, newdata = plt_yr_df_es,
                                    re.form = NA, type = "response")
      }
      #re.form specify random effects to include
      ##NA include none
      
      #bind rows again
      plt_yr_df <- bind_rows(plt_yr_df_df,plt_yr_df_pp) %>% bind_rows(.,plt_yr_df_es) %>% arrange(TRE_CN)
      
      for(i in 1:nrow(plt_yr_df)) {#for each tree on plot in given year
        Species <- plt_yr_df$SPCD[i]
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
        plt_yr_df$DG[i] <- k * (sqrt((plt_yr_df$DIA[i]/k)^2 + exp(plt_yr_df$dds[i])) -
                                  (plt_yr_df$DIA[i]/k))
        plt_yr_df$DIA2[i] <- plt_yr_df$DIA[i] + plt_yr_df$DG[i]
      }
      
      #so DBH0 + k*DG = DBH
      plt_yr2_df <- plt_df %>%
        filter(Year == (growthyr+1)) %>%
        arrange(TRE_CN)
      for(i in 1:nrow(plt_yr2_df)){
        plt_yr2_df$DIA[i] <- plt_yr_df$DIA2[i]
      }
      
      #TODO grow nonfocal trees
      
      #join with nonfocal
      #fitler for trees on plots in validation set in same year
      non_foc_filt <- nonfocal %>%
        dplyr::select(PLT_CN, TRE_CN, TreeId, SUBP, SPCD,TPA_UNADJ,Year,DIA) %>%
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
        SDIMAX <- ifelse(is.na(plt_yr2_df$SDIMAX_RMRS[i]),
                         CR_fvs_df$SDIMAX[CR_fvs_df$species == Species],
                         plt_yr2_df$SDIMAX_RMRS[i])
        #Calculate relative density
        RD <- plt_yr2_df$SDI[i]/SDIMAX
        
        #Calculate average stand crown ratio (ACR) for each species in the stand
        d0 <- CR_fvs_df$d0[CR_fvs_df$species == Species]
        d1 <- CR_fvs_df$d1[CR_fvs_df$species == Species]
        ACR <- d0 + d1 * RD * 100
        
        #Parameters of Weibull distribution: A,B,C
        a0 <- CR_fvs_df$a0[CR_fvs_df$species == Species]
        b0 <- CR_fvs_df$b0[CR_fvs_df$species == Species]
        b1 <- CR_fvs_df$b1[CR_fvs_df$species == Species]
        c0 <- CR_fvs_df$c0[CR_fvs_df$species == Species]
        c1 <- CR_fvs_df$c1[CR_fvs_df$species == Species]
        
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
        CR_fvs <- X * 10
        
        #get CR the year before
        TRE_CN <- plt_yr2_df$TRE_CN[i]
        #growthyr is still previous year
        CR_1 <- plt_df$CR_fvs[plt_df$TRE_CN == TRE_CN & 
                                plt_df$Year == growthyr] #or CR_fvs[N] for the first round
        #bound to 1% change per year
        cr_bound1 <- CR_1 * .01
        plt_yr2_df$CR_fvs[i] <- ifelse(CR_1 > CR_fvs, 
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

tpa_foc_60 <- proj_tpa_foc %>%
  ungroup() %>%
  filter(Year <= 2060) %>%
  mutate(PLT_CN = as.numeric(PLT_CN))

tr_an <- proj_an(data = proj_dset, tpa_df=proj_tpa_foc, mod_df = an_red_df, mod_pp = an_red_pp, 
                 mod_es = an_cred_es, sp_stats = sp_stats, nonfocal = proj_nonf, bratio = bratio_df,
                 ccf_df = ccf_df, CR_fvs_df = CR_fvs_df)
save(tr_an, file = "./data/formatted/projection/tr_an.Rdata")

#climate
proj_fclim <- function(data,tpa_df,mod_df,mod_pp,mod_es,sp_stats,nonfocal,bratio,ccf_df,CR_fvs_df,cur_clim,fut_clim) {
  #parameters:
  #data - trees from FIADB
  data <- data %>%
    ungroup() %>%
    mutate(PLT_CN = as.numeric(PLT_CN),
           TRE_CN = as.numeric(TRE_CN))
  tpa_df <- tpa_df %>%
    ungroup() %>%
    mutate(PLT_CN = as.numeric(PLT_CN))
  #tpa_df - updated tpa from fvs includes mortality
  data_rep <- data %>%
    dplyr::select(PLT_CN, TRE_CN, SPCD, SUBP, MEASYEAR, 
                  DESIGNCD, CR,
                  ASPECT,SLOPE,sin,cos,LAT,LON,ELEV,
                  FVS_LOC_CD,SDIMAX_RMRS,SICOND_c,solrad_MayAug,n_ppt,n_tmp) %>%
    right_join(.,tpa_df) %>%
    mutate(DIA = NA,
           DG = NA,
           CCF = NA,
           PCCF = NA,
           BAL = NA,
           SDI = NA,
           CR_fvs = NA)

  ##density for new data in MEASYEAR will already be calculated
  #fill in DIA,CCF, PCCF, BAL, SDI, CR_weib, where year = measyear
  for(i in 1:nrow(data_rep)){
    TRE_CN <- data_rep$TRE_CN[i]
    if(data_rep$Year[i] == data_rep$MEASYEAR[i]){
      data_rep$DIA[i] <- data$DIA[data$TRE_CN == TRE_CN ]
      data_rep$CCF[i] <- data$CCF[data$TRE_CN == TRE_CN]
      data_rep$PCCF[i] <- data$PCCF[data$TRE_CN == TRE_CN]
      data_rep$BAL[i] <- data$BAL[data$TRE_CN == TRE_CN]
      data_rep$SDI[i] <- data$SDI_RMRS[data$TRE_CN == TRE_CN]
      data_rep$CR_fvs[i] <- data$CR[data$TRE_CN == TRE_CN]
    }
  }
  #climate projections
  #current climate
  cur_clim <- cur_clim %>%
    ungroup() %>%
    mutate(PLT_CN = as.numeric(PLT_CN))%>%
    filter(Year <= 2019) %>%
    filter(PLT_CN %in% unique(data$PLT_CN)) %>%
    dplyr::select(PLT_CN,Year,ppt_pJunSep,tmax_JunAug,tmax_FebJul,tmax_pAug)
  #future climate
  fut_clim <- fut_clim %>%
    ungroup() %>%
    mutate(Year = as.numeric(year),
           PLT_CN = as.numeric(PLT_CN)) %>%
    filter(Year >= 2020) %>%
    filter(PLT_CN %in% unique(data$PLT_CN)) %>%
    dplyr::select(PLT_CN,Year,ppt_pJunSep,tmax_JunAug,tmax_FebJul,tmax_pAug)
  #all climate
  all_clim <- bind_rows(cur_clim,fut_clim)
  #fill climate
  data_rep <- left_join(data_rep,all_clim, by = c("PLT_CN","Year"))

  #mod_obj - list of model objects for each species
  #sp_stats - list of species statistics (mean and standard deviation) for each covariate
  ##will be used to standardize data covariates
  #nonfocal - trees on the same plot as focal trees
  nonfocal <- nonfocal %>%
    ungroup() %>%
    mutate(PLT_CN = as.numeric(PLT_CN),
           TRE_CN = as.numeric(TRE_CN),
           DIA = DIA_int,
           Year = Year_int)
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
        
        #standardize covariates
        #get mean/sd for each species
        if(Species == 202){para_std <- sp_stats$sp_202}
        if(Species == 122){para_std <- sp_stats$sp_122}
        if(Species == 93) {para_std <- sp_stats$sp_93}
        
        #first row is mean, second row is sd
        #the inputs of the equation are from a list so output needs to be unlisted
        plt_yr_df$z.DIA_C[i] = unlist((plt_yr_df[i,"DIA"] - para_std[1,"DIA_C"]) / para_std[2,"DIA_C"])
        plt_yr_df$z.CR_fvs[i] = unlist((plt_yr_df[i,"CR_fvs"] - para_std[1,"CR_fvs"]) / para_std[2,"CR_fvs"])
        plt_yr_df$z.CR[i] = unlist((plt_yr_df[i,"CR"] - para_std[1,"CR"]) / para_std[2,"CR"])
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
        plt_yr_df$z.n_ppt[i] = unlist((plt_yr_df[i,"n_ppt"] - 
                                               para_std[1,"n_ppt"]) / para_std[2,"n_ppt"])
        plt_yr_df$z.n_tmp[i] = unlist((plt_yr_df[i,"n_tmp"] - 
                                               para_std[1,"n_tmp"]) / para_std[2,"n_tmp"])
        
        #TODO there must be a better way to do this? for loop?
        
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
      }
      
      #predict
      plt_yr_df$dds <- NA
      
      #but first separate by species
      # modobj will change with different species
      #DF
      plt_yr_df_df <- plt_yr_df %>% filter(SPCD == 202)
      if(dim(plt_yr_df_df)[1] > 0){
        plt_yr_df_df$dds <- predict(object = mod_df, newdata = plt_yr_df_df,
                                    re.form = NA, type = "response")
      }
      #PP
      plt_yr_df_pp <- plt_yr_df %>% filter(SPCD == 122)
      if(dim(plt_yr_df_pp)[1] > 0){
        plt_yr_df_pp$dds <- predict(object = mod_pp, newdata = plt_yr_df_pp,
                                    re.form = NA, type = "response")
      }
      #ES
      plt_yr_df_es <- plt_yr_df %>% filter(SPCD == 93)
      if(dim(plt_yr_df_es)[1] > 0){
        plt_yr_df_es$dds <- predict(object = mod_es, newdata = plt_yr_df_es,
                                    re.form = NA, type = "response")
      }
      #re.form specify random effects to include
      ##NA include none
      
      #bind rows again
      plt_yr_df <- bind_rows(plt_yr_df_df,plt_yr_df_pp) %>% bind_rows(.,plt_yr_df_es) %>% arrange(TRE_CN)
      
      for(i in 1:nrow(plt_yr_df)) {#for each tree on plot in given year
        Species <- plt_yr_df$SPCD[i]
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
        plt_yr_df$DG[i] <- k * (sqrt((plt_yr_df$DIA[i]/k)^2 + exp(plt_yr_df$dds[i])) -
                                  (plt_yr_df$DIA[i]/k))
        plt_yr_df$DIA2[i] <- plt_yr_df$DIA[i] + plt_yr_df$DG[i]
      }
      
      #so DBH0 + k*DG = DBH
      plt_yr2_df <- plt_df %>%
        filter(Year == (growthyr+1)) %>%
        arrange(TRE_CN)
      for(i in 1:nrow(plt_yr2_df)){
        plt_yr2_df$DIA[i] <- plt_yr_df$DIA2[i]
      }
      
      #TODO grow nonfocal trees
      
      #join with nonfocal
      #fitler for trees on plots in validation set in same year
      non_foc_filt <- nonfocal %>%
        dplyr::select(PLT_CN, TRE_CN, SUBP, SPCD,TPA_UNADJ,Year,DIA) %>%
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
        SDIMAX <- ifelse(is.na(plt_yr2_df$SDIMAX_RMRS[i]),
                         CR_fvs_df$SDIMAX[CR_fvs_df$species == Species],
                         plt_yr2_df$SDIMAX_RMRS[i])
        #Calculate relative density
        RD <- plt_yr2_df$SDI[i]/SDIMAX
        
        #Calculate average stand crown ratio (ACR) for each species in the stand
        d0 <- CR_fvs_df$d0[CR_fvs_df$species == Species]
        d1 <- CR_fvs_df$d1[CR_fvs_df$species == Species]
        ACR <- d0 + d1 * RD * 100
        
        #Parameters of Weibull distribution: A,B,C
        a0 <- CR_fvs_df$a0[CR_fvs_df$species == Species]
        b0 <- CR_fvs_df$b0[CR_fvs_df$species == Species]
        b1 <- CR_fvs_df$b1[CR_fvs_df$species == Species]
        c0 <- CR_fvs_df$c0[CR_fvs_df$species == Species]
        c1 <- CR_fvs_df$c1[CR_fvs_df$species == Species]
        
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
        CR_fvs <- X * 10
        
        #get CR the year before
        TRE_CN <- plt_yr2_df$TRE_CN[i]
        #growthyr is still previous year
        CR_1 <- plt_df$CR_fvs[plt_df$TRE_CN == TRE_CN & 
                                plt_df$Year == growthyr] #or CR_fvs[N] for the first round
        #bound to 1% change per year
        cr_bound1 <- CR_1 * .01
        plt_yr2_df$CR_fvs[i] <- ifelse(CR_1 > CR_fvs, 
                                       CR_1 - cr_bound1,
                                       CR_1 + cr_bound1)
      }
      
      #join with plt_df
      for(i in 1:nrow(plt_df)){
        TRE_CN <- plt_df$TRE_CN[i]
        if(plt_df$Year[i] == (growthyr)){
          plt_df$DG[i] <- plt_yr_df$DG[plt_yr_df$TRE_CN == TRE_CN]
        }
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

load('./data/formatted/proj_dset.Rdata')
load("./data/formatted/proj_tpa_foc.Rdata")
load('./data/formatted/proj_nonf.Rdata')
load('./data/formatted/proj_clim.Rdata')

df_stats <- varmt_df_z %>%
  ungroup() %>%
  dplyr::select(DIA_C,SICOND,tASPECT,SLOPE,BAL,SDI,CR,CR_fvs,PCCF,CCF,
                cos,sin,solrad_MayAug,ppt_pJunSep,tmax_FebJul,n_ppt,n_tmp) %>%
  sapply(.,function(x) c(mean=mean(x,na.rm = T),
                         sd=sd(x,na.rm = T)))
pp_stats <- varmt_pp_z %>%
  ungroup() %>%
  dplyr::select(DIA_C,SICOND,tASPECT,SLOPE,BAL,SDI,CR,CR_fvs,PCCF,CCF,
                cos,sin,solrad_MayAug,ppt_pJunSep,tmax_JunAug,n_ppt,n_tmp) %>%
  sapply(.,function(x) c(mean=mean(x,na.rm = T),
                         sd=sd(x,na.rm = T)))
es_stats <- varmt_es_z %>%
  ungroup() %>%
  dplyr::select(DIA_C,SICOND,tASPECT,SLOPE,BAL,SDI,CR,CR_fvs,PCCF,CCF,
                cos,sin,solrad_MayAug,ppt_pJunSep,tmax_pAug,n_ppt,n_tmp) %>%
  sapply(.,function(x) c(mean=mean(x,na.rm = T),
                         sd=sd(x,na.rm = T)))
sp_stats <- list(sp_202 = df_stats, sp_122 = pp_stats,sp_93 = es_stats)

load("./data/formatted/fut_clim/acc_rcp85.Rdata")
red_acc85 <- proj_fclim(data = proj_dset, tpa_df=proj_tpa_foc, mod_df = clim_red_df, mod_pp = clim_red_pp, 
                        mod_es = clim_cred_es, sp_stats = sp_stats, nonfocal = proj_nonf, bratio = bratio_df,
                        ccf_df = ccf_df, CR_fvs_df = CR_fvs_df, cur_clim = proj_clim, fut_clim = acc_rcp85)
save(red_acc85, file = "./data/formatted/projection/red clim/red_acc85.Rdata")
rm(red_acc85,acc_rcp85)

load("./data/formatted/fut_clim/bcc_rcp26.Rdata")
red_bcc26 <- proj_fclim(data = proj_dset, tpa_df=proj_tpa_foc, mod_df = clim_red_df, mod_pp = clim_red_pp, 
                        mod_es = clim_cred_es, sp_stats = sp_stats, nonfocal = proj_nonf, bratio = bratio_df,
                         ccf_df = ccf_df, CR_fvs_df = CR_fvs_df, cur_clim = proj_clim, fut_clim = bcc_rcp26)
save(red_bcc26, file = "./data/formatted/projection/red clim/red_bcc26.Rdata")
rm(red_bcc26,bcc_rcp26)

load("./data/formatted/fut_clim/bcc_rcp85.Rdata")
red_bcc85 <- proj_fclim(data = proj_dset, tpa_df=proj_tpa_foc, mod_df = clim_red_df, mod_pp = clim_red_pp, 
                        mod_es = clim_cred_es, sp_stats = sp_stats, nonfocal = proj_nonf, bratio = bratio_df,
                        ccf_df = ccf_df, cur_clim = proj_clim, fut_clim = bcc_rcp85)
save(red_bcc85, file = "./data/formatted/projection/red clim/red_bcc85.Rdata")

load("./data/formatted/fut_clim/bccm_rcp85.Rdata")
red_bccm85 <- proj_fclim(data = proj_dset, tpa_df=proj_tpa_foc, mod_df = clim_red_df, mod_pp = clim_red_pp, 
                         mod_es = clim_cred_es, sp_stats = sp_stats, nonfocal = proj_nonf, bratio = bratio_df,
                        ccf_df = ccf_df, CR_fvs_df = CR_fvs_df, cur_clim = proj_clim, fut_clim = bccm_rcp85)
save(red_bccm85, file = "./data/formatted/projection/red clim/red_bccm85.Rdata")
rm(red_bccm85,bccm_rcp85)

load("./data/formatted/fut_clim/can_rcp26.Rdata")
red_can26 <- proj_fclim(data = proj_dset, tpa_df=proj_tpa_foc, mod_df = clim_red_df, mod_pp = clim_red_pp, 
                        mod_es = clim_cred_es, sp_stats = sp_stats, nonfocal = proj_nonf, bratio = bratio_df,
                        ccf_df = ccf_df,CR_fvs_df = CR_fvs_df,cur_clim = proj_clim, fut_clim = can_rcp26)
save(red_can26, file = "./data/formatted/projection/red clim/red_can26.Rdata")
rm(red_can26,can_rcp26)

load("./data/formatted/fut_clim/can_rcp85.Rdata")
red_can85 <- proj_fclim(data = proj_dset, tpa_df=proj_tpa_foc, mod_df = clim_red_df, mod_pp = clim_red_pp, 
                        mod_es = clim_cred_es, sp_stats = sp_stats, nonfocal = proj_nonf, bratio = bratio_df,
                        ccf_df = ccf_df, CR_fvs_df = CR_fvs_df, cur_clim = proj_clim, fut_clim = can_rcp85)
save(red_can85, file = "./data/formatted/projection/red clim/red_can85.Rdata")
rm(red_can85,can_rcp85)

load("./data/formatted/fut_clim/ccs_rcp26.Rdata")
red_ccs26 <- proj_fclim(data = proj_dset, tpa_df=proj_tpa_foc, mod_df = clim_red_df, mod_pp = clim_red_pp, 
                        mod_es = clim_cred_es, sp_stats = sp_stats, nonfocal = proj_nonf, bratio = bratio_df,
                        ccf_df = ccf_df,CR_fvs_df = CR_fvs_df, cur_clim = proj_clim, fut_clim = ccs_rcp26)
save(red_ccs26, file = "./data/formatted/projection/red clim/red_ccs26.Rdata")
rm(red_ccs26,ccs_rcp26)

load("./data/formatted/fut_clim/ccs_rcp85.Rdata")
red_ccs85 <- proj_fclim(data = proj_dset, tpa_df=proj_tpa_foc, mod_df = clim_red_df, mod_pp = clim_red_pp, 
                        mod_es = clim_cred_es, sp_stats = sp_stats, nonfocal = proj_nonf, bratio = bratio_df,
                        ccf_df = ccf_df, CR_fvs_df = CR_fvs_df,cur_clim = proj_clim, fut_clim = ccs_rcp85)
save(red_ccs85, file = "./data/formatted/projection/red clim/red_ccs85.Rdata")
rm(red_ccs85,ccs_rcp85)

load("./data/formatted/fut_clim/ces_rcp26.Rdata")
red_ces26 <- proj_fclim(data = proj_dset, tpa_df=proj_tpa_foc,mod_df = clim_red_df, mod_pp = clim_red_pp, 
                        mod_es = clim_cred_es, sp_stats = sp_stats, nonfocal = proj_nonf, bratio = bratio_df,
                        ccf_df = ccf_df, CR_fvs_df = CR_fvs_df,cur_clim = proj_clim, fut_clim = ces_rcp26)
save(red_ces26, file = "./data/formatted/projection/red clim/red_ces26.Rdata")
rm(red_ces26,ces_rcp26)

load("./data/formatted/fut_clim/ces_rcp85.Rdata")
red_ces85 <- proj_fclim(data = proj_dset, tpa_df=proj_tpa_foc, mod_df = clim_red_df, mod_pp = clim_red_pp, 
                        mod_es = clim_cred_es, sp_stats = sp_stats, nonfocal = proj_nonf, bratio = bratio_df,
                        ccf_df = ccf_df,CR_fvs_df = CR_fvs_df, cur_clim = proj_clim, fut_clim = ces_rcp85)
save(red_ces85, file = "./data/formatted/projection/red clim/red_ces85.Rdata")
rm(red_ces85,ces_rcp85)

load("./data/formatted/fut_clim/cesb_rcp85.Rdata")
red_cesb85 <- proj_fclim(data = proj_dset, tpa_df=proj_tpa_foc, mod_df = clim_red_df, mod_pp = clim_red_pp, 
                         mod_es = clim_cred_es, sp_stats = sp_stats, nonfocal = proj_nonf, bratio = bratio_df,
                       ccf_df = ccf_df, CR_fvs_df = CR_fvs_df,cur_clim = proj_clim, fut_clim = cesb_rcp85)
save(red_cesb85, file = "./data/formatted/projection/red clim/red_cesb85.Rdata")
rm(red_cesb85,cesb_rcp85)

load("./data/formatted/fut_clim/cmc_rcp85.Rdata")
red_cmc85 <- proj_fclim(data = proj_dset, tpa_df=proj_tpa_foc, mod_df = clim_red_df, mod_pp = clim_red_pp, 
                        mod_es = clim_cred_es, sp_stats = sp_stats, nonfocal = proj_nonf, bratio = bratio_df,
                      ccf_df = ccf_df, CR_fvs_df = CR_fvs_df,cur_clim = proj_clim, fut_clim = cmc_rcp85)
save(red_cmc85, file = "./data/formatted/projection/red clim/red_cmcb85.Rdata")
rm(red_cmc85,cmc_rcp85)

load("./data/formatted/fut_clim/cnm_rcp85.Rdata")
red_cnm85 <- proj_fclim(data = proj_dset, tpa_df=proj_tpa_foc, mod_df = clim_red_df, mod_pp = clim_red_pp, 
                        mod_es = clim_cred_es, sp_stats = sp_stats, nonfocal = proj_nonf, bratio = bratio_df,
                      ccf_df = ccf_df, CR_fvs_df = CR_fvs_df,cur_clim = proj_clim, fut_clim = cnm_rcp85)
save(red_cnm85, file = "./data/formatted/projection/red clim/red_cnm85.Rdata")
rm(red_cnm85,cnm_rcp85)

load("./data/formatted/fut_clim/csr_rcp26.Rdata")
red_csr26 <- proj_fclim(data = proj_dset, tpa_df=proj_tpa_foc, mod_df = clim_red_df, mod_pp = clim_red_pp, 
                        mod_es = clim_cred_es, sp_stats = sp_stats, nonfocal = proj_nonf, bratio = bratio_df,
                      ccf_df = ccf_df, CR_fvs_df = CR_fvs_df,cur_clim = proj_clim, fut_clim = csr_rcp26)
save(red_csr26, file = "./data/formatted/projection/red clim/red_csr26.Rdata")
rm(red_csr26,csr_rcp26)

load("./data/formatted/fut_clim/csr_rcp85.Rdata")
red_csr85 <- proj_fclim(data = proj_dset, tpa_df=proj_tpa_foc,mod_df = clim_red_df, mod_pp = clim_red_pp, 
                        mod_es = clim_cred_es, sp_stats = sp_stats, nonfocal = proj_nonf, bratio = bratio_df,
                      ccf_df = ccf_df, CR_fvs_df = CR_fvs_df,cur_clim = proj_clim, fut_clim = csr_rcp85)
save(red_csr85, file = "./data/formatted/projection/red clim/red_csr85.Rdata")
rm(red_csr85,csr_rcp85)

load("./data/formatted/fut_clim/fgl_rcp26.Rdata")
red_fgl26 <- proj_fclim(data = proj_dset, tpa_df=proj_tpa_foc, mod_df = clim_red_df, mod_pp = clim_red_pp, 
                        mod_es = clim_cred_es, sp_stats = sp_stats, nonfocal = proj_nonf, bratio = bratio_df,
                        ccf_df = ccf_df, CR_fvs_df = CR_fvs_df,cur_clim = proj_clim, fut_clim = fgl_rcp26)
save(red_fgl26, file = "./data/formatted/projection/red clim/red_fgl26.Rdata")
rm(red_fgl26,fgl_rcp26)

load("./data/formatted/fut_clim/fgl_rcp85.Rdata")
red_fgl85 <- proj_fclim(data = proj_dset, tpa_df=proj_tpa_foc, mod_df = clim_red_df, mod_pp = clim_red_pp, 
                        mod_es = clim_cred_es, sp_stats = sp_stats, nonfocal = proj_nonf, bratio = bratio_df,
                        ccf_df = ccf_df, CR_fvs_df = CR_fvs_df,cur_clim = proj_clim, fut_clim = fgl_rcp85)
save(red_fgl85, file = "./data/formatted/projection/red clim/red_fgl85.Rdata")
rm(red_fgl85,fgl_rcp85)

load("./data/formatted/fut_clim/fio_rcp26.Rdata")
red_fio26 <- proj_fclim(data = proj_dset, tpa_df=proj_tpa_foc, mod_df = clim_red_df, mod_pp = clim_red_pp, 
                        mod_es = clim_cred_es, sp_stats = sp_stats, nonfocal = proj_nonf, bratio = bratio_df,
                        ccf_df = ccf_df, CR_fvs_df = CR_fvs_df,cur_clim = proj_clim, fut_clim = fio_rcp26)
save(red_fio26, file = "./data/formatted/projection/red clim/red_fio26.Rdata")
rm(fio_rcp26,red_fio26)

load("./data/formatted/fut_clim/fio_rcp85.Rdata")
red_fio85 <- proj_fclim(data = proj_dset, tpa_df=proj_tpa_foc, mod_df = clim_red_df, mod_pp = clim_red_pp, 
                        mod_es = clim_cred_es, sp_stats = sp_stats, nonfocal = proj_nonf, bratio = bratio_df,
                        ccf_df = ccf_df, CR_fvs_df = CR_fvs_df,cur_clim = proj_clim, fut_clim = fio_rcp85)
save(red_fio85, file = "./data/formatted/projection/red clim/red_fio85.Rdata")
rm(red_fio85,fio_rcp85)

load("./data/formatted/fut_clim/gfc_rcp26.Rdata")
red_gfc26 <- proj_fclim(data = proj_dset, tpa_df=proj_tpa_foc, mod_df = clim_red_df, mod_pp = clim_red_pp, 
                        mod_es = clim_cred_es,sp_stats = sp_stats, nonfocal = proj_nonf, bratio = bratio_df,
                        ccf_df = ccf_df, CR_fvs_df = CR_fvs_df,cur_clim = proj_clim, fut_clim = gfc_rcp26)
save(red_gfc26, file = "./data/formatted/projection/red clim/red_gfc26.Rdata")
rm(red_gfc26,gfc_rcp26)

load("./data/formatted/fut_clim/gfc_rcp85.Rdata")
red_gfc85 <- proj_fclim(data = proj_dset, tpa_df=proj_tpa_foc, mod_df = clim_red_df, mod_pp = clim_red_pp, 
                        mod_es = clim_cred_es, sp_stats = sp_stats, nonfocal = proj_nonf, bratio = bratio_df,
                        ccf_df = ccf_df,CR_fvs_df = CR_fvs_df, cur_clim = proj_clim, fut_clim = gfc_rcp85)
save(red_gfc85, file = "./data/formatted/projection/red clim/red_gfc85.Rdata")
rm(red_gfc85,gfc_rcp85)

load("./data/formatted/fut_clim/gfg_rcp26.Rdata")
red_gfg26 <- proj_fclim(data = proj_dset, tpa_df=proj_tpa_foc, mod_df = clim_red_df, mod_pp = clim_red_pp, 
                        mod_es = clim_cred_es,sp_stats = sp_stats, nonfocal = proj_nonf, bratio = bratio_df,
                        ccf_df = ccf_df, CR_fvs_df = CR_fvs_df,cur_clim = proj_clim, fut_clim = gfg_rcp26)
save(red_gfg26, file = "./data/formatted/projection/red clim/red_gfg26.Rdata")
rm(red_gfg26,gfg_rcp26)

load("./data/formatted/fut_clim/gfg_rcp85.Rdata")
red_gfg85 <- proj_fclim(data = proj_dset, tpa_df=proj_tpa_foc, mod_df = clim_red_df, mod_pp = clim_red_pp, 
                        mod_es = clim_cred_es, sp_stats = sp_stats, nonfocal = proj_nonf, bratio = bratio_df,
                        ccf_df = ccf_df,CR_fvs_df = CR_fvs_df, cur_clim = proj_clim, fut_clim = gfg_rcp85)
save(red_gfg85, file = "./data/formatted/projection/red clim/red_gfg85.Rdata")
rm(red_gfg85,gfg_rcp85)

load("./data/formatted/fut_clim/gfm_rcp26.Rdata")
red_gfm26 <- proj_fclim(data = proj_dset, tpa_df=proj_tpa_foc, mod_df = clim_red_df, mod_pp = clim_red_pp, 
                        mod_es = clim_cred_es,sp_stats = sp_stats, nonfocal = proj_nonf, bratio = bratio_df,
                        ccf_df = ccf_df, CR_fvs_df = CR_fvs_df,cur_clim = proj_clim, fut_clim = gfm_rcp26)
save(red_gfm26, file = "./data/formatted/projection/red clim/red_gfm26.Rdata")
rm(red_gfm26,gfm_rcp26)

load("./data/formatted/fut_clim/gfm_rcp85.Rdata")
red_gfm85 <- proj_fclim(data = proj_dset, tpa_df=proj_tpa_foc, mod_df = clim_red_df, mod_pp = clim_red_pp, 
                        mod_es = clim_cred_es, sp_stats = sp_stats, nonfocal = proj_nonf, bratio = bratio_df,
                        ccf_df = ccf_df,CR_fvs_df = CR_fvs_df, cur_clim = proj_clim, fut_clim = gfm_rcp85)
save(red_gfm85, file = "./data/formatted/projection/red clim/red_gfm85.Rdata")
rm(red_gfm85,gfm_rcp85)

load("./data/formatted/fut_clim/gis_rcp26.Rdata")
red_gis26 <- proj_fclim(data = proj_dset, tpa_df=proj_tpa_foc, mod_df = clim_red_df, mod_pp = clim_red_pp, 
                        mod_es = clim_cred_es,sp_stats = sp_stats, nonfocal = proj_nonf, bratio = bratio_df,
                        ccf_df = ccf_df, CR_fvs_df = CR_fvs_df,cur_clim = proj_clim, fut_clim = gis_rcp26)
save(red_gis26, file = "./data/formatted/projection/red clim/red_gis26.Rdata")
rm(red_gis26,gis_rcp26)

load("./data/formatted/fut_clim/gis_rcp85.Rdata")
red_gis85 <- proj_fclim(data = proj_dset, tpa_df=proj_tpa_foc, mod_df = clim_red_df, mod_pp = clim_red_pp, 
                        mod_es = clim_cred_es, sp_stats = sp_stats, nonfocal = proj_nonf, bratio = bratio_df,
                        ccf_df = ccf_df,CR_fvs_df = CR_fvs_df, cur_clim = proj_clim, fut_clim = gis_rcp85)
save(red_gis85, file = "./data/formatted/projection/red clim/red_gis85.Rdata")
rm(red_gis85,gis_rcp85)

load("./data/formatted/fut_clim/had_rcp26.Rdata")
red_had26 <- proj_fclim(data = proj_dset, tpa_df=proj_tpa_foc, mod_df = clim_red_df, mod_pp = clim_red_pp, 
                        mod_es = clim_cred_es,sp_stats = sp_stats, nonfocal = proj_nonf, bratio = bratio_df,
                        ccf_df = ccf_df, CR_fvs_df = CR_fvs_df,cur_clim = proj_clim, fut_clim = had_rcp26)
save(red_had26, file = "./data/formatted/projection/red clim/red_had26.Rdata")
rm(red_had26,had_rcp26)

load("./data/formatted/fut_clim/had_rcp85.Rdata")
red_had85 <- proj_fclim(data = proj_dset, tpa_df=proj_tpa_foc, mod_df = clim_red_df, mod_pp = clim_red_pp, 
                        mod_es = clim_cred_es, sp_stats = sp_stats, nonfocal = proj_nonf, bratio = bratio_df,
                        ccf_df = ccf_df,CR_fvs_df = CR_fvs_df, cur_clim = proj_clim, fut_clim = had_rcp85)
save(red_had85, file = "./data/formatted/projection/red clim/red_had85.Rdata")
rm(red_had85,had_rcp85)

load("./data/formatted/fut_clim/hada_rcp26.Rdata")
red_hada26 <- proj_fclim(data = proj_dset, tpa_df=proj_tpa_foc, mod_df = clim_red_df, mod_pp = clim_red_pp, 
                        mod_es = clim_cred_es,sp_stats = sp_stats, nonfocal = proj_nonf, bratio = bratio_df,
                        ccf_df = ccf_df, CR_fvs_df = CR_fvs_df,cur_clim = proj_clim, fut_clim = hada_rcp26)
save(red_hada26, file = "./data/formatted/projection/red clim/red_hada26.Rdata")
rm(red_hada26,hada_rcp26)

load("./data/formatted/fut_clim/hada_rcp85.Rdata")
red_hada85 <- proj_fclim(data = proj_dset, tpa_df=proj_tpa_foc, mod_df = clim_red_df, mod_pp = clim_red_pp, 
                        mod_es = clim_cred_es, sp_stats = sp_stats, nonfocal = proj_nonf, bratio = bratio_df,
                        ccf_df = ccf_df,CR_fvs_df = CR_fvs_df, cur_clim = proj_clim, fut_clim = hada_rcp85)
save(red_hada85, file = "./data/formatted/projection/red clim/red_hada85.Rdata")
rm(red_hada85,hada_rcp85)

load("./data/formatted/fut_clim/hadc_rcp85.Rdata")
red_hadc85 <- proj_fclim(data = proj_dset, tpa_df=proj_tpa_foc, mod_df = clim_red_df, mod_pp = clim_red_pp, 
                        mod_es = clim_cred_es, sp_stats = sp_stats, nonfocal = proj_nonf, bratio = bratio_df,
                        ccf_df = ccf_df,CR_fvs_df = CR_fvs_df, cur_clim = proj_clim, fut_clim = hadc_rcp85)
save(red_hadc85, file = "./data/formatted/projection/red clim/red_hadc85.Rdata")
rm(red_hadc85,hadc_rcp85)

load("./data/formatted/fut_clim/inm_rcp85.Rdata")
red_inm85 <- proj_fclim(data = proj_dset, tpa_df=proj_tpa_foc, mod_df = clim_red_df, mod_pp = clim_red_pp, 
                         mod_es = clim_cred_es, sp_stats = sp_stats, nonfocal = proj_nonf, bratio = bratio_df,
                         ccf_df = ccf_df,CR_fvs_df = CR_fvs_df, cur_clim = proj_clim, fut_clim = inm_rcp85)
save(red_inm85, file = "./data/formatted/projection/red clim/red_inm85.Rdata")
rm(red_inm85,inm_rcp85)

load("./data/formatted/fut_clim/ipl_rcp85.Rdata")
red_ipl85 <- proj_fclim(data = proj_dset, tpa_df=proj_tpa_foc, mod_df = clim_red_df, mod_pp = clim_red_pp, 
                        mod_es = clim_cred_es, sp_stats = sp_stats, nonfocal = proj_nonf, bratio = bratio_df,
                        ccf_df = ccf_df,CR_fvs_df = CR_fvs_df, cur_clim = proj_clim, fut_clim = ipl_rcp85)
save(red_ipl85, file = "./data/formatted/projection/red clim/red_ipl85.Rdata")
rm(red_ipl85,ipl_rcp85)

load("./data/formatted/fut_clim/ipm_rcp26.Rdata")
red_ipm26 <- proj_fclim(data = proj_dset, tpa_df=proj_tpa_foc, mod_df = clim_red_df, mod_pp = clim_red_pp, 
                        mod_es = clim_cred_es,sp_stats = sp_stats, nonfocal = proj_nonf, bratio = bratio_df,
                        ccf_df = ccf_df, CR_fvs_df = CR_fvs_df,cur_clim = proj_clim, fut_clim = ipm_rcp26)
save(red_ipm26, file = "./data/formatted/projection/red clim/red_ipm26.Rdata")
rm(red_ipm26,ipm_rcp26)

load("./data/formatted/fut_clim/ipm_rcp85.Rdata")
red_ipm85 <- proj_fclim(data = proj_dset, tpa_df=proj_tpa_foc, mod_df = clim_red_df, mod_pp = clim_red_pp, 
                        mod_es = clim_cred_es, sp_stats = sp_stats, nonfocal = proj_nonf, bratio = bratio_df,
                        ccf_df = ccf_df,CR_fvs_df = CR_fvs_df, cur_clim = proj_clim, fut_clim = ipm_rcp85)
save(red_ipm85, file = "./data/formatted/projection/red clim/red_ipm85.Rdata")
rm(red_ipm85,ipm_rcp85)

load("./data/formatted/fut_clim/mir_rcp26.Rdata")
red_mir26 <- proj_fclim(data = proj_dset, tpa_df=proj_tpa_foc, mod_df = clim_red_df, mod_pp = clim_red_pp, 
                        mod_es = clim_cred_es,sp_stats = sp_stats, nonfocal = proj_nonf, bratio = bratio_df,
                        ccf_df = ccf_df, CR_fvs_df = CR_fvs_df,cur_clim = proj_clim, fut_clim = mir_rcp26)
save(red_mir26, file = "./data/formatted/projection/red clim/red_mir26.Rdata")
rm(red_mir26,mir_rcp26)

load("./data/formatted/fut_clim/mir_rcp85.Rdata")
red_mir85 <- proj_fclim(data = proj_dset, tpa_df=proj_tpa_foc, mod_df = clim_red_df, mod_pp = clim_red_pp, 
                        mod_es = clim_cred_es, sp_stats = sp_stats, nonfocal = proj_nonf, bratio = bratio_df,
                        ccf_df = ccf_df,CR_fvs_df = CR_fvs_df, cur_clim = proj_clim, fut_clim = mir_rcp85)
save(red_mir85, file = "./data/formatted/projection/red clim/red_mir85.Rdata")
rm(red_mir85,mir_rcp85)

load("./data/formatted/fut_clim/mpl_rcp26.Rdata")
red_mpl26 <- proj_fclim(data = proj_dset, tpa_df=proj_tpa_foc, mod_df = clim_red_df, mod_pp = clim_red_pp, 
                        mod_es = clim_cred_es,sp_stats = sp_stats, nonfocal = proj_nonf, bratio = bratio_df,
                        ccf_df = ccf_df, CR_fvs_df = CR_fvs_df,cur_clim = proj_clim, fut_clim = mpl_rcp26)
save(red_mpl26, file = "./data/formatted/projection/red clim/red_mpl26.Rdata")
rm(red_mpl26,mpl_rcp26)

load("./data/formatted/fut_clim/mpl_rcp85.Rdata")
red_mpl85 <- proj_fclim(data = proj_dset, tpa_df=proj_tpa_foc, mod_df = clim_red_df, mod_pp = clim_red_pp, 
                        mod_es = clim_cred_es, sp_stats = sp_stats, nonfocal = proj_nonf, bratio = bratio_df,
                        ccf_df = ccf_df,CR_fvs_df = CR_fvs_df, cur_clim = proj_clim, fut_clim = mpl_rcp85)
save(red_mpl85, file = "./data/formatted/projection/red clim/red_mpl85.Rdata")
rm(red_mpl85,mpl_rcp85)

load("./data/formatted/fut_clim/mpm_rcp26.Rdata")
red_mpm26 <- proj_fclim(data = proj_dset, tpa_df=proj_tpa_foc, mod_df = clim_red_df, mod_pp = clim_red_pp, 
                        mod_es = clim_cred_es,sp_stats = sp_stats, nonfocal = proj_nonf, bratio = bratio_df,
                        ccf_df = ccf_df, CR_fvs_df = CR_fvs_df,cur_clim = proj_clim, fut_clim = mpm_rcp26)
save(red_mpm26, file = "./data/formatted/projection/red clim/red_mpm26.Rdata")
rm(red_mpm26,mpm_rcp26)

load("./data/formatted/fut_clim/mpm_rcp85.Rdata")
red_mpm85 <- proj_fclim(data = proj_dset, tpa_df=proj_tpa_foc, mod_df = clim_red_df, mod_pp = clim_red_pp, 
                        mod_es = clim_cred_es, sp_stats = sp_stats, nonfocal = proj_nonf, bratio = bratio_df,
                        ccf_df = ccf_df,CR_fvs_df = CR_fvs_df, cur_clim = proj_clim, fut_clim = mpm_rcp85)
save(red_mpm85, file = "./data/formatted/projection/red clim/red_mpm85.Rdata")
rm(red_mpm85,mpm_rcp85)

load("./data/formatted/fut_clim/mrc_rcp26.Rdata")
red_mrc26 <- proj_fclim(data = proj_dset, tpa_df=proj_tpa_foc, mod_df = clim_red_df, mod_pp = clim_red_pp, 
                        mod_es = clim_cred_es,sp_stats = sp_stats, nonfocal = proj_nonf, bratio = bratio_df,
                        ccf_df = ccf_df, CR_fvs_df = CR_fvs_df,cur_clim = proj_clim, fut_clim = mrc_rcp26)
save(red_mrc26, file = "./data/formatted/projection/red clim/red_mrc26.Rdata")
rm(red_mrc26,mrc_rcp26)

load("./data/formatted/fut_clim/mrc_rcp85.Rdata")
red_mrc85 <- proj_fclim(data = proj_dset, tpa_df=proj_tpa_foc, mod_df = clim_red_df, mod_pp = clim_red_pp, 
                        mod_es = clim_cred_es, sp_stats = sp_stats, nonfocal = proj_nonf, bratio = bratio_df,
                        ccf_df = ccf_df,CR_fvs_df = CR_fvs_df, cur_clim = proj_clim, fut_clim = mrc_rcp85)
save(red_mrc85, file = "./data/formatted/projection/red clim/red_mrc85.Rdata")
rm(red_mrc85,mrc_rcp85)

load("./data/formatted/fut_clim/mre_rcp26.Rdata")
red_mre26 <- proj_fclim(data = proj_dset, tpa_df=proj_tpa_foc, mod_df = clim_red_df, mod_pp = clim_red_pp, 
                        mod_es = clim_cred_es,sp_stats = sp_stats, nonfocal = proj_nonf, bratio = bratio_df,
                        ccf_df = ccf_df, CR_fvs_df = CR_fvs_df,cur_clim = proj_clim, fut_clim = mre_rcp26)
save(red_mre26, file = "./data/formatted/projection/red clim/red_mre26.Rdata")
rm(red_mre26,mre_rcp26)

load("./data/formatted/fut_clim/mre_rcp85.Rdata")
red_mre85 <- proj_fclim(data = proj_dset, tpa_df=proj_tpa_foc, mod_df = clim_red_df, mod_pp = clim_red_pp, 
                        mod_es = clim_cred_es, sp_stats = sp_stats, nonfocal = proj_nonf, bratio = bratio_df,
                        ccf_df = ccf_df,CR_fvs_df = CR_fvs_df, cur_clim = proj_clim, fut_clim = mre_rcp85)
save(red_mre85, file = "./data/formatted/projection/red clim/red_mre85.Rdata")
rm(red_mre85,mre_rcp85)

load("./data/formatted/fut_clim/mri_rcp26.Rdata")
red_mri26 <- proj_fclim(data = proj_dset, tpa_df=proj_tpa_foc, mod_df = clim_red_df, mod_pp = clim_red_pp, 
                        mod_es = clim_cred_es,sp_stats = sp_stats, nonfocal = proj_nonf, bratio = bratio_df,
                        ccf_df = ccf_df, CR_fvs_df = CR_fvs_df,cur_clim = proj_clim, fut_clim = mri_rcp26)
save(red_mri26, file = "./data/formatted/projection/red clim/red_mri26.Rdata")
rm(red_mri26,mri_rcp26)

load("./data/formatted/fut_clim/mri_rcp85.Rdata")
red_mri85 <- proj_fclim(data = proj_dset, tpa_df=proj_tpa_foc, mod_df = clim_red_df, mod_pp = clim_red_pp, 
                        mod_es = clim_cred_es, sp_stats = sp_stats, nonfocal = proj_nonf, bratio = bratio_df,
                        ccf_df = ccf_df,CR_fvs_df = CR_fvs_df, cur_clim = proj_clim, fut_clim = mri_rcp85)
save(red_mri85, file = "./data/formatted/projection/red clim/red_mri85.Rdata")
rm(red_mri85,mri_rcp85)

load("./data/formatted/fut_clim/nor_rcp26.Rdata")
red_nor26 <- proj_fclim(data = proj_dset, tpa_df=proj_tpa_foc, mod_df = clim_red_df, mod_pp = clim_red_pp, 
                        mod_es = clim_cred_es,sp_stats = sp_stats, nonfocal = proj_nonf, bratio = bratio_df,
                        ccf_df = ccf_df, CR_fvs_df = CR_fvs_df,cur_clim = proj_clim, fut_clim = nor_rcp26)
save(red_nor26, file = "./data/formatted/projection/red clim/red_nor26.Rdata")
rm(red_nor26,mrc_nor26)

load("./data/formatted/fut_clim/nor_rcp85.Rdata")
red_nor85 <- proj_fclim(data = proj_dset, tpa_df=proj_tpa_foc, mod_df = clim_red_df, mod_pp = clim_red_pp, 
                        mod_es = clim_cred_es, sp_stats = sp_stats, nonfocal = proj_nonf, bratio = bratio_df,
                        ccf_df = ccf_df,CR_fvs_df = CR_fvs_df, cur_clim = proj_clim, fut_clim = nor_rcp85)
save(red_nor85, file = "./data/formatted/projection/red clim/red_nor85.Rdata")
rm(red_nor85,nor_rcp85)


#normals
load("./data/formatted/fut_clim/acc_rcp85.Rdata")
n_acc85 <- proj_fclim(data = proj_dset, tpa_df=proj_tpa_foc, mod_df = clim_n_df, mod_pp = clim_n_pp, 
                      mod_es = clim_n_es, sp_stats = sp_stats, nonfocal = proj_nonf, bratio = bratio_df,
                      ccf_df = ccf_df,CR_fvs_df = CR_fvs_df, cur_clim = proj_clim, fut_clim = acc_rcp85)
save(n_acc85, file = "./data/formatted/projection/normals/n_acc85.Rdata")
rm(acc_rcp85,n_acc85)

load("./data/formatted/fut_clim/bcc_rcp26.Rdata")
n_bcc26 <- proj_fclim(data = proj_dset, tpa_df=proj_tpa_foc, mod_df = clim_n_df, mod_pp = clim_n_pp, 
                      mod_es = clim_n_es, sp_stats = sp_stats, nonfocal = proj_nonf, bratio = bratio_df,
                        ccf_df = ccf_df,CR_fvs_df = CR_fvs_df, cur_clim = proj_clim, fut_clim = bcc_rcp26)
save(n_bcc26, file = "./data/formatted/projection/normals/n_bcc26.Rdata")
rm(bcc_rcp26,n_bcc26)

load("./data/formatted/fut_clim/bcc_rcp85.Rdata")
n_bcc85 <- proj_fclim(data = proj_dset, tpa_df=proj_tpa_foc, mod_df = clim_n_df, mod_pp = clim_n_pp, 
                      mod_es = clim_n_es, sp_stats = sp_stats, nonfocal = proj_nonf, bratio = bratio_df,
                        ccf_df = ccf_df,CR_fvs_df = CR_fvs_df, cur_clim = proj_clim, fut_clim = bcc_rcp85)
save(n_bcc85, file = "./data/formatted/projection/normals/n_bcc85.Rdata")
rm(bcc_rcp85,n_bcc85)

load("./data/formatted/fut_clim/bccm_rcp85.Rdata")
n_bccm85 <- proj_fclim(data = proj_dset, tpa_df=proj_tpa_foc, mod_df = clim_n_df, mod_pp = clim_n_pp, 
                       mod_es = clim_n_es, sp_stats = sp_stats, nonfocal = proj_nonf, bratio = bratio_df,
                         ccf_df = ccf_df,CR_fvs_df = CR_fvs_df, cur_clim = proj_clim, fut_clim = bccm_rcp85)
save(n_bccm85, file = "./data/formatted/projection/normals/n_bccm85.Rdata")
rm(bccm_rcp85,n_bccm85)

load("./data/formatted/fut_clim/can_rcp26.Rdata")
n_can26 <- proj_fclim(data = proj_dset, tpa_df=proj_tpa_foc, mod_df = clim_n_df, mod_pp = clim_n_pp, 
                      mod_es = clim_n_es, sp_stats = sp_stats, nonfocal = proj_nonf, bratio = bratio_df,
                        ccf_df = ccf_df,CR_fvs_df = CR_fvs_df,cur_clim = proj_clim, fut_clim = can_rcp26)
save(n_can26, file = "./data/formatted/projection/normals/n_can26.Rdata")
rm(can_rcp26,n_can26)

load("./data/formatted/fut_clim/can_rcp85.Rdata")
n_can85 <- proj_fclim(data = proj_dset, tpa_df=proj_tpa_foc, mod_df = clim_n_df, mod_pp = clim_n_pp, 
                      mod_es = clim_n_es, sp_stats = sp_stats, nonfocal = proj_nonf, bratio = bratio_df,
                      ccf_df = ccf_df,CR_fvs_df = CR_fvs_df, cur_clim = proj_clim, fut_clim = can_rcp85)
save(n_can85, file = "./data/formatted/projection/normals/n_can85.Rdata")
rm(can_rcp85,n_can85)

load("./data/formatted/fut_clim/ccs_rcp26.Rdata")
n_ccs26 <- proj_fclim(data = proj_dset, tpa_df=proj_tpa_foc, mod_df = clim_n_df, mod_pp = clim_n_pp, 
                      mod_es = clim_n_es, sp_stats = sp_stats, nonfocal = proj_nonf, bratio = bratio_df,
                        ccf_df = ccf_df, CR_fvs_df = CR_fvs_df,cur_clim = proj_clim, fut_clim = ccs_rcp26)
save(n_ccs26, file = "./data/formatted/projection/normals/n_ccs26.Rdata")
rm(ccs_rcp26,n_ccs26)

load("./data/formatted/fut_clim/ccs_rcp85.Rdata")
n_ccs85 <- proj_fclim(data = proj_dset, tpa_df=proj_tpa_foc, mod_df = clim_n_df, mod_pp = clim_n_pp, 
                      mod_es = clim_n_es, sp_stats = sp_stats, nonfocal = proj_nonf, bratio = bratio_df,
                        ccf_df = ccf_df, CR_fvs_df = CR_fvs_df, cur_clim = proj_clim, fut_clim = ccs_rcp85)
save(n_ccs85, file = "./data/formatted/projection/normals/n_ccs85.Rdata")
rm(n_ccs85,ccs_rcp85)

load("./data/formatted/fut_clim/ces_rcp26.Rdata")
n_ces26 <- proj_fclim(data = proj_dset, tpa_df=proj_tpa_foc, mod_df = clim_n_df, mod_pp = clim_n_pp, 
                      mod_es = clim_n_es, sp_stats = sp_stats, nonfocal = proj_nonf, bratio = bratio_df,
                        ccf_df = ccf_df, CR_fvs_df = CR_fvs_df, cur_clim = proj_clim, fut_clim = ces_rcp26)
save(n_ces26, file = "./data/formatted/projection/normals/n_ces26.Rdata")
rm(n_ces26,ces_rcp26)

load("./data/formatted/fut_clim/ces_rcp85.Rdata")
n_ces85 <- proj_fclim(data = proj_dset, tpa_df=proj_tpa_foc, mod_df = clim_n_df, mod_pp = clim_n_pp, 
                      mod_es = clim_n_es, sp_stats = sp_stats, nonfocal = proj_nonf, bratio = bratio_df,
                        ccf_df = ccf_df, CR_fvs_df = CR_fvs_df, cur_clim = proj_clim, fut_clim = ces_rcp85)
save(n_ces85, file = "./data/formatted/projection/normals/n_ces85.Rdata")
rm(n_ces85,ces_rcp85)

load("./data/formatted/fut_clim/cesb_rcp85.Rdata")
n_cesb85 <- proj_fclim(data = proj_dset, tpa_df=proj_tpa_foc, mod_df = clim_n_df, mod_pp = clim_n_pp, 
                       mod_es = clim_n_es, sp_stats = sp_stats, nonfocal = proj_nonf, bratio = bratio_df,
                      ccf_df = ccf_df, CR_fvs_df = CR_fvs_df, cur_clim = proj_clim, fut_clim = cesb_rcp85)
save(n_cesb85, file = "./data/formatted/projection/normals/n_cesb85.Rdata")
rm(n_cesb85,cesb_rcp85)

load("./data/formatted/fut_clim/cmc_rcp85.Rdata")
n_cmc85 <- proj_fclim(data = proj_dset, tpa_df=proj_tpa_foc, mod_df = clim_n_df, mod_pp = clim_n_pp, 
                      mod_es = clim_n_es, sp_stats = sp_stats, nonfocal = proj_nonf, bratio = bratio_df,
                       ccf_df = ccf_df, CR_fvs_df = CR_fvs_df, cur_clim = proj_clim, fut_clim = cmc_rcp85)
save(n_cmc85, file = "./data/formatted/projection/normals/n_cmc85.Rdata")
rm(n_cmc85,cmc_rcp85)

load("./data/formatted/fut_clim/cnm_rcp85.Rdata")
n_cnm85 <- proj_fclim(data = proj_dset, tpa_df=proj_tpa_foc, mod_df = clim_n_df, mod_pp = clim_n_pp, 
                      mod_es = clim_n_es, sp_stats = sp_stats, nonfocal = proj_nonf, bratio = bratio_df,
                       ccf_df = ccf_df, CR_fvs_df = CR_fvs_df, cur_clim = proj_clim, fut_clim = cnm_rcp85)
save(n_cnm85, file = "./data/formatted/projection/normals/n_cnm85.Rdata")
rm(n_cnm85,cnm_rcp85)

load("./data/formatted/fut_clim/csr_rcp26.Rdata")
n_csr26 <- proj_fclim(data = proj_dset, tpa_df=proj_tpa_foc, mod_df = clim_n_df, mod_pp = clim_n_pp, 
                      mod_es = clim_n_es, sp_stats = sp_stats, nonfocal = proj_nonf, bratio = bratio_df,
                      ccf_df = ccf_df, CR_fvs_df = CR_fvs_df, cur_clim = proj_clim, fut_clim = csr_rcp26)
save(n_csr26, file = "./data/formatted/projection/normals/n_csr26.Rdata")
rm(n_csr26,csr_rcp26)

load("./data/formatted/fut_clim/csr_rcp85.Rdata")
n_csr85 <- proj_fclim(data = proj_dset, tpa_df=proj_tpa_foc, mod_df = clim_n_df, mod_pp = clim_n_pp, 
                      mod_es = clim_n_es, sp_stats = sp_stats, nonfocal = proj_nonf, bratio = bratio_df,
                      ccf_df = ccf_df, CR_fvs_df = CR_fvs_df, cur_clim = proj_clim, fut_clim = csr_rcp85)
save(n_csr85, file = "./data/formatted/projection/normals/n_csr85.Rdata")
rm(n_csr85,csr_rcp85)

load("./data/formatted/fut_clim/fgl_rcp26.Rdata")
n_fgl26 <- proj_fclim(data = proj_dset, tpa_df=proj_tpa_foc, mod_df = clim_n_df, mod_pp = clim_n_pp, 
                        mod_es = clim_n_es, sp_stats = sp_stats, nonfocal = proj_nonf, bratio = bratio_df,
                        ccf_df = ccf_df, CR_fvs_df = CR_fvs_df,cur_clim = proj_clim, fut_clim = fgl_rcp26)
save(n_fgl26, file = "./data/formatted/projection/normals/n_fgl26.Rdata")
rm(n_fgl26,fgl_rcp26)

load("./data/formatted/fut_clim/fgl_rcp85.Rdata")
n_fgl85 <- proj_fclim(data = proj_dset, tpa_df=proj_tpa_foc, mod_df = clim_n_df, mod_pp = clim_n_pp, 
                        mod_es = clim_n_es, sp_stats = sp_stats, nonfocal = proj_nonf, bratio = bratio_df,
                        ccf_df = ccf_df, CR_fvs_df = CR_fvs_df,cur_clim = proj_clim, fut_clim = fgl_rcp85)
save(n_fgl85, file = "./data/formatted/projection/normals/n_fgl85.Rdata")
rm(n_fgl85,fgl_rcp85)

load("./data/formatted/fut_clim/fio_rcp26.Rdata")
n_fio26 <- proj_fclim(data = proj_dset, tpa_df=proj_tpa_foc, mod_df = clim_n_df, mod_pp = clim_n_pp, 
                        mod_es = clim_n_es, sp_stats = sp_stats, nonfocal = proj_nonf, bratio = bratio_df,
                        ccf_df = ccf_df, CR_fvs_df = CR_fvs_df,cur_clim = proj_clim, fut_clim = fio_rcp26)
save(n_fio26, file = "./data/formatted/projection/normals/n_fio26.Rdata")
rm(fio_rcp26,n_fio26)

load("./data/formatted/fut_clim/fio_rcp85.Rdata")
n_fio85 <- proj_fclim(data = proj_dset, tpa_df=proj_tpa_foc, mod_df = clim_n_df, mod_pp = clim_n_pp, 
                        mod_es = clim_n_es, sp_stats = sp_stats, nonfocal = proj_nonf, bratio = bratio_df,
                        ccf_df = ccf_df, CR_fvs_df = CR_fvs_df,cur_clim = proj_clim, fut_clim = fio_rcp85)
save(n_fio85, file = "./data/formatted/projection/normals/n_fio85.Rdata")
rm(n_fio85,fio_rcp85)

load("./data/formatted/fut_clim/gfc_rcp26.Rdata")
n_gfc26 <- proj_fclim(data = proj_dset, tpa_df=proj_tpa_foc, mod_df = clim_n_df, mod_pp = clim_n_pp, 
                        mod_es = clim_n_es,sp_stats = sp_stats, nonfocal = proj_nonf, bratio = bratio_df,
                        ccf_df = ccf_df, CR_fvs_df = CR_fvs_df,cur_clim = proj_clim, fut_clim = gfc_rcp26)
save(n_gfc26, file = "./data/formatted/projection/normals/n_gfc26.Rdata")
rm(n_gfc26,gfc_rcp26)

load("./data/formatted/fut_clim/gfc_rcp85.Rdata")
n_gfc85 <- proj_fclim(data = proj_dset, tpa_df=proj_tpa_foc, mod_df = clim_n_df, mod_pp = clim_n_pp, 
                        mod_es = clim_n_es, sp_stats = sp_stats, nonfocal = proj_nonf, bratio = bratio_df,
                        ccf_df = ccf_df,CR_fvs_df = CR_fvs_df, cur_clim = proj_clim, fut_clim = gfc_rcp85)
save(n_gfc85, file = "./data/formatted/projection/normals/n_gfc85.Rdata")
rm(n_gfc85,gfc_rcp85)

load("./data/formatted/fut_clim/gfg_rcp26.Rdata")
n_gfg26 <- proj_fclim(data = proj_dset, tpa_df=proj_tpa_foc, mod_df = clim_n_df, mod_pp = clim_n_pp, 
                        mod_es = clim_n_es, sp_stats = sp_stats, nonfocal = proj_nonf, bratio = bratio_df,
                        ccf_df = ccf_df, CR_fvs_df = CR_fvs_df,cur_clim = proj_clim, fut_clim = gfg_rcp26)
save(n_gfg26, file = "./data/formatted/projection/normals/n_gfg26.Rdata")
rm(n_gfg26,gfg_rcp26)

load("./data/formatted/fut_clim/gfg_rcp85.Rdata")
n_gfg85 <- proj_fclim(data = proj_dset, tpa_df=proj_tpa_foc, mod_df = clim_n_df, mod_pp = clim_n_pp, 
                        mod_es = clim_n_es, sp_stats = sp_stats, nonfocal = proj_nonf, bratio = bratio_df,
                        ccf_df = ccf_df,CR_fvs_df = CR_fvs_df, cur_clim = proj_clim, fut_clim = gfg_rcp85)
save(n_gfg85, file = "./data/formatted/projection/normals/n_gfg85.Rdata")
rm(n_gfg85,gfg_rcp85)

load("./data/formatted/fut_clim/gfm_rcp26.Rdata")
n_gfm26 <- proj_fclim(data = proj_dset, tpa_df=proj_tpa_foc, mod_df = clim_n_df, mod_pp = clim_n_pp, 
                        mod_es = clim_n_es, sp_stats = sp_stats, nonfocal = proj_nonf, bratio = bratio_df,
                        ccf_df = ccf_df, CR_fvs_df = CR_fvs_df,cur_clim = proj_clim, fut_clim = gfm_rcp26)
save(n_gfm26, file = "./data/formatted/projection/normals/n_gfm26.Rdata")
rm(n_gfm26,gfm_rcp26)

load("./data/formatted/fut_clim/gfm_rcp85.Rdata")
n_gfm85 <- proj_fclim(data = proj_dset, tpa_df=proj_tpa_foc, mod_df = clim_n_df, mod_pp = clim_n_pp, 
                        mod_es = clim_n_es, sp_stats = sp_stats, nonfocal = proj_nonf, bratio = bratio_df,
                        ccf_df = ccf_df,CR_fvs_df = CR_fvs_df, cur_clim = proj_clim, fut_clim = gfm_rcp85)
save(n_gfm85, file = "./data/formatted/projection/normals/n_gfm85.Rdata")
rm(n_gfm85,gfm_rcp85)

load("./data/formatted/fut_clim/gis_rcp26.Rdata")
n_gis26 <- proj_fclim(data = proj_dset, tpa_df=proj_tpa_foc, mod_df = clim_n_df, mod_pp = clim_n_pp, 
                        mod_es = clim_n_es, sp_stats = sp_stats, nonfocal = proj_nonf, bratio = bratio_df,
                        ccf_df = ccf_df, CR_fvs_df = CR_fvs_df,cur_clim = proj_clim, fut_clim = gis_rcp26)
save(n_gis26, file = "./data/formatted/projection/normals/n_gis26.Rdata")
rm(n_gis26,gis_rcp26)

load("./data/formatted/fut_clim/gis_rcp85.Rdata")
n_gis85 <- proj_fclim(data = proj_dset, tpa_df=proj_tpa_foc, mod_df = clim_n_df, mod_pp = clim_n_pp, 
                        mod_es = clim_n_es, sp_stats = sp_stats, nonfocal = proj_nonf, bratio = bratio_df,
                        ccf_df = ccf_df,CR_fvs_df = CR_fvs_df, cur_clim = proj_clim, fut_clim = gis_rcp85)
save(n_gis85, file = "./data/formatted/projection/normals/n_gis85.Rdata")
rm(n_gis85,gis_rcp85)

load("./data/formatted/fut_clim/had_rcp26.Rdata")
n_had26 <- proj_fclim(data = proj_dset, tpa_df=proj_tpa_foc, mod_df = clim_n_df, mod_pp = clim_n_pp, 
                        mod_es = clim_n_es, sp_stats = sp_stats, nonfocal = proj_nonf, bratio = bratio_df,
                        ccf_df = ccf_df, CR_fvs_df = CR_fvs_df,cur_clim = proj_clim, fut_clim = had_rcp26)
save(n_had26, file = "./data/formatted/projection/normals/n_had26.Rdata")
rm(n_had26,had_rcp26)

load("./data/formatted/fut_clim/had_rcp85.Rdata")
n_had85 <- proj_fclim(data = proj_dset, tpa_df=proj_tpa_foc, mod_df = clim_n_df, mod_pp = clim_n_pp, 
                        mod_es = clim_n_es, sp_stats = sp_stats, nonfocal = proj_nonf, bratio = bratio_df,
                        ccf_df = ccf_df,CR_fvs_df = CR_fvs_df, cur_clim = proj_clim, fut_clim = had_rcp85)
save(n_had85, file = "./data/formatted/projection/normals/n_had85.Rdata")
rm(n_had85,had_rcp85)

load("./data/formatted/fut_clim/hada_rcp26.Rdata")
n_hada26 <- proj_fclim(data = proj_dset, tpa_df=proj_tpa_foc, mod_df = clim_n_df, mod_pp = clim_n_pp, 
                         mod_es = clim_n_es, sp_stats = sp_stats, nonfocal = proj_nonf, bratio = bratio_df,
                         ccf_df = ccf_df, CR_fvs_df = CR_fvs_df,cur_clim = proj_clim, fut_clim = hada_rcp26)
save(n_hada26, file = "./data/formatted/projection/normals/n_hada26.Rdata")
rm(n_hada26,hada_rcp26)

load("./data/formatted/fut_clim/hada_rcp85.Rdata")
n_hada85 <- proj_fclim(data = proj_dset, tpa_df=proj_tpa_foc,mod_df = clim_n_df, mod_pp = clim_n_pp, 
                         mod_es = clim_n_es, sp_stats = sp_stats, nonfocal = proj_nonf, bratio = bratio_df,
                         ccf_df = ccf_df,CR_fvs_df = CR_fvs_df, cur_clim = proj_clim, fut_clim = hada_rcp85)
save(n_hada85, file = "./data/formatted/projection/normals/n_hada85.Rdata")
rm(n_hada85,hada_rcp85)

load("./data/formatted/fut_clim/hadc_rcp85.Rdata")
n_hadc85 <- proj_fclim(data = proj_dset, tpa_df=proj_tpa_foc, mod_df = clim_n_df, mod_pp = clim_n_pp, 
                         mod_es = clim_n_es, sp_stats = sp_stats, nonfocal = proj_nonf, bratio = bratio_df,
                         ccf_df = ccf_df,CR_fvs_df = CR_fvs_df, cur_clim = proj_clim, fut_clim = hadc_rcp85)
save(n_hadc85, file = "./data/formatted/projection/normals/n_hadc85.Rdata")
rm(n_hadc85,hadc_rcp85)

load("./data/formatted/fut_clim/inm_rcp85.Rdata")
n_inm85 <- proj_fclim(data = proj_dset, tpa_df=proj_tpa_foc, mod_df = clim_n_df, mod_pp = clim_n_pp, 
                        mod_es = clim_n_es, sp_stats = sp_stats, nonfocal = proj_nonf, bratio = bratio_df,
                        ccf_df = ccf_df,CR_fvs_df = CR_fvs_df, cur_clim = proj_clim, fut_clim = inm_rcp85)
save(n_inm85, file = "./data/formatted/projection/normals/n_inm85.Rdata")
rm(n_inm85,inm_rcp85)

load("./data/formatted/fut_clim/ipl_rcp85.Rdata")
n_ipl85 <- proj_fclim(data = proj_dset, tpa_df=proj_tpa_foc, mod_df = clim_red_df, mod_pp = clim_red_pp, 
                        mod_es = clim_cred_es, sp_stats = sp_stats, nonfocal = proj_nonf, bratio = bratio_df,
                        ccf_df = ccf_df,CR_fvs_df = CR_fvs_df, cur_clim = proj_clim, fut_clim = ipl_rcp85)
save(n_ipl85, file = "./data/formatted/projection/normals/n_ipl85.Rdata")
rm(n_ipl85,ipl_rcp85)

load("./data/formatted/fut_clim/ipm_rcp26.Rdata")
n_ipm26 <- proj_fclim(data = proj_dset, tpa_df=proj_tpa_foc, mod_df = clim_n_df, mod_pp = clim_n_pp, 
                        mod_es = clim_n_es, sp_stats = sp_stats, nonfocal = proj_nonf, bratio = bratio_df,
                        ccf_df = ccf_df, CR_fvs_df = CR_fvs_df,cur_clim = proj_clim, fut_clim = ipm_rcp26)
save(n_ipm26, file = "./data/formatted/projection/normals/n_ipm26.Rdata")
rm(n_ipm26,ipm_rcp26)

load("./data/formatted/fut_clim/ipm_rcp85.Rdata")
n_ipm85 <- proj_fclim(data = proj_dset, tpa_df=proj_tpa_foc, mod_df = clim_n_df, mod_pp = clim_n_pp, 
                        mod_es = clim_n_es, sp_stats = sp_stats, nonfocal = proj_nonf, bratio = bratio_df,
                        ccf_df = ccf_df,CR_fvs_df = CR_fvs_df, cur_clim = proj_clim, fut_clim = ipm_rcp85)
save(n_ipm85, file = "./data/formatted/projection/normals/n_ipm85.Rdata")
rm(n_ipm85,ipm_rcp85)

load("./data/formatted/fut_clim/mir_rcp26.Rdata")
n_mir26 <- proj_fclim(data = proj_dset, tpa_df=proj_tpa_foc, mod_df = clim_n_df, mod_pp = clim_n_pp, 
                        mod_es = clim_n_es, sp_stats = sp_stats, nonfocal = proj_nonf, bratio = bratio_df,
                        ccf_df = ccf_df, CR_fvs_df = CR_fvs_df,cur_clim = proj_clim, fut_clim = mir_rcp26)
save(n_mir26, file = "./data/formatted/projection/normals/n_mir26.Rdata")
rm(n_mir26,mir_rcp26)

load("./data/formatted/fut_clim/mir_rcp85.Rdata")
n_mir85 <- proj_fclim(data = proj_dset, tpa_df=proj_tpa_foc, mod_df = clim_n_df, mod_pp = clim_n_pp, 
                        mod_es = clim_n_es, sp_stats = sp_stats, nonfocal = proj_nonf, bratio = bratio_df,
                        ccf_df = ccf_df,CR_fvs_df = CR_fvs_df, cur_clim = proj_clim, fut_clim = mir_rcp85)
save(n_mir85, file = "./data/formatted/projection/normals/n_mir85.Rdata")
rm(n_mir85,mir_rcp85)

load("./data/formatted/fut_clim/mpl_rcp26.Rdata")
n_mpl26 <- proj_fclim(data = proj_dset, tpa_df=proj_tpa_foc, mod_df = clim_n_df, mod_pp = clim_n_pp, 
                        mod_es = clim_n_es, sp_stats = sp_stats, nonfocal = proj_nonf, bratio = bratio_df,
                        ccf_df = ccf_df, CR_fvs_df = CR_fvs_df,cur_clim = proj_clim, fut_clim = mpl_rcp26)
save(n_mpl26, file = "./data/formatted/projection/normals/n_mpl26.Rdata")
rm(n_mpl26,mpl_rcp26)

load("./data/formatted/fut_clim/mpl_rcp85.Rdata")
n_mpl85 <- proj_fclim(data = proj_dset, tpa_df=proj_tpa_foc, mod_df = clim_n_df, mod_pp = clim_n_pp, 
                        mod_es = clim_n_es, sp_stats = sp_stats, nonfocal = proj_nonf, bratio = bratio_df,
                        ccf_df = ccf_df,CR_fvs_df = CR_fvs_df, cur_clim = proj_clim, fut_clim = mpl_rcp85)
save(n_mpl85, file = "./data/formatted/projection/normals/n_mpl85.Rdata")
rm(n_mpl85,mpl_rcp85)

load("./data/formatted/fut_clim/mpm_rcp26.Rdata")
n_mpm26 <- proj_fclim(data = proj_dset, tpa_df=proj_tpa_foc, mod_df = clim_n_df, mod_pp = clim_n_pp, 
                        mod_es = clim_n_es, sp_stats = sp_stats, nonfocal = proj_nonf, bratio = bratio_df,
                        ccf_df = ccf_df, CR_fvs_df = CR_fvs_df,cur_clim = proj_clim, fut_clim = mpm_rcp26)
save(n_mpm26, file = "./data/formatted/projection/normals/n_mpm26.Rdata")
rm(n_mpm26,mpm_rcp26)

load("./data/formatted/fut_clim/mpm_rcp85.Rdata")
n_mpm85 <- proj_fclim(data = proj_dset, tpa_df=proj_tpa_foc, mod_df = clim_n_df, mod_pp = clim_n_pp, 
                        mod_es = clim_n_es, sp_stats = sp_stats, nonfocal = proj_nonf, bratio = bratio_df,
                        ccf_df = ccf_df,CR_fvs_df = CR_fvs_df, cur_clim = proj_clim, fut_clim = mpm_rcp85)
save(n_mpm85, file = "./data/formatted/projection/normals/n_mpm85.Rdata")
rm(n_mpm85,mpm_rcp85)

load("./data/formatted/fut_clim/mrc_rcp26.Rdata")
n_mrc26 <- proj_fclim(data = proj_dset, tpa_df=proj_tpa_foc, mod_df = clim_n_df, mod_pp = clim_n_pp, 
                        mod_es = clim_n_es, sp_stats = sp_stats, nonfocal = proj_nonf, bratio = bratio_df,
                        ccf_df = ccf_df, CR_fvs_df = CR_fvs_df,cur_clim = proj_clim, fut_clim = mrc_rcp26)
save(n_mrc26, file = "./data/formatted/projection/normals/n_mrc26.Rdata")
rm(n_mrc26,mrc_rcp26)

load("./data/formatted/fut_clim/mrc_rcp85.Rdata")
n_mrc85 <- proj_fclim(data = proj_dset, tpa_df=proj_tpa_foc, mod_df = clim_n_df, mod_pp = clim_n_pp, 
                        mod_es = clim_n_es, sp_stats = sp_stats, nonfocal = proj_nonf, bratio = bratio_df,
                        ccf_df = ccf_df,CR_fvs_df = CR_fvs_df, cur_clim = proj_clim, fut_clim = mrc_rcp85)
save(n_mrc85, file = "./data/formatted/projection/normals/n_mrc85.Rdata")
rm(n_mrc85,mrc_rcp85)

load("./data/formatted/fut_clim/mre_rcp26.Rdata")
n_mre26 <- proj_fclim(data = proj_dset, tpa_df=proj_tpa_foc, mod_df = clim_n_df, mod_pp = clim_n_pp, 
                        mod_es = clim_n_es, sp_stats = sp_stats, nonfocal = proj_nonf, bratio = bratio_df,
                        ccf_df = ccf_df, CR_fvs_df = CR_fvs_df,cur_clim = proj_clim, fut_clim = mre_rcp26)
save(n_mre26, file = "./data/formatted/projection/normals/n_mre26.Rdata")
rm(n_mre26,mre_rcp26)

load("./data/formatted/fut_clim/mre_rcp85.Rdata")
n_mre85 <- proj_fclim(data = proj_dset, tpa_df=proj_tpa_foc, mod_df = clim_n_df, mod_pp = clim_n_pp, 
                        mod_es = clim_n_es, sp_stats = sp_stats, nonfocal = proj_nonf, bratio = bratio_df,
                        ccf_df = ccf_df,CR_fvs_df = CR_fvs_df, cur_clim = proj_clim, fut_clim = mre_rcp85)
save(n_mre85, file = "./data/formatted/projection/normals/n_mre85.Rdata")
rm(n_mre85,mre_rcp85)

load("./data/formatted/fut_clim/mri_rcp26.Rdata")
n_mri26 <- proj_fclim(data = proj_dset, tpa_df=proj_tpa_foc, mod_df = clim_n_df, mod_pp = clim_n_pp, 
                        mod_es = clim_n_es, sp_stats = sp_stats, nonfocal = proj_nonf, bratio = bratio_df,
                        ccf_df = ccf_df, CR_fvs_df = CR_fvs_df,cur_clim = proj_clim, fut_clim = mri_rcp26)
save(n_mri26, file = "./data/formatted/projection/normals/n_mri26.Rdata")
rm(n_mri26,mri_rcp26)

load("./data/formatted/fut_clim/mri_rcp85.Rdata")
n_mri85 <- proj_fclim(data = proj_dset, tpa_df=proj_tpa_foc, mod_df = clim_n_df, mod_pp = clim_n_pp, 
                        mod_es = clim_n_es, sp_stats = sp_stats, nonfocal = proj_nonf, bratio = bratio_df,
                        ccf_df = ccf_df,CR_fvs_df = CR_fvs_df, cur_clim = proj_clim, fut_clim = mri_rcp85)
save(n_mri85, file = "./data/formatted/projection/normals/n_mri85.Rdata")
rm(n_mri85,mri_rcp85)

load("./data/formatted/fut_clim/nor_rcp26.Rdata")
n_nor26 <- proj_fclim(data = proj_dset, tpa_df=proj_tpa_foc, mod_df = clim_n_df, mod_pp = clim_n_pp, 
                        mod_es = clim_n_es, sp_stats = sp_stats, nonfocal = proj_nonf, bratio = bratio_df,
                        ccf_df = ccf_df, CR_fvs_df = CR_fvs_df,cur_clim = proj_clim, fut_clim = nor_rcp26)
save(n_nor26, file = "./data/formatted/projection/normals/n_nor26.Rdata")
rm(n_nor26,mrc_nor26)

load("./data/formatted/fut_clim/nor_rcp85.Rdata")
n_nor85 <- proj_fclim(data = proj_dset, tpa_df=proj_tpa_foc, mod_df = clim_n_df, mod_pp = clim_n_pp, 
                        mod_es = clim_n_es, sp_stats = sp_stats, nonfocal = proj_nonf, bratio = bratio_df,
                        ccf_df = ccf_df,CR_fvs_df = CR_fvs_df, cur_clim = proj_clim, fut_clim = nor_rcp85)
save(n_nor85, file = "./data/formatted/projection/normals/n_nor85.Rdata")
rm(n_nor85,nor_rcp85)



#normals + interaction
proj_tpa_foc <- proj_tpa_foc %>% filter(Year <= 2060)

load("./data/formatted/fut_clim/acc_rcp85.Rdata")
nint_acc85 <- proj_fclim(data = proj_dset, tpa_df=proj_tpa_foc, mod_df = clim_nint_df, mod_pp = clim_nint_pp, 
                        mod_es = clim_nint_es, sp_stats = sp_stats, nonfocal = proj_nonf, bratio = bratio_df,
                        ccf_df = ccf_df, CR_fvs_df = CR_fvs_df, cur_clim = proj_clim, fut_clim = acc_rcp85)
save(nint_acc85, file = "./data/formatted/projection/nint_acc85.Rdata")
rm(nint_acc85,acc_rcp85)

load("./data/formatted/fut_clim/bcc_rcp26.Rdata")
nint_bcc26 <- proj_fclim(data = proj_dset, tpa_df=proj_tpa_foc, mod_df = clim_nint_df, mod_pp = clim_nint_pp, 
                         mod_es = clim_nint_es, sp_stats = sp_stats, nonfocal = proj_nonf, bratio = bratio_df,
                        ccf_df = ccf_df, CR_fvs_df = CR_fvs_df, cur_clim = proj_clim, fut_clim = bcc_rcp26)
save(nint_bcc26, file = "./data/formatted/projection/nint_bcc26.Rdata")
rm(nint_bcc26,bcc_rcp26)

load("./data/formatted/fut_clim/bcc_rcp85.Rdata")
nint_bcc85 <- proj_fclim(data = proj_dset, tpa_df=proj_tpa_foc, mod_df = clim_nint_df, mod_pp = clim_nint_pp, 
                         mod_es = clim_nint_es, sp_stats = sp_stats, nonfocal = proj_nonf, bratio = bratio_df,
                        ccf_df = ccf_df, CR_fvs_df = CR_fvs_df, cur_clim = proj_clim, fut_clim = bcc_rcp85)
save(nint_bcc85, file = "./data/formatted/projection/nint_bcc85.Rdata")
rm(nint_bcc85,bcc_rcp85)

load("./data/formatted/fut_clim/bccm_rcp85.Rdata")
nint_bccm85 <- proj_fclim(data = proj_dset, tpa_df=proj_tpa_foc, mod_df = clim_nint_df, mod_pp = clim_nint_pp, 
                       mod_es = clim_nint_es, sp_stats = sp_stats, nonfocal = proj_nonf, bratio = bratio_df,
                       ccf_df = ccf_df,CR_fvs_df = CR_fvs_df, cur_clim = proj_clim, fut_clim = bccm_rcp85)
save(nint_bccm85, file = "./data/formatted/projection/normals/nint_bccm85.Rdata")
rm(bccm_rcp85,nint_bccm85)

load("./data/formatted/fut_clim/can_rcp26.Rdata")
nint_can26 <- proj_fclim(data = proj_dset, tpa_df=proj_tpa_foc, mod_df = clim_nint_df, mod_pp = clim_nint_pp, 
                      mod_es = clim_nint_es, sp_stats = sp_stats, nonfocal = proj_nonf, bratio = bratio_df,
                      ccf_df = ccf_df,CR_fvs_df = CR_fvs_df,cur_clim = proj_clim, fut_clim = can_rcp26)
save(nint_can26, file = "./data/formatted/projection/normals/nint_can26.Rdata")
rm(can_rcp26,nint_can26)

load("./data/formatted/fut_clim/can_rcp85.Rdata")
nint_can85 <- proj_fclim(data = proj_dset, tpa_df=proj_tpa_foc, mod_df = clim_nint_df, mod_pp = clim_nint_pp, 
                      mod_es = clim_nint_es, sp_stats = sp_stats, nonfocal = proj_nonf, bratio = bratio_df,
                      ccf_df = ccf_df,CR_fvs_df = CR_fvs_df, cur_clim = proj_clim, fut_clim = can_rcp85)
save(nint_can85, file = "./data/formatted/projection/normals/nint_can85.Rdata")
rm(can_rcp85,nint_can85)

load("./data/formatted/fut_clim/ccs_rcp26.Rdata")
nint_ccs26 <- proj_fclim(data = proj_dset, tpa_df=proj_tpa_foc, mod_df = clim_nint_df, mod_pp = clim_nint_pp, 
                      mod_es = clim_nint_es, sp_stats = sp_stats, nonfocal = proj_nonf, bratio = bratio_df,
                      ccf_df = ccf_df, CR_fvs_df = CR_fvs_df,cur_clim = proj_clim, fut_clim = ccs_rcp26)
save(nint_ccs26, file = "./data/formatted/projection/normals/nint_ccs26.Rdata")
rm(ccs_rcp26,nint_ccs26)

load("./data/formatted/fut_clim/ccs_rcp85.Rdata")
nint_ccs85 <- proj_fclim(data = proj_dset, tpa_df=proj_tpa_foc, mod_df = clim_nint_df, mod_pp = clim_nint_pp, 
                      mod_es = clim_nint_es, sp_stats = sp_stats, nonfocal = proj_nonf, bratio = bratio_df,
                      ccf_df = ccf_df, CR_fvs_df = CR_fvs_df, cur_clim = proj_clim, fut_clim = ccs_rcp85)
save(nint_ccs85, file = "./data/formatted/projection/normals/nint_ccs85.Rdata")
rm(nint_ccs85,ccs_rcp85)

load("./data/formatted/fut_clim/ces_rcp26.Rdata")
nint_ces26 <- proj_fclim(data = proj_dset, tpa_df=proj_tpa_foc, mod_df = clim_nint_df, mod_pp = clim_nint_pp, 
                      mod_es = clim_nint_es, sp_stats = sp_stats, nonfocal = proj_nonf, bratio = bratio_df,
                      ccf_df = ccf_df, CR_fvs_df = CR_fvs_df, cur_clim = proj_clim, fut_clim = ces_rcp26)
save(nint_ces26, file = "./data/formatted/projection/normals/nint_ces26.Rdata")
rm(nint_ces26,ces_rcp26)

load("./data/formatted/fut_clim/ces_rcp85.Rdata")
nint_ces85 <- proj_fclim(data = proj_dset, tpa_df=proj_tpa_foc, mod_df = clim_nint_df, mod_pp = clim_nint_pp, 
                      mod_es = clim_nint_es, sp_stats = sp_stats, nonfocal = proj_nonf, bratio = bratio_df,
                      ccf_df = ccf_df, CR_fvs_df = CR_fvs_df, cur_clim = proj_clim, fut_clim = ces_rcp85)
save(nint_ces85, file = "./data/formatted/projection/normals/nint_ces85.Rdata")
rm(nint_ces85,ces_rcp85)

load("./data/formatted/fut_clim/cesb_rcp85.Rdata")
nint_cesb85 <- proj_fclim(data = proj_dset, tpa_df=proj_tpa_foc, mod_df = clim_nint_df, mod_pp = clim_nint_pp, 
                       mod_es = clim_nint_es, sp_stats = sp_stats, nonfocal = proj_nonf, bratio = bratio_df,
                       ccf_df = ccf_df, CR_fvs_df = CR_fvs_df, cur_clim = proj_clim, fut_clim = cesb_rcp85)
save(nint_cesb85, file = "./data/formatted/projection/normals/nint_cesb85.Rdata")
rm(nint_cesb85,cesb_rcp85)

load("./data/formatted/fut_clim/cmc_rcp85.Rdata")
nint_cmc85 <- proj_fclim(data = proj_dset, tpa_df=proj_tpa_foc,mod_df = clim_nint_df, mod_pp = clim_nint_pp, 
                      mod_es = clim_nint_es, sp_stats = sp_stats, nonfocal = proj_nonf, bratio = bratio_df,
                      ccf_df = ccf_df, CR_fvs_df = CR_fvs_df, cur_clim = proj_clim, fut_clim = cmc_rcp85)
save(nint_cmc85, file = "./data/formatted/projection/normals/nint_cmcb85.Rdata")
rm(nint_cmc85,cmc_rcp85)

load("./data/formatted/fut_clim/cnm_rcp85.Rdata")
nint_cnm85 <- proj_fclim(data = proj_dset, tpa_df=proj_tpa_foc,mod_df = clim_nint_df, mod_pp = clim_nint_pp, 
                      mod_es = clim_nint_es, sp_stats = sp_stats, nonfocal = proj_nonf, bratio = bratio_df,
                      ccf_df = ccf_df, CR_fvs_df = CR_fvs_df, cur_clim = proj_clim, fut_clim = cnm_rcp85)
save(nint_cnm85, file = "./data/formatted/projection/normals/nint_cnm85.Rdata")
rm(nint_cnm85,cnm_rcp85)

load("./data/formatted/fut_clim/csr_rcp26.Rdata")
nint_csr26 <- proj_fclim(data = proj_dset, tpa_df=proj_tpa_foc, mod_df = clim_nint_df, mod_pp = clim_nint_pp, 
                      mod_es = clim_nint_es, sp_stats = sp_stats, nonfocal = proj_nonf, bratio = bratio_df,
                      ccf_df = ccf_df, CR_fvs_df = CR_fvs_df, cur_clim = proj_clim, fut_clim = csr_rcp26)
save(nint_csr26, file = "./data/formatted/projection/normals/nint_csr26.Rdata")
rm(nint_csr26,csr_rcp26)

load("./data/formatted/fut_clim/csr_rcp85.Rdata")
nint_csr85 <- proj_fclim(data = proj_dset, tpa_df=proj_tpa_foc,mod_df = clim_nint_df, mod_pp = clim_nint_pp, 
                      mod_es = clim_nint_es, sp_stats = sp_stats, nonfocal = proj_nonf, bratio = bratio_df,
                      ccf_df = ccf_df, CR_fvs_df = CR_fvs_df, cur_clim = proj_clim, fut_clim = csr_rcp85)
save(nint_csr85, file = "./data/formatted/projection/normals/nint_csr85.Rdata")
rm(nint_csr85,csr_rcp85)

load("./data/formatted/fut_clim/fgl_rcp26.Rdata")
nint_fgl26 <- proj_fclim(data = proj_dset, tpa_df=proj_tpa_foc,mod_df = clim_nint_df, mod_pp = clim_nint_pp, 
                      mod_es = clim_nint_es, sp_stats = sp_stats, nonfocal = proj_nonf, bratio = bratio_df,
                      ccf_df = ccf_df, CR_fvs_df = CR_fvs_df,cur_clim = proj_clim, fut_clim = fgl_rcp26)
save(nint_fgl26, file = "./data/formatted/projection/normals/nint_fgl26.Rdata")
rm(nint_fgl26,fgl_rcp26)

load("./data/formatted/fut_clim/fgl_rcp85.Rdata")
nint_fgl85 <- proj_fclim(data = proj_dset, tpa_df=proj_tpa_foc, mod_df = clim_nint_df, mod_pp = clim_nint_pp, 
                      mod_es = clim_nint_es, sp_stats = sp_stats, nonfocal = proj_nonf, bratio = bratio_df,
                      ccf_df = ccf_df, CR_fvs_df = CR_fvs_df,cur_clim = proj_clim, fut_clim = fgl_rcp85)
save(nint_fgl85, file = "./data/formatted/projection/normals/nint_fgl85.Rdata")
rm(nint_fgl85,fgl_rcp85)

load("./data/formatted/fut_clim/fio_rcp26.Rdata")
nint_fio26 <- proj_fclim(data = proj_dset, tpa_df=proj_tpa_foc, mod_df = clim_nint_df, mod_pp = clim_nint_pp, 
                      mod_es = clim_nint_es, sp_stats = sp_stats, nonfocal = proj_nonf, bratio = bratio_df,
                      ccf_df = ccf_df, CR_fvs_df = CR_fvs_df,cur_clim = proj_clim, fut_clim = fio_rcp26)
save(nint_fio26, file = "./data/formatted/projection/normals/nint_fio26.Rdata")
rm(fio_rcp26,nint_fio26)

load("./data/formatted/fut_clim/fio_rcp85.Rdata")
nint_fio85 <- proj_fclim(data = proj_dset, tpa_df=proj_tpa_foc, mod_df = clim_nint_df, mod_pp = clim_nint_pp, 
                      mod_es = clim_nint_es, sp_stats = sp_stats, nonfocal = proj_nonf, bratio = bratio_df,
                      ccf_df = ccf_df, CR_fvs_df = CR_fvs_df,cur_clim = proj_clim, fut_clim = fio_rcp85)
save(nint_fio85, file = "./data/formatted/projection/normals/nint_fio85.Rdata")
rm(nint_fio85,fio_rcp85)

load("./data/formatted/fut_clim/gfc_rcp26.Rdata")
nint_gfc26 <- proj_fclim(data = proj_dset, tpa_df=proj_tpa_foc, mod_df = clim_nint_df, mod_pp = clim_nint_pp, 
                      mod_es = clim_nint_es, sp_stats = sp_stats, nonfocal = proj_nonf, bratio = bratio_df,
                      ccf_df = ccf_df, CR_fvs_df = CR_fvs_df,cur_clim = proj_clim, fut_clim = gfc_rcp26)
save(nint_gfc26, file = "./data/formatted/projection/normals/nint_gfc26.Rdata")
rm(nint_gfc26,gfc_rcp26)

load("./data/formatted/fut_clim/gfc_rcp85.Rdata")
nint_gfc85 <- proj_fclim(data = proj_dset, tpa_df=proj_tpa_foc, mod_df = clim_nint_df, mod_pp = clim_nint_pp, 
                      mod_es = clim_nint_es, sp_stats = sp_stats, nonfocal = proj_nonf, bratio = bratio_df,
                      ccf_df = ccf_df,CR_fvs_df = CR_fvs_df, cur_clim = proj_clim, fut_clim = gfc_rcp85)
save(nint_gfc85, file = "./data/formatted/projection/normals/nint_gfc85.Rdata")
rm(nint_gfc85,gfc_rcp85)

load("./data/formatted/fut_clim/gfg_rcp26.Rdata")
nint_gfg26 <- proj_fclim(data = proj_dset, tpa_df=proj_tpa_foc, mod_df = clim_nint_df, mod_pp = clim_nint_pp, 
                      mod_es = clim_nint_es, sp_stats = sp_stats, nonfocal = proj_nonf, bratio = bratio_df,
                      ccf_df = ccf_df, CR_fvs_df = CR_fvs_df,cur_clim = proj_clim, fut_clim = gfg_rcp26)
save(nint_gfg26, file = "./data/formatted/projection/normals/nint_gfg26.Rdata")
rm(nint_gfg26,gfg_rcp26)

load("./data/formatted/fut_clim/gfg_rcp85.Rdata")
nint_gfg85 <- proj_fclim(data = proj_dset, tpa_df=proj_tpa_foc,mod_df = clim_nint_df, mod_pp = clim_nint_pp, 
                      mod_es = clim_nint_es, sp_stats = sp_stats, nonfocal = proj_nonf, bratio = bratio_df,
                      ccf_df = ccf_df,CR_fvs_df = CR_fvs_df, cur_clim = proj_clim, fut_clim = gfg_rcp85)
save(nint_gfg85, file = "./data/formatted/projection/normals/nint_gfg85.Rdata")
rm(nint_gfg85,gfg_rcp85)

load("./data/formatted/fut_clim/gfm_rcp26.Rdata")
nint_gfm26 <- proj_fclim(data = proj_dset, tpa_df=proj_tpa_foc, mod_df = clim_nint_df, mod_pp = clim_nint_pp, 
                      mod_es = clim_nint_es, sp_stats = sp_stats, nonfocal = proj_nonf, bratio = bratio_df,
                      ccf_df = ccf_df, CR_fvs_df = CR_fvs_df,cur_clim = proj_clim, fut_clim = gfm_rcp26)
save(nint_gfm26, file = "./data/formatted/projection/normals/nint_gfm26.Rdata")
rm(nint_gfm26,gfm_rcp26)

load("./data/formatted/fut_clim/gfm_rcp85.Rdata")
nint_gfm85 <- proj_fclim(data = proj_dset, tpa_df=proj_tpa_foc, mod_df = clim_nint_df, mod_pp = clim_nint_pp, 
                      mod_es = clim_nint_es, sp_stats = sp_stats, nonfocal = proj_nonf, bratio = bratio_df,
                      ccf_df = ccf_df,CR_fvs_df = CR_fvs_df, cur_clim = proj_clim, fut_clim = gfm_rcp85)
save(nint_gfm85, file = "./data/formatted/projection/normals/nint_gfm85.Rdata")
rm(nint_gfm85,gfm_rcp85)

load("./data/formatted/fut_clim/gis_rcp26.Rdata")
nint_gis26 <- proj_fclim(data = proj_dset, tpa_df=proj_tpa_foc, mod_df = clim_nint_df, mod_pp = clim_nint_pp, 
                      mod_es = clim_nint_es, sp_stats = sp_stats, nonfocal = proj_nonf, bratio = bratio_df,
                      ccf_df = ccf_df, CR_fvs_df = CR_fvs_df,cur_clim = proj_clim, fut_clim = gis_rcp26)
save(nint_gis26, file = "./data/formatted/projection/normals/nint_gis26.Rdata")
rm(nint_gis26,gis_rcp26)

load("./data/formatted/fut_clim/gis_rcp85.Rdata")
nint_gis85 <- proj_fclim(data = proj_dset, tpa_df=proj_tpa_foc, mod_df = clim_nint_df, mod_pp = clim_nint_pp, 
                      mod_es = clim_nint_es, sp_stats = sp_stats, nonfocal = proj_nonf, bratio = bratio_df,
                      ccf_df = ccf_df,CR_fvs_df = CR_fvs_df, cur_clim = proj_clim, fut_clim = gis_rcp85)
save(nint_gis85, file = "./data/formatted/projection/normals/nint_gis85.Rdata")
rm(nint_gis85,gis_rcp85)

load("./data/formatted/fut_clim/had_rcp26.Rdata")
nint_had26 <- proj_fclim(data = proj_dset, tpa_df=proj_tpa_foc, mod_df = clim_nint_df, mod_pp = clim_nint_pp, 
                      mod_es = clim_nint_es, sp_stats = sp_stats, nonfocal = proj_nonf, bratio = bratio_df,
                      ccf_df = ccf_df, CR_fvs_df = CR_fvs_df,cur_clim = proj_clim, fut_clim = had_rcp26)
save(nint_had26, file = "./data/formatted/projection/normals/nint_had26.Rdata")
rm(nint_had26,had_rcp26)

load("./data/formatted/fut_clim/had_rcp85.Rdata")
nint_had85 <- proj_fclim(data = proj_dset, tpa_df=proj_tpa_foc,mod_df = clim_nint_df, mod_pp = clim_nint_pp, 
                      mod_es = clim_nint_es, sp_stats = sp_stats, nonfocal = proj_nonf, bratio = bratio_df,
                      ccf_df = ccf_df,CR_fvs_df = CR_fvs_df, cur_clim = proj_clim, fut_clim = had_rcp85)
save(nint_had85, file = "./data/formatted/projection/normals/nint_had85.Rdata")
rm(nint_had85,had_rcp85)

load("./data/formatted/fut_clim/hada_rcp26.Rdata")
nint_hada26 <- proj_fclim(data = proj_dset, tpa_df=proj_tpa_foc, mod_df = clim_nint_df, mod_pp = clim_nint_pp, 
                       mod_es = clim_nint_es, sp_stats = sp_stats, nonfocal = proj_nonf, bratio = bratio_df,
                       ccf_df = ccf_df, CR_fvs_df = CR_fvs_df,cur_clim = proj_clim, fut_clim = hada_rcp26)
save(nint_hada26, file = "./data/formatted/projection/normals/nint_hada26.Rdata")
rm(nint_hada26,hada_rcp26)

load("./data/formatted/fut_clim/hada_rcp85.Rdata")
nint_hada85 <- proj_fclim(data = proj_dset, tpa_df=proj_tpa_foc, mod_df = clim_nint_df, mod_pp = clim_nint_pp, 
                       mod_es = clim_nint_es, sp_stats = sp_stats, nonfocal = proj_nonf, bratio = bratio_df,
                       ccf_df = ccf_df,CR_fvs_df = CR_fvs_df, cur_clim = proj_clim, fut_clim = hada_rcp85)
save(nint_hada85, file = "./data/formatted/projection/normals/nint_hada85.Rdata")
rm(nint_hada85,hada_rcp85)

load("./data/formatted/fut_clim/hadc_rcp85.Rdata")
nint_hadc85 <- proj_fclim(data = proj_dset, tpa_df=proj_tpa_foc, mod_df = clim_nint_df, mod_pp = clim_nint_pp, 
                       mod_es = clim_nint_es, sp_stats = sp_stats, nonfocal = proj_nonf, bratio = bratio_df,
                       ccf_df = ccf_df,CR_fvs_df = CR_fvs_df, cur_clim = proj_clim, fut_clim = hadc_rcp85)
save(nint_hadc85, file = "./data/formatted/projection/normals/nint_hadc85.Rdata")
rm(nint_hadc85,hadc_rcp85)

load("./data/formatted/fut_clim/inm_rcp85.Rdata")
nint_inm85 <- proj_fclim(data = proj_dset, tpa_df=proj_tpa_foc, mod_df = clim_nint_df, mod_pp = clim_nint_pp, 
                      mod_es = clim_nint_es, sp_stats = sp_stats, nonfocal = proj_nonf, bratio = bratio_df,
                      ccf_df = ccf_df,CR_fvs_df = CR_fvs_df, cur_clim = proj_clim, fut_clim = inm_rcp85)
save(nint_inm85, file = "./data/formatted/projection/normals/nint_inm85.Rdata")
rm(nint_inm85,inm_rcp85)

load("./data/formatted/fut_clim/ipm_rcp26.Rdata")
nint_ipm26 <- proj_fclim(data = proj_dset, tpa_df=proj_tpa_foc, mod_df = clim_nint_df, mod_pp = clim_nint_pp, 
                      mod_es = clim_nint_es, sp_stats = sp_stats, nonfocal = proj_nonf, bratio = bratio_df,
                      ccf_df = ccf_df, CR_fvs_df = CR_fvs_df,cur_clim = proj_clim, fut_clim = ipm_rcp26)
save(nint_ipm26, file = "./data/formatted/projection/normals/nint_ipm26.Rdata")
rm(nint_ipm26,ipm_rcp26)

load("./data/formatted/fut_clim/ipm_rcp85.Rdata")
nint_ipm85 <- proj_fclim(data = proj_dset, tpa_df=proj_tpa_foc, mod_df = clim_nint_df, mod_pp = clim_nint_pp, 
                      mod_es = clim_nint_es, sp_stats = sp_stats, nonfocal = proj_nonf, bratio = bratio_df,
                      ccf_df = ccf_df,CR_fvs_df = CR_fvs_df, cur_clim = proj_clim, fut_clim = ipm_rcp85)
save(nint_ipm85, file = "./data/formatted/projection/normals/nint_ipm85.Rdata")
rm(nint_ipm85,ipm_rcp85)

load("./data/formatted/fut_clim/ipl_rcp85.Rdata")
nint_ipl85 <- proj_fclim(data = proj_dset, tpa_df=proj_tpa_foc,mod_df = clim_nint_df, mod_pp = clim_nint_pp, 
                         mod_es = clim_nint_es, sp_stats = sp_stats, nonfocal = proj_nonf, bratio = bratio_df,
                      ccf_df = ccf_df,CR_fvs_df = CR_fvs_df, cur_clim = proj_clim, fut_clim = ipl_rcp85)
save(nint_ipl85, file = "./data/formatted/projection/normals/nint_ipl85.Rdata")
rm(nint_ipl85,ipl_rcp85)

load("./data/formatted/fut_clim/mir_rcp26.Rdata")
nint_mir26 <- proj_fclim(data = proj_dset, tpa_df=proj_tpa_foc, mod_df = clim_nint_df, mod_pp = clim_nint_pp, 
                      mod_es = clim_nint_es, sp_stats = sp_stats, nonfocal = proj_nonf, bratio = bratio_df,
                      ccf_df = ccf_df, CR_fvs_df = CR_fvs_df,cur_clim = proj_clim, fut_clim = mir_rcp26)
save(nint_mir26, file = "./data/formatted/projection/normals/nint_mir26.Rdata")
rm(nint_mir26,mir_rcp26)

load("./data/formatted/fut_clim/mir_rcp85.Rdata")
nint_mir85 <- proj_fclim(data = proj_dset, tpa_df=proj_tpa_foc, mod_df = clim_nint_df, mod_pp = clim_nint_pp, 
                      mod_es = clim_nint_es, sp_stats = sp_stats, nonfocal = proj_nonf, bratio = bratio_df,
                      ccf_df = ccf_df,CR_fvs_df = CR_fvs_df, cur_clim = proj_clim, fut_clim = mir_rcp85)
save(nint_mir85, file = "./data/formatted/projection/normals/nint_mir85.Rdata")
rm(nint_mir85,mir_rcp85)

load("./data/formatted/fut_clim/mpl_rcp26.Rdata")
nint_mpl26 <- proj_fclim(data = proj_dset, tpa_df=proj_tpa_foc, mod_df = clim_nint_df, mod_pp = clim_nint_pp, 
                      mod_es = clim_nint_es, sp_stats = sp_stats, nonfocal = proj_nonf, bratio = bratio_df,
                      ccf_df = ccf_df, CR_fvs_df = CR_fvs_df,cur_clim = proj_clim, fut_clim = mpl_rcp26)
save(nint_mpl26, file = "./data/formatted/projection/normals/nint_mpl26.Rdata")
rm(nint_mpl26,mpl_rcp26)

load("./data/formatted/fut_clim/mpl_rcp85.Rdata")
nint_mpl85 <- proj_fclim(data = proj_dset, tpa_df=proj_tpa_foc, mod_df = clim_nint_df, mod_pp = clim_nint_pp, 
                      mod_es = clim_nint_es, sp_stats = sp_stats, nonfocal = proj_nonf, bratio = bratio_df,
                      ccf_df = ccf_df,CR_fvs_df = CR_fvs_df, cur_clim = proj_clim, fut_clim = mpl_rcp85)
save(nint_mpl85, file = "./data/formatted/projection/normals/nint_mpl85.Rdata")
rm(nint_mpl85,mpl_rcp85)

load("./data/formatted/fut_clim/mpm_rcp26.Rdata")
nint_mpm26 <- proj_fclim(data = proj_dset, tpa_df=proj_tpa_foc, mod_df = clim_nint_df, mod_pp = clim_nint_pp, 
                      mod_es = clim_nint_es, sp_stats = sp_stats, nonfocal = proj_nonf, bratio = bratio_df,
                      ccf_df = ccf_df, CR_fvs_df = CR_fvs_df,cur_clim = proj_clim, fut_clim = mpm_rcp26)
save(nint_mpm26, file = "./data/formatted/projection/normals/nint_mpm26.Rdata")
rm(nint_mpm26,mpm_rcp26)

load("./data/formatted/fut_clim/mpm_rcp85.Rdata")
nint_mpm85 <- proj_fclim(data = proj_dset, tpa_df=proj_tpa_foc,mod_df = clim_nint_df, mod_pp = clim_nint_pp, 
                      mod_es = clim_nint_es, sp_stats = sp_stats, nonfocal = proj_nonf, bratio = bratio_df,
                      ccf_df = ccf_df,CR_fvs_df = CR_fvs_df, cur_clim = proj_clim, fut_clim = mpm_rcp85)
save(nint_mpm85, file = "./data/formatted/projection/normals/nint_mpm85.Rdata")
rm(nint_mpm85,mpm_rcp85)

load("./data/formatted/fut_clim/mrc_rcp26.Rdata")
nint_mrc26 <- proj_fclim(data = proj_dset, tpa_df=proj_tpa_foc,mod_df = clim_nint_df, mod_pp = clim_nint_pp, 
                      mod_es = clim_nint_es, sp_stats = sp_stats, nonfocal = proj_nonf, bratio = bratio_df,
                      ccf_df = ccf_df, CR_fvs_df = CR_fvs_df,cur_clim = proj_clim, fut_clim = mrc_rcp26)
save(nint_mrc26, file = "./data/formatted/projection/normals/nint_mrc26.Rdata")
rm(nint_mrc26,mrc_rcp26)

load("./data/formatted/fut_clim/mrc_rcp85.Rdata")
nint_mrc85 <- proj_fclim(data = proj_dset, tpa_df=proj_tpa_foc, mod_df = clim_nint_df, mod_pp = clim_nint_pp, 
                      mod_es = clim_nint_es, sp_stats = sp_stats, nonfocal = proj_nonf, bratio = bratio_df,
                      ccf_df = ccf_df,CR_fvs_df = CR_fvs_df, cur_clim = proj_clim, fut_clim = mrc_rcp85)
save(nint_mrc85, file = "./data/formatted/projection/normals/nint_mrc85.Rdata")
rm(nint_mrc85,mrc_rcp85)

load("./data/formatted/fut_clim/mre_rcp26.Rdata")
nint_mre26 <- proj_fclim(data = proj_dset, tpa_df=proj_tpa_foc, mod_df = clim_nint_df, mod_pp = clim_nint_pp, 
                      mod_es = clim_nint_es, sp_stats = sp_stats, nonfocal = proj_nonf, bratio = bratio_df,
                      ccf_df = ccf_df, CR_fvs_df = CR_fvs_df,cur_clim = proj_clim, fut_clim = mre_rcp26)
save(nint_mre26, file = "./data/formatted/projection/normals/nint_mre26.Rdata")
rm(nint_mre26,mre_rcp26)

load("./data/formatted/fut_clim/mre_rcp85.Rdata")
nint_mre85 <- proj_fclim(data = proj_dset, tpa_df=proj_tpa_foc, mod_df = clim_nint_df, mod_pp = clim_nint_pp, 
                      mod_es = clim_nint_es, sp_stats = sp_stats, nonfocal = proj_nonf, bratio = bratio_df,
                      ccf_df = ccf_df,CR_fvs_df = CR_fvs_df, cur_clim = proj_clim, fut_clim = mre_rcp85)
save(nint_mre85, file = "./data/formatted/projection/normals/nint_mre85.Rdata")
rm(nint_mre85,mre_rcp85)

load("./data/formatted/fut_clim/mri_rcp26.Rdata")
nint_mri26 <- proj_fclim(data = proj_dset, tpa_df=proj_tpa_foc, mod_df = clim_nint_df, mod_pp = clim_nint_pp, 
                      mod_es = clim_nint_es, sp_stats = sp_stats, nonfocal = proj_nonf, bratio = bratio_df,
                      ccf_df = ccf_df, CR_fvs_df = CR_fvs_df,cur_clim = proj_clim, fut_clim = mri_rcp26)
save(nint_mri26, file = "./data/formatted/projection/normals/nint_mri26.Rdata")
rm(nint_mri26,mri_rcp26)

load("./data/formatted/fut_clim/mri_rcp85.Rdata")
nint_mri85 <- proj_fclim(data = proj_dset, tpa_df=proj_tpa_foc, mod_df = clim_nint_df, mod_pp = clim_nint_pp, 
                      mod_es = clim_nint_es, sp_stats = sp_stats, nonfocal = proj_nonf, bratio = bratio_df,
                      ccf_df = ccf_df,CR_fvs_df = CR_fvs_df, cur_clim = proj_clim, fut_clim = mri_rcp85)
save(nint_mri85, file = "./data/formatted/projection/normals/nint_mri85.Rdata")
rm(nint_mri85,mri_rcp85)

load("./data/formatted/fut_clim/nor_rcp26.Rdata")
nint_nor26 <- proj_fclim(data = proj_dset, tpa_df=proj_tpa_foc, mod_df = clim_nint_df, mod_pp = clim_nint_pp, 
                      mod_es = clim_nint_es, sp_stats = sp_stats, nonfocal = proj_nonf, bratio = bratio_df,
                      ccf_df = ccf_df, CR_fvs_df = CR_fvs_df,cur_clim = proj_clim, fut_clim = nor_rcp26)
save(nint_nor26, file = "./data/formatted/projection/normals/nint_nor26.Rdata")
rm(nint_nor26,mrc_nor26)

load("./data/formatted/fut_clim/nor_rcp85.Rdata")
nint_nor85 <- proj_fclim(data = proj_dset, tpa_df=proj_tpa_foc, mod_df = clim_nint_df, mod_pp = clim_nint_pp, 
                      mod_es = clim_nint_es, sp_stats = sp_stats, nonfocal = proj_nonf, bratio = bratio_df,
                      ccf_df = ccf_df,CR_fvs_df = CR_fvs_df, cur_clim = proj_clim, fut_clim = nor_rcp85)
save(nint_nor85, file = "./data/formatted/projection/normals/nint_nor85.Rdata")
rm(nint_nor85,nor_rcp85)
