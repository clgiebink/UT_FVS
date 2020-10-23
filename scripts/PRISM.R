#Original code by Michiel Pillet
#mdpillet@gmail.com
#Updated by Courtney Giebink
#clgiebink@email.arizona.edu

library(raster)

### PRISM download April, 2019
### http://www.prism.oregonstate.edu/
### January 1895 through September 2018
### (123*12) + 9 = 1485 files

# Search for PRISM files
PRISM.path <-  "./data/raw/climate/PRISM/"
pptFiles <- list.files(path = PRISM.path, pattern = glob2rx("*ppt*.bil"), full.names = TRUE)
tminFiles <- list.files(path = PRISM.path, pattern = glob2rx("*tmin*.bil"), full.names = TRUE)
tmaxFiles <- list.files(path = PRISM.path, pattern = glob2rx("*tmax*.bil"), full.names = TRUE)

# Stack monthly data
pptStack <- stack()
for (i in pptFiles) {
  print(i)
  pptStack <- stack(pptStack, raster(i))
}

tminStack <- stack()
for (i in tminFiles) {
  print(i)
  tminStack <- stack(tminStack, raster(i))
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
tminStackCropped <- crop(tminStack, cropUT)
tmaxStackCropped <- crop(tmaxStack, cropUT)

# Export rasters
clim.path <-  "./data/formatted/"
writeRaster(pptStackCropped, paste0(clim.path, "pptStack.tif"), overwrite = T)
writeRaster(tminStackCropped, paste0(clim.path, "tminStack.tif"), overwrite = T)
writeRaster(tmaxStackCropped, paste0(clim.path, "tmaxStack.tif"), overwrite = T)

#original code from Margaret Evans
#margaret.ekevans@gmail.com
#updated by Courtney Giebink

#load trees
#covariate data
load('./data/formatted/per_cov.Rdata')
#time series
load('./data/formatted/incr_calcov.Rdata')
#data after tree and stand covariates calculated and filtered

#wide format
#one tree per row
final_trees <- per_cov[per_cov$TRE_CN %in% incr_calcov$TRE_CN,]

# Make lat, lon data spatial
ut_tree_spat <- SpatialPointsDataFrame(coords = cbind(final_trees$LON, final_trees$LAT), 
                                       data = final_trees, 
                                       proj4string = CRS("+proj=longlat +datum=NAD83"))

# THE FOLLOWING LINES CAN BE SKIPPED if you've already generated the "ppt_extr.csv" etc. files
# Read in PRISM climate stacks
clim.path <-  "./data/formatted/"
ppt <- stack(paste(clim.path,"pptStack.tif",sep=''))
tmax <- stack(paste(clim.path,"tmaxStack.tif",sep=''))
tmin <- stack(paste(clim.path,"tminStack.tif",sep=''))

# raster::extract PRISM data
ppt.extr <- raster::extract(ppt, ut_tree_spat) # this step takes about 30 minutes each (laptop)
tmin.extr <- raster::extract(tmin, ut_tree_spat)
tmax.extr <- raster::extract(tmax, ut_tree_spat)

#Jan 1895 - Dec 2000
ppt.extr <- ppt.extr[, 1:1272] #1895 - 2000
tmin.extr <- tmin.extr[, 1:1272]
tmax.extr <- tmax.extr[, 1:1272]

# Add sensible column names for raster::extracted climate data
ppt.extr <- as.data.frame(ppt.extr)
tmin.extr <- as.data.frame(tmin.extr)
tmax.extr <- as.data.frame(tmax.extr)
PRISM.path <-  "./data/raw/climate/PRISM/"
pptFiles <- list.files(path = PRISM.path, pattern = glob2rx("*ppt*.bil"), full.names = TRUE)
pptFiles <- pptFiles[1:1272] # (hack to deal with CRS incompatibility, vpd .bil file Nov, 2016)
#tmpFiles <- list.files(path = PRISM.path, pattern = glob2rx("*tmean*.bil"), full.names = TRUE)
#vpdFiles <- list.files(path = PRISM.path, pattern = glob2rx("*vpdmin*.bil"), full.names = TRUE)
colNames <- lapply(strsplit(pptFiles, "4kmM._"), function (x) x[2])
colNames <- unlist(colNames)
colNames <- lapply(strsplit(colNames, "_"), function (x) x[1])
colNames <- unlist(colNames)
colnames(ppt.extr) <- paste0("ppt_", colNames)
colnames(tmin.extr) <- paste0("tmin_", colNames)
colnames(tmax.extr) <- paste0("tmax_", colNames)

# Export climate data
processed.path <- "./data/formatted/"
write.csv(ppt.extr, paste0(processed.path,"ppt_extr.csv"), row.names = F)
write.csv(tmin.extr, paste0(processed.path,"tmin_extr.csv"), row.names = F)
write.csv(tmax.extr, paste0(processed.path,"tmax_extr.csv"), row.names = F)

#Climate Normals ----
#5/27/2020
#for validation data set

#Original code by Michiel Pillet
#mdpillet@gmail.com
#Updated by Courtney Giebink
#clgiebink@gmail.com

### PRISM download May, 2020
### http://www.prism.oregonstate.edu/

library(raster)
library(rgdal)

# Read PRISM normals
PRISM.norm.path <-  "./data/raw/climate/normals"
ppt.norm.files <- list.files(path = PRISM.norm.path, pattern = glob2rx("*ppt*.bil"), full.names = TRUE)
tmp.norm.files <- list.files(path = PRISM.norm.path, pattern = glob2rx("*tmean*.bil"), full.names = TRUE)
tmx.norm.files <- list.files(path = PRISM.norm.path, pattern = glob2rx("*tmax*.bil"), full.names = TRUE)
ppt.normals <- stack(ppt.norm.files)
tmp.normals <- stack(tmp.norm.files)
tmx.normals <- stack(tmx.norm.files)

# Crop normals to extent of UT
library(maps)
m <- ggplot2::map_data('state', region = 'Utah')
ut_spat <- SpatialPointsDataFrame(coords = cbind(m$long, m$lat), 
                                  data = m, 
                                  proj4string = CRS("+proj=longlat +datum=NAD83"))
cropUT <- extent(ut_spat)
PPT.norm <- crop(ppt.normals, cropUT)
TMP.norm <- crop(tmp.normals, cropUT)
TMX.norm <- crop(tmx.normals, cropUT)

# Export monthly normals
writeRaster(PPT.norm, paste0(PRISM.norm.path, "pptNormals.tif"), overwrite = T)
writeRaster(TMP.norm, paste0(PRISM.norm.path, "tmpNormals.tif"), overwrite = T)
writeRaster(TMX.norm, paste0(PRISM.norm.path, "tmxNormals.tif"), overwrite = T)

#original code from Margaret Evans
#margaret.ekevans@gmail.com

#validation trees w/ LAT & LON
val_trees <- val_dset %>%
  dplyr::select(TRE_CN,LON,LAT)

# Make lat, lon data spatial
val_tree_spat <- SpatialPointsDataFrame(coords = cbind(val_trees$LON, val_trees$LAT), 
                                        data = val_trees, 
                                        proj4string = CRS("+proj=longlat +datum=NAD83"))

# Read in PRISM climate stacks
ppt.norm <- stack(paste(PRISM.norm.path,"pptNormals.tif",sep=''))
#tmx.norm <- stack(paste(PRISM.norm.path,"tmxNormals.tif",sep=''))
tmp.norm <- stack(paste(PRISM.norm.path,"tmpNormals.tif",sep=''))

# raster::extract PRISM data
n.ppt.extr <- raster::extract(ppt.norm, val_tree_spat)
n.tmp.extr <- raster::extract(tmp.norm, val_tree_spat)
#n.tmx.extr <- raster::extract(tmx.norm, val_tree_spat)

# Add tre_cn column to link to other dataframes
n.ppt.extr <- as.data.frame(n.ppt.extr)
n.ppt.extr$TRE_CN <- val_trees$TRE_CN
n.tmp.extr <- as.data.frame(n.tmp.extr)
n.tmp.extr$TRE_CN <- val_trees$TRE_CN
#n.tmx.extr <- as.data.frame(n.tmx.extr)
#n.tmx.extr$TRE_CN <- val_trees$TRE_CN

# Export climate data
processed.path <- "./data/formatted/"
write.csv(n.ppt.extr, paste0(processed.path,"n_ppt_extr.csv"), row.names = F)
write.csv(n.tmp.extr, paste0(processed.path,"n_tmp_extr.csv"), row.names = F)
write.csv(n.tmx.extr, paste0(processed.path,"n_tmx_extr.csv"), row.names = F)

#join with validation dataset
val_dset$n_ppt <- n.ppt.extr$normalspptNormals[match(val_dset$TRE_CN,n.ppt.extr$TRE_CN)]
val_dset$n_tmp <- n.tmp.extr$normalstmpNormals[match(val_dset$TRE_CN,n.tmp.extr$TRE_CN)]

#calibration trees w/ LAT & LON
load("./data/formatted/data_all.Rdata")
cal_plt <- data_all %>%
  ungroup() %>%
  dplyr::select(PLT_CN,LON,LAT) %>%
  distinct() %>%
  drop_na()

# Make lat, lon data spatial
cal_plt_spat <- SpatialPointsDataFrame(coords = cbind(cal_plt$LON, cal_plt$LAT), 
                                        data = cal_plt, 
                                        proj4string = CRS("+proj=longlat +datum=NAD83"))

# Read in PRISM climate stacks
ppt.norm <- stack(paste(PRISM.norm.path,"pptNormals.tif",sep=''))
tmx.norm <- stack(paste(PRISM.norm.path,"tmxNormals.tif",sep=''))
tmp.norm <- stack(paste(PRISM.norm.path,"tmpNormals.tif",sep=''))

# raster::extract PRISM data
n.ppt.extr <- raster::extract(ppt.norm, cal_plt_spat)
n.tmp.extr <- raster::extract(tmp.norm, cal_plt_spat)
n.tmx.extr <- raster::extract(tmx.norm, cal_plt_spat)

# Add tre_cn column to link to other dataframes
n.ppt.extr <- as.data.frame(n.ppt.extr)
n.ppt.extr$PLT_CN <- cal_plt$PLT_CN
n.tmp.extr <- as.data.frame(n.tmp.extr)
n.tmp.extr$PLT_CN <- cal_plt$PLT_CN
n.tmx.extr <- as.data.frame(n.tmx.extr)
n.tmx.extr$PLT_CN <- cal_plt$PLT_CN

# Export climate data
processed.path <- "./data/formatted/"
write.csv(n.ppt.extr, paste0(processed.path,"n_ppt_cal.csv"), row.names = F)
write.csv(n.tmp.extr, paste0(processed.path,"n_tmp_cal.csv"), row.names = F)
write.csv(n.tmx.extr, paste0(processed.path,"n_tmx_cal.csv"), row.names = F)

