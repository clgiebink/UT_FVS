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


#"UT":
#{
#  "name": "Utah",
#  "min_lat": 36.9982,
#  "max_lat": 41.9993,
#  "min_lng": -114.0504,
#  "max_lng": -109.0462
#}

# Crop climate to extent of Utah
library(maps)
m = map_data('state', region = 'Utah')

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

#trees with increment cores
load('./data/formatted/incr_calcov')
#data after tree and stand covariates calculated and filtered

#wide format
final_trees <- per_cov[per_cov$TRE_CN %in% incr_calcov$TRE_CN,]
#final_trees_DF <- final_trees[final_trees$SPCD == 202,]
#final_trees_PP <- final_trees[final_trees$SPCD == 122,]
#final_trees_ES <- final_trees[final_trees$SPCD == 93,]

# Make lat, lon data spatial
ut_tree_spat <- SpatialPointsDataFrame(coords = cbind(final_trees$LON, final_trees$LAT), 
                                       data = final_trees, 
                                       proj4string = CRS("+proj=longlat +datum=NAD83"))

# THE FOLLOWING LINES CAN BE SKIPPED if you've already generated the "ppt_extr.csv" etc. files
### should be moved to historic.R
### should be done once for both the survival and growth data, and dead trees subsetted out (one line) for growth analysis
# Read in PRISM climate stacks
clim.path <-  "./data/formatted/"
ppt <- stack(paste(clim.path,"pptStack.tif",sep=''))
tmax <- stack(paste(clim.path,"tmaxStack.tif",sep=''))
tmin <- stack(paste(clim.path,"tminStack.tif",sep=''))

# raster::extract PRISM data
ppt.extr <- raster::extract(ppt, ut_tree_spat) # this step takes about 8 minutes each (laptop)
tmin.extr <- raster::extract(tmin, ut_tree_spat)
tmax.extr <- raster::extract(tmax, ut_tree_spat)

#Jan 1895 - Dec 2000
# Remove data after Oct, 2016 (because of different CRS Nov, 2016 vpdmax .bil)
# note that the work-around for this problem is to assign the CRS of another layer to Nov and Dec of 2016
# crs(vpdNov2016_raster) <- crs(vpdOct2016_raster)
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
