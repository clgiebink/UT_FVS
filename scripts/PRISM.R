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

# Crop to extent of tree data in Utah
ut_tree_spat <- SpatialPointsDataFrame(coords = cbind(glmm.data.cd$LON, glmm.data.cd$LAT), 
                                 data = glmm.data.cd, 
                                 proj4string = CRS("+proj=longlat +datum=NAD83"))
cropExtent <- extent(ut_tree_spat)
pptStackCropped <- crop(pptStack, cropExtent)
tminStackCropped <- crop(tminStack, cropExtent)
tmaxStackCropped <- crop(tmaxStack, cropExtent)

# Export rasters
clim.path <-  "./data/formatted/"
writeRaster(pptStackCropped, paste0(clim.path, "pptStack.tif"), overwrite = T)
writeRaster(tminStackCropped, paste0(clim.path, "tminStack.tif"), overwrite = T)
writeRaster(tmaxStackCropped, paste0(clim.path, "tmaxStack.tif"), overwrite = T)

#original code from Margaret Evans
#margaret.ekevans@gmail.com

#load data
#wide format

# Make lat, lon data spatial
ut_tree_spat <- SpatialPointsDataFrame(coords = cbind(glmm.data.cd$LON, glmm.data.cd$LAT), 
                                       data = glmm.data.cd, 
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

# Remove data after Oct, 2016 (because of different CRS Nov, 2016 vpdmax .bil)
# note that the work-around for this problem is to assign the CRS of another layer to Nov and Dec of 2016
# crs(vpdNov2016_raster) <- crs(vpdOct2016_raster)
ppt.extr <- ppt.extr[, 1:1272] 
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
write.csv(tmax.extr, paste0(processed.path,"tamx_extr.csv"), row.names = F)


#subset for DF to obtain specific climate growth relationships

#load data
#wide format: glmm.data.cd
data_DF <- glmm.data.cd %>%
  filter(SPCD == 202)

# Make lat, lon data spatial
ut_DF_spat <- SpatialPointsDataFrame(coords = cbind(data_DF$LON, data_DF$LAT), 
                                       data = data_DF, 
                                       proj4string = CRS("+proj=longlat +datum=NAD83"))

# raster::extract PRISM data
ppt.extr_DF <- raster::extract(ppt, ut_DF_spat) # this step takes about 8 minutes each (laptop)
tmin.extr_DF <- raster::extract(tmin, ut_DF_spat)
tmax.extr_DF <- raster::extract(tmax, ut_DF_spat)

# Remove data after Oct, 2016 (because of different CRS Nov, 2016 vpdmax .bil)
# note that the work-around for this problem is to assign the CRS of another layer to Nov and Dec of 2016
# crs(vpdNov2016_raster) <- crs(vpdOct2016_raster)
ppt.extr_DF <- ppt.extr_DF[, 1:1272] 
tmin.extr_DF <- tmin.extr_DF[, 1:1272]
tmax.extr_DF <- tmax.extr_DF[, 1:1272]

# Add sensible column names for raster::extracted climate data
ppt.extr_DF <- as.data.frame(ppt.extr_DF)
tmin.extr_DF <- as.data.frame(tmin.extr_DF)
tmax.extr_DF <- as.data.frame(tmax.extr_DF)
#PRISM.path <-  "./data/raw/climate/PRISM/"
#pptFiles <- list.files(path = PRISM.path, pattern = glob2rx("*ppt*.bil"), full.names = TRUE)
#pptFiles <- pptFiles[1:1272] # (hack to deal with CRS incompatibility, vpd .bil file Nov, 2016)
#tmpFiles <- list.files(path = PRISM.path, pattern = glob2rx("*tmean*.bil"), full.names = TRUE)
#vpdFiles <- list.files(path = PRISM.path, pattern = glob2rx("*vpdmin*.bil"), full.names = TRUE)
#colNames <- lapply(strsplit(pptFiles, "4kmM._"), function (x) x[2])
#colNames <- unlist(colNames)
#colNames <- lapply(strsplit(colNames, "_"), function (x) x[1])
#colNames <- unlist(colNames)
colnames(ppt.extr_DF) <- paste0("ppt_", colNames)
colnames(tmin.extr_DF) <- paste0("tmin_", colNames)
colnames(tmax.extr_DF) <- paste0("tmax_", colNames)

# Export climate data
#processed.path <- "./data/formatted/"
write.csv(ppt.extr_DF, paste0(processed.path,"ppt_extr_DF.csv"), row.names = F)
write.csv(tmin.extr_DF, paste0(processed.path,"tmin_extr_DF.csv"), row.names = F)
write.csv(tmax.extr_DF, paste0(processed.path,"tamx_extr_DF.csv"), row.names = F)


#subset for PP to obtain specific climate growth relationships

#load data
#wide format: glmm.data.cd
data_PP <- glmm.data.cd %>%
  filter(SPCD == 122)

# Make lat, lon data spatial
ut_PP_spat <- SpatialPointsDataFrame(coords = cbind(data_PP$LON, data_PP$LAT), 
                                     data = data_PP, 
                                     proj4string = CRS("+proj=longlat +datum=NAD83"))

# raster::extract PRISM data
ppt.extr_PP <- raster::extract(ppt, ut_PP_spat) # this step takes about 8 minutes each (laptop)
tmin.extr_PP <- raster::extract(tmin, ut_PP_spat)
tmax.extr_PP <- raster::extract(tmax, ut_PP_spat)

# Remove data after Oct, 2016 (because of different CRS Nov, 2016 vpdmax .bil)
# note that the work-around for this problem is to assign the CRS of another layer to Nov and Dec of 2016
# crs(vpdNov2016_raster) <- crs(vpdOct2016_raster)
ppt.extr_PP <- ppt.extr_PP[, 1:1272] 
tmin.extr_PP <- tmin.extr_PP[, 1:1272]
tmax.extr_PP <- tmax.extr_PP[, 1:1272]

# Add sensible column names for raster::extracted climate data
ppt.extr_PP <- as.data.frame(ppt.extr_PP)
tmin.extr_PP <- as.data.frame(tmin.extr_PP)
tmax.extr_PP <- as.data.frame(tmax.extr_PP)
#PRISM.path <-  "./data/raw/climate/PRISM/"
#pptFiles <- list.files(path = PRISM.path, pattern = glob2rx("*ppt*.bil"), full.names = TRUE)
#pptFiles <- pptFiles[1:1272] # (hack to deal with CRS incompatibility, vpd .bil file Nov, 2016)
#tmpFiles <- list.files(path = PRISM.path, pattern = glob2rx("*tmean*.bil"), full.names = TRUE)
#vpdFiles <- list.files(path = PRISM.path, pattern = glob2rx("*vpdmin*.bil"), full.names = TRUE)
#colNames <- lapply(strsplit(pptFiles, "4kmM._"), function (x) x[2])
#colNames <- unlist(colNames)
#colNames <- lapply(strsplit(colNames, "_"), function (x) x[1])
#colNames <- unlist(colNames)
colnames(ppt.extr_PP) <- paste0("ppt_", colNames)
colnames(tmin.extr_PP) <- paste0("tmin_", colNames)
colnames(tmax.extr_PP) <- paste0("tmax_", colNames)

# Export climate data
#processed.path <- "./data/formatted/"
write.csv(ppt.extr_PP, paste0(processed.path,"ppt_extr_PP.csv"), row.names = F)
write.csv(tmin.extr_PP, paste0(processed.path,"tmin_extr_PP.csv"), row.names = F)
write.csv(tmax.extr_PP, paste0(processed.path,"tamx_extr_PP.csv"), row.names = F)


#subset for ES to obtain specific climate growth relationships

#load data
#wide format: glmm.data.cd
data_ES <- glmm.data.cd %>%
  filter(SPCD == 93)

# Make lat, lon data spatial
ut_ES_spat <- SpatialPointsDataFrame(coords = cbind(data_ES$LON, data_ES$LAT), 
                                     data = data_ES, 
                                     proj4string = CRS("+proj=longlat +datum=NAD83"))

# raster::extract PRISM data
ppt.extr_ES <- raster::extract(ppt, ut_ES_spat) # this step takes about 8 minutes each (laptop)
tmin.extr_ES <- raster::extract(tmin, ut_ES_spat)
tmax.extr_ES <- raster::extract(tmax, ut_ES_spat)

# Remove data after Oct, 2016 (because of different CRS Nov, 2016 vpdmax .bil)
# note that the work-around for this problem is to assign the CRS of another layer to Nov and Dec of 2016
# crs(vpdNov2016_raster) <- crs(vpdOct2016_raster)
ppt.extr_ES <- ppt.extr_ES[, 1:1272] 
tmin.extr_ES <- tmin.extr_ES[, 1:1272]
tmax.extr_ES <- tmax.extr_ES[, 1:1272]

# Add sensible column names for raster::extracted climate data
ppt.extr_ES <- as.data.frame(ppt.extr_ES)
tmin.extr_ES <- as.data.frame(tmin.extr_ES)
tmax.extr_ES <- as.data.frame(tmax.extr_ES)
#PRISM.path <-  "./data/raw/climate/PRISM/"
#pptFiles <- list.files(path = PRISM.path, pattern = glob2rx("*ppt*.bil"), full.names = TRUE)
#pptFiles <- pptFiles[1:1272] # (hack to deal with CRS incompatibility, vpd .bil file Nov, 2016)
#tmpFiles <- list.files(path = PRISM.path, pattern = glob2rx("*tmean*.bil"), full.names = TRUE)
#vpdFiles <- list.files(path = PRISM.path, pattern = glob2rx("*vpdmin*.bil"), full.names = TRUE)
#colNames <- lapply(strsplit(pptFiles, "4kmM._"), function (x) x[2])
#colNames <- unlist(colNames)
#colNames <- lapply(strsplit(colNames, "_"), function (x) x[1])
#colNames <- unlist(colNames)
colnames(ppt.extr_ES) <- paste0("ppt_", colNames)
colnames(tmin.extr_ES) <- paste0("tmin_", colNames)
colnames(tmax.extr_ES) <- paste0("tmax_", colNames)

# Export climate data
#processed.path <- "./data/formatted/"
write.csv(ppt.extr_ES, paste0(processed.path,"ppt_extr_ES.csv"), row.names = F)
write.csv(tmin.extr_ES, paste0(processed.path,"tmin_extr_ES.csv"), row.names = F)
write.csv(tmax.extr_ES, paste0(processed.path,"tamx_extr_ES.csv"), row.names = F)


### CONTINUE HERE IF YOU ALREADY HAVE GENERATED THE ABOVE csv files

ppt.extr <- read.csv(paste(processed.path,"ppt_extr.csv",sep=''), header = T)
tmin.extr <- read.csv(paste(processed.path,"tmin_extr.csv",sep=''), header = T)
tmax.extr <- read.csv(paste(processed.path,"tmax_extr.csv",sep=''), header = T)

# Calculate seasonal climate variables for each year
for (i in 1982:2016) {
  print(i)
  # cool season = pNov - Mar
  ppt.extr[, paste0("PPT_c_", i)] <- rowSums(ppt.extr[, c(paste0("ppt_", i-1, "11"), 
                                                          paste0("ppt_", i-1, "12"), 
                                                          paste0("ppt_", i, "01"),
                                                          paste0("ppt_", i, "02"),
                                                          paste0("ppt_", i, "03"))])
  tmp.extr[, paste0("T_c_", i)] <- rowMeans(tmp.extr[, c(paste0("tmp_", i-1, "11"), 
                                                         paste0("tmp_", i-1, "12"), 
                                                         paste0("tmp_", i, "01"),
                                                         paste0("tmp_", i, "02"),
                                                         paste0("tmp_", i, "03"))])
  tmp.extr[, paste0("Tex_c_", i)] <- max(tmp.extr[, c(paste0("tmp_", i-1, "11"), 
                                                      paste0("tmp_", i-1, "12"), 
                                                      paste0("tmp_", i, "01"),
                                                      paste0("tmp_", i, "02"),
                                                      paste0("tmp_", i, "03"))])
  vpd.extr[, paste0("VPD_c_", i)] <- rowMeans(vpd.extr[, c(paste0("vpd_", i-1, "11"), 
                                                           paste0("vpd_", i-1, "12"), 
                                                           paste0("vpd_", i, "01"),
                                                           paste0("vpd_", i, "02"),
                                                           paste0("vpd_", i, "03"))])
  vpd.extr[, paste0("VPDex_c_", i)] <- max(vpd.extr[, c(paste0("vpd_", i-1, "11"), 
                                                        paste0("vpd_", i-1, "12"), 
                                                        paste0("vpd_", i, "01"),
                                                        paste0("vpd_", i, "02"),
                                                        paste0("vpd_", i, "03"))])
  # previous fall = pSep - pOct
  ppt.extr[, paste0("PPT_pf_", i)] <- rowSums(ppt.extr[, c(paste0("ppt_", i-1, "09"),
                                                           paste0("ppt_", i-1, "10"))])
  tmp.extr[, paste0("T_pf_", i)] <- rowMeans(tmp.extr[, c(paste0("tmp_", i-1, "09"),
                                                          paste0("tmp_", i-1, "10"))])
  tmp.extr[, paste0("Tex_pf_", i)] <- max(tmp.extr[, c(paste0("tmp_", i-1, "09"),
                                                       paste0("tmp_", i-1, "10"))])
  vpd.extr[, paste0("VPD_pf_", i)] <- rowMeans(vpd.extr[, c(paste0("vpd_", i-1, "09"),
                                                            paste0("vpd_", i-1, "10"))])
  vpd.extr[, paste0("VPDex_pf_", i)] <- max(vpd.extr[, c(paste0("vpd_", i-1, "09"),
                                                         paste0("vpd_", i-1, "10"))])
  # foresummer = Apr - Jun
  ppt.extr[, paste0("PPT_fs_", i)] <- rowSums(ppt.extr[, c(paste0("ppt_", i, "04"),
                                                           paste0("ppt_", i, "05"),
                                                           paste0("ppt_", i, "06"))])
  tmp.extr[, paste0("T_fs_", i)] <- rowMeans(tmp.extr[, c(paste0("tmp_", i, "04"),
                                                          paste0("tmp_", i, "05"),
                                                          paste0("tmp_", i, "06"))])
  tmp.extr[, paste0("Tex_fs_", i)] <- max(tmp.extr[, c(paste0("tmp_", i, "04"),
                                                       paste0("tmp_", i, "05"),
                                                       paste0("tmp_", i, "06"))])
  vpd.extr[, paste0("VPD_fs_", i)] <- rowMeans(vpd.extr[, c(paste0("vpd_", i, "04"),
                                                            paste0("vpd_", i, "05"),
                                                            paste0("vpd_", i, "06"))])
  vpd.extr[, paste0("VPDex_fs_", i)] <- max(vpd.extr[, c(paste0("vpd_", i, "04"),
                                                         paste0("vpd_", i, "05"),
                                                         paste0("vpd_", i, "06"))])
  
  # warm dry months = pSep - pOct + Apr - Jun
  ppt.extr[, paste0("PPT_wd_", i)] <- rowSums(ppt.extr[, c(paste0("ppt_", i-1, "09"),
                                                           paste0("ppt_", i-1, "10"),
                                                           paste0("ppt_", i, "04"),
                                                           paste0("ppt_", i, "05"),
                                                           paste0("ppt_", i, "06"))])
  tmp.extr[, paste0("T_wd_", i)] <- rowMeans(tmp.extr[, c(paste0("tmp_", i-1, "09"),
                                                          paste0("tmp_", i-1, "10"),
                                                          paste0("tmp_", i, "04"),
                                                          paste0("tmp_", i, "05"),
                                                          paste0("tmp_", i, "06"))])
  tmp.extr[, paste0("Tex_wd_", i)] <- max(tmp.extr[, c(paste0("tmp_", i-1, "09"),
                                                       paste0("tmp_", i-1, "10"),
                                                       paste0("tmp_", i, "04"),
                                                       paste0("tmp_", i, "05"),
                                                       paste0("tmp_", i, "06"))])
  vpd.extr[, paste0("VPD_wd_", i)] <- rowMeans(vpd.extr[, c(paste0("vpd_", i-1, "09"),
                                                            paste0("vpd_", i-1, "10"),
                                                            paste0("vpd_", i, "04"),
                                                            paste0("vpd_", i, "05"),
                                                            paste0("vpd_", i, "06"))])
  vpd.extr[, paste0("VPDex_wd_", i)] <- max(vpd.extr[, c(paste0("vpd_", i-1, "09"),
                                                         paste0("vpd_", i-1, "10"),
                                                         paste0("vpd_", i, "04"),
                                                         paste0("vpd_", i, "05"),
                                                         paste0("vpd_", i, "06"))])
  
  # monsoon = Jul-Aug
  ppt.extr[, paste0("PPT_m_", i)] <- rowSums(ppt.extr[, c(paste0("ppt_", i, "07"),
                                                          paste0("ppt_", i, "08"))])
  tmp.extr[, paste0("T_m_", i)] <- rowMeans(tmp.extr[, c(paste0("tmp_", i, "07"),
                                                         paste0("tmp_", i, "08"))])
  tmp.extr[, paste0("Tex_m_", i)] <- max(tmp.extr[, c(paste0("tmp_", i, "07"),
                                                      paste0("tmp_", i, "08"))])
  vpd.extr[, paste0("VPD_m_", i)] <- rowMeans(vpd.extr[, c(paste0("vpd_", i, "07"),
                                                           paste0("vpd_", i, "08"))])
  vpd.extr[, paste0("VPDex_m_", i)] <- max(vpd.extr[, c(paste0("vpd_", i, "07"),
                                                        paste0("vpd_", i, "08"))])
  # water year = pSept - Aug
  ppt.extr[, paste0("PPT_yr_", i)] <- rowSums(ppt.extr[, c(paste0("ppt_", i-1, "09"),
                                                           paste0("ppt_", i-1, "10"),
                                                           paste0("ppt_", i-1, "11"), 
                                                           paste0("ppt_", i-1, "12"), 
                                                           paste0("ppt_", i, "01"),
                                                           paste0("ppt_", i, "02"),
                                                           paste0("ppt_", i, "03"),
                                                           paste0("ppt_", i, "04"),
                                                           paste0("ppt_", i, "05"),
                                                           paste0("ppt_", i, "06"),
                                                           paste0("ppt_", i, "07"),
                                                           paste0("ppt_", i, "08"))])
  tmp.extr[, paste0("T_yr_", i)] <- rowMeans(tmp.extr[, c(paste0("tmp_", i-1, "09"),
                                                          paste0("tmp_", i-1, "10"),
                                                          paste0("tmp_", i-1, "11"), 
                                                          paste0("tmp_", i-1, "12"), 
                                                          paste0("tmp_", i, "01"),
                                                          paste0("tmp_", i, "02"),
                                                          paste0("tmp_", i, "03"),
                                                          paste0("tmp_", i, "04"),
                                                          paste0("tmp_", i, "05"),
                                                          paste0("tmp_", i, "06"),
                                                          paste0("tmp_", i, "07"),
                                                          paste0("tmp_", i, "08"))])
  vpd.extr[, paste0("VPD_yr_", i)] <- rowMeans(vpd.extr[, c(paste0("vpd_", i-1, "09"),
                                                            paste0("vpd_", i-1, "10"),
                                                            paste0("vpd_", i-1, "11"), 
                                                            paste0("vpd_", i-1, "12"), 
                                                            paste0("vpd_", i, "01"),
                                                            paste0("vpd_", i, "02"),
                                                            paste0("vpd_", i, "03"),
                                                            paste0("vpd_", i, "04"),
                                                            paste0("vpd_", i, "05"),
                                                            paste0("vpd_", i, "06"),
                                                            paste0("vpd_", i, "07"),
                                                            paste0("vpd_", i, "08"))])
}


### THE FOLLOWING ONLY NEEDS TO BE DONE ONCE
### should be moved to normals.R
# Add seasonal climate variables (specific to each tree's census interval) to growth data frame
grData_remeas$PPT_c <- rowMeans(ppt.extr[, paste0("PPT_c_", (grData_remeas[, "PREV_MEASYEAR"]+1):(grData_remeas[, "MEASYEAR"]))])
grData_remeas$T_c <- rowMeans(tmp.extr[, paste0("T_c_", (grData_remeas[i, "PREV_MEASYEAR"]+1):(grData_remeas[i, "MEASYEAR"]))])
grData_remeas$VPD_c <- rowMeans(vpd.extr[, paste0("VPD_c_", (grData_remeas[i, "PREV_MEASYEAR"]+1):(grData_remeas[i, "MEASYEAR"]))])
grData_remeas$PPTex_c <- apply(ppt.extr[, paste0("PPT_c_", (grData_remeas[, "PREV_MEASYEAR"]+1):(grData_remeas[, "MEASYEAR"]))], 1, min)
grData_remeas$Tex_c <- apply(tmp.extr[, paste0("T_c_", (grData_remeas[i, "PREV_MEASYEAR"]+1):(grData_remeas[i, "MEASYEAR"]))], 1, max)
grData_remeas$VPDex_c <- apply(vpd.extr[, paste0("VPD_c_", (grData_remeas[i, "PREV_MEASYEAR"]+1):(grData_remeas[i, "MEASYEAR"]))], 1, max)

grData_remeas$PPT_pf <- rowMeans(ppt.extr[, paste0("PPT_pf_", (grData_remeas[i, "PREV_MEASYEAR"]+1):(grData_remeas[i, "MEASYEAR"]))])
grData_remeas$T_pf <- rowMeans(tmp.extr[, paste0("T_pf_", (grData_remeas[i, "PREV_MEASYEAR"]+1):(grData_remeas[i, "MEASYEAR"]))])
grData_remeas$VPD_pf <- rowMeans(vpd.extr[, paste0("VPD_pf_", (grData_remeas[i, "PREV_MEASYEAR"]+1):(grData_remeas[i, "MEASYEAR"]))])
grData_remeas$PPTex_pf <- apply(ppt.extr[, paste0("PPT_pf_", (grData_remeas[i, "PREV_MEASYEAR"]+1):(grData_remeas[i, "MEASYEAR"]))], 1, min)
grData_remeas$Tex_pf <- apply(tmp.extr[, paste0("T_pf_", (grData_remeas[i, "PREV_MEASYEAR"]+1):(grData_remeas[i, "MEASYEAR"]))], 1, max)
grData_remeas$VPDex_pf <- apply(vpd.extr[, paste0("VPD_pf_", (grData_remeas[i, "PREV_MEASYEAR"]+1):(grData_remeas[i, "MEASYEAR"]))], 1, max)

grData_remeas$PPT_fs <- rowMeans(ppt.extr[, paste0("PPT_fs_", (grData_remeas[i, "PREV_MEASYEAR"]+1):(grData_remeas[i, "MEASYEAR"]))])
grData_remeas$T_fs <- rowMeans(tmp.extr[, paste0("T_fs_", (grData_remeas[i, "PREV_MEASYEAR"]+1):(grData_remeas[i, "MEASYEAR"]))])
grData_remeas$VPD_fs <- rowMeans(vpd.extr[, paste0("VPD_fs_", (grData_remeas[i, "PREV_MEASYEAR"]+1):(grData_remeas[i, "MEASYEAR"]))])
grData_remeas$PPTex_fs <- apply(ppt.extr[, paste0("PPT_fs_", (grData_remeas[i, "PREV_MEASYEAR"]+1):(grData_remeas[i, "MEASYEAR"]))], 1, min)
grData_remeas$Tex_fs <- apply(tmp.extr[, paste0("T_fs_", (grData_remeas[i, "PREV_MEASYEAR"]+1):(grData_remeas[i, "MEASYEAR"]))], 1, max)
grData_remeas$VPDex_fs <- apply(vpd.extr[, paste0("VPD_fs_", (grData_remeas[i, "PREV_MEASYEAR"]+1):(grData_remeas[i, "MEASYEAR"]))], 1, max)

grData_remeas$PPT_wd <- rowMeans(ppt.extr[, paste0("PPT_wd_", (grData_remeas[i, "PREV_MEASYEAR"]+1):(grData_remeas[i, "MEASYEAR"]))])
grData_remeas$T_wd <- rowMeans(tmp.extr[, paste0("T_wd_", (grData_remeas[i, "PREV_MEASYEAR"]+1):(grData_remeas[i, "MEASYEAR"]))])
grData_remeas$VPD_wd <- rowMeans(vpd.extr[, paste0("VPD_wd_", (grData_remeas[i, "PREV_MEASYEAR"]+1):(grData_remeas[i, "MEASYEAR"]))])
grData_remeas$PPTex_wd <- apply(ppt.extr[, paste0("PPT_wd_", (grData_remeas[i, "PREV_MEASYEAR"]+1):(grData_remeas[i, "MEASYEAR"]))], 1, min)
grData_remeas$Tex_wd <- apply(tmp.extr[, paste0("T_wd_", (grData_remeas[i, "PREV_MEASYEAR"]+1):(grData_remeas[i, "MEASYEAR"]))], 1, max)
grData_remeas$VPDex_wd <- apply(vpd.extr[, paste0("VPD_wd_", (grData_remeas[i, "PREV_MEASYEAR"]+1):(grData_remeas[i, "MEASYEAR"]))], 1, max)

grData_remeas$PPT_m <- rowMeans(ppt.extr[, paste0("PPT_m_", grData_remeas[i, "PREV_MEASYEAR"]:(grData_remeas[i, "MEASYEAR"]))])
grData_remeas$T_m <- rowMeans(tmp.extr[, paste0("T_m_", grData_remeas[i, "PREV_MEASYEAR"]:(grData_remeas[i, "MEASYEAR"]))])
grData_remeas$VPD_m <- rowMeans(vpd.extr[, paste0("VPD_m_", grData_remeas[i, "PREV_MEASYEAR"]:(grData_remeas[i, "MEASYEAR"]))])
grData_remeas$PPTex_m <- apply(ppt.extr[, paste0("PPT_m_", grData_remeas[i, "PREV_MEASYEAR"]:(grData_remeas[i, "MEASYEAR"]))], 1, min)
grData_remeas$Tex_m <- apply(tmp.extr[, paste0("T_m_", grData_remeas[i, "PREV_MEASYEAR"]:(grData_remeas[i, "MEASYEAR"]))], 1, max)
grData_remeas$VPDex_m <- apply(vpd.extr[, paste0("VPD_m_", grData_remeas[i, "PREV_MEASYEAR"]:(grData_remeas[i, "MEASYEAR"]))], 1, max)

grData_remeas$PPT_yr <- rowMeans(ppt.extr[, paste0("PPT_yr_", (grData_remeas[i, "PREV_MEASYEAR"]+1):(grData_remeas[i, "MEASYEAR"]))])
grData_remeas$T_yr <- rowMeans(tmp.extr[, paste0("T_yr_", (grData_remeas[i, "PREV_MEASYEAR"]+1):(grData_remeas[i, "MEASYEAR"]))])
grData_remeas$VPD_yr <- rowMeans(vpd.extr[, paste0("VPD_yr_", (grData_remeas[i, "PREV_MEASYEAR"]+1):(grData_remeas[i, "MEASYEAR"]))])
grData_remeas$PPTex_yr <- apply(ppt.extr[, paste0("PPT_yr_", (grData_remeas[i, "PREV_MEASYEAR"]+1):(grData_remeas[i, "MEASYEAR"]))], 1, min)
grData_remeas$Tex_yr <- apply(tmp.extr[, paste0("T_yr_", (grData_remeas[i, "PREV_MEASYEAR"]+1):(grData_remeas[i, "MEASYEAR"]))], 1, max)
grData_remeas$VPDex_yr <- apply(vpd.extr[, paste0("VPD_yr_", (grData_remeas[i, "PREV_MEASYEAR"]+1):(grData_remeas[i, "MEASYEAR"]))], 1, max)


