#Climate
#original code from Margaret Evans
#margaret.ekevans@gmail.com

#load data
#wide format

# Make lat, lon data spatial
grSpat <- SpatialPointsDataFrame(coords = cbind(grData_remeas$LON, grData_remeas$LAT), 
                                 data = grData_remeas, 
                                 proj4string = CRS("+proj=longlat +datum=NAD83"))

# THE FOLLOWING LINES CAN BE SKIPPED if you've already generated the "ppt_extr.csv" etc. files
### should be moved to historic.R
### should be done once for both the survival and growth data, and dead trees subsetted out (one line) for growth analysis
# Read in PRISM climate stacks
clim.path <-  ".data/raw/recent/"
ppt <- stack(paste(clim.path,"pptStack.tif",sep=''))
tmax <- stack(paste(clim.path,"tmaxStack.tif",sep=''))
tmin <- stack(paste(clim.path,"tminStack.tif",sep=''))

# raster::extract PRISM data
ppt.extr <- raster::extract(ppt, grSpat) # this step takes about 8 minutes each (laptop)
tmin.extr <- raster::extract(tmin, grSpat)
tmax.extr <- raster::extract(tmax, grSpat)

# Remove data after Oct, 2016 (because of different CRS Nov, 2016 vpdmax .bil)
# note that the work-around for this problem is to assign the CRS of another layer to Nov and Dec of 2016
# crs(vpdNov2016_raster) <- crs(vpdOct2016_raster)
ppt.extr <- ppt.extr[, 1:430] 
tmin.extr <- tmin.extr[, 1:430]
tmax.extr <- tmax.extr[, 1:430]

# Add sensible column names for raster::extracted climate data
ppt.extr <- as.data.frame(ppt.extr)
tmin.extr <- as.data.frame(tmin.extr)
tmax.extr <- as.data.frame(tmax.extr)
PRISM.path <-  "./ClimateData/PRISM/"
pptFiles <- list.files(path = PRISM.path, pattern = glob2rx("*ppt*.bil"), full.names = TRUE)
pptFiles <- pptFiles[1:430] # (hack to deal with CRS incompatibility, vpd .bil file Nov, 2016)
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
processed.path <- "./Processed/Growth/"
write.csv(ppt.extr, paste0(processed.path,"ppt_extr.csv"), row.names = F)
write.csv(tmp.extr, paste0(processed.path,"tmin_extr.csv"), row.names = F)
write.csv(vpd.extr, paste0(processed.path,"tamx_extr.csv"), row.names = F)

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


# create and add anomalies to growth data frame
# monsoon and warm dry season precip anomalies are positive
# cool season and water year anomalies are left-skewed (heavy tail in negative values)
# temperature anomalies are positive (no surprise)


##### plotting climate and growth data
plot(grData_remeas$LAT, grData_remeas$PPT_m, xlab = "latitude", ylab = "precipitation Jul-Aug")


howmanymatchs <- vector(mode="numeric", length=nrow(grData_remeas))
# Look up previous AGB
for (i in 1:nrow(grData_remeas)) {
  print(i)
  tmpCN <- grData_remeas[i, "PREV_TRE_CN"]
  tmpSubset <- subset(grData, CN == tmpCN) #this should only work if the TRE_CN changed from time 1 to time 2
  #  howmanymatchs[i] <- nrow(tmpSubset) #sum(howmanymatchs) = 15740
  if (nrow(tmpSubset) == 1) {
    grData_remeas[i, "PREV_DRYBIO_AG1"] <- tmpSubset$DRYBIO_AG
    grData_remeas[i, "PREV_PLT_CN1"] <- tmpSubset$PLT_CN
    grData_remeas[i, "PREV_CONDID1"] <- tmpSubset$CONDID
  }
  else { # throws away any tree with more (or less) than 2 measurements
    grData_remeas[i, "PREV_DRYBIO_AG1"] <- NA
    grData_remeas[i, "PREV_PLT_CN1"] <- NA
    grData_remeas[i, "PREV_CONDID1"] <- NA
  }
}

# look up coordinates and previous measurement year
for (i in 1:nrow(grData_remeas)) {
  print(i)
  # Get latitude, longitude, and elevation
  tmpPlot <- grData_remeas[i, "PLT_CN"]
  tmpSubset <- subset(plots, CN == tmpPlot)
  grData_remeas[i, "LAT1"] <- tmpSubset$LAT
  grData_remeas[i, "LON1"] <- tmpSubset$LON
  grData_remeas[i, "ELEV1"] <- tmpSubset$ELEV
  grData_remeas[i, "MEASYEAR1"] <- tmpSubset$MEASYEAR
  # Get previous measurement year
  tmpPlot <- grData_remeas[i, "PREV_PLT_CN"]
  tmpSubset <- subset(plots, CN == tmpPlot)
  grData_remeas[i, "PREV_MEASYEAR1"] <- tmpSubset$MEASYEAR
}

### look up basal area of live trees
for (i in 1:nrow(grData_remeas)) {
  print(i)
  tmpPlot <- grData_remeas[i, "PREV_PLT_CN"]
  tmpCondID <- grData_remeas[i, "PREV_CONDID"]
  condsMatch <- subset(conds, PLT_CN == tmpPlot & CONDID == tmpCondID)
  balive <- condsMatch$BALIVE
  #props <- condsMatch$CONDPROP_UNADJ
  #wmean <- weighted.mean(balive, props, na.rm = T)
  if (length(balive) == 0) grData_remeas[i, "baLive1"] <- NA
  else grData_remeas[i, "baLive1"] <- balive
}


for (i in 1:nrow(grData_remeas)) {
  print(i)
  tmpPlot <- grData_remeas[i, "PLT_CN"]
  condsMatch <- subset(conds, PLT_CN == tmpPlot)
  balive <- condsMatch$BALIVE
  props <- condsMatch$CONDPROP_UNADJ
  howmanymatchs[i] <- length(props) 
  wmean <- weighted.mean(balive, props, na.rm = T)
  if (length(balive) == 0) grData_remeas[i, "baLive2"] <- NA
  else grData_remeas[i, "baLive2"] <- wmean
}

plot(grData_remeas$BALIVE, grData_remeas$baLive1) # exactly on the one:one line
plot(grData_remeas$BALIVE, grData_remeas$baLive2) # scatter...biased below the one:one line (makes sense)
