library(sp)
library(raster)
library(rgdal)

#periodic inventory and tree ring data from Justin DeRose
UT_per <- read.csv("T_Utah_periodic_metadata.csv", header = T)
UT_rw <- read.csv("Q_Utah_Courtney_ringwidth.csv",header = T)

#From database
UT_plot <- read.csv("PLOT.txt",header=T)
UT_tree <- read.csv("TREE.txt",header = T)
UT_cond <- read.csv("COND.txt",header = T)

# Read tree data and subset species
#PP 122 PIPO, DF 202 PSME, PI common pinyon 106 PIED Pinus edulis,ES Engelmann spruce 093 PIEN
grData <- UT_tree
#read.csv(paste(data.path,"TREE_COMBINED.csv",sep=''), header = T, stringsAsFactors = F)
##grData <- subset(grData, SPCD == 106)

# Only keep remeasured trees
##grData_remeas <- subset(grData, !is.na(PREVDIA))
# trees that either lived or died (n= 20,609)
##grData_remeas <- subset(grData_remeas, STATUSCD == 1 | STATUSCD == 2)
# Only keep trees that didn't die (n= 15,935)
##grData_remeas <- subset(grData_remeas, STATUSCD == 1)

#matchCNs <- grData_remeas$CN == grData_remeas$PREV_TRE_CN #all CN's changed from time 1 to time 2

### look up previous AGB, PLT_CN, and CONDID
##grData$PREV_DRYBIO_AG <- grData$DRYBIO_AG[match(grData_remeas$PREV_TRE_CN, grData$CN)]
grData$PREV_PLT_CN <- grData$PLT_CN[match(grData_remeas$PREV_TRE_CN, grData$CN)]
grData$PREV_CONDID <- grData$CONDID[match(grData_remeas$PREV_TRE_CN, grData$CN)]

# Subset records with previous AGB found
##grData_remeas <- subset(grData_remeas, !is.na(PREV_DRYBIO_AG)) # should be 15740...yes
##grData_remeas$DRYBIO_AG_DIFF <- grData_remeas$DRYBIO_AG - grData_remeas$PREV_DRYBIO_AG
##grData_remeas$DIA_DIFF <- grData_remeas$DIA - grData_remeas$PREVDIA
#grData_remeas <- subset(grData_remeas, DRYBIO_AG_DIFF >= 0) # truncates data...could be the cause of bad residuals

# basal area increment
##grData_remeas$BAt1 <- ((grData_remeas$PREVDIA/2)^2)*3.14159
##grData_remeas$BAt2 <- ((grData_remeas$DIA/2)^2)*3.14159
##grData_remeas$BAI <- grData_remeas$BAt2 - grData_remeas$BAt1

# Read in plot data and get coordinates and previous measurement year
plots <- read.csv(paste(data.path,"PLOT_COMBINED.csv",sep=''), header = T, stringsAsFactors = F)

grData_remeas$LAT <- plots$LAT[match(grData_remeas$PLT_CN, plots$CN)]
grData_remeas$LON <- plots$LON[match(grData_remeas$PLT_CN, plots$CN)]
grData_remeas$ELEV <- plots$ELEV[match(grData_remeas$PLT_CN, plots$CN)]
grData_remeas$MEASYEAR <- plots$MEASYEAR[match(grData_remeas$PLT_CN, plots$CN)]
grData_remeas$PREV_MEASYEAR <- plots$MEASYEAR[match(grData_remeas$PREV_PLT_CN, plots$CN)]

# Calculate census interval
grData_remeas$CENSUS_INTERVAL <- grData_remeas$MEASYEAR - grData_remeas$PREV_MEASYEAR

# look up previous (tree-specific) condition-level BALIVE
conds <- read.csv(paste(data.path,"COND_COMBINED.csv",sep=''), header = T, stringsAsFactors = F)
conds <- subset(conds, COND_STATUS_CD == 1) # "accessible forest land" by FIA classification
grData_remeas$BALIVE <- apply(X = grData_remeas[, c("PREV_PLT_CN", "PREV_CONDID")], 
                              MARGIN = 1, # applies function to each row in grData_remeas
                              FUN = function(x, conds.df) {
                                conds.df$BALIVE[conds.df$PLT_CN %in% x["PREV_PLT_CN"] &
                                                  conds.df$CONDID %in% x["PREV_CONDID"]]
                              },
                              conds.df = conds)
grData_remeas[is.nan(grData_remeas$BALIVE), "BALIVE"] <- NA
grData_remeas <- subset(grData_remeas, !is.na(BALIVE))

# Make lat, lon data spatial
grSpat <- SpatialPointsDataFrame(coords = cbind(grData_remeas$LON, grData_remeas$LAT), 
                                 data = grData_remeas, 
                                 proj4string = CRS("+proj=longlat +datum=NAD83"))

# THE FOLLOWING LINES CAN BE SKIPPED if you've already generated the "ppt_extr.csv" etc. files
### should be moved to historic.R
### should be done once for both the survival and growth data, and dead trees subsetted out (one line) for growth analysis
# Read in PRISM climate stacks
clim.path <-  "E:/Bayes/DemogRangeMod/ProofOfConcept/FIA-data/westernData/NewData/IWStates/PiedIPM/MEKEvans/ClimateData/"
ppt <- stack(paste(clim.path,"pptStack.tif",sep=''))
tmp <- stack(paste(clim.path,"tmpStack.tif",sep=''))
vpd <- stack(paste(clim.path,"vpdStack.tif",sep=''))

# raster::extract PRISM data
ppt.extr <- raster::extract(ppt, grSpat) # this step takes about 8 minutes each (laptop)
tmp.extr <- raster::extract(tmp, grSpat)
vpd.extr <- raster::extract(vpd, grSpat)

# Remove data after Oct, 2016 (because of different CRS Nov, 2016 vpdmax .bil)
# note that the work-around for this problem is to assign the CRS of another layer to Nov and Dec of 2016
# crs(vpdNov2016_raster) <- crs(vpdOct2016_raster)
ppt.extr <- ppt.extr[, 1:430] 
tmp.extr <- tmp.extr[, 1:430]
vpd.extr <- vpd.extr[, 1:430]

# Add sensible column names for raster::extracted climate data
ppt.extr <- as.data.frame(ppt.extr)
tmp.extr <- as.data.frame(tmp.extr)
vpd.extr <- as.data.frame(vpd.extr)
PRISM.path <-  "E:/Bayes/DemogRangeMod/ProofOfConcept/FIA-data/westernData/NewData/IWStates/PiedIPM/MEKEvans/ClimateData/PRISM/"
pptFiles <- list.files(path = PRISM.path, pattern = glob2rx("*ppt*.bil"), full.names = TRUE)
pptFiles <- pptFiles[1:430] # (hack to deal with CRS incompatibility, vpd .bil file Nov, 2016)
#tmpFiles <- list.files(path = PRISM.path, pattern = glob2rx("*tmean*.bil"), full.names = TRUE)
#vpdFiles <- list.files(path = PRISM.path, pattern = glob2rx("*vpdmin*.bil"), full.names = TRUE)
colNames <- lapply(strsplit(pptFiles, "4kmM._"), function (x) x[2])
colNames <- unlist(colNames)
colNames <- lapply(strsplit(colNames, "_"), function (x) x[1])
colNames <- unlist(colNames)
colnames(ppt.extr) <- paste0("ppt_", colNames)
colnames(tmp.extr) <- paste0("tmp_", colNames)
colnames(vpd.extr) <- paste0("vpd_", colNames)

# Export climate data
processed.path <- "C:/Users/mekevans/Documents/old_user/Documents/CDrive/Bayes/DemogRangeMod/ProofOfConcept/FIA-data/westernData/NewData/IWStates/PiedIPM/MEKEvans/Processed/Growth/"
write.csv(ppt.extr, paste0(processed.path,"ppt_extr.csv"), row.names = F)
write.csv(tmp.extr, paste0(processed.path,"tmp_extr.csv"), row.names = F)
write.csv(vpd.extr, paste0(processed.path,"vpd_extr.csv"), row.names = F)

### CONTINUE HERE IF YOU ALREADY HAVE GENERATED THE ABOVE csv files

ppt.extr <- read.csv(paste(processed.path,"ppt_extr.csv",sep=''), header = T)
tmp.extr <- read.csv(paste(processed.path,"tmp_extr.csv",sep=''), header = T)
vpd.extr <- read.csv(paste(processed.path,"vpd_extr.csv",sep=''), header = T)

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
# import PRISM normals
PRISM.norm.path <-  "E:/Bayes/DemogRangeMod/ProofOfConcept/FIA-data/westernData/NewData/IWStates/PiedIPM/MEKEvans/ClimateData/PRISM/Normals/"
PPT.norm <- stack(paste(clim.path,"pptNormals.tif",sep=''))
TMP.norm <- stack(paste(clim.path,"tmpNormals.tif",sep=''))
VPD.norm <- stack(paste(clim.path,"vpdNormals.tif",sep=''))

# raster::extract PRISM normals 1981-2010
ppt.norm.extr <- raster::extract(PPT.norm, grSpat)
tmp.norm.extr <- raster::extract(TMP.norm, grSpat)
vpd.norm.extr <- raster::extract(VPD.norm, grSpat)

ppt.norm.extr <- as.data.frame(ppt.norm.extr) # not sure this is necessary?
tmp.norm.extr <- as.data.frame(tmp.norm.extr)
vpd.norm.extr <- as.data.frame(vpd.norm.extr)

# reasonable column names (not actually necessary)
colnames(ppt.norm.extr) <- paste0("ppt_", 1:12) 
colnames(tmp.norm.extr) <- paste0("tmp_", 1:12)
colnames(vpd.norm.extr) <- paste0("vpd_", 1:12)

# make seasonal normals and add to growth data frame
# cool season = Nov-Mar
grData_remeas$PPT_c_norm <- rowSums(ppt.norm.extr[, c(1:3, 11:12)])
grData_remeas$T_c_norm <- rowMeans(tmp.norm.extr[, c(1:3, 11:12)])
grData_remeas$VPD_c_norm <- rowMeans(vpd.norm.extr[, c(1:3, 11:12)])
# previous fall = pSept-pOct
grData_remeas$PPT_pf_norm <- rowSums(ppt.norm.extr[, c(9:10)])
grData_remeas$T_pf_norm <- rowMeans(tmp.norm.extr[, c(9:10)])
grData_remeas$VPD_pf_norm <- rowMeans(vpd.norm.extr[, c(9:10)])
# foresummer = Apr-Jun
grData_remeas$PPT_fs_norm <- rowSums(ppt.norm.extr[, c(4:6)])
grData_remeas$T_fs_norm <- rowMeans(tmp.norm.extr[, c(4:6)])
grData_remeas$VPD_fs_norm <- rowMeans(vpd.norm.extr[, c(4:6)])
# warm, dry months = Apr-Jun + Sept-Oct
grData_remeas$PPT_wd_norm <- rowSums(ppt.norm.extr[, c(4:6, 9:10)])
grData_remeas$T_wd_norm <- rowMeans(tmp.norm.extr[, c(4:6, 9:10)])
grData_remeas$VPD_wd_norm <- rowMeans(vpd.norm.extr[, c(4:6, 9:10)])
# monsoon = Jul-Aug
grData_remeas$PPT_m_norm <- rowSums(ppt.norm.extr[, c(7:8)])
grData_remeas$T_m_norm <- rowMeans(tmp.norm.extr[, c(7:8)])
grData_remeas$VPD_m_norm <- rowMeans(vpd.norm.extr[, c(7:8)])
# water year
grData_remeas$PPT_yr_norm <- rowSums(ppt.norm.extr[, c(1:12)])
grData_remeas$T_yr_norm <- rowMeans(tmp.norm.extr[, c(1:12)])
grData_remeas$VPD_yr_norm <- rowMeans(vpd.norm.extr[, c(1:12)])

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
# cool season (pNov-Mar)
grData_remeas$PPT_c_anom <- grData_remeas$PPT_c - grData_remeas$PPT_c_norm # negative
grData_remeas$T_c_anom <- grData_remeas$T_c - grData_remeas$T_c_norm # positive
grData_remeas$VPD_c_anom <- grData_remeas$VPD_c - grData_remeas$VPD_c_norm # positive
grData_remeas$PPTex_c_anom <- grData_remeas$PPTex_c - grData_remeas$PPT_c_norm 
grData_remeas$Tex_c_anom <- grData_remeas$Tex_c - grData_remeas$T_c_norm
grData_remeas$VPDex_c_anom <- grData_remeas$VPDex_c - grData_remeas$VPD_c_norm

# previous fall (pSep-pOct)
grData_remeas$PPT_pf_anom <- grData_remeas$PPT_pf - grData_remeas$PPT_pf_norm # positive anomaly!
grData_remeas$T_pf_anom <- grData_remeas$T_pf - grData_remeas$T_pf_norm # positive
grData_remeas$VPD_pf_anom <- grData_remeas$VPD_pf - grData_remeas$VPD_pf_norm # positive
grData_remeas$PPTex_pf_anom <- grData_remeas$PPTex_pf - grData_remeas$PPT_pf_norm # negative
grData_remeas$Tex_pf_anom <- grData_remeas$Tex_pf - grData_remeas$T_pf_norm # positive
grData_remeas$VPDex_pf_anom <- grData_remeas$VPDex_pf - grData_remeas$VPD_pf_norm # positive
# foresummer (Apr-Jun)
grData_remeas$PPT_fs_anom <- grData_remeas$PPT_fs - grData_remeas$PPT_fs_norm # positive!
grData_remeas$T_fs_anom <- grData_remeas$T_fs - grData_remeas$T_fs_norm # positive
grData_remeas$VPD_fs_anom <- grData_remeas$VPD_fs - grData_remeas$VPD_fs_norm # positive
grData_remeas$PPTex_fs_anom <- grData_remeas$PPTex_fs - grData_remeas$PPT_fs_norm # centered on zero
grData_remeas$Tex_fs_anom <- grData_remeas$Tex_fs - grData_remeas$T_fs_norm # positive
grData_remeas$VPDex_fs_anom <- grData_remeas$VPDex_fs - grData_remeas$VPD_fs_norm # positive
# warm dry months (pSep-pOct + Apr-Jun)
grData_remeas$PPT_wd_anom <- grData_remeas$PPT_wd - grData_remeas$PPT_wd_norm # positive!
grData_remeas$T_wd_anom <- grData_remeas$T_wd - grData_remeas$T_wd_norm
grData_remeas$VPD_wd_anom <- grData_remeas$VPD_wd - grData_remeas$VPD_wd_norm
grData_remeas$PPTex_wd_anom <- grData_remeas$PPTex_wd - grData_remeas$PPT_wd_norm # positive!
grData_remeas$Tex_wd_anom <- grData_remeas$Tex_wd - grData_remeas$T_wd_norm
grData_remeas$VPDex_wd_anom <- grData_remeas$VPDex_wd - grData_remeas$VPD_wd_norm
# monsoon (Jul-Aug)
grData_remeas$PPT_m_anom <- grData_remeas$PPT_m - grData_remeas$PPT_m_norm # positive!
grData_remeas$T_m_anom <- grData_remeas$T_m - grData_remeas$T_m_norm
grData_remeas$VPD_m_anom <- grData_remeas$VPD_m - grData_remeas$VPD_m_norm
grData_remeas$PPTex_m_anom <- grData_remeas$PPTex_m - grData_remeas$PPT_m_norm # centered on zero
grData_remeas$Tex_m_anom <- grData_remeas$Tex_m - grData_remeas$T_m_norm
grData_remeas$VPDex_m_anom <- grData_remeas$VPDex_m - grData_remeas$VPD_m_norm
# water year (pSept - Aug)
grData_remeas$PPT_yr_anom <- grData_remeas$PPT_yr - grData_remeas$PPT_yr_norm # negative
grData_remeas$T_yr_anom <- grData_remeas$T_yr - grData_remeas$T_yr_norm
grData_remeas$VPD_yr_anom <- grData_remeas$VPD_yr - grData_remeas$VPD_yr_norm
grData_remeas$PPTex_yr_anom <- grData_remeas$PPTex_yr - grData_remeas$PPT_yr_norm
grData_remeas$Tex_yr_anom <- grData_remeas$Tex_yr - grData_remeas$T_yr_norm
grData_remeas$VPDex_yr_anom <- grData_remeas$VPDex_yr - grData_remeas$VPD_yr_norm

# Create output data frame
output <- grData_remeas[, c("CN", "PREV_TRE_CN", "PLT_CN", "PREV_PLT_CN", "LAT", "LON", "ELEV",
                            "DRYBIO_AG", "PREV_DRYBIO_AG", "DRYBIO_AG_DIFF", #state variable
                            "DIA", "PREVDIA", "DIA_DIFF", #alternative state variable
                            "MEASYEAR", "PREV_MEASYEAR", "CENSUS_INTERVAL", "BALIVE", 
                            "PPT_c", "T_c", "VPD_c", "PPTex_c", "Tex_c", "VPDex_c",
                            "PPT_wd", "T_wd", "VPD_wd", "PPTex_wd", "Tex_wd", "VPDex_wd",
                            "PPT_pf", "T_pf", "VPD_pf", "PPTex_pf", "Tex_pf", "VPDex_pf",
                            "PPT_fs", "T_fs", "VPD_fs", "PPTex_fs", "Tex_fs", "VPDex_fs",
                            "PPT_m", "T_m", "VPD_m", "PPTex_m", "Tex_m", "VPDex_m",
                            "PPT_yr", "T_yr", "VPD_yr", "PPTex_yr", "Tex_yr", "VPDex_yr",
                            "PPT_c_norm", "T_c_norm", "VPD_c_norm",
                            "PPT_wd_norm", "T_wd_norm", "VPD_wd_norm",
                            "PPT_pf_norm", "T_pf_norm", "VPD_pf_norm",
                            "PPT_fs_norm", "T_fs_norm", "VPD_fs_norm",
                            "PPT_m_norm", "T_m_norm", "VPD_m_norm",
                            "PPT_yr_norm", "T_yr_norm", "VPD_yr_norm",
                            "PPT_c_anom", "T_c_anom", "VPD_c_anom", "PPTex_c_anom", "Tex_c_anom", "VPDex_c_anom",
                            "PPT_wd_anom", "T_wd_anom", "VPD_wd_anom", "PPTex_wd_anom", "Tex_wd_anom", "VPDex_wd_anom",
                            "PPT_pf_anom", "T_pf_anom", "VPD_pf_anom", "PPTex_pf_anom", "Tex_pf_anom", "VPDex_pf_anom",
                            "PPT_fs_anom", "T_fs_anom", "VPD_fs_anom", "PPTex_fs_anom", "Tex_fs_anom", "VPDex_fs_anom",
                            "PPT_m_anom", "T_m_anom", "VPD_m_anom", "PPTex_m_anom", "Tex_m_anom", "VPDex_m_anom",
                            "PPT_yr_anom", "T_yr_anom", "VPD_yr_anom", "PPTex_yr_anom", "Tex_yr_anom", "VPDex_yr_anom")]
write.csv(output, "C:/Users/mekevans/Documents/old_user/Documents/CDrive/Bayes/DemogRangeMod/ProofOfConcept/FIA-data/westernData/NewData/IWStates/PiedIPM/MEKEvans/Processed/Growth/GrowthData.csv", row.names = F)



##### plotting climate and growth data
plot(grData_remeas$LAT, grData_remeas$PPT_m, xlab = "latitude", ylab = "precipitation Jul-Aug")

# make figure showing average climatogram
ppt.mn <- apply(ppt.norm.extr, 2, mean) # find mean 
tmn.mn <- apply(tmp.norm.extr, 2, mean)

ppt.q <- apply(ppt.norm.extr, 2, quantile) # quantiles
ppt.min <- ppt.q["25%", ]
ppt.max <- ppt.q["75%", ]
tmn.q <- apply(tmp.norm.extr, 2, quantile)
tmn.min <- tmn.q["25%", ]
tmn.max <- tmn.q["75%", ]

# data.frame for ggplot
df <- data.frame(tmn.mn, tmn.min, tmn.max,
                 ppt.mn, ppt.min, ppt.max)

### using ggplot2
library("ggplot2")
ggplot(data = df, aes(x = tmn.mn, y = ppt.mn)) +
  geom_point() +
  geom_errorbarh(aes(xmin = tmn.min, xmax = tmn.max)) +
  geom_errorbar(aes(ymin = ppt.min, ymax = ppt.max)) +
  xlab("Temperature ( C)") + 
  ylab("Precipitation (mm)") + 
  theme_bw()


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
