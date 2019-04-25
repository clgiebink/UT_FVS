#Original code by Margaret Evans 

library(raster)

### PRISM download June 14, 2018
### January 1981 through November 2017
### (36*12) + 11 = 443 files

# Search for PRISM files
PRISM.path <-  "E:/Bayes/DemogRangeMod/ProofOfConcept/FIA-data/westernData/NewData/IWStates/PiedIPM/MEKEvans/ClimateData/PRISM/"
pptFiles <- list.files(path = PRISM.path, pattern = glob2rx("*ppt*.bil"), full.names = TRUE)
tmpFiles <- list.files(path = PRISM.path, pattern = glob2rx("*tmean*.bil"), full.names = TRUE)
#vpdminFiles <- list.files(path = PRISM.path, pattern = glob2rx("*vpdmin*.bil"), full.names = TRUE)
vpdmaxFiles <- list.files(path = PRISM.path, pattern = glob2rx("*vpdmax*.bil"), full.names = TRUE)

# Stack monthly data
pptStack <- stack()
for (i in pptFiles) {
  print(i)
  pptStack <- stack(pptStack, raster(i))
}

tmpStack <- stack()
for (i in tmpFiles) {
  print(i)
  tmpStack <- stack(tmpStack, raster(i))
}

vpdStack <- stack()
for (i in 1:length(vpdmaxFiles)) {
  print(i)
  vpdStack <- stack(vpdStack, raster(vpdmaxFiles[i]))
}

# Crop to extent of FIA Pinus edulis occurrences
cropExtent <- extent(raster("C:/Users/mekevans/Documents/old_user/Documents/CDrive/Bayes/DemogRangeMod/ProofOfConcept/FIA-data/westernData/NewData/IWStates/PiedIPM/MEKEvans/BA/BA.tif"))
pptStackCropped <- crop(pptStack, cropExtent)
tmpStackCropped <- crop(tmpStack, cropExtent)
vpdStackCropped <- crop(vpdStack, cropExtent)

# Export rasters
clim.path <-  "E:/Bayes/DemogRangeMod/ProofOfConcept/FIA-data/westernData/NewData/IWStates/PiedIPM/MEKEvans/ClimateData/"
writeRaster(pptStackCropped, paste0(clim.path, "pptStack.tif"), overwrite = T)
writeRaster(tmpStackCropped, paste0(clim.path, "tmpStack.tif"), overwrite = T)
writeRaster(vpdStackCropped, paste0(clim.path, "vpdStack.tif"), overwrite = T)