#Growth projection
#under climate change
#Courtney Giebink
#clgiebink@gmail.com
#August 2020

#climate data
#climateNA: 
#or
#climate explorer: 
#https://climexp.knmi.nl/selectfield_cmip5.cgi?id=someone@somewhere
#can be downloaded as ASCII or NetCDF files

#for NetCDF

library(ncdf4) # package for netcdf manipulation
library(raster) # package for raster manipulation
library(rgdal) # package for geospatial analysis
library(ggplot2) # package for plotting

#load climate data
#pr - precipitation (kg m-2 s-1)
nc_pr <- nc_open('./data/raw/climate/projections/icmip5_pr_Amon_modmean_rcp85_-114.095882--108.910335E_42.044053-36.961812N_n_000.nc')
#tasmax - Maximum Near-Surface Air Temperature (C)
nc_tmax <- nc_open('./data/raw/climate/projections/icmip5_tasmax_Amon_modmean_rcp85_-114.095882--108.910335E_42.044053-36.961812N_n_su_000.nc')

nc_tmax
# don't include lon lat because it is averaged over all utah
# lon <- ncvar_get(nc_tmax, "lon")
# lat <- ncvar_get(nc_tmax, "lat", verbose = F)
nc_tmax$dim$time$units
t <- ncvar_get(nc_tmax, "time")

tas <- ncvar_get(nc_tmax, "tasmax")
dim(tas)
#fillvalue_tas <- ncatt_get(nc_tmax, "tasmax", "_FillValue")

#give tas year and month values
tas <- as.data.frame(tas)
tas$Year <- rep(1861:2100, each = 12)
tas$month <- rep(1:12, times = 240)

#remove data before 2009
tas <- tas %>%
  filter(Year > 2009)

#close connection
nc_close(nc_tmax)

#project growth

#load trees
load('./data/formatted/val_dset.Rdata')


