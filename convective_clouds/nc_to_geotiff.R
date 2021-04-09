####################################################################################################
#
#
#  	--- Last updated:  03.22.2021 BY Shawn P. Serbin <sserbin@bnl.gov>
####################################################################################################


#--------------------------------------------------------------------------------------------------#
# Close all devices and delete all variables.
rm(list=ls(all=TRUE))   # clear workspace
graphics.off()          # close any open graphics
closeAllConnections()   # close any open connections to files

setwd("~/Data/RS_GIS_Data/")
outdir <- file.path("~/Data/RS_GIS_Data/")
#--------------------------------------------------------------------------------------------------#

#--------------------------------------------------------------------------------------------------#
#### Load R libraries
list.of.packages <- c("raster","maps","mapdata","ggplot2","ncdf4")
# check for dependencies and install if needed
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages, dependencies=c("Depends", "Imports",
                                                                       "LinkingTo"))
# Load libraries
invisible(lapply(list.of.packages, library, character.only = TRUE))
`%notin%` <- Negate(`%in%`)
#--------------------------------------------------------------------------------------------------#


#--------------------------------------------------------------------------------------------------#
ncpath <- "~/Data/RS_GIS_Data/"
ncname <- "CNV_CLD_MSK_SEUS"  
ncfname <- paste(ncpath, ncname, ".nc", sep="")
ncin <- nc_open(ncfname)
print(ncin)


lon <- ncvar_get(ncin,"lon")
nlon <- dim(lon)
head(lon)

lat <- ncvar_get(ncin,"lat")
nlat <- dim(lat)
head(lat)

print(c(nlon,nlat))
#--------------------------------------------------------------------------------------------------#


#--------------------------------------------------------------------------------------------------#
# get variables
CU_pct <- ncvar_get(nc = ncin, varid = "CU_pct", verbose = F)
dim(CU_pct)
hist(CU_pct)
CU_pct_vec <- as.vector(CU_pct)


OvershootingCb_pct <- ncvar_get(nc = ncin, varid = "OvershootingCb_pct", verbose = F)
dim(OvershootingCb_pct)
hist(OvershootingCb_pct)

ToweringCU_pct <- ncvar_get(nc = ncin, varid = "ToweringCU_pct", verbose = F)
dim(ToweringCU_pct)
hist(ToweringCU_pct)

nc_close(ncin)
#--------------------------------------------------------------------------------------------------#


#--------------------------------------------------------------------------------------------------#
## setup rasters
lonlat <- as.matrix(expand.grid(lon,lat))
dim(lonlat)

# CU_pct
CU_pct_df <- data.frame(cbind(lonlat,CU_pct_vec))
head(CU_pct_df)
names(CU_pct_df) <- c("lon","lat","CU_pct")
dim(CU_pct_df)

dat_raster <- list()
dat_raster$x <- seq(min(lon)-360,by=round(unique(diff(seq(min(lon)-360, max(lon)-360,len=nlon)))[1],2),
                    len=nlon)
length(dat_raster$x)
dat_raster$y <- seq(min(lat),by=unique(diff(seq(min(lat), max(lat),len=nlat)))[1],
                    len=nlat)
length(dat_raster$y)
dat_raster$z <- matrix(CU_pct_df$CU_pct, nrow = length(dat_raster$x), byrow = F)
length(dat_raster$z)

CU_pct_raster <- raster::raster(dat_raster,crs=CRS("+init=epsg:4326"))
names(CU_pct_raster) <- "CU_pct"
image(CU_pct_raster)

rm(dat_raster)

# OvershootingCb_pct
OvershootingCb_pct_df <- data.frame(cbind(lonlat,as.vector(OvershootingCb_pct)))
head(OvershootingCb_pct_df)
names(OvershootingCb_pct_df) <- c("lon","lat","OvershootingCb_pct")
dim(OvershootingCb_pct_df)

dat_raster <- list()
dat_raster$x <- seq(min(lon)-360,by=round(unique(diff(seq(min(lon)-360, max(lon)-360,len=nlon)))[1],2),
                    len=nlon)
length(dat_raster$x)
dat_raster$y <- seq(min(lat),by=unique(diff(seq(min(lat), max(lat),len=nlat)))[1],
                    len=nlat)
length(dat_raster$y)
dat_raster$z <- matrix(OvershootingCb_pct_df$OvershootingCb_pct, 
                       nrow = length(dat_raster$x), byrow = F)
length(dat_raster$z)

OvershootingCb_pct_raster <- raster::raster(dat_raster,crs=CRS("+init=epsg:4326"))
names(OvershootingCb_pct_raster) <- "OvershootingCb_pct"
image(OvershootingCb_pct_raster)

rm(dat_raster)

# ToweringCU_pct
ToweringCU_pct_df <- data.frame(cbind(lonlat,as.vector(ToweringCU_pct)))
head(ToweringCU_pct_df)
names(ToweringCU_pct_df) <- c("lon","lat","ToweringCU_pct")
dim(ToweringCU_pct_df)

dat_raster <- list()
dat_raster$x <- seq(min(lon)-360,by=round(unique(diff(seq(min(lon)-360, max(lon)-360,len=nlon)))[1],2),
                    len=nlon)
length(dat_raster$x)
dat_raster$y <- seq(min(lat),by=unique(diff(seq(min(lat), max(lat),len=nlat)))[1],
                    len=nlat)
length(dat_raster$y)
dat_raster$z <- matrix(ToweringCU_pct_df$ToweringCU_pct, 
                       nrow = length(dat_raster$x), byrow = F)
length(dat_raster$z)

ToweringCU_pct_raster <- raster::raster(dat_raster,crs=CRS("+init=epsg:4326"))
names(ToweringCU_pct_raster) <- "ToweringCU_pct"
image(ToweringCU_pct_raster)

rm(dat_raster)



# output raster file
lstack <- stack(CU_pct_raster, OvershootingCb_pct_raster,ToweringCU_pct_raster)
lstack
raster::writeRaster(lstack, filename = file.path(outdir,"CNV_CLD_MSK_SEUS.tiff"),
                    format = "GTiff", overwrite=T)


#--------------------------------------------------------------------------------------------------#

