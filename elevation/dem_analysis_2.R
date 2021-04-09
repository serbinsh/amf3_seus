####################################################################################################
#
#
#  	--- Last updated:  02.23.2021 BY Shawn P. Serbin <sserbin@bnl.gov>
####################################################################################################


#--------------------------------------------------------------------------------------------------#
# Close all devices and delete all variables.
rm(list=ls(all=TRUE))   # clear workspace
graphics.off()          # close any open graphics
closeAllConnections()   # close any open connections to files
#--------------------------------------------------------------------------------------------------#


#--------------------------------------------------------------------------------------------------#
list.of.packages <- c("raster","maps","mapdata","ggplot2", "lubridate", "spatial.tools","sp",
                      "elevatr", "maptools")
# check for dependencies and install if needed
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages, dependencies=c("Depends", "Imports",
                                                                       "LinkingTo"))
# Load libraries
invisible(lapply(list.of.packages, library, character.only = TRUE))
`%notin%` <- Negate(`%in%`)

#sessionInfo()
#--------------------------------------------------------------------------------------------------#


#--------------------------------------------------------------------------------------------------#
# https://www.rdocumentation.org/packages/raster/versions/3.4-5/topics/terrain
# TPI for different neighborhood size:
tpiw <- function(x, w=5) {
  m <- matrix(1/(w^2-1), nc=w, nr=w)
  m[ceiling(0.5 * length(m))] <- 0
  f <- focal(x, m)
  x - f
}
#--------------------------------------------------------------------------------------------------#


#--------------------------------------------------------------------------------------------------#
multstates <- maps::map("state", region=c("Alabama", "Florida", "Georgia","Mississippi", "North Carolina", 
                                          "South Carolina", "Tennessee"), fill = TRUE, plot = FALSE)
#IDs <- sapply(strsplit(AL$names, ":"), function(x) x[1])
IDs <- multstates$names
multstates <- map2SpatialPolygons(multstates, IDs=IDs, proj4string=CRS("+proj=longlat +datum=WGS84"))
plot(multstates)
#--------------------------------------------------------------------------------------------------#


#--------------------------------------------------------------------------------------------------#
### Get SRTM
# https://www.rdocumentation.org/packages/raster/versions/3.4-5/topics/getData
# https://dwtkns.com/srtm/

elev <- getData('SRTM', lon=-87, lat=34)
terrain_anal <- terrain(elev, opt=c('slope', 'aspect', 'TPI', 'TRI', 'roughness', 'flowdir'), 
                        unit='degrees', neighbors=8)

terrain_anal2 <- tpiw(elev, w=5)
#--------------------------------------------------------------------------------------------------#


#--------------------------------------------------------------------------------------------------#
### Some plotting
plot(terrain_anal$slope)
plot(multstates, add=T)

#plot(terrain_anal$aspect)
#plot(AL2, add=T)

plot(terrain_anal$tri)
plot(multstates, add=T)

#plot(terrain_anal$tpi)
#plot(AL2, add=T)

plot(terrain_anal$flowdir)
plot(multstates, add=T)

plot(terrain_anal$roughness)
plot(multstates, add=T)



plot(terrain_anal2)
plot(multstates, add=T)
#--------------------------------------------------------------------------------------------------#


#--------------------------------------------------------------------------------------------------#
### get multiple states
#https://www.gis-blog.com/r-raster-data-acquisition/
#Download two more tiles
srtm2 <- getData('SRTM', lon=-83, lat=33)
#srtm3 <- getData('SRTM', lon=9, lat=48)

#Mosaic/merge srtm tiles
srtmmosaic <- mosaic(elev, srtm2, fun=mean)

terrain_anal <- terrain(srtmmosaic, opt=c('slope', 'TPI', 'roughness'), 
                        unit='degrees', neighbors=8)

plot(terrain_anal$slope)
plot(mulstates, add=T)

#--------------------------------------------------------------------------------------------------#


#--------------------------------------------------------------------------------------------------#
### EOF