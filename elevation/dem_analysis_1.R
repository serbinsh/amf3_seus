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
#### Load R libraries
# https://developers.google.com/earth-engine/guides/python_install
# https://cran.r-project.org/web/packages/elevatr/vignettes/introduction_to_elevatr.html
#devtools::install_github("jhollist/elevatr")
list.of.packages <- c("raster","maps","mapdata","ggplot2", "lubridate", "spatial.tools",
                      "elevatr", "maptools", "rgee")
# check for dependencies and install if needed
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages, dependencies=c("Depends", "Imports",
                                                                       "LinkingTo"))
# Load libraries
invisible(lapply(list.of.packages, library, character.only = TRUE))
`%notin%` <- Negate(`%in%`)

sessionInfo()
#--------------------------------------------------------------------------------------------------#



#?rgee::ee_Initialize()
#rgee::ee_Initialize(email="serbinsh@gmail.com")
#rgee::ee_user_info()

#usmap <- maps::map('usa')
#maptools::map2SpatialPolygons(usmap)
#elevation <- elevatr::get_elev_raster(usmap, z = 12)

data(lake)
elevation <- elevatr::get_elev_raster(lake,z = 9)
plot(elevation)
#plot(lake, add=TRUE)
str(lake)
crs(lake)


conus <- rgdal::readOGR("~/Data/GitHub/amf3_seus/shapefiles/state_boundaries/cb_2018_us_state_500k/cb_2018_us_state_500k.shp")
crs(conus) <- CRS("+init=epsg:4326")
crs(conus)
#str(conus)
sp::wkt(conus)

elevation <- elevatr::get_elev_raster(conus, z = 12, prj=CRS("+init=epsg:4326") )



