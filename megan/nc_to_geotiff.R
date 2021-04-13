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
outdir <- file.path("~/Data/RS_GIS_Data/MEGAN")
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
#### Output
output_stats_raster_dir <- file.path("~/Data/RS_GIS_Data/MEGAN/")
if (! file.exists(output_stats_raster_dir)) dir.create(file.path(output_stats_raster_dir),
                                                       recursive=TRUE, 
                                                       showWarnings = FALSE)
#--------------------------------------------------------------------------------------------------#


#--------------------------------------------------------------------------------------------------#
ncpath <- "~/Data/AMF3/MEGAN/MEGAN31_Calc_Input_TCEQ12/MAP/"
ncname <- "EFMAPS31.2019b.tceq_12km.J4"  
ncfname <- paste(ncpath, ncname, ".ncf", sep="")
ncin <- nc_open(ncfname)
print(ncin)

X <- ncin$dim$COL$vals
nX <- length(X)

Y <- ncin$dim$ROW$vals
nY <- length(Y)

print(c(nX,nY))

EF_ISOP <- ncvar_get(nc = ncin, varid = "EF_ISOP", verbose = F)
dim(EF_ISOP)

EF_MT_PINE <- ncvar_get(nc = ncin, varid = "EF_MT_PINE", verbose = F)
dim(EF_MT_PINE)

minc <- c(-978.0,-1626.0)
maxc <- c(1734.0,330.0)
#dat_raster$y <- seq(330.0,-1626.0,-12)

nc_close(ncin)
#--------------------------------------------------------------------------------------------------#


#--------------------------------------------------------------------------------------------------#
### create projected grid
crs <- CRS("+proj=lcc +lat_0=40.0 +lon_0=-97 +lat_1=33 +lat_2=45 +R=6370000.0 +datum=WGS84")
#crs <- CRS("+proj=lcc +lon_0=-97 +lat_1=33 +lat_2=45 +R=6370000.0")
# xy <- expand.grid(X,Y)
# p <- SpatialPoints(xy, proj4string=crs)
# plot(p)


## create EF_ISOP raster
dat_raster <- list()
dat_raster$x <- seq(-978.0,1734.0,12)
dat_raster$y <- seq(-1626.0,330.0,12)

dat <- raster(
  xmn = min(dat_raster$x)*1000, xmx = max(dat_raster$x)*1000,
  ymn = min(dat_raster$y)*1000, ymx = max(dat_raster$y)*1000,
  nrows = nY, ncols = nX,
  crs = crs,
  vals = as.vector(EF_ISOP))
dat
dat2 <- flip(dat,direction='y')
plot(dat2)

names(dat2) <- "EF_ISOP"
dat2

dat3 <- projectRaster(dat2,crs=CRS("+init=epsg:4326"))
dat3[dat3<=0] <- 0
MEGAN_EF_ISOP_latlong <- dat3

raster::writeRaster(dat2, filename = file.path(outdir,"MEGAN_EF_ISOP_LCC.tif"),
                    format = "GTiff", overwrite=T)
raster::writeRaster(dat3, filename = file.path(outdir,"MEGAN_EF_ISOP_latlong.tif"),
                    format = "GTiff", overwrite=T)
raster::writeRaster(dat3, filename = file.path("~/Data/GitHub/amf3_seus/megan/","MEGAN_EF_ISOP_latlong.tif"),
                    format = "GTiff", overwrite=T)
rm(dat_raster, dat, dat2, dat3)


## create EF_MT_PINE raster
dat_raster <- list()
dat_raster$x <- seq(-978.0,1734.0,12)
dat_raster$y <- seq(-1626.0,330.0,12)

dat <- raster(
  xmn = min(dat_raster$x)*1000, xmx = max(dat_raster$x)*1000,
  ymn = min(dat_raster$y)*1000, ymx = max(dat_raster$y)*1000,
  nrows = nY, ncols = nX,
  crs = crs,
  vals = as.vector(EF_MT_PINE))
dat
dat2 <- flip(dat,direction='y')
plot(dat2)

names(dat2) <- "EF_MT_PINE"
dat2

dat3 <- projectRaster(dat2,crs=CRS("+init=epsg:4326"))
dat3[dat3<=0] <- 0
MEGAN_EF_MT_PINE_latlong <- dat3

raster::writeRaster(dat2, filename = file.path(outdir,"MEGAN_EF_MT_PINE_LCC.tif"),
                    format = "GTiff", overwrite=T)
raster::writeRaster(dat3, filename = file.path(outdir,"MEGAN_EF_MT_PINE_latlong.tif"),
                    format = "GTiff", overwrite=T)
raster::writeRaster(dat3, filename = file.path("~/Data/GitHub/amf3_seus/megan/","MEGAN_EF_MT_PINE_latlong.tif"),
                    format = "GTiff", overwrite=T)
rm(dat_raster, dat, dat2, dat3)


#--------------------------------------------------------------------------------------------------#


#--------------------------------------------------------------------------------------------------#
### Quicklooks
#MEGAN_EF_ISOP_latlong
#MEGAN_EF_MT_PINE_latlong

plot_xlims <- c(-94,-77)
plot_ylims <- c(28,39.5)

raster_name <- "MEGAN_EF_ISOP"
MEGAN_EF_ISOP_latlong_df <- as.data.frame(MEGAN_EF_ISOP_latlong, xy = TRUE) 
out_plot <- ggplot() +
  geom_raster(data = MEGAN_EF_ISOP_latlong_df , 
              aes(x = x, y = y, 
                  fill = EF_ISOP)) + 
  scale_fill_gradientn(limits = c(0.001, 18.5),colours = rev(RColorBrewer::brewer.pal(11, "Spectral")),
                       na.value = "white") + borders(database="state",
                                                     colour = "black") + 
  coord_fixed(xlim = plot_xlims,ylim=plot_ylims) + ggtitle(raster_name) +
  labs(fill = "EF ISOP (nanomol/m^2/s)") + xlab("Latitude (dd)") + ylab("Longitude (dd)") + 
#  geom_point(data = sites, aes(x = longitude, y = latitude), size = 4, 
#             shape = 21, fill = "grey60") + 
  theme(legend.position="bottom",legend.key.width = unit(2, "cm"),legend.box="horizontal",
        legend.title = element_text(color = "black", size = 14, face="bold"),
        legend.text = element_text(color = "black", size = 12),
        panel.grid.major = element_line(colour = gray(0.5), linetype = "dashed", 
                                        size = 0.5), 
        axis.title.x = element_text(size=14, face="bold"),
        axis.text.x = element_text(size=12),
        axis.title.y = element_text(size=14, face="bold"),
        axis.text.y = element_text(size=12),
        panel.border = element_rect(colour = "black", fill=NA, size=2))

out_plot

fig_name <- paste0(raster_name,".png")
ggsave(file.path(output_stats_raster_dir,fig_name), plot = out_plot, width = 26, 
       height = 20, units = "cm", dpi = 300)

ggsave(file.path("~/Data/GitHub/amf3_seus/megan/",fig_name), plot = out_plot, width = 26, 
       height = 20, units = "cm", dpi = 300)

raster_name <- "MEGAN_EF_MT_PINE"
MEGAN_EF_MT_PINE_latlong_df <- as.data.frame(MEGAN_EF_MT_PINE_latlong, xy = TRUE) 
out_plot <- ggplot() +
  geom_raster(data = MEGAN_EF_MT_PINE_latlong_df , 
              aes(x = x, y = y, 
                  fill = EF_MT_PINE)) + 
  scale_fill_gradientn(limits = c(0.001, 0.4),colours = rev(RColorBrewer::brewer.pal(11, "Spectral")),
                       na.value = "white") + borders(database="state",
                                                     colour = "black") + 
  coord_fixed(xlim = plot_xlims,ylim=plot_ylims) + ggtitle(raster_name) +
  labs(fill = "EF MT PINE (nanomol/m^2/s)") + xlab("Latitude (dd)") + ylab("Longitude (dd)") + 
  #  geom_point(data = sites, aes(x = longitude, y = latitude), size = 4, 
  #             shape = 21, fill = "grey60") + 
  theme(legend.position="bottom",legend.key.width = unit(2, "cm"),legend.box="horizontal",
        legend.title = element_text(color = "black", size = 14, face="bold"),
        legend.text = element_text(color = "black", size = 12),
        panel.grid.major = element_line(colour = gray(0.5), linetype = "dashed", 
                                        size = 0.5), 
        axis.title.x = element_text(size=14, face="bold"),
        axis.text.x = element_text(size=12),
        axis.title.y = element_text(size=14, face="bold"),
        axis.text.y = element_text(size=12),
        panel.border = element_rect(colour = "black", fill=NA, size=2))

out_plot

fig_name <- paste0(raster_name,".png")
ggsave(file.path(output_stats_raster_dir,fig_name), plot = out_plot, width = 26, 
       height = 20, units = "cm", dpi = 300)

ggsave(file.path("~/Data/GitHub/amf3_seus/megan/",fig_name), plot = out_plot, width = 26, 
       height = 20, units = "cm", dpi = 300)
#--------------------------------------------------------------------------------------------------#













### chaff
#--------------------------------------------------------------------------------------------------#
### create projected matrix
#EF_ISOP <- ncvar_get(nc = ncin, varid = "EF_ISOP", verbose = F)
#EF_ISOPr <- raster(EF_ISOP)
#rext <- extent(0, 512, 0, 512)
#extent(EF_ISOPr) <- rext
#EF_ISOPr <- setExtent(EF_ISOPr, rext, keepres=TRUE)
#EF_ISOPr

#crs(EF_ISOPr) <- CRS('+init=EPSG:102009')
#crs(EF_ISOPr) <- CRS("+init=epsg:102009")
#crs <- CRS("+proj=lcc +lat_1=30 +lat_2=60 +lat_0=38 +lon_0=126 +datum=WGS84")
#crs <- CRS("+proj=lcc +lat_0=40.0 +lon_0=-97 +lat_1=33 +lat_2=45 +R=6370000.0 +datum=WGS84")
#xy <- expand.grid(X,Y)
#p <- SpatialPoints(xy, proj4string=crs)
#extent(p)

#crs(EF_ISOPr) <- crs
#EF_ISOPr
#image(EF_ISOPr)
#plot(EF_ISOPr)

#pr1 <- projectRaster(EF_ISOPr, crs="+proj=lcc +lat_0=40.0 +lon_0=-97 +lat_1=33 +lat_2=45 +R=6370000.0")

#coordinates(EF_ISOPr)

#EF_ISOPr <- extent(EF_ISOPr,matrix(c(0, 512, 0, 512), nrow=2))
#r <- EF_ISOPr
#xyFromCell(r, c(1, ncol(r), ncell(r)-ncol(r)+1, ncell(r)))

#xFromCol(r, c(1, 120, 180))

#XY <- as.matrix(expand.grid(X,Y))
#dim(XY)

#--------------------------------------------------------------------------------------------------#
### EOF
