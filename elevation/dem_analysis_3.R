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

setwd("~/Data/GitHub/amf3_seus/dem/")
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
#### Output
output_stats_raster_dir <- file.path("~/Data/AMF3/SRTM/geotiff_stats")
if (! file.exists(output_stats_raster_dir)) dir.create(file.path(output_stats_raster_dir),
                                                       recursive=TRUE, 
                                                       showWarnings = FALSE)
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
### Get SRTM - 90meter elevation data
#https://www.gis-blog.com/r-raster-data-acquisition/
#Download two more tiles

srtm <- getData('SRTM', lon=-89, lat=33)
srtm2 <- getData('SRTM', lon=-86, lat=33)
srtm3 <- getData('SRTM', lon=-83, lat=33)
srtm4 <- getData('SRTM', lon=-86, lat=36)
srtm5 <- getData('SRTM', lon=-82, lat=36)
srtm6 <- getData('SRTM', lon=-79, lat=36)
srtm7 <- getData('SRTM', lon=-79, lat=33)
srtm8 <- getData('SRTM', lon=-92, lat=33)
srtm9 <- getData('SRTM', lon=-92, lat=36)

srtmmosaic <- mosaic(srtm, srtm2, srtm3, srtm4, srtm5, srtm6, srtm7, srtm8, srtm9, 
                     fun=mean)

#?terrain
terrain_anal <- terrain(srtmmosaic, opt=c('aspect', 'slope', 'TPI', 'TRI','roughness'), 
                        unit='degrees', neighbors=8)
#terrain_hillshade <- hillShade(terrain(srtmmosaic, opt=c('aspect'),unit='radians'),
#                               terrain(srtmmosaic, opt=c('slope'),unit='radians'),
#                               angle=45, direction=0, filename='', normalize=FALSE)

#terrain_hillshade <- hillShade(terrain_anal$slope, terrain_anal$aspect, angle=45, 
#                               direction=0, filename='', 
#                               normalize=TRUE)
#plot(terrain_hillshade, col=grey(0:100/100), legend=FALSE)
#--------------------------------------------------------------------------------------------------#


#--------------------------------------------------------------------------------------------------#
### Output rasters at original resolution
roughness <- terrain_anal$roughness
names(roughness) <- "Roughness_meters"
raster_name <- "SRTM_90m_Terrain_Roughness"
raster::writeRaster(roughness, filename = file.path(output_stats_raster_dir,raster_name),
                    format = "GTiff", overwrite=TRUE)

slopevals <- terrain_anal$slope
names(slopevals) <- "Slope_degrees"
raster_name <- "SRTM_90m_Slope"
raster::writeRaster(slopevals, filename = file.path(output_stats_raster_dir,raster_name),
                    format = "GTiff", overwrite=TRUE)

#names(terrain_hillshade) <- "Hillshade"
#terrain_hillshade
#raster_name <- "SRTM_90m_Hillshade"
#raster::writeRaster(terrain_hillshade, filename = file.path(output_stats_raster_dir,raster_name),
#                    format = "GTiff", overwrite=TRUE)

#--------------------------------------------------------------------------------------------------#


#--------------------------------------------------------------------------------------------------#
#plot_xlims <- c(-93.7,-75.8)
#plot_ylims <- c(24,38.2)
plot_xlims <- c(-89.3,-76)
plot_ylims <- c(31,39.5)
sites <- data.frame(names = c("BNF", "NEON_TALL"), 
                    longitude = c(-87.273031, -87.393259), 
                    latitude = c( 34.304652, 32.95047))

# rougness - for plotting coarsen the raster for faster plotting
roughness <- terrain_anal$roughness
names(roughness) <- "Roughness_meters"
# https://stackoverflow.com/questions/32278825/how-to-change-the-resolution-of-a-raster-layer-in-r
roughness_res <- aggregate(roughness, fact = 0.02/res(roughness)) # aggregate output
res(roughness_res)

raster_name <- "SRTM_90m_Terrain_Roughness"
names(roughness_res) <- "Roughness_meters"
roughness_df <- as.data.frame(roughness_res, xy = TRUE) 
out_plot <- ggplot() +
  geom_raster(data = roughness_df , 
              aes(x = x, y = y, 
                  fill = Roughness_meters)) + 
  #scale_fill_gradientn(limits = c(0, 100),colours = rev(rainbow(15)),
  scale_fill_gradientn(limits = c(0, 125),colours = rev(RColorBrewer::brewer.pal(11, "Spectral")),
                       na.value = "white") + borders(database="state",
                                                     colour = "black") +
  coord_fixed(xlim = plot_xlims,ylim=plot_ylims) + ggtitle(raster_name) +
  labs(fill = "Terrain Roughness (meters)") + xlab("Latitude (dd)") + ylab("Longitude (dd)") + 
  geom_point(data = sites, aes(x = longitude, y = latitude), size = 4, 
             shape = 21, fill = "grey60") + 
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

#--------------------------------------------------------------------------------------------------#


#--------------------------------------------------------------------------------------------------#
### Zoom in - BNF
plot_xlims <- c(-87.6,-87.0)
plot_ylims <- c( 34.13, 34.5)
extent(plot_xlims,plot_ylims)

roughness <- terrain_anal$roughness
names(roughness) <- "Roughness_meters"
# subset to zoom in area
roughnesscrop <- crop(roughness, extent(plot_xlims,plot_ylims))
plot(roughnesscrop)


raster_name <- "SRTM_90m_Terrain_Roughness_BNF"
roughness_df <- as.data.frame(roughnesscrop, xy = TRUE) 
out_plot <- ggplot() +
  geom_raster(data = roughness_df , 
              aes(x = x, y = y, 
                  fill = Roughness_meters)) + 
  scale_fill_gradientn(limits = c(0, 100),colours = rev(RColorBrewer::brewer.pal(11, "Spectral")),
                       na.value = "white") +
  coord_fixed(xlim = plot_xlims,ylim=plot_ylims) + ggtitle(raster_name) +
  labs(fill = "Terrain Roughness (meters)") + xlab("Latitude (dd)") + ylab("Longitude (dd)") + 
  geom_point(data = sites, aes(x = longitude, y = latitude), size = 4, 
             shape = 21, fill = "grey60") + 
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




slopevals <- terrain_anal$slope
names(slopevals) <- "Slope_degrees"
# subset to zoom in area
slopevalscrop <- crop(slopevals, extent(plot_xlims,plot_ylims))
plot(slopevalscrop)

raster_name <- "SRTM_90m_Terrain_Slope_BNF"
slopevalscrop_df <- as.data.frame(slopevalscrop, xy = TRUE) 
out_plot <- ggplot() +
  geom_raster(data = slopevalscrop_df , 
              aes(x = x, y = y, 
                  fill = Slope_degrees)) + 
  scale_fill_gradientn(limits = c(0, 30),colours = rev(RColorBrewer::brewer.pal(11, "Spectral")),
                       na.value = "white") +
  coord_fixed(xlim = plot_xlims,ylim=plot_ylims) + ggtitle(raster_name) +
  labs(fill = "Slope (degrees)") + xlab("Latitude (dd)") + ylab("Longitude (dd)") + 
  geom_point(data = sites, aes(x = longitude, y = latitude), size = 4, 
             shape = 21, fill = "grey60") + 
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
#--------------------------------------------------------------------------------------------------#



#--------------------------------------------------------------------------------------------------#
### Zoom in - TALL
plot_xlims <- c(-87.670148, -87.079650)
plot_ylims <- c(   32.745124, 33.082520)
extent(plot_xlims,plot_ylims)

roughness <- terrain_anal$roughness
names(roughness) <- "Roughness_meters"
# subset to zoom in area
roughnesscrop <- crop(roughness, extent(plot_xlims,plot_ylims))
plot(roughnesscrop)

raster_name <- "SRTM_90m_Terrain_Roughness_TALL"
roughness_df <- as.data.frame(roughnesscrop, xy = TRUE) 
out_plot <- ggplot() +
  geom_raster(data = roughness_df , 
              aes(x = x, y = y, 
                  fill = Roughness_meters)) + 
  scale_fill_gradientn(limits = c(0, 80),colours = rev(RColorBrewer::brewer.pal(11, "Spectral")),
                       na.value = "white") +
  coord_fixed(xlim = plot_xlims,ylim=plot_ylims) + ggtitle(raster_name) +
  labs(fill = "Terrain Roughness (meters)") + xlab("Latitude (dd)") + ylab("Longitude (dd)") + 
  geom_point(data = sites, aes(x = longitude, y = latitude), size = 4, 
             shape = 21, fill = "grey60") + 
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
#--------------------------------------------------------------------------------------------------#


#--------------------------------------------------------------------------------------------------#
### Larger SEUS maps without any sites plotted

#plot_xlims <- c(-89.3,-76)
#plot_ylims <- c(31,39.5)
plot_xlims <- c(-93.7,-75.8)
plot_ylims <- c(31,39.5)

# rougness - for plotting coarsen the raster for faster plotting
roughness <- terrain_anal$roughness
names(roughness) <- "Roughness_meters"
# https://stackoverflow.com/questions/32278825/how-to-change-the-resolution-of-a-raster-layer-in-r
#roughness_res <- aggregate(roughness, fact = 0.02/res(roughness)) # aggregate output
roughness_res <- aggregate(roughness, fact = 0.03/res(roughness)) # aggregate output
#roughness_res <- aggregate(roughness, fact = 0.1/res(roughness)) # aggregate output
res(roughness_res)

raster_name <- "Full_SEUS_SRTM_90m_Terrain_Roughness"
names(roughness_res) <- "Roughness_meters"
roughness_df <- as.data.frame(roughness_res, xy = TRUE) 
out_plot <- ggplot() +
  geom_raster(data = roughness_df , 
              aes(x = x, y = y, 
                  fill = Roughness_meters)) + 
  #scale_fill_gradientn(limits = c(0, 100),colours = rev(rainbow(15)),
  scale_fill_gradientn(limits = c(0, 125),colours = rev(RColorBrewer::brewer.pal(11, "Spectral")),
                       na.value = "white") + borders(database="state",
                                                     colour = "black") +
  coord_fixed(xlim = plot_xlims,ylim=plot_ylims) + ggtitle(raster_name) +
  labs(fill = "Terrain Roughness (meters)") + xlab("Latitude (dd)") + ylab("Longitude (dd)") + 
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
ggsave(file.path(output_stats_raster_dir,fig_name), plot = out_plot, width = 28, 
       height = 25, units = "cm", dpi = 300)


## slope
slopevals <- terrain_anal$slope
names(slopevals) <- "Slope_degrees"
#slopevals_res <- aggregate(slopevals, fact = 0.02/res(roughness)) # aggregate output
slopevals_res <- aggregate(slopevals, fact = 0.1/res(roughness)) # aggregate output
res(slopevals_res)






#--------------------------------------------------------------------------------------------------#


#--------------------------------------------------------------------------------------------------#
### EOF