####################################################################################################
#
#
#  	--- Last updated:  04.13.2021 BY Shawn P. Serbin <sserbin@bnl.gov>
####################################################################################################


#--------------------------------------------------------------------------------------------------#
# Close all devices and delete all variables.
rm(list=ls(all=TRUE))   # clear workspace
graphics.off()          # close any open graphics
closeAllConnections()   # close any open connections to files
#--------------------------------------------------------------------------------------------------#


#--------------------------------------------------------------------------------------------------#
#### Load R libraries
#devtools::install_github("statnmap/HatchedPolygons")
devtools::install_github("valentinitnelav/geobuffer")

list.of.packages <- c("raster","maps","mapdata","maptools","ggplot2","ncdf4","sf","rgdal",
                      "ggspatial","rnaturalearth")
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
output_stats_raster_dir <- file.path("~/Data/GitHub/amf3_seus/siting_criteria/maps/")
if (! file.exists(output_stats_raster_dir)) dir.create(file.path(output_stats_raster_dir),
                                                       recursive=TRUE, 
                                                       showWarnings = FALSE)
#--------------------------------------------------------------------------------------------------#


#--------------------------------------------------------------------------------------------------#
### Define siting criteria

#--------------------------------------------------------------------------------------------------#


#--------------------------------------------------------------------------------------------------#
### Load spatial data
no2_raster_dir <- file.path("~/Data/RS_GIS_Data/TROPOMI/geotiff_stats/")
no2_data <- file.path(no2_raster_dir,"Annual_Tropospheric_NO2_maxvalcomp.tif")
no2_stack <- raster::stack(x = file.path(no2_data))
no2_stack <- brick(no2_stack)
names(no2_stack) <- "Tropospheric_NO2"
no2_stack[no2_stack==9999] <- NA
#no2_stack[no2_stack==0] <- NA
no2_stack
crs(no2_stack)

#proj4string(no2_stack) <- CRS('+init=epsg:4326')
#proj4string(no2_stack) <- CRS(+init="epsg:4326")
crs(no2_stack)
no2_stack




# newproj <- "+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +towgs84=1,1,-1,0,0,0,0 +units=m +no_defs "
# no2_stack <- projectRaster(no2_stack, crs=newproj)
# no2_stack
# no2_stack_maxvalcomp_df <- as.data.frame(no2_stack, xy = TRUE) 
# ggplot() +
#   geom_raster(data = no2_stack_maxvalcomp_df , 
#               aes(x = x, y = y, 
#                   fill = Tropospheric_NO2*0.01))
# 
# st_crs("ESRI:102009")
# crs(no2_stack)
# 
# image(no2_stack)


# get the GIS layers
gis_data <- file.path("~/Data/GitHub/amf3_seus/shapefiles/")
#nexrad_points <- st_read(file.path(gis_data,"nexrad/nexrad.shp"))
#nexrad_points_sf <- st_as_sf(nexrad_points, 
#                   crs = 4326, agr = "constant")

nexrad_points <- read_sf(dsn = file.path(gis_data,"nexrad/"), layer = "nexrad")
ggplot(data = nexrad_points) +
  geom_sf()
#nexrad_points_df <- fortify(nexrad_points)


nexrad_points_reproj <- st_transform(nexrad_points, 3395)
st_crs(nexrad_points_reproj)$units
nexrad_points_buffer <- st_buffer(nexrad_points_reproj, dist=110000)
nexrad_points_buffer <- nexrad_points_buffer %>%
  st_union()
ggplot(data = nexrad_points_buffer) +
  geom_sf()


#nexrad_buffer <- read_sf(dsn = file.path(gis_data,"nexrad/"), layer = "nexrad_buffer")
#ggplot(data = nexrad_buffer) +
#  geom_sf()

#nexrad_buffer_df <- df_spatial(nexrad_buffer)

#fortify(nexrad_buffer_df)
#ggplot()+ geom_polygon(data=nexrad_buffer_df, aes(x, y), 
#                       colour = alpha("darkred", 1/2), size = 0.7, fill = 'skyblue', alpha = .3)


#st_set_crs(no2_stack,4326)
#st_crs(nexrad_buffer)
#st_crs(nexrad_buffer) <- 4326


#nexrad_points <- readOGR(dsn = file.path(gis_data,"nexrad/nexrad.shp"), stringsAsFactors = F)
#plot(nexrad_points, pch=21)
#summary(nexrad_points@data)

#maptools::readShapePoints
#maptools::readShapePoly	
#nexrad_buffer <- sf::st_read(file.path(gis_data,"nexrad/nexrad_buffer.shp"))
#nexrad_buffer_df <- fortify(nexrad_buffer)

#st_coordinates(nexrad_buffer_df)

#ggplot()+ geom_polygon(data=nexrad_buffer_df, aes(X, Y), 
#                       colour = alpha("darkred", 1/2), size = 0.7, fill = 'skyblue', alpha = .3)
#--------------------------------------------------------------------------------------------------#


#--------------------------------------------------------------------------------------------------#
#plot_xlims <- c(-102,-75)
plot_xlims <- c(-98,-75)
plot_ylims <- c(26,38.2)

#plot_xlims <- c(-98,-74)
#plot_ylims <- c(26,49)
# usmap <- map('usa')
# data(wrld_simpl)
# usamap <- subset(wrld_simpl, NAME=="USA")
# 
# no2_stack_crop <- raster::crop(no2_stack, extent(usamap))
# no2_stack_crop_masked <- mask(raster(no2_stack),usmap)
# 
# no2_stack_crop <- raster::crop(no2_stack, extent(st_as_sf(usmap)))
# no2_stack_crop <- raster(no2_stack_crop)
# no2_stack_crop_masked <- mask(no2_stack_crop,usmap)


# data(wrld_simpl)
# usamap <- subset(wrld_simpl, NAME=="USA")
# no2_masked <- mask(no2_stack, st_as_sf(nexrad_points_buffer), inverse = TRUE)


library(rnaturalearth)
coast <- ne_countries(country = "United States of America")

no2_stack_crop <- raster::crop(no2_stack, extent(usamap))
no2_masked <- mask(no2_stack, coast, inverse = FALSE)

raster_name <- "Annual_Tropospheric_NO2_maxvalcomp_w_nexrad"
out_plot <- ggplot() + layer_spatial(no2_masked, aes(fill = stat(band1*0.01))) + 
  ggtitle(raster_name) + 
  scale_fill_gradientn(limits = c(0, 6),colours = rev(rainbow(8)),
                       na.value = "white") + borders(database="state",
                                                     colour = "black") +
  geom_sf(data = nexrad_points, size = 4, shape = 21, fill = "black") + 
  geom_sf(data = nexrad_points_buffer, colour="black", fill="transparent") + 
  annotation_scale(location = "bl", pad_x = unit(6, "cm"), pad_y = unit(1, "cm")) + 
  annotation_north_arrow(location = "br") +
  labs(fill = expression(paste("NO"[2]," Tropospheric column ", "(10"^{15}, " molec./cm"^{2},")",sep=""))) + 
  xlab("Latitude (dd)") + ylab("Longitude (dd)") +
  coord_sf(xlim = plot_xlims, ylim = plot_ylims, crs = crs(no2_stack), expand = TRUE) + 
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

fig_name <- paste0(raster_name,".png")
ggsave(file.path(output_stats_raster_dir,fig_name), plot = out_plot, width = 30, 
       height = 22, units = "cm", dpi = 250)
  





raster_name <- "Annual_Tropospheric_NO2_maxvalcomp_w_nexrad"
no2_stack_maxvalcomp_df <- as.data.frame(no2_stack, xy = TRUE) 
out_plot <- ggplot() +
  geom_raster(data = no2_stack_maxvalcomp_df , 
              aes(x = x, y = y, 
                  fill = Tropospheric_NO2*0.01)) + 
  scale_fill_gradientn(limits = c(0.0001, 6),colours = rev(rainbow(8)),
                       na.value = "white") + borders(database="state",
                                                     colour = "black") +
  #coord_fixed(xlim = plot_xlims,ylim=plot_ylims) + ggtitle(raster_name) +
  labs(fill = expression(paste("NO"[2]," Tropospheric column ", "(10"^{15}, " molec./cm"^{2},")",sep=""))) + 
  xlab("Latitude (dd)") + ylab("Longitude (dd)") + 
  #geom_polygon(data = fortify(nexrad_buffer_df), aes(x = x, y = y))
  #geom_sf(data = nexrad_points, size = 4, shape = 21, fill = "black") + 
  #geom_sf(data = nexrad_buffer, colour="black", fill="transparent") + 
  #geom_sf(data = nexrad_points_buffer, colour="black", fill=NA) + 
  #geom_polygon(nexrad_points_buffer, stat = stat_sf) + 
  #coord_sf(xlim = plot_xlims, ylim = plot_ylims, crs = crs(no2_stack), expand = TRUE) + 
  #coord_fixed(xlim = plot_xlims,ylim=plot_ylims)
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

fig_name <- paste0(raster_name,".png")
ggsave(file.path(output_stats_raster_dir,fig_name), plot = out_plot, width = 30, 
       height = 22, units = "cm", dpi = 250)
#--------------------------------------------------------------------------------------------------#


