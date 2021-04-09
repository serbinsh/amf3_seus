####################################################################################################
#
#
#  	--- Last updated:  04.09.2021 BY Shawn P. Serbin <sserbin@bnl.gov>
####################################################################################################


#--------------------------------------------------------------------------------------------------#
# Close all devices and delete all variables.
rm(list=ls(all=TRUE))   # clear workspace
graphics.off()          # close any open graphics
closeAllConnections()   # close any open connections to files
#--------------------------------------------------------------------------------------------------#


#--------------------------------------------------------------------------------------------------#
#### Load R libraries
#install.packages("spatial.tools", repos="http://R-Forge.R-project.org")
#install.packages("spatial.tools")

list.of.packages <- c("raster","maps","mapdata","ggplot2", "lubridate", "spatial.tools")
# check for dependencies and install if needed
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages, dependencies=c("Depends", "Imports",
                                                                       "LinkingTo"))
# Load libraries
invisible(lapply(list.of.packages, library, character.only = TRUE))
`%notin%` <- Negate(`%in%`)
#--------------------------------------------------------------------------------------------------#


#--------------------------------------------------------------------------------------------------#
#### Options
#product_months <- c(09,10,11) # FALL
#raster_name <- "SON_Tropospheric_NO2_maxvalcomp"
#product_months <- c(12,01,02) # winter
#raster_name <- "DJF_Tropospheric_NO2_maxvalcomp"
#product_months <- c(03,04,05) # spring
#raster_name <- "MAM_Tropospheric_NO2_maxvalcomp"
#product_months <- c(06,07,08) # summer
#raster_name <- "JJA_Tropospheric_NO2_maxvalcomp"

product_months <- seq(1,12,1) # annual
raster_name <- "Annual_Tropospheric_NO2_maxvalcomp"

#--------------------------------------------------------------------------------------------------#


#--------------------------------------------------------------------------------------------------#
#### Input geotiffs
local_geotiff_storage_dir <- file.path("~/Data/TROPOMI/geotiff_grid_files")

#### Output
output_stats_raster_dir <- file.path("~/Data/TROPOMI/geotiff_stats")
if (! file.exists(output_stats_raster_dir)) dir.create(file.path(output_stats_raster_dir),recursive=TRUE, 
                                                         showWarnings = FALSE)
#--------------------------------------------------------------------------------------------------#


#--------------------------------------------------------------------------------------------------#
# this part is a little hacky right now
local_geotiff_files <- list.files(local_geotiff_storage_dir)
file_dates <- gsub(".tif", "", gsub("no2_", "", local_geotiff_files))

file_dates2 <- as.Date(paste0(as.character(file_dates), '01'), format='%Y%m%d')
mns <- lubridate::month(file_dates2)

keep <- mns %in% product_months

local_geotiff_files2 <- local_geotiff_files[keep]
#--------------------------------------------------------------------------------------------------#


#--------------------------------------------------------------------------------------------------#
### Build raster stack
no2_stack <- raster::stack(x = file.path(local_geotiff_storage_dir,local_geotiff_files2))
no2_stack <- brick(no2_stack)
no2_stack
no2_stack[no2_stack==9999] <- NA
#--------------------------------------------------------------------------------------------------#


#--------------------------------------------------------------------------------------------------#
### Raster stats
#https://gis.stackexchange.com/questions/224208/how-to-make-maximum-values-of-a-stacked-raster-to-form-a-new-raster-in-r-using-r
no2_stack_maxvalcomp <- stackApply(no2_stack, indices = rep(1, nlayers(no2_stack)), fun = max,
                                   na.rm=TRUE)
names(no2_stack_maxvalcomp) <- "Tropospheric_NO2"
#calc(b, function(x){max(x)})

#rast_mode <- modal(no2_stack, ties='random', na.rm=FALSE, freq=FALSE)


#--------------------------------------------------------------------------------------------------#


#--------------------------------------------------------------------------------------------------#
plot_xlims <- c(-93.7,-75.8)
plot_ylims <- c(24,38.2)
sites <- data.frame(names = c("BNF", "NEON_TALL"), 
                    longitude = c(-87.273031, -87.393259), 
                    latitude = c( 34.304652, 32.95047))



# max val composite
no2_stack_maxvalcomp_df <- as.data.frame(no2_stack_maxvalcomp, xy = TRUE) 

out_plot <- ggplot() +
  geom_raster(data = no2_stack_maxvalcomp_df , 
              aes(x = x, y = y, 
                  fill = Tropospheric_NO2*0.01)) + 
  scale_fill_gradientn(limits = c(0, 6),colours = rev(rainbow(8)),
                       na.value = "white") + borders(database="state",
                                                     colour = "black") +
  coord_fixed(xlim = plot_xlims,ylim=plot_ylims) + ggtitle(raster_name) +
  labs(fill = expression(paste("NO"[2]," Tropospheric column ", "(10"^{15}, " molec./cm"^{2},")",sep=""))) + 
  xlab("Latitude (dd)") + ylab("Longitude (dd)") + 
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
ggsave(file.path(output_stats_raster_dir,fig_name), plot = out_plot, width = 24, 
       height = 20, units = "cm", dpi = 300)
raster::writeRaster(no2_stack_maxvalcomp, filename = file.path(output_stats_raster_dir,raster_name),
                    format = "GTiff", overwrite=TRUE)

#--------------------------------------------------------------------------------------------------#