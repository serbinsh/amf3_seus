####################################################################################################
#
#   Plot TROPOMI NO2 data
#
#
####################################################################################################

#--------------------------------------------------------------------------------------------------#
rm(list=ls(all=TRUE))   # clear workspace
graphics.off()          # close any open graphics
closeAllConnections()   # close any open connections to files
#--------------------------------------------------------------------------------------------------#

#--------------------------------------------------------------------------------------------------#
#### Load R libraries
list.of.packages <- c("raster","maps","mapdata","ggplot2")
# check for dependencies and install if needed
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages, dependencies=c("Depends", "Imports",
                                                                       "LinkingTo"))
# Load libraries
invisible(lapply(list.of.packages, library, character.only = TRUE))
`%notin%` <- Negate(`%in%`)
#--------------------------------------------------------------------------------------------------#

#--------------------------------------------------------------------------------------------------#
#### Load TROPOMI file
file <- "no2_202007.tif"
local_tropomi_file_path <- file.path("~/Data/TROPOMI/geotiff_grid_files/")
tropomi.raster <- raster::brick(file.path(local_tropomi_file_path,file), nl = 1)
#--------------------------------------------------------------------------------------------------#

#--------------------------------------------------------------------------------------------------#
#### Output figure directory
# Specify output directory, output_dir 
# Options: 
# tempdir - use a OS-specified temporary directory 
# user defined PATH - e.g. "~/scratch/tropomi"
output_dir <- "tempdir"

if (output_dir=="tempdir") {
  outdir <- tempdir()
} else {
  if (! file.exists(output_dir)) dir.create(output_dir,recursive=TRUE)
  outdir <- file.path(path.expand(output_dir))
}
setwd(outdir) # set working directory
getwd()  # check wd
#--------------------------------------------------------------------------------------------------#

#--------------------------------------------------------------------------------------------------#
#### Create plot
tropomi.raster <- tropomi.raster[[1]]
names(tropomi.raster) <- "Tropospheric_NO2"
tropomi.raster
### Replace the nan value
tropomi.raster[tropomi.raster == 1] <- NA
tropomi.raster[tropomi.raster==2593] <- NA
tropomi_raster_df <- as.data.frame(tropomi.raster, xy = TRUE) 

# define area to plot
xlims <- c(-93.7,-75.8)
ylims <- c(24,38.2)
plot_xlims = xlims
plot_ylims = ylims

# define range for color ramp
lims <- c(0, 4)

out_plot <- ggplot() +
  geom_raster(data = tropomi_raster_df , 
              aes(x = x, y = y, 
                  fill = Tropospheric_NO2*0.01)) + 
  scale_fill_gradientn(limits = lims, colours = rev(rainbow(8)),
                       na.value = "white") + borders(database="state",
                                                     colour = "black") +
  coord_fixed(xlim = plot_xlims,ylim=plot_ylims) + ggtitle(gsub(pattern = ".tif", replacement = "", file)) +
  labs(fill = expression(paste("NO"[2]," Tropospheric column ", "(10"^{15}, " molec./cm"^{2},")",sep=""))) + 
  xlab("Latitude (dd)") + ylab("Longitude (dd)") + 
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
fig_name <- gsub(pattern = ".tif", replacement = ".png", file)
ggsave(file.path(outdir,fig_name), plot = out_plot, width = 24, 
       height = 20, units = "cm", dpi = 300)
#--------------------------------------------------------------------------------------------------#

#--------------------------------------------------------------------------------------------------#
### EOF