####################################################################################################
#
#  	--- Last updated:  04.15.2020 BY Shawn Serbin <sserbin@bnl.gov>
####################################################################################################


#--------------------------------------------------------------------------------------------------#
# Close all devices and delete all variables.
rm(list=ls(all=TRUE))   # clear workspace
graphics.off()          # close any open graphics
closeAllConnections()   # close any open connections to files
#--------------------------------------------------------------------------------------------------#


#--------------------------------------------------------------------------------------------------#
library(raster)
library(maps)
library(mapdata)
library(ggplot2)
#--------------------------------------------------------------------------------------------------#


#--------------------------------------------------------------------------------------------------#
### Define file location and name
ascii_grid_dir <- file.path("~/Data/TROPOMI")
tropomi_files <- list.files(ascii_grid_dir, pattern = "*.asc$", full.names = TRUE)
head(tropomi_files)
#--------------------------------------------------------------------------------------------------#


#--------------------------------------------------------------------------------------------------#
### Grab TROPOMI ascii grid data
tropomi_ascii_dat <- readLines(tropomi_files[1])
head(tropomi_ascii_dat)
#--------------------------------------------------------------------------------------------------#


#--------------------------------------------------------------------------------------------------#
### Parse ascii data to allow for re-gridding
pattern <- "[[:digit:]]+(?=\\sbins)"
m <- regexpr(pattern, tropomi_ascii_dat[3], perl=TRUE)
dim <- regmatches(tropomi_ascii_dat[3], m)

m <- regexpr(pattern, tropomi_ascii_dat[4], perl=TRUE)
dim[2] <- regmatches(tropomi_ascii_dat[4], m)

dim <- as.integer(dim)

# parse NA string
pattern <- "(?<=undef=).*"
m <- regexpr(pattern, tropomi_ascii_dat[2], perl=TRUE)
na_string <- regmatches(tropomi_ascii_dat[2], m)

# parse data
dat1 <- tropomi_ascii_dat[-(1:4)]
sep <- grepl("=", dat1, fixed=TRUE)
dat2a <- dat1[sep] #might be useful 
dat2b <- dat1[!sep] #the data
dat2b <- lapply(dat2b, substring, 
                first=seq(1,nchar(dat2b[1]),4), 
                last=  seq(4,nchar(dat2b[1]),4))
dat2b <- unlist(dat2b)
dat2b <- as.numeric(dat2b)
dat2b[dat2b==as.numeric(na_string)] <- NA
dat2b <- matrix(dat2b, nrow=dim[1], byrow=FALSE)
str(dat2b[nrow(dat2b):1, ])
#--------------------------------------------------------------------------------------------------#


#--------------------------------------------------------------------------------------------------#
### Rasterize
dat_raster <- list()

# pixel center
dat_raster$x <- seq(-179.9375,by=0.125,len=2880)
dat_raster$y <- seq(-89.9375,by=0.125,len=1440)
dat_raster$z <- dat2b

tropomi_raster <- raster(dat_raster,crs=CRS("+init=epsg:4326"))
names(tropomi_raster) <- "Tropospheric_NO2"
tropomi_raster
#--------------------------------------------------------------------------------------------------#


#--------------------------------------------------------------------------------------------------#
### Plot gridded data for SEUS
tropomi_raster_df <- as.data.frame(tropomi_raster, xy = TRUE) 

xlims <- c(-93.7,-75.8)
ylims <- c(24,38.2)

ggplot() +
  geom_raster(data = tropomi_raster_df , 
              aes(x = x, y = y, 
                  fill = Tropospheric_NO2*0.01)) + 
  scale_fill_gradientn(limits = c(0, 4),colours = rev(rainbow(8)),
                       na.value = "white") + borders(database="state",
                                                     colour = "black") +
  coord_fixed(xlim = xlims,ylim=ylims) + labs(fill = "Tropospheric NO2 (10^15 molec./cm2)") + 
  xlab("Latitude (dd)") + ylab("Longitude (dd)") + 
theme(legend.position="bottom",legend.key.width = unit(2, "cm"),
      panel.grid.major = element_line(colour = gray(0.5), linetype = "dashed", 
                                      size = 0.5), 
      axis.title.x = element_text(size=14, face="bold"),
      axis.text.x = element_text(size=12),
      axis.title.y = element_text(size=14, face="bold"),
      axis.text.y = element_text(size=12),
      panel.border = element_rect(colour = "black", fill=NA, size=2))
#--------------------------------------------------------------------------------------------------#


#--------------------------------------------------------------------------------------------------#
### EOF