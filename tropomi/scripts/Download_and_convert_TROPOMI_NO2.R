####################################################################################################
#
#
#   Download compressed TROPOMI NO2 ascii grid files in TOMS format and convert to 
#   GeoTiff format.  Upload to OSF space
#
#
#
#
#  	--- Last updated:  06.09.2021 BY Shawn P. Serbin <sserbin@bnl.gov>
####################################################################################################


#--------------------------------------------------------------------------------------------------#
# Close all devices and delete all variables.
rm(list=ls(all=TRUE))   # clear workspace
graphics.off()          # close any open graphics
closeAllConnections()   # close any open connections to files
#--------------------------------------------------------------------------------------------------#


#--------------------------------------------------------------------------------------------------#
#### Load R libraries
list.of.packages <- c("raster","maps","mapdata","ggplot2","here")
# check for dependencies and install if needed
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages, dependencies=c("Depends", "Imports",
                                                                       "LinkingTo"))
# Load libraries
invisible(lapply(list.of.packages, library, character.only = TRUE))
`%notin%` <- Negate(`%in%`)
#--------------------------------------------------------------------------------------------------#


#--------------------------------------------------------------------------------------------------#
### Function arguments
local_ascii_storage_dir <- file.path("~/Data/RS_GIS_Data/TROPOMI/ascii_grid_files")
if (! file.exists(local_ascii_storage_dir)) dir.create(file.path(local_ascii_storage_dir),recursive=TRUE, 
                                                 showWarnings = FALSE)
local_geotiff_storage_dir <- file.path("~/Data/RS_GIS_Data//TROPOMI/geotiff_grid_files")
if (! file.exists(local_geotiff_storage_dir)) dir.create(file.path(local_geotiff_storage_dir),recursive=TRUE, 
                                                 showWarnings = FALSE)
local_figure_dir <- file.path("~/Data/RS_GIS_Data/TROPOMI/quicklooks")
if (! file.exists(local_figure_dir)) dir.create(file.path(local_figure_dir),recursive=TRUE, 
                                                 showWarnings = FALSE)
# download function
# data source: https://www.temis.nl/airpollution/no2col/no2month_tropomi.php
# temis_data_url <- "http://www.temis.nl/airpollution/no2col/data/" --OLD URL
#temis_data_url <- "https://d1qb6yzwaaq4he.cloudfront.net/tropomi/no2/2020/12/no2_202012.asc.gz"
temis_data_url <- "https://d1qb6yzwaaq4he.cloudfront.net"
product_year <- 2021
product_months <- seq(05,06,1)
overwrite_download = TRUE
run_parallel = TRUE
if (run_parallel) {  # temporary work around
  if (Sys.getenv("RSTUDIO") == "1" && !nzchar(Sys.getenv("RSTUDIO_TERM")) && 
      Sys.info()["sysname"] == "Darwin" && getRversion() == "4.0.2") {
    parallel:::setDefaultClusterOptions(setup_strategy = "sequential")
  }
}
ncores = 5
compressed = TRUE

# conversion function
overwrite_geotiff = TRUE
xlims <- c(-93.7,-75.8)
ylims <- c(24,38.2)
#--------------------------------------------------------------------------------------------------#


#--------------------------------------------------------------------------------------------------#
### Download function
download.tropomi.no2 <- function(outdir=NULL, target_dataset = "tropomi", target_variable = "no2",
                                 product_year = NULL, product_months = NULL, run_parallel = TRUE, 
                                 ncores = NULL, data_url = temis_data_url, compressed = TRUE,
                                 overwrite = FALSE) {
  
  ## setup output folder
  if (! file.exists(outdir)) dir.create(outdir,recursive=TRUE)
  
  ## setup parallel
  if (run_parallel) {
    if (!is.null(ncores)) {
      ncores <- ncores
    } else {
      ncores <- parallel::detectCores() -1
    }
    print(paste0("Running in parallel with: ", ncores))
  }
  
  URL <- temis_data_url
  
  target_filename_prefix <- "no2"
  file_ext <- ".asc.gz"
  target_downloads <- sort(as.vector(paste0(product_year,sprintf("%02d", product_months))))
  
  no2_files <- paste0(target_filename_prefix,"_",target_downloads,file_ext)
  #local_files <- file.path(outdir,gsub(".zip", ".tif",no2_files))
  local_files <- file.path(outdir,no2_files)
  
  no2_data_urls <- paste(URL,target_dataset,target_variable,
                         product_year,sprintf("%02d", product_months),no2_files,sep="/")
  
  # check for local files first
  if (!all(file.exists(local_files)) && !isTRUE(overwrite)) {
    no2_files_final <- no2_files[!file.exists(local_files)]
    no2_data_urls_final <- no2_data_urls[!file.exists(local_files)]
  } else {
    no2_files_final <- no2_files
    no2_data_urls_final <- no2_data_urls
  }
  
  # setup download
  if (length(no2_files_final)<1) {
    warning("*** Requested files already exist on this host  ***")
  } else {
    #check_urls <- paste0(unique(dirname(no2_data_urls_final), fromLast = TRUE),"/") # old approach with old URL
    check_urls <- unique(no2_data_urls_final, fromLast = TRUE) # works with new URL - this is all a little brittle
    remote_file_status <- Map(function(p) httr::HEAD(p), check_urls)
    file_check_list <- array(data = NA, dim = c(length(no2_files_final),2))
    for (i in seq_along(1:length(no2_files_final))) {
      file_check_list[i,1] <- remote_file_status[[i]]$url
      file_check_list[i,2] <- remote_file_status[[i]]$status_code
    }
    if (any(as.numeric(file_check_list[,2]) != 200)) {
      bad_files <- which(as.numeric(file_check_list[,2]) != 200)
      warning(paste0("*** WARNING: file missing from remote. download will be skipped ",
                   no2_data_urls_final[bad_files]))
      no2_data_urls_final <- no2_data_urls_final[which(no2_data_urls_final %notin% no2_data_urls_final[bad_files])]
      no2_files_final <- no2_files_final[which(no2_files_final %notin% no2_files_final[bad_files])]
    }
    if (run_parallel) {
      `%dopar%` <- foreach::`%dopar%`
      cl <- parallel::makeCluster(ncores)
      doParallel::registerDoParallel(cl)
      foreach::foreach(i=1:length(no2_files_final)) %dopar% 
        try(utils::download.file(no2_data_urls_final[i], file.path(outdir, no2_files_final[i])))
    } else {
      Map(function(u, d) download.file(u, d), no2_data_urls_final, file.path(outdir,
                                                                             no2_files_final))
    }
    print("*** Downloading complete ***")
    if (compressed) {
      ## unpack files
      gz_files <- list.files(file.path(outdir), pattern = "*.gz", full.names = TRUE)
      foreach::foreach(k=1:length(gz_files)) %dopar% try(R.utils::gunzip(gz_files[k],remove=FALSE,
                                                                         overwrite=TRUE))
    } # compressed
  } # download
  if (run_parallel) {
    parallel::stopCluster(cl)
  }
} # end of function
download.tropomi.no2(outdir <- local_ascii_storage_dir, target_dataset = "tropomi", target_variable = "no2",
                     product_year=product_year, product_months=product_months, run_parallel = run_parallel, 
                     ncores = ncores, data_url = temis_data_url, compressed = compressed, 
                     overwrite = overwrite_download)
#--------------------------------------------------------------------------------------------------#


#--------------------------------------------------------------------------------------------------#
### Convert to GeoTiff
convert.tropomi.no2 <- function(inputdir=NULL, outdir=NULL, figdir=NULL, product_year = NULL,
                                product_months = NULL, generate_plot = TRUE, plot_xlims = xlims,
                                plot_ylims = ylims,
                                overwrite = FALSE) {
  if (! file.exists(outdir)) dir.create(outdir,recursive=TRUE)
  if (! file.exists(figdir)) dir.create(figdir,recursive=TRUE)
  
  # list all files in input dir
  ascii_grid_dir <- file.path(inputdir)
  tropomi_files <- list.files(ascii_grid_dir, pattern = "*.asc$", full.names = FALSE)
  tropomi_files_fullnames <- list.files(ascii_grid_dir, pattern = "*.asc$", full.names = TRUE)
  
  # files to process
  requested_dates <- sort(as.vector(paste0(product_year,sprintf("%02d", product_months))))
  requested_dates_names <- paste0("no2_",requested_dates,".asc")
  final_files <- requested_dates_names[which(requested_dates_names %in% tropomi_files)]
  
  for (i in seq_along(1:length(final_files))) {
    tropomi_ascii_dat <- readLines(file.path(ascii_grid_dir,final_files[i]))
    head(tropomi_ascii_dat)
    
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
    #str(dat2b[nrow(dat2b):1, ])
    
    ### Rasterize
    dat_raster <- list()
    # pixel center
    dat_raster$x <- seq(-179.9375,by=0.125,len=2880)
    dat_raster$y <- seq(-89.9375,by=0.125,len=1440)
    dat_raster$z <- dat2b
    tropomi_raster <- raster(dat_raster,crs=CRS("+init=epsg:4326"))
    names(tropomi_raster) <- "Tropospheric_NO2"
    #tropomi_raster
    
    # clean up
    tropomi_raster[tropomi_raster[]<0] <- 0
    
    # output raster file
    raster_name <- gsub(pattern = ".asc", replacement = ".tif", final_files[i])
    raster::writeRaster(tropomi_raster, filename = file.path(outdir,raster_name),
                        format = "GTiff", overwrite=overwrite)
    
    if (generate_plot) {
      tropomi_raster_df <- as.data.frame(tropomi_raster, xy = TRUE) 
      if (is.null(plot_xlims)) {
        ggplot() +
          geom_raster(data = tropomi_raster_df , 
                      aes(x = x, y = y, 
                          fill = Tropospheric_NO2*0.01)) + 
          scale_fill_gradientn(limits = c(0, 4),colours = rev(rainbow(8)),
                               na.value = "white") + borders(database="state",
                                                             colour = "black") +
          labs(fill = "Tropospheric NO2 (10^15 molec./cm2)") + 
          xlab("Latitude (dd)") + ylab("Longitude (dd)") + 
          theme(legend.position="bottom",legend.key.width = unit(2, "cm"),
                panel.grid.major = element_line(colour = gray(0.5), linetype = "dashed", 
                                                size = 0.5), 
                axis.title.x = element_text(size=14, face="bold"),
                axis.text.x = element_text(size=12),
                axis.title.y = element_text(size=14, face="bold"),
                axis.text.y = element_text(size=12),
                panel.border = element_rect(colour = "black", fill=NA, size=2))
      } else {
        out_plot <- ggplot() +
          geom_raster(data = tropomi_raster_df , 
                      aes(x = x, y = y, 
                          fill = Tropospheric_NO2*0.01)) + 
          scale_fill_gradientn(limits = c(0, 6),colours = rev(rainbow(8)),
                               na.value = "white") + borders(database="state",
                                                             colour = "black") +
          coord_fixed(xlim = plot_xlims,ylim=plot_ylims) + ggtitle(gsub(pattern = ".tif", replacement = "", raster_name)) +
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
      } # plot extent if/else
      fig_name <- gsub(pattern = ".tif", replacement = ".png", raster_name)
      ggsave(file.path(figdir,fig_name), plot = out_plot, width = 24, 
             height = 20, units = "cm", dpi = 300)
    } # plot if/else
  } #raster loop
} ### end of function
convert.tropomi.no2(inputdir=local_ascii_storage_dir, outdir=local_geotiff_storage_dir, 
                    figdir=local_figure_dir, product_year = product_year,
                    product_months = product_months, generate_plot = TRUE, plot_xlims = xlims,
                    plot_ylims = ylims,
                    overwrite = overwrite_geotiff)
#--------------------------------------------------------------------------------------------------#


#--------------------------------------------------------------------------------------------------#
### EOF