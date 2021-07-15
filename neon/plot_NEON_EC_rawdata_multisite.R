####################################################################################################
#
#  	--- Last updated:  07.14.2021 BY Shawn Serbin <sserbin@bnl.gov>
####################################################################################################


#--------------------------------------------------------------------------------------------------#
# Close all devices and delete all variables.
rm(list=ls(all=TRUE))   # clear workspace
graphics.off()          # close any open graphics
closeAllConnections()   # close any open connections to files
#--------------------------------------------------------------------------------------------------#


#--------------------------------------------------------------------------------------------------#
# https://www.neonscience.org/resources/learning-hub/tutorials/eddy-data-intro 
# https://www.neonscience.org/resources/learning-hub/tutorials/neonstore-stackfromstore-tutorial

### Load libraries
devtools::install_github("cboettig/neonstore")

list.of.packages <- c("dplyr","here","ggplot2","gridExtra","neonstore",
                      "neonUtilities","openair","clifro")
invisible(lapply(list.of.packages, library, character.only = TRUE))

here::here()
#--------------------------------------------------------------------------------------------------#


#--------------------------------------------------------------------------------------------------#
###  Setup neonstore sys variables to customize storage locations
neonstore::neon_db_dir() # location of duckdb database
database_dir <- file.path(here::here(),"neon","neonstore","database")
if (! file.exists(database_dir)) dir.create(file.path(database_dir), recursive=TRUE, 
                                               showWarnings = FALSE)
Sys.setenv(NEONSTORE_DB = database_dir)
neonstore::neon_db_dir()

neonstore::neon_dir() # download and storage location
data_dir <- file.path(here::here(),"neon","neonstore","data")
if (! file.exists(data_dir)) dir.create(file.path(data_dir), recursive=TRUE, 
                                            showWarnings = FALSE)
Sys.setenv(NEONSTORE_HOME = data_dir)
neonstore::neon_dir()

## Unset
#Sys.unsetenv("NEONSTORE_HOME")

figure_dir <- file.path(here::here(),"neon","figures")
if (! file.exists(figure_dir)) dir.create(file.path(figure_dir), recursive=TRUE, 
                                            showWarnings = FALSE)
#--------------------------------------------------------------------------------------------------#


#--------------------------------------------------------------------------------------------------#
products <- neonstore::neon_products()
i <- grepl("Populations", products$themes)
products[i, c("productCode", "productName")]

all_sites <- neonstore::neon_sites()

sites <- c("TALL", "NIWO")
startdate <- "2017-12-01"
enddate <- "2020-01-01"
#--------------------------------------------------------------------------------------------------#


#--------------------------------------------------------------------------------------------------#
### Get NEON data

# fluxes
neonstore::neon_download(product="DP4.00200.001", site = sites, type = "basic", start_date = startdate,
                         end_date = enddate)


#Tair - single aspirated 
neonstore::neon_download(product="DP1.00002.001", site = sites, type = "basic", start_date = startdate,
                         end_date = enddate)

#Tair - triple asperated 
#neonstore::neon_download(product="DP1.00003.001", site = sites, type = "basic", start_date = startdate,
#                         end_date = enddate)

# PAR 
neonstore::neon_download(product="DP1.00024.001", site = sites, type = "basic", start_date = startdate,
                         end_date = enddate)

# Wind
neonstore::neon_download(product="DP1.00001.001", site = sites, type = "basic", start_date = startdate,
                         end_date = enddate)

#--------------------------------------------------------------------------------------------------#


#--------------------------------------------------------------------------------------------------#
### Fluxes
fluxes <- stackFromStore(filepaths=neon_dir(),
                       dpID="DP4.00200.001",
                       site=sites,
                       package="basic",
                       level="dp01",timeIndex=30,
                       var=c("rtioMoleDryCo2","rtioMoleDryH2o",
                             "dlta13CCo2","dlta18OH2o"),
                       nCores = 6)
names(fluxes)
names(fluxes$NIWO)

term <- unlist(strsplit(names(fluxes[[sites[1]]]), split=".", fixed=T))
fluxes$objDesc[which(fluxes$objDesc$Object %in% term),]
fluxes$variables
#unique(fluxes[[sites[1]]]$qfqm.fluxCo2.nsae.qfFinl)
#qfFinl       The final quality flag indicating if the data are valid for the given aggregation period (1=fail, 0=pass)
#--------------------------------------------------------------------------------------------------#

co2Turb A WORK IN PROGRESS
#--------------------------------------------------------------------------------------------------#
## Create some plots
for (i in seq_along(1:2)) {
  png(file = file.path(figure_dir, paste0(sites[i],"_NSAE_CO2_flux_all_dates.png")), 
      height=3000,width=4300, res=340)
  par(mfrow=c(1,1), mar=c(4.5,5.0,1,0.4), oma=c(0.3,0.9,0.3,0.1)) # B, L, T, R
  plot(fluxes[[sites[i]]]$data.co2Stor.rtioMoleDryCo2.mean~fluxes[[sites[i]]]$timeBgn, 
       pch=".", xlab="Date", ylab="Net Surface-Atmosphere CO2 flux (umolCo2 m-2 s-1)",
       main = paste0("NEON_",sites[i]))
  box(lwd=2.2)
  dev.off()
  
  png(file = file.path(figure_dir, paste0(sites[i],"_TURB_CO2_flux_all_dates.png")),
      height=3000, width=4300, res=340)
  par(mfrow=c(1,1), mar=c(4.5,5.0,1,0.4), oma=c(0.3,0.9,0.3,0.1)) # B, L, T, R
  plot(fluxes[[sites[i]]]$data.fluxCo2.turb.flux~fluxes[[sites[i]]]$timeBgn, 
       pch=".", xlab="Date", ylab="Turbulent CO2 flux (umolCo2 m-2 s-1)", ylim=c(-26,26),
       main = paste0("NEON_",sites[i]))
  box(lwd=2.2)
  dev.off()
  
  png(file = file.path(figure_dir, paste0(sites[i],"_NSAE_H2O_flux_all_dates.png")),
      height=3000, width=4300, res=340)
  par(mfrow=c(1,1), mar=c(4.5,5.0,1,0.4), oma=c(0.3,0.9,0.3,0.1)) # B, L, T, R
  plot(fluxes[[sites[i]]]$data.fluxH2o.nsae.flux~fluxes[[sites[i]]]$timeBgn, 
       pch=".", xlab="Date", ylab="Latent Heat Flux (W m-2)", ylim=c(-46,46),
       main = paste0("NEON_",sites[i]))
  box(lwd=2.2)
  dev.off()
  
  png(file = file.path(figure_dir, paste0(sites[i],"_NSAE_SH_flux_all_dates.png")),
      height=3000,width=4300, res=340)
  par(mfrow=c(1,1), mar=c(4.5,5.0,1,0.4), oma=c(0.3,0.9,0.3,0.1)) # B, L, T, R
  plot(fluxes[[sites[i]]]$data.fluxTemp.nsae.flux~fluxes[[sites[i]]]$timeBgn, 
       pch=".", xlab="Date", ylab="Sensible Heat Flux (W m-2)", ylim=c(-90,90),
       main = paste0("NEON_",sites[i]))
  box(lwd=2.2)
  dev.off()
  
  png(file = file.path(figure_dir, paste0(sites[i],"_veloFric_all_dates.png")),
      height=3000, width=4300, res=340)
  par(mfrow=c(1,1), mar=c(4.5,5.0,1,0.4), oma=c(0.3,0.9,0.3,0.1)) # B, L, T, R
  plot(fluxes[[sites[i]]]$data.fluxMome.turb.veloFric~fluxes[[sites[i]]]$timeBgn, 
       pch=".", xlab="Date", ylab="Friction Velocity (m s-1)", ylim=c(0,3.5),
       main = paste0("NEON_",sites[i]))
  box(lwd=2.2)
  dev.off()
  
}
#--------------------------------------------------------------------------------------------------#


#--------------------------------------------------------------------------------------------------#
## Create some plots - apply QC flags
for (i in seq_along(1:2)) { #-- could just make this a function
  
  co2Stor_plot_data <- fluxes[[sites[i]]] %>%
    filter(qfqm.co2Stor.rateRtioMoleDryCo2.qfFinl==0)
  turb_co2_plot_data <- fluxes[[sites[i]]] %>%
    filter(qfqm.fluxCo2.turb.qfFinl==0)
  nsae_h2o_plot_data <- fluxes[[sites[i]]] %>%
    filter(qfqm.fluxH2o.nsae.qfFinl==0)
  
  png(file = file.path(figure_dir, paste0(sites[i],"_NSAE_CO2_flux_all_dates_QC.png")), 
      height=3000,width=4300, res=340)
  par(mfrow=c(1,1), mar=c(4.5,5.0,1,0.4), oma=c(0.3,0.9,0.3,0.1)) # B, L, T, R
  plot(co2Stor_plot_data$data.co2Stor.rateRtioMoleDryCo2.mean~co2Stor_plot_data$timeBgn, 
       pch=".", xlab="Date", ylab="Net Surface-Atmosphere CO2 flux (umolCo2 m-2 s-1)",
       main = paste0("NEON_",sites[i],"_QC"))
  box(lwd=2.2)
  dev.off()
  
  png(file = file.path(figure_dir, paste0(sites[i],"_TURB_CO2_flux_all_dates_QC.png")),
      height=3000, width=4300, res=340)
  par(mfrow=c(1,1), mar=c(4.5,5.0,1,0.4), oma=c(0.3,0.9,0.3,0.1)) # B, L, T, R
  plot(turb_co2_plot_data$data.fluxCo2.turb.flux~turb_co2_plot_data$timeBgn, 
       pch=".", xlab="Date", ylab="Turbulent CO2 flux (umolCo2 m-2 s-1)", ylim=c(-26,26),
       main = paste0("NEON_",sites[i],"_QC"))
  box(lwd=2.2)
  dev.off()
  
  png(file = file.path(figure_dir, paste0(sites[i],"_NSAE_H2O_flux_all_dates_QC.png")),
      height=3000, width=4300, res=340)
  par(mfrow=c(1,1), mar=c(4.5,5.0,1,0.4), oma=c(0.3,0.9,0.3,0.1)) # B, L, T, R
  plot(nsae_h2o_plot_data$data.fluxH2o.nsae.flux~nsae_h2o_plot_data$timeBgn, 
       pch=".", xlab="Date", ylab="Latent Heat Flux (W m-2)", ylim=c(-46,46),
       main = paste0("NEON_",sites[i]))
  box(lwd=2.2)
  dev.off()
  
  
  rm(nsae_co2_plot_data,turb_co2_plot_data,nsae_h2o_plot_data)
}

#--------------------------------------------------------------------------------------------------#


#--------------------------------------------------------------------------------------------------#
## Create some plots - apply QC flags
for (i in seq_along(1:2)) { #-- could just make this a function
  
  plot_data <- fluxes[[sites[i]]] %>%
    filter(qfqm.co2Stor.rateRtioMoleDryCo2.qfFinl==1) %>%
    filter(timeBgn >= as.POSIXct("2019-07-01", tz="GMT")) %>%
    filter(timeBgn <= as.POSIXct("2019-07-30", tz="GMT"))
  
  png(file = file.path(figure_dir, paste0(sites[i],"_NSAE_CO2_flux_July2019_QC.png")), 
      height=3000,width=4300, res=340)
  par(mfrow=c(1,1), mar=c(4.5,5.0,1,0.4), oma=c(0.3,0.9,0.3,0.1)) # B, L, T, R
  plot(plot_data$data.co2Stor.rateRtioMoleDryCo2.mean~plot_data$timeBgn, 
       pch=20, xlab="Date", ylab="Net Surface-Atmosphere CO2 flux (umolCo2 m-2 s-1)", 
       main = paste0("NEON_",sites[i],"_QC"))
  box(lwd=2.2)
  dev.off()
  
  rm(plot_data) 
}
#--------------------------------------------------------------------------------------------------#


#--------------------------------------------------------------------------------------------------#
tall_wind_data <- neonUtilities::stackFromStore(filepaths=neon_dir(), 
                       dpID="DP1.00001.001", 
                       site=sites[[1]],
                       package="basic", 
                       level="dp04")
names(tall_wind_data)


# flux <- stackFromStore(filepaths=neon_dir(), 
#                        dpID="DP4.00200.001", 
#                        site="WREF",
#                        startdate="2019-04",
#                        enddate="2019-05",
#                        package="basic", 
#                        level="dp04")


#--------------------------------------------------------------------------------------------------#


#--------------------------------------------------------------------------------------------------#


#--------------------------------------------------------------------------------------------------#
tall_wind_data.2m.top <- tall_wind_data$twoDWSD_2min[which(tall_wind_data$twoDWSD_2min$verticalPosition==
                                                           max(tall_wind_data$twoDWSD_2min$verticalPosition)),]
names(tall_wind_data.2m.top)
unique(tall_wind_data$twoDWSD_2min$verticalPosition)

tall_wind_data.2m.top <- na.omit(tall_wind_data.2m.top)

tall_wind_data.2m.top$Month <- as.numeric(format(tall_wind_data.2m.top$startDateTime, format="%m"))
unique(tall_wind_data.2m.top$Month)

tall_wind_data.2m.top <- tall_wind_data.2m.top %>%
  mutate(Season = case_when(
    Month %in%  9:11 ~ "SON",
    Month %in%  c(12, 1, 2)  ~ "DJF",
    Month %in%  3:5  ~ "MAM",
    TRUE ~ "JJA"))
head(tall_wind_data.2m.top)
unique(tall_wind_data.2m.top$Season)
names(tall_wind_data.2m.top)


tall_wind_data.2m.top$tempdate <- format(tall_wind_data.2m.top$startDateTime, format="%d %B %Y")
dates <- data.frame(tall_wind_data.2m.top$tempdate)
#dates <- format(tall_wind_data.2m.top$startDateTime, format="%d %B %Y")
temp <- openair::cutData(dates, type = "season", local.tz ="GMT")
tall_wind_data.2m.top$season <- temp$season
unique(tall_wind_data.2m.top$season)
rm(dates, temp)





tall_wind_data.30m.top <- tall_wind_data$twoDWSD_30min[which(tall_wind_data$twoDWSD_30min$verticalPosition==
                                                               max(tall_wind_data$twoDWSD_30min$verticalPosition)),]
names(tall_wind_data.30m.top)
tall_wind_data.30m.top$tempdate <- format(tall_wind_data.30m.top$startDateTime, format="%d %B %Y")
dates <- data.frame(tall_wind_data.30m.top$tempdate)
temp <- openair::cutData(dates, type = "season", local.tz ="GMT")
tall_wind_data.30m.top$season <- temp$season
rm(dates, temp)


png(file = file.path(figure_dir, "TALL_2min_40m_WindRose_all_dates.png"),height=3000,
    width=4300, res=340)
openair::windRose(tall_wind_data.2m.top,ws="windSpeedMean",wd="windDirMean")
dev.off()

png(file = file.path(figure_dir, "TALL_2min_40m_WindRose_seasonal_all_dates.png"),height=3200,
    width=4700, res=400)
openair::windRose(tall_wind_data.2m.top,ws="windSpeedMean",wd="windDirMean",
                  type = "Season")
dev.off()







png(file = file.path(figure_dir, "TALL_30min_40m_WindRose_seasonal_all_dates.png"),height=3000,
    width=4300, res=340)
openair::windRose(tall_wind_data.2m.top,ws="windSpeedMean",wd="windDirMean",
                  type = tall_wind_data.30m.top$season)
dev.off()






# with clifro
with(tall_wind_data.2m.top, clifro::windrose(windSpeedMean, windDirMean))

with(tall_wind_data.2m.top, clifro::windrose(windSpeedMean, windDirMean,
                       speed_cuts = c(1, 2, 3, 4, 6, 12, 18),
                       legend_title = "Wind Speed\n(m/s)",
                       legend.title.align = .5,
                       ggtheme = "bw",
                       col_pal = "GnBu"))

with(tall_wind_data.2m.top, clifro::windrose(windSpeedMean, windDirMean,season, n_col = 2,
                                             speed_cuts = c(1, 2, 3, 4, 6, 12, 18),
                                             legend_title = "Wind Speed\n(m/s)",
                                             legend.title.align = .5,
                                             ggtheme = "bw",
                                             col_pal = "GnBu"))

#library(ggplot2)
#ggsave("my_windrose.png")
#--------------------------------------------------------------------------------------------------#





