####################################################################################################
#
#  	--- Last updated:  06.22.2021 BY Shawn Serbin <sserbin@bnl.gov>
####################################################################################################


#--------------------------------------------------------------------------------------------------#
# Close all devices and delete all variables.
rm(list=ls(all=TRUE))   # clear workspace
graphics.off()          # close any open graphics
closeAllConnections()   # close any open connections to files
#--------------------------------------------------------------------------------------------------#


#--------------------------------------------------------------------------------------------------#
### Load libraries
#install.packages(c('BiocManager','neonUtilities'))
#BiocManager::install('rhdf5')

list.of.packages <- c("dplyr","here","ggplot2","gridExtra","neonUtilities")
invisible(lapply(list.of.packages, library, character.only = TRUE))

here::here()

cleanup <- FALSE
download_data <- FALSE
#--------------------------------------------------------------------------------------------------#


#--------------------------------------------------------------------------------------------------#
### Get NEON data
site <- "NIWO"
output_data_dir <- file.path(here::here(),"neon","eddy_covariance",site)
if (! file.exists(output_data_dir)) dir.create(file.path(output_data_dir),recursive=TRUE, showWarnings = FALSE)

# cleanup before continuing
if (cleanup) {
  unlink(file.path(output_data_dir,"filesToStack00200"), recursive = T)
}

if (download_data) {
  zipsByProduct(dpID="DP4.00200.001", package="basic", 
                site=site,
                startdate="2017-12", enddate="2020-01",
                savepath=file.path(output_data_dir), 
                check.size=F)
} else {
  print("*** Data already downloaded ***")
}
#--------------------------------------------------------------------------------------------------#


#--------------------------------------------------------------------------------------------------#
### extract flux data
fluxes <- stackEddy(filepath=file.path(output_data_dir,"filesToStack00200"),
                    level="dp04")
names(fluxes)
head(fluxes)

term <- unlist(strsplit(names(fluxes[[site]]), split=".", fixed=T))
fluxes$objDesc[which(fluxes$objDesc$Object %in% term),]
fluxes$variables
#--------------------------------------------------------------------------------------------------#


#--------------------------------------------------------------------------------------------------#
site_num <- 1

png(file = file.path(output_data_dir, "NSAE_CO2_flux_all_dates.png"),height=3000,
    width=4300, res=340)
par(mfrow=c(1,1), mar=c(4.5,5.0,1,0.4), oma=c(0.3,0.9,0.3,0.1)) # B, L, T, R
plot(fluxes[[site[site_num]]]$data.fluxCo2.nsae.flux~fluxes[[site[site_num]]]$timeBgn, 
     pch=".", xlab="Date", ylab="Net Surface-Atmosphere CO2 flux (umolCo2 m-2 s-1)", ylim=c(-26,26),
     main = "Niwot Ridge")
box(lwd=2.2)
dev.off()

png(file = file.path(output_data_dir, "TURB_CO2_flux_all_dates.png"),height=3000,
    width=4300, res=340)
par(mfrow=c(1,1), mar=c(4.5,5.0,1,0.4), oma=c(0.3,0.9,0.3,0.1)) # B, L, T, R
plot(fluxes[[site[site_num]]]$data.fluxCo2.turb.flux~fluxes[[site[site_num]]]$timeBgn, 
     pch=".", xlab="Date", ylab="Turbulent CO2 flux (umolCo2 m-2 s-1)", ylim=c(-26,26),
     main = "Niwot Ridge")
box(lwd=2.2)
dev.off()

png(file = file.path(output_data_dir, "NSAE_H2O_flux_all_dates.png"),height=3000,
    width=4300, res=340)
par(mfrow=c(1,1), mar=c(4.5,5.0,1,0.4), oma=c(0.3,0.9,0.3,0.1)) # B, L, T, R
plot(fluxes[[site[site_num]]]$data.fluxH2o.nsae.flux~fluxes[[site[site_num]]]$timeBgn, 
     pch=".", xlab="Date", ylab="Latent Heat Flux (W m-2)", ylim=c(-46,46),
     main = "Niwot Ridge")
box(lwd=2.2)
dev.off()

png(file = file.path(output_data_dir, "NSAE_SH_flux_all_dates.png"),height=3000,
    width=4300, res=340)
par(mfrow=c(1,1), mar=c(4.5,5.0,1,0.4), oma=c(0.3,0.9,0.3,0.1)) # B, L, T, R
plot(fluxes[[site[site_num]]]$data.fluxTemp.nsae.flux~fluxes[[site[site_num]]]$timeBgn, 
     pch=".", xlab="Date", ylab="Sensible Heat Flux (W m-2)", ylim=c(-90,90),
     main = "Niwot Ridge")
box(lwd=2.2)
dev.off()

png(file = file.path(output_data_dir, "veloFric_all_dates.png"),height=3000,
    width=4300, res=340)
par(mfrow=c(1,1), mar=c(4.5,5.0,1,0.4), oma=c(0.3,0.9,0.3,0.1)) # B, L, T, R
plot(fluxes[[site[site_num]]]$data.fluxMome.turb.veloFric~fluxes[[site[site_num]]]$timeBgn, 
     pch=".", xlab="Date", ylab="Friction Velocity (m s-1)", ylim=c(0,3.5),
     main = "Niwot Ridge")
box(lwd=2.2)
dev.off()
#--------------------------------------------------------------------------------------------------#


#--------------------------------------------------------------------------------------------------#
fluxes.2 <- fluxes[[site[site_num]]] %>%
  filter(timeBgn >= as.POSIXct("2018-07-01", tz="GMT")) %>%
  filter(timeBgn <= as.POSIXct("2018-07-15", tz="GMT"))

png(file = file.path(output_data_dir, "NSAE_CO2_flux_July2018.png"),height=3000,
    width=4300, res=340)
plot(fluxes.2$data.fluxCo2.nsae.flux~fluxes.2$timeBgn, 
     pch=20, xlab="Date", ylab="Net Surface-Atmosphere CO2 flux (umolCo2 m-2 s-1)", 
     main="Niwot Ridge",
     ylim=c(-24,24), xaxt="n")
axis.POSIXct(1, x=fluxes.2$timeBgn, 
             format="%Y-%m-%d")
box(lwd=2.2)
dev.off()

#--------------------------------------------------------------------------------------------------#


#--------------------------------------------------------------------------------------------------#
### get met data

#PAR/met
pr <- loadByProduct("DP1.00024.001", site=site, 
                    timeIndex=30, package="basic", 
                    startdate="2017-12", enddate="2020-01",
                    check.size=F)

pr.top <- pr$PARPAR_30min[which(pr$PARPAR_30min$verticalPosition==
                                  max(pr$PARPAR_30min$verticalPosition)),]
names(pr.top)

png(file = file.path(output_data_dir, "PAR_all_dates.png"),height=3000,
    width=4300, res=340)
plot(pr.top$PARMean~pr.top$startDateTime, pch=20, xlab="Date", 
     ylab="PAR (umols m-2 s-1)", main="Niwot Ridge")
box(lwd=2.2)
dev.off()

#Tair
Tair <- loadByProduct("DP1.00003.001", site=site, 
                      timeIndex=30, package="basic", 
                      startdate="2017-12", enddate="2020-01",
                      check.size=F)
Tair$sensor_positions_00003
Tair$variables_00003
Tair$TAAT_30min
head(Tair$TAAT_30min)

png(file = file.path(output_data_dir, "Tair_all_dates.png"),height=3000,
    width=4300, res=340)
plot(Tair$TAAT_30min$tempTripleMean~Tair$TAAT_30min$startDateTime, pch=20, xlab="Date", 
     ylab="Tair (deg C)", main="Niwot Ridge")
box(lwd=2.2)
dev.off()
#--------------------------------------------------------------------------------------------------#


#--------------------------------------------------------------------------------------------------#
pr.top$timeBgn <- pr.top$startDateTime
fx.pr <- merge(pr.top, fluxes[[site[site_num]]], by="timeBgn")

png(file = file.path(output_data_dir, "NSAE_CO2_flux_vs_PAR_all_dates.png"),height=3000,
    width=4300, res=340)
plot(fx.pr$data.fluxCo2.nsae.flux~fx.pr$PARMean,
     pch=".", ylim=c(-20,20),
     xlab="PAR (umols m-2 s-1)", ylab="Net Surface-Atmosphere CO2 flux (umolCo2 m-2 s-1)",
     main="Niwot Ridge")
box(lwd=2.2)
dev.off()


Tair$TAAT_30min$timeBgn <- Tair$TAAT_30min$startDateTime
fx.pr.tair <- merge(Tair$TAAT_30min, fluxes[[site[site_num]]], by="timeBgn")

png(file = file.path(output_data_dir, "NSAE_CO2_flux_vs_Tair_all_dates.png"),height=3000,
    width=4300, res=340)
plot(fx.pr.tair$data.fluxCo2.nsae.flux~fx.pr.tair$tempTripleMean,
     pch=".", ylim=c(-35,35),
     xlab="Tair (deg C)", ylab="Net Surface-Atmosphere CO2 flux (umolCo2 m-2 s-1)",
     main="Niwot Ridge")
box(lwd=2.2)
dev.off()

fx.pr.2 <- fx.pr %>%
  filter(timeBgn >= as.POSIXct("2018-07-01", tz="GMT")) %>%
  filter(timeBgn <= as.POSIXct("2018-07-15", tz="GMT"))


png(file = file.path(output_data_dir, "NSAE_CO2_flux_vs_PAR_July2018.png"),height=3000,
    width=4300, res=340)
plot(fx.pr.2$data.fluxCo2.nsae.flux~fx.pr.2$PARMean,
     pch=20, ylim=c(-15,15),
     xlab="PAR (umols m-2 s-1)", ylab="Net Surface-Atmosphere CO2 flux (umolCo2 m-2 s-1)",
     main="Niwot Ridge")
box(lwd=2.2)
box(lwd=2.2)
dev.off()


fx.pr.tair.2 <- fx.pr.tair %>%
  filter(timeBgn >= as.POSIXct("2018-07-01", tz="GMT")) %>%
  filter(timeBgn <= as.POSIXct("2018-07-15", tz="GMT"))

png(file = file.path(output_data_dir, "NSAE_CO2_flux_vs_Tair_July2018.png"),height=3000,
    width=4300, res=340)
plot(fx.pr.tair.2$data.fluxCo2.nsae.flux~fx.pr.tair.2$tempTripleMean,
     pch=20, ylim=c(-35,35),
     xlab="Tair (deg C)", ylab="Net Surface-Atmosphere CO2 flux (umolCo2 m-2 s-1)",
     main="Niwot Ridge")
box(lwd=2.2)
dev.off()

#--------------------------------------------------------------------------------------------------#

