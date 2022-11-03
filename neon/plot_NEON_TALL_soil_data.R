####################################################################################################
#
#  	--- Last updated:  03.09.2022 BY Shawn Serbin <sserbin@bnl.gov>
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

list.of.packages <- c("dplyr","here","ggplot2","gridExtra","neonUtilities", 
                      "reshape2", "plotrix")
invisible(lapply(list.of.packages, library, character.only = TRUE))

here::here()

cleanup <- FALSE
download_data <- FALSE
extract_data <- FALSE
#--------------------------------------------------------------------------------------------------#


#--------------------------------------------------------------------------------------------------#
### Get NEON data
site <- "TALL"
output_data_dir <- file.path(here::here(),"neon","soil",site)
if (! file.exists(output_data_dir)) dir.create(file.path(output_data_dir),
                                               recursive=TRUE, showWarnings = FALSE)
if (! file.exists(file.path(output_data_dir,"figs"))) dir.create(file.path(file.path(output_data_dir,"figs")),
                                               recursive=TRUE, showWarnings = FALSE)

# cleanup before continuing
if (cleanup) {
  unlink(file.path(output_data_dir,"filesToStack00041"), recursive = T)
  unlink(file.path(output_data_dir,"filesToStack00094"), recursive = T)
}

if (download_data) {
  zipsByProduct(dpID="DP1.00041.001", package="basic", 
                site=site,
                startdate="2019-04", enddate="2022-01",
                savepath=file.path(output_data_dir), 
                check.size=F)
  zipsByProduct(dpID="DP1.00094.001", package="basic", 
                site=site,
                startdate="2019-04", enddate="2022-01",
                savepath=file.path(output_data_dir), 
                check.size=F)
  
} else {
  print("*** Data already downloaded ***")
}

if (extract_data) {
  stackByTable(filepath=file.path(output_data_dir,"filesToStack00041"),
               nCores = 8)
  stackByTable(filepath=file.path(output_data_dir,"filesToStack00094"),
               nCores = 8)
} else {
  print("*** Data already extracted ***")
}
#--------------------------------------------------------------------------------------------------#


#--------------------------------------------------------------------------------------------------#
### extract data
soil_temperature_data <- readTableNEON(dataFile=file.path(output_data_dir,"filesToStack00041/stackedFiles/ST_30_minute.csv"), 
                                    varFile=file.path(output_data_dir,"filesToStack00041/stackedFiles/variables_00041.csv"))
soil_moisture_data <- readTableNEON(dataFile=file.path(output_data_dir,"filesToStack00094/stackedFiles/SWS_30_minute.csv"), 
                           varFile=file.path(output_data_dir,"filesToStack00094/stackedFiles/variables_00094.csv"))
names(soil_moisture_data)
head(soil_moisture_data)
#--------------------------------------------------------------------------------------------------#


#--------------------------------------------------------------------------------------------------#
# prep data for plotting

#--------------------------------------------------------------------------------------------------#
moist_to_melt <- soil_moisture_data[,c(3:5,7)]
head(moist_to_melt)

moist_to_melt2 <- moist_to_melt %>%
  filter(lubridate::year(startDateTime)==2020)
head(moist_to_melt2)

moist_to_melt2$Month <- lubridate::month(moist_to_melt2$startDateTime)
head(moist_to_melt2)


p1 <- ggplot(moist_to_melt2, aes(x=as.factor(Month), y=VSWCMean, fill=horizontalPosition)) + 
  geom_boxplot() + facet_wrap(~horizontalPosition)
p1

png(file=file.path(file.path(output_data_dir,"figs"),'NEON_TALL_VWC_bp_by_soil_position.png'),
    height=1900,width=3400, res=280)
par(mfrow=c(1,1), mar=c(4.5,4.5,2,0.4), oma=c(0.3,0.9,0.3,0.1)) # B, L, T, R
p1 <- ggplot(moist_to_melt2, aes(x=as.factor(Month), y=VSWCMean, fill=horizontalPosition)) + 
  geom_boxplot() + labs(x="Month (2020)", y="VSWCMean (%)") + 
  theme(axis.text=element_text(size=18),legend.position="bottom",
        legend.text = element_text(colour="black", size=12, face="bold"),
        axis.title=element_text(size=20,face="bold"), 
        panel.border = element_rect(linetype = "solid", fill = NA, size=1.5),
        axis.text.x = element_text(angle = 0, hjust = 1) )
p1
#y=expression(paste(LMA," (",g~m^{-2},")"))
dev.off()

moist_to_melt3 <- moist_to_melt2 %>%
  filter(verticalPosition=="501")
png(file=file.path(file.path(output_data_dir,"figs"),'NEON_TALL_VWC_vertPos501_bp_by_soil_position.png'),
    height=1900,width=3400, res=280)
par(mfrow=c(1,1), mar=c(4.5,4.5,2,0.4), oma=c(0.3,0.9,0.3,0.1)) # B, L, T, R
p1 <- ggplot(moist_to_melt3, aes(x=as.factor(Month), y=VSWCMean, fill=horizontalPosition)) + 
  geom_boxplot() + labs(x="Month (2020)", y="VSWCMean (%)") + 
  theme(axis.text=element_text(size=18),legend.position="bottom",
        legend.text = element_text(colour="black", size=12, face="bold"),
        axis.title=element_text(size=20,face="bold"), 
        panel.border = element_rect(linetype = "solid", fill = NA, size=1.5),
        axis.text.x = element_text(angle = 0, hjust = 1) )
p1
#y=expression(paste(LMA," (",g~m^{-2},")"))
dev.off()

moist_to_melt3 <- moist_to_melt2 %>%
  filter(verticalPosition=="502")
png(file=file.path(file.path(output_data_dir,"figs"),'NEON_TALL_VWC_vertPos502_bp_by_soil_position.png'),
    height=1900,width=3400, res=280)
par(mfrow=c(1,1), mar=c(4.5,4.5,2,0.4), oma=c(0.3,0.9,0.3,0.1)) # B, L, T, R
p1 <- ggplot(moist_to_melt3, aes(x=as.factor(Month), y=VSWCMean, fill=horizontalPosition)) + 
  geom_boxplot() + labs(x="Month (2020)", y="VSWCMean (%)") + 
  theme(axis.text=element_text(size=18),legend.position="bottom",
        legend.text = element_text(colour="black", size=12, face="bold"),
        axis.title=element_text(size=20,face="bold"), 
        panel.border = element_rect(linetype = "solid", fill = NA, size=1.5),
        axis.text.x = element_text(angle = 0, hjust = 1) )
p1
#y=expression(paste(LMA," (",g~m^{-2},")"))
dev.off()


moist_to_melt3 <- moist_to_melt2 %>%
  filter(verticalPosition=="503")
png(file=file.path(file.path(output_data_dir,"figs"),'NEON_TALL_VWC_vertPos503_bp_by_soil_position.png'),
    height=1900,width=3400, res=280)
par(mfrow=c(1,1), mar=c(4.5,4.5,2,0.4), oma=c(0.3,0.9,0.3,0.1)) # B, L, T, R
p1 <- ggplot(moist_to_melt3, aes(x=as.factor(Month), y=VSWCMean, fill=horizontalPosition)) + 
  geom_boxplot() + labs(x="Month (2020)", y="VSWCMean (%)") + 
  theme(axis.text=element_text(size=18),legend.position="bottom",
        legend.text = element_text(colour="black", size=12, face="bold"),
        axis.title=element_text(size=20,face="bold"), 
        panel.border = element_rect(linetype = "solid", fill = NA, size=1.5),
        axis.text.x = element_text(angle = 0, hjust = 1) )
p1
#y=expression(paste(LMA," (",g~m^{-2},")"))
dev.off()


moist_to_melt3 <- moist_to_melt2 %>%
  filter(verticalPosition=="505")
png(file=file.path(file.path(output_data_dir,"figs"),'NEON_TALL_VWC_vertPos505_bp_by_soil_position.png'),
    height=1900,width=3400, res=280)
par(mfrow=c(1,1), mar=c(4.5,4.5,2,0.4), oma=c(0.3,0.9,0.3,0.1)) # B, L, T, R
p1 <- ggplot(moist_to_melt3, aes(x=as.factor(Month), y=VSWCMean, fill=horizontalPosition)) + 
  geom_boxplot() + labs(x="Month (2020)", y="VSWCMean (%)") + 
  theme(axis.text=element_text(size=18),legend.position="bottom",
        legend.text = element_text(colour="black", size=12, face="bold"),
        axis.title=element_text(size=20,face="bold"), 
        panel.border = element_rect(linetype = "solid", fill = NA, size=1.5),
        axis.text.x = element_text(angle = 0, hjust = 1) )
p1
#y=expression(paste(LMA," (",g~m^{-2},")"))
dev.off()

moist_to_melt3 <- moist_to_melt2 %>%
  filter(verticalPosition=="507")
png(file=file.path(file.path(output_data_dir,"figs"),'NEON_TALL_VWC_vertPos507_bp_by_soil_position.png'),
    height=1900,width=3400, res=280)
par(mfrow=c(1,1), mar=c(4.5,4.5,2,0.4), oma=c(0.3,0.9,0.3,0.1)) # B, L, T, R
p1 <- ggplot(moist_to_melt3, aes(x=as.factor(Month), y=VSWCMean, fill=horizontalPosition)) + 
  geom_boxplot() + labs(x="Month (2020)", y="VSWCMean (%)") + 
  theme(axis.text=element_text(size=18),legend.position="bottom",
        legend.text = element_text(colour="black", size=12, face="bold"),
        axis.title=element_text(size=20,face="bold"), 
        panel.border = element_rect(linetype = "solid", fill = NA, size=1.5),
        axis.text.x = element_text(angle = 0, hjust = 1) )
p1
#y=expression(paste(LMA," (",g~m^{-2},")"))
dev.off()


moist_to_melt3 <- moist_to_melt2 %>%
  filter(verticalPosition=="508")
png(file=file.path(file.path(output_data_dir,"figs"),'NEON_TALL_VWC_vertPos508_bp_by_soil_position.png'),
    height=1900,width=3400, res=280)
par(mfrow=c(1,1), mar=c(4.5,4.5,2,0.4), oma=c(0.3,0.9,0.3,0.1)) # B, L, T, R
p1 <- ggplot(moist_to_melt3, aes(x=as.factor(Month), y=VSWCMean, fill=horizontalPosition)) + 
  geom_boxplot() + labs(x="Month (2020)", y="VSWCMean (%)") + 
  theme(axis.text=element_text(size=18),legend.position="bottom",
        legend.text = element_text(colour="black", size=12, face="bold"),
        axis.title=element_text(size=20,face="bold"), 
        panel.border = element_rect(linetype = "solid", fill = NA, size=1.5),
        axis.text.x = element_text(angle = 0, hjust = 1) )
p1
#y=expression(paste(LMA," (",g~m^{-2},")"))
dev.off()


moist_to_melt <- soil_moisture_data[,c(3:5,7)]
head(moist_to_melt)

moist_to_melt2 <- moist_to_melt %>%
  filter(lubridate::year(startDateTime)==2021)
head(moist_to_melt2)

moist_to_melt2$Month <- lubridate::month(moist_to_melt2$startDateTime)
head(moist_to_melt2)


png(file=file.path(file.path(output_data_dir,"figs"),'2021_NEON_TALL_VWC_bp_by_soil_position.png'),
    height=1900,width=3400, res=280)
par(mfrow=c(1,1), mar=c(4.5,4.5,2,0.4), oma=c(0.3,0.9,0.3,0.1)) # B, L, T, R
p1 <- ggplot(moist_to_melt2, aes(x=as.factor(Month), y=VSWCMean, fill=horizontalPosition)) + 
  geom_boxplot() + labs(x="Month (2021)", y="VSWCMean (%)") + 
  theme(axis.text=element_text(size=18),legend.position="bottom",
        legend.text = element_text(colour="black", size=12, face="bold"),
        axis.title=element_text(size=20,face="bold"), 
        panel.border = element_rect(linetype = "solid", fill = NA, size=1.5),
        axis.text.x = element_text(angle = 0, hjust = 1) )
p1
#y=expression(paste(LMA," (",g~m^{-2},")"))
dev.off()



moist_to_melt <- soil_moisture_data[,c(3:5,7)]
head(moist_to_melt)

moist_to_melt2 <- moist_to_melt %>%
  filter(lubridate::year(startDateTime)==2019)
head(moist_to_melt2)

moist_to_melt2$Month <- lubridate::month(moist_to_melt2$startDateTime)
head(moist_to_melt2)

png(file=file.path(file.path(output_data_dir,"figs"),'2019_NEON_TALL_VWC_bp_by_soil_position.png'),
    height=1900,width=3400, res=280)
par(mfrow=c(1,1), mar=c(4.5,4.5,2,0.4), oma=c(0.3,0.9,0.3,0.1)) # B, L, T, R
p1 <- ggplot(moist_to_melt2, aes(x=as.factor(Month), y=VSWCMean, fill=horizontalPosition)) + 
  geom_boxplot() + labs(x="Month (2019)", y="VSWCMean (%)") + 
  theme(axis.text=element_text(size=18),legend.position="bottom",
        legend.text = element_text(colour="black", size=12, face="bold"),
        axis.title=element_text(size=20,face="bold"), 
        panel.border = element_rect(linetype = "solid", fill = NA, size=1.5),
        axis.text.x = element_text(angle = 0, hjust = 1) )
p1
#y=expression(paste(LMA," (",g~m^{-2},")"))
dev.off()

#--------------------------------------------------------------------------------------------------#
# prep data for plotting

soil_moisture_data2 <- soil_moisture_data %>%
  filter(horizontalPosition=="001") %>%
  filter(verticalPosition=="501")

plot(soil_moisture_data2$startDateTime,soil_moisture_data2$VSWCMean)

head(soil_moisture_data)
moist_to_melt <- soil_moisture_data[,c(3:5,7)]
head(moist_to_melt)

moist_to_melt2 <- moist_to_melt %>%
  filter(lubridate::year(startDateTime)==2020)
head(moist_to_melt2)

moist_to_melt_stats <- moist_to_melt2 %>%
  group_by(horizontalPosition) %>%
  group_by(lubridate::month(startDateTime)) %>%
  summarize(Mean = mean(VSWCMean, na.rm=TRUE))
head(moist_to_melt_stats)

moist_to_melt_stats <- moist_to_melt2 %>% 
  group_by(Month=lubridate::month(startDateTime), horizontalPosition) %>%
  summarize(monthly_mean = mean(VSWCMean, na.rm=TRUE),
            monthly_sdev = sd(VSWCMean, na.rm=TRUE))
  #summarize_at(month_average = mean(VSWCMean, na.rm=TRUE))
  #summarise_at(vars(height:mass), mean, na.rm = TRUE)
head(moist_to_melt_stats)

#moist_melt <- melt(moist_to_melt, id.vars = c("horizontalPosition","verticalPosition",
#                                              "startDateTime"), measure.vars = "VSWCMean")
moist_melt <- melt(moist_to_melt_stats, id.vars = c("Month","horizontalPosition"), 
                   measure.vars = "monthly_mean")
head(moist_melt)

png(file=file.path(file.path(output_data_dir,"figs"),'NEON_TALL_VWC_by_soil_position.png'),
    height=1900,width=3400, res=280)
par(mfrow=c(1,1), mar=c(4.5,4.5,2,0.4), oma=c(0.3,0.9,0.3,0.1)) # B, L, T, R
dat_to_plot <- moist_to_melt_stats[which(moist_to_melt_stats$horizontalPosition=="001"),]
plotrix::plotCI(dat_to_plot$Month, dat_to_plot$monthly_mean, ylim=c(0,0.45),
                uiw=dat_to_plot$monthly_sdev, liw=dat_to_plot$monthly_sdev, scol="grey60",
                pch=21, pt.bg = alpha("black",0.99), cex = 1.3, xlab="Month (2020)",
                ylab="VWC (0-1)", cex.axis=1.4, cex.lab=2, main = "NEON TALL")
dat_to_plot <- moist_to_melt_stats[which(moist_to_melt_stats$horizontalPosition=="002"),]
plotrix::plotCI(jitter(dat_to_plot$Month, 1.2), dat_to_plot$monthly_mean,
                uiw=dat_to_plot$monthly_sdev, liw=dat_to_plot$monthly_sdev,
                add=TRUE, scol="grey60", pch=21, pt.bg = alpha("blue",0.5), cex = 1.4)
dat_to_plot <- moist_to_melt_stats[which(moist_to_melt_stats$horizontalPosition=="003"),]
plotrix::plotCI(jitter(dat_to_plot$Month, 1.4), dat_to_plot$monthly_mean,
                uiw=dat_to_plot$monthly_sdev, liw=dat_to_plot$monthly_sdev,
                add=TRUE, scol="grey60", pch=21, cex = 1.4, 
                pt.bg = alpha("green", 0.5))
dat_to_plot <- moist_to_melt_stats[which(moist_to_melt_stats$horizontalPosition=="004"),]
plotrix::plotCI(jitter(dat_to_plot$Month, 1.4), dat_to_plot$monthly_mean,
                uiw=dat_to_plot$monthly_sdev, liw=dat_to_plot$monthly_sdev,
                add=TRUE, scol="grey60", pch=21, cex = 1.4, 
                pt.bg = alpha("grey40", 0.5))
dat_to_plot <- moist_to_melt_stats[which(moist_to_melt_stats$horizontalPosition=="005"),]
plotrix::plotCI(jitter(dat_to_plot$Month, 1.2), dat_to_plot$monthly_mean,
                uiw=dat_to_plot$monthly_sdev, liw=dat_to_plot$monthly_sdev,
                add=TRUE, scol="grey60", pch=21, cex = 1.4, 
                pt.bg = alpha("dark red", 0.7))
legend("topright",legend=c("Soil Plot 01","Soil Plot 02","Soil Plot 03",
                           "Soil Plot 04","Soil Plot 05"), 
       col = "black", pch = 21, pt.cex = 2,
       bty = "n", pt.bg = c("black","blue","green","grey60","dark red"))
box(lwd=2.2)
dev.off()


#--------------------------------------------------------------------------------------------------#


#--------------------------------------------------------------------------------------------------#
# prep data for plotting
head(soil_temperature_data)
moist_to_melt <- soil_temperature_data[,c(3:5,7)]
head(moist_to_melt)

moist_to_melt2 <- moist_to_melt %>%
  filter(lubridate::year(startDateTime)==2020)
head(moist_to_melt2)

moist_to_melt_stats <- moist_to_melt2 %>%
  group_by(horizontalPosition) %>%
  group_by(lubridate::month(startDateTime)) %>%
  summarize(Mean = mean(soilTempMean, na.rm=TRUE))
head(moist_to_melt_stats)

moist_to_melt_stats <- moist_to_melt2 %>% 
  group_by(Month=lubridate::month(startDateTime), horizontalPosition) %>%
  summarize(monthly_mean = mean(soilTempMean, na.rm=TRUE),
            monthly_sdev = sd(soilTempMean, na.rm=TRUE))
head(moist_to_melt_stats)



png(file=file.path(file.path(output_data_dir,"figs"),'NEON_TALL_soilT_by_soil_position.png'),
    height=1900,width=3400, res=280)
par(mfrow=c(1,1), mar=c(4.5,4.5,2,0.4), oma=c(0.3,0.9,0.3,0.1)) # B, L, T, R
dat_to_plot <- moist_to_melt_stats[which(moist_to_melt_stats$horizontalPosition=="001"),]
plotrix::plotCI(dat_to_plot$Month, dat_to_plot$monthly_mean, ylim=c(6,30),
                uiw=dat_to_plot$monthly_sdev, liw=dat_to_plot$monthly_sdev, scol="grey60",
                pch=21, pt.bg = alpha("black",0.99), cex = 1.3, xlab="Month (2020)",
                ylab="Tsoil (degC)", cex.axis=1.4, cex.lab=2, main = "NEON TALL")
dat_to_plot <- moist_to_melt_stats[which(moist_to_melt_stats$horizontalPosition=="002"),]
plotrix::plotCI(jitter(dat_to_plot$Month, 1.2), dat_to_plot$monthly_mean,
                uiw=dat_to_plot$monthly_sdev, liw=dat_to_plot$monthly_sdev,
                add=TRUE, scol="grey60", pch=21, pt.bg = alpha("blue",0.5), cex = 1.4)
dat_to_plot <- moist_to_melt_stats[which(moist_to_melt_stats$horizontalPosition=="003"),]
plotrix::plotCI(jitter(dat_to_plot$Month, 1.4), dat_to_plot$monthly_mean,
                uiw=dat_to_plot$monthly_sdev, liw=dat_to_plot$monthly_sdev,
                add=TRUE, scol="grey60", pch=21, cex = 1.4, 
                pt.bg = alpha("green", 0.5))
dat_to_plot <- moist_to_melt_stats[which(moist_to_melt_stats$horizontalPosition=="004"),]
plotrix::plotCI(jitter(dat_to_plot$Month, 1.4), dat_to_plot$monthly_mean,
                uiw=dat_to_plot$monthly_sdev, liw=dat_to_plot$monthly_sdev,
                add=TRUE, scol="grey60", pch=21, cex = 1.4, 
                pt.bg = alpha("grey40", 0.5))
dat_to_plot <- moist_to_melt_stats[which(moist_to_melt_stats$horizontalPosition=="005"),]
plotrix::plotCI(jitter(dat_to_plot$Month, 1.2), dat_to_plot$monthly_mean,
                uiw=dat_to_plot$monthly_sdev, liw=dat_to_plot$monthly_sdev,
                add=TRUE, scol="grey60", pch=21, cex = 1.4, 
                pt.bg = alpha("dark red", 0.7))
legend("topright",legend=c("Soil Plot 01","Soil Plot 02","Soil Plot 03",
                           "Soil Plot 04","Soil Plot 05"), 
       col = "black", pch = 21, pt.cex = 2,
       bty = "n", pt.bg = c("black","blue","green","grey60","dark red"))
box(lwd=2.2)
dev.off()

#--------------------------------------------------------------------------------------------------#
