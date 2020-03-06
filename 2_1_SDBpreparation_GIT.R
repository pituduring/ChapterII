##CHAPTER II: SOC MAP OF SPAIN (8 de octubre de 2019)
#CARBON SOIL DATA BASE FROM PROFILE SAMPLES

gc()
rm(list=ls())

setwd("D:/Carto_Chap2/")

##REGRESSION MATRIX
library(raster)
library(GSIF)
library(aqp)
library(plyr)
library(rgdal)
library(sp)
library(gstat)
library(PerformanceAnalytics)

#JOIN PROFILE AND HORIZON DATABASES

dat1=read.table("CARBOSOL_profileV2.tab", sep="\t", header=T, dec=".")
dat2=read.table("CARBOSOL_horizonsV2.tab", sep='\t', header=T, dec=".") 
dat_t <- merge(dat2,dat1,by.x=names(dat2[8]), by.y=names(dat1[1]),all=FALSE)

saveRDS(dat, "./Data/CARBOSOL_tot.rds")
dat_t <- readRDS("./Data/CARBOSOL_tot.rds")

#To start once soil dat is merged:
#dat <- readRDS("D:/_Pitu/_Tesis2016/_Tesis_PD/Bases de datos/_3Spain/_BD/datasets/CARBOSOL_tot.rds")

names(dat_t)

#Samples in Penisnula Iberica only (PI)
#^ character for beginning of line/string:
dat_PI<-dat_t[!grepl("^BALEARS", dat$Province.x), ]

#Profile carbon database to odel
co=data.frame(co=dat_PI$TOC...., Lat=dat_PI$Latitude.x, Long=dat_PI$Longitude.x, Depth_top=dat_PI$Depth.top..m., Depth_bot=dat_PI$Depth.bot..m., ID_PROF=as.character(dat_PI$Sample.ID..Unique.identification.number......1), Id_hz=as.character(dat_PI$Position..Horizon.position.in.the.soil.....))

co=na.omit(co) #problem when the order is inverted
co[co$co<=0.0,]<-NA
co=na.omit(co)

checkdata(dataframe = co)
co <- co[complete.cases(co),]
############################ DESCRIPTIVE STATISTICS ############################

summary(co)

#Carbon distribution by Histogram
hist (co$co) 
hist(log(co$co)) #De esta forma se ve mejor la distribucion de los datos

#Standard deviation
sd(co$co)
sd(na.omit(co$co)) 
#[1] 2.934578

#Carbon distribution by boxplot
boxplot(co$co ~ (co$Id_hz))
boxplot(log(co$co) ~ co$Id_hz) 

############################ "soilProfileCollection" ############################
names(co)
dat_aqp <- co
dat_aqp$ID_PROF <- as.character(dat_aqp$ID_PROF)
depths(dat_aqp) <- ID_PROF ~ Depth_top + Depth_bot
site(dat_aqp) <- ~ Long + Lat 
coordinates(dat_aqp) <- ~ Long + Lat 
proj4string(dat_aqp) <- CRS("+proj=longlat +datum=WGS84")

#Interpolating Carbon density values by GlobalSoilMap.net specifications

try(OCS <- mpspline(dat_aqp, 'co'))

###ERROR!!
