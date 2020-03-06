#Regression matrix

gc()
rm(list=ls())

setwd("D:/Carto_Chap2/")

library(sp)
library(raster)
library(rgdal)
library(aqp)
library(GSIF)
library(stringr)

dat<-readRDS("./Data/CARBOSOL_PI.rds")
co<-readRDS("./Data/SOC_PI.rds")

#####Extract values from covariates to sample locations######
d1<-co
coordinates(d1) <- ~ Long + Lat 
proj4string(d1) <- CRS("+proj=longlat +datum=WGS84") #all the data in the same CRS

###Tiles format (.tif) COV
  #The nationalwide scene includes 6 tiles
COV_T.tif.list <- list.files(path="./COV_Tiles/", pattern="*.tif$", full.names=T,recursive=F)
lis<-COV_T.tif.list
lis1<-lis[seq(1,length(lis),by=6)]
nam<-list()
    #Generate a vector of cov names and an empty data.frame with SOC and COV as column names
    for (i in 1:length(lis1)){
      colnam<-str_sub(lis1[i],13,-13)
      nam<-c(nam,colnam)
      print (i)
      }
df1<- setNames(data.frame(matrix(ncol = (length(lis)/6), nrow = 0)), nam)
df<-cbind(CO=numeric(),Depth_top=numeric, Depth_bot=numeric(), ID_PROF= numeric(), Id_hz=numeric(),x=numeric(),y=numeric(),df1)
names_df<-names(df)
 
    #extract COV over soil samples
    for(j in 1:6){
    lis2<-lis[seq(j,length(lis),by=6)]
    s<-stack(lis2)
    proj4string(d1) <- projection(s)
    ov<-cbind(data.frame(d1@data, d1@coords,extract(s,d1)))
    ov <- ov[complete.cases(ov),]
    colnames(ov)<-names_df
    df <- rbind(df, ov)
    print(j)
  }


  ##Nationwide COV
COV_Nw.tif.list<-list.files(path="./COV_Nw/", pattern="*.tif$", full.names=T,recursive=F)
lis3<-COV_Nw.tif.list
COV_Nw<-stack(lis3)
proj4string(d1) <- projection(s)
df2 <- cbind(d1@data, d1@coords, extract(COV_Nw, d1))
df2 <- df2[complete.cases(df2),]
    #merge df y df2 en base a las coordenadas xy


  ##Categorical format cov

lis<-readRDS("G:/Carto_Chap2/CLC/lisCLCmap_dummy_WGS84.rds")
saveRDS(dat_subset_TOP, "G:/Carto_Chap2/prueba/dat_subset_TOP.rds")  

#Para colgarlo en GIT, dividir en: DataBase preparation, regression matrix, cov preparation (dummy), prediction matrix

