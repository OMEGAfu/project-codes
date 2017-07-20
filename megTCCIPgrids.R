remove(list=ls())           # 清除所有變數設定

# 若要讀多個資料夾下檔案的方法
#path <- "C:/TCCIP_Data/01_OBS_Grid/TCCIP_v3/rain_5km-v3/"
#files <- list.files(path=path, pattern="*.csv")
#names.path <- paste0(path, files)

# 資料有column未命名造成的讀取問題
# 解法參照 https://stackoverflow.com/questions/25771071/r-read-csv-more-columns-than-column-names-error
ind <- list(31:39, 46:54)  # yr1988-1996, yr2003-2011
for (i in 1:2) {
  dat <- readLines("C:/TCCIP_Data/01_OBS_Grid/TCCIP_v3/rain_5km-v3/TCCIP_rain_5km_1960-2012_winter12-02.csv")
  file <- read.csv(textConnection(paste0(dat, collapse="\n")), header=FALSE, stringsAsFactors=FALSE)[-1,c(-1,-2,-3,-59)]
  colnames(file) <- c("lon","lat", paste0("yr",1960:2012))
  prec.win <- round(apply(file[,ind[[i]]], 1, mean, na.rm=TRUE), 3)
  prec.win <- cbind(file[,1:2], prec.win)
  prec.win[prec.win==-99.9] <- NA
  
  dat <- readLines("C:/TCCIP_Data/01_OBS_Grid/TCCIP_v3/rain_5km-v3/TCCIP_rain_5km_1960-2012_summer06-08.csv")
  file <- read.csv(textConnection(paste0(dat, collapse="\n")), header=FALSE, stringsAsFactors=FALSE)[-1,c(-1,-2,-3,-59)]
  colnames(file) <- c("lon","lat", paste0("yr",1960:2012))
  prec.sum <- round(apply(file[,ind[[i]]], 1, mean, na.rm=TRUE), 3)
  prec.sum <- cbind(file[,1:2], prec.sum)
  prec.sum[prec.sum==-99.9] <- NA
  
  dat <- readLines("C:/TCCIP_Data/01_OBS_Grid/TCCIP_v4/tavg_5km-v4/TCCIP_tavg_5km_1960-2012_Ann.csv")
  file <- read.csv(textConnection(paste0(dat, collapse="\n")), header=FALSE, stringsAsFactors=FALSE)[-1,c(-1,-2,-3,-59)]
  colnames(file) <- c("lon","lat", paste0("yr",1960:2012))
  tavg.ann <- round(apply(file[,ind[[i]]], 1, mean, na.rm=TRUE), 3)
  tavg.ann <- cbind(file[,1:2], tavg.ann)
  tavg.ann[tavg.ann==-99.9] <- NA
  
  dat <- readLines("C:/TCCIP_Data/01_OBS_Grid/TCCIP_v4/tmax_5km-v4/TCCIP_tmax_5km_1960-2012_JJA.csv")
  file <- read.csv(textConnection(paste0(dat, collapse="\n")), header=FALSE, stringsAsFactors=FALSE)[-1,c(-1,-2,-3,-59)]
  colnames(file) <- c("lon","lat", paste0("yr",1960:2012))
  tmax.sum <- round(apply(file[,ind[[i]]], 1, mean, na.rm=TRUE), 3)
  tmax.sum <- cbind(file[,1:2], tmax.sum)
  tmax.sum[tmax.sum==-99.9] <- NA
  
  dat <- readLines("C:/TCCIP_Data/01_OBS_Grid/TCCIP_v4/tmin_5km-v4/TCCIP_tmin_5km_1960-2012_DJF.csv")
  file <- read.csv(textConnection(paste0(dat, collapse="\n")), header=FALSE, stringsAsFactors=FALSE)[-1,c(-1,-2,-3,-59)]
  colnames(file) <- c("lon","lat", paste0("yr",1960:2012))
  tmin.win <- round(apply(file[,ind[[i]]], 1, mean, na.rm=TRUE), 3)
  tmin.win <- cbind(file[,1:2], tmin.win)
  tmin.win[tmin.win==-99.9] <- NA
  
  weather <- Reduce(function(x,y) cbind(x,y[,3]), list(tavg.ann, tmax.sum, tmin.win, prec.sum, prec.win))
  colnames(weather)[4:7] <- c("tmax.sum", "tmin.win", "prec.sum", "prec.win")
  if (i==1) {
    pts.data1 <- weather[complete.cases(weather), ]
    pts.data1$lon <- as.numeric(pts.data1$lon)
    pts.data1$lat <- as.numeric(pts.data1$lat)
  }
  if (i==2) {
    pts.data2 <- weather[complete.cases(weather), ]
    pts.data2$lon <- as.numeric(pts.data2$lon)
    pts.data2$lat <- as.numeric(pts.data2$lat)
  }
}

# Getting the height information from DEM and tranform to the grids
library(raster)
library(sp)
library(rgdal)

setwd("D:/20120201_twdtm_asterV2_30m/tif file")
DEM <- raster("twdtm_asterV2_30m.tif")
agDEM <- aggregate(DEM, fact=10, fun=mean)

# extract specific values from a DEM
ddf <- rasterToPoints(agDEM)
ddf <- as.data.frame(ddf)
colnames(ddf) <- c("lon","lat","alt")
ddf <- round(ddf, 2)
aggddf <- aggregate(alt ~ lon + lat, data=ddf, mean)

# restrict to .05deg to .05deg
aggddf$lon.05 <- round(aggddf$lon*100/5)*5/100
aggddf$lat.05 <- round(aggddf$lat*100/5)*5/100
aggddf$alt <- round(aggddf$alt, 2)
subddf <- subset(aggddf, select=c(lon.05, lat.05, alt))
subddf <- aggregate(alt ~ lon.05 + lat.05, data=subddf, median)
colnames(subddf) <- c("lon","lat","alt")
# Go through each row and determine if a value is zero
row_sub = apply(subddf, 1, function(row) all(row !=0 ))
subddf <- subddf[row_sub,]

# Merge data to TCCIP grids
pts.data1 <- merge(pts.data1, subddf, by=c("lon","lat"), all.x=TRUE)
pts.data2 <- merge(pts.data2, subddf, by=c("lon","lat"), all.x=TRUE)
pts.data1$alt[is.na(pts.data1$alt)] <- 0.01
pts.data2$alt[is.na(pts.data2$alt)] <- 0.01