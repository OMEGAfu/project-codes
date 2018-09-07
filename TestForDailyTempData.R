## If gibberish, please select "Reopen with Encoding..." to UTF-8.
remove(list=ls())  # 清除所有變數設定

library(lubridate)     # To perform date, time related functions
library(RColorBrewer)  # To draw colorful
library(ncdf4)         # To access the NetCDF file

####[[1]] 網格化歷史推估日均溫資料 ####
floder <- "D:/TaiPAD_Statistical_201802/daily/rcp85/"  # 資料夾路徑
files <- list.files(path=floder, pattern="tas.*\\.nc")  # 檔案所屬資料夾與類型
path <- paste0(floder, files)

ntime <- vector()
ncfile.list <- list()
for (i in 1:5) {
  ncfile <- nc_open(path[i])
  ntime[i] <- length(ncvar_get(ncfile, "time"))  # 天數
  ncfile.list[[i]] <- ncfile
}
#ntime/95 # 檢查一年長度(2006-2100，共95年)，HadGEM2_CC為360天餘為365天

# 日均溫分成 16-22, 22-28, 28-34 三段溫度計算一年平均天數
#for (i in 1:5) {
  #print(paste0("i=",i))
  i=5
  ncfile <- ncfile.list[[i]]
  time <- ncvar_get(ncfile, "time")
  lon <- ncvar_get(ncfile, "lon")
  lat <- ncvar_get(ncfile, "lat")
  lonlat <- data.frame(as.matrix(expand.grid(lon, lat)))  # 化成二維資料
  colnames(lonlat) <- c("lon", "lat")

  rv <- ncvar_get(ncfile, "tas")
  rv.sub <- rv[,,(365*2+1):(365*12)]  # 因此2008-2017年，是第3年初(365*2+1)到第12年底(365*12)
  rv.1622 <- apply(rv.sub, c(1,2), function(cod) sum(1*(cod>16 & cod <=22))/10 )
  rv.2228 <- apply(rv.sub, c(1,2), function(med) sum(1*(med>22 & med <=28))/10 )
  rv.2834 <- apply(rv.sub, c(1,2), function(hot) sum(1*(hot>28 & hot <=34))/10 )
  #test <- rv.1622 + rv.2228 + rv.2834  #確定沒超過365天

  tas.cool <- matrix(nrow=4860, ncol=1)
  tas.warm <- matrix(nrow=4860, ncol=1)
  tas.hot  <- matrix(nrow=4860, ncol=1)
  
  tas.cool <- as.vector(rv.1622)
  tas.warm <- as.vector(rv.2228)
  tas.hot  <- as.vector(rv.2834)
  
  tas.3cut <- cbind(lonlat, tas.cool, tas.warm, tas.hot)
  tas.3cut$index <- (tas.3cut$tas.cool *1 + tas.3cut$tas.warm *3 + tas.3cut$tas.hot *2) / (6*365)
  tas.index <- tas.3cut[complete.cases(tas.3cut),]
  colnames(tas.index)[1:2] <- c("lon","lat")

### 簡易繪圖觀察
library(ggplot2)
library(viridis)  # 數值漸層著色

perc.rank <- function(x) { floor(rank(x))/length(x) }  # 將數值轉成排序百分位數

value <- perc.rank(tas.index$index)
windows()
ggplot() + 
  geom_raster(data = tas.index, aes(x=lon, y=lat, fill=value)) +
  coord_fixed(ratio = 1) +
  scale_fill_viridis(direction=-1)

value <- perc.rank(tas.index$tas.hot)
windows()
ggplot() + 
  geom_raster(data = tas.index, aes(x=lon, y=lat, fill=value)) +
  coord_fixed(ratio = 1) +
  scale_fill_viridis(direction=-1)

####[[1]]埃及斑蚊鄉鎮分布列表 (Aeg.dist) ####
Aeg.dist <- read.csv(file="D:/TaiwanMapsShapefiles/AedeAegDist.csv", header=TRUE, sep=",", stringsAsFactors=FALSE)
#str(DF.dist)
Aeg.dist$Name <- paste0(Aeg.dist$County, Aeg.dist$Town)

library(glmnet)         ## PLR
library(ggplot2)
library(rgeos)          ## shapefile/TIFF GIS 
library(rgdal)          ## shapefile/TIFF GIS
library(sp)             ## Data management
library(spdep)          ## shapefile GIS (poly2nb)
library(RColorBrewer)
library(plyr)
library(ggmap)
library(latticeExtra)
library(maptools)
library(raster)

####[[2]]鄉鎮行政區圖層 (shp.sub) ####
## Import shapefile Data by library(rgdal)
setCPLConfigOption("SHAPE_ENCODING","")   # 讓中文不顯示亂碼
tw.poly <- readOGR("D:\\TaiwanMapsShapefiles\\Town\\Town_MOI_1070330.shp", stringsAsFactors=F)
tw.poly@data$Name <- paste0(tw.poly@data$COUNTYNAME, tw.poly@data$TOWNNAME)  # 將縣市鄉鎮名字無空格相接
shp.sub <- subset(tw.poly, tw.poly$COUNTYNAME!="金門縣" & tw.poly$COUNTYNAME!="連江縣")  # 去掉金馬地區
shp.new <- merge(shp.sub, Aeg.dist, by="Name", sort=F)  # "sort=F"加比較保險，避免使用預設的id合併

data.verify <- tas.index
pts.data <- data.verify
coordinates(pts.data) <- ~lon + lat
crs <- CRS("+proj=longlat +ellps=GRS80 +no_defs")
proj4string(pts.data)= crs  # 地圖投影設定與shapefile data相同才能讓圖層附加資料訊息

####[[4]]將歷史氣象網格資料對應行政區 (pts.data) ####
pts.data$Name <- over(pts.data, tw.poly)$Name  # 將每一網格點所屬行政區Name加上
#head(pts.data@data)
na.index <- which(is.na(pts.data$Name))         # 找出未對應到行政區的網格點其資料位置
na.pts <- pts.data[na.index, ]                  # 挑出落在行政區外的網格點
#windows()                                     # 確認那些點的位置
#plot(na.pts, pch=16, col="red")                
#plot(shp.sub, add=TRUE)

# 找這些點最接近行政區的方法參考 https://stackoverflow.com/questions/26308426/how-do-i-find-the-polygon-nearest-to-a-point-in-r
# need library(rgeos)
# First project data into a planar coordinate system
poly.UTM <- spTransform(shp.sub, crs)
pts.UTM <- spTransform(na.pts, crs)
# Set up container for results
n <- length(pts.UTM)
nearest <- character(n)
# For each point, find name of nearest polygon
for (i in seq_along(nearest)) {
  index <- which.min(gDistance(pts.UTM[i,], poly.UTM, byid=TRUE))
  nearest[i] <- poly.UTM$Name[index]
}
#投影系統不是平面故會跳出距離計算警告，但在小地圖找最接近點時沒有影響
#nearest   # Check that it worked
pts.data$Name[na.index] <- nearest  # 將最接近行政區的名字填補回去
#head(pts.data@data)

####[[5]]依鄉鎮行政區其內的歷史氣象網格資料 (shp.new) ####
# 確認行政區與點資料形態
#head(shp.sub@data)   # polygons
#head(pts.data@data)  # points
#plot(shp.sub, xlim=c(119,122.5), ylim=c(21.5,25.5))
#points(pts.data, pch=20)

agg.data <- aggregate(.~Name, data=pts.data, median)  # 將行政區內的變數值取中位數
meg.data <- merge(agg.data, Aeg.dist[,-(1:4)], by="Name", all.y=TRUE)  # 合併氣象資料與埃及斑蚊分布資料
shp.new <- merge(shp.sub, meg.data, by="Name", sort=F)  # "sort=F"加比較保險，避免使用預設的id合併
out.data <- shp.new@data[,c("Name","tas.cool","tas.warm","tas.hot","index")]

write.csv(out.data, "D:/Rresult/TempDayData.csv", row.names=F)