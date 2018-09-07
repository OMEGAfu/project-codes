## If gibberish, please select "Reopen with Encoding..." to UTF-8.
remove(list=ls())  # 清除所有變數設定

library(rgeos)          ## shapefile/TIFF GIS
library(rgdal)          ## shapefile/TIFF GIS
library(sp)             ## Data management
library(spdep)          ## shapefile GIS
library(RColorBrewer)

####[[1]]民99普查資料 ####
floder <- paste0("D:/Demography_Data/CensusData/")
files <- "99年常住人口數及人口密度(含外籍勞工)統計_鄉鎮市區.csv"
path <- paste0(floder, files)  # 常住人口密度
X <- read.csv(path, header=TRUE, stringsAsFactors=FALSE)
## sum(X$縣市名稱=="桃園縣")
substr(X$鄉鎮市區名稱[X$縣市名稱=="桃園縣"], start=3, stop=3) <- "區"  # 直接修改桃園鄉鎮第三字為區
X$縣市名稱[X$縣市名稱=="桃園縣"] <- "桃園市"  # 桃園縣改桃園市
X$Name <- paste0(X$縣市名稱, X$鄉鎮市區名稱)
X$Name[X$Name=="彰化縣員林鎮"] <- "彰化縣員林市"
X$Name[X$Name=="苗栗縣頭份鎮"] <- "苗栗縣頭份市"
X.tw <- subset(X, X$縣市名稱!="金門縣" & X$縣市名稱!="連江縣")
X.data <- X.tw[,c("Name","常住人口數.總計","土地面積","人口密度")]
colnames(X.data) <- c("Name", "P_CNT.cen.all", "AREA", "P_DEN.cen.all")
write.csv(X.data, paste0("D:/Rresult/CensusData/PopDensity.csv"), row.names=F)

####[[2]]鄉鎮行政區圖層 (shp.sub) ####
## Import shapefile Data
setCPLConfigOption("SHAPE_ENCODING","")   # 讓中文不顯示亂碼
tw.poly <- readOGR("D:\\TaiwanMapsShapefiles\\Town\\Town_MOI_1070330.shp", stringsAsFactors=F)
tw.poly@data$Name <- paste0(tw.poly@data$COUNTYNAME, tw.poly@data$TOWNNAME)  # 將縣市鄉鎮名字無空格相接
shp.sub <- subset(tw.poly, tw.poly$COUNTYNAME!="金門縣" & tw.poly$COUNTYNAME!="連江縣")  # 去掉金馬地區

shp.new <- merge(shp.sub, X.data, by="Name", sort=F)  # sort=F is critical!!!!

grps <- 9
brks <- quantile(shp.new$P_CNT.cen.all+0.01, 0:(grps-1)/(grps-1), na.rm=TRUE)  # +0.1這樣100%才有顏色
windows()
spplot(shp.new, "P_CNT.cen.all", at=brks, col.regions=brewer.pal(grps,"Reds"), col="black", 
       xlim=c(119,122.5), ylim=c(21.5,25.5))

grps <- 9
brks <- quantile(shp.new$P_DEN.cen.all+0.01, 0:(grps-1)/(grps-1), na.rm=TRUE)  # +0.01這樣100%才有顏色
windows()
spplot(shp.new, "P_DEN.cen.all", at=brks, col.regions=brewer.pal(grps,"Reds"), col="black", 
       xlim=c(119,122.5), ylim=c(21.5,25.5))