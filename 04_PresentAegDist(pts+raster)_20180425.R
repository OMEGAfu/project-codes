## If gibberish, please select "Reopen with Encoding..." to UTF-8.
remove(list=ls())  # 清除所有變數設定

####[[0]]環境設定 ####
library(rgdal)         # shapefile data
library(plyr)          # spatial data management
library(sp)            # spatial data management
library(latticeExtra)  # spplot addition
library(raster)        # raster data
library(ggmap)         # get google map
library(ggplot2)       # plot

####[[1]]埃及斑蚊鄉鎮分布列表 (Aeg.dist) ####
Aeg.dist <- read.csv(file="D:/TaiwanMapsShapefiles/AedeAegDist.csv", header=TRUE, sep=",", stringsAsFactors=FALSE)
#str(DF.dist)
Aeg.dist$Name <- paste0(Aeg.dist$County, Aeg.dist$Town)

####[[2]]鄉鎮行政區圖層 (shp.sub) ####
## Import shapefile Data by library(rgdal)
setCPLConfigOption("SHAPE_ENCODING","")   # 讓中文不顯示亂碼
tw.poly <- readOGR("D:/TaiwanMapsShapefiles/Town/Town_MOI_1041215.shp", stringsAsFactors=F)
tw.poly@data$Name <- paste0(tw.poly@data$C_Name, tw.poly@data$T_Name)  # 將縣市鄉鎮名字無空格相接
shp.sub <- subset(tw.poly, tw.poly$C_Name!="金門縣" & tw.poly$C_Name!="連江縣")  # 去掉金馬地區
shp.new <- merge(shp.sub, Aeg.dist, by="Name", sort=F)  # "sort=F"加比較保險，避免使用預設的id合併

####[[3]]取得 TCCIP 的網格點範圍 (pts.data) ####
# http://www.nickeubank.com/wp-content/uploads/2015/10/RGIS2_MergingSpatialData_part1_Joins.html
pts.data <- read.csv(file="D:/Rresult/grids_lonlat.csv", header=TRUE, sep=",", stringsAsFactors=FALSE)

####[[4]]取得鄉鎮區埃及斑蚊分布(<=1000m)圖層 (Aeg1000m) 並轉成網格點資料 (Ae.aegDist) ####
## Import raster Data by library(raster)
setwd("D:/AegDist")  # 設定資料夾
Aeg1000m <- raster("QGIS_AedeAeg_1000m.tif")  # 設定檔案名稱

ddf <- as.data.frame( rasterToPoints(Aeg1000m) )  # 將資料轉成網格輸出
colnames(ddf) <- c("lon","lat","value")  # 將原本的 x, y, QGIS_AedeAeg_1000m 重新命名
table(ddf$value)  # 觀察 value 數值定出有埃及斑蚊分布的數值
ddf$value[ddf$value!=255] <- 1  
ddf$value[ddf$value==255] <- 0  # 255為無分布

ddf <- round(ddf, 2)  # 降低解析度 lon, lat 四捨五入到小數兩位
aggddf <- aggregate(value ~ lon + lat, data=ddf, median)  # 將四捨五入後相同的網格點value取中位數

# 為配合 TCCIP 的網格(每0.05度一個網格點)進行資料合併
aggddf$lon.05 <- round(aggddf$lon*100/5)*5/100   # c.xy -> cxy -> round(cxy/5)
aggddf$lat.05 <- round(aggddf$lat*100/5)*5/100
subddf <- subset(aggddf, select=c(lon.05, lat.05, value))
subddf <- aggregate(value ~ lon.05 + lat.05, data=subddf, mean)
subddf$value <- ceiling(subddf$value)  # 無條件進位
colnames(subddf) <- c("lon","lat","value")

subddf <- merge(pts.data, subddf, by=c("lon","lat"), all.x=TRUE)  # 只取跟 pts.data 網格點相同的位置
subddf$value[is.na(subddf$value)] <- 0
Ae.aegDist <- subddf[apply(subddf, 1, function(row) all(row!=0 )),]  # 只留有埃及斑蚊分布的網格點
write.csv(Ae.aegDist, paste0("D:/Rresult/DataVerify/AegDistGrids.csv"), row.names=F)

# 簡單繪圖檢視，需要 library(sp); library(latticeExtra)
windows()
p <- spplot(shp.new, "Aeg2003_2011", col.regions=c("white","yellow"), col="black", xlim=c(119,122.5), ylim=c(21.5,25.5), colorkey=FALSE)
p + layer(panel.points(lon, lat, col="red", pch=19, cex=0.5), data=Ae.aegDist)

####[[5]]繪製在 Google 地圖上 ####
####--(1) Google 地圖圖層 (gmap) ----####
# 需要 library(ggmap)
style1 <- c(element = "labels", visibility = "off")
style2 <- c("&style=", feature = "road", visibility = "off")
style3 <- c("&style=", feature = "poi.park", visibility = "off")
style <- c(style1, style2, style3)
gmap <- get_googlemap(center= 'Taiwan', zoom = 7, language = "zh-TW", maptype = "roadmap", style = style)  # 去字必須用get_googlemap
# 可參考 https://stackoverflow.com/questions/36367335/ggmap-removing-country-names-from-googles-terrain-map
# style 用法見 https://developers.google.com/maps/documentation/static-maps/
# 複合指令下法 https://stackoverflow.com/questions/41049782/making-multiple-style-references-in-google-maps-api
# 去公路的 API 指令 https://stackoverflow.com/questions/10760813/turn-off-roads-overlay-on-google-maps

####--(2)鄉鎮區界行政區圖層2D (twmap) ----####
twmap <- shp.new
twmap@data$id <- 1:length(twmap)  # 讓地區編ID

twmap.points <- fortify(twmap, region="id")  # 需要 library(ggplot2) 將 shapfile 變成 data frame
twmap.points$long <- twmap.points$long + 0.020
twmap.points$lat  <- twmap.points$lat  - 0.045  # 微調繪行政區圖位置

twmap.df = join(twmap.points, twmap@data, by="id")  # 得到各經緯度下的地理資訊，需要 library(plyr)

####--(3)縣市界行政區圖層2D (cmap) ----####
setCPLConfigOption("SHAPE_ENCODING","")   # 讓中文不顯示亂碼
ctw.poly <- readOGR("D:\\TaiwanMapsShapefiles\\CountyTWD97\\County_MOI_1041005.shp", stringsAsFactors=F)
shp.county <- subset(ctw.poly, ctw.poly$C_Name!="金門縣" & ctw.poly$C_Name!="連江縣")  # 去掉金馬地區
cmap <- shp.county
cmap@data$id <- 1:length(cmap)  # 讓地區編ID

cmap.points <- fortify(cmap, region="id")  # 需要 library(ggplot2) 將 shapfile 變成 data frame
cmap.points$long <- cmap.points$long + 0.020
cmap.points$lat  <- cmap.points$lat  - 0.045  # 微調繪行政區圖位置

cmap.df = join(cmap.points, cmap@data, by="id")  # 得到各經緯度下的地理資訊資料，需要 library(plyr)

####--(4)疊圖 ----####
## Points Distribution Only
ggm <- ggmap(gmap, extent = "device") +  # extent去掉地圖經緯度座標軸
  scale_x_continuous(limits = c(119.2, 122.1), expand = c(0, 0)) +
  scale_y_continuous(limits = c(21.7, 25.4), expand = c(0, 0)) +
  geom_polygon(data = twmap.df, 
               aes(x = long, y = lat, group = group, fill=NA), 
               color = "gray75", size = 0.5) +
  geom_polygon(data = cmap.df, 
               aes(x = long, y = lat, group = group, fill=NA), 
               color = "gray60", size = 0.5) +
  scale_fill_manual(values=NA) +  # fill 在整個繪圖裡只能用一次，即使不填色也必須下指令
  coord_equal() +
  theme(legend.position="none") +  # 不顯示圖示
  geom_point(data=Ae.aegDist, aes(lon+ 0.020, lat- 0.045), colour = "red", cex=1)

png(filename=paste0("D:/Rresult/AegDist(pts).png"), width=630, height=800, res=120)
ggm
dev.off()

## Points + Raster Distribution
ggm <- ggmap(gmap, extent = "device") +  # extent去掉地圖經緯度座標軸
  scale_x_continuous(limits = c(119.2, 122.1), expand = c(0, 0)) +
  scale_y_continuous(limits = c(21.7, 25.4), expand = c(0, 0)) +
  geom_tile(data = ddf,
            aes(x = lon + 0.020, y = lat - 0.045, fill=factor(value)), alpha = 0.5) +  # alpha 是透明度
  geom_polygon(data = twmap.df, 
               aes(x = long, y = lat, group = group, fill=NA), 
               color = "gray75", size = 0.5) +
  geom_polygon(data = cmap.df, 
               aes(x = long, y = lat, group = group, fill=NA), 
               color = "gray60", size = 0.5) +
  scale_fill_manual(name="value", values = c(NA, "yellow")) +  # fill 在整個繪圖裡只能用一次
  coord_equal() +
  theme(legend.position="none") +  # 不顯示圖示
  geom_point(data=Ae.aegDist, aes(lon+ 0.020, lat- 0.045), colour = "red", cex=1)

png(filename=paste0("D:/Rresult/AegDist(pts+raster).png"), width=630, height=800, res=120)
ggm
dev.off()
#### END ####