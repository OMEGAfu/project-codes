## If gibberish, please select "Reopen with Encoding..." to UTF-8.
remove(list=ls())  # 清除所有變數設定
GCM <- "MPI_ESM_LR"

####[[0]]環境設定 ####
#install.packages("<name>")
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

####[[1]]埃及斑蚊鄉鎮分布列表 (Aeg.dist) ####
Aeg.dist <- read.csv(file="D:\\TaiwanMapsShapefiles\\AedeAegDist.csv", header=TRUE, sep=",", stringsAsFactors=FALSE)
#str(DF.dist)
Aeg.dist$Name <- paste0(Aeg.dist$County, Aeg.dist$Town)

####[[2]]鄉鎮行政區圖層 (shp.sub) ####
## Import shapefile Data
setCPLConfigOption("SHAPE_ENCODING","")   # 讓中文不顯示亂碼
tw.poly <- readOGR("D:\\TaiwanMapsShapefiles\\Town\\Town_MOI_1041215.shp", stringsAsFactors=F)
tw.poly@data$Name <- paste0(tw.poly@data$C_Name, tw.poly@data$T_Name)  # 將縣市鄉鎮名字無空格相接
shp.sub <- subset(tw.poly, tw.poly$C_Name!="金門縣" & tw.poly$C_Name!="連江縣")  # 去掉金馬地區
#mydata <- shp.sub@data  # 檢查屬性資料

####[[3]]將歷史氣象網格資料轉為spatial dataframe (pts.data) ####
# http://www.nickeubank.com/wp-content/uploads/2015/10/RGIS2_MergingSpatialData_part1_Joins.html
data.verify <- read.csv(file="D:/Rresult/seasons_vrf.csv", header=TRUE, sep=",", stringsAsFactors=FALSE)
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

# 利用夏季均溫檢查資料
#range(shp.new@data[,"tavg.sum"], na.rm=T)  # 查詢最大最小值範圍
grps <- 7
brks <- seq(from=16, to=30, by=2)
windows()
spplot(shp.new, "tavg.sum", at=brks, col.regions=brewer.pal(grps,"Reds"), col="black",
       xlim=c(119,122.5), ylim=c(21.5,25.5), main=list(label="夏季(7-9月)日均溫", cex=1.5))
# 可以發現有些行政區沒有值，表示未包含網格點
# 方法一：提高網格解析度；方法二：用附近行政區的平均值填補

# 行政區無網格資料者用地理鄰居的平均值填補
# need library(spdep)
coords <- coordinates(shp.new)
shp.nb <- poly2nb(shp.new)  # list結構，儲存每個行政區id的鄰居id (跟直接用shp.nb@data輸出順序不同)
#which(colnames(shp.new@data) %in% c("tavg.ann","prec.win"))
obs.name <- c(paste0("prec.", c("spr","mei","sum","aut","win","dry","wet","ann")),  # 平均日降雨量
              paste0("tavg.", c("spr","mei","sum","aut","win","dry","wet","ann")),  # 均溫
              paste0("tmax.", c("spr","mei","sum","aut","win","dry","wet","ann")),  # 平均日最高溫
              paste0("tmin.", c("spr","mei","sum","aut","win","dry","wet","ann")),  # 平均日最低溫
              paste0("rday.", c("spr","mei","sum","aut","win","dry","wet","ann")),  # 降雨日數
              "alt")  # 夏季最高溫、冬季最低溫、海拔高度

for (j in 1:length(obs.name) +length(shp.sub@data)) {  # 只處理自變數，length(obs.name)為匯入的氣象變數數量
  rep = 1
  while (rep <=2) {  # 進行兩次地理鄰居值填補，以防第一次抓到有鄰居但鄰居皆無值的點 
    # print(rep)
    for (i in which( is.na(shp.new@data[,j]), arr.ind=TRUE) ) {  # 找有NA值的區域id 
      shp.new@data[i,j]=mean(shp.new@data[shp.nb[[i]],j], na.rm=TRUE)  # 將該區域附近鄰居的數值平均
    }
    rep = rep + 1
  }
}

# 利用夏季均溫檢查資料
grps <- 7
brks <- seq(from=16, to=30, by=2)
windows()
spplot(shp.new, "tavg.sum", at=brks, col.regions=brewer.pal(grps,"Reds"), col="black",
       xlim=c(119,122.5), ylim=c(21.5,25.5), main=list(label="夏季(7-9月)日均溫", cex=1.5))

# 註：若網格點都在行政區範圍內只需要以下步驟(使用package sp裡的over函數)
#shp.sub@data$id <- 1:length(shp.sub)  # 讓地區編ID
#pts.data$id <- over(pts.data, shp.sub)$id  # 將每一網格點所屬行政區id加上
#agg.data <- over(shp.sub, pts.data, fn=median)   # 將行政區內的變數值取中位數
# 如要進階呈現可參考 https://www.nceas.ucsb.edu/scicomp/usecases/point-in-polygon

####[[6]]建立統計分析的資料結構 (AegData, X.Data) ####
AegData <- shp.new@data[,c("Name", "Aeg2003_2011", obs.name)]
colnames(AegData)[2] <- "index"
AegData$index[is.na(AegData$index)] <- 0  
AegData <- AegData[complete.cases(AegData),]  # 澎湖南部離島七美鄉沒有任何氣象觀測值，故在此步驟被刪除

obs.class <- c(paste0("prec.", c("spr","mei","sum","aut","win")),  # 平均日降雨量
               paste0("tavg.", c("spr","mei","sum","aut","win")),  # 均溫
               paste0("rday.", c("spr","mei","sum","aut","win")),  # 降雨日數
               "tmax.sum", "tmin.win", "alt")  # 夏季最高溫、冬季最低溫、海拔高度
X.Data <- as.data.frame(AegData[,obs.class])  # 只有自變數的資料

####--(4)引入推估用資料 (data.base, rcp85.XXXX)--####
# 基期
stat.base <- read.csv(file=paste0("D:/Rresult/DataVerify/weather_base_",GCM,".csv"), header=TRUE, sep=",", stringsAsFactors=FALSE)
# rcp85
rcp85.1635 <- read.csv(file=paste0("D:/Rresult/DataVerify/weather_rcp85_",GCM,"_yr1635.csv"), header=TRUE, sep=",", stringsAsFactors=FALSE)
rcp85.4665 <- read.csv(file=paste0("D:/Rresult/DataVerify/weather_rcp85_",GCM,"_yr4665.csv"), header=TRUE, sep=",", stringsAsFactors=FALSE)
rcp85.8100 <- read.csv(file=paste0("D:/Rresult/DataVerify/weather_rcp85_",GCM,"_yr8100.csv"), header=TRUE, sep=",", stringsAsFactors=FALSE)
rcp85.2C   <- read.csv(file=paste0("D:/Rresult/DataVerify/weather_rcp85_",GCM,"_+2C.csv"), header=TRUE, sep=",", stringsAsFactors=FALSE)

####[[7]]以懲罰羅吉斯回歸進行未來資料預估 ####
####--(1)未來推估結果產出 --####
x.chosen <- c("tmin.win","rday.spr","prec.spr","prec.wet","prec.dry")

x <- as.matrix(AegData[,x.chosen])
y <- AegData[,"index"]
alpha.set <- 0.5
repn = 1000  # 進行 1000 次變數選擇結果統計

set.seed(9527)
for (i in 1:repn)
{
  print(i)
  cvPLR <- cv.glmnet(x, y, family='binomial', type.measure = "class", alpha=alpha.set)
  
  ## 2003-2011
  newx <- as.matrix(data.verify[,x.chosen])
  verify <- as.numeric(predict(cvPLR, newx, s ="lambda.min", type = "class", alpha=alpha.set))
  if (i==1) {vrf.sum = verify} else {vrf.sum = vrf.sum + verify}
  
  ## 基期
  newx <- as.matrix(stat.base[,x.chosen])
  base <- as.numeric(predict(cvPLR, newx, s ="lambda.min", type = "class", alpha=alpha.set))
  if (i==1) {base.sum = base} else {base.sum = base.sum + base}
  
  ## RCP 8.5
  newx <- as.matrix(rcp85.1635[,x.chosen])
  rcp851 <- as.numeric(predict(cvPLR, newx, s ="lambda.min", type = "class", alpha=alpha.set))
  if (i==1) {rcp851.sum = rcp851} else {rcp851.sum = rcp851.sum + rcp851}
  
  newx <- as.matrix(rcp85.4665[,x.chosen])
  rcp852 <- as.numeric(predict(cvPLR, newx, s ="lambda.min", type = "class", alpha=alpha.set))
  if (i==1) {rcp852.sum = rcp852} else {rcp852.sum = rcp852.sum + rcp852}
  
  newx <- as.matrix(rcp85.8100[,x.chosen])
  rcp853 <- as.numeric(predict(cvPLR, newx, s ="lambda.min", type = "class", alpha=alpha.set))
  if (i==1) {rcp853.sum = rcp853} else {rcp853.sum = rcp853.sum + rcp853}
  
  newx <- as.matrix(rcp85.2C[,x.chosen])
  rcp854 <- as.numeric(predict(cvPLR, newx, s ="lambda.min", type = "class", alpha=alpha.set))
  if (i==1) {rcp854.sum = rcp854} else {rcp854.sum = rcp854.sum + rcp854}
}
vrf.out <- cbind(data.verify[,c("lon","lat")], round(vrf.sum/repn))
vrf.out <- vrf.out[apply(vrf.out, 1, function(row) all(row!=0 )),]

base.out <- cbind(stat.base[,c("lon","lat")], round(base.sum/repn))
base.out <- base.out[apply(base.out, 1, function(row) all(row!=0 )),]

rcp851.out <- cbind(rcp85.1635[,c("lon","lat")], round(rcp851.sum/repn))
rcp851.out <- rcp851.out[apply(rcp851.out, 1, function(row) all(row!=0 )),]

rcp852.out <- cbind(rcp85.4665[,c("lon","lat")], round(rcp852.sum/repn))
rcp852.out <- rcp852.out[apply(rcp852.out, 1, function(row) all(row!=0 )),]

rcp853.out <- cbind(rcp85.8100[,c("lon","lat")], round(rcp853.sum/repn))
rcp853.out <- rcp853.out[apply(rcp853.out, 1, function(row) all(row!=0 )),]

rcp854.out <- cbind(rcp85.2C[,c("lon","lat")], round(rcp854.sum/repn))
rcp854.out <- rcp854.out[apply(rcp854.out, 1, function(row) all(row!=0 )),]

####--(3)繪製在 Google 地圖上--####
####----[1] Google 地圖圖層 (gmap) ----####
#gmap <- get_map(location = 'Taiwan', zoom = 7, language = "zh-TW", maptype = "roadmap")
style1 <- c(element = "labels", visibility = "off")
style2 <- c("&style=", feature = "road", visibility = "off")
style3 <- c("&style=", feature = "poi.park", visibility = "off")
#style4 <- c("&style=", feature = "water", visibility = "off")
style <- c(style1, style2, style3)
gmap <- get_googlemap(center= 'Taiwan', zoom = 7, language = "zh-TW", maptype = "roadmap", style = style)  # 去字必須用get_googlemap
# 可參考 https://stackoverflow.com/questions/36367335/ggmap-removing-country-names-from-googles-terrain-map
# style 用法見 https://developers.google.com/maps/documentation/static-maps/
# 複合指令下法 https://stackoverflow.com/questions/41049782/making-multiple-style-references-in-google-maps-api
# 去公路的 API 指令 https://stackoverflow.com/questions/10760813/turn-off-roads-overlay-on-google-maps

####----[2]鄉鎮區埃及斑蚊分布(<=1000m)圖層 (Aeg1000m) ----####
setwd("D:/AegDist")  # 設定資料夾
Aeg1000m <- raster("QGIS_AedeAeg_1000m.tif")  # 設定檔案名稱

Aeg1000m.df <- as.data.frame(as(Aeg1000m, "SpatialPixelsDataFrame"))
colnames(Aeg1000m.df) <- c("value", "long", "lat")
Aeg1000m.df$long <- Aeg1000m.df$long + 0.020
Aeg1000m.df$lat  <- Aeg1000m.df$lat  - 0.045  # 微調分布位置
Aeg1000m.df$value[Aeg1000m.df$value!=255] <- 1
Aeg1000m.df$value[Aeg1000m.df$value==255] <- 0

####----[3]鄉鎮區界行政區圖層2D (twmap) ----####
twmap <- shp.new
twmap@data$id <- 1:length(twmap)  # 讓地區編ID

twmap.points <- fortify(twmap, region="id")  
# fortify from ggplot2 package: shapfile -> data.frame
twmap.points$long <- twmap.points$long + 0.020
twmap.points$lat  <- twmap.points$lat  - 0.045  # 微調繪行政區圖位置

twmap.df = join(twmap.points, twmap@data, by="id")  # 得到各經緯度下的地理資訊

####----[4]縣市界行政區圖層2D (cmap) ----####
setCPLConfigOption("SHAPE_ENCODING","")   # 讓中文不顯示亂碼
ctw.poly <- readOGR("D:\\TaiwanMapsShapefiles\\CountyTWD97\\County_MOI_1041005.shp", stringsAsFactors=F)
shp.county <- subset(ctw.poly, ctw.poly$C_Name!="金門縣" & ctw.poly$C_Name!="連江縣")  # 去掉金馬地區
cmap <- shp.county
cmap@data$id <- 1:length(cmap)  # 讓地區編ID

cmap.points <- fortify(cmap, region="id")  
# fortify from ggplot2 package: shapefile -> data.frame
cmap.points$long <- cmap.points$long + 0.020
cmap.points$lat  <- cmap.points$lat  - 0.045  # 微調繪行政區圖位置

cmap.df = join(cmap.points, cmap@data, by="id")  # 得到各經緯度下的地理資訊資料

####[[A]]修改取用資料 產圖專用 ####
## 將[1][2]都疊到[3]
## vrf.out (有埃及斑蚊現況分布底圖)
ggm <- ggmap(gmap, extent = "device") +  # extent去掉地圖經緯度座標軸
  scale_x_continuous(limits = c(119.2, 122.1), expand = c(0, 0)) +
  scale_y_continuous(limits = c(21.7, 25.4), expand = c(0, 0)) +
  geom_tile(data = Aeg1000m.df,
            aes(x = long, y = lat, fill=factor(value)), alpha = 0.5) +  # alpha 是透明度
  geom_polygon(data = twmap.df, 
               aes(x = long, y = lat, group = group, fill=NA), 
               color = "gray75", size = 0.5) +
  geom_polygon(data = cmap.df, 
               aes(x = long, y = lat, group = group, fill=NA), 
               color = "gray60", size = 0.5) +
  scale_fill_manual(name="value", values = c(NA, "yellow")) +  # fill 在整個繪圖裡只能用一次
  coord_equal() +
  theme(legend.position="none") +  # 不顯示圖示
  geom_point(data=vrf.out, aes(lon+ 0.020, lat- 0.045), colour = "red", cex=1)

png(filename=paste0("D:/Rresult/NewEdition_PLR/M2/rcp85_",GCM,"/0_vrf(2003-2011)+.png"), width=630, height=800, res=120)
ggm
dev.off()

## vrf.out (無埃及斑蚊現況分布底圖)
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
  geom_point(data=vrf.out, aes(lon+ 0.020, lat- 0.045), colour = "red", cex=1)

png(filename=paste0("D:/Rresult/NewEdition_PLR/M2/rcp85_",GCM,"/0_vrf(2003-2011)-.png"), width=630, height=800, res=120)
ggm
dev.off()

## base.out
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
  geom_point(data=base.out, aes(lon+ 0.020, lat- 0.045), colour = "red", cex=1)

png(filename=paste0("D:/Rresult/NewEdition_PLR/M2/rcp85_",GCM,"/1_base(1986-2005).png"), width=630, height=800, res=120)
ggm
dev.off()

## rcp851.out
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
  geom_point(data=rcp851.out, aes(lon+ 0.020, lat- 0.045), colour = "red", cex=1)

png(filename=paste0("D:/Rresult/NewEdition_PLR/M2/rcp85_",GCM,"/2_rcp85(2016-2035).png"), width=630, height=800, res=120)
ggm
dev.off()

## rcp852.out
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
  geom_point(data=rcp852.out, aes(lon+ 0.020, lat- 0.045), colour = "red", cex=1)

png(filename=paste0("D:/Rresult/NewEdition_PLR/M2/rcp85_",GCM,"/3_rcp85(2046-2065).png"), width=630, height=800, res=120)
ggm
dev.off()

## rcp853.out
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
  geom_point(data=rcp853.out, aes(lon+ 0.020, lat- 0.045), colour = "red", cex=1)

png(filename=paste0("D:/Rresult/NewEdition_PLR/M2/rcp85_",GCM,"/4_rcp85(2081-2100).png"), width=630, height=800, res=120)
ggm
dev.off()

## rcp854.out
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
  geom_point(data=rcp854.out, aes(lon+ 0.020, lat- 0.045), colour = "red", cex=1)

png(filename=paste0("D:/Rresult/NewEdition_PLR/M2/rcp85_",GCM,"/5_rcp85(+2C).png"), width=630, height=800, res=120)
ggm
dev.off()
#### END ####