#remove(list=ls())           # 清除所有變數設定

#install.packages("<name>")
library(glmnet)
library(pROC)
library(arm)
library(ggplot2)
library(rgeos)        ## shapefile/TIFF GIS
library(rgdal)        ## shapefile/TIFF GIS
library(sp)           ## Data management
library(spdep)        ## shapefile GIS
library(RColorBrewer)
library(plyr)
library(maptools)
library(ggmap)
library(mapproj)
library(gridExtra)
library(latticeExtra)

##[1] 埃及斑蚊鄉鎮分布 (Aeg.dist)
Aeg.dist <- read.csv(file="D:\\TaiwanMapsShapefiles\\AedeAegDist.csv", header=TRUE, sep=",", stringsAsFactors=FALSE)
#str(DF.dist)
Aeg.dist$Name <- paste0(Aeg.dist$County, Aeg.dist$Town)

##[2] 鄉鎮界行政區 (shp.sub)
## Import shapefile Data
setCPLConfigOption("SHAPE_ENCODING","")   # 讓中文不顯示亂碼
ttw.poly <- readOGR("D:\\TaiwanMapsShapefiles\\Town\\Town_MOI_1041215.shp", stringsAsFactors=F)
ttw.poly@data$Name <- paste0(ttw.poly@data$C_Name, ttw.poly@data$T_Name)
shp.sub <- subset(ttw.poly, ttw.poly$C_Name!="金門縣" & ttw.poly$C_Name!="連江縣") # 去掉金馬地區
#mydata <- shp.sub@data

pts.data <- pts.data2
##[3] 將氣象網格資料轉為spatial dataframe (pts.data)
#data.base = read.csv(file="c:/R/TCCIP_climate_grid_data/Base/Base_1986-2005.csv", stringsAsFactors=F)  # 讀取csv檔
#data.base[data.base==-99.9] <- NA
#pts.data <- data.base[complete.cases(data.base), ]
# 需先執行 megTCCIPgrids.R
coordinates(pts.data) <- ~lon + lat
crs <- CRS("+proj=longlat +ellps=GRS80 +no_defs")
proj4string(pts.data)= crs# 設定與shapefile data相同

##[4] 將氣象網格資料對應行政區 (pts.data)
pts.data$Name <- over(pts.data, ttw.poly)$Name  # 將每一網格點所屬行政區Name加上
#mydata <- pts.data@data
na.index <- which(is.na(pts.data$Name))         # 找出未對應到行政區的網格點其資料位置
na.pts <- pts.data[na.index, ]                  # 挑出落在行政區外的網格點
#plot(na.pts, pch=16, col="red")                # 確認那些點的位置
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
#nearest   # Check that it worked
pts.data$Name[na.index] <- nearest  # 將最接近行政區的名字填補回去
#mydata <- pts.data@data

##[5] 將氣象網格資料依行政區平均
# 確認行政區與點資料形態
#head(shp.sub@data)   # polygons
#head(pts.data@data)  # points
#plot(shp.sub, xlim=c(119,122.5), ylim=c(21.5,25.5))
#points(pts.data, pch=20)

agg.data.mean <- aggregate(.~Name, data=pts.data, mean)   # 將行政區內的變數值取平均
agg.data.median <- aggregate(.~Name, data=pts.data, median)
#agg.data.min <- aggregate(.~Name, data=pts.data, min)
agg.data <- cbind(agg.data.mean[,-7], agg.data.median[,7])
colnames(agg.data)[7] <- "alt"
meg.data <- merge(agg.data, Aeg.dist[,5:length(Aeg.dist)], by="Name", all.y=TRUE)    # 合併氣象資料與埃及斑蚊分布資料
shp.new <- merge(shp.sub, meg.data, by="Name", sort=F)  # sort=F is critical!!!!
#mydata <- shp.new@data[c(1,12:19)]

# 註：若網格點都在範圍內只需要以下步驟(使用package sp裡的over函數)
#shp.sub@data$id <- 1:length(shp.sub)  # 讓地區編ID
#pts.data$id <- over(pts.data, shp.sub)$id  # 將每一網格點所屬行政區id加上
#agg.data <- over(shp.sub, pts.data, fn = mean)   # 將行政區內的變數值取平均
# 如要進階呈現可參考 https://www.nceas.ucsb.edu/scicomp/usecases/point-in-polygon

# 行政區無網格資料者用地理鄰居的平均值填補
# need library(spdep)
coords <- coordinates(shp.new)
shp.nb <- poly2nb(shp.new)  # list結構，儲存每個行政區id的鄰居id (跟直接用shp.nb@data輸出順序不同)
#which(colnames(shp.new@data) %in% c("tavg.ann","prec.win"))

for (j in 12:17) {  # 只處理氣象變數
  rep = 1
  while (rep <=2) {  # 進行兩次地理鄰居值填補，以防第一次抓到有鄰居但鄰居皆無值的點 
    # print(rep)
    for (i in which( is.na(shp.new@data[,j]), arr.ind=TRUE) ) {  # 找有NA值的區域id 
      shp.new@data[i,j]=mean(shp.new@data[shp.nb[[i]],j], na.rm=TRUE)  # 將該區域附近鄰居的數值平均
    }
    rep = rep + 1
  }
}
#mydata <- shp.new@data[c(1,12:19)]
shp.new@data[is.na(shp.new@data)] <- 0    # 如果還有遺失值直接補0

##[6] 繪圖確認
## https://edzer.github.io/sp/
## http://www.nickeubank.com/wp-content/uploads/2015/10/RGIS3_MakingMaps_part1_mappingVectorData.html
# 海拔
#grps <- 8
#brks <- seq(from=0, to=2000, by=250)
#windows()
#spplot(shp.new, "alt", at=brks, col.regions=brewer.pal(grps,"Blues"), col="black", xlim=c(119,122.5), ylim=c(21.5,25.5))

# 冬季降雨
#grps <- 9
#brks <- quantile(shp.new$prec.win+0.1, 0:(grps-1)/(grps-1), na.rm=TRUE)  # +0.1這樣100%才有顏色
#windows()
#spplot(shp.new, "prec.win", at=brks, col.regions=brewer.pal(grps,"Blues"), col="black", xlim=c(119,122.5), ylim=c(21.5,25.5))

# 全年均溫
#grps <- 7
#brks <- seq(from=12, to=26, by=2)
#windows()
#spplot(shp.new, "tavg.ann", at=brks, col.regions=rev(brewer.pal(grps,"Blues")), col="black", xlim=c(119,122.5), ylim=c(21.5,25.5))

# 埃及斑蚊分布
#windows()
#spplot(shp.new, "Aeg2003_2011", col.regions=c("white","yellow"), col="black", xlim=c(119,122.5), ylim=c(21.5,25.5), colorkey=FALSE)

####----建立資料結構----####
AegData <- shp.new@data[,c(1,20,12:17)]
colnames(AegData)[2] <- "index"

round(rbind(mu=sapply(AegData[,-1], mean), SD=sapply(AegData[,-1], sd)), 3)  # 描述變項的平均與標準差 

####----logistic regression model----####
lm.f <- glm (index~ alt + tavg.ann + tmax.sum + tmin.win + prec.sum + prec.win, data=AegData, family=binomial)
summary(lm.f)

lm.n <- glm (index~ 1, data=AegData, family=binomial)
summary(lm.n)

# model selection
backwards = step(lm.f) # Backwards selection is the default
forwards = step(lm.n, scope=list(lower=formula(lm.n),upper=formula(lm.f)), direction="forward")
stepwise = step(lm.n, list(lower=formula(lm.n),upper=formula(lm.f)), direction="both", trace=0)

formula(backwards)  # index ~ tavg.ann + tmax.sum + prec.sum + prec.win
formula(forwards)   # index ~ prec.win + prec.sum + tmin.win + tmax.sum + tavg.ann
formula(stepwise)   # index ~ prec.win + prec.sum + tmax.sum + tavg.ann

#由於資料容易產生(類)完美分類問題，並不適合直接用logistic reg

####----Penalized Logistic Regression----####
# Rcode範例http://stats.stackexchange.com/questions/72251/an-example-lasso-regression-using-glmnet-for-binary-outcome
# glmnet說明https://web.stanford.edu/~hastie/glmnet/glmnet_alpha.html

set.seed(9527)
# 進行1000次變數選擇結果統計(由於交叉驗證(cv) K-fold的K為隨機選取，故會結果不同)
#str(AegData)
x <- as.matrix(AegData[,c(3,5:8)])
y <- AegData[,2]

for (i in 1:1000)
{
  print(i)
  cvPLR <- cv.glmnet(x, y, family='binomial', type.measure = "class", alpha=0.5) 
  # alpha=1, LASSO; alpha=0, ridge; alpha=0.5, both
  if (i==1) {
    coef.1se <- coef(cvPLR, s="lambda.1se")
    coef.min <- coef(cvPLR, s="lambda.min")
  } else {
    coef.1se <- cbind(coef.1se, coef(cvPLR, s="lambda.1se"))
    coef.min <- cbind(coef.min, coef(cvPLR, s="lambda.min"))
  }
  base <- as.numeric(predict(cvPLR, x, s ="lambda.min", type = "class", alpha=0.5))
  if (i==1) {PLR.sum = base} else {PLR.sum = PLR.sum + base}
}
PLR.out <- cbind(AegData[,1], round(PLR.sum/1000))
colnames(PLR.out) <- c("Name","fit")
write.table(PLR.out, file="PLR_fit2003_2011.CSV", row.names=F, sep=",")

mat.1se <- as.data.frame(t(as.matrix(coef.1se))[,-1]); rownames(mat.1se) <- 1:1000
mat.min <- as.data.frame(t(as.matrix(coef.min))[,-1]); rownames(mat.min) <- 1:1000
result.median <- rbind(round(apply(mat.1se, 2, median),3), round(apply(mat.min, 2, median),3))
rownames(result.median) <- c("mat.1se","mat.min")
result.median
round(exp(result.median),3)

mat.1se.count <- (mat.1se!=0)*1; rownames(mat.1se.count) <- 1:1000
mat.min.count <- (mat.min!=0)*1; rownames(mat.min.count) <- 1:1000
select.sum <- rbind(apply(mat.1se.count, 2, sum), apply(mat.min.count, 2, sum))
rownames(select.sum) <- c("mat.1se","mat.min")
select.sum

shp.fit <- merge(shp.new, PLR.out, by="Name", sort=F)  # sort=F is critical!!!!
# require gridExtra
windows()
grid.arrange(
  spplot(shp.fit, "fit", col.regions=c("white","yellow"), col="black", xlim=c(119,122.5), ylim=c(21.5,25.5), colorkey=FALSE),
  spplot(shp.fit, "Aeg2003_2011", col.regions=c("white","yellow"), col="black", xlim=c(119,122.5), ylim=c(21.5,25.5), colorkey=FALSE) )

## Predict Grid Data Firstly##
## 考慮全年均溫、冬季最低溫、夏季降雨、冬季降雨與海拔高度
x <- as.matrix(AegData[,c(3,5:8)])
y <- AegData[,2]
data.verify <- pts.data2

set.seed(9527)
repn = 1000
for (i in 1:repn)
{
  print(i)
  cvPLR <- cv.glmnet(x, y, family='binomial', type.measure="class", alpha=0.5)
  
  ## 2003-2011
  newx <- as.matrix(data.verify[,c(3,5:8)])
  PLR.out <- as.numeric(predict(cvPLR, newx, s ="lambda.min", type = "class", alpha=0.5))
  
  ## 輸出結果
  if (i==1) {PLR.sum = PLR.out} else {PLR.sum = PLR.sum + PLR.out}
}
PLR.out <- cbind(data.verify[,1:2], round(PLR.sum/repn))
colnames(PLR.out) <- c("lon","lat","fit")

row_sub = apply(PLR.out, 1, function(row) all(row !=0 ))
PLR.out <- PLR.out[row_sub,]

## 檢視真實分布地圖與估計點圖的差別
#[1] panel.points need library(latticeExtra)
p <- spplot(shp.new, "Aeg2003_2011", col.regions=c("white","yellow"), col="black", xlim=c(119,122.5), ylim=c(21.5,25.5), colorkey=FALSE)
windows()
p + layer(panel.points(lon, lat, col="red", pch=19, cex=0.5), data=PLR.out)

#[2] Plot on Google Maps
ctwmap <- shp.new
ctwmap@data$id <- 1:length(ctwmap)  # 讓地區編ID
mydata <- ctwmap@data

ctwmap.points <- fortify(ctwmap, region="id")   # need library(maptools)
# fortify from ggplot2 package: shapfile -> data.frame
ctwmap.points$long <- ctwmap.points$long + 0.02
ctwmap.points$lat  <- ctwmap.points$lat  - 0.01  # 微調繪行政區圖位置

ctwmap.df = join(ctwmap.points, mydata, by="id")  # 得到各經緯度下的地理資訊資料

map <- get_map(location = 'Taiwan', zoom = 7, language = "zh-TW", maptype = "roadmap")
windows()
ggm <- ggmap(map) +
  scale_x_continuous(limits = c(119, 122.5), expand = c(0, 0)) +
  scale_y_continuous(limits = c(21.5, 25.5), expand = c(0, 0)) +
  geom_polygon(data = ctwmap.df, 
               aes(x = long, y = lat, group = group, fill=factor(Aeg2003_2011)), 
               color = "black", size = 0.25, alpha = 0.5) +  # alpha 是透明度
  scale_fill_manual(name="Aeg2003_2011", values=c("white","yellow"))+
  geom_point(data=PLR.out, aes(lon+0.02, lat-0.01), colour = "red", cex=0.5) +
  ggtitle("Aeg. Dist.")
ggm

## 檢視估計分布地圖與估計分布點圖的差別
p <- spplot(shp.fit, "fit", col.regions=c("white","yellow"), col="black", xlim=c(119,122.5), ylim=c(21.5,25.5), colorkey=FALSE)
windows()
p + layer(panel.points(lon, lat, col="red", pch=19, cex=0.5), data=PLR.out)