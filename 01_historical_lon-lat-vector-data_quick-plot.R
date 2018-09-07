library(ggplot2)
library(viridis)  # 數值漸層著色
# https://cran.r-project.org/web/packages/viridis/vignettes/intro-to-viridis.html

## data frame 型態的 vector data
data.verify <- read.csv(file="D:/Rresult/seasons_vrf.csv", header=TRUE, sep=",", 
                        stringsAsFactors=FALSE)
perc.rank <- function(x) { floor(rank(x))/length(x) }  # 將數值轉成排序百分位數

## 海拔高度
value <- perc.rank(data.verify$alt)
windows()
ggplot() + 
  geom_raster(data = data.verify, aes(x=lon, y=lat, fill=value)) +
  coord_fixed(ratio = 1) +
  scale_fill_viridis(direction=-1, begin=0.1, end=0.8)

## 冬季最低溫
value <- perc.rank(data.verify$tmin.win)
windows()
ggplot() + 
  geom_raster(data = data.verify, aes(x=lon, y=lat, fill=value)) +
  coord_fixed(ratio = 1) +
  scale_fill_viridis(direction=-1, begin=0.5, end=1, option="A")

## 春季降雨
value <- perc.rank(data.verify$prec.spr)
windows()
ggplot() + 
  geom_raster(data = data.verify, aes(x=lon, y=lat, fill=value)) +
  coord_fixed(ratio = 1) +
  scale_fill_viridis(direction=-1)

## 梅雨季降雨
value <- perc.rank(data.verify$prec.mei)
windows()
ggplot() + 
  geom_raster(data = data.verify, aes(x=lon, y=lat, fill=value)) +
  coord_fixed(ratio = 1) +
  scale_fill_viridis(direction=-1)

## 夏季降雨
value <- perc.rank(data.verify$prec.sum)
windows()
ggplot() + 
  geom_raster(data = data.verify, aes(x=lon, y=lat, fill=value)) +
  coord_fixed(ratio = 1) +
  scale_fill_viridis(direction=-1)

## 少雨季降雨
value <- perc.rank(data.verify$prec.dry)
windows()
ggplot() + 
  geom_raster(data = data.verify, aes(x=lon, y=lat, fill=value)) +
  coord_fixed(ratio = 1) +
  scale_fill_viridis(direction=-1)