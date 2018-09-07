## If gibberish, please select "Reopen with Encoding..." to UTF-8.
remove(list=ls())  # 清除所有變數設定

library(lubridate)     # To perform date, time related functions
library(RColorBrewer)  # To draw colorful
library(ncdf4)         # To access the NetCDF file
library(ggplot2)
library(viridis)

files.name <- c("rcp85_bcc_csm1_1","rcp85_HadGEM2_CC","rcp85_IPSL_CM5A_MR","rcp85_MPI_ESM_LR","rcp85_NorESM1_M")
i=4  # MPI-ESM-LR

####[[1]] 統計降尺度未來情境RCP 8.5推估日降雨量資料整合成月資料 ####
floder <- "D:/TaiPAD_Statistical_201802/daily/rcp85/"  # 資料夾路徑
files <- list.files(path=floder, pattern="^pr.*.nc")  # 檔案所屬資料夾與類型
# 正則表示法可參考 http://datascienceandr.org/articles/RegularExpression.html
path <- paste0(floder, files)

ind365 <- list(1:31, 32:59, 60:90, 91:120, 121:151, 152:181, 182:212, 213:243, 244:273, 274:304, 305:334, 335:365)
ncfile <- nc_open(path[i])
#ncfile  # 直接檢視 nc 檔的資訊

##(1) 日降雨量轉月平均日降雨量 ##
prec <- ncvar_get(ncfile, "pr")  # 降雨量pr
time <- ncvar_get(ncfile, "time")  # 2006-2100，共95年
lon <- ncvar_get(ncfile, "lon")  # lon 長度 60
lat <- ncvar_get(ncfile, "lat")  # lat 長度 81
lonlat <- data.frame(as.matrix(expand.grid(lon, lat)))  # 化成二維資料
colnames(lonlat) <- c("lon", "lat")

# 每一網格點的降雨日數以list方式逐年紀錄(+2C: 2030-2059)
N.yr=30
rcp85 <- list()
for (y in 1:N.yr) {rcp85[[y]] <- prec[,,1:365+365*(y+23)]}  # 2030-2006-1=23

rcp85.2C <- matrix(nrow=4860, ncol=12)  # 換成經緯度60x81的時期內月資料

for (k in 1:N.yr) {  # k為逐年
  print(paste0("k=",k))
  # 將三維日資料先依月平均成月資料再轉成二維資料
  for (m in 1:12) {rcp85.2C[,m] <- as.vector(apply(rcp85[[k]][,,ind365[[m]]], 1:2, mean))}
  if (k==1) {df.rcp85.2C <- rcp85.2C} else {df.rcp85.2C <- df.rcp85.2C + rcp85.2C}  # 逐年累加
}

df.rcp85.2C <- cbind(lonlat, df.rcp85.2C/N.yr)
df.rcp85.2C <- df.rcp85.2C[with(df.rcp85.2C, order(lat,lon)), ]
index <- complete.cases(df.rcp85.2C)  # 有完整資料的位置
rcp85.2C.prec <- data.frame(df.rcp85.2C[index,])

# 簡單繪圖驗證
#windows()
#ggplot() + 
#  geom_raster(data = rcp85.2C.prec, aes(x=lon, y = lat, fill=X1)) + 
#  coord_fixed(ratio = 1) +
#  scale_fill_viridis(direction = -1)

##(2) 日降雨量轉成月降雨日數 ##
rday <- (prec>=1)*1  # 轉換成降雨日數，定義降雨量超過1mm為有降雨
rcp85 <- list()
for (y in 1:N.yr) {rcp85[[y]] <- rday[,,1:365+365*(y+23)]}

# 每一網格點分不同時期逐年將日資料整合成月資料再取時期內逐月總和
rcp85.2C <- matrix(nrow=4860, ncol=12)

for (k in 1:N.yr) {
  print(paste0("k=",k))
  for (m in 1:12) {rcp85.2C[,m] <- as.vector(apply(rcp85[[k]][,,ind365[[m]]], 1:2, sum))}
  if (k==1) {df.rcp85.2C <- rcp85.2C} else {df.rcp85.2C <- df.rcp85.2C + rcp85.2C}
}

df.rcp85.2C <- cbind(lonlat, df.rcp85.2C/N.yr)
df.rcp85.2C <- df.rcp85.2C[with(df.rcp85.2C, order(lat,lon)), ]
index <- complete.cases(df.rcp85.2C)  # 有完整資料的位置
rcp85.2C.rday <- data.frame(df.rcp85.2C[index,])

# 簡單繪圖驗證
#windows()
#ggplot() + 
#  geom_raster(data = rcp85.2C.rday, aes(x=lon, y = lat, fill=X1)) + 
#  coord_fixed(ratio = 1) +
#  scale_fill_viridis(direction = -1)

# 雨量資料整合輸出 
rain.data.2C <- cbind(rcp85.2C.prec, rcp85.2C.rday[,-(1:2)])
colnames(rain.data.2C) <- c("lon","lat",paste0("prec.",1:12),paste0("rday.",1:12))
write.csv(rain.data.2C, paste0("D:/Rresult/DataVerify/rain_",files.name[i],"_2C.csv"), row.names=F)

####[[2]] 統計降尺度未來情境RCP 8.5推估月氣溫資料 ####
floder <- "D:/TaiPAD_Statistical_201802/monthly/rcp85/"  # 資料夾路徑
vr.names <- c("tas","tasmax","tasmin")

for (j in 1:3) {
  files <- list.files(path=floder, pattern=paste0("^",vr.names[j],"_.*.nc"))  # 檔案所屬資料夾與類型
  path <- paste0(floder, files)
  
  ncfile <- nc_open(path[i])
  tas <- ncvar_get(ncfile, vr.names[j])
  time <- ncvar_get(ncfile, "time")  # 2006-2100，共95年
  lon <- ncvar_get(ncfile, "lon")
  lat <- ncvar_get(ncfile, "lat")
  lonlat <- data.frame(as.matrix(expand.grid(lon, lat)))  # 化成二維資料
  colnames(lonlat) <- c("lon", "lat")
  
  # 每一網格點的降雨日數以list方式逐年紀錄(+2C: 2030-2059)
  N.yr=30
  rcp85 <- list()
  for (y in 1:N.yr) {rcp85[[y]] <- tas[,,1:12+12*(y+23)]}  # 2030-2006-1=23
  
  rcp85.2C <- matrix(nrow=4860, ncol=12)
  
  for (k in 1:N.yr) {
    for (m in 1:12) {rcp85.2C[,m] <- as.vector(rcp85[[k]][,,m])}
    if (k==1) {df.rcp85.2C <- rcp85.2C} else {df.rcp85.2C <- df.rcp85.2C + rcp85.2C}
  }
  
  df.rcp85.2C <- cbind(lonlat, df.rcp85.2C/N.yr)
  df.rcp85.2C <- df.rcp85.2C[with(df.rcp85.2C, order(lat,lon)), ]
  index <- complete.cases(df.rcp85.2C)  # 有完整資料的位置
  
  if (j==1) {rcp85.2C.tavg <- data.frame(df.rcp85.2C[index,])}
  if (j==2) {rcp85.2C.tmax <- data.frame(df.rcp85.2C[index,])}
  if (j==3) {rcp85.2C.tmin <- data.frame(df.rcp85.2C[index,])}
}

# 雨量資料整合輸出 
rain.data.2C <- cbind(rcp85.2C.prec, rcp85.2C.rday[,-(1:2)])
colnames(rain.data.2C) <- c("lon","lat",paste0("prec.",1:12),paste0("rday.",1:12))
write.csv(rain.data.2C, paste0("D:/Rresult/DataVerify/rain_",files.name[i],"_2C.csv"), row.names=F)

####[[2]] 與其他統計降尺度未來情境RCP 8.5推估氣象月資料整合 ####
## 由日資料產生月降雨日數、有降雨日平均日降雨強度的函數
MeanMerge <- function(x) {
  # 依台灣降雨型態將季節重新分類成春季(2-4)、梅雨季(5-6)、夏季(7-9)、秋季(10-11)、冬季(12-1)
  x$x.spr <- (x$X2 *28 + x$X3 *31 + x$X4 *30)/89
  x$x.mei <- (x$X5 *31 + x$X6 *30)/61
  x$x.sum <- (x$X7 *31 + x$X8 *31 + x$X9 *30)/92
  x$x.aut <- (x$X10*31 + x$X11*30)/61
  x$x.win <- (x$X12*31 + x$X1 *31)/62
  x$x.wet <- (x$X5 *31 + x$X6 *30 + x$X7 *31 + x$X8 *31 + x$X9*30)/153
  x$x.dry <- (x$X10*31 + x$X11*30 + x$X12*31 + x$X1 *31)/123
  x$x.ann <- (x$X1 *31 + x$X2 *28 + x$X3 *31 + x$X4 *30 + x$X5 *31 + x$X6 *30 +
              x$X7 *31 + x$X8 *31 + x$X9 *30 + x$X10*31 + x$X11*30 + x$X12*31)/365
  x <- x[,c("lon","lat", paste0("x.",c("spr","mei","sum","aut","win","wet","dry","ann")))]
  return(x)
}

SumMerge <- function(x) {
  # 依台灣降雨型態將季節重新分類成春季(2-4)、梅雨季(5-6)、夏季(7-9)、秋季(10-11)、冬季(12-1)
  x$x.spr <- x$X2 + x$X3 + x$X4
  x$x.mei <- x$X5 + x$X6
  x$x.sum <- x$X7 + x$X8 + x$X9
  x$x.aut <- x$X10+ x$X11
  x$x.win <- x$X12+ x$X1
  x$x.wet <- x$x.mei + x$x.sum
  x$x.dry <- x$x.aut + x$x.win
  x$x.ann <- x$x.spr + x$x.wet + x$x.dry
  x <- x[,c("lon","lat", paste0("x.",c("spr","mei","sum","aut","win","wet","dry","ann")))]
  return(x)
}

prec.5s <- MeanMerge(rcp85.2C.prec)
rday.5s <-  SumMerge(rcp85.2C.rday)
tavg.5s <- MeanMerge(rcp85.2C.tavg)
tmax.5s <- MeanMerge(rcp85.2C.tmax)
tmin.5s <- MeanMerge(rcp85.2C.tmin)
colnames(prec.5s) <- c("lon","lat",paste0("prec.",c("spr","mei","sum","aut","win","wet","dry","ann")))
colnames(rday.5s) <- c("lon","lat",paste0("rday.",c("spr","mei","sum","aut","win","wet","dry","ann")))
colnames(tavg.5s) <- c("lon","lat",paste0("tavg.",c("spr","mei","sum","aut","win","wet","dry","ann")))
colnames(tmax.5s) <- c("lon","lat",paste0("tmax.",c("spr","mei","sum","aut","win","wet","dry","ann")))
colnames(tmin.5s) <- c("lon","lat",paste0("tmin.",c("spr","mei","sum","aut","win","wet","dry","ann")))

pts.rcp85C2 <- cbind(tavg.5s, tmax.5s[,-(1:2)], tmin.5s[,-(1:2)], prec.5s[,-(1:2)], rday.5s[,-(1:2)])
write.csv(pts.rcp85C2, paste0("D:/Rresult/DataVerify/weather_",files.name[i],"_+2C.csv"), row.names=F)