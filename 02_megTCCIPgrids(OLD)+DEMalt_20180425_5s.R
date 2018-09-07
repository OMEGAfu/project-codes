## If gibberish, please select "Reopen with Encoding..." to UTF-8.
remove(list=ls())  # 清除所有變數設定

####[[0]] 擷取台灣 DEM 圖層中的高度訊息 (raster資料處理) ####
## Getting the height information from DEM and tranform to the grids
library(raster)
library(sp)
library(rgdal)

setwd("D:/20120201_twdtm_asterV2_30m/tif file")  # 設定資料夾
DEM <- raster("twdtm_asterV2_30m.tif")  # 設定檔案名稱
agDEM <- aggregate(DEM, fact=10, fun=mean)  # 解析度太高降低成取每10個網格的平均值(資料量太大 median 太花時間)
range(values(agDEM))  # 確定數值範圍[0, 3805.49]
## 簡易繪圖呈現可直接用plot
windows()
plot(agDEM, col=terrain.colors(30), zlim=c(0.01,max(values(agDEM))))  # 海面留白

## extract specific values from a DEM (or other raster data) by library(raster)
ddf <- rasterToPoints(agDEM)  # 轉成 lon, lat, alt 形式 
ddf <- as.data.frame(ddf)
colnames(ddf) <- c("lon","lat","alt")
ddf <- round(ddf, 2)          # 降低解析度 lon, lat 四捨五入到小數兩位
aggddf <- aggregate(alt ~ lon + lat, data=ddf, median)  # 將四捨五入後相同的網格點海拔取中位數

## restrict to .05deg to .05deg
# 為配合 TCCIP 的網格(每0.05度一個網格點)進行資料合併
aggddf$lon.05 <- round(aggddf$lon*100/5)*5/100   # c.xy -> cxy -> round(cxy/5)
aggddf$lat.05 <- round(aggddf$lat*100/5)*5/100
aggddf$alt <- round(aggddf$alt, 2)
subddf <- subset(aggddf, select=c(lon.05, lat.05, alt))
subddf <- aggregate(alt ~ lon.05 + lat.05, data=subddf, median)
colnames(subddf) <- c("lon","lat","alt")
# Go through each row and determine if a value is zero
pts.alt <- subddf[apply(subddf, 1, function(row) all(row !=0 )), ]  # 如果有海拔高度為0的點則捨棄
write.csv(pts.alt, "D:/Rresult/DataVerify/pts_alt.csv", row.names=F)

####[[1]] 匯入舊版歷史氣象網格資料 (統計降尺度) 並整合海拔高度訊息 ####
####--(1) 建立 01_OBS_Grid 資料夾下多個檔案的路徑 --####
floder <- "D:/TCCIP_Data/01_OBS_Grid/TCCIP_v3/rain_5km-v3/"  # 資料夾路徑
files <- list.files(path=floder, pattern="*.csv")  # 檔案所屬資料夾與類型
path.prec <- paste0(floder, files)[1:12]  # 1月到12月

floder <- "D:/TCCIP_Data/01_OBS_Grid/TCCIP_v4/tavg_5km-v4/"  # 資料夾路徑
files <- list.files(path=floder, pattern="*.csv")  # 檔案所屬資料夾與類型
path.tavg <- paste0(floder, files)[1:12]  # 1月到12月

floder <- "D:/TCCIP_Data/01_OBS_Grid/TCCIP_v4/tmax_5km-v4/"  # 資料夾路徑
files <- list.files(path=floder, pattern="*.csv")  # 檔案所屬資料夾與類型
path.tmax <- paste0(floder, files)[1:12]  # 1月到12月

floder <- "D:/TCCIP_Data/01_OBS_Grid/TCCIP_v4/tmin_5km-v4/"  # 資料夾路徑
files <- list.files(path=floder, pattern="*.csv")  # 檔案所屬資料夾與類型
path.tmin <- paste0(floder, files)[1:12]  # 1月到12月

path.all <- c(path.prec, path.tavg, path.tmax, path.tmin)  # 整理所有檔案路徑

int.name <- c(paste0("prec.0",1:9), paste0("prec.1",0:2),
              paste0("tavg.0",1:9), paste0("tavg.1",0:2),
              paste0("tmax.0",1:9), paste0("tmax.1",0:2),
              paste0("tmin.0",1:9), paste0("tmin.1",0:2))  # 依照檔案來源建立整合資料的變數名稱
#length(path.all)==length(int.name)  # 檢查

####--(2) 匯入歷史氣象資料 --#### 
## 由於有多個相同格式的 csv檔以相同方式匯入故寫成 function: DataImport
DataImport <- function(path,i) {
  ind <- list(46:54, 29:48)  # yr2003-2011, yr1986-2005
  dat <- readLines(path)  # 資料有 column 未命名造成的讀取問題, 故用 readLines
  file <- read.csv(textConnection(paste0(dat, collapse="\n")), header=FALSE, stringsAsFactors=FALSE)[-1,c(-1,-2,-3,-59)]
  colnames(file) <- c("lon","lat", paste0("yr",1960:2012))
  obs <- round(apply(file[,ind[[i]]], 1, mean, na.rm=TRUE), 3)
  obs <- cbind(file[,1:2], obs)
  obs[obs==-99.9] <- NA
  return(obs)
}
# 資料有 column 未命名造成的讀取問題
# 解法參照 https://stackoverflow.com/questions/25771071/r-read-csv-more-columns-than-column-names-error

## 氣象資料讀取主程式
for (i in 1:2) {  # i=1, yr2003-2011; i=2, yr1986-2005
  for (j in 1:length(path.all)) {
    X <- DataImport(path.all[j],i)
    if (j==1) {weather <- X} else {weather <- cbind(weather, X[,3])}  # 需先確認原始資料格式完全相同
  }
  colnames(weather)[1:length(path.all) +2] <- int.name  # {1:n +2}=={3:(n+2)}, colnames[1:2]=c("lon","lat")
  if (i==2) {baseline <- weather}  # baseline 在之後未來推估資料需要用到
  # 依氣象局台灣降雨型態將季節重新分類成春季(2-4)、梅雨季(5-6)、夏季(7-9)、秋季(10-11)、冬季(12-1)
  # 降雨為季節別之平均日降雨量
  weather$prec.spr <- (weather$prec.02*28 + weather$prec.03*31 + weather$prec.04*30)/89
  weather$prec.mei <- (weather$prec.05*31 + weather$prec.06*30)/61
  weather$prec.sum <- (weather$prec.07*31 + weather$prec.08*31 + weather$prec.09*30)/92
  weather$prec.aut <- (weather$prec.10*31 + weather$prec.11*30)/61
  weather$prec.win <- (weather$prec.12*31 + weather$prec.01*31)/62
  weather$prec.wet <- (weather$prec.05*31 + weather$prec.06*30 + weather$prec.07*31 + weather$prec.08*31 + weather$prec.09*30)/153
  weather$prec.dry <- (weather$prec.10*31 + weather$prec.11*30 + weather$prec.12*31 + weather$prec.01*31)/123
  weather$prec.ann <- (weather$prec.01*31 + weather$prec.02*28 + weather$prec.03*31 + weather$prec.04*30 + weather$prec.05*31 + weather$prec.06*30 +
                       weather$prec.07*31 + weather$prec.08*31 + weather$prec.09*30 + weather$prec.10*31 + weather$prec.11*30 + weather$prec.12*31)/365
  
  # 均溫為季節別之日均溫
  weather$tavg.spr <- (weather$tavg.02*28 + weather$tavg.03*31 + weather$tavg.04*30)/89
  weather$tavg.mei <- (weather$tavg.05*31 + weather$tavg.06*30)/61
  weather$tavg.sum <- (weather$tavg.07*31 + weather$tavg.08*31 + weather$tavg.09*30)/92
  weather$tavg.aut <- (weather$tavg.10*31 + weather$tavg.11*30)/61
  weather$tavg.win <- (weather$tavg.12*31 + weather$tavg.01*31)/62
  weather$tavg.wet <- (weather$tavg.05*31 + weather$tavg.06*30 + weather$tavg.07*31 + weather$tavg.08*31 + weather$tavg.09*30)/153
  weather$tavg.dry <- (weather$tavg.10*31 + weather$tavg.11*30 + weather$tavg.12*31 + weather$tavg.01*31)/123
  weather$tavg.ann <- (weather$tavg.01*31 + weather$tavg.02*28 + weather$tavg.03*31 + weather$tavg.04*30 + weather$tavg.05*31 + weather$tavg.06*30 +
                       weather$tavg.07*31 + weather$tavg.08*31 + weather$tavg.09*30 + weather$tavg.10*31 + weather$tavg.11*30 + weather$tavg.12*31)/365
  
  # 夏季平均日最高溫
  weather$tmax.spr <- (weather$tmax.02*28 + weather$tmax.03*31 + weather$tmax.04*30)/89
  weather$tmax.mei <- (weather$tmax.05*31 + weather$tmax.06*30)/61
  weather$tmax.sum <- (weather$tmax.07*31 + weather$tmax.08*31 + weather$tmax.09*30)/92
  weather$tmax.aut <- (weather$tmax.10*31 + weather$tmax.11*30)/61
  weather$tmax.win <- (weather$tmax.12*31 + weather$tmax.01*31)/62
  weather$tmax.wet <- (weather$tmax.05*31 + weather$tmax.06*30 + weather$tmax.07*31 + weather$tmax.08*31 + weather$tmax.09*30)/153
  weather$tmax.dry <- (weather$tmax.10*31 + weather$tmax.11*30 + weather$tmax.12*31 + weather$tmax.01*31)/123
  weather$tmax.ann <- (weather$tmax.01*31 + weather$tmax.02*28 + weather$tmax.03*31 + weather$tmax.04*30 + weather$tmax.05*31 + weather$tmax.06*30 +
                       weather$tmax.07*31 + weather$tmax.08*31 + weather$tmax.09*30 + weather$tmax.10*31 + weather$tmax.11*30 + weather$tmax.12*31)/365
  
  # 冬季平均日最低溫
  weather$tmin.spr <- (weather$tmin.02*28 + weather$tmin.03*31 + weather$tmin.04*30)/89
  weather$tmin.mei <- (weather$tmin.05*31 + weather$tmin.06*30)/61
  weather$tmin.sum <- (weather$tmin.07*31 + weather$tmin.08*31 + weather$tmin.09*30)/92
  weather$tmin.aut <- (weather$tmin.10*31 + weather$tmin.11*30)/61
  weather$tmin.win <- (weather$tmin.12*31 + weather$tmin.01*31)/62
  weather$tmin.wet <- (weather$tmin.05*31 + weather$tmin.06*30 + weather$tmin.07*31 + weather$tmin.08*31 + weather$tmin.09*30)/153
  weather$tmin.dry <- (weather$tmin.10*31 + weather$tmin.11*30 + weather$tmin.12*31 + weather$tmin.01*31)/123
  weather$tmin.ann <- (weather$tmin.01*31 + weather$tmin.02*28 + weather$tmin.03*31 + weather$tmin.04*30 + weather$tmin.05*31 + weather$tmin.06*30 +
                       weather$tmin.07*31 + weather$tmin.08*31 + weather$tmin.09*30 + weather$tmin.10*31 + weather$tmin.11*30 + weather$tmin.12*31)/365
    
  # 經緯度
  weather$lon <- as.numeric(weather$lon)
  weather$lat <- as.numeric(weather$lat)
  
  obs.name <- c("prec.spr", "prec.mei", "prec.sum", "prec.aut", "prec.win", "prec.wet", "prec.dry", "prec.ann",  # 降雨
                "tavg.spr", "tavg.mei", "tavg.sum", "tavg.aut", "tavg.win", "tavg.wet", "tavg.dry", "tavg.ann",  # 均溫
                "tmax.spr", "tmax.mei", "tmax.sum", "tmax.aut", "tmax.win", "tmax.wet", "tmax.dry", "tmax.ann",  # 最高溫
                "tmin.spr", "tmin.mei", "tmin.sum", "tmin.aut", "tmin.win", "tmin.wet", "tmin.dry", "tmin.ann")  # 最低溫
  weather <- weather[ ,c("lon", "lat", obs.name)]
  if (i==1) {pts.data <- weather[complete.cases(weather), ]}
  if (i==2) {pts.base <- weather[complete.cases(weather), ]}
}
write.csv(pts.base, "D:/Rresult/DataVerify/5seasons/pts_base.csv", row.names=F)
write.csv(pts.data, "D:/Rresult/DataVerify/5seasons/pts_varify.csv", row.names=F)

####--(3) 整合海拔高度訊息到氣象歷史網格資料 (pts.data1, pts.data2, pts.base)--####
pts.data <- merge(pts.data, pts.alt, by=c("lon","lat"), all.x=TRUE)  # 以 TCCIP 網格資料為主進行合併
pts.base <- merge(pts.base, pts.alt, by=c("lon","lat"), all.x=TRUE)
pts.data$alt[is.na(pts.data$alt)] <- 0.01  # 如果 TCCIP 網格資料有值海拔資料無值，設定海拔為0.01m
pts.base$alt[is.na(pts.base$alt)] <- 0.01

####[[2]] 匯入未來氣象網格資料 (統計降尺度) 並整合海拔高度訊息 ####
## 情境種類設定
rcps <- c("rcp26","rcp45","rcp60","rcp85")
periods <- c("2016-2035","2046-2065","2081-2100")
index <- c("1635","4665","8100")

## 由於有多個相同格式的 csv檔以相同方式匯入故寫成 function
FutureDataImport <- function(path, ind) {
  file <- read.csv(path, header=TRUE, stringsAsFactors=FALSE)[,4:17]  # 4:lon, 5:lat, 6-17:month01-12 -> 1:lon, 2:lat, 3-14:month01-12
  file[file==-99.9] <- NA
  # baseline 1:lon, 2:lat, 2+(1~12):prec01-12, 14+(1~12):tavg01-12, 26+(1~12):tmax01-12, 38+(1~12):tmin01,12
  if (ind==1) {file <- cbind(file[,1:2], baseline[, 3:14] + baseline[,3:14]*file[,3:14]/100) }  # prec(是相對於基期改變率%)
  if (ind==2) {file <- cbind(file[,1:2], baseline[,15:26] + file[,3:14]) }  # temp(是相對於基期改變量)
  if (ind==3) {file <- cbind(file[,1:2], baseline[,27:38] + file[,3:14]) }
  if (ind==4) {file <- cbind(file[,1:2], baseline[,39:50] + file[,3:14]) }
  colnames(file) <- c("lon", "lat", paste0("X.0",1:9), paste0("X.1",0:2))

  # 依氣象局台灣降雨型態將季節重新分類成春季(2-4)、梅雨季(5-6)、夏季(7-9)、秋季(10-11)、冬季(12-1)
  file$X.spr <- (file$X.02*28 + file$X.03*31 + file$X.04*30)/89
  file$X.mei <- (file$X.05*31 + file$X.06*30)/61
  file$X.sum <- (file$X.07*31 + file$X.08*31 + file$X.09*30)/92
  file$X.aut <- (file$X.10*31 + file$X.11*30)/61
  file$X.win <- (file$X.12*31 + file$X.01*31)/62
  file$X.wet <- (file$X.05*31 + file$X.06*30 + file$X.07*31 + file$X.08*31 + file$X.09*30)/153
  file$X.dry <- (file$X.10*31 + file$X.11*30 + file$X.12*31 + file$X.01*31)/123
  file$X.ann <- (file$X.01*31 + file$X.02*28 + file$X.03*31 + file$X.04*30 + file$X.05*31 + file$X.06*30 +
                 file$X.07*31 + file$X.08*31 + file$X.09*30 + file$X.10*31 + file$X.11*30 + file$X.12*31)/365
  file <- file[,c("lon","lat","X.spr","X.mei","X.sum","X.aut","X.win","X.wet","X.dry","X.ann")]
  return(file)
}

## 氣象資料讀取
for (j in 1:length(periods)) {
  for (i in 1:length(rcps)) {
    # 日雨量
    path.prec <- paste0("D:/TCCIP_Data/04_Statistical_Downscaling/AR5_change_aphro_5km/pr-",rcps[i],
                        "/ensemble_",rcps[i],"_5km_",periods[j],".csv")
    X.prec <- FutureDataImport(path.prec, 1)
    colnames(X.prec) <- c("lon", "lat", paste0("prec.",c("spr","mei","sum","aut","win","wet","dry","ann")))
    
    # 日均溫
    path.tavg <- paste0("D:/TCCIP_Data/04_Statistical_Downscaling/AR5_change_aphro_5km/tas-",rcps[i],
                        "/ensemble_",rcps[i],"_5km_",periods[j],".csv")
    X.tavg <- FutureDataImport(path.tavg, 2)
    colnames(X.tavg) <- c("lon", "lat", paste0("tavg.",c("spr","mei","sum","aut","win","wet","dry","ann")))
    
    # 夏季最高溫
    path.tmax <- paste0("D:/TCCIP_Data/04_Statistical_Downscaling/AR5_change_CRU_5km_v4/tasmax-",rcps[i],
                        "/ensemble_",rcps[i],"_5km_",periods[j],".csv")
    X.tmax <- FutureDataImport(path.tmax, 3)
    colnames(X.tmax) <- c("lon", "lat", paste0("tmax.",c("spr","mei","sum","aut","win","wet","dry","ann")))
    
    # 冬季最低溫
    path.tmin <- paste0("D:/TCCIP_Data/04_Statistical_Downscaling/AR5_change_CRU_5km_v4/tasmin-",rcps[i],
                        "/ensemble_",rcps[i],"_5km_",periods[j],".csv")
    X.tmin <- FutureDataImport(path.tmin, 4)
    colnames(X.tmin) <- c("lon", "lat", paste0("tmin.",c("spr","mei","sum","aut","win","wet","dry","ann")))
    
    # 資料整合成 降雨五季、均溫五季、颱風季最高溫、冬季最低溫
    X.obs <- cbind(X.prec, X.tavg[,c(-1,-2)], X.tmax[,c(-1,-2)], X.tmin[,c(-1,-2)])
    X.obs <- X.obs[complete.cases(X.obs), ]

    # 整合海拔高度訊息
    X.obs <- merge(X.obs, pts.alt, by=c("lon","lat"), all.x=TRUE)  # 以 TCCIP 網格資料為主進行合併
    X.obs$alt[is.na(X.obs$alt)] <- 0.01  # 如果 TCCIP 網格資料有值海拔資料無值，設定海拔為0.01m
    
    assign(paste0(rcps[i],"yr",index[j]), X.obs)  # assign values to a name
    write.csv(X.obs, paste0("D:/Rresult/DataVerify/5seasons/",rcps[i],"_yr",index[j],".csv"), row.names=F)
  }
}
