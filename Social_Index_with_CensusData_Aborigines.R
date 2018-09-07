## If gibberish, please select "Reopen with Encoding..." to UTF-8.
remove(list=ls())  # 清除所有變數設定

library(rgeos)          ## shapefile/TIFF GIS
library(rgdal)          ## shapefile/TIFF GIS
library(sp)             ## Data management
library(spdep)          ## shapefile GIS
library(RColorBrewer)

####[[1]]民99普查資料 ####
floder <- paste0("D:/Demography_Data/CensusData/")
files <- "99年原住民族常住人口之性別及年齡結構統計_鄉鎮市區.csv"
path <- paste0(floder, files)
X <- read.csv(path, header=TRUE, stringsAsFactors=FALSE)
## sum(X$縣市名稱=="桃園縣")
substr(X$鄉鎮市區名稱[X$縣市名稱=="桃園縣"], start=3, stop=3) <- "區"  # 直接修改桃園鄉鎮第三字為區
X$縣市名稱[X$縣市名稱=="桃園縣"] <- "桃園市"  # 桃園縣改桃園市
X$Name <- paste0(X$縣市名稱, X$鄉鎮市區名稱)
X$Name[X$Name=="彰化縣員林鎮"] <- "彰化縣員林市"
X$Name[X$Name=="苗栗縣頭份鎮"] <- "苗栗縣頭份市"
X.tw <- subset(X, X$縣市名稱!="金門縣" & X$縣市名稱!="連江縣")
X.data <- X.tw[,c("Name","總計")]
colnames(X.data) <- c("Name", "O_CNT.cen")
X.data$O_CNT.cen <- as.numeric(X.data$O_CNT.cen)

## 定義原住民山地鄉 ##
X.data$MA_Town <- 0
MA_Name <- c("宜蘭縣大同鄉","宜蘭縣南澳鄉","花蓮縣秀林鄉","花蓮縣萬榮鄉","花蓮縣卓溪鄉",
             "南投縣仁愛鄉","南投縣信義鄉","屏東縣霧臺鄉","屏東縣三地門鄉","屏東縣瑪家鄉",
             "屏東縣泰武鄉","屏東縣來義鄉","屏東縣春日鄉","屏東縣獅子鄉","屏東縣牡丹鄉",
             "苗栗縣泰安鄉","桃園市復興區","高雄市桃源區","高雄市那瑪夏區","高雄市茂林區",
             "新北市烏來區","新竹縣五峰鄉","新竹縣尖石鄉","嘉義縣阿里山鄉","臺中市和平區",
             "臺東縣蘭嶼鄉","臺東縣延平鄉","臺東縣海端鄉","臺東縣金峰鄉","臺東縣達仁鄉")
X.data$MA_Town[which(X.data$Name %in% MA_Name )] <- 1
X.data$MA_Town <- as.factor(X.data$MA_Town)
#summary(X.data$MA_Town)
write.csv(X.data, paste0("D:/Rresult/CensusData/Aborigines.csv"), row.names=F)
