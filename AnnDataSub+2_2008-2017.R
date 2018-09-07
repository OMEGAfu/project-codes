####--Data Code Book--####
## M_F_RAT: 男女比(每百位女性為單位)
## P_H_CNT: 戶量(每戶平均人口數)
## P_DEN: 人口密度(每平方公里之人口數)
## DEPENDENCY_RAT: 扶養比(0-14歲與65歲以上人口數/15-64歲人口數，乘上100)
## A0A14_A15A65_RAT: 扶幼比(0-14歲人口數/15-64歲人口數，乘上100)
## A65UP_A15A64_RAT: 扶老比(65歲以上人口數/15-64歲人口數，乘上100)
## A65_A0A14_RAT: 老化指數(65歲以上人口數/0-14歲人口數，乘上100)
## H_CNT: 戶數
## P_CNT: 戶籍人口數
## M_CNT: 男性戶籍人口數
## F_CNT: 女性戶籍人口數
## RLH_CNT: 低收入戶
## RLH_H_RAC: 低收入戶占總戶數比例(乘上1000)
## RLP_CNT: 低收入戶人數
## RLP_P_RAC: 低收入戶人數占總人數比例(乘上1000)
## H_CNT.x: 老年人戶數(戶中有65歲以上老年人口者之戶數)
## A_AGE_CNT: 戶長平均年齡
## M_H_CNT: 戶長男性人口數
## F_N_CNT: 戶長女性人口數
## O_CNT: 原住民戶籍人口數
## O_M_CNT: 原住民男性戶籍人口數
## O_F_CNT: 原住民女性戶籍人口數
## O1_CNT: 平地原住民人口數(光復前原籍在平地行政區內，且戶口調查簿登記其本人或直系血親尊親屬屬於原住民，並申請登記有案者)
## O1_M_CNT: 平地原住民男性人口數
## O1_F_CNT: 平地原住民女性人口數
## O2_CNT: 山地原住民人口數(光復前原籍在山地行政區內，且戶口調查簿登記其本人或直系血親尊親屬屬於原住民，並申請登記有案者)
## O2_M_CNT: 山地原住民男性人口數
## O2_F_CNT: 山地原住民女性人口數
## NON_O_CNT: 非原住民人口數
## H_CNT.y: 醫療院所家數
## H_BED: 醫療院所床數
## H_SRVP: 醫療院所平均每家服務人數
## H_SRVB: 醫療院所平均每千人擁有病床數

## If gibberish, please select "Reopen with Encoding..." to UTF-8.
remove(list=ls())  # 清除所有變數設定

for (Year in 2008:2017) {
  floder <- paste0("D:/Demography_Data_Subset/STAT",Year,"06/")
  files <- list.files(path=floder, pattern="*\\.csv$")  # 檔案所屬資料夾與類型
  path <- paste0(floder, files)
  for (i in 1:length(path)) {
    X <- read.csv(path[i], header=TRUE, stringsAsFactors=FALSE)
    # 縣市鄉鎮升級時間表 https://zh.wikipedia.org/wiki/中華民國臺灣地區鄉鎮市區列表
    # 2010/12/25新北由台北縣升格為市，台中、台南、高雄為縣市合併
    X$TOWN[X$COUNTY=="臺北縣"] <- gsub("鄉$|鎮$|市$", "區", X$TOWN[X$COUNTY=="臺北縣"])  # 加上$只判別字尾不會取代中間字
    X$COUNTY[X$COUNTY=="臺北縣"] <- "新北市"  # 臺北縣改新北市  
    X$TOWN[X$COUNTY=="臺中縣"] <- gsub("鄉$|鎮$|市$", "區", X$TOWN[X$COUNTY=="臺中縣"])  
    X$COUNTY[X$COUNTY=="臺中縣"] <- "臺中市"  # 臺中縣改台中市
    X$TOWN[X$COUNTY=="臺南縣"] <- gsub("鄉$|鎮$|市$", "區", X$TOWN[X$COUNTY=="臺南縣"])  
    X$COUNTY[X$COUNTY=="臺南縣"] <- "臺南市"  # 臺南縣改台南市
    X$TOWN[X$COUNTY=="高雄縣"] <- gsub("鄉$|鎮$|市$", "區", X$TOWN[X$COUNTY=="高雄縣"])  
    X$COUNTY[X$COUNTY=="高雄縣"] <- "高雄市"  # 高雄縣改高雄市
    # 2014/12/25桃園才由縣升級為直轄市
    X$TOWN[X$COUNTY=="桃園縣"] <- gsub("鄉$|鎮$|市$", "區", X$TOWN[X$COUNTY=="桃園縣"])
    X$COUNTY[X$COUNTY=="桃園縣"] <- "桃園市"  # 桃園縣改桃園市
    X$Name <- paste0(X$COUNTY, X$TOWN)
    X$Name[X$Name=="彰化縣員林鎮"] <- "彰化縣員林市"  # 2015/08/08
    X$Name[X$Name=="苗栗縣頭份鎮"] <- "苗栗縣頭份市"  # 2015/10/05
    X <- X[order(X$Name),]
    #assign(paste0("X",i), X)  # 檢查每組引入資料時使用
    if (i==1) {X.merge <- X[,-(length(X)-1)]} else {X.merge <- merge(X.merge, X[,-c(1:4,length(X)-1)], by="Name")}
  }
  X.merge[is.na(X.merge)] <- 0
  X.merge <- cbind(X.merge, Year)
  assign(paste0("X",Year), X.merge)
  if (Year==2008) {X.all <- X.merge} else {X.all <- rbind(X.all, X.merge)}
}

X.Data <- subset(X.all, X.all$COUNTY!="金門縣" & X.all$COUNTY!="連江縣")  # 去掉金馬地區
X.Data <- X.Data[,-c(2:5)]
X.Data$Area <- X.Data$P_CNT / X.Data$P_DEN
X.sub1 <-  X.Data[,c("Name","P_CNT","A15A64_CNT","H_CNT","O_CNT","Area","A0A14_CNT","A65UP_CNT","Year")]

for (year in 2011:2017) {
  floder <- paste0("D:/Demography_Data_Subset2/STAT",year,"06/")
  files <- list.files(path=floder, pattern="*\\.csv$")  # 檔案所屬資料夾與類型
  path <- paste0(floder, files)
  for (i in 1:length(path)) {
    X <- read.csv(path[i], header=TRUE, stringsAsFactors=FALSE)
    # 縣市鄉鎮升級時間表 https://zh.wikipedia.org/wiki/中華民國臺灣地區鄉鎮市區列表
    # 2014/12/25桃園才由縣升級為直轄市
    X$TOWN[X$COUNTY=="桃園縣"] <- gsub("鄉$|鎮$|市$", "區", X$TOWN[X$COUNTY=="桃園縣"])
    X$COUNTY[X$COUNTY=="桃園縣"] <- "桃園市"  # 桃園縣改桃園市
    X$Name <- paste0(X$COUNTY, X$TOWN)
    X$Name[X$Name=="彰化縣員林鎮"] <- "彰化縣員林市"  # 2015/08/08
    X$Name[X$Name=="苗栗縣頭份鎮"] <- "苗栗縣頭份市"  # 2015/10/05
    X <- X[order(X$Name),]
    #assign(paste0("X",i), X)  # 檢查每組引入資料時使用
    if (i==1) {X.merge <- X[,-(length(X)-1)]} else {X.merge <- merge(X.merge, X[,-c(1:4,length(X)-1)], by="Name")}
  }
  X.merge[is.na(X.merge)] <- 0
  X.merge <- cbind(X.merge, year)
  assign(paste0("X",year), X.merge)
  if (year==2011) {X.all <- X.merge} else {X.all <- rbind(X.all, X.merge)}
}

X.Data <- subset(X.all, X.all$COUNTY!="金門縣" & X.all$COUNTY!="連江縣")  # 去掉金馬地區
X.Data <- X.Data[,-c(2:5)]
X.sub2 <- X.Data[,c("Name", "RLP_CNT","H_BED","year")]

write.csv(X.sub1, "D:/Rresult/AnnData/AnnData_Pop_2008-2017.csv", row.names=F)
write.csv(X.sub2, "D:/Rresult/AnnData/AnnData_RLP+H_2011-2017.csv", row.names=F)

X.Pmean <- aggregate(. ~ Name, data = X.sub1, mean)
AnnData <- X.Pmean[,c("Name","Area")]
AnnData$P_DEN.ann.K <- X.Pmean$P_CNT / X.Pmean$Area / 1000
AnnData$H_CNT.ann.K <- X.Pmean$H_CNT / 1000
AnnData$A15A64.ann.prob <- X.Pmean$A15A64_CNT / X.Pmean$P_CNT
AnnData$O_CNT.ann.prob <- X.Pmean$O_CNT / X.Pmean$P_CNT
AnnData$A0A14_A15A64_RAT.ann <- X.Pmean$A0A14_CNT / X.Pmean$A15A64_CNT
AnnData$A65UP_A0A14_RAT.ann <- X.Pmean$A65UP_CNT / X.Pmean$A0A14_CNT

X.RHmean <- aggregate(. ~ Name, data = X.sub2, mean)
AnnData$RLP.ann.Kprob <- X.RHmean$RLP_CNT / X.Pmean$P_CNT *10
AnnData$H_SRVB.ann.K <- X.RHmean$H_BED / X.Pmean$P_CNT * 1000

write.csv(AnnData, "D:/Rresult/AnnData/AnnData_P+RLP+H_BasePeriod.csv", row.names=F)