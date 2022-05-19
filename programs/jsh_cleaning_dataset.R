# JSH データカットのみ
# Mamiko Yonejima
# 2019/4/11
# 2021/06/24 Kumiko Agata JSH集計クリーニング
# 2020/06/15 Kumiko Agata NHOH集計クリーニング
# 2022/5/20 Mamiko Yonejima JSH集計クリーニング,NHOH集計クリーニング

day.shimekiri <- "20220520"
kYear <- "2021"
library(tidyverse)

#JSHの場合はコメント解除
FileNameOutput <- "JSH_cleaning_DS.csv"
prtpath <- "C:/Users/MamikoYonejima/Box/Datacenter/Trials/JSH/Registry/10.03.10 データレビュー書/2021年診断/クリーニング/20220509"
rawdatapath <- paste0(prtpath, "/rawdata/")
# jsh_report <- read_csv(paste0(rawdatapath, "JSH_report_220509_0819.csv"), na.strings = c(""), as.is=T, fileEncoding="CP932")
# jsh.rgst <- read.csv(paste0(rawdatapath, "JSH_registration_220509_0819.csv"), na.strings = c(""), as.is=T, fileEncoding="CP932")
jsh_report <- read_csv(paste0(rawdatapath, "JSH_report_220509_0819.csv")) # tidyverseパッケージのread_csvを使用
jsh.rgst <- read_csv(paste0(rawdatapath, "JSH_registration_220509_0819.csv")) # tidyverseパッケージのread_csvを使用

#NHOHの場合はコメント解除
# FileNameOutput <- "NHOH_cleaning_DS.csv"
# prtpath <- "C:/Users/MamikoYonejima/Box/Datacenter/Trials/JSH/Registry/10.03.10 データレビュー書/2021年診断/クリーニング/20220509"
# rawdatapath <- paste0(prtpath, "/rawdata/")
# jsh_report <- read.csv(paste0(rawdatapath, "NHOH_report_210601_1114.csv"), na.strings = c(""), as.is=T, fileEncoding="CP932")
# jsh.rgst <- read.csv(paste0(rawdatapath, "NHOH_registration_210601_1114.csv"), na.strings = c(""), as.is=T, fileEncoding="CP932")

# jsh_report <- read_csv(paste0(rawdatapath, "NHOH_report_220509_1015.csv"))   # tidyverseパッケージのread_csvを使用
# jsh.rgst <- read_csv(paste0(rawdatapath, "NHOH_registration_220509_1015.csv"))  # tidyverseパッケージのread_csvを使用

###########重複データ確認###################################
duplicate <- jsh_report$登録コード[duplicated(jsh_report$登録コード)]
# grep(191414, jsh_report$登録コード) #重複している登録番号を記載
# grep(191417, jsh_report$登録コード)
# grep(191422, jsh_report$登録コード)
# grep(196549, jsh_report$登録コード)
# jsh_report <- jsh_report[- c(184381, 184384, 184390, 190449), ]  # 重複が確認された場合、その行番号を入力
# インシデントにより削除され代理入力した症例
# add_data <- 164062
############################################################
dxt_jsh_report <- subset(jsh_report, format(as.Date(jsh_report$作成日), "%Y%m%d") <= day.shimekiri) # 作成日カット
dxt2_jsh_report <- subset(dxt_jsh_report, substr(dxt_jsh_report$診断年月日, 1, 4) == kYear)  # 診断年のみ抽出
colnames(jsh.rgst) <- paste0("registration_", colnames(jsh.rgst))
cleaning_ds <- merge(jsh.rgst, dxt2_jsh_report, by.x = "registration_登録コード", by.y = "登録コード", all.y = T)
cleaning_ds[is.na(cleaning_ds)] <- ""
outputpath <- paste0(prtpath, "/output/")
write.csv(cleaning_ds, paste0(outputpath, FileNameOutput), row.names = F)
