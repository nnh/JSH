# JSH データカットのみ
# Mamiko Yonejima
# 2019/4/11
# 2020/05/13 Kumiko Agata JSHクリーニング用

day.shimekiri <- "20200621"
kYear <- "2019"
FileNameOutput <- "JSH_cleaning_DS.csv"
prtpath <- "//192.168.200.222/Datacenter/学会事務/130_日本血液学会/04.03.02 データ集計/2020/クリーニング/20200513"

rawdatapath <- paste0(prtpath, "/rawdata/")
jsh_report <- read.csv(paste0(rawdatapath, "JSH_report_200511_1121.csv"), na.strings = c(""), as.is=T, fileEncoding="CP932")
jsh.rgst <- read.csv(paste0(rawdatapath, "JSH_registration_200511_1121.csv"), na.strings = c(""), as.is=T, fileEncoding="CP932")

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
