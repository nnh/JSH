# JSH 削除症例カウント
# Mamiko Yonejima
# 2019/9/10

## 設定 #****************************
# Originalには削除前のデータ、Fixには集計し使用したデータを設定する
prtpath <- "//192.168.200.222/Datacenter/学会事務/130_日本血液学会/04.03.02 データ集計/2019/2019年度削除症例カウント"
kOriginalReport <- "JSH_report_190411_1039.csv"
kOriginalRegist <- "JSH_registration_190411_1039.csv"
kFixReport <- "JSH_report_190801_1100.csv"
kFixRegist <- "JSH_registration_190801_1100.csv"
#*************************************
kDxt <- function(dataframe){
  dataframe[, c("作成日", "登録コード", "シート作成時施設コード", "シート作成時施設名", "シート作成時診療科名", "シート作成時担当医" )]
}

# file読み込み
rawdatapath <- paste0(prtpath, "/rawdata/")
original_report <- read.csv(paste0(rawdatapath, kOriginalReport), na.strings = c(""), as.is=T, fileEncoding="CP932")
original_rgst <- read.csv(paste0(rawdatapath, kOriginalRegist), na.strings = c(""), as.is=T, fileEncoding="CP932")
fix_report <- read.csv(paste0(rawdatapath, kFixReport), na.strings = c(""), as.is=T, fileEncoding="CP932")
fix_rgst <- read.csv(paste0(rawdatapath, kFixRegist), na.strings = c(""), as.is=T, fileEncoding="CP932")
# 必要データのみ抽出
dxt_original_report <- kDxt(original_report)
colnames(dxt_original_report) <- paste0("削除前_", colnames(dxt_original_report))
dxt_fix_report <- kDxt(fix_report)
colnames(dxt_fix_report) <- paste0("削除後_", colnames(dxt_fix_report))
dxt_original_rgst <- kDxt(original_rgst)
colnames(dxt_original_rgst) <- paste0("削除前_", colnames(dxt_original_rgst))
dxt_fix_rgst <- kDxt(fix_rgst)
colnames(dxt_fix_rgst) <- paste0("削除後_", colnames(dxt_fix_rgst))
# Originalをtrueにしてマージ
marge_report <- merge(dxt_original_report, dxt_fix_report, by.x = "削除前_登録コード", by.y = "削除後_登録コード", all.x = T)
marge_rgst <- merge(dxt_original_rgst, dxt_fix_rgst, by.x = "削除前_登録コード", by.y = "削除後_登録コード", all.x = T)

write.csv(marge_report, paste0(prtpath, "/output/report.csv"), row.names = F)
write.csv(marge_rgst, paste0(prtpath, "/output/rgst.csv"), row.names = F)
