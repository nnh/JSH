# JSPHO to WHO mapping 2017年集計用
# Mamiko Yonejima
# 2017/4/20
# 2019/7/10 更新

day.shimekiri <- "20190531"
kYear <- "2018"
prtpath <- "//192.168.200.222/Datacenter/学会事務/130_日本血液学会/04.03.02 データ集計/2019/集計/準備"


rawdatapath <- paste0(prtpath, "/rawdata/")
jspho.rgst <- read.csv(paste0(rawdatapath, "JSPHO_registration_190611_1505.csv"), na.strings = c(""), as.is=T, fileEncoding="CP932")
jspho_outcome <- read.csv(paste0(rawdatapath, "JSPHO_190611_1505.csv"), na.strings = c(""), as.is=T, fileEncoding="CP932")
jsh_report <- read.csv(paste0(rawdatapath, "JSH_report_190711_1616.csv"), na.strings = c(""), as.is=T, fileEncoding="CP932")
jsh.rgst <- read.csv(paste0(rawdatapath, "JSH_registration_190711_1616.csv"), na.strings = c(""), as.is=T, fileEncoding="CP932")
jsh_outcome <- read.csv(paste0(rawdatapath, "JSH_190711_1616.csv"), na.strings = c(""), as.is=T, fileEncoding="CP932")
nhoh_report <- read.csv(paste0(rawdatapath, "NHOH_report_190702_1917.csv"), na.strings = c(""), as.is=T, fileEncoding="CP932")
nhoh.rgst <- read.csv(paste0(rawdatapath, "NHOH_registration_190702_1917.csv"), na.strings = c(""), as.is=T, fileEncoding="CP932")
nhoh_outcome <- read.csv(paste0(rawdatapath, "NHOH_190702_1917.csv"), na.strings = c(""), as.is=T, fileEncoding="CP932")

list <- list.files(paste0(prtpath, "/input"))
df.name <- sub(".csv.*", "", list)  
for (i in 1:length(list)) {
  assign(df.name[i], read.csv(paste0(prtpath, "/input/", list[i]), as.is=T, na.strings = c(""), fileEncoding='UTF-8-BOM'))
}
# 関数の定義 ###############################################
YearDif <- function(starting, ending) {
  # 2つの日付の年差（切り下げ）を計算する。startingに生年月日を指定すれば満年齢計算に使用可能。
  as.integer((as.integer(format(as.Date(ending), "%Y%m%d")) - as.integer(format(as.Date(starting), "%Y%m%d"))) / 10000)
}
Sys.setlocale("LC_TIME", "C") #必須：日本時間にコンピュータ設定を合わせるforwindows

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
#------JSPHO---------
#性別、転帰をマージする処理(JSPHO)
dxt_jspho_outcome <- jspho_outcome[, c("登録コード", "生死", "死亡日", "最終確認日")]
jspho  <- merge(jspho.rgst, dxt_jspho_outcome, by = "登録コード", all.x = T)
jspho$year <- substr(jspho$診断年月日, 1, 4)  
jspho$SCSTRESC <- floor(as.integer(sub("^.*.-","",jspho$field173))/1000)

# STUDYID
jspho$STUDYID <- "JSPHO"
# WHO2006をWHO2016に変換
jspho$MHDECOD <- ifelse(nchar(jspho$field1) != 5, round(jspho$field1 * 10 + 10000, digits = 0)
                                    , jspho$field1)
jspho$MHDECOD <- ifelse(jspho$MHDECOD == 10930, 10931, jspho$MHDECOD)
jspho <- merge(jspho, Disease_Name_v2, by.x = "MHDECOD", by.y = "code", all.x = T)

# 診断年月日2012年以降、必要変数を抽出
jspho <- jspho[!(is.na(as.integer(jspho$year))), ]
jspho$age.diagnosis <- YearDif(jspho$生年月日, jspho$診断年月日)
jspho <- jspho[jspho$age.diagnosis < 20 , ]
jspho_ads <- jspho[as.integer(jspho$year) > 2011 & as.integer(jspho$year) <= kYear ,
                 c("作成日", "登録コード", "性別", "SCSTRESC", "生死", "死亡日", "最終確認日", "シート作成時施設コード", "MHDECOD",
                   "name_ja", "生年月日", "診断年月日", "STUDYID")]
colnames(jspho_ads)[1:12] <- c("created.date", "SUBJID", "SEX", "SCSTRESC", "DTHFL", "DTHDTC", "DSSTDTC", "SITEID", "MHDECOD", "MHTERM",
                            "BRTHDTC", "MHSTDTC")
#------NHOH---------
#施設コードをマージする処理(NHOH)
p.nhoh.rgst <- nhoh.rgst[,c("登録コード", "field7", "生年月日", "性別")]
p.nhoh.rgst$SCSTRESC <- floor(as.integer(sub("^.*.-","",p.nhoh.rgst$field))/1000)
dxt_nhoh_outcome <- nhoh_outcome[, c("登録コード", "生死", "死亡日", "最終確認日")]
m.nhoh_0 <- merge(nhoh_report,p.nhoh.rgst, by="登録コード", all.x= T)
m.nhoh <- merge(m.nhoh_0, dxt_nhoh_outcome, by="登録コード", all.x= T)
# STUDYID
m.nhoh$STUDYID <- "NHOH"
# WHO2006をWHO2016に変換
m.nhoh$MHDECOD <- ifelse(nchar(m.nhoh$field2) != 5, round(m.nhoh$field2 * 10 + 10000, digits = 0)
                        , m.nhoh$field2)
m.nhoh$MHDECOD <- ifelse(m.nhoh$MHDECOD == 10930, 10931, m.nhoh$MHDECOD)
m.nhoh <- merge(m.nhoh, Disease_Name_v2, by.x = "MHDECOD", by.y = "code", all.x = T)
# 診断年月日2012年以降、必要変数抽出
nhoh.1 <- m.nhoh[as.integer(substr(m.nhoh$診断年月日, 1, 4)) > 2011 & as.integer(substr(m.nhoh$診断年月日, 1, 4)) <= kYear ,
                 c("作成日", "登録コード", "性別", "SCSTRESC", "生死", "死亡日.y", "最終確認日", "シート作成時施設コード", "MHDECOD",
                   "name_ja", "生年月日", "診断年月日", "STUDYID")]
colnames(nhoh.1)[1:12] <- c("created.date", "SUBJID", "SEX", "SCSTRESC", "DTHFL", "DTHDTC", "DSSTDTC", "SITEID", "MHDECOD", "MHTERM",
                            "BRTHDTC", "MHSTDTC")
# BRTHDTC, MHSTDTCが逆転している症例を除く
nhoh.1 <- nhoh.1[(format(as.Date(nhoh.1$BRTHDTC), "%Y%m%d")) <=  (format(as.Date(nhoh.1$MHSTDTC), "%Y%m%d")), ]
#------JSH---------
#施設コードをマージする処理(JSH)
p.jsh.rgst <- jsh.rgst[, c("登録コード", "field114", "生年月日", "性別")]
p.jsh.rgst$SCSTRESC <- floor(as.integer(sub("^.*.-", "", p.jsh.rgst$field114))/1000)
dxt_jsh_outcome <- jsh_outcome[, c("登録コード", "生死", "死亡日", "最終確認日")]
m.jsh_0 <- merge(jsh_report, p.jsh.rgst, by = "登録コード", all.x = T)
m.jsh <- merge(m.jsh_0, dxt_jsh_outcome, by = "登録コード", all.x = T)
# STUDYID
m.jsh$STUDYID <- "JSH"
# WHO2006をWHO2016に変換
m.jsh$MHDECOD <- ifelse(nchar(m.jsh$field1) != 5, round(m.jsh$field1 * 10 + 10000, digits = 0)
                         , m.jsh$field1)
m.jsh$MHDECOD <- ifelse(m.jsh$MHDECOD == 10930, 10931, m.jsh$MHDECOD)
m.jsh <- merge(m.jsh, Disease_Name_v2, by.x = "MHDECOD", by.y = "code", all.x = T)
# 診断年月日2012年以降、腫瘍性病変のみを抽出
jsh.1 <- m.jsh[as.integer(substr(m.jsh$診断年月日, 1, 4)) > 2011 & as.integer(substr(m.jsh$診断年月日, 1, 4)) <= kYear ,
               c("作成日", "登録コード", "性別", "SCSTRESC", "生死", "死亡日", "最終確認日", "シート作成時施設コード", "MHDECOD",
                 "name_ja", "生年月日", "診断年月日", "STUDYID")]
colnames(jsh.1)[1:12] <- c("created.date", "SUBJID", "SEX", "SCSTRESC", "DTHFL", "DTHDTC", "DSSTDTC", "SITEID", "MHDECOD", "MHTERM",
                           "BRTHDTC", "MHSTDTC")
# BRTHDTC, MHSTDTCが逆転している症例を除く
# jsh.1 <- jsh.1[(format(as.Date(jsh.1$BRTHDTC), "%Y%m%d")) <= (format(as.Date(jsh.1$MHSTDTC), "%Y%m%d")), ]
jsh.1 <- subset(jsh.1, as.integer(as.integer(format(as.Date(jsh.1$MHSTDTC), "%Y%m%d")) - as.integer(format(as.Date(jsh.1$BRTHDTC), "%Y%m%d"))) >= 0)
# 3団体を繋げた基本のデータセットを作成
dataset.3org <-  rbind(jsh.1, nhoh.1, jspho_ads) 

# age diagnosis
dataset.3org$age.diagnosis <- YearDif(dataset.3org$BRTHDTC, dataset.3org$MHSTDTC)

# count用に"1"を入力
dataset.3org$count <- 1

#詳細集計用に出力
dataset.3org_yyyy <- dataset.3org[format(as.Date( dataset.3org$created.date), "%Y%m%d") <= day.shimekiri & as.integer(substr(dataset.3org$MHSTDTC, 1, 4)) == 2018, ]

write.csv(dataset.3org_yyyy,  paste0(prtpath, "/output/dataset_3org.csv"), row.names = F)

# 団体別登録数
# 施設数
dxt.dataset.3org.year <- dataset.3org_yyyy
by.org.c.facilities <- xtabs( ~ SITEID + STUDYID , data = dxt.dataset.3org.year )
by.org.c.facilities.df <- as.data.frame(by.org.c.facilities)
by.org.c.facilities.df$count <- ifelse(by.org.c.facilities.df$Freq == 0, 0, 1)
by.org.facilities <- xtabs( ~ STUDYID + count , data = by.org.c.facilities.df )
by.org.facilities.mat <- matrix(by.org.facilities , nrow(by.org.facilities), ncol(by.org.facilities))
rownames(by.org.facilities.mat) <- rownames(by.org.facilities)
colnames(by.org.facilities.mat) <- c("登録なし", "施設数")
# 登録数
by.org.np <- xtabs( ~ STUDYID + count , data = dxt.dataset.3org.year )
by.org.mat <- matrix(by.org.np, nrow(by.org.np), ncol(by.org.np))
rownames(by.org.mat) <- rownames(by.org.np)
colnames(by.org.mat) <- "登録数"
# 施設数と登録数をつなぐ
by.org <- cbind(by.org.facilities.mat, by.org.mat)
by.organization <- by.org[, c(2,3)]

res.by.organization <- data.frame(apply(by.organization, 2, function(d){ c(d, sum(d))}))  # 総計の行追加

# # 施設別登録数
# by.facilities <- xtabs( ~ SITEID + count , data = dxt.dataset.3org.year)
# by.facilities.df <- as.data.frame(by.facilities)
# by.facilities.df <- by.facilities.df[, c(1, 3)]
# colnames(by.facilities.df)[2] <- "登録数"
# by.facilities.np <- merge(by.facilities.df, facilities, by.x = "SITEID", by.y = "施設CD", all.x = T)
# dxt.by.facilities <- by.facilities.np[, c(1, 3, 2)]
# colnames(dxt.by.facilities)[2] <- "施設名"
# 
# total <- data.frame(
#   SITEID = "00000",
#   施設名 = "合計", 
#   登録数 = sum(dxt.by.facilities$登録数)
# )  # 合計の計算
# 
# res.by.facilities <- rbind(dxt.by.facilities, total)


# 疾患別集計
dxt.dataset.3org.year$cat.age.diagnosis <- cut(dxt.dataset.3org.year$age.diagnosis, breaks = c(0, 15, 20, 30, 40, 150),
                                               labels= c("0-14", "15-19", "20-29", "30-39", "40-"), right=FALSE)
by.disease <- xtabs(count ~ MHDECOD + cat.age.diagnosis, data = dxt.dataset.3org.year)
by.disease.mat <- matrix(by.disease , nrow(by.disease), ncol(by.disease))
colnames(by.disease.mat) <- c("0-14", "15-19", "20-29", "30-39", "40-")
rownames(by.disease.mat) <- rownames(by.disease)
sum <- apply(by.disease.mat, 1, sum)
wip.by.disease <- as.data.frame(cbind(by.disease.mat, sum))
wip.by.disease$MHDECOD <- rownames(by.disease)

#　病名コードとマージ
# dxt.disease <- disease[disease$大分類 == "hematology", ]  # 血液疾患のみ抽出
res.by.disease<- merge(wip.by.disease, Disease_Name_v2, , by.x = "MHDECOD", by.y = "code", all.x = T )

# NA処理
res.by.disease[is.na(res.by.disease)] <- ""
write.csv(res.by.disease, paste0(prtpath, "/output/result_disease.csv"), row.names = F)

# 詳細集計用データの作成
# JSPHO
ds.jspho <- dxt.dataset.3org.year[dxt.dataset.3org.year$STUDYID == "JSPHO", c(1:16)]
dxt.jspho <- jspho[, c(1, 2, 17:420)]
ds.md.jspho <- merge(ds.jspho, dxt.jspho, by.x = "SUBJID", by.y = "登録コード", all.x = T)
ds.md.jspho[is.na(ds.md.jspho)] <- ""
#JSH
ds.jsh <- dxt.dataset.3org.year[dxt.dataset.3org.year$STUDYID == "JSH", c(1:16)]
dxt.jsh <- m.jsh[, c(1, 2, 13:188)]
ds.md.jsh <- merge(ds.jsh, dxt.jsh, by.x = "SUBJID", by.y = "登録コード", all.x = T)
ds.md.jsh[is.na(ds.md.jsh)] <- ""
#NHOH
ds.nhoh <- dxt.dataset.3org.year[dxt.dataset.3org.year$STUDYID == "NHOH", c(1:16)]
dxt.nhoh <- m.nhoh[, c(1, 2, 13:294)]
ds.md.nhoh <- merge(ds.nhoh, dxt.nhoh, by.x = "SUBJID", by.y = "登録コード", all.x = T)
ds.md.nhoh[is.na(ds.md.nhoh)] <- ""

write.csv(ds.md.jspho, paste0(prtpath, "/output/JSPHO_MoreDetails.csv"), row.names = F)
write.csv(ds.md.jsh, paste0(prtpath, "/output/JSH_MoreDetails.csv"), row.names = F)
write.csv(ds.md.nhoh, paste0(prtpath, "/output/NHOH_MoreDetails.csv"), row.names = F)

#write.csv(jsh.1, paste0(prtpath, "/output/test.csv"), row.names = F)
# write.csv(jspho_ads, "./output/JSH-NHO-datacleaning-20180613.csv",row.names = F)

