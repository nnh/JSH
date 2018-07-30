# JSPHO to WHO mapping 2017年集計用
# Mamiko Yonejima
# 2017/4/20
day.shimekiri <- "20180531"
# jspho_exclusion <- "340212686"  # JSPHO参加施設外の保険医療機関コードを入力
kYear <- "2017"
prtpath <- "//192.168.200.222/Datacenter/学会事務/130_日本血液学会/データ集計/2018/201807"


rawdatapath <- paste0(prtpath, "/rawdata/")
jspho.rgst <- read.csv(paste0(rawdatapath, "JSPHO_registration_180726_1506.csv"), na.strings = c(""), as.is=T, fileEncoding="CP932")
jspho_outcome <- read.csv(paste0(rawdatapath, "JSPHO_180726_1506.csv"), na.strings = c(""), as.is=T, fileEncoding="CP932")
jsh_report <- read.csv(paste0(rawdatapath, "JSH_report_180717_1530.csv"), na.strings = c(""), as.is=T, fileEncoding="CP932")
jsh.rgst <- read.csv(paste0(rawdatapath, "JSH_registration_180717_1530.csv"), na.strings = c(""), as.is=T, fileEncoding="CP932")
jsh_outcome <- read.csv(paste0(rawdatapath, "JSH_180717_1530.csv"), na.strings = c(""), as.is=T, fileEncoding="CP932")
nhoh_report <- read.csv(paste0(rawdatapath, "NHOH_report_180726_1459.csv"), na.strings = c(""), as.is=T, fileEncoding="CP932")
nhoh.rgst <- read.csv(paste0(rawdatapath, "NHOH_registration_180726_1459.csv"), na.strings = c(""), as.is=T, fileEncoding="CP932")
nhoh_outcome <- read.csv(paste0(rawdatapath, "NHOH_180726_1459.csv"), na.strings = c(""), as.is=T, fileEncoding="CP932")

disease <- read.csv(paste0(prtpath, "./input/disease_20170721.csv"), fileEncoding="UTF-8-BOM", header=F, as.is=T, na.strings = c(""))  
colnames(disease) <- c("大分類", "MHDECOD", "病名略", "MHTERM", "病名英語", "MHDECODplus.0", "中分類番号1", "中分類略名" ,"中分類名日本語" ,"中分類名英語", "tumor_or_nontumor", "中分類番号2")
facilities <- read.csv(paste0(prtpath, "./input/facilities.csv"), fileEncoding="UTF-8-BOM", header=T, as.is=T)  

###########重複データ確認###################################
duplicate <- jsh_report$登録コード[duplicated(jsh_report$登録コード)]  
# grep(191414, jsh_report$登録コード) #重複している登録番号を記載
# grep(191417, jsh_report$登録コード)
# grep(191422, jsh_report$登録コード)
# grep(196549, jsh_report$登録コード)
jsh_report <- jsh_report[- c(184381, 184384, 184390, 190449), ]  # 重複が確認された場合、その行番号を入力
# インシデントにより削除され代理入力した症例
add_data <- 164062
############################################################
#性別、転帰をマージする処理(JSPHO)
dxt_jspho_outcome <- jspho_outcome[, c("登録コード", "生死", "死亡日", "最終確認日")]
jspho  <- merge(jspho.rgst, dxt_jspho_outcome, by = "登録コード", all.x = T)

source("./programs/jsphotowho.R", chdir=F, encoding="UTF-8")
source("./programs/jsphotowho_nontumor.R", chdir=F, encoding="UTF-8")
# # BRTHDTC, MHSTDTCが逆転している症例を除く
jspho.1 <- jspho.1[(format(as.Date(jspho.1$BRTHDTC), "%Y%m%d")) <=  (format(as.Date(jspho.1$MHSTDTC), "%Y%m%d")), ]
jspho.non.t.1 <- jspho.non.t.1[(format(as.Date(jspho.non.t.1$BRTHDTC), "%Y%m%d")) <=  (format(as.Date(jspho.non.t.1$MHSTDTC), "%Y%m%d")), ]
#施設コードをマージする処理(NHOH)
p.nhoh.rgst <- nhoh.rgst[,c("登録コード", "初発時住所", "生年月日", "性別")]
p.nhoh.rgst$SCSTRESC <- floor(as.integer(sub("^.*.-","",p.nhoh.rgst$初発時住所))/1000)
dxt_nhoh_outcome <- nhoh_outcome[, c("登録コード", "生死", "死亡日", "最終確認日")]
m.nhoh_0 <- merge(nhoh_report,p.nhoh.rgst, by="登録コード", all.x= T)
m.nhoh <- merge(m.nhoh_0, dxt_nhoh_outcome, by="登録コード", all.x= T)

# 診断年月日2012年以降、腫瘍性病変のみを抽出
nhoh.1 <- m.nhoh[as.integer(substr(m.nhoh$診断年月日, 1, 4)) > 2011 & as.integer(substr(m.nhoh$診断年月日, 1, 4)) <= 2017 ,
                 c("作成日", "登録コード", "性別", "SCSTRESC", "生死", "死亡日.y", "最終確認日", "シート作成時施設コード", "field2",
                   "確定診断名", "生年月日", "診断年月日")]
colnames(nhoh.1)[1:12] <- c("created.date", "SUBJID", "SEX", "SCSTRESC", "DTHFL", "DTHDTC", "DSSTDTC", "SITEID", "MHDECOD", "MHTERM",
                            "BRTHDTC", "MHSTDTC")
# BRTHDTC, MHSTDTCが逆転している症例を除く
nhoh.1 <- nhoh.1[(format(as.Date(nhoh.1$BRTHDTC), "%Y%m%d")) <=  (format(as.Date(nhoh.1$MHSTDTC), "%Y%m%d")), ]
nhoh.1$STUDYID <- "NHOH"

#施設コードをマージする処理(JSH)
p.jsh.rgst <- jsh.rgst[, c("登録コード", "初発時住所", "生年月日", "性別")]
p.jsh.rgst$SCSTRESC <- floor(as.integer(sub("^.*.-", "", p.jsh.rgst$初発時住所))/1000)
dxt_jsh_outcome <- jsh_outcome[, c("登録コード", "生死", "死亡日", "最終確認日")]
m.jsh_0 <- merge(jsh_report, p.jsh.rgst, by = "登録コード", all.x = T)
m.jsh <- merge(m.jsh_0, dxt_jsh_outcome, by = "登録コード", all.x = T)

# 診断年月日2012年以降、腫瘍性病変のみを抽出
jsh.1 <- m.jsh[as.integer(substr(m.jsh$診断年月日, 1, 4)) > 2011 & as.integer(substr(m.jsh$診断年月日, 1, 4)) <= 2017 ,
               c("作成日", "登録コード", "性別", "SCSTRESC", "生死", "死亡日", "最終確認日", "シート作成時施設コード", "field1",
                 "確定診断名", "生年月日", "診断年月日")]
colnames(jsh.1)[1:12] <- c("created.date", "SUBJID", "SEX", "SCSTRESC", "DTHFL", "DTHDTC", "DSSTDTC", "SITEID", "MHDECOD", "MHTERM",
                           "BRTHDTC", "MHSTDTC")
# BRTHDTC, MHSTDTCが逆転している症例を除く
jsh.1 <- jsh.1[(format(as.Date(jsh.1$BRTHDTC), "%Y%m%d")) <=  (format(as.Date(jsh.1$MHSTDTC), "%Y%m%d")), ]

jsh.1$STUDYID <- "JSH"
# 3団体を繋げた基本のデータセットを作成
jspho.1$age.diagnosis <- YearDif(jspho.1$BRTHDTC, jspho.1$MHSTDTC)
jspho.non.t.1$age.diagnosis <- YearDif(jspho.non.t.1$BRTHDTC, jspho.non.t.1$MHSTDTC)
jsh.1$age.diagnosis <- YearDif(jsh.1$BRTHDTC, jsh.1$MHSTDTC)
nhoh.1$age.diagnosis <- YearDif(nhoh.1$BRTHDTC, nhoh.1$MHSTDTC)

# jspho.1 <- jspho.1[jspho.1$SITEID != jspho_exclusion, ]  #JSPHOの参加外施設を除外
dataset.3org <-  rbind(jsh.1, nhoh.1, jspho.1, jspho.non.t.1) 
# 3団体を繋げた基本のデータセットを作成
# dataset.3org$age.diagnosis <- YearDif(dataset.3org$BRTHDTC, dataset.3org$MHSTDTC)
dxt.dataset.3org.year.0 <- dataset.3org[(format(as.Date(dataset.3org$created.date), "%Y%m%d") <= day.shimekiri) && dataset.3org$SUBJID != add_data , ] 
dxt.dataset.3org.year.1 <- subset(dxt.dataset.3org.year.0, substr(dxt.dataset.3org.year.0$MHSTDTC, 1, 4) == kYear)  # 診断年のみ抽出
# # BRTHDTC, MHSTDTCが逆転している症例を除く
dxt.dataset.3org.year <- subset(dxt.dataset.3org.year.1, as.Date(dxt.dataset.3org.year.1$BRTHDTC) <= as.Date(dxt.dataset.3org.year.1$MHSTDTC))
## incidentの症例を追加
# add <- dataset.3org[dataset.3org$SUBJID == add_data, ]
# dxt.dataset.3org.year <- rbind(dxt.dataset.3org.year, add)


dxt.dataset.3org.year$count <- 1

#詳細集計用に出力
write.csv(dxt.dataset.3org.year,  paste0(prtpath, "/output/dataset_3org.csv"), row.names = F)

# 団体別登録数
# 施設数
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

# 施設別登録数
by.facilities <- xtabs( ~ SITEID + count , data = dxt.dataset.3org.year)
by.facilities.df <- as.data.frame(by.facilities)
by.facilities.df <- by.facilities.df[, c(1, 3)]
colnames(by.facilities.df)[2] <- "登録数"
by.facilities.np <- merge(by.facilities.df, facilities, by.x = "SITEID", by.y = "施設CD", all.x = T)
dxt.by.facilities <- by.facilities.np[, c(1, 3, 2)]
colnames(dxt.by.facilities)[2] <- "施設名"

total <- data.frame(
  SITEID = "00000",
  施設名 = "合計", 
  登録数 = sum(dxt.by.facilities$登録数)
)  # 合計の計算

res.by.facilities <- rbind(dxt.by.facilities, total)


# 疾患別集計
dxt.dataset.3org.year$cat.age.diagnosis <- cut(dxt.dataset.3org.year$age.diagnosis, breaks = c(0,20,150),
                                               labels= c("20歳未満", "20歳以上"), right=FALSE)
by.disease <- xtabs(count ~ MHDECOD + cat.age.diagnosis, data = dxt.dataset.3org.year)
by.disease.mat <- matrix(by.disease , nrow(by.disease), ncol(by.disease))
colnames(by.disease.mat) <- c("20歳未満", "20歳以上")
rownames(by.disease.mat) <- rownames(by.disease)
sum <- apply(by.disease.mat, 1, sum)
wip.by.disease <- as.data.frame(cbind(by.disease.mat, sum))
wip.by.disease$MHDECOD <- rownames(by.disease)

#　病名コードとマージ
dxt.disease <- disease[disease$大分類 == "hematology", ]  # 血液疾患のみ抽出
wip.merge.disease <- merge(dxt.disease, wip.by.disease, by = "MHDECOD", all.x = T )
res.by.disease <- wip.merge.disease[, c(7, 1, 9, 4, 13:15)]

# NA処理
res.by.organization[is.na(res.by.organization)] <- ""
res.by.facilities[is.na(res.by.facilities)] <- ""
res.by.disease[is.na(res.by.disease)] <- 0
# 成型された表の出力
library(formattable)
res.by.organization -> temp
formattable::formattable(temp)
res.by.facilities -> temp
formattable::formattable(temp)  # Viewer > Export > Save as WebPage
res.by.disease -> temp
formattable::formattable(temp)

# 詳細集計用データの作成
# JSPHO
ds.jspho <- dxt.dataset.3org.year[dxt.dataset.3org.year$STUDYID == "JSPHO", c(2:16)]
dxt.jspho <- jspho[, c(1, 146, 147, 150:393)]
ds.md.jspho <- merge(ds.jspho, dxt.jspho, by.x = "SUBJID", by.y = "登録コード", all.x = T)
ds.md.jspho[is.na(ds.md.jspho)] <- ""
#JSH
ds.jsh <- dxt.dataset.3org.year[dxt.dataset.3org.year$STUDYID == "JSH", c(2:16)]
dxt.jsh <- m.jsh[, c(1, 16:178)]
ds.md.jsh <- merge(ds.jsh, dxt.jsh, by.x = "SUBJID", by.y = "登録コード", all.x = T)
ds.md.jsh[is.na(ds.md.jsh)] <- ""
#NHOH
ds.nhoh <- dxt.dataset.3org.year[dxt.dataset.3org.year$STUDYID == "NHOH", c(2:16)]
dxt.nhoh <- m.nhoh[, c(1, 28:277)]
ds.md.nhoh <- merge(ds.nhoh, dxt.nhoh, by.x = "SUBJID", by.y = "登録コード", all.x = T)
ds.md.nhoh[is.na(ds.md.nhoh)] <- ""

write.csv(ds.md.jspho, paste0(prtpath, "/output/JSPHO_MoreDetails.csv"), row.names = F)
write.csv(ds.md.jsh, paste0(prtpath, "/output/JSH_MoreDetails.csv"), row.names = F)
write.csv(ds.md.nhoh, paste0(prtpath, "/output/NHOH_MoreDetails.csv"), row.names = F)

# jsh.1 <- jsh.1[(format(as.Date(jsh.1$created.date), "%Y%m%d") <= day.shimekiri) & (substr(jsh.1$MHSTDTC, 1, 4) == kYear), ]
# nhoh.1 <- nhoh.1[(format(as.Date(nhoh.1$created.date), "%Y%m%d") <= day.shimekiri) & (substr(nhoh.1$MHSTDTC, 1, 4) == kYear), ] # 診断年のみ抽出
# result <- rbind(jsh.1, nhoh.1)
# write.csv(result, "./output/JSH-NHO-datacleaning-20180613.csv",row.names = F)
