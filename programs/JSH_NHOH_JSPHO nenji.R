# JSH, NHOH, JSPHO 団体集計 プログラム
# Mamiko Yonejima
# 2017/4/20
# 2019/7/10 更新

day.shimekiri <- "20190531"
kYear <- "2018"
prtpath <- "//192.168.200.222/Datacenter/学会事務/130_日本血液学会/04.03.02 データ集計/2019/集計/20190806"


rawdatapath <- paste0(prtpath, "/rawdata/")
jspho.rgst <- read.csv(paste0(rawdatapath, "JSPHO_registration_190806_0937.csv"), na.strings = c(""), as.is=T, fileEncoding="CP932")
jspho_outcome <- read.csv(paste0(rawdatapath, "JSPHO_190806_0937.csv"), na.strings = c(""), as.is=T, fileEncoding="CP932")
jsh_report <- read.csv(paste0(rawdatapath, "JSH_report_190801_1100.csv"), na.strings = c(""), as.is=T, fileEncoding="CP932")
jsh.rgst <- read.csv(paste0(rawdatapath, "JSH_registration_190801_1100.csv"), na.strings = c(""), as.is=T, fileEncoding="CP932")
jsh_outcome <- read.csv(paste0(rawdatapath, "JSH_190801_1100.csv"), na.strings = c(""), as.is=T, fileEncoding="CP932")
nhoh_report <- read.csv(paste0(rawdatapath, "NHOH_report_190801_1056.csv"), na.strings = c(""), as.is=T, fileEncoding="CP932")
nhoh.rgst <- read.csv(paste0(rawdatapath, "NHOH_registration_190801_1056.csv"), na.strings = c(""), as.is=T, fileEncoding="CP932")
nhoh_outcome <- read.csv(paste0(rawdatapath, "NHOH_190801_1056.csv"), na.strings = c(""), as.is=T, fileEncoding="CP932")

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
                 c("作成日", "登録コード", "性別", "SCSTRESC", "生死", "死亡日", "最終確認日", "field161", "MHDECOD",
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
# # 3団体を繋げた基本のデータセットを作成
dataset.3org <-  rbind(jsh.1, nhoh.1, jspho_ads) 

# age diagnosis
dataset.3org$age.diagnosis <- YearDif(dataset.3org$BRTHDTC, dataset.3org$MHSTDTC)

# count用に"1"を入力
dataset.3org$count <- 1

# 集計対象年のみ抽出
dataset.3org_yyyy <- dataset.3org[format(as.Date( dataset.3org$created.date), "%Y%m%d") <= day.shimekiri & as.integer(substr(dataset.3org$MHSTDTC, 1, 4)) == kYear, ]

# write.csv(dataset.3org_yyyy,  paste0(prtpath, "/output/dataset_3org.csv"), row.names = F)



# 疾患別集計
dxt.dataset.3org.year <- dataset.3org_yyyy
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
res.by.disease<- merge(wip.by.disease, Disease_Name_v2, by.x = "MHDECOD", by.y = "code", all = T )

# NA処理
res.by.disease[is.na(res.by.disease)] <- 0
write.csv(res.by.disease, paste0(prtpath, "/output/result_disease.csv"), row.names = F)

# 詳細集計用データの作成

## JSPHO
dxt.jspho <- jspho[, c(1, 2, 17:420)]
dxt.jspho$MDS染色体 <- "取得なし"
dxt.jspho$骨髄異形成関連変化随伴急性骨髄性白血病 <- "取得なし"
dxt.jspho$急性赤白血病 <- "取得なし"
dxt.jspho$AML詳細 <- "取得なし"
dxt.jspho$FAB分類 <- "取得なし"
dxt.jspho$ヘアリー細胞白血病 <- "取得なし"
dxt.jspho$多発性骨髄腫 <- "取得なし"
dxt.jspho$濾胞性リンパ腫 <- "取得なし"
dxt.jspho$濾胞性リンパ腫国際予後因子..FLIPI <- "取得なし"
dxt.jspho$びまん性大細胞型Ｂ細胞性リンパ腫 <- "取得なし"
dxt.jspho$血管内大細胞型Ｂ細胞性リンパ腫 <- "取得なし"
dxt.jspho$キャッスルマン <- "取得なし"
dxt.jspho$成人Ｔ細胞白血病リンパ腫 <- "取得なし"
dxt.jspho$腸管症関連Ｔ細胞リンパ腫 <- "取得なし"
dxt.jspho$末梢性Ｔ細胞リンパ腫 <- "取得なし"
dxt.jspho$HL付加事項 <- "取得なし"
dxt.jspho$HL.国際予後スコア.IPS. <- "取得なし"
dxt.jspho$免疫不全関連リンパ腫の場合 <- "取得なし"
dxt.jspho$二次性寒冷凝集素症 <- "取得なし"
dxt.jspho$ITP血小板数 <- "取得なし"
dxt.jspho$ITP.抗リン脂質抗体 <- "取得なし"
dxt.jspho$ヘパリン起因性血小板減少症 <- "取得なし"
dxt.jspho$ヘパリン起因性血小板減少症.抗HIT抗体. <- "取得なし"
dxt.jspho$凝固異常症.血友病A.インヒビター合併. <- "取得なし"
dxt.jspho$凝固異常症.血友病B.インヒビター合併. <- "取得なし"
dxt.jspho1 <- dxt.jspho[, c("登録コード", "CMLの細分類", "MDS染色体", "AML.染色体遺伝子",
                            "骨髄異形成関連変化随伴急性骨髄性白血病", "AML.FAB分類", "急性赤白血病",
                            "AML詳細", "血液腫瘍性.疾患名", "FAB分類", "ヘアリー細胞白血病", 
                            "多発性骨髄腫", "濾胞性リンパ腫", "濾胞性リンパ腫国際予後因子..FLIPI",
                            "びまん性大細胞型Ｂ細胞性リンパ腫", "血管内大細胞型Ｂ細胞性リンパ腫", "キャッスルマン", 
                            "成人Ｔ細胞白血病リンパ腫", "腸管症関連Ｔ細胞リンパ腫", 
                            "末梢性Ｔ細胞リンパ腫", "HL.Stage.Ann.Arbor.", "HL付加事項", "HL.国際予後スコア.IPS.", "免疫不全関連リンパ腫の場合",
                            "再生不良性貧血の重症度", "続発性赤芽球癆の場合.原疾患", "サラセミア", "温式自己免疫性溶血性貧血.AIHA.",  "温式自己免疫性溶血性貧血が二次性の場合.その原因",
                            "寒冷凝集素症", "二次性寒冷凝集素症",  "ビタミンB12欠乏性貧血の原因", "ビタミンB12欠乏性貧血の原因が内因子の欠乏の場合",
                            "葉酸欠乏性貧血の場合の原因", "鉄芽球性貧血", "慢性特発性血小板減少性紫斑病.診断時の血小板数", "慢性特発性血小板減少性紫斑病の場合.抗リン脂質抗体の有無",
                            "ヘパリン起因性血小板減少症",  "ヘパリン起因性血小板減少症.抗HIT抗体.", "凝固異常症.血友病A.インヒビター合併." ,"凝固異常症.血友病B.インヒビター合併.",
                            "抗リン脂質抗体症候群の分類", "抗リン脂質抗体症候群の場合.合併症", "無顆粒球症の原因")]
colnames(dxt.jspho1) <- c("登録コード", "CMLの細分類", "MDS染色体", "急性前骨髄球性白血病_染色体遺伝子","骨髄異形成関連変化随伴急性骨髄性白血病_詳細",
                          "AML.FAB分類", "急性赤白血病_詳細", "AML_詳細", "Tリンパ芽球性白血病_リンパ腫" ,"FAB分類", "ヘアリー細胞白血病",
                          "多発性骨髄腫_詳細",  "濾胞性リンパ腫_詳細","濾胞性リンパ腫国際予後因子_FLIPI", "びまん性大細胞型Ｂ細胞性リンパ腫_詳細",
                          "血管内大細胞型Ｂ細胞性リンパ腫_詳細", "キャッスルマン_詳細", "成人Ｔ細胞白血病リンパ腫_詳細" , "腸管症関連Ｔ細胞リンパ腫_詳細",
                          "末梢性Ｔ細胞リンパ腫_詳細", "HL.Stage.Ann.Arbor","HL付加事項", "HL.国際予後スコア.IPS.", "免疫不全関連リンパ腫の場合" ,
                          "再生不良性貧血の重症度", "続発性赤芽球癆の場合.原疾患", "サラセミア", "温式自己免疫性溶血性貧血.AIHA.",  "温式自己免疫性溶血性貧血が二次性の場合.その原因",
                          "寒冷凝集素症", "二次性寒冷凝集素症", "ビタミンB12欠乏性貧血の原因", "ビタミンB12欠乏性貧血の原因が内因子の欠乏の場合", 
                          "葉酸欠乏性貧血の場合の原因", "鉄芽球性貧血", "JSH.NHOH_ITP_血小板数.JSPHO_慢性特発性血小板減少性紫斑病の血小板数", 
                          "JSH.NHOH_ITP_抗リン脂質抗体.JSPHO_慢性特発性血小板減少性紫斑病の場合の抗リン脂質抗体","ヘパリン起因性血小板減少症", 
                          "ヘパリン起因性血小板減少症.抗HIT抗体.", "凝固異常症.血友病A.インヒビター合併." ,"凝固異常症.血友病B.インヒビター合併.", 
                          "抗リン脂質抗体症候群の分類", "抗リン脂質抗体症候群の場合.合併症", "無顆粒球症の原因")
syousai_jspho <- merge(jspho_ads, dxt.jspho1, by.x = "SUBJID", by.y = "登録コード", all.x = T)             
# ds.md.jspho[is.na(ds.md.jspho)] <- ""

## JSH
dxt.jsh <- m.jsh[, c(1, 2, 13:188)]
dxt.jsh$AML詳細 <- "取得なし"
dxt.jsh1 <- dxt.jsh[, c("登録コード", "CML病期", "MDS染色体", "APL", 
                        "AML.MRC", "AML.M5.", "AML.M6.",
                        "AML詳細", "Tリンパ芽球性白血病.リンパ腫", "FAB分類", "ヘアリーセル白血病.HCL.",
                        "多発性骨髄腫", "濾胞性リンパ腫", "濾胞性リンパ腫国際予後因子..FLIPI.",
                        "国際予後因子.IPI", "血管内B細胞リンパ腫.IVLBCL.", "キャッスルマン病", 
                        "ATLL", "EATL", 
                        "T.NK細胞腫瘍.PTCL", "Ann.Arbor.分類病期", "付加事項", "HL.国際予後スコア.IPS.", "免疫不全関連リンパ腫の場合",
                        "再生不良性貧血の重症度", "続発性赤芽球癆.原疾患.", "サラセミア.細分類.", "自己免疫性溶血性貧血AIHA", "二次性自己免疫性溶血性貧血AIHAの詳細",
                        "寒冷凝集素症", "二次性寒冷凝集素症", "巨赤芽球性貧血.ビタミンB12欠乏", "巨赤芽球性貧血.ビタミンB12欠乏.内因子の欠乏",
                        "巨赤芽球性貧血.葉酸欠乏" , "鉄芽球性貧血..Sideroblastic.anemia.SA.", "ITP..血小板数..μL.", "ITP.抗リン脂質抗体.",
                        "ヘパリン起因性血小板減少症",  "ヘパリン起因性血小板減少症.抗HIT抗体.",  "凝固異常症.血友病A.インヒビター合併.","凝固異常症.血友病B.インヒビター合併.",
                        "抗リン脂質抗体症候群", "抗リン脂質抗体症候群.合併症.", "無顆粒球症")]
colnames(dxt.jsh1) <- c("登録コード", "CMLの細分類", "MDS染色体", "急性前骨髄球性白血病_染色体遺伝子","骨髄異形成関連変化随伴急性骨髄性白血病_詳細",
                          "AML.FAB分類", "急性赤白血病_詳細", "AML_詳細", "Tリンパ芽球性白血病_リンパ腫" ,"FAB分類", "ヘアリー細胞白血病",
                          "多発性骨髄腫_詳細",  "濾胞性リンパ腫_詳細","濾胞性リンパ腫国際予後因子_FLIPI", "びまん性大細胞型Ｂ細胞性リンパ腫_詳細",
                          "血管内大細胞型Ｂ細胞性リンパ腫_詳細", "キャッスルマン_詳細", "成人Ｔ細胞白血病リンパ腫_詳細" , "腸管症関連Ｔ細胞リンパ腫_詳細",
                          "末梢性Ｔ細胞リンパ腫_詳細", "HL.Stage.Ann.Arbor","HL付加事項", "HL.国際予後スコア.IPS.", "免疫不全関連リンパ腫の場合",
                        "再生不良性貧血の重症度", "続発性赤芽球癆の場合.原疾患", "サラセミア", "温式自己免疫性溶血性貧血.AIHA.",  "温式自己免疫性溶血性貧血が二次性の場合.その原因",
                        "寒冷凝集素症", "二次性寒冷凝集素症", "ビタミンB12欠乏性貧血の原因", "ビタミンB12欠乏性貧血の原因が内因子の欠乏の場合", 
                        "葉酸欠乏性貧血の場合の原因", "鉄芽球性貧血", "JSH.NHOH_ITP_血小板数.JSPHO_慢性特発性血小板減少性紫斑病の血小板数", 
                        "JSH.NHOH_ITP_抗リン脂質抗体.JSPHO_慢性特発性血小板減少性紫斑病の場合の抗リン脂質抗体","ヘパリン起因性血小板減少症", 
                        "ヘパリン起因性血小板減少症.抗HIT抗体.", "凝固異常症.血友病A.インヒビター合併." ,"凝固異常症.血友病B.インヒビター合併.", 
                        "抗リン脂質抗体症候群の分類", "抗リン脂質抗体症候群の場合.合併症", "無顆粒球症の原因" )
syousai_jsh <- merge(jsh.1, dxt.jsh1, by.x = "SUBJID", by.y = "登録コード", all.x = T)    
# ds.md.jsh[is.na(ds.md.jsh)] <- ""

## NHOH
dxt.nhoh <- m.nhoh[, c(1, 2, 13:294)]
dxt.nhoh$骨髄異形成関連変化随伴急性骨髄性白血病 <- "取得なし"
dxt.nhoh$FAB分類 <- "取得なし"
dxt.nhoh$続発性赤芽球癆.原疾患. <- "取得なし"
dxt.nhoh$サラセミア <- "取得なし"
dxt.nhoh$自己免疫性溶血性貧血AIHA <- "取得なし"
dxt.nhoh$AIHA_二次性の場合の原因 <- "取得なし"
dxt.nhoh$寒冷凝集素症 <- "取得なし"
dxt.nhoh$二次性寒冷凝集素症 <- "取得なし"
dxt.nhoh$巨赤芽球性貧血.ビタミンB12欠乏 <- "取得なし"  
dxt.nhoh$巨赤芽球性貧血.ビタミンB12欠乏.内因子の欠乏 <- "取得なし"
dxt.nhoh$葉酸欠乏性貧血の場合の原因 <- "取得なし"
dxt.nhoh$鉄芽球性貧血 <- "取得なし"
dxt.nhoh$ヘパリン起因性血小板減少症 <- "取得なし"
dxt.nhoh$ヘパリン起因性血小板減少症.抗HIT抗体. <- "取得なし"
dxt.nhoh1 <- dxt.nhoh[, c("登録コード", "慢性骨髄増殖性白血病.CML...病期..MPN_4","骨髄異形成症候群..染色体.", "急性骨髄性白血病.APL.with.t.15.17..and.variantsの詳細" ,
                          "骨髄異形成関連変化随伴急性骨髄性白血病", "急性骨髄性白血病.FAB分類", "急性骨髄性白血病.Acute.erythroid.leukemiaの詳細",
                          "急性骨髄性白血病.染色体.遺伝子解析が不可能.発病形式.", "Tリンパ芽球性白血病.リンパ腫", "FAB分類", "Mature.B.cell.neoplasms.有毛細胞白血病.Variant.",
                          "Mature.B.cell.neoplasms.多発性骨髄腫の詳細.MB_4_3","Mature.B.cell.neoplasms.濾胞性リンパ腫.組織型.", "濾胞性リンパ腫国際予後因子..FLIPI.",
                          "国際予後因子.全年齢..International.Prognostic.Index.IPI.","Mature.B.cell.neoplasms.血管内B細胞リンパ腫.Variant.",  "Mature.B.cell.neoplasms.Castleman病.細分類.",
                          "T.NK細胞腫瘍.成人T細胞白血病.リンパ腫.病型..TNK_5_1", "T.NK細胞腫瘍.Enteropathy.associated.T.cell.lymphoma.病型.",
                          "T.NK細胞腫瘍.末梢性T細胞リンパ腫.特定不能.Variant.", "Ann.Arbor.分類病期", "付加事項", "HL.国際予後スコア.International.Prognostic.Score.IPS.", "Mature.B.cell.neoplasms.免疫不全関連リンパ腫の詳細",
                          "Aplastic.anemia.の重症度", "続発性赤芽球癆.原疾患.", "サラセミア", "自己免疫性溶血性貧血AIHA", "AIHA_二次性の場合の原因",
                          "寒冷凝集素症", "二次性寒冷凝集素症", "巨赤芽球性貧血.ビタミンB12欠乏", "巨赤芽球性貧血.ビタミンB12欠乏.内因子の欠乏",
                          "葉酸欠乏性貧血の場合の原因", "鉄芽球性貧血", "血小板減少症.特発性血小板減少性紫斑病.血小板数.", "血小板減少症.特発性血小板減少性紫斑病.抗リン脂質抗体.",
                          "ヘパリン起因性血小板減少症",  "ヘパリン起因性血小板減少症.抗HIT抗体.", "凝固異常症.血友病A.インヒビター合併.", "凝固異常症.血友病B.インヒビター合併.",
                          "血栓傾向.抗リン脂質抗体症候群.分類.",  "血栓傾向.抗リン脂質抗体症候群.合併症.","好中球減少症.無顆粒球症の詳細")]
colnames(dxt.nhoh1) <- c("登録コード", "CMLの細分類", "MDS染色体", "急性前骨髄球性白血病_染色体遺伝子","骨髄異形成関連変化随伴急性骨髄性白血病_詳細",
                        "AML.FAB分類", "急性赤白血病_詳細", "AML_詳細", "Tリンパ芽球性白血病_リンパ腫" ,"FAB分類", "ヘアリー細胞白血病",
                        "多発性骨髄腫_詳細",  "濾胞性リンパ腫_詳細","濾胞性リンパ腫国際予後因子_FLIPI", "びまん性大細胞型Ｂ細胞性リンパ腫_詳細",
                        "血管内大細胞型Ｂ細胞性リンパ腫_詳細", "キャッスルマン_詳細", "成人Ｔ細胞白血病リンパ腫_詳細" , "腸管症関連Ｔ細胞リンパ腫_詳細",
                        "末梢性Ｔ細胞リンパ腫_詳細", "HL.Stage.Ann.Arbor","HL付加事項", "HL.国際予後スコア.IPS.", "免疫不全関連リンパ腫の場合",
                        "再生不良性貧血の重症度", "続発性赤芽球癆の場合.原疾患", "サラセミア", "温式自己免疫性溶血性貧血.AIHA.",  "温式自己免疫性溶血性貧血が二次性の場合.その原因",
                        "寒冷凝集素症", "二次性寒冷凝集素症", "ビタミンB12欠乏性貧血の原因", "ビタミンB12欠乏性貧血の原因が内因子の欠乏の場合", 
                        "葉酸欠乏性貧血の場合の原因", "鉄芽球性貧血", "JSH.NHOH_ITP_血小板数.JSPHO_慢性特発性血小板減少性紫斑病の血小板数", 
                        "JSH.NHOH_ITP_抗リン脂質抗体.JSPHO_慢性特発性血小板減少性紫斑病の場合の抗リン脂質抗体","ヘパリン起因性血小板減少症", 
                        "ヘパリン起因性血小板減少症.抗HIT抗体.", "凝固異常症.血友病A.インヒビター合併." ,"凝固異常症.血友病B.インヒビター合併.", 
                        "抗リン脂質抗体症候群の分類", "抗リン脂質抗体症候群の場合.合併症", "無顆粒球症の原因" )
syousai_nhoh <- merge(nhoh.1, dxt.nhoh1, by.x = "SUBJID", by.y = "登録コード", all.x = T)   
#バインド
dataset.3org.syousai <- rbind(syousai_jspho, syousai_jsh, syousai_nhoh)
# age diagnosis
dataset.3org.syousai$age.diagnosis <- YearDif(dataset.3org.syousai$BRTHDTC, dataset.3org.syousai$MHSTDTC)
dataset.3org.syousai$cat.age.diagnosis <- cut(dataset.3org.syousai$age.diagnosis, breaks = c(0, 15, 20, 30, 40, 150),
                                               labels= c("0-14", "15-19", "20-29", "30-39", "40-"), right=FALSE)
# 集計対象年のみ抽出
dataset.3org.syousai <- dataset.3org.syousai[format(as.Date(dataset.3org.syousai$created.date), "%Y%m%d") <= day.shimekiri & as.integer(substr(dataset.3org.syousai$MHSTDTC, 1, 4)) == kYear, ]
# 
dataset.3org.syousai <- merge(dataset.3org.syousai, Disease_Name_v2, by.x = "MHDECOD", by.y = "code", all.x = T)
dataset.3org.syousai[is.na(dataset.3org.syousai)] <- ""

write.csv(dataset.3org.syousai, paste0(prtpath, "/output/JSH_NHOH_JSPHO_ads.csv"), row.names = F)
# write.csv(ds.md.jsh, paste0(prtpath, "/output/JSH_MoreDetails.csv"), row.names = F)
# write.csv(ds.md.nhoh, paste0(prtpath, "/output/NHOH_MoreDetails.csv"), row.names = F)

#write.csv(dataset.3org.syousai, paste0(prtpath, "/output/test.csv"), row.names = F)
# write.csv(jspho_ads, "./output/JSH-NHO-datacleaning-20180613.csv",row.names = F)

