# JSH, NHOH, JSPHO 団体集計 プログラム
# Mamiko Yonejima
# 2017/4/20 作成
# 2021/7/26 集計のため更新（Agata.K）
# 2021/7/29 WHO2016にWHO2008が混在している為、両方に対応出来るように修正（JSHのみ）
# 2022/8/16csvのread方法変更に伴うコード修正、重複症例削除処理の追加（Agata.K）
# 2023/8/10 2023年度集計(Agata.K)
# 2024/10/23 2024年度集計(Agata.K)

library(tidyverse) # read_csv利用の為のライブラリ(2022/7/28 Agata.K)

date.cutoff <- "20240531" # データ固定日
kYear <- "2023"           # 集計する診断年
flag <- 1                 # WHO2016で集計する場合は1を入力、WHO2008で集計する集計する場合は2を入力
# programを保管しているパス
prtpath <- "C:/Users/c0002392/work/GIT/JSH/work/JSH_NHOH_JSPHO nenji"
kToday <- Sys.Date()

# rawdataフォルダ内のファイル読込（tidyverseパッケージのread_csvを使用）(2022/7/28 Agata.K)
rawdatapath <- paste0(prtpath, "/rawdata/") # DLデータ保管フォルダ
jspho_rgst <- read_csv(paste0(rawdatapath, "JSPHO_registration_240801_1352.csv")) # JSPHOのDLデータ読込
jspho_outcome <- read_csv(paste0(rawdatapath, "JSPHO_220722_1533.csv"))

jsh_report <- read_csv(paste0(rawdatapath, "JSH_report_240701_0834.csv"))         # JSHのDLデータ読込
jsh.rgst <- read_csv(paste0(rawdatapath, "JSH_registration_240701_0834.csv"))
jsh_outcome <- read_csv(paste0(rawdatapath, "JSH_220701_0827.csv"))

# nhoh_report <- read_csv(paste0(rawdatapath, "NHOH_report_230703_1201.csv"))       # NHOHのDLデータ読込
# nhoh.rgst <- read_csv(paste0(rawdatapath, "NHOH_registration_230703_1201.csv"))
# nhoh_outcome <- read_csv(paste0(rawdatapath, "NHOH_220701_1207.csv"))

# inputフォルダ内のファイルを読込（Disease_Name_v2.csv、mhcod_20161006.csv、WHO2008.csv、Duplicate.csv）(2022/7/28 Agata.K)
list <- list.files(paste0(prtpath, "/input"))
df.name <- sub(".csv.*", "", list)
for (i in 1:length(list)) {
  assign(df.name[i], read_csv(paste0(prtpath, "/input/", list[i])))
}

# 関数の定義 ###############################################
YearDif <- function(starting, ending) {
  # 2つの日付の年差（切り下げ）を計算する。startingに生年月日を指定すれば満年齢計算に使用可能。
  as.integer((as.integer(format(as.Date(ending), "%Y%m%d")) - as.integer(format(as.Date(starting), "%Y%m%d"))) / 10000)
}
Sys.setlocale("LC_TIME", "C") #必須：日本時間にコンピュータ設定を合わせるforwindows

# 重複データ削除###################################
# duplicate <- jsh_report$登録コード[duplicated(jsh_report$登録コード)]
# grep(191414, jsh_report$登録コード) #重複している登録番号を記載
# grep(191417, jsh_report$登録コード)
# grep(191422, jsh_report$登録コード)
# grep(196549, jsh_report$登録コード)
# jsh_report <- jsh_report[- c(184381, 184384, 184390, 190449), ]  # 重複が確認された場合、その行番号を入力
# インシデントにより削除され代理入力した症例
# add_data <- 164062

# 2019年診断対応（2020/08/07 Agata.K）
# JSPHO 2020/6/22以降のデータと参加外施設（30319、30323）の情報を削除する
# jspho_rgst <- jspho_rgst[jspho_rgst$作成日 <= "2020/06/21",]
# jspho_rgst <- jspho_rgst[jspho_rgst$登録コード != "30319",]
# jspho_rgst <- jspho_rgst[jspho_rgst$登録コード != "30323",]

# 2020年診断集計対応（2021/7/26 Agata.K）
# jspho_rgst <- jspho_rgst[jspho_rgst$作成日 <= "2021/05/31",]  # JSPHO 2021/6/1以降削除
# nhoh_report <- nhoh_report[nhoh_report$登録コード != "37037",]  #NHOH 37037を削除（JSHと重複の為）
# nhoh_report <- nhoh_report[nhoh_report$登録コード != "38525",]  #NHOH 38525を削除（JSHと重複の為）
# nhoh_report <- nhoh_report[nhoh_report$登録コード != "37849",]  #NHOH 37849を削除（JSPHOと重複の為）
# jsh_report <- jsh_report[jsh_report$登録コード != "310144",]  #JSH 310144を削除（参加外施設の為）

# 2021年診断集計対応。Duplicate.csvの症例を削除する（2022/8/16 Agata.K)
# for (j in 1: length(Duplicate$解析に使用する症例)){

#   #JSHの症例を削除
#   if(Duplicate$解析に使用する症例[j] != "JSH" && is.na(Duplicate$JSH[j]) == FALSE ) {
#     jsh_report <- jsh_report[jsh_report$登録コード!= Duplicate$JSH[j] ,]
#   }

#   #NHOHの症例を削除
#   if(Duplicate$解析に使用する症例[j] != "NHOH" && is.na(Duplicate$NHOH[j]) == FALSE){
#     nhoh_report <- nhoh_report[nhoh_report$登録コード != Duplicate$NHOH[j] ,]
#   }

#   #JSPHOの症例を削除
#   if(Duplicate$解析に使用する症例[j] != "JSPHO" && is.na(Duplicate$JSPHO[j]) == FALSE){
#     jspho_rgst <- jspho_rgst[jspho_rgst$登録コード != Duplicate$JSPHO[j] ,]
#   }
# }

# # 2症例削除（2021年診断だけ）2022.08.23 Agata.K
# jsh_report <- jsh_report[jsh_report$登録コード != "337116",]  #JSH 337116を削除（参加外施設の為）
# jsh_report <- jsh_report[jsh_report$登録コード != "345069",]  #JSH 345069を削除（参加外施設の為）

# 2症例削除（2021年診断だけ）2022.08.23 Agata.K
# jsh_report <- jsh_report[jsh_report$登録コード != "426094",]    #JSH 426094を削除（参加外施設の為）
# nhoh_report <- nhoh_report[nhoh_report$登録コード != "43558",]  #NHOH 43558を削除（参加外施設の為）
############################################################

# 各団体の行数（症例数）を取得
jspho_total <- nrow(jspho_rgst)
jsh_total <- nrow(jsh_report)
# nho_total <- nrow(nhoh_report)

#------JSPHO---------
#性別、転帰をマージする処理(JSPHO)
dxt_jspho_outcome <- jspho_outcome[, c("登録コード", "生死", "死亡日", "最終確認日")]

#JSPHOの診断名が空値を埋める
# 2018/5/30までに登録されたグループと、2018/5/30以降に登録されたグループに分ける
# jspho.rgst$診断年月日が空値、または作成日が空値の症例を除外する
dropout_emp_year <- nrow(jspho_rgst[is.na(jspho_rgst$診断年月日), ])
jspho_rgst2 <- jspho_rgst[!is.na(jspho_rgst$診断年月日), ]
dropout_emp_cdate <- nrow(jspho_rgst2[is.na(jspho_rgst2$作成日), ])
jspho.rgst <- jspho_rgst2[!is.na(jspho_rgst2$作成日), ]

jspho.rgst$year <- as.integer(substr(jspho.rgst$診断年月日, 1, 4))
before201806_jspho <- subset(jspho.rgst, jspho.rgst$作成日 <= "2018/05/31")
after201806_jspho <- subset(jspho.rgst, jspho.rgst$作成日 >= "2018/06/01")

# 2018/6/1までの登録症例のグループに対しては、フィールドの入力値からWHO2008分類の病名を当てはめる
before201806_jspho$flag <- ifelse(before201806_jspho$field7 == 2 | (before201806_jspho$field7 == 1 & before201806_jspho$field37 == 8 & before201806_jspho$field69 == 2), "non_tumor", "tumor")
df.tumor <- subset(before201806_jspho, before201806_jspho$flag == "tumor")

# df.tumor <- df.tumor[, -16]
df.tumor$MHDECOD1 <- ifelse((df.tumor$field7 == 1 & df.tumor$field37 == 2 & df.tumor$field10 == 1) | (df.tumor$field7 == 1 & df.tumor$field37 == 2 & df.tumor$field10 == 2), 53,
                            ifelse(df.tumor$field7 == 1 & df.tumor$field37 == 10, 52,
                                   ifelse((df.tumor$field37 == 1 & df.tumor$field20 == 6) | (df.tumor$field37 == 1 & df.tumor$field20 == 7) | (df.tumor$field37 == 1 & df.tumor$field20 == 8), 65,
                                          ifelse((df.tumor$field37 == 1 & df.tumor$field20 == 1) | (df.tumor$field37 == 1 & df.tumor$field20 == 2) | (df.tumor$field37 == 1 & df.tumor$field20 == 3), 66,
                                                 ifelse(df.tumor$field37 == 1 & df.tumor$field19 == 2, 62,
                                                        ifelse((df.tumor$field37 == 1 & df.tumor$field19 == 4) | (df.tumor$field37 == 1 & df.tumor$field19 == 5) | (df.tumor$field37 == 1 & df.tumor$field19 == 6) |(df.tumor$field37 == 1 & df.tumor$field19 == 7), 63,
                                                               ifelse(df.tumor$field37 == 1 & df.tumor$field19 == 3, 64,
                                                                      ifelse(df.tumor$field37 == 1 & df.tumor$field19 == 14, 67,
                                                                             ifelse(df.tumor$field37 == 1 & df.tumor$field19 == 8, 68,
                                                                                    ifelse((df.tumor$field37 == 1 & df.tumor$field17 == 1) | (df.tumor$field37 == 5 & df.tumor$field55 == 2), 61,
                                                                                           ifelse((df.tumor$field37 == 1 & df.tumor$field17 == 3) | (df.tumor$field37 == 5 & df.tumor$field55 == 1), 69,
                                                                                                  ifelse(df.tumor$field37 == 1 & df.tumor$field17 == 2, 70, # End Classification of ALL★★
                                                                                                         ifelse((df.tumor$field37 == 2 & df.tumor$field26 == 4)| (df.tumor$field37 == 2 & df.tumor$field25 == 7), 31,
                                                                                                                ifelse((df.tumor$field37 == 2 & df.tumor$field26 == 3)| (df.tumor$field37 == 2 & df.tumor$field25 == 4) | (df.tumor$field37 == 2 & df.tumor$field25 == 5), 32,
                                                                                                                       ifelse(df.tumor$field37 == 2 & df.tumor$field26 == 6 , 33,
                                                                                                                              ifelse(df.tumor$field37 == 2 & df.tumor$field26 == 2 , 30,
                                                                                                                                     ifelse(df.tumor$field37 == 2 & df.tumor$field26 == 12, 34,
                                                                                                                                            ifelse(df.tumor$field37 == 2 & df.tumor$field26 == 13, 36,
                                                                                                                                                   ifelse(df.tumor$field37 == 2 & df.tumor$field26 == 9 , 57,
                                                                                                                                                          ifelse(df.tumor$field37 == 2 & df.tumor$field25 == 1 , 42,
                                                                                                                                                                 ifelse(df.tumor$field37 == 2 & df.tumor$field25 == 2 , 43,
                                                                                                                                                                        ifelse(df.tumor$field37 == 2 & df.tumor$field25 == 3 , 44,
                                                                                                                                                                               ifelse(df.tumor$field37 == 2 & df.tumor$field25 == 6 , 45,
                                                                                                                                                                                      ifelse((df.tumor$field37 == 2 & df.tumor$field25 == 8) | (df.tumor$field37 == 2 & df.tumor$field25 == 9), 46,
                                                                                                                                                                                             ifelse((df.tumor$field37 == 2 & df.tumor$field25 == 10) | (df.tumor$field37 == 2 & df.tumor$field25 == 11),47,
                                                                                                                                                                                                    ifelse(df.tumor$field37 == 2 & df.tumor$field25 == 12,  48,
                                                                                                                                                                                                           ifelse(df.tumor$field37 == 2 & df.tumor$field24 == 5, 51,
                                                                                                                                                                                                                  ifelse(df.tumor$field37 == 2 & df.tumor$field24 == 6, 54,
                                                                                                                                                                                                                         ifelse(df.tumor$field37 == 2, 41,  # End Classification of AML
                                                                                                                                                                                                                                ifelse(df.tumor$field37 == 4 & df.tumor$field159 == 1, 1,　　　　　　　　　　　
                                                                                                                                                                                                                                       ifelse(df.tumor$field37 == 4 & df.tumor$field159 == 2 & df.tumor$field164 == 1 & df.tumor$field35 == 2, 5,
                                                                                                                                                                                                                                              ifelse(df.tumor$field37 == 4 & df.tumor$field159 == 2 & df.tumor$field164 == 1 & df.tumor$field35 == 3, 3,
                                                                                                                                                                                                                                                     ifelse(df.tumor$field37 == 4 & df.tumor$field159 == 2 & df.tumor$field164 == 1 & df.tumor$field35 == 7, 4,
                                                                                                                                                                                                                                                            ifelse(df.tumor$field37 == 4 & df.tumor$field159 == 2 & df.tumor$field164 == 1 & df.tumor$field35 == 5, 7,
                                                                                                                                                                                                                                                                   ifelse(df.tumor$field37 == 4 & df.tumor$field159 == 2 & df.tumor$field164 == 1 & df.tumor$field35 == 6, 9,
                                                                                                                                                                                                                                                                          ifelse(df.tumor$field37 == 4 & df.tumor$field159 == 2 & df.tumor$field164 == 1 & df.tumor$field35 == 4, 12,
                                                                                                                                                                                                                                                                                 ifelse(df.tumor$field37 == 4 & df.tumor$field159 == 2 & df.tumor$field164 == 2 & df.tumor$field47 == 1, 16,
                                                                                                                                                                                                                                                                                        ifelse(df.tumor$field37 == 4 & df.tumor$field159 == 2 & df.tumor$field164 == 2 & df.tumor$field47 == 2, 17,
                                                                                                                                                                                                                                                                                               ifelse(df.tumor$field37 == 4 & df.tumor$field159 == 2 & df.tumor$field164 == 2 & df.tumor$field47 == 3, 18, NA)))))))))))))))))))))))))))))))))))))))
df.tumor$MHDECOD2 <- ifelse(df.tumor$field37 == 4 & df.tumor$field159 == 2 & df.tumor$field164 == 2 & df.tumor$field47 == 4, 19,
                            ifelse(df.tumor$field37 == 4 & df.tumor$field159 == 2 & df.tumor$field164 == 3 & df.tumor$field51 == 5, 27,
                                   ifelse(df.tumor$field37 == 4 & df.tumor$field159 == 2 & df.tumor$field164 == 3 & df.tumor$field49 == 1, 21,
                                          ifelse(df.tumor$field37 == 4 & df.tumor$field159 == 2 & df.tumor$field164 == 3 & df.tumor$field49 == 2, 24,
                                                 ifelse((df.tumor$field37 == 4 & df.tumor$field159 == 2 & df.tumor$field164 == 3 & df.tumor$field49 == 3) | (df.tumor$field37 == 4 & df.tumor$field159 == 2 & df.tumor$field164 == 3 & df.tumor$field49 == 4), 25,
                                                        ifelse((df.tumor$field37 == 4 & df.tumor$field159 == 2 & df.tumor$field164 == 3 & df.tumor$field49 == 5) | (df.tumor$field37 == 4 & df.tumor$field159 == 2 & df.tumor$field164 == 3 & df.tumor$field49 == 6), 26,
                                                               ifelse(df.tumor$field37 == 4 & df.tumor$field159 == 2 & df.tumor$field164 == 3 & df.tumor$field49 == 7, 28,
                                                                      ifelse(df.tumor$field37 == 4 & df.tumor$field159 == 2 & df.tumor$field164 == 3 & df.tumor$field49 == 9, 29, # End Classification of MDS MPD
                                                                             ifelse(df.tumor$field37 == 3 & df.tumor$field32 == 3, 57,
                                                                                    ifelse(df.tumor$field37 == 3 & df.tumor$field32 == 2, 56,
                                                                                           ifelse((df.tumor$field37 == 3 & df.tumor$field28 == 1 & df.tumor$field29 == 3) | (df.tumor$field37 == 3 & df.tumor$field28 == 6), 60,
                                                                                                  ifelse(df.tumor$field37 == 3 & df.tumor$field28 == 1 & df.tumor$field29 == 1, 58,
                                                                                                         ifelse(df.tumor$field37 == 3 & df.tumor$field28 == 1 & df.tumor$field29 == 2, 59,
                                                                                                                ifelse(df.tumor$field37 == 3 & df.tumor$field28 == 2 , 55, # End Classification of Rare leukemia
                                                                                                                       ifelse(df.tumor$field37 == 5 & df.tumor$field55 == 3, 106,
                                                                                                                              ifelse(df.tumor$field37 == 5 & df.tumor$field55 == 4, 93,
                                                                                                                                     ifelse(df.tumor$field37 == 5 & df.tumor$field55 == 5, 100,
                                                                                                                                            ifelse(df.tumor$field37 == 5 & df.tumor$field55 == 6, 90,
                                                                                                                                                   ifelse(df.tumor$field37 == 5 & df.tumor$field55 == 7 & df.tumor$field67 == 1, 129,
                                                                                                                                                          ifelse(df.tumor$field37 == 5 & df.tumor$field55 == 7 & df.tumor$field67 == 2, 130,
                                                                                                                                                                 ifelse(df.tumor$field37 == 5 & df.tumor$field55 == 9, 116,
                                                                                                                                                                        ifelse(df.tumor$field37 == 5 & df.tumor$field55 == 10, 119,
                                                                                                                                                                               ifelse(df.tumor$field37 == 5 & df.tumor$field55 == 11, 127,
                                                                                                                                                                                      ifelse(df.tumor$field37 == 5 & df.tumor$field55 == 13, 86,
                                                                                                                                                                                             ifelse(df.tumor$field37 == 5 & df.tumor$field55 == 14, 88,
                                                                                                                                                                                                    ifelse(df.tumor$field37 == 5 & df.tumor$field55 == 15, 107,
                                                                                                                                                                                                           ifelse(df.tumor$field37 == 5 & df.tumor$field55 == 16, 117,
                                                                                                                                                                                                                  ifelse(df.tumor$field37 == 5 & df.tumor$field55 == 17, 113,
                                                                                                                                                                                                                         ifelse(df.tumor$field37 == 5 & df.tumor$field55 == 15, 107,  # End Classification of NHL
                                                                                                                                                                                                                                ifelse(df.tumor$field37 == 6 & df.tumor$field61 == 1, 131,
                                                                                                                                                                                                                                       ifelse(df.tumor$field37 == 6 & df.tumor$field61 == 2, 133,
                                                                                                                                                                                                                                              ifelse(df.tumor$field37 == 6 & df.tumor$field61 == 3, 134,
                                                                                                                                                                                                                                                     ifelse(df.tumor$field37 == 6 & df.tumor$field61 == 4, 135,
                                                                                                                                                                                                                                                            ifelse(df.tumor$field37 == 6 & df.tumor$field61 == 5, 136,
                                                                                                                                                                                                                                                                   ifelse(df.tumor$field37 == 6 & df.tumor$field61 == 7, 132,  # End Classification of HL
                                                                                                                                                                                                                                                                          ifelse(df.tumor$field37 == 8 & df.tumor$field69 == 1, 138,
                                                                                                                                                                                                                                                                                 ifelse(df.tumor$field37 == 8 & df.tumor$field69 == 4, 137,
                                                                                                                                                                                                                                                                                        ifelse(df.tumor$field37 == 8 & df.tumor$field69 == 5, 144,
                                                                                                                                                                                                                                                                                               ifelse(df.tumor$field37 == 7 & df.tumor$field77 == 4, 145,
                                                                                                                                                                                                                                                                                                      ifelse(df.tumor$field37 == 7 & df.tumor$field77 == 2, 154,
                                                                                                                                                                                                                                                                                                             ifelse(df.tumor$field37 == 7 & df.tumor$field77 == 3, 155, NA)))))))))))))))))))))))))))))))))))))))))
df.tumor$MHDECOD <- ifelse(is.na(df.tumor$MHDECOD1), df.tumor$MHDECOD2, df.tumor$MHDECOD1)  # 空欄はあてはまらないもの

df.non.t <- subset(before201806_jspho, before201806_jspho$flag == "non_tumor")

df.non.t$MHDECOD1 <- ifelse(df.non.t$field7 == 2 & df.non.t$field84 == 1 & df.non.t$field88 == 4, 1001,
                            ifelse(df.non.t$field7 == 2 & df.non.t$field84 == 1 & df.non.t$field88 == 5, 1002,
                                   ifelse(df.non.t$field7 == 2 & df.non.t$field84 == 1 & df.non.t$field88 == 6, 1003,
                                          ifelse(df.non.t$field7 == 2 & df.non.t$field84 == 1 & df.non.t$field88 == 1, 1004,
                                                 ifelse(df.non.t$field7 == 2 & df.non.t$field84 == 1 & df.non.t$field88 == 2, 1008,
                                                        ifelse(df.non.t$field7 == 2 & df.non.t$field84 == 1 & df.non.t$field88 == 3 & df.non.t$field90 == 1, 1009,
                                                               ifelse(df.non.t$field7 == 2 & df.non.t$field84 == 1 & df.non.t$field88 == 3 & df.non.t$field90 == 2, 1010,
                                                                      ifelse(df.non.t$field7 == 2 & df.non.t$field84 == 8 & df.non.t$field94 == 1, 1011,
                                                                             ifelse(df.non.t$field7 == 2 & df.non.t$field84 == 8 & df.non.t$field94 == 2, 1012,
                                                                                    ifelse(df.non.t$field7 == 2 & df.non.t$field84 == 8 & df.non.t$field94 == 3, 1013,
                                                                                           ifelse(df.non.t$field7 == 2 & df.non.t$field84 == 9 & df.non.t$field98 == 1, 1014,
                                                                                                  ifelse(df.non.t$field7 == 2 & df.non.t$field84 == 9 & df.non.t$field98 == 2, 1015,
                                                                                                         ifelse(df.non.t$field7 == 2 & df.non.t$field84 == 9 & df.non.t$field98 == 3, 1016,
                                                                                                                ifelse(df.non.t$field7 == 2 & df.non.t$field84 == 9 & df.non.t$field98 == 4, 1017,
                                                                                                                       ifelse(df.non.t$field7 == 2 & df.non.t$field84 == 9 & df.non.t$field98 == 5, 1018,
                                                                                                                              ifelse(df.non.t$field7 == 2 & df.non.t$field84 == 9 & df.non.t$field98 == 6, 1019,
                                                                                                                                     ifelse(df.non.t$field7 == 2 & df.non.t$field84 == 9 & df.non.t$field98 == 7, 1020,
                                                                                                                                            ifelse(df.non.t$field7 == 2 & df.non.t$field84 == 9 & df.non.t$field98 == 8, 1021,
                                                                                                                                                   ifelse(df.non.t$field7 == 2 & df.non.t$field84 == 9 & df.non.t$field98 == 9, 1022,
                                                                                                                                                          ifelse(df.non.t$field7 == 2 & df.non.t$field84 == 9 & df.non.t$field98 == 10, 1023,
                                                                                                                                                                 ifelse(df.non.t$field7 == 2 & df.non.t$field84 == 9 & df.non.t$field98 == 11, 1024,
                                                                                                                                                                        ifelse(df.non.t$field7 == 2 & df.non.t$field84 == 9 & df.non.t$field98 == 12, 1024,
                                                                                                                                                                               ifelse(df.non.t$field7 == 2 & df.non.t$field84 == 9 & df.non.t$field98 == 13, 1025,
                                                                                                                                                                                      ifelse(df.non.t$field7 == 2 & df.non.t$field84 == 9 & df.non.t$field98 == 14, 1026,
                                                                                                                                                                                             ifelse(df.non.t$field7 == 2 & df.non.t$field84 == 9 & df.non.t$field98 == 15, 1027,
                                                                                                                                                                                                    ifelse(df.non.t$field7 == 2 & df.non.t$field84 == 2, 1028,
                                                                                                                                                                                                           ifelse(df.non.t$field7 == 2 & df.non.t$field84 == 9 & df.non.t$field98 == 16 & df.non.t$field103 == 1, 1029,
                                                                                                                                                                                                                  ifelse(df.non.t$field7 == 2 & df.non.t$field84 == 9 & df.non.t$field98 == 16 & df.non.t$field103 == 2, 1030,
                                                                                                                                                                                                                         ifelse(df.non.t$field7 == 2 & df.non.t$field84 == 9 & df.non.t$field98 == 16 & df.non.t$field103 == 3, 1031,
                                                                                                                                                                                                                                ifelse(df.non.t$field7 == 2 & df.non.t$field84 == 9 & df.non.t$field98 == 16 & df.non.t$field103 == 4, 1035,
                                                                                                                                                                                                                                       ifelse(df.non.t$field7 == 2 & df.non.t$field84 == 9 & df.non.t$field98 == 17, 1036,
                                                                                                                                                                                                                                              ifelse(df.non.t$field7 == 2 & df.non.t$field84 == 3 & df.non.t$field108 == 1 & df.non.t$field109 == 1, 1037,
                                                                                                                                                                                                                                                     ifelse(df.non.t$field7 == 2 & df.non.t$field84 == 3 & df.non.t$field108 == 1 & df.non.t$field109 == 2, 1038,
                                                                                                                                                                                                                                                            ifelse(df.non.t$field7 == 2 & df.non.t$field84 == 3 & df.non.t$field108 == 1 & df.non.t$field109 == 3, 1039,
                                                                                                                                                                                                                                                                   ifelse(df.non.t$field7 == 2 & df.non.t$field84 == 3 & df.non.t$field108 == 2, 1040,
                                                                                                                                                                                                                                                                          ifelse(df.non.t$field7 == 2 & df.non.t$field84 == 3 & df.non.t$field108 == 3, 1041,
                                                                                                                                                                                                                                                                                 ifelse(df.non.t$field7 == 2 & df.non.t$field84 == 3 & df.non.t$field108 == 4, 1042,
                                                                                                                                                                                                                                                                                        ifelse(df.non.t$field7 == 2 & df.non.t$field84 == 3 & df.non.t$field108 == 5, 1043,
                                                                                                                                                                                                                                                                                               ifelse(df.non.t$field7 == 2 & df.non.t$field84 == 10 & df.non.t$field117 == 1, 1046,
                                                                                                                                                                                                                                                                                                      ifelse(df.non.t$field7 == 2 & df.non.t$field84 == 10 & df.non.t$field117 == 2, 1047,
                                                                                                                                                                                                                                                                                                             ifelse(df.non.t$field7 == 2 & df.non.t$field84 == 10 & df.non.t$field117 == 3, 1048,
                                                                                                                                                                                                                                                                                                                    ifelse(df.non.t$field7 == 2 & df.non.t$field84 == 10 & df.non.t$field117 == 4, 1049, NA))))))))))))))))))))))))))))))))))))))))))
df.non.t$MHDECOD2 <- ifelse(df.non.t$field7 == 2 & df.non.t$field84 == 11 & df.non.t$field120 == 1, 1050,
                            ifelse(df.non.t$field7 == 2 & df.non.t$field84 == 11 & df.non.t$field120 == 2 & df.non.t$field123 == 1, 1051,
                                   ifelse(df.non.t$field7 == 2 & df.non.t$field84 == 11 & df.non.t$field120 == 2 & df.non.t$field123 == 2, 1052,
                                          ifelse(df.non.t$field7 == 2 & df.non.t$field84 == 11 & df.non.t$field120 == 7, 1054,
                                                 ifelse(df.non.t$field7 == 2 & df.non.t$field84 == 11 & df.non.t$field120 == 3, 1055,
                                                        ifelse(df.non.t$field7 == 2 & df.non.t$field84 == 11 & df.non.t$field120 == 4, 1056,
                                                               ifelse(df.non.t$field7 == 2 & df.non.t$field84 == 11 & df.non.t$field120 == 6, 1057,
                                                                      ifelse(df.non.t$field7 == 2 & df.non.t$field84 == 11 & df.non.t$field120 == 5, 1058,
                                                                             ifelse(df.non.t$field7 == 2 & df.non.t$field84 == 5 & df.non.t$field130 == 1, 1062,
                                                                                    ifelse(df.non.t$field7 == 2 & df.non.t$field84 == 5 & df.non.t$field130 == 2, 1063,
                                                                                           ifelse(df.non.t$field7 == 2 & df.non.t$field84 == 5 & df.non.t$field130 == 3, 1064,
                                                                                                  ifelse(df.non.t$field7 == 2 & df.non.t$field84 == 5 & df.non.t$field130 == 9, 1064,
                                                                                                         ifelse(df.non.t$field7 == 2 & df.non.t$field84 == 5 & df.non.t$field130 == 4, 1065,
                                                                                                                ifelse(df.non.t$field7 == 2 & df.non.t$field84 == 5 & df.non.t$field130 == 10, 1065,
                                                                                                                       ifelse(df.non.t$field7 == 2 & df.non.t$field84 == 5 & df.non.t$field130 == 11, 1065,
                                                                                                                              ifelse(df.non.t$field7 == 2 & df.non.t$field84 == 5 & df.non.t$field130 == 6, 1067,
                                                                                                                                     ifelse(df.non.t$field7 == 2 & df.non.t$field84 == 5 & df.non.t$field130 == 5, 1067,
                                                                                                                                            ifelse(df.non.t$field7 == 2 & df.non.t$field84 == 5 & df.non.t$field130 == 7, 1068,
                                                                                                                                                   ifelse(df.non.t$field7 == 2 & df.non.t$field84 == 5 & df.non.t$field130 == 8, 1069,
                                                                                                                                                          ifelse(df.non.t$field7 == 2 & df.non.t$field84 == 12 & df.non.t$field135 == 1, 1070,
                                                                                                                                                                 ifelse(df.non.t$field7 == 2 & df.non.t$field84 == 12 & df.non.t$field135 == 2, 1071,
                                                                                                                                                                        ifelse(df.non.t$field7 == 2 & df.non.t$field84 == 12 & df.non.t$field135 == 3, 1072,
                                                                                                                                                                               ifelse(df.non.t$field7 == 2 & df.non.t$field84 == 12 & df.non.t$field135 == 8, 1073,
                                                                                                                                                                                      ifelse(df.non.t$field7 == 2 & df.non.t$field84 == 12 & df.non.t$field135 == 7, 1074,
                                                                                                                                                                                             ifelse(df.non.t$field7 == 2 & df.non.t$field84 == 12 & df.non.t$field135 == 9, 1075,
                                                                                                                                                                                                    ifelse(df.non.t$field7 == 2 & df.non.t$field84 == 12 & df.non.t$field135 == 5, 1076,
                                                                                                                                                                                                           ifelse(df.non.t$field7 == 2 & df.non.t$field84 == 12 & df.non.t$field135 == 6, 1077,
                                                                                                                                                                                                                  ifelse(df.non.t$field7 == 2 & df.non.t$field84 == 13 & df.non.t$field141 == 1, 1079,
                                                                                                                                                                                                                         ifelse(df.non.t$field7 == 2 & df.non.t$field84 == 13 & df.non.t$field141 == 2, 1080,
                                                                                                                                                                                                                                ifelse(df.non.t$field7 == 2 & df.non.t$field84 == 13 & df.non.t$field141 == 3, 1081,
                                                                                                                                                                                                                                       ifelse(df.non.t$field7 == 2 & df.non.t$field84 == 13 & df.non.t$field141 == 4, 1082,
                                                                                                                                                                                                                                              ifelse(df.non.t$field7 == 2 & df.non.t$field84 == 13 & df.non.t$field141 == 5, 1083,
                                                                                                                                                                                                                                                     ifelse(df.non.t$field7 == 2 & df.non.t$field84 == 13 & df.non.t$field141 == 6, 1084,
                                                                                                                                                                                                                                                            ifelse(df.non.t$field7 == 2 & df.non.t$field84 == 7 & df.non.t$field148 == 1, 1086,
                                                                                                                                                                                                                                                                   ifelse(df.non.t$field7 == 2 & df.non.t$field84 == 7 & df.non.t$field148 == 2, 1087,
                                                                                                                                                                                                                                                                          ifelse(df.non.t$field7 == 2 & df.non.t$field84 == 7 & df.non.t$field148 == 3, 1088,
                                                                                                                                                                                                                                                                                 ifelse(df.non.t$field7 == 2 & df.non.t$field84 == 7 & df.non.t$field148 == 4, 1089,
                                                                                                                                                                                                                                                                                        ifelse(df.non.t$field7 == 2 & df.non.t$field84 == 7 & df.non.t$field148 == 5, 1090,
                                                                                                                                                                                                                                                                                               ifelse(df.non.t$field7 == 2 & df.non.t$field84 == 7 & df.non.t$field148 == 6, 1091,
                                                                                                                                                                                                                                                                                                      ifelse(df.non.t$field7 == 2 & df.non.t$field84 == 14 & df.non.t$field151 == 1, 1092,
                                                                                                                                                                                                                                                                                                             ifelse(df.non.t$field7 == 2 & df.non.t$field84 == 14 & df.non.t$field151 == 2, 1093,
                                                                                                                                                                                                                                                                                                                    ifelse(df.non.t$field7 == 2 & df.non.t$field84 == 14 & df.non.t$field151 == 3, 1094,
                                                                                                                                                                                                                                                                                                                           ifelse(df.non.t$field7 == 2 & df.non.t$field84 == 14 & df.non.t$field151 == 4, 1095,
                                                                                                                                                                                                                                                                                                                                  ifelse(df.non.t$field7 == 2 & df.non.t$field84 == 14 & df.non.t$field151 == 5, 1096,
                                                                                                                                                                                                                                                                                                                                         ifelse(df.non.t$field7 == 2 & df.non.t$field84 == 14 & df.non.t$field151 == 6, 1097,
                                                                                                                                                                                                                                                                                                                                                ifelse(df.non.t$field7 == 2 & df.non.t$field84 == 14 & df.non.t$field151 == 7, 1098,
                                                                                                                                                                                                                                                                                                                                                       ifelse(df.non.t$field7 == 2 & df.non.t$field84 == 16 & df.non.t$field153 == 1, 1099,
                                                                                                                                                                                                                                                                                                                                                              ifelse(df.non.t$field7 == 2 & df.non.t$field84 == 16 & df.non.t$field153 == 2, 1100,
                                                                                                                                                                                                                                                                                                                                                                     ifelse(df.non.t$field7 == 2 & df.non.t$field84 == 16 & df.non.t$field153 == 3, 1102,
                                                                                                                                                                                                                                                                                                                                                                            ifelse(df.non.t$field7 == 1 & df.non.t$field37 == 8 & df.non.t$field69 == 2, 1102, NA))))))))))))))))))))))))))))))))))))))))))))))))))

#  あてはまらない病名に仮コードを付与 # その他の血液疾患 9004
df.non.t$MHDECOD <- ifelse(is.na(df.non.t$MHDECOD1), df.non.t$MHDECOD2,
                           df.non.t$MHDECOD1)
df.tumor <- df.tumor[, -c(408, 409)]
df.non.t <- df.non.t[, -c(408, 409)]
result_2017_jspho0 <- rbind(df.tumor, df.non.t)
result_2017_jspho <- subset(result_2017_jspho0, !is.na(result_2017_jspho0$MHDECOD))
result_2017_jspho_dropout <- nrow(subset(result_2017_jspho0, is.na(result_2017_jspho0$MHDECOD)))  # dropoutした人数


# colnamesを合わせ、すべてのデータをバインドする
after201806_jspho$MHDECOD <- after201806_jspho$field1
result_2017_jspho <- result_2017_jspho[, -407]
jspho_bind <- rbind(result_2017_jspho, after201806_jspho)
jspho_merge  <- merge(jspho_bind, dxt_jspho_outcome, by = "登録コード", all.x = T)

# 県コードを作成
jspho_merge$SCSTRESC <- floor(as.integer(sub("^.*.-","",jspho_merge$field173))/1000)

# STUDYID
jspho_merge$STUDYID <- "JSPHO"

# 20歳未満抽出
jspho_merge$age.diagnosis <- YearDif(jspho_merge$生年月日, jspho_merge$診断年月日)
jspho <- jspho_merge[jspho_merge$age.diagnosis < 20 , ]
jspho_dropout <- nrow(subset(jspho_merge,jspho_merge$age.diagnosis >= 20))  # dropoutした人数

#WHO2008をWHO2016に変換
if (flag == 1) {
  jspho$MHDECOD <- ifelse(nchar(jspho$MHDECOD) != 5, round(jspho$MHDECOD * 10 + 10000, digits = 0)
                          , jspho$MHDECOD)
  jspho$MHDECOD <- ifelse(jspho$MHDECOD == 10930, 10931, jspho$MHDECOD)
  jspho <- merge(jspho, Disease_Name_v2, by.x = "MHDECOD", by.y = "code", all.x = T)
} else {
  # WHO2016をWHO2008に変換
  jspho$MHDECOD <- ifelse(nchar(jspho$MHDECOD) == 5, round((jspho$MHDECOD - 10000) / 10, digits = 0)
                          , jspho$MHDECOD)
  jspho <- merge(jspho, WHO2008, by.x = "MHDECOD", by.y = "code", all.x = T)
}

jspho_year_dropout <- nrow(subset(jspho, jspho$year <= 2011 | as.integer(jspho$year) > kYear))  # dropoutした人数
jspho_ads <- jspho[as.integer(jspho$year) > 2011 & as.integer(jspho$year) <= kYear ,
                   c("作成日", "登録コード", "性別", "SCSTRESC", "生死", "死亡日", "最終確認日", "field161", "MHDECOD",
                     "name_ja", "生年月日", "診断年月日", "STUDYID")]

colnames(jspho_ads)[1:12] <- c("created.date", "SUBJID", "SEX", "SCSTRESC", "DTHFL", "DTHDTC", "DSSTDTC", "SITEID", "MHDECOD", "MHTERM",
                               "BRTHDTC", "MHSTDTC")

#------NHOH---------
#施設コードをマージする処理(NHOH)
# p.nhoh.rgst <- nhoh.rgst[,c("登録コード", "field7", "生年月日", "性別")]
# p.nhoh.rgst$SCSTRESC <- floor(as.integer(sub("^.*.-","",p.nhoh.rgst$field7))/1000)
# dxt_nhoh_outcome <- nhoh_outcome[, c("登録コード", "生死", "死亡日", "最終確認日")]
# m.nhoh_0 <- merge(nhoh_report,p.nhoh.rgst, by="登録コード", all.x= T)
# m.nhoh <- merge(m.nhoh_0, dxt_nhoh_outcome, by="登録コード", all.x= T)
# STUDYID
# m.nhoh$STUDYID <- "NHOH"
#WHO2008をWHO2016に変換
# if (flag == 1) {
#     m.nhoh$MHDECOD <- ifelse(nchar(m.nhoh$field2) != 5, round(m.nhoh$field2 * 10 + 10000, digits = 0), m.nhoh$field2)
#     # elseの時,WHO2016に変換した情報までWHO2008へ上書きしてしまう処理になっていたため修正（2022/08/16 Agata.K）
#     # m.nhoh$MHDECOD <- ifelse(m.nhoh$field2 == 10930, 10931, m.nhoh$field2)
#     m.nhoh$MHDECOD <- ifelse(m.nhoh$field2 == 10930, 10931, m.nhoh$MHDECOD)
#     m.nhoh <- merge(m.nhoh, Disease_Name_v2, by.x = "MHDECOD", by.y = "code", all.x = T)
# } else {
#     # WHO2016をWHO2008に変換
#     m.nhoh$MHDECOD <- ifelse(nchar(m.nhoh$field2) == 5, round((m.nhoh$field2 - 10000) / 10, digits = 0), m.nhoh$field2)
#     m.nhoh <- merge(m.nhoh, WHO2008, by.x = "MHDECOD", by.y = "code", all.x = T)
# }

# 診断年月日2012年以降、必要変数抽出
# nho_year_dropout <- nrow(subset(m.nhoh,
#                                 as.integer(substr(m.nhoh$診断年月日, 1, 4)) <= 2011 | as.integer(substr(m.nhoh$診断年月日, 1, 4)) > kYear))  # dropoutした人数
# nhoh.1 <- m.nhoh[as.integer(substr(m.nhoh$診断年月日, 1, 4)) > 2011 & as.integer(substr(m.nhoh$診断年月日, 1, 4)) <= kYear ,
#                  c("作成日", "登録コード", "性別", "SCSTRESC", "生死", "死亡日.y", "最終確認日", "シート作成時施設コード", "MHDECOD",
#                    "name_ja", "生年月日", "診断年月日", "STUDYID")]
# colnames(nhoh.1)[1:12] <- c("created.date", "SUBJID", "SEX", "SCSTRESC", "DTHFL", "DTHDTC", "DSSTDTC", "SITEID", "MHDECOD", "MHTERM",
#                             "BRTHDTC", "MHSTDTC")
# # BRTHDTC, MHSTDTCが逆転している症例を除く
# nho_reverse_dropout <- nrow(subset(nhoh.1, ((format(as.Date(nhoh.1$BRTHDTC), "%Y%m%d")) >  format(as.Date(nhoh.1$MHSTDTC), "%Y%m%d"))))  # dropoutした人数
# nhoh.1 <- nhoh.1[format(as.Date(nhoh.1$BRTHDTC), "%Y%m%d") <=  format(as.Date(nhoh.1$MHSTDTC), "%Y%m%d"), ]

#------JSH---------
#施設コードをマージする処理(JSH)
p.jsh.rgst <- jsh.rgst[, c("登録コード", "field114", "生年月日", "性別")]
p.jsh.rgst$SCSTRESC <- floor(as.integer(sub("^.*.-", "", p.jsh.rgst$field114))/1000)
dxt_jsh_outcome <- jsh_outcome[, c("登録コード", "生死", "死亡日", "最終確認日")]
m.jsh_0 <- merge(jsh_report, p.jsh.rgst, by = "登録コード", all.x = T)
m.jsh <- merge(m.jsh_0, dxt_jsh_outcome, by = "登録コード", all.x = T)
# STUDYID
m.jsh$STUDYID <- "JSH"
#WHO2008をWHO2016に変換
if (flag == 1) {
  m.jsh$MHDECOD <- ifelse(nchar(m.jsh$field1) != 5, round(m.jsh$field1 * 10 + 10000, digits = 0), m.jsh$field1)
  # elseの時,WHO2016に変換した情報までWHO2008へ上書きしてしまう処理になっていたため修正（2022/08/16 Agata.K）
  # m.jsh$MHDECOD <- ifelse(m.jsh$field1 == 10930, 10931, m.jsh$field1)
  m.jsh$MHDECOD <- ifelse(m.jsh$field1 == 10930, 10931, m.jsh$MHDECOD)
  m.jsh <- merge(m.jsh, Disease_Name_v2, by.x = "MHDECOD", by.y = "code", all.x = T)
} else {
  m.jsh$MHDECOD <- ifelse(nchar(m.jsh$field1) == 5, round((m.jsh$field1 - 10000) / 10, digits = 0)
                          , m.jsh$field1)
  m.jsh <- merge(m.jsh, WHO2008, by.x = "MHDECOD", by.y = "code", all.x = T)
}

# 診断年月日2012年以降、腫瘍性病変のみを抽出
jsh_year_dropout <- nrow(m.jsh[as.integer(substr(m.jsh$診断年月日, 1, 4)) <= 2011 | as.integer(substr(m.jsh$診断年月日, 1, 4)) > kYear, ])  # dropoutした人数
jsh.2 <- m.jsh[as.integer(substr(m.jsh$診断年月日, 1, 4)) > 2011 & as.integer(substr(m.jsh$診断年月日, 1, 4)) <= kYear ,
               c("作成日", "登録コード", "性別", "SCSTRESC", "生死", "死亡日", "最終確認日", "シート作成時施設コード", "MHDECOD",
                 "name_ja", "生年月日", "診断年月日", "STUDYID")]
colnames(jsh.2)[1:12] <- c("created.date", "SUBJID", "SEX", "SCSTRESC", "DTHFL", "DTHDTC", "DSSTDTC", "SITEID", "MHDECOD", "MHTERM",
                           "BRTHDTC", "MHSTDTC")

# BRTHDTC, MHSTDTCが逆転している症例を除く
jsh_reverse_dropout <- nrow(jsh.2[is.na(jsh.2$BRTHDTC) | as.integer(format(as.Date(jsh.2$MHSTDTC), "%Y%m%d")) - as.integer(format(as.Date(jsh.2$BRTHDTC), "%Y%m%d")) < 0, ]) # dropoutした人数
jsh.1 <- subset(jsh.2, !is.na(jsh.2$BRTHDTC) & (format(as.Date(jsh.2$BRTHDTC), "%Y%m%d") <=  format(as.Date(jsh.2$MHSTDTC), "%Y%m%d")))

# # 3団体を繋げた基本のデータセットを作成
dataset.3org0 <-  rbind(jsh.1, jspho_ads)
# dataset.3org0 <-  rbind(jsh.1, nhoh.1, jspho_ads)

#MHTERMの空欄は解析対象から除外する
MHTERM_jspho_fail_dropout <- nrow(dataset.3org0[is.na(dataset.3org0$MHTERM) & dataset.3org0$STUDYID == "JSPHO", ])
MHTERM_jsh_fail_dropout <- nrow(dataset.3org0[is.na(dataset.3org0$MHTERM) & dataset.3org0$STUDYID == "JSH", ])
# MHTERM_nho_fail_dropout <- nrow(dataset.3org0[is.na(dataset.3org0$MHTERM) & dataset.3org0$STUDYID == "NHOH", ])
dataset.3org0 <- dataset.3org0[!is.na(dataset.3org0$MHTERM), ]

# SEX, SCSTRESC, BRTHDTCの空欄は解析対象から除外する
sys_fail_dropout <- dataset.3org0[is.na(dataset.3org0$SEX) | is.na(dataset.3org0$SCSTRESC) | is.na(dataset.3org0$BRTHDTC), ]
dataset.3org <- dataset.3org0[!is.na(dataset.3org0$SEX) & !is.na(dataset.3org0$SCSTRESC) & !is.na(dataset.3org0$BRTHDTC) , ]

# age diagnosis
dataset.3org$age.diagnosis <- as.integer(YearDif(dataset.3org$BRTHDTC, dataset.3org$MHSTDTC))

# flagが2の場合はここで、データ出力
if(flag == 2) {
  dataset.3org <- dataset.3org[dataset.3org$age.diagnosis > 9 | dataset.3org$age.diagnosis <= 9 & dataset.3org$STUDYID == "JSPHO", ] # JSH/NHOHの0-9歳を疾患によらず全て削除
  dataset.3org[is.na(dataset.3org)] <- ""
  write.csv(dataset.3org, paste0(prtpath, "/output/JSH_NHOH_JSPHO_ads_WHO2008", "_", kToday, ".csv"), row.names = F)
  
  # count用に"1"を入力
  dataset.3org$count <- 1
  # ICD-10による区分をマージ
  ads_mhcod <-  merge(dataset.3org, mhcod_20161006, by = "MHDECOD", all.x = T)
  # 年齢区分を挿入
  ads_mhcod$cat.age.diagnosis <- cut(as.integer(ads_mhcod$age.diagnosis), breaks = c(0, 15, 200),
                                     labels= c("0-14", "15-"), right=FALSE)
  # 診断年区分を挿入
  ads_mhcod$year.diagnosis <- paste0("JSH_", substr(ads_mhcod$MHSTDTC, 1, 4))
  # 診断年、診断区分別に集計
  by.year.diagnosis <- xtabs(count ~ MHSCAT + year.diagnosis, data = ads_mhcod)
  # 15歳未満の症例に対し、診断年、診断区分別に集計
  under15 <- ads_mhcod[ads_mhcod$cat.age.diagnosis == "0-14", ]
  by.year.diagnosis.u15 <- xtabs(count ~ MHSCAT + year.diagnosis, data = under15)
  # 15歳以上の症例に対し、診断年、診断区分別に集計
  over15 <- ads_mhcod[ads_mhcod$cat.age.diagnosis == "15-", ]
  by.year.diagnosis.o15 <- xtabs(count ~ MHSCAT + year.diagnosis, data = over15)
  
  # 条件設定により落ちた症例をカウント
  dropout <- data.frame(
    項目 = c("全登録数", "JSPHO詳細登録の内容よりWHO分類にマッピング不能", "WHO2008に分類できない", "作成日または診断年月日が空値", "診断時年齢20歳以上", "集計対象年以外", "生年月日と診断年月日の逆転",　"不具合による脱落
           ", "解析対象症例数"),
    JSPHO = c(jspho_total, result_2017_jspho_dropout, MHTERM_jspho_fail_dropout, (dropout_emp_year + dropout_emp_cdate),  jspho_dropout, jspho_year_dropout, 0, nrow(sys_fail_dropout[sys_fail_dropout$STUDYID == "JSPHO",]), nrow(dataset.3org[dataset.3org$STUDYID == "JSPHO", ])),
    JSH =  c(jsh_total, 0, MHTERM_jsh_fail_dropout, 0, 0, jsh_year_dropout, jsh_reverse_dropout, nrow(sys_fail_dropout[sys_fail_dropout$STUDYID == "JSH",]),  nrow(dataset.3org[dataset.3org$STUDYID == "JSH", ])),
    NHO =  c(nho_total, 0, MHTERM_nho_fail_dropout, 0, 0, nho_year_dropout, nho_reverse_dropout, nrow(sys_fail_dropout[sys_fail_dropout$STUDYID == "NHOH",]),  nrow(dataset.3org[dataset.3org$STUDYID == "NHOH", ]))
  )
  write.csv(dropout, paste0(prtpath, "/output/dropout", "_", kToday, ".csv"), row.names = F)
  # flagが2の場合はここで終わりにする、2ではないときは、次へ行く、というのを入れたい  #
  # →if文以降の処理をelse側にそのまま入れた
  
} else {
  # count用に"1"を入力
  dataset.3org$count <- 1
  
  # 集計対象年のみ抽出
  dataset.3org_yyyy <- dataset.3org[format(as.Date( dataset.3org$created.date), "%Y%m%d") <= date.cutoff & as.integer(substr(dataset.3org$MHSTDTC, 1, 4)) == kYear, ]
  
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
  res.by.disease<- merge(wip.by.disease, Disease_Name_v2, by.x = "MHDECOD", by.y = "code", all = T )
  
  # NA処理
  res.by.disease[is.na(res.by.disease)] <- 0
  write.csv(res.by.disease, paste0(prtpath, "/output/result_disease.csv"), row.names = F, fileEncoding = "CP932")
  
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
  # readcsvの方法を変更したことにより、一部項目名を修正（2022.07.27 Agata.K）
  dxt.jspho1 <- dxt.jspho[, c("登録コード", "CMLの細分類", "MDS染色体", "AML 染色体遺伝子", "骨髄異形成関連変化随伴急性骨髄性白血病", "AML FAB分類",
                              "急性赤白血病", "AML詳細", "血液腫瘍性 疾患名", "FAB分類", "ヘアリー細胞白血病", "多発性骨髄腫", "濾胞性リンパ腫",
                              "濾胞性リンパ腫国際予後因子..FLIPI", "びまん性大細胞型Ｂ細胞性リンパ腫", "血管内大細胞型Ｂ細胞性リンパ腫",
                              "キャッスルマン", "成人Ｔ細胞白血病リンパ腫", "腸管症関連Ｔ細胞リンパ腫", "末梢性Ｔ細胞リンパ腫", "HL Stage(Ann Arbor)",
                              "HL付加事項", "HL.国際予後スコア.IPS.", "免疫不全関連リンパ腫の場合", "再生不良性貧血の重症度", "続発性赤芽球癆の場合、原疾患",
                              "サラセミア", "温式自己免疫性溶血性貧血（AIHA)", "温式自己免疫性溶血性貧血が二次性の場合、その原因", "寒冷凝集素症",
                              "二次性寒冷凝集素症", "ビタミンB12欠乏性貧血の原因", "ビタミンB12欠乏性貧血の原因が内因子の欠乏の場合",
                              "葉酸欠乏性貧血の場合の原因", "鉄芽球性貧血", "慢性特発性血小板減少性紫斑病 診断時の血小板数",
                              "慢性特発性血小板減少性紫斑病の場合、抗リン脂質抗体の有無", "ヘパリン起因性血小板減少症", "ヘパリン起因性血小板減少症.抗HIT抗体.",
                              "凝固異常症.血友病A.インヒビター合併." ,"凝固異常症.血友病B.インヒビター合併.", "抗リン脂質抗体症候群の分類",
                              "抗リン脂質抗体症候群の場合、合併症", "無顆粒球症の原因")]
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
  
  ## JSH
  dxt.jsh <- m.jsh[, c(1, 2, 13:188)]
  dxt.jsh$AML詳細 <- "取得なし"
  # readcsvの方法を変更したことにより、一部項目名を修正（2022.07.27 Agata.K）
  dxt.jsh1 <- dxt.jsh[, c("登録コード", "CML病期", "MDS染色体", "APL", "AML/MRC", "AML(M5)", "AML(M6)", "AML詳細", "Tリンパ芽球性白血病・リンパ腫",
                          "FAB分類","ヘアリーセル白血病（HCL）","多発性骨髄腫", "濾胞性リンパ腫", "濾胞性リンパ腫国際予後因子 （FLIPI）", "国際予後因子：IPI",
                          "血管内B細胞リンパ腫（IVLBCL)", "キャッスルマン病", "ATLL", "EATL", "T/NK細胞腫瘍：PTCL", "Ann Arbor 分類病期", "付加事項",
                          "HL　国際予後スコア(IPS)", "免疫不全関連リンパ腫の場合", "再生不良性貧血の重症度", "続発性赤芽球癆（原疾患）", "サラセミア（細分類）",
                          "自己免疫性溶血性貧血AIHA", "二次性自己免疫性溶血性貧血AIHAの詳細", "寒冷凝集素症", "二次性寒冷凝集素症",
                          "巨赤芽球性貧血 ビタミンB12欠乏", "巨赤芽球性貧血 ビタミンB12欠乏 内因子の欠乏", "巨赤芽球性貧血 葉酸欠乏" ,
                          "鉄芽球性貧血: Sideroblastic anemia(SA)", "ITP （血小板数　/μL）", "ITP（抗リン脂質抗体）", "ヘパリン起因性血小板減少症",
                          "ヘパリン起因性血小板減少症（抗HIT抗体）",  "凝固異常症：血友病A（インヒビター合併）","凝固異常症：血友病B（インヒビター合併）",
                          "抗リン脂質抗体症候群", "抗リン脂質抗体症候群（合併症）", "無顆粒球症")]
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
  
  ## NHOH
  # dxt.nhoh <- m.nhoh[, c(1, 2, 13:294)]
  # dxt.nhoh$骨髄異形成関連変化随伴急性骨髄性白血病 <- "取得なし"
  # dxt.nhoh$FAB分類 <- "取得なし"
  # dxt.nhoh$続発性赤芽球癆.原疾患. <- "取得なし"
  # dxt.nhoh$サラセミア <- "取得なし"
  # dxt.nhoh$自己免疫性溶血性貧血AIHA <- "取得なし"
  # dxt.nhoh$AIHA_二次性の場合の原因 <- "取得なし"
  # dxt.nhoh$寒冷凝集素症 <- "取得なし"
  # dxt.nhoh$二次性寒冷凝集素症 <- "取得なし"
  # dxt.nhoh$巨赤芽球性貧血.ビタミンB12欠乏 <- "取得なし"
  # dxt.nhoh$巨赤芽球性貧血.ビタミンB12欠乏.内因子の欠乏 <- "取得なし"
  # dxt.nhoh$葉酸欠乏性貧血の場合の原因 <- "取得なし"
  # dxt.nhoh$鉄芽球性貧血 <- "取得なし"
  # dxt.nhoh$ヘパリン起因性血小板減少症 <- "取得なし"
  # dxt.nhoh$ヘパリン起因性血小板減少症.抗HIT抗体. <- "取得なし"
  # # readcsvの方法を変更したことにより、一部項目名を修正（2022.07.27 Agata.K）
  # dxt.nhoh1 <- dxt.nhoh[, c("登録コード", "慢性骨髄増殖性白血病(CML)：（病期） MPN_4","骨髄異形成症候群：（染色体）", "急性骨髄性白血病：APL with t(15;17) and variantsの詳細" ,
  #                           "骨髄異形成関連変化随伴急性骨髄性白血病", "急性骨髄性白血病：FAB分類", "急性骨髄性白血病：Acute erythroid leukemiaの詳細",
  #                           "急性骨髄性白血病：染色体・遺伝子解析が不可能（発病形式）", "Tリンパ芽球性白血病・リンパ腫", "FAB分類", "Mature B-cell neoplasms：有毛細胞白血病（Variant）",
  #                           "Mature B-cell neoplasms：多発性骨髄腫の詳細 MB_4_3","Mature B-cell neoplasms：濾胞性リンパ腫（組織型）", "濾胞性リンパ腫国際予後因子 （FLIPI）",
  #                           "国際予後因子（全年齢）(International Prognostic Index：IPI)","Mature B-cell neoplasms：血管内B細胞リンパ腫（Variant）",  "Mature B-cell neoplasms：Castleman病（細分類）",
  #                           "T/NK細胞腫瘍：成人T細胞白血病/リンパ腫（病型） TNK_5_1", "T/NK細胞腫瘍：Enteropathy-associated T-cell lymphoma（病型）",
  #                           "T/NK細胞腫瘍：末梢性T細胞リンパ腫、特定不能（Variant）", "Ann Arbor 分類病期", "付加事項", "HL　国際予後スコア（International Prognostic Score：IPS）", "Mature B-cell neoplasms：免疫不全関連リンパ腫の詳細",
  #                           "Aplastic anemia の重症度", "続発性赤芽球癆.原疾患.", "サラセミア", "自己免疫性溶血性貧血AIHA", "AIHA_二次性の場合の原因",
  #                           "寒冷凝集素症", "二次性寒冷凝集素症", "巨赤芽球性貧血.ビタミンB12欠乏", "巨赤芽球性貧血.ビタミンB12欠乏.内因子の欠乏",
  #                           "葉酸欠乏性貧血の場合の原因", "鉄芽球性貧血", "血小板減少症：特発性血小板減少性紫斑病（血小板数）", "血小板減少症：特発性血小板減少性紫斑病（抗リン脂質抗体）",
  #                           "ヘパリン起因性血小板減少症",  "ヘパリン起因性血小板減少症.抗HIT抗体.", "凝固異常症：血友病A（インヒビター合併）", "凝固異常症：血友病B（インヒビター合併）",
  #                           "血栓傾向：抗リン脂質抗体症候群（分類）",  "血栓傾向：抗リン脂質抗体症候群（合併症）","好中球減少症：無顆粒球症の詳細")]
  # colnames(dxt.nhoh1) <- c("登録コード", "CMLの細分類", "MDS染色体", "急性前骨髄球性白血病_染色体遺伝子","骨髄異形成関連変化随伴急性骨髄性白血病_詳細",
  #                          "AML.FAB分類", "急性赤白血病_詳細", "AML_詳細", "Tリンパ芽球性白血病_リンパ腫" ,"FAB分類", "ヘアリー細胞白血病",
  #                          "多発性骨髄腫_詳細",  "濾胞性リンパ腫_詳細","濾胞性リンパ腫国際予後因子_FLIPI", "びまん性大細胞型Ｂ細胞性リンパ腫_詳細",
  #                          "血管内大細胞型Ｂ細胞性リンパ腫_詳細", "キャッスルマン_詳細", "成人Ｔ細胞白血病リンパ腫_詳細" , "腸管症関連Ｔ細胞リンパ腫_詳細",
  #                          "末梢性Ｔ細胞リンパ腫_詳細", "HL.Stage.Ann.Arbor","HL付加事項", "HL.国際予後スコア.IPS.", "免疫不全関連リンパ腫の場合",
  #                          "再生不良性貧血の重症度", "続発性赤芽球癆の場合.原疾患", "サラセミア", "温式自己免疫性溶血性貧血.AIHA.",  "温式自己免疫性溶血性貧血が二次性の場合.その原因",
  #                          "寒冷凝集素症", "二次性寒冷凝集素症", "ビタミンB12欠乏性貧血の原因", "ビタミンB12欠乏性貧血の原因が内因子の欠乏の場合",
  #                          "葉酸欠乏性貧血の場合の原因", "鉄芽球性貧血", "JSH.NHOH_ITP_血小板数.JSPHO_慢性特発性血小板減少性紫斑病の血小板数",
  #                          "JSH.NHOH_ITP_抗リン脂質抗体.JSPHO_慢性特発性血小板減少性紫斑病の場合の抗リン脂質抗体","ヘパリン起因性血小板減少症",
  #                          "ヘパリン起因性血小板減少症.抗HIT抗体.", "凝固異常症.血友病A.インヒビター合併." ,"凝固異常症.血友病B.インヒビター合併.",
  #                          "抗リン脂質抗体症候群の分類", "抗リン脂質抗体症候群の場合.合併症", "無顆粒球症の原因" )
  # syousai_nhoh <- merge(nhoh.1, dxt.nhoh1, by.x = "SUBJID", by.y = "登録コード", all.x = T)
  
  #バインド
  dataset.3org.syousai <- rbind(syousai_jspho, syousai_jsh)
  # dataset.3org.syousai <- rbind(syousai_jspho, syousai_jsh, syousai_nhoh)
  
  # age diagnosis
  dataset.3org.syousai$age.diagnosis <- YearDif(dataset.3org.syousai$BRTHDTC, dataset.3org.syousai$MHSTDTC)
  dataset.3org.syousai$cat.age.diagnosis <- cut(dataset.3org.syousai$age.diagnosis, breaks = c(0, 15, 20, 30, 40, 150),
                                                labels= c("0-14", "15-19", "20-29", "30-39", "40-"), right=FALSE)
  # 集計対象年のみ抽出
  dataset.3org.syousai <- dataset.3org.syousai[format(as.Date(dataset.3org.syousai$created.date), "%Y%m%d") <= date.cutoff & as.integer(substr(dataset.3org.syousai$MHSTDTC, 1, 4)) == kYear, ]
  # WHO分類のCSVをマージする
  dataset.3org.syousai <- merge(dataset.3org.syousai, Disease_Name_v2, by.x = "MHDECOD", by.y = "code", all.x = T)
  dataset.3org.syousai[is.na(dataset.3org.syousai)] <- ""
  
  # # JSH-376891のSITEIDを95210074(2022.08.23 Agata.K 2021年診断のみ対応)
  # dataset.3org.syousai$SITEID <- ifelse(dataset.3org.syousai$SUBJID == 376891, 95210074, dataset.3org.syousai$SITEID)
  
  # CSV出力
  write.csv(dataset.3org.syousai, paste0(prtpath, "/output/JSH_JSPHO_ads.csv"), row.names = F, fileEncoding = "CP932")
  
}
