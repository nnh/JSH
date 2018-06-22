# JSPHO to WHO mapping
# Mamiko Yonejima
# 2017/4/19

YearDif <- function(starting, ending) {
# 2つの日付の年差（切り下げ）を計算する。startingに生年月日を指定すれば満年齢計算に使用可能。
  as.integer((as.integer(format(as.Date(ending), "%Y%m%d")) - as.integer(format(as.Date(starting), "%Y%m%d"))) / 10000)
}
Sys.setlocale("LC_TIME", "C") #必須：日本時間にコンピュータ設定を合わせるforwindows

# 2012年診断以降,2016年診断
jspho$year <- substr(jspho$診断年月日, 1, 4)
jspho <- jspho[!is.na(jspho$year) & jspho$year >= 2012 &  jspho$year <= 2017, ]

# Cut jspho /age diagnosis is over　20
jspho$age_diagnosis <- YearDif(jspho$生年月日, jspho$診断年月日)
jspho <- jspho[jspho$age_diagnosis < 20, ]

# except nontumor
jspho$flag <- ifelse(jspho$field7 == 2 | (jspho$field7 == 1 & jspho$field37 == 8 & jspho$field69 == 2), "non_tumor", "tumor")

# Make a group of tumor
df.tumor <- subset(jspho, jspho$flag == "tumor")
# WHO分類にあてはめる
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
                     ifelse(df.tumor$field37 == 1 & df.tumor$field17 == 2, 70,  # End Classification of ALL
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
                     ifelse(df.tumor$field37 == 4 & df.tumor$field159 == 2 & df.tumor$field164 == 2 & df.tumor$field47 == 3, 18,
                     ifelse(df.tumor$field37 == 8 & df.tumor$field69 == 3, 9005, NA))))))))))))))))))))))))))))))))))))))))

df.tumor$MHDECOD2 <-ifelse(df.tumor$field37 == 4 & df.tumor$field159 == 2 & df.tumor$field164 == 2 & df.tumor$field47 == 4, 19,
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
                    ifelse(df.tumor$field37 == 7 & df.tumor$field77 == 3, 155, 
                    ifelse(df.tumor$field37 == 7 & df.tumor$field77 == 5, 9001, #  あてはまらない病名に仮コードを付与 # その他のリンパ増殖性疾患_その他	9001
                    ifelse(df.tumor$field37 == 5 & df.tumor$field55 == 12, 9002, #  NHL_病理診断_その他			9002 
                    ifelse(df.tumor$field37 == 9, 9003,NA)))))))))))))))))))))))))))))))))))))))))))) #  その他の造血器腫瘍			9003


df.tumor$MHDECOD <- ifelse(is.na(df.tumor$MHDECOD1), df.tumor$MHDECOD2, df.tumor$MHDECOD1)  # 空欄はあてはまらないもの

#SCSTRESC
df.tumor$SCSTRESC <- floor(as.integer(sub("^.*.-","",df.tumor$初発時住所))/1000)
#STUDYID
df.tumor$STUDYID <- "JSPHO"

# Read external data
p.disease <- subset(disease, disease$tumor_or_nontumor == "tumor")
jspho.0 <- merge(df.tumor, p.disease, by="MHDECOD", all.x=T)

# Pick up some jspho using
WHOjspho <- jspho.0[, c("作成日","生年月日", "診断年月日", "STUDYID", "登録コード", "性別", "SCSTRESC", "生死", "死亡日",
                        "最終確認日", "field161", "MHTERM", "MHDECOD")]
colnames(WHOjspho)[1:11] <- c("created.date","BRTHDTC", "MHSTDTC", "STUDYID", "SUBJID", "SEX", "SCSTRESC", "DTHFL", "DTHDTC",
                              "DSSTDTC", "SITEID")
jspho.1 <- WHOjspho[, c("created.date", "SUBJID", "SEX", "SCSTRESC", "DTHFL", "DTHDTC", "DSSTDTC", "SITEID", "MHDECOD", "MHTERM",
                      "BRTHDTC", "MHSTDTC", "STUDYID")]
