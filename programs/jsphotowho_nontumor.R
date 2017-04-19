#JSPHO to WHO non tumor version
#Ando Sahoko & Mamiko Yonejima
#2017/4/XX
#################################
##########ここから
setwd("./rawdata")
jspho <- read.csv("JSPHO_registration_170410_1047.csv", na.strings = "", as.is=T, fileEncoding="CP932")

YearDif <- function(starting, ending) {
  # 2つの日付の年差（切り下げ）を計算する。startingに生年月日を指定すれば満年齢計算に使用可能。
  as.integer((as.integer(format(as.Date(ending), "%Y%m%d")) - as.integer(format(as.Date(starting), "%Y%m%d"))) / 10000)
}
Sys.setlocale("LC_TIME", "C") #必須：日本時間にコンピュータ設定を合わせるforwindows

# 2012年診断以降,2016年診断
jspho$year <- substr(jspho$診断年月日, 1, 4)
jspho <- jspho[!is.na(jspho$year) & jspho$year >= 2012 &  jspho$year <= 2016, ]

# Cut jspho /age diagnosis is over　20

jspho$age_diagnosis <- YearDif(jspho$診断年月日, jspho$生年月日)
jspho <- jspho[jspho$age_diagnosis < 20, ]

# except nontumor
for (i in 1:length(jspho$登録コード)) {
  ifelse(((jspho$field7[i] == 2) |
            (jspho$field37[i] == 8 && jspho$field69[i] == 2)), jspho$MHDECOD[i] <- "non_tumor", jspho$MHDECOD[i] <- "")
}

###############ここまではtumorとつないだら削除
# Make a group of non tumor
df.non.t <- jspho[jspho$MHDECOD == "non_tumor", ]
df.non.t <- df.non.t[, c(1:298)]

#
df.non.t$MHDECOD <- ifelse(df.non.t$field7 == 2 & df.non.t$field84 == 1 & df.non.t$field88 == 4, 1001,
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
                  　ifelse(df.non.t$field7 == 2 & df.non.t$field84 == 10 & df.non.t$field117 == 4, 1049,
                  　ifelse(df.non.t$field7 == 2 & df.non.t$field84 == 11 & df.non.t$field120 == 1, 1050,
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
                  　ifelse(df.non.t$field7 == 2 & df.non.t$field84 == 16 & df.non.t$field153 == 3, 1102, NA)))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))
                                ##TODO　前半、後半でDFを分ける   )
## TODO Ando 以下にサンプルかる書いておきます
df.tumor$MHDECOD <- ifelse(is.na(df.tumor$MHDECOD1), df.tumor$MHDECOD2, df.tumor$MHDECOD1)  # 空欄はあてはまらないもの
