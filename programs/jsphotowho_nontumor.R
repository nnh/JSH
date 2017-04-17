#JSPHO to WHO non tumor version
#Ando Sahoko & Mamiko Yonejima
#2017/4/XX
#################################
##########ここから
setwd("./rawdata")
jspho <- read.csv("JSPHO_registration_160720_1501.csv", na.strings = "", as.is=T, fileEncoding="CP932")

YearDif <- function(starting, ending) {
  # 2つの日付の年差（切り下げ）を計算する。startingに生年月日を指定すれば満年齢計算に使用可能。
  as.integer((as.integer(format(as.Date(ending), "%Y%m%d")) - as.integer(format(as.Date(starting), "%Y%m%d"))) / 10000)
}
Sys.setlocale("LC_TIME", "C") #必須：日本時間にコンピュータ設定を合わせるforwindows

# 2012年診断以降,2016年診断
jspho$year <- substr(jspho$診断年月日, 1, 4)
jspho <- jspho[jspho$year >= 2012 &  jspho$year <= 2016, ]

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
                                ##TODO　Andoここから埋める     )
