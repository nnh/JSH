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
###############ここまではtumorとつないだら削除