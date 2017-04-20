# JSPHO to WHO mapping 2017年集計用
# Mamiko Yonejima
# 2017/4/20

setwd("./rawdata")
jspho <- read.csv("JSPHO_registration_160720_1501.csv", na.strings = c(""), as.is=T, fileEncoding="CP932")

setwd("../input")
disease <- read.csv("disease.csv", header=F, as.is=T)  # fileEncoding="UTF-8-BOM"付けると読み込めません
colnames(disease) <- c("大分類", "MHDECOD", "病名略", "MHTERM", "病名英語", "MHDECOD+0", "中分類番号", "中分類略名" ,"中分類名日本語" ,"中分類名英語", "tumor_or_nontumor", "中分類番号")

source("../programs/jsphotowho.R", chdir=F, encoding="UTF-8")
source("../programs/jsphotowho_nontumor.R", chdir=F, encoding="UTF-8")

dataset.jspho <- rbind(jspho.1, jspho.non.t.1)  # 腫瘍性と非腫瘍性をくっつける
dxt.jspho <- jspho[, c(15, 39:284)]  # 詳細をくっつける

ads.jspho <- merge(dataset.jspho, dxt.jspho,  by.x = "SUBJID", by.y = "登録コード", all.x = T)
ads.jspho[is.na(ads.jspho)] <- ""
setwd("../output")
write.csv(ads.jspho, "Mappig.data.JSPHO_for_JSH.csv", row.names = F)
setwd("..")
