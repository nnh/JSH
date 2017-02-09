# JSH NHOH JSPHO 3団体を繋げる
# 2017/2/2
# Mamiko Yonejima

setwd("./rawdata")
jsh <- read.csv("JSH_report_160905_1920.csv", as.is=T, fileEncoding="CP932")
jspho <- read.csv("JSPHO_registration_160720_1501.csv", as.is=T, fileEncoding="CP932")
nhoh <- read.csv("NHOH_report_161216_1045.csv", as.is=T, fileEncoding="CP932")
prefecture <- read.csv("../input/prefectures.csv", header=F, as.is=T, fileEncoding="UTF-8-BOM")
colnames(prefecture) <- c("SCSTRESC", "県名", "ふりがな", "ローマ字", "地区")

# 診断年月日2012年以降、腫瘍性病変のみを抽出
nhoh.1 <- nhoh[as.integer(substr(nhoh$診断年月日, 1, 4)) > 2011 & nhoh$field2 < 1000,
               c("登録コード", "性別", "住所", "生死", "死亡日", "最終確認日", "シート作成時施設コード", "field2",
                 "確定診断名", "生年月日", "診断年月日")]
nhoh.1$prefecture <- sub(" ", "", substr(nhoh.1$住所, 1, 4))  # 県名抽出
nhoh.2 <- merge(nhoh.1, prefecture, by.x="prefecture", by.y="県名", all.x=T)
colnames(nhoh.2)[2:12] <- c("SUBJID", "SEX", "住所", "DTHFL", "DTHDTC", "DSSTDTC", "SITEID", "MHDECOD", "MHTERM",
                          "BRTHDTC", "MHSTDTC")
nhoh.3 <- nhoh.2[, c("SUBJID", "SEX", "SCSTRESC", "DTHFL", "DTHDTC", "DSSTDTC", "SITEID", "MHDECOD", "MHTERM",
  "BRTHDTC", "MHSTDTC")]
nhoh.3$STUDYID <- "NHOH"

# 診断年月日2012年以降、腫瘍性病変のみを抽出
jsh.1 <- jsh[as.integer(substr(jsh$診断年月日, 1, 4)) > 2011 & jsh$field1 < 1000,
           c("登録コード", "性別", "住所", "生死", "死亡日", "最終確認日", "シート作成時施設コード", "field1",
             "確定診断名", "生年月日", "診断日")]
colnames(jsh.1)[1:11] <- c("SUBJID", "SEX", "SCSTRESC", "DTHFL", "DTHDTC", "DSSTDTC", "SITEID", "MHDECOD", "MHTERM",
                           "BRTHDTC", "MHSTDTC")
jsh.1$STUDYID <- "JSH"

setwd("..")
source("./programs/jsphotowho.R", chdir=F, encoding="UTF-8")
ads <- rbind(jsh.1, nhoh.3, jspho)

setwd("../output")
write.csv(ads, "ads_3_organization.csv", row.names=F, fileEncoding='CP932')
setwd("..")
