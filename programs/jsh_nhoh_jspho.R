# JSH NHOH JSPHO 3団体を繋げる
# 2017/2/2
# Mamiko Yonejima

setwd("./rawdata")
jsh <- read.csv("JSH_report_160623_1459.csv", as.is=T, fileEncoding="CP932")
jsh.rgst <- read.csv("JSH_registration_160623_1459.csv", as.is=T, fileEncoding="CP932")
jspho <- read.csv("JSPHO_registration_160720_1501.csv", as.is=T, fileEncoding="CP932")
nhoh <- read.csv("NHOH_report_161018_1705.csv", as.is=T, fileEncoding="CP932")
nhoh.rgst <- read.csv("NHOH_registration_161018_1705.csv", as.is=T, fileEncoding="CP932")
disease <- read.csv("./input/disease.csv", header=F, as.is=T, fileEncoding="UTF-8-BOM")  # TODO yonejima colnamesをつける、hematologyのみ抽出するコード
prefecture <- read.csv("prefectures.csv", header=F, as.is=T, fileEncoding="UTF-8-BOM")
colnames(prefecture) <- c("SCSTRESC", "県名", "ふりがな", "ローマ字", "地区")

#施設コードをマージする処理(NHOH)
p.nhoh.rgst <- nhoh.rgst[,c("登録コード","初発時住所")]
p.nhoh.rgst$SCSTRESC <- floor(as.integer(sub("^.*.-","",p.nhoh.rgst$初発時住所))/1000)
m.nhoh <- merge(nhoh,p.nhoh.rgst, by="登録コード", all.x= T)

# 診断年月日2012年以降、腫瘍性病変のみを抽出
nhoh.1 <- m.nhoh[as.integer(substr(m.nhoh$診断年月日, 1, 4)) > 2011 & as.integer(substr(m.nhoh$診断年月日, 1, 4)) <= 2016 & (m.nhoh$field2 < 1000),
               c("登録コード", "性別", "SCSTRESC", "生死", "死亡日", "最終確認日", "シート作成時施設コード", "field2",
                 "確定診断名", "生年月日", "診断年月日")] 
colnames(nhoh.1)[1:11] <- c("SUBJID", "SEX", "SCSTRESC", "DTHFL", "DTHDTC", "DSSTDTC", "SITEID", "MHDECOD", "MHTERM",
                          "BRTHDTC", "MHSTDTC")

nhoh.1$STUDYID <- "NHOH"

#施設コードをマージする処理(JSH)
p.jsh.rgst <- jsh.rgst[,c("登録コード","初発時住所")]
p.jsh.rgst$SCSTRESC <- floor(as.integer(sub("^.*.-","",p.jsh.rgst$初発時住所))/1000)
m.jsh <- merge(jsh, p.jsh.rgst, by="登録コード", all.x= T)

# 診断年月日2012年以降、腫瘍性病変のみを抽出
jsh.1 <- m.jsh[as.integer(substr(m.jsh$診断年月日, 1, 4)) > 2011 & as.integer(substr(m.jsh$診断年月日, 1, 4)) <= 2016 & (m.jsh$field1 < 1000),
           c("登録コード", "性別", "SCSTRESC", "生死", "死亡日", "最終確認日", "シート作成時施設コード", "field1",
             "確定診断名", "生年月日", "診断日")]
colnames(jsh.1)[1:11] <- c("SUBJID", "SEX", "SCSTRESC", "DTHFL", "DTHDTC", "DSSTDTC", "SITEID", "MHDECOD", "MHTERM",
                           "BRTHDTC", "MHSTDTC")
jsh.1$STUDYID <- "JSH"

setwd("..")
source("./programs/jsphotowho.R", chdir=F, encoding="UTF-8")
ads <- rbind(jsh.1, nhoh.1, jspho.1)

setwd("./output")
write.csv(ads, "ads_3_organization.csv", row.names=F, fileEncoding='CP932')
setwd("..")
