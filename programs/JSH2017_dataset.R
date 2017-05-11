# JSPHO to WHO mapping 2017年集計用
# Mamiko Yonejima
# 2017/4/20

setwd("./rawdata")
jspho <- read.csv("JSPHO_registration_170510_1715.csv", na.strings = c(""), as.is=T, fileEncoding="CP932")
jsh <- read.csv("JSH_report_170510_1753.csv", as.is=T, fileEncoding="CP932")
jsh.rgst <- read.csv("JSH_registration_170510_1753.csv", as.is=T, fileEncoding="CP932")
nhoh <- read.csv("NHOH_report_161018_1705.csv", as.is=T, fileEncoding="CP932")
nhoh.rgst <- read.csv("NHOH_registration_161018_1705.csv", as.is=T, fileEncoding="CP932")

setwd("../input")
disease <- read.csv("disease.csv", fileEncoding="UTF-8-BOM", header=F, as.is=T, na.strings = c(""))  
colnames(disease) <- c("大分類", "MHDECOD", "病名略", "MHTERM", "病名英語", "MHDECODplus.0", "中分類番号1", "中分類略名" ,"中分類名日本語" ,"中分類名英語", "tumor_or_nontumor", "中分類番号2")
facilities <- read.csv("facilities.csv", fileEncoding="UTF-8-BOM", header=T, as.is=T)  

source("../programs/jsphotowho.R", chdir=F, encoding="UTF-8")
source("../programs/jsphotowho_nontumor.R", chdir=F, encoding="UTF-8")

#施設コードをマージする処理(NHOH)
p.nhoh.rgst <- nhoh.rgst[,c("登録コード","初発時住所")]
p.nhoh.rgst$SCSTRESC <- floor(as.integer(sub("^.*.-","",p.nhoh.rgst$初発時住所))/1000)
m.nhoh <- merge(nhoh,p.nhoh.rgst, by="登録コード", all.x= T)

# 診断年月日2012年以降、腫瘍性病変のみを抽出
nhoh.1 <- m.nhoh[as.integer(substr(m.nhoh$診断年月日, 1, 4)) > 2011 & as.integer(substr(m.nhoh$診断年月日, 1, 4)) <= 2016 ,
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
jsh.1 <- m.jsh[as.integer(substr(m.jsh$診断年月日, 1, 4)) > 2011 & as.integer(substr(m.jsh$診断年月日, 1, 4)) <= 2016 ,
               c("登録コード", "性別", "SCSTRESC", "生死", "死亡日", "最終確認日", "シート作成時施設コード", "field1",
                 "確定診断名", "生年月日", "診断日")]
colnames(jsh.1)[1:11] <- c("SUBJID", "SEX", "SCSTRESC", "DTHFL", "DTHDTC", "DSSTDTC", "SITEID", "MHDECOD", "MHTERM",
                           "BRTHDTC", "MHSTDTC")
jsh.1$STUDYID <- "JSH"

dataset.3org <- rbind(jsh.1, nhoh.1, jspho.1, jspho.non.t.1)  # 3団体を繋げた基本のデータセットを作成
dataset.3org$age.diagnosis <- YearDif(dataset.3org$BRTHDTC, dataset.3org$MHSTDTC)
dxt.dataset.3org.year <- dataset.3org[substr(dataset.3org$MHSTDTC, 1, 4) == 2016, ]  # 診断年のみ抽出
#詳細集計用に一度出力
setwd("../output")
write.csv(dxt.dataset.3org.year, "dataset_3org.csv", row.names = F)

dxt.dataset.3org.year$count <- 1

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
                                    labels= c("<20", "20 <="), right=FALSE)
by.disease <- xtabs(count ~ MHDECOD + cat.age.diagnosis, data = dxt.dataset.3org.year)
by.disease.mat <- matrix(by.disease , nrow(by.disease), ncol(by.disease))
colnames(by.disease.mat) <- c("less than 20y", "over.20y")
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

# dxt.jspho <- jspho[, c(15, 39:286)]  # 詳細をくっつける
# 
# ads.jspho <- merge(dataset.jspho, dxt.jspho,  by.x = "SUBJID", by.y = "登録コード", all.x = T)
# ads.jspho[is.na(ads.jspho)] <- ""
# setwd("../output")
# write.csv(ads.jspho, "Mappig.data.JSPHO_for_JSH.csv", row.names = F)
# setwd("..")
