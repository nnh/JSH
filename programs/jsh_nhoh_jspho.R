# JSH NHOH JSPHO 3団体を繋げる
# 2017/2/2
# Mamiko Yonejima
# 2017/10/5 mod Mariko Ohtsuka

# output,rowdataはaronas上にて入出力する
prtpath <- "//aronas/Datacenter/Trials/JSH/Registry"
# Rowdata path set
rowdatafld <- "2017年集計用RAWDATA"
rowdatapath <- paste(prtpath, rowdatafld, sep="/")
# output path set
outputfld <- "jsh2017/output"
outputpath <- paste(prtpath, outputfld, "ads_3_organization.csv", sep="/")

# csvname search
jsh.csvnm <- list.files(rowdatapath, pattern="JSH_report_")
jshrgst.csvnm <- list.files(rowdatapath, pattern="JSH_registration_")
jspho.csvnm <- list.files(rowdatapath, pattern="JSPHO_registration_")
nhoh.csvnm <- list.files(rowdatapath, pattern="NHOH_report_")
nhohrgst.csvnm <- list.files(rowdatapath, pattern="NHOH_registration_")
# csvpath set
jsh.csvpath <- paste(rowdatapath, jsh.csvnm, sep="/")
jshrgst.csvpath <- paste(rowdatapath, jshrgst.csvnm, sep="/")
jspho.csvpath <- paste(rowdatapath, jspho.csvnm, sep="/")
nhoh.csvpath <- paste(rowdatapath, nhoh.csvnm, sep="/")
nhohrgst.csvpath <- paste(rowdatapath, nhohrgst.csvnm, sep="/")
# csv input
jsh <- read.csv(jsh.csvpath, as.is=T, fileEncoding="CP932")
jsh.rgst <- read.csv(jshrgst.csvpath, as.is=T, fileEncoding="CP932")
jspho <- read.csv(jspho.csvpath, as.is=T, fileEncoding="CP932")
nhoh <- read.csv(nhoh.csvpath, as.is=T, fileEncoding="CP932")
nhoh.rgst <- read.csv(nhohrgst.csvpath, as.is=T, fileEncoding="CP932")
disease <- read.csv("./input/disease.csv", header=F, as.is=T, fileEncoding="UTF-8-BOM")  # fileEncoding="UTF-8-BOM"付けると読み込めません、hematologyのみ抽出するコード
colnames(disease) <- c("大分類", "MHDECOD", "病名略", "MHTERM", "病名英語", "MHDECOD+0", "中分類番号", "中分類略名" ,"中分類名日本語" ,"中分類名英語", "tumor_or_nontumor", "中分類番号")

#施設コードをマージする処理(NHOH)
p.nhoh.rgst <- nhoh.rgst[,c("登録コード","初発時住所")]
p.nhoh.rgst$SCSTRESC <- floor(as.integer(sub("^.*.-","",p.nhoh.rgst$初発時住所))/1000)
m.nhoh <- merge(nhoh,p.nhoh.rgst, by="登録コード", all.x= T)

# 診断年月日2012年以降、腫瘍性病変のみを抽出
nhoh.1 <- m.nhoh[as.integer(substr(m.nhoh$診断年月日, 1, 4)) > 2011 & as.integer(substr(m.nhoh$診断年月日, 1, 4)) <= 2016 & (m.nhoh$field2 < 1000),
                 c("作成日","登録コード", "性別", "SCSTRESC", "生死", "死亡日", "最終確認日", "シート作成時施設コード", "field2",
                   "確定診断名", "生年月日", "診断年月日")]
colnames(nhoh.1)[1:12] <- c("created.date","SUBJID", "SEX", "SCSTRESC", "DTHFL", "DTHDTC", "DSSTDTC", "SITEID", "MHDECOD", "MHTERM",
                            "BRTHDTC", "MHSTDTC")

nhoh.1$STUDYID <- "NHOH"

#施設コードをマージする処理(JSH)
p.jsh.rgst <- jsh.rgst[,c("登録コード","初発時住所")]
p.jsh.rgst$SCSTRESC <- floor(as.integer(sub("^.*.-","",p.jsh.rgst$初発時住所))/1000)
m.jsh <- merge(jsh, p.jsh.rgst, by="登録コード", all.x= T)

# 診断年月日2012年以降、腫瘍性病変のみを抽出
jsh.1 <- m.jsh[as.integer(substr(m.jsh$診断年月日, 1, 4)) > 2011 & as.integer(substr(m.jsh$診断年月日, 1, 4)) <= 2016 & (m.jsh$field1 < 1000),
               c("作成日","登録コード", "性別", "SCSTRESC", "生死", "死亡日", "最終確認日", "シート作成時施設コード", "field1",
                 "確定診断名", "生年月日", "診断日")]
colnames(jsh.1)[1:12] <- c("created.date","SUBJID", "SEX", "SCSTRESC", "DTHFL", "DTHDTC", "DSSTDTC", "SITEID", "MHDECOD", "MHTERM",
                           "BRTHDTC", "MHSTDTC")
jsh.1$STUDYID <- "JSH"

source("./programs/jsphotowho.R", chdir=F, encoding="UTF-8")
ads <- rbind(jsh.1, nhoh.1, jspho.1)

# csv output
write.csv(ads, outputpath, row.names=F, fileEncoding='CP932', na="")
