#JSH NHOH JSPHO 3団体を繋げる
#2017/2/2
#Mamiko Yonejima

setwd("./rawdata")
filenames <- list.files()
for (i in 1:length(filenames)) {
  assign(substr(filenames[i], 1, 5), read.csv(filenames[i], as.is=T, fileEncoding='CP932'))
}
JSH_r -> jsh
NHOH_ -> nhoh
JSPHO -> jspho

setwd("../programs")
source("jsphotowho")

jsh <- jsh[,c("登録コード","性別","住所","生死","死亡日","最終確認日","シート作成時施設コード","field1","確定診断名","生年月日","診断日")]
jsh$STUDYID <- "JSH"
nhoh <- nhoh[,c("登録コード","性別","住所","生死","死亡日","最終確認日","シート作成時施設コード","field2","確定診断名","生年月日","診断年月日")]
nhoh$STUDYID <- "NHOH"

colnames(jsh)[1:11] <- c("SUBJID","SEX","SCSTRESC","DTHFL","DTHDTC","DSSTDTC","SITEID","MHDECOD","MHTERM","BRTHDTC","MHSTDTC")
colnames(nhoh)[1:11] <- c("SUBJID","SEX","SCSTRESC","DTHFL","DTHDTC","DSSTDTC","SITEID","MHDECOD","MHTERM","BRTHDTC","MHSTDTC")

ads <- rbind(jsh,nhoh,jspho)

setwd("../output")
write.csv(ads,"ads 3Organization.csv",row.names=F)
