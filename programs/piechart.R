# pie chart
# Mariko Ohtsuka
# 2017/10/10
# package : sas7bdat

# Import library
library(sas7bdat)
# 世代AGECAT2N  1:child,2:aya,3:adult,4:old, all列に合計を入れる
cstGene.lst <- c("child","aya","adult","old","all")
cstGene.cnt <- length(cstGene.lst)
# グラフの色をセット
graph.col.lst <- rainbow(12)

# InputData path
sasdatprt.path <- "//aronas/Stat/Trials"
sasdatads.path <- "JSH2017/ADS"
sasdat.nm <- "ads.sas7bdat"
sasdat.path <- paste(sasdatprt.path, sasdatads.path, sasdat.nm, sep="/")

# READ SAS Output Data
sasdat <- read.sas7bdat(sasdat.path)
# 件数集計用
sasdat$wk.sum <- rep(1:1, nrow(sasdat))

# データから疾患群リスト作成
MHGRPTERM.lst <- levels(sasdat$MHGRPTERM)
# for (i in 1:length(MHGRPTERM.lst)) {
  i <- 1
  # 疾患群ごとにデータ分け
  wk.sasdat <- subset(sasdat, MHGRPTERM==MHGRPTERM.lst[i])
  # 疾患群ごとに詳細病名リストが変更になる
  MHTERM.lst <- levels(factor(wk.sasdat$MHTERM))
  # 世代ごとの列を作成したデータフレーム
  wk2.ds <- data.frame(matrix(rep(NA, cstGene.cnt), nrow=1))[numeric(0), ]
  colnames(wk2.ds) <- cstGene.lst
  for (k in 1:cstGene.cnt){
  # 世代ごとに集計
    wk.gene <- cstGene.lst[k]
    for (j in 1:length(MHTERM.lst)){
      # 詳細病名ごとに件数集計
      wk.disease <- MHTERM.lst[j]
      if (k!=cstGene.cnt) {
        wk3.ds <- subset(wk.sasdat, ((MHTERM==wk.disease) & (AGECAT2N==k)))

      } else {
        # ALLに合計を格納
        wk3.ds <- subset(wk.sasdat, MHTERM==wk.disease)
      }
      wk2.ds[wk.disease, wk.gene] <- sum(wk3.ds$wk.sum)
    }
  }
  # 世代ALLの詳細病名件数の降順にソートし、色を決定
  # 上位11＋その合計の表を作成
  # Sort(desc)
  srtKey <- cstGene.lst[cstGene.cnt]
  wk.Srtlist <- order(wk2.ds[[srtKey]], decreasing=T)
  wk4.ds <- wk2.ds[wk.Srtlist, ]
  wk4.ds$graphcolor <- graph.col.lst[1:nrow(wk4.ds)]
  wk4.ds$wk.per <- NA
  wk4.ds$wk.lbl <- NA
  # グラフ生成
#  par(mfrow = c(1, cstGene.cnt))
#  test <- paste("C:/Users/MarikoOhtsuka/Desktop/plot",9 ,".png", sep="")
#  png(test, width = 2000, height = 400)  # 描画デバイスを開く
  #par(mai=c(0,0,0,0)) #余白
  #par(xpd=F,pty="s")
  #par(mgp=c(-10,-10,-10))
  for (m in 1:cstGene.cnt){
    test <- paste("C:/Users/MarikoOhtsuka/Desktop/plot",m ,".png", sep="")
    png(test, width = 500, height = 500)  # 描画デバイスを開く
    # ラベル作成
    wk.denom <- sum(wk4.ds[,m])
    wk4.ds$wk.per <- floor(((wk4.ds[,m] / wk.denom) * 100) + 0.5)
    wk4.ds$wk.lbl <- paste(wk4.ds$wk.per, "%")
    pie(wk4.ds[,m], label=wk4.ds$wk.lbl, main=colnames(wk4.ds[m]),cex.main=2, col=wk4.ds$graphcolor, radius=0.5)
    if (m==3) {
      legend("bottom",legend=rownames(wk4.ds), fill=wk4.ds$graphcolor, ncol=4)
    }
    dev.off()
  }
#  dev.off()
#}

