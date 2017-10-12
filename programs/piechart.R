# pie chart
# Mariko Ohtsuka
# 2017/10/10
# package : sas7bdat
# Import library
library(sas7bdat)

# Constant section
# 世代(AGECAT2N)  1:child,2:aya,3:adult,4:old, all列に合計を入れる
kGenelst <- c("child","aya","adult","old","all")
gene_cnt <- length(kGenelst)
# グラフの色をセット 上位11病名+合計
kRainbow <- rainbow(12)
# rainbowから1赤、4緑、8ロイヤルブルー、3黄、12マゼンタ、6緑、9青、2オレンジ、11赤紫、5緑、7水色、10青紫
kGraph_color <- c(kRainbow[1], kRainbow[4], kRainbow[8], kRainbow[3], kRainbow[12], kRainbow[6], kRainbow[9], kRainbow[2], kRainbow[11], kRainbow[5], kRainbow[7], kRainbow[10])
# 実行環境がmacかwindowsか
kOs_mac <- 0
kOs_win <- 1
# todo 実行環境情報取得
# getenv_platform <- Sys.getenv("R_PLATFORM")
# if (grep(getenv_platform, pattern="apple") == 1) {
#   # mac
#   platform_f <- kOs_mac
# } elseif (grep(getenv_platform, pattern="windows") == 1) {
#   # windows
#   platform_f <- kOs_win
# }
platform_f <- kOs_win

# InputData path
sasdat_parent_path <- "//aronas/Stat/Trials"
sasdat_ads.folder <- "JSH2017/ADS"
sasdat_name <- "ads.sas7bdat"
sasdat_path <- paste(sasdat_parent_path, sasdat_ads.folder, sasdat_name, sep="/")
# OutputData path
output_path <- "C:/Users/MarikoOhtsuka/Desktop/plot/"
if (platform_f == kOs_mac) {
  output_ext <- ""
} else {
  output_ext <- ".xmf"
}
# READ SASOutputData
sasdat <- read.sas7bdat(sasdat_path)

# InputDataから疾患群リスト作成
mhgrpterm_lst <- levels(sasdat$MHGRPTERM)
# for (i in 1:length(mhgrpterm_lst)) {
i <- 1
# 疾患群ごとにデータ分け
wk_sasdat <- subset(sasdat, MHGRPTERM == mhgrpterm_lst[i])
# 疾患群ごとに詳細病名リストが変更になるので入れなおす
mhterm_lst <- levels(factor(wk_sasdat$MHTERM))
# 世代ごとの列を作成した空のデータフレームを作成
dst_gene_mhterm <- data.frame(matrix(rep(NA, gene_cnt), nrow=1))[numeric(0), ]
colnames(dst_gene_mhterm) <- kGenelst
# 世代ごとに集計
for (k in 1:gene_cnt){
  # 詳細病名ごとに件数集計
  for (j in 1:length(mhterm_lst)){
    wk_disease <- mhterm_lst[j]
    if (k!=gene_cnt) {
      # 各世代の列は世代ごとに集計
      dst_wk_sum <- subset(wk_sasdat, ((MHTERM == wk_disease) & (AGECAT2N == k)))
    } else {
      # ALL列に合計を格納
      dst_wk_sum <- subset(wk_sasdat, MHTERM == wk_disease)
    }
    # dst_gene_mhterm[wk_disease, kGenelst[k]] <- sum(dst_wk_sum$wk_sum)
    dst_gene_mhterm[wk_disease, kGenelst[k]] <- nrow(dst_wk_sum)
  }
}
# 世代ALLの詳細病名件数の降順にソートし、色を決定
# 上位11＋その合計の表を作成
sort_key <- kGenelst[gene_cnt]
wk_sortlist <- order(dst_gene_mhterm[[sort_key]], decreasing=T)
dst_piechart <- dst_gene_mhterm[wk_sortlist, ]
if (nrow(dst_piechart) > 11) {
  dst_piechart <- dst_piechart[1:11]
}
# パイチャート設定色をセット
dst_piechart$graph_color <- kGraph_color[1:nrow(dst_piechart)]

# 各項目のパーセンテージラベル作成作業用列
dst_piechart$wk_per <- NA
dst_piechart$wk_lbl <- NA
# グラフ生成
graphics.off()

par(mai=c(0, 0, 0, 0)) #余白
par(omi=c(0, 0, 0, 0))
for (m in 1:gene_cnt){
  wk_disease_list <- rownames(dst_piechart)
  # 疾患群名+連番でファイル名を生成
  # 禁止文字の除去
  # todo 正規表現で他のも
  wk_filename <- gsub("/", "", wk_disease_list[i])
  output_filename <- paste(output_path, wk_filename, "_", m, output_ext, sep="")
  # メタファイルに出力
  if (platform_f == kOs_mac) {
    # todo mac
  } else {
    win.metafile(filename=output_filename)
  }
    # 各項目のパーセンテージラベル作成
  wk_denom <- sum(dst_piechart[,m])
  dst_piechart$wk_per <- floor(((dst_piechart[ ,m] / wk_denom) * 100) + 0.5)
  dst_piechart$wk_lbl <- paste(dst_piechart$wk_per, "%")
  # パイチャート出力
  pie(dst_piechart[ ,m], label=dst_piechart$wk_lbl, col=dst_piechart$graph_color, radius=0.5)
  legend("bottom", legend=wk_disease_list, fill=dst_piechart$graph_color, ncol=3)
  # 世代名を上部に出力
  par(new=T)
  text(0,0.7,colnames(dst_piechart[m]))
  par(new=F)
  dev.off()
}


#}

