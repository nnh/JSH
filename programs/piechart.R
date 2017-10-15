# Import library
library(sas7bdat)

# Constant section
# 出力デバイス
output_ext <- ".png"

# 使用する関数名を指定
# 世代(AGECAT2N)  1:child,2:aya,3:adult,4:old, all列に合計を入れる
kGenelst <- c("child","aya","adult","old","all")
gene_cnt <- length(kGenelst)
# グラフの色をセット 上位11病名+合計 https://oku.edu.mie-u.ac.jp/~okumura/stat/colors.html
# 1空色、2青、3緑、4黄色、5オレンジ、6赤、7明るいピンク、8紫、9明るい黄緑、10茶、11明るいグレー
kGraph_color <- c("#66ccff", "#0041ff", "#35a16b", "#faf500", "#ff9900", "#ff2800", "#ffd1d1",
                  "#9a0079", "#cbf266", "#663300", "#c8c8cb")
if (Sys.getenv("R_PLATFORM") == "") {
  basepath <- "//aronas/Stat/Trials/JSH2017"   # Windows
  } else {
  basepath <- "/Volumes/Stat/Trials/JSH2017"   # Mac
}

# InputData path
ads_folder_name <- "ADS"
sasdat_name <- "ads.sas7bdat"
sasdat_path <- paste(basepath, ads_folder_name, sasdat_name, sep="/")

# OutputData path
output_folder_name <-"output"
output_path <- paste(basepath, output_folder_name, sep="/")
# output_path <- "/Users/tosh/Desktop/tempJSH2017"

# READ SAS analysis data set (ADS)
sasdat <- read.sas7bdat(sasdat_path)

# 疾患大分類リスト作成
mhgrpterm_lst <- levels(factor(sasdat$MHGRPTERM))
# mhgrpterm_lst1 <- levels(factor(paste(sasdat$MHGRPCOD,sasdat$MHGRPTERM,sep="_")))
for (i in 1:length(mhgrpterm_lst)) {
  # 疾患群ごとにデータ分け
  wk_sasdat <- subset(sasdat, MHGRPTERM == mhgrpterm_lst[i])
  # 疾患群ごとに詳細病名リストが変更になるので入れなおす
  mhterm_lst <- levels(factor(wk_sasdat$MHTERM))
  # 世代ごとの列を作成した空のデータフレームを作成
  dst_gene_mhterm <- data.frame(matrix(rep(NA, gene_cnt), nrow=1))[numeric(0), ]
  colnames(dst_gene_mhterm) <- kGenelst
  # 世代ごとに集計
  for (k in 1:gene_cnt) {
    # 詳細病名ごとに件数集計
    for (j in 1:length(mhterm_lst)) {
      wk_disease <- mhterm_lst[j]
      if (k!=gene_cnt) {
        # 各世代の列は世代ごとに集計
        dst_wk_sum <- subset(wk_sasdat, ((MHTERM == wk_disease) & (AGECAT2N == k)))
      } else {
        # ALL列に合計を格納
        dst_wk_sum <- subset(wk_sasdat, MHTERM == wk_disease)
      }
      dst_gene_mhterm[wk_disease, kGenelst[k]] <- nrow(dst_wk_sum)
    }
  }
  # 世代ALLの詳細病名件数の降順にソートし、色を決定
  # 上位11＋その合計の表を作成
  sort_key <- kGenelst[gene_cnt]
  wk_sortlist <- order(dst_gene_mhterm[[sort_key]], decreasing=T)
  dst_piechart <- dst_gene_mhterm[wk_sortlist, ]
  if (nrow(dst_piechart) > 11) {
    dst_piechart <- dst_piechart[c(1:11), ]
  }
  # パイチャート設定色をセット
  dst_piechart$graph_color <- kGraph_color[1:nrow(dst_piechart)]

  # 各項目のパーセンテージラベル作成作業用列
  dst_piechart$wk_per <- NA
  dst_piechart$wk_lbl <- NA
  # グラフ生成
  graphics.off()
  for (m in 1:gene_cnt) {
    # 0件ならスキップ
    if (sum(dst_piechart[ ,m]) > 0 ) {
      wk_disease_list <- rownames(dst_piechart)
      # 疾患群名+連番でファイル名を生成
      # 禁止文字の除去
      # todo 正規表現でまとめる
      wk_filename <- gsub("/", "", mhgrpterm_lst[i])
      wk_filename <- gsub("-", "", wk_filename)
      output_filename <- paste0(wk_filename, "_", m, output_ext)
      output_filepath <- paste(output_path, output_filename, sep="/")
      # win.metafile(filename=output_filename)
      png(output_filepath)
      # setEPS()
      # postscript(output_filename)
      # 各項目のパーセンテージラベル作成
      wk_denom <- sum(dst_piechart[,m])
      dst_piechart$wk_per <- floor(((dst_piechart[ ,m] / wk_denom) * 100) + 0.5)
      dst_piechart$wk_lbl <- paste(dst_piechart$wk_per, "%")
      # todo 0%ならラベル出力しない
      # dst_piechart$wk_lbl <- gsub("^0/%$", "", dst_piechart$wk_lbl)
      # パイチャート出力
      # todo legendの出力行数によって余白と円グラフの大きさを調整する
      par(mar=c(8, 0.2, 1.2, 0.2))
      pie(dst_piechart[ ,m], label=dst_piechart$wk_lbl, main=colnames(dst_piechart[m]),
          col=dst_piechart$graph_color, radius=0.8, cex=3, cex.main=3, clockwise=TRUE, border="white")
      par(xpd=T) # グラフの外を指定する
      # legendの列数を計算、1行20文字までとする
      # 一番長い文字数で20を割り、切り捨て
      wk_name_length <- sapply(wk_disease_list, nchar)
      max_length <- max(wk_name_length)
      if (max_length < 20) {
        column.count <- trunc(20 / max_length)
      } else {
        column.count <- 1
      }
      if (column.count > length(wk_disease_list)) {
        column.count <- length(wk_disease_list)
      }
      legend(x=par()$usr[1], y=par()$usr[3], legend=wk_disease_list, fill=dst_piechart$graph_color, cex=2, ncol=column.count)
      par(xpd=F) # グラフの中を指定する
      dev.off()
    }
  }
}
