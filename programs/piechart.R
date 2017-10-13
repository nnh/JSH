# pie chart
# Mariko Ohtsuka
# 2017/10/10
# package : sas7bdat
# Import library
library(sas7bdat)

# Constant section
# 出力デバイス
# 使用する関数名を指定
kOut_device <- "eps"
# 世代(AGECAT2N)  1:child,2:aya,3:adult,4:old, all列に合計を入れる
kGenelst <- c("child","aya","adult","old","all")
gene_cnt <- length(kGenelst)
# グラフの色をセット 上位11病名+合計
kRainbow <- rainbow(12)
# rainbowから1赤、4緑、8ロイヤルブルー、3黄、12マゼンタ、6緑、9青、2オレンジ、11赤紫、5緑、7水色、10青紫
kGraph_color <- c(kRainbow[1], kRainbow[4], kRainbow[8], kRainbow[3], kRainbow[12], kRainbow[6], kRainbow[9], kRainbow[2], kRainbow[11], kRainbow[5], kRainbow[7], kRainbow[10])
# 実行環境がmacかwindowsか
kOs_mac <- 0
kBasepath_mac <- "/Volumes"   # Mac
kOs_win <- 1
kBasepath_win <- "//aronas"   # Windows

platform_f <- kOs_win
basepath <- kBasepath_win
# output_ext <- ".xmf"
output_ext <- ".eps"

getenv_platform <- Sys.getenv("R_PLATFORM")
if (getenv_platform != "") {
  if (grep(getenv_platform, pattern="apple") == 1) {
    # mac
    platform_f <- kOs_mac
    basepath <- kBasepath_mac
    output_ext = ""
  }
}

# InputData path
sasdat_parent_path <- basepath
sasdat_ads.folder <- "Stat/Trials/JSH2017/ADS"
sasdat_name <- "ads.sas7bdat"
sasdat_path <- paste(sasdat_parent_path, sasdat_ads.folder, sasdat_name, sep="/")
# OutputData path
output_path <- "C:/Users/MarikoOhtsuka/Desktop/plot/"

# READ SASOutputData
sasdat <- read.sas7bdat(sasdat_path)

# 疾患大分類リスト作成
mhscat_lst <- levels(factor(sasdat$MHSCAT))
for (p in 1:length(mhscat_lst)) {
  wk_cat <- subset(sasdat, MHSCAT == mhscat_lst[p])
  mhgrpterm_lst <- levels(factor(wk_cat$MHGRPTERM))
  for (i in 1:length(mhgrpterm_lst)) {
    # 疾患群ごとにデータ分け
    wk_sasdat <- subset(wk_cat, MHGRPTERM == mhgrpterm_lst[i])
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
        wk_catname <- gsub("/", "", mhscat_lst[p])
        wk_filename <- gsub("/", "", mhgrpterm_lst[i])
        wk_filename <- gsub("-", "", wk_filename)
        output_filename <- paste(output_path, wk_catname, "_", wk_filename, "_", m, output_ext, sep="")
        # メタファイルに出力
        # win.metafile(filename=output_filename)
        # png(filename=output_filename)
        setEPS()
        postscript(output_filename)
        # 各項目のパーセンテージラベル作成
        wk_denom <- sum(dst_piechart[,m])
        dst_piechart$wk_per <- floor(((dst_piechart[ ,m] / wk_denom) * 100) + 0.5)
        dst_piechart$wk_lbl <- paste(dst_piechart$wk_per, "%")
        # todo 0%ならラベル出力しない
        # dst_piechart$wk_lbl <- gsub("^0/%$", "", dst_piechart$wk_lbl)
        # パイチャート出力
        # todo legendの出力行数によって余白と円グラフの大きさを調整する
        par(mar=c(8, 0.2, 1.2, 0.2))
        pie(dst_piechart[ ,m], label=dst_piechart$wk_lbl, main=colnames(dst_piechart[m]), col=dst_piechart$graph_color, radius=0.8, cex=3, cex.main=3)
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
}
