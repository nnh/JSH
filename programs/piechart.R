# Import library
library(sas7bdat)

# function
SortOptdst <- function(iptdst, optdst, n){
  # 列名セット
  # colnames(optdst) <- c("diseasename", "cnt", "generation", "per")
  # 世代名をセット
  optdst$generation <- kGenelst[n]
  # 件数と病名をセット
  optdst$cnt <- iptdst[kGenelst[n]]
  optdst$diseasename <- iptdst$diseasename
  optdst$per <- floor(((optdst$cnt / sum(optdst$cnt)) * 100) + 0.5)
  # 病名件数降順でソート
  # wk_sortlist <- order(optdst$count, decreasing=T)
  # optdst <- optdst[wk_sortlist, ]
#  optdst <- SortDisease(optdst, "per")
  return(optdst)
}

SortDisease <- function(optdst, sort_key){
  # 病名件数降順でソート
  wk_sortlist <- order(optdst[[sort_key]], decreasing=T)
  optdst <- optdst[wk_sortlist, ]
  rownames(optdst) <- c(1:nrow(optdst))
  return(optdst)
}

# Constant section
# 出力デバイス
output_ext <- "png"
# output_ext <- "eps"

# 使用する関数名を指定
# 世代(AGECAT2N)  1:child,2:aya,3:adult,4:old, all列に合計を入れる
kGenelst <- c("child","aya","adult","old","all")
gene_cnt <- length(kGenelst)
# グラフの色をセット 上位11病名+合計 https://oku.edu.mie-u.ac.jp/~okumura/stat/colors.html
# 1空色、2青、3緑、4黄色、5オレンジ、6赤、7明るいピンク、8紫、9明るい黄緑、10 DarkBrown、11明るいグレー
kGraph_color <- c("#66ccff", "#0041ff", "#35a16b", "#faf500", "#ff9900", "#ff2800", "#ffd1d1",
                  "#9a0079", "#cbf266", "#191714", "#c8c8cb")
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
output_folder_name <- "output"
output_path <- paste(basepath, output_folder_name, output_ext, sep="/")
# output_path <- paste("C:/Users/MarikoOhtsuka/Desktop/plot", output_ext, sep="/")
# output_path <- paste("/Users/tosh/Desktop/tempJSH2017/output", output_ext, sep="/")

# READ SAS analysis data set (ADS)
sasdat <- read.sas7bdat(sasdat_path)

# 疾患大分類リスト作成
mhgrpterm_lst <- levels(factor(sasdat$MHGRPTERM))
for (i in 1:length(mhgrpterm_lst)) {
  # 疾患群ごとにデータ分け
  wk_sasdat <- subset(sasdat, MHGRPTERM == mhgrpterm_lst[i])
  # 疾患群ごとに詳細病名リストが変更になるので入れなおす
  mhterm_lst <- levels(factor(wk_sasdat$MHTERM))
  # 世代ごとの列を作成した空のデータフレームを作成
  dst_gene_mhterm <- data.frame(matrix(rep(NA, gene_cnt +1), nrow=1))[numeric(0), ]
  colnames(dst_gene_mhterm) <- c(kGenelst, "diseasename")
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
  dst_gene_mhterm$diseasename <- rownames(dst_gene_mhterm)
  rownames(dst_gene_mhterm) <- NULL
  # 世代ALLの詳細病名件数の降順にソートし、色を決定
  # 上位11＋その合計の表を作成, TODO(Ohtsuka): 上位10と残りの疾患合計の11に分ける

  # 世代毎テーブルを作成し、データをソート-

  wk_clmcnt <- 4
  wk_dst <- data.frame(matrix("", nrow(dst_gene_mhterm) * 5, wk_clmcnt))
  colnames(wk_dst) <- c("diseasename", "cnt", "generation", "per")

  dst_child <- data.frame(matrix("", nrow(dst_gene_mhterm), wk_clmcnt))
  dst_aya <- data.frame(matrix("", nrow(dst_gene_mhterm),wk_clmcnt))
  dst_adult <- data.frame(matrix("", nrow(dst_gene_mhterm), wk_clmcnt))
  dst_old <- data.frame(matrix("", nrow(dst_gene_mhterm), wk_clmcnt))
  dst_total <- data.frame(matrix("", nrow(dst_gene_mhterm), wk_clmcnt))
  colnames(dst_child) <- c("diseasename", "cnt", "generation", "per")
  colnames(dst_aya) <- c("diseasename", "cnt", "generation", "per")
  colnames(dst_adult) <- c("diseasename", "cnt", "generation", "per")
  colnames(dst_old) <- c("diseasename", "cnt", "generation", "per")
  colnames(dst_total) <- c("diseasename", "cnt", "generation", "per")

  # 世代名をセット
  dst_child$generation <- kGenelst[1]
  # 件数と病名をセット
  dst_child$cnt <- dst_gene_mhterm$child
  dst_child$diseasename <- dst_gene_mhterm$diseasename
  dst_child$per <- floor(((dst_child$cnt / sum(dst_child$cnt)) * 100) + 0.5)
  # 世代名をセット
  dst_aya$generation <- kGenelst[2]
  # 件数と病名をセット
  dst_aya$cnt <- dst_gene_mhterm$aya
  dst_aya$diseasename <- dst_gene_mhterm$diseasename
  dst_aya$per <- floor(((dst_aya$cnt / sum(dst_aya$cnt)) * 100) + 0.5)
#  dst_adult <- SortOptdst(dst_gene_mhterm, dst_adult, 3)
  # 世代名をセット
  dst_adult$generation <- kGenelst[3]
  # 件数と病名をセット
  dst_adult$cnt <- dst_gene_mhterm$adult
  dst_adult$diseasename <- dst_gene_mhterm$diseasename
  dst_adult$per <- floor(((dst_adult$cnt / sum(dst_adult$cnt)) * 100) + 0.5)
  #  dst_old <- SortOptdst(dst_gene_mhterm, dst_old, 4)
  # 世代名をセット
  dst_old$generation <- kGenelst[4]
  # 件数と病名をセット
  dst_old$cnt <- dst_gene_mhterm$old
  dst_old$diseasename <- dst_gene_mhterm$diseasename
  dst_old$per <- floor(((dst_old$cnt / sum(dst_old$cnt)) * 100) + 0.5)
#  dst_total <- SortOptdst(dst_gene_mhterm, dst_total, 5)
  # 世代名をセット
  dst_total$generation <- kGenelst[5]
  # 件数と病名をセット
  dst_total$cnt <- dst_gene_mhterm$all
  dst_total$diseasename <- dst_gene_mhterm$diseasename
  dst_total$per <- floor(((dst_total$cnt / sum(dst_total$cnt)) * 100) + 0.5)
  dst_sortall <- rbind(dst_child,dst_aya,dst_adult,dst_old,dst_total)
  # パーセンテージで降順にソート
  dst_sortall <- SortDisease(dst_sortall,"per")
  #
  dst_piechart <- data.frame(matrix(rep(NA, gene_cnt + 1), nrow=1))[numeric(0), ]
  colnames(dst_piechart) <- c(kGenelst,"diseasename")
  dst_others_piechart <- dst_piechart

  dis_disease <- unique(dst_sortall$diseasename)
  if (length(dis_disease) > 10){
    TOP10_disease <- dis_disease[1:10]
    other_disease <- dis_disease[11:length(dis_disease)]
    for (p in 1:length(TOP10_disease)){
      if (!is.na(TOP10_disease[p])){
        dst_piechart[p, ] <- subset(dst_gene_mhterm, dst_gene_mhterm$diseasename == TOP10_disease[p])
      } else {
        dst_piechart[p, ] <- NA
      }
    }
  # allの件数で再ソート
#  dst_piechart <- dst_piechart[!is.na(dst_piechart$diseasename)]
    dst_piechart <- SortDisease(dst_piechart, "all")
    for (p in 1:length(other_disease)){
      if (!is.na(other_disease[p])){
        dst_others_piechart[p, ] <- subset(dst_gene_mhterm, dst_gene_mhterm$diseasename == other_disease[p])
      } else {
        dst_others_piechart[p, ] <- NA
      }
    }
    dst_others_piechart <- subset(dst_others_piechart, !is.na(dst_others_piechart$diseasename))
    dst_piechart[11, ] <- c(apply(dst_others_piechart[ ,1:5],2,sum),"others")
#  rownames(dst_piechart) <- NULL
  } else {
    dst_piechart <- dst_gene_mhterm
  }
  # パイチャート設定色をセット
  dst_piechart$graph_color <- kGraph_color[1:nrow(dst_piechart)]
  # 各項目のパーセンテージラベル作成作業用列
  dst_piechart$wk_per <- NA
  dst_piechart$wk_lbl <- NA
  # グラフ生成
  graphics.off()
  for (m in 1:gene_cnt) { #todo 出力順変更
    # 0件ならスキップ
    wk_sum <- sum(as.numeric((dst_piechart[ ,m])))
    if (wk_sum > 0) {
      wk_disease_list <- dst_piechart$diseasename
      output_filename <- paste0(gsub("[-/]", "", mhgrpterm_lst[i]), "_", m, ".", output_ext)
      output_filepath <- paste(output_path, output_filename, sep="/")
      if (output_ext == "eps") {
        setEPS()
        postscript(output_filepath)
      } else if (output_ext == "png") {
        png(output_filepath)
      }
      # 各項目のパーセンテージラベル作成
      dst_piechart$wk_per <- floor(((as.numeric(dst_piechart[, m]) / wk_sum) * 100) + 0.5)
      # 3%以上の場合のみラベルを出力する
      dst_piechart$wk_lbl <- ifelse(dst_piechart$wk_per > 2, paste(dst_piechart$wk_per, "%"), "")
      # パイチャート出力
      par(mar=c(8, 0.2, 1.2, 0.2))
#      pie(as.numeric(dst_piechart[ ,m]))
      pie(as.numeric(dst_piechart[, m]), label=dst_piechart$wk_lbl, main=mhgrpterm_lst[i],
          col=dst_piechart$graph_color, radius=0.8, cex=2.7, cex.main=1.5, clockwise=TRUE, border="white")
      # ドーナッツグラフにする https://ladder-consulting.com/r-graphic-circle/
      par(new=TRUE)
      pie(1, radius=0.5, col='white', border='white', labels='')
      text(0, 0, labels=paste(colnames(dst_piechart[m]), "\nn =", wk_sum, "\n"), cex=2.7,
           col=par('col.main'), font=par('font.main'))
      # todo 凡例を1項目ずつ出す
      legend("bottom", legend=wk_disease_list, fill=dst_piechart$graph_color)
      dev.off()
    }
  }
}
