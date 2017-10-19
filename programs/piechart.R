# Import library
library(sas7bdat)

# function section
SortDisease <- function(optdst, sort_key){
  # 病名件数降順でソート
  wk_sortclm <- as.numeric(optdst[[sort_key]])
  wk_sortlist <- order(wk_sortclm, decreasing=T)
  optdst <- optdst[wk_sortlist, ]
  # 行ラベルの再セット
  rownames(optdst) <- c(1:nrow(optdst))
  return(optdst)
}

ResPercentage <- function(intcnt){
  # 件数列の情報からパーセンテージを計算して返す
  wk_per <- floor((intcnt / sum(intcnt) * 100) + 0.5)
  return(wk_per)
}

Open_Outputdevice <- function(extension, filepath){
  # 出力デバイスオープン
  if (extension == "eps") {
    setEPS()
    postscript(filepath)
  } else if (extension == "png") {
    png(filepath)
  }
}

# Constant section
# 出力デバイス
# output_ext <- "png"
output_ext <- "eps"

# 使用する関数名を指定
# 世代(AGECAT2N)  1:child,2:aya,3:adult,4:old, all列に合計を入れる
kGenelst <- c("child" ,"aya" ,"adult" ,"old" ,"all")
gene_cnt <- length(kGenelst)
kGeneTotal_colname <- kGenelst[length(kGenelst)]  # 合計でソートする
# ファイルの出力順
# 1:child,2:aya,3:adult,4:old,5:all
# ↓
# 2:child,3:aya,4:adult,5:old,1:total
kOutputSeq <- c(2 ,3 ,4 ,5 ,1)
# グラフの色をセット 上位10病名+その他合計 https://oku.edu.mie-u.ac.jp/~okumura/stat/colors.html
# 1空色、2青、3緑、4黄色、5オレンジ、6赤、7明るいピンク、8紫、9明るい黄緑、10 DarkBrown、11明るいグレー
kGraph_color <- c("#66ccff", "#0041ff", "#35a16b", "#faf500", "#ff9900", "#ff2800", "#ffd1d1",
                  "#9a0079", "#cbf266", "#191714", "#c8c8cb")
# その他病名は明るいグレーに集約
kOther_disease_start <- length(kGraph_color)
# 病名上位件数
kOutput_disease_count <- kOther_disease_start - 1
# 病名件数ソート用データフレーム列名
kDisease_colname <- "diseasename"
kSortkey_per <- "per"  # ソートキー：パーセンテージ
kSortall_colname <- c(kDisease_colname, "cnt", "generation", kSortkey_per)
sortall_col_cnt <- length(kSortall_colname)
kOthers_colname <- "Others"

# OS毎パス切り分け
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

# READ SAS analysis data set (ADS)
sasdat <- read.sas7bdat(sasdat_path)

# 疾患分類毎合計を集計
mhgrpterm_lst <- levels(factor(sasdat$MHGRPTERM))

# 世代毎合計を集計
wk_sum_lst <- addmargins(table(sasdat$MHGRPTERM, sasdat$AGECAT2N))
# replace "sum" -> 5
colnames(wk_sum_lst)[ncol(wk_sum_lst)] <- 5
# 集約用空のデータフレームを作成
# 世代ごとの列＋病名格納列を作成した空のデータフレームを作成
wk_dst_colname <- c(kGenelst, kDisease_colname)
dst_total <- data.frame(matrix("", length(rownames(wk_sum_lst)), length(wk_dst_colname)))
colnames(dst_total) <- wk_dst_colname
for (i in 1:gene_cnt) {
  dst_total[ ,i] <- wk_sum_lst[ ,i]
}
dst_total[ ,kDisease_colname] <- rownames(wk_sum_lst)
# 合計と病名毎計を分離
# ALLの件数降順でソート
dst_total <- SortDisease(subset(dst_total, diseasename != "Sum"), kGeneTotal_colname)
# パイチャート設定色をセット　暫定でレインボーをセット
dst_total$graph_color <- rainbow(nrow(dst_total))
# 各項目のパーセンテージラベル作成作業用列
dst_total$wk_per <- NA
dst_total$wk_lbl <- NA

# 世代毎にパイチャートを出力
for (m in 1:gene_cnt) {
  output_filename <- paste0("total_", kOutputSeq[m], ".", output_ext)
  output_filepath <- paste(output_path, output_filename, sep="/")
  # 出力デバイスオープン
  Open_Outputdevice(output_ext, output_filepath)
  # 各項目のパーセンテージ計算
  dst_total$wk_per <- ResPercentage(as.numeric(dst_total[, m]))
  # 3%以上の場合のみラベルを出力する
  dst_total$wk_lbl <- ifelse(dst_total$wk_per > 2, paste(dst_total$wk_per, "%"), "")
  # 余白の設定（下、左、上、右）
  par(mar=c(0, 2.2, 0, 2.2))
  pie(as.numeric(dst_total[, m]), label=dst_total$wk_lbl,
      col=dst_total$graph_color, radius=0.8, cex=2.7, clockwise=TRUE, border="white")
  # ドーナッツグラフにする https://ladder-consulting.com/r-graphic-circle/
  par(new=TRUE)
  pie(1, radius=0.5, col='white', border='white', labels='')
  text(0, 0, labels=paste(colnames(dst_total[m]), "\nn =", sum(as.numeric((dst_total[ ,m]))), "\n"), cex=2.7,
       col=par('col.main'), font=par('font.main'))
  # 出力デバイスクローズ
  dev.off()
}
# ダミーグラフ内にlegend出力
output_filename <- paste0("total_legend",  ".", output_ext)
output_filepath <- paste(output_path, output_filename, sep="/")
# 出力デバイスオープン
Open_Outputdevice(output_ext, output_filepath)
# 余白の設定（下、左、上、右）
par(mar=c(0, 0, 0, 0))
pie(1, col="white", radius=0.1, clockwise=TRUE, border="white", labels = "")
legend("center", legend=dst_total$diseasename, cex=2.7, fill=dst_total$graph_color, ncol=1)
# 出力デバイスクローズ
dev.off()

for (i in 1:length(mhgrpterm_lst)) {
  # 疾患群ごとにデータ分け
  wk_sasdat <- subset(sasdat, MHGRPTERM == mhgrpterm_lst[i])
  # 疾患群ごとに詳細病名リストが変更になるので入れなおす
  mhterm_lst <- levels(factor(wk_sasdat$MHTERM))
  # 世代ごとの列＋病名格納列を作成した空のデータフレームを作成
  dst_gene_mhterm <- data.frame(matrix(rep(NA, (gene_cnt + 1)), nrow=1))[numeric(0), ]
  colnames(dst_gene_mhterm) <- c(kGenelst, kDisease_colname)
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
       dst_gene_mhterm[j, kGenelst[k]] <- nrow(dst_wk_sum)
    }
  }
  # 病名リストを格納
  dst_gene_mhterm$diseasename <- mhterm_lst
  # 作業用オブジェクトの削除
  rm(dst_wk_sum)
  rm(wk_sasdat)

  # 世代ALLの詳細病名件数の降順にソートし、色を決定
  # 上位10と残りの疾患合計の11に分ける

  # 世代毎テーブルを作成し、データをソート
  # 集約用空のデータフレームを作成
  dst_sortall <- data.frame(matrix(rep(NA, sortall_col_cnt), nrow=1))[numeric(0), ]
  colnames(dst_sortall) <- kSortall_colname
  # 作業用、病名数の行を作成したデータフレーム
  wk_dst_gene <- data.frame(matrix("", nrow(dst_gene_mhterm), sortall_col_cnt))
  colnames(wk_dst_gene) <- kSortall_colname
  for (q in 1:gene_cnt) {
    # 世代毎データ集約
    # 病名は共通
    wk_dst_gene$diseasename <- dst_gene_mhterm$diseasename
    # 世代、件数
    wk_dst_gene$generation <- kGenelst[q]
    wk_dst_gene$cnt <- dst_gene_mhterm[ ,q]
    # 件数からパーセンテージを取得
    wk_dst_gene$per <- ResPercentage(wk_dst_gene$cnt)
    dst_sortall <- rbind(dst_sortall, wk_dst_gene)
  }
  # 作業用オブジェクトの削除
  rm(wk_dst_gene)
  # パーセンテージで降順にソート
  dst_sortall <- SortDisease(dst_sortall, kSortkey_per)
  #
  # 世代数＋病名の列を持つ空行のデータフレームを作成
  piechart_colname <- c(kGenelst, kDisease_colname)
  piechart_col_cnt <- length(piechart_colname)
  dst_piechart <- data.frame(matrix(rep(NA, piechart_col_cnt), nrow=1))[numeric(0), ]
  colnames(dst_piechart) <- piechart_colname
  dst_others_piechart <- dst_piechart

  # 重複病名を除去
  dis_disease <- unique(dst_sortall$diseasename)
  if (length(dis_disease) > kOutput_disease_count){
    # 病名が10件以上の場合、上位10件＋その他に集約する
    top10_disease <- dis_disease[1:kOutput_disease_count]
    other_disease <- dis_disease[kOther_disease_start:length(dis_disease)]
    for (p in 1:length(top10_disease)){
      if (!is.na(top10_disease[p])){
        # 該当病名行を抽出して格納
        dst_piechart[p, ] <- subset(dst_gene_mhterm, dst_gene_mhterm$diseasename == top10_disease[p])
      } else {
        dst_piechart[p, ] <- NA
      }
    }
    for (p in 1:length(other_disease)){
      if (!is.na(other_disease[p])){
        # 該当病名行を抽出して格納
        dst_others_piechart[p, ] <- subset(dst_gene_mhterm, dst_gene_mhterm$diseasename == other_disease[p])
      } else {
        dst_others_piechart[p, ] <- NA
      }
    }
    # NA行があれば除去
    dst_others_piechart <- subset(dst_others_piechart, !is.na(dst_others_piechart$diseasename))
    # 世代合計の件数で再ソート
    # Othersは最終行に
    dst_piechart <- SortDisease(dst_piechart, kGeneTotal_colname)
    dst_piechart[kOther_disease_start, ] <- c(apply(dst_others_piechart[ ,1:gene_cnt], 2, sum), kOthers_colname)
    # 作業用オブジェクトの削除
    rm(dst_others_piechart)
  } else {
    # 10件以下ならそのまま格納
    dst_piechart <- SortDisease(dst_gene_mhterm, kGeneTotal_colname)
  }
    # パイチャート設定色をセット
  dst_piechart$graph_color <- kGraph_color[1:nrow(dst_piechart)]
  # 各項目のパーセンテージラベル作成作業用列
  dst_piechart$wk_per <- NA
  dst_piechart$wk_lbl <- NA
  # グラフ生成
  graphics.off()
  for (m in 1:gene_cnt) {
    # 件数0なら出力しない
    wk_sum <- sum(as.numeric((dst_piechart[ ,m])))
    if (wk_sum > 0) {
      output_filename <- paste0(gsub("[-/]", "", mhgrpterm_lst[i]), "_", kOutputSeq[m], ".", output_ext)
      output_filepath <- paste(output_path, output_filename, sep="/")
      # 出力デバイスオープン
      Open_Outputdevice(output_ext, output_filepath)
      # 各項目のパーセンテージ計算
      dst_piechart$wk_per <- ResPercentage(as.numeric(dst_piechart[, m]))
      # 3%以上の場合のみラベルを出力する
      dst_piechart$wk_lbl <- ifelse(dst_piechart$wk_per > 2, paste(dst_piechart$wk_per, "%"), "")
      # 余白の設定（下、左、上、右）
      par(mar=c(0, 2.2, 0, 2.2))
      pie(as.numeric(dst_piechart[, m]), label=dst_piechart$wk_lbl,
          col=dst_piechart$graph_color, radius=0.8, cex=2.7, clockwise=TRUE, border="white")
      # ドーナッツグラフにする https://ladder-consulting.com/r-graphic-circle/
      par(new=TRUE)
      pie(1, radius=0.5, col='white', border='white', labels='')
      text(0, 0, labels=paste(colnames(dst_piechart[m]), "\nn =", wk_sum, "\n"), cex=2.7,
           col=par('col.main'), font=par('font.main'))
      # 出力デバイスクローズ
      dev.off()
    }
  }
  # ダミーグラフ内にlegend出力
  output_filename <- paste0(gsub("[-/]", "", mhgrpterm_lst[i]), "_legend",  ".", output_ext)
  output_filepath <- paste(output_path, output_filename, sep="/")
  # 出力デバイスオープン
  Open_Outputdevice(output_ext, output_filepath)
  # 余白の設定（下、左、上、右）
  par(mar=c(0, 0, 0, 0))
  pie(1, col="white", radius=0.1, clockwise=TRUE, border="white", labels = "")
  legend("center", legend=dst_piechart$diseasename, cex=2, fill=dst_piechart$graph_color, ncol=1)
  # 出力デバイスクローズ
  dev.off()
}


