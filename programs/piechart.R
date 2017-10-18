# Import library
library(sas7bdat)

SortDisease <- function(optdst, sort_key){
  # 病名件数降順でソート
  wk_sortlist <- order(optdst[[sort_key]], decreasing=T)
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

# Constant section
# 出力デバイス
output_ext <- "png"
# output_ext <- "eps"

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
# グラフの色をセット 上位11病名+合計 https://oku.edu.mie-u.ac.jp/~okumura/stat/colors.html
# 1空色、2青、3緑、4黄色、5オレンジ、6赤、7明るいピンク、8紫、9明るい黄緑、10 DarkBrown、11明るいグレー
kGraph_color <- c("#66ccff", "#0041ff", "#35a16b", "#faf500", "#ff9900", "#ff2800", "#ffd1d1",
                  "#9a0079", "#cbf266", "#191714", "#c8c8cb")
# 病名上位何位まで出力するか
kOutput_disease_count <- 10
# その他開始病名数
other_disease_start <- kOutput_disease_count + 1
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

# ************************************************

# 疾患大分類リスト作成
mhgrpterm_lst <- levels(factor(sasdat$MHGRPTERM))
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

# ************************************************

  # 世代ALLの詳細病名件数の降順にソートし、色を決定
  # 上位11＋その合計の表を作成, TODO(Ohtsuka): 上位10と残りの疾患合計の11に分ける

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
    other_disease <- dis_disease[other_disease_start:length(dis_disease)]
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
    dst_piechart[other_disease_start, ] <- c(apply(dst_others_piechart[ ,1:gene_cnt], 2, sum), kOthers_colname)
    # 作業用オブジェクトの削除
    rm(dst_others_piechart)
  } else {
    # 10件以下ならそのまま格納
    dst_piechart <- dst_gene_mhterm
  }
  # 世代合計の件数で再ソート
  dst_piechart <- SortDisease(dst_piechart, kGeneTotal_colname)

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
      output_filename <- paste0(gsub("[-/]", "", mhgrpterm_lst[i]), "_", kOutputSeq[m], ".", output_ext)
      output_filepath <- paste(output_path, output_filename, sep="/")
      if (output_ext == "eps") {
        setEPS()
        postscript(output_filepath)
      } else if (output_ext == "png") {
        png(output_filepath)
      }
      # 各項目のパーセンテージラベル作成
      dst_piechart$wk_per <- ResPercentage(as.numeric(dst_piechart[, m]))
      # 3%以上の場合のみラベルを出力する
      dst_piechart$wk_lbl <- ifelse(dst_piechart$wk_per > 2, paste(dst_piechart$wk_per, "%"), "")
      # パイチャート出力
      par(mar=c(8, 0.2, 1.2, 0.2))
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
