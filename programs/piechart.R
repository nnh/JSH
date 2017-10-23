# Import library
library(sas7bdat)

# function section
SortDisease <- function(input_dataframe, sort_key){
  # input_dataframeをsort_key列の降順でソートして返す
  sort_column <- as.numeric(input_dataframe[[sort_key]])
  sort_order <- order(sort_column, decreasing=T)
  output_dataframe <- input_dataframe[sort_order, ]
  # 行ラベルの再セット
  rownames(output_dataframe) <- c(1:nrow(output_dataframe))
  return(output_dataframe)
}

CalculatePercentage <- function(count_colname){
  # count_colname列の情報から各項目のパーセンテージを計算して返す
  # 数値型に変換
  count_colname <- as.numeric(count_colname)
  percentage_list <- floor((count_colname / sum(count_colname) * 100) + 0.5)
  return(percentage_list)
}

OpenOutputdevice <- function(extension, filepath){
  # 出力デバイスオープン
  if (extension == "eps") {
    setEPS()
    postscript(filepath)
  } else if (extension == "png") {
    png(filepath)
  }
}

EditDoughnutChart <- function(filepath, extension, mar_list, pie_option_list, pie_list, doughnut_text_list){
  # ドーナッツグラフ出力
  # 外円の半径、項目ラベルの文字大きさ
  pie_radius <- pie_option_list[1]
  pie_cex <- pie_option_list[2]
  # パイチャート要素、項目名、グラフ色
  pie_x <- as.numeric(pie_list[ ,1])
  pie_label <- pie_list[ ,2]
  pie_color <- pie_list[ ,3]
  # 内円の半径、円内テキストの文字大きさ、内容
  doughnut_radius <- as.numeric(doughnut_text_list[1])
  doughnut_text_cex <- as.numeric(doughnut_text_list[2])
  doughnut_text_label <- doughnut_text_list[3]

  # 出力デバイスオープン
  OpenOutputdevice(extension, filepath)
  # 余白の設定（下、左、上、右）
  par(mar = mar_list)
  pie(pie_x, label = pie_label, col = pie_color, radius = pie_radius, cex = pie_cex, clockwise = T, border = "white")
  # ドーナッツグラフにする https://ladder-consulting.com/r-graphic-circle/
  par(new = T)
  pie(1, radius = doughnut_radius, col = 'white', border = 'white', labels = '')
  text(0, 0, labels = doughnut_text_label, cex = doughnut_text_cex, col = par('col.main'), font = par('font.main'))
  # 出力デバイスクローズ
  dev.off()
}

EditLegend <- function(filepath, extension, legend_text, legend_cex, legend_fill){
  # ダミーグラフ内に凡例出力
  # 出力デバイスオープン
  OpenOutputdevice(extension, filepath)
  # 余白の設定（下、左、上、右）
  par(mar = c(0, 0, 0, 0))
  pie(1, col = "white", radius = 0.1, clockwise = T, border = "white", labels = "")
  legend("center", legend = legend_text, cex = legend_cex, fill = legend_fill, ncol = 1)
  # 出力デバイスクローズ
  dev.off()
}

# Constant section
# 出力デバイス
# output_extensionension <- "png"
output_extensionension <- "eps"

# 世代(AGECAT2N)  1:child,2:aya,3:adult,4:old
# 5:allに1～4の合計を格納
kGenerationList <- c("child", "aya", "adult", "old", "all")
generation_length <- length(kGenerationList)  # 世代数
generation_total_colname <- kGenerationList[generation_length]  # 合計（all列）でソートする
# ファイルの出力順変換
# 1:child,2:aya,3:adult,4:old,5:all
# ↓
# 2:child,3:aya,4:adult,5:old,1:total
kOrder_OutputFile <- c(2, 3, 4, 5, 1)
# グラフの色をセット 上位10病名+その他合計 https://oku.edu.mie-u.ac.jp/~okumura/stat/colors.html
# 1空色、2青、3緑、4黄色、5オレンジ、6赤、7明るいピンク、8紫、9明るい黄緑、10 DarkBrown、11明るいグレー
kGraph_color <- c("#66ccff", "#0041ff", "#35a16b", "#faf500", "#ff9900", "#ff2800", "#ffd1d1",
                  "#9a0079", "#cbf266", "#191714", "#c8c8cb")
# グラフ総計の色を追加
kTotalGraph_color_add <- c("#FFFFFF")  # 白
kTotalGraph_color <- c(kGraph_color, kTotalGraph_color_add)

# その他病名は11：明るいグレーに集約
kOther_disease_start <- length(kGraph_color)
# 病名TOP10
kOutput_disease_count <- kOther_disease_start - 1

# データフレーム列名定義
kSum_colname <- "Sum"  # addmargins関数の合計行列名
kDisease_colname <- "diseasename"  # 病名
kGeneration_colname <- "generation"  # 世代
kCount_colname <- "diseasecount"  # 病名件数
kPercentage_colname <- "percentage"  # ソートキー：パーセンテージ
kLabel_colname <- "piechartlabel"
kOthers_colname <- "Others"  # その他病名
# パイチャート出力作業用データフレーム列名：世代（病名毎の件数を格納）、病名
kPiechart_dst_colname <- c(kGenerationList, kDisease_colname)
# 病名TOP10抽出作業用データフレーム列名：病名、世代、病名毎の件数
kSortall_colname <- c(kDisease_colname, kGeneration_colname, kCount_colname)

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
# output_path <- paste(basepath, output_folder_name, output_extension, sep="/")
output_path <- "C:/Users/MarikoOhtsuka/Desktop/plot/test"

# READ SAS analysis data set (ADS)
sasdat <- read.sas7bdat(sasdat_path)

# ******************
# * TOTAL Piechart *
# ******************
# 世代・疾患分類毎の件数合計
sum_table <- addmargins(table(sasdat$MHGRPTERM, sasdat$AGECAT2N))
# 世代名、病名のデータフレームを作成
sum_table <- cbind(sum_table, rownames(sum_table))
dst_total <- as.data.frame(sum_table, stringsAsFactors = F)
colnames(dst_total) <- kPiechart_dst_colname
# 世代合計"ALL"列の件数降順でソート
dst_total <- SortDisease(subset(dst_total, diseasename != kSum_colname), generation_total_colname)
# パイチャート設定色をセット
dst_total$graph_color <- kTotalGraph_color
# 各項目のパーセンテージラベル作成作業用列
dst_total[ ,kPercentage_colname] <- NA
dst_total[ ,kLabel_colname] <- NA

# 世代毎にパイチャートを出力
for (i in 1:generation_length) {
  output_filename <- paste0("total_", kOrder_OutputFile[i], ".", output_extension)
  output_filepath <- paste(output_path, output_filename, sep="/")
  # 各項目のパーセンテージ計算
  dst_total[ ,kPercentage_colname] <- CalculatePercentage(dst_total[ ,i])
  # 3%以上の場合のみラベルを出力する
  dst_total[ ,kLabel_colname] <- ifelse(dst_total[ ,kPercentage_colname] > 2, paste(dst_total[ ,kPercentage_colname], "%"), "")
  wk_pie_option <- c(0.8, 2.7)
  wk_pie <- cbind(as.numeric(dst_total[, i]), dst_total[ ,kLabel_colname], dst_total$graph_color)
  wk_doughnut <- c(0.5, 2.7, paste(colnames(dst_total[i]), "\nn =", sum(as.numeric(dst_total[ ,i])), "\n"))
  EditDoughnutChart(output_filepath, output_extension, c(0, 2.2, 0, 2.2), wk_pie_option, wk_pie, wk_doughnut)
}
# ダミーグラフ内にlegend出力
output_filename <- paste0("total_legend",  ".", output_extension)
output_filepath <- paste(output_path, output_filename, sep="/")
EditLegend(output_filepath, output_extension, dst_total$diseasename, 2.7, dst_total$graph_color)

# ********************
# * DISEASE Piechart *
# ********************
# 疾患分類リスト作成
mhgrpterm_lst <- levels(factor(sasdat$MHGRPTERM))
for (i in 1:length(mhgrpterm_lst)) {
  # 疾患群ごとにデータ分け
  wk_sasdat <- subset(sasdat, MHGRPTERM == mhgrpterm_lst[i])

  # 世代毎合計を集計
  sum_table <- addmargins(table(wk_sasdat$MHTERM, wk_sasdat$AGECAT2N))
  # 件数有のみ対象とし、合計行は削除する
  sum_table <- subset(sum_table, sum_table[ ,kSum_colname] > 0)
  sum_table <- sum_table[1:(nrow(sum_table) - 1), ]
  # 合計削除
  # TOP10集計のため退避
  wk2_sum_lst <- sum_table
  # 世代名、病名のデータフレーム作成
  sum_table <- cbind(sum_table, rownames(sum_table))
  dst_gene_mhterm <- as.data.frame(sum_table, stringsAsFactors=F)
  colnames(dst_gene_mhterm) <- kPiechart_dst_colname

  # 世代ALLの詳細病名件数の降順にソートし、色を決定
  # 上位10と残りの疾患合計の11に分ける
  colnames(wk2_sum_lst) <- kGenerationList
  dst_sortall <- data.frame(wk2_sum_lst, stringsAsFactors=F)
  colnames(dst_sortall) <- kSortall_colname
  # factor -> character
  dst_sortall$generation <- as.character(dst_sortall$generation)
  dst_sortall$per <- NA
  # 世代毎に病名数からパーセンテージを算出
  for (j in 1:generation_length){
    dst_sortall[(dst_sortall$generation == kGenerationList[j]),kPercentage_colname] <- CalculatePercentage(dst_sortall[(dst_sortall$generation == kGenerationList[j]), "diseasecount"])
  }
  # パーセンテージで降順にソート
  dst_sortall <- SortDisease(dst_sortall,kPercentage_colname)
  # 重複病名を除去 fuctor -> characterに変換
  dis_disease <- as.character(unique(dst_sortall$diseasename))
  # 病名集計作業用データフレーム
  dst_piechart <- data.frame(matrix(rep(NA, ncol(dst_gene_mhterm)), nrow=1))[numeric(0), ]
  colnames(dst_piechart) <- colnames(dst_gene_mhterm)
  dst_others_piechart <- dst_piechart

  if (length(dis_disease) > kOutput_disease_count){
    # 病名が10件以上の場合、上位10件＋その他に集約する
    top10_disease <- dis_disease[1:kOutput_disease_count]
    other_disease <- dis_disease[kOther_disease_start:length(dis_disease)]
    for (p in 1:length(top10_disease)){
      # 該当病名行を抽出して格納
      dst_piechart[p, ] <- subset(dst_gene_mhterm, dst_gene_mhterm$diseasename == top10_disease[p])
    }
    for (p in 1:length(other_disease)){
      # 該当病名行を抽出して格納
      dst_others_piechart[p, ] <- subset(dst_gene_mhterm, dst_gene_mhterm$diseasename == other_disease[p])
    }
    # 世代合計の件数で再ソート
    # Othersは最終行に
    dst_piechart <- SortDisease(dst_piechart, generation_total_colname)
    # factor -> numeric
    dst_others_piechart[ 1:generation_length] <- lapply(dst_others_piechart[ ,1:generation_length], as.numeric)
    dst_piechart[kOther_disease_start, ] <- c(apply(dst_others_piechart[ ,1:generation_length], 2, sum), kOthers_colname)
  } else {
    # 10件以下ならそのまま格納
    dst_piechart <- SortDisease(dst_gene_mhterm, generation_total_colname)
  }

  # パイチャート設定色をセット
  dst_piechart$graph_color <- kGraph_color[1:nrow(dst_piechart)]
  # 各項目のパーセンテージラベル作成作業用列
  dst_piechart$wk_per <- NA
  dst_piechart$wk_lbl <- NA
  # グラフ生成
  for (m in 1:generation_length) {
    # 件数0なら出力しない
    wk_sum <- sum(as.numeric((dst_piechart[ ,m])))
    if (wk_sum > 0) {
      output_filename <- paste0(gsub("[-/]", "", mhgrpterm_lst[i]), "_", kOrder_OutputFile[m], ".", output_extension)
      output_filepath <- paste(output_path, output_filename, sep="/")
      # 各項目のパーセンテージ計算
      dst_piechart$wk_per <- CalculatePercentage(as.numeric(dst_piechart[, m]))
      # 3%以上の場合のみラベルを出力する
      dst_piechart$wk_lbl <- ifelse(dst_piechart$wk_per > 2, paste(dst_piechart$wk_per, "%"), "")
      # ドーナッツグラフ出力
      wk_pie_option <- c(0.8, 2.7)
      wk_pie <- cbind(as.numeric(dst_piechart[, m]), dst_piechart$wk_lbl, dst_piechart$graph_color)
      wk_doughnut <- c(0.5, 2.7, paste(colnames(dst_piechart[m]), "\nn =", sum(as.numeric((dst_piechart[ ,m]))), "\n"))
      EditDoughnutChart(output_filepath, output_extension, c(0, 2.2, 0, 2.2), wk_pie_option, wk_pie, wk_doughnut)
    }
  }
  # ダミーグラフ内にlegend出力
  output_filename <- paste0(gsub("[-/]", "", mhgrpterm_lst[i]), "_legend",  ".", output_extension)
  output_filepath <- paste(output_path, output_filename, sep="/")
  EditLegend(output_filepath, output_extension, dst_piechart$diseasename, 2, dst_piechart$graph_color)
}

