#######################################################################
# 作成者: Sayuri Yamashita
# 作成日: 2021.09.10
# 対応フロー: JSHregistry集計用データ作成プログラム仕様書_20211013.xlsx
# コピー元: none
# 変更履歴: 2022.08.18 Agata.K 2022年度集計に伴う、プログラム修正とコメント追加
#           2023.08.09 Agata.K 2023年度集計：パスを手動設定に変更、outputのディレクトリを無ければ生成するよう修正
#######################################################################

# 手動で設定する事項の定義
######################################################################################
# inputフォルダを保管しているフォルダのパス
prtpath <- "C:/Users/c0002392/work/GIT/JSH/work/summarize"
######################################################################################


# libraryの読込が失敗する場合、インストールが出来ていない可能性あり、以下コマンドをconsole上ですれば解決する
# Rのバージョンアップ後は毎回必要な模様
# install.packages("tidyverse", dependencies=TRUE)
library(tidyverse)

#--------------------------------
# 関数：AddTotal
# 内容：最下行に合計値を追加する
# 引数：df()
# 　　　col_no(numeric合計値を出す最初の列番号、指定なければ2)
# 返値：df
# 履歴：2022.08.18 Agata.K 修正
# 備考：表を分割することによりgtのsummary_rowsが使えない表が対象
#--------------------------------
AddTotal <- function(df, col_no = 2){
  
  col_no <- as.numeric(col_no)
  
  # col_no-最終列までの登録数の合計を算出
  total <- df %>% summarize(across(c(col_no:ncol(.)), sum))
  
  # 最下行に結合
  df <- bind_rows(df, total)
  
  # 行項目名に合計を入れる（Nullに合計を入れるとしていたが修正）
  # 最下行番号を取得し入れる(グラフ作成関係上、col_no=3は2列最下行に合計を入れたい)
  # df[is.na(df)] <- "合計"
  df[nrow(df), col_no-1] <- "合計"
  
  # 上記処理でNAに合計を入れているので、1～2列目が項目名の場合両方に合計が入るため、最終行1列のみNullにする処理？
  # 処理修正に伴い不要になったと思われる
  # if (col_no == 3){
  #   df[nrow(df),1] <- ""
  # } # とりあえずcol_noが2か3しか想定していない
  
  return(df)
}

#--------------------------------
# 関数：RegistSum
# 内容：団体別かつ登録者有無別の施設数の集計
# 引数：df（Ptoshデータ）
# 　　　site_master（SITEID.txt）
# 返値：result
# 履歴：
# 備考：団体別施設登録数及び登録数
#--------------------------------
RegistSum <- function(df, site_master){
  
  # 団体別の登録者数の算出（STUDYIDでdfをグループ化、グループ単位で登録数を集計）
  study_summary <- df %>% group_by(STUDYID) %>% summarize(登録数 = n())
  
  # 団体・施設別の登録者数の算出（STUDYIDと医療機関コードでdfをグループ化、グループ単位で登録数を集計）
  registered_study_site <- df %>% group_by(STUDYID, SITEID) %>% summarize(siteN = n())
  
  # 団体別施設登録数及び登録数
  #  SITEIDとregistered_study_siteを結合（key:STUDYID,SITEID）
  #  is_registered列を追加(登録あり／登録なし)
  #  グループ化（STUDYID,is_registered）
  #  登録あり／登録なし施設数をcount
  #  グループ化（STUDYID)
  #  登録あり/登録なしの施設数合計を算出
  #  表の形を整えて,study_summaryを結合する
  study_site_summary <- site_master %>% left_join(registered_study_site, by = c("STUDYID"="STUDYID", "SITEID"="SITEID")) %>%
    mutate(is_registered = ifelse(!is.na(siteN), "登録あり", "登録なし")) %>% group_by(STUDYID, is_registered) %>%
    count() %>% group_by(STUDYID) %>% mutate(合計 = sum(n)) %>% spread(is_registered, n) %>% left_join(study_summary) %>% ungroup
  
  # 項目名のrename
  result <-  study_site_summary %>% rename(団体名 = STUDYID)
  
  return(result)
  
}

#--------------------------------
# 関数：SiteRegistSum
# 内容：施設別登録数（症例登録あり施設のみ）
# 引数：df（Ptoshデータ）
# 返値：result
# 履歴：
# 備考：
#--------------------------------
SiteRegistSum <- function(df){
  
  # 施設別登録数
  #  dfをグループ化(SITEID),登録数を算出
  #  施設名日本語を結合(key:SITEID)、施設名と登録数のみ抜粋
  site_summary <- df %>% group_by(SITEID) %>% summarize(登録数 = n()) %>% left_join(distinct(sites[,1:2])) %>% select(施設名 = SITENAME, 登録数)
  
  # 最下行に登録数合計を算出して入れる
  result <- AddTotal(site_summary, 2)
  
  return(result)
  
}

#--------------------------------
# 関数：DiseaseMajorSum
# 内容：腫瘍性疾患／非腫瘍性疾患の大分類の集計
# 引数：df（Ptoshデータ）
#       cat(腫瘍性／非腫瘍性)
# 返値：result
# 履歴：2022.07.28 Agata.K 修正
# 備考：
#--------------------------------
DiseaseMajorSum <- function(df, cat){
  
  # 疾患のマスタデータをベースに集計するように改修（登録数0の疾患も0として結果を出すため）(2022.07.28 Agata.K)
  
  # 腫瘍性か非腫瘍性かで使うテーブルを変更する(2022.07.28 Agata.K)
  ifelse(cat == 1, dis_code <- neo_group_code, dis_code <- nonN_group_code)
  ifelse(cat == 1, dg_master <- diseases_gr[diseases_gr$category == 1,], dg_master <- diseases_gr[diseases_gr$category == 2,])
  
  # dfをグループ化(key:group_code, group_ja, cat.age.diagnosis)
  tmp <- df %>% group_by(group_code, group_ja, cat.age.diagnosis)
  
  # 修正(2022.07.28 Agata.K)
  # result <- df %>% summarize(n = n()) %>% filter(group_code %in% dis_code) %>% mutate(合計 = sum(n)) %>%
  #     spread(cat.age.diagnosis, n) %>% rename_with( ~ paste0(., "歳"), matches("\\d-\\d")) %>%
  #     rename(診断名 = group_ja, `40歳以上` = `40-`) %>% ungroup %>% select(-group_code) %>%
  #     mutate(tmp = 合計) %>% select(-合計) %>% rename(合計 = tmp)　        #合計列を末尾に移動
  
  # 大分類の集計(2022.07.28 Agata.K)
  #  グループ毎の登録数を算出
  #  dis_codeと大分類コードが一致するもののみ残す
  #  大分類毎の合計を算出する
  #  合計の列を一番後ろに移動する
  tmp <- tmp %>% summarize(n = n()) %>% filter(group_code %in% dis_code) %>% mutate(合計 = sum(n)) %>%
    spread(cat.age.diagnosis, n) %>% ungroup %>% mutate(tmp = 合計) %>% select(-合計) %>% rename(合計 = tmp)
  
  # マスタとresultをマージ（key：code,group_code）(2022.07.28 Agata.K)
  tmp <- merge(dg_master, tmp, by.x = "code", by.y = "group_code", all.x = T)
  
  # 不要な項目を削除する(2022.07.28 Agata.K)
  tmp <- tmp %>% select(-c(code, category, abbr, group_ja))
  
  # 項目名変更(2022.07.28 Agata.K)
  result <- tmp %>% rename_with( ~ paste0(., "歳"), matches("\\d-\\d")) %>% rename(診断名 = name_ja, `40歳以上` = `40-`)
  
  # 集計結果でNAの箇所は0にする
  result[is.na(result)] <- 0
  
  return(result)
  
}

#--------------------------------
# 関数：DiseaseMinorSum
# 内容：腫瘍性疾患／非腫瘍性疾患の中分類の集計
# 引数：df（Ptoshデータ）
#       cat(腫瘍性／非腫瘍性)
# 返値：
# 履歴：2022.07.28 Agata.K 修正
# 備考：
#--------------------------------
DiseaseMinorSum <- function(df, cat){
  
  # 疾患のマスタデータをベースに集計するように改修（登録数0の疾患も0として結果を出すため）(2022.07.28 Agata.K)
  
  # 削除(2022.07.28 Agata.K)
  # 中分類の分類不能型骨髄異形成症候群のうちMHDECODで分類される詳細名を括弧で追記したラベル
  # 　label <- df %>% filter(grepl("1028[1-3]", MHDECOD)) %>% select(MHDECOD, MHTERM, abbr) %>% distinct() %>%
  #     mutate(MHTERM = paste0(MHTERM, " (", abbr, ")")) %>% pull(MHTERM)
  
  # 腫瘍性か非腫瘍性かでテーブルを変更する(2022.07.28 Agata.K)
  ifelse(cat == 1, dis_code <- neo_group_code, dis_code <- nonN_group_code)
  
  # df <- df %>% group_by(group_code, group_ja, MHTERM, MHDECOD, cat.age.diagnosis)
  # dfをグループ化(key:group_code, group_ja, cat.age.diagnosis)
  tmp <- df %>% group_by(group_code, MHDECOD, group_ja, MHTERM, cat.age.diagnosis)
  
  # 修正(2022.07.28 Agata.K)
  # result <- df %>% summarize(n = n()) %>% filter(group_code %in% dis_code) %>% mutate(合計 = sum(n)) %>%
  #   spread(cat.age.diagnosis, n) %>% rename_with( ~ paste0(., "歳"), matches("\\d-\\d")) %>%
  #   rename(診断名 = group_ja, `40歳以上` = `40-`) %>% ungroup %>% select(-group_code, -MHDECOD) %>%
  #   mutate(tmp = 合計) %>% select(-合計) %>% rename(合計 = tmp)　        #合計列を末尾に移動
  
  # 集計方法を修正(2022.07.28 Agata.K) -----------------#
  # 大分類の集計
  #  グループ毎の登録数を算出
  #  dis_codeと大分類コードが一致するもののみ残す
  #  大分類毎の合計を算出する
  #  合計の列を一番後ろに移動する
  tmp <- tmp %>% summarize(n = n()) %>% filter(group_code %in% dis_code) %>% mutate(合計 = sum(n)) %>%
    spread(cat.age.diagnosis, n) %>% ungroup %>% mutate(tmp = 合計) %>% select(-合計) %>% rename(合計 = tmp)
  
  # 疾患グループのマスタ情報を取得
  ifelse(cat == 1, dg_master <- diseases[diseases$category1 == 1,], dg_master <- diseases[diseases$category1 == 2,])
  
  # マスタとresultをマージ（key：code,group_code)
  tmp <- merge(dg_master, tmp, by.x = "code", by.y = "MHDECOD", all.x = T )
  tmp <- tmp[order(tmp$group_code.x),]
  
  # group_jaについてNullがあれば埋める、codeが同じでgroup_jaが異なる場合はマスタに合わせる
  for (i in 1: length(tmp$code)){
    
    # group_codeが一致すれば、その日本語名を取得する
    for (j in 1: length(diseases_gr$code)){
      if(tmp$group_code.x[i] == diseases_gr$code[j]){  # 一致したらgr_jaへ退避
        gr_ja <- diseases_gr$name_ja[j]
        break
      }
    }
    
    # group_jaがNullがなら、埋める
    if(is.na(tmp$group_ja[i]) == TRUE){
      tmp$group_ja[i] <- diseases_gr$name_ja[j]
      # group_jaが異なるなら、修正する
    }else if(tmp$group_ja[i] != gr_ja){
      tmp$group_ja[i] <- gr_ja
    }
  }
  
  # 不要な項目を削除する
  tmp <- tmp %>% select(-c(code, category1, category2, group_code.x, abbr, group_code.y, MHTERM))
  
  # 列順変更
  tmp <- tmp[,c(2,1,3,4,5,6,7,8)]
  
  # 項目名変更
  result <- tmp %>% rename_with( ~ paste0(., "歳"), matches("\\d-\\d")) %>% rename(`疾患名・中分類` = `group_ja`, 診断名 = name_ja, `40歳以上` = `40-`)
  
  #----------------------------------------------------#
  
  # NAの箇所は0にする
  result[is.na(result)] <- 0
  
  # 最下行に登録数合計を算出して入れる
  result <- AddTotal(result, 3)
  
  #「合計」の左隣の列には一行上の値を代入する(gtフォーマット用)
  result[nrow(result),1] <- result[nrow(result)-1,1]
  
  return(result)
  
}


#--------------------------------
# 関数：DfFiltering
# 内容：detail_masterに従い、疾患詳細の集計対象を絞り込む
# 引数：df（Ptoshデータ）
#       master(detail_master)
# 返値：df（抽出結果）
# 履歴：
# 備考：
#--------------------------------
DfFiltering <- function(df, master){
  
  # 項目を取得
  field_name <- rlang::sym(master$field)
  # 対象を取得
  target_name <- rlang::sym(master$target)
  
  # fieldとvalのいずれかが「－」なら、取得なしとNA以外の行を抽出する
  if (master$field == "-" || master$val == "-") {
    df <- df %>% filter(!(!!target_name) %in% c("取得なし", NA))
    
    # valが「急性骨髄性白血病および関連腫瘍」なら、who2016.1005かつNHOHの行を抽出する
  } else if (master$val == "who2016.1005") {
    df <- df %>% filter(!!field_name == master$val) %>% filter(STUDYID == "NHOH")
    
    # valに「；」を含むなら、;を区切りとしてデータを切りvalsへ格納、一致する行を抽出する
  } else if (str_detect(master$val, ";")) {
    vals <- as.vector(str_split(master$val, ";", simplify = T))
    df <- df %>% filter(!!field_name %in% vals)
    
    # 上記以外はfield_nameの項目が、valと一致する行を抽出する
  } else {
    df <- df %>% filter(!!field_name == master$val)
  }
  
  return(df)
  
}

#--------------------------------
# 関数：DfCurating
# 内容：マスターのcuration列に従って、targetの項目について情報を置換する（選択肢の統一）
# 引数：df（DfFilteringで抽出したデータ）
#       master(detail_master)
# 返値：df
# 履歴：
# 備考：
#--------------------------------
DfCurating <- function(df, master){
  
  # curationがNullでないなら、curationの情報で置換する
  if (!is.na(master$curation)) {
    replace_words <- as.vector(str_split(master$curation, ",", simplify = T))　# curationをカンマで分ける
    
    for(word in replace_words) {
      bf_af <- as.vector(str_split(word, ">", simplify = T))　　　　　　　   # curationを＞で分ける
      df[,master$target] <- gsub(bf_af[1], bf_af[2], df[,master$target])   　# curationの＞の左辺を右辺に置換する
    }
    
    # curationが「HL.Stage.Ann.Arbor」なら、「期」が入っていないものは統一
  } else if (master$target == "HL.Stage.Ann.Arbor") {
    idx <- grep("^I+$|^IV$", df[,master$target])　　　　　　　　　　　# 期が入っていない行番号を抽出
    df[,master$target][idx] <- paste0(df[,master$target][idx], "期")  # 取得した行番号について期を入れる
  }
  
  # curationがNullでないなら、取得なしをNAへ置換する
  df[,master$target] <- gsub("取得なし", NA, df[,master$target])
  
  return(df)
  
}

#--------------------------------
# 関数：FormatDetailSum
# 内容：すべての年代別カラムが表示されるように調整する
# 引数：result(DetailSumの集計結果)
# 返値：result
# 履歴：
# 備考：
#--------------------------------
FormatDetailSum <- function(result){
  
  # formというdataframeを定義する
  form <- data.frame(matrix(rep(0, 7), nrow=1))[numeric(0),]
  # 1列目は文字列型
  form$X1 <- as.character(form$X1)
  # 1行目の項目名を設定
  colnames(form) <- c(names(result)[1], "0-14歳", "15-19歳", "20-29歳", "30-39歳", "40歳以上", "合計")
  # resultをformへバインド
  result <- bind_rows(form, result)
  # NAは0にする
  result[is.na(result)] <- 0
  
  return(result)
  
}

#--------------------------------
# 関数：DetailSum
# 内容：
# 引数：df（DfFilteringで抽出、DfCuratingで置換したデータ）
#       master(detail_master)
# 返値：
# 履歴：
# 備考：
#--------------------------------
DetailSum <- function(df, master){
  
  # 詳細集計する項目名を取得
  item <- rlang::sym(master$target)
  
  # dfを年齢別で集計する
  result <- df %>% select(!!item, cat.age.diagnosis) %>% group_by(!!item, cat.age.diagnosis) %>% summarize(n = n()) %>% mutate(合計 = sum(n)) %>%
    spread(cat.age.diagnosis, n)  %>% rename_with( ~ paste0(., "歳"), matches("\\d-\\d")) %>% rename(`40歳以上` = `40-`) %>% ungroup
  
  # NAの項目名を修正
  result[is.na(result[,1]),1] <- "未入力または未取得"
  
  # NAの集計結果を0で修正
  result[is.na(result)] <- 0
  
  # 表の整え
  #result <- AddTotal(result, 2)
  result <- FormatDetailSum(result)
  
  return(result)
  
}

# ここからメインの処理-----------
# 履歴：2022.08.18 Agata.K 修正
#       2023.08.09 Agata.K 修正
#--------------------------------

# 入力データ、マスターの明示（2023.08.09 Agata.K 追加）
input_ads <- paste0(prtpath, "/input/JSH_JSPHO_ads.csv")     # Ptoshデータ
sites_master <- paste0(prtpath, "/input/SITEID.txt")              # 参加施設一覧（3団体がっしゃんこした物）
detail_diag_master <- paste0(prtpath, "/input/detail_master.txt") # 疾患詳細集計用のマスタテーブル
report_form <- paste0(prtpath, "/input/report_form.txt")          # 出力様式定義用の設定ファイル
# 疾患マスタテーブル（2022.07 Agata.K 2022年集計に伴い２ファイル追加）
# Box\Datacenter\Trials\JSH\Registry\WHO分類コードを編集してinputdataを準備
diseases_master <- paste0(prtpath, "/input/Disease.txt")
diseasesgr_master <- paste0(prtpath, "/input/DiseaseGroup.txt")

# 出力先のoutputディレクトリが無ければ、生成する（2023.08.09 Agata.K 追加）
output_dir <- paste0(prtpath, "/output")
if(file.exists(output_dir) == FALSE) dir.create(output_dir)

# input_ads <- "./input/JSH_NHOH_JSPHO_ads.csv"       # Ptoshデータ
# sites_master <- "./input/SITEID.txt"                # 参加施設一覧（3団体がっしゃんこした物）
# detail_diag_master <- "./input/detail_master.txt"   # 疾患詳細集計用のマスタテーブル
# report_form <- "./input/report_form.txt"            # 出力様式定義用の設定ファイル
# # 疾患マスタテーブル（2022.07 Agata.K 2022年集計に伴い２ファイル追加）
# # Box\Datacenter\Trials\JSH\Registry\WHO分類コードを編集してinputdataを準備
# diseases_master <- "./input/Disease.txt"
# diseasesgr_master <- "./input/DiseaseGroup.txt"

# 結果を格納するためのlistを用意
registration <- list()
disease <- list()
disease_detail <- list()

# 入力データ、マスターのインポート
df <- read.delim(input_ads, sep=",", header=T, na.strings = "", fileEncoding = "Shift-JIS")                 # Ptoshデータ
sites <- read.delim(sites_master, sep="\t", header=T,  na.strings = "", fileEncoding="UTF-8")               # 参加施設一覧（3団体がっしゃんこしたファイル）
colnames(sites) <- c("SITEID", "SITENAME", "STUDYID")                                                       # 参加施設一覧の項目名を変更
detail_diag <- read.delim(detail_diag_master, sep="\t", header=T, na.strings = "", fileEncoding = "UTF-8")  # 疾患詳細集計用のマスタテーブル
report <- read.delim(report_form, sep="\t", header=T, na.strings = "", fileEncoding = "UTF-8")              # 出力様式定義用の設定ファイル
diseases <- read.delim(diseases_master, sep="\t", header=T, na.strings = "", fileEncoding = "UTF-8")        # 疾患マスタテーブル（2022.07 Agata.K 2022年集計に伴い追加）
diseases_gr <- read.delim(diseasesgr_master, sep="\t", header=T, na.strings = "", fileEncoding = "UTF-8")   # 疾患マスタテーブル（2022.07 Agata.K 2022年集計に伴い追加）

# 団体別施設登録数及び登録数
registration[["団体別施設登録数及び登録数"]] <- RegistSum(df, sites)

# 施設別登録数
registration[["施設別登録数"]] <- SiteRegistSum(df)

# 血液疾患（大分類・中分類）のグループコードを定義
# 腫瘍性
neo_group_code <- c(sprintf("who2016.10%02d", 1:12), sprintf("who2016.1007.%01d", 1:3), "who2016.1001.1", "who2016.1005.1")
neo_group_code <- neo_group_code[order(neo_group_code)]
# 非腫瘍性
nonN_group_code <- sprintf("who2016.11%02d", 1:14)

# 疾患集計(2022.07 Agata.K 関数の引数変更。1:腫瘍性／2:非腫瘍性)
disease[["腫瘍性疾患・大分類"]] <- DiseaseMajorSum(df, 1)
disease[["非腫瘍性疾患・大分類"]] <- DiseaseMajorSum(df, 2)
disease[["腫瘍性疾患・中分類"]] <- DiseaseMinorSum(df, 1)
disease[["非腫瘍性疾患・中分類"]] <- DiseaseMinorSum(df, 2)

# 腫瘍性疾患・非腫瘍性疾患の詳細集計
# errorOut <- "./output/【要確認】疾患詳細項目.txt"（2023.08.09 Agata.K 修正）
errorOut <- paste0(output_dir, "/【要確認】疾患詳細項目.txt")
cat(paste0(
  "下記の項目はreport_form.txtに含まれません。\n文字列を補正あるいは新規項目として追加の必要があります。\n\n"), file = errorOut)

disease_detail <- lapply(1:nrow(detail_diag), function(i){
  
  # マスターで一つ前の集計対象に絞り込んだ上で、さらに詳細を集計する場合の処理
  if (detail_diag$field[i] != "MHDECOD" && detail_diag$field[i] != "group_code" && detail_diag$field[i] != "-") {
    # detail_diagのfieldが上記でない行は、1つ上の行に集計する対象疾患があるので、まずは絞り込む
    df <- DfFiltering(df, detail_diag[i-1,])
  }
  
  # デフォルトの疾患詳細の集計処理default totaling for detail
  df <- DfFiltering(df, detail_diag[i,])　# field-targetの情報で絞り込み
  df <- DfCurating(df, detail_diag[i,])   # targetの項目についてcurationの情報で置換
  df_R <- DetailSum(df, detail_diag[i,])  # 年代別カラムが表示されるように調整
  
  # 今年度の集計項目(df_R)にあってマスター(report_form.txt)にない疾患詳細項目があるか調べる
  colnames(df_R)[1] <- "detail"
  df_R$id <- i
  df_L <- report %>% select(id, detail) %>% filter(id == i)
  missing_name <- setdiff(df_R$detail, df_L$detail)
  
  # マスターにない項目を【要確認】ファイルに記載する
  if (length(missing_name) != 0) {
    missing <- df_R %>% filter(detail %in% missing_name) %>% select(id, detail)
    write.table(missing, file = errorOut, append = T, sep="\t", row.names = F, col.names = T, quote = F)
    return(NULL)
  } else {
    
    # summarize_outputでidが必要なので、ここでは削除しない（2022.7.28 Agata.K）
    # result <- df_L %>% left_join(df_R, by = c("id"="id", "detail"="detail")) %>% select(detail, everything()) %>%
    #   select(-id)
    result <- df_L %>% left_join(df_R, by = c("id"="id", "detail"="detail")) %>% select(detail, everything())
    result[is.na(result)] <- 0
  }
  return(result)
})

report_title <- report %>% select(id, category, disease) %>% distinct()
names(disease_detail) <- report_title$disease

#######################################################################
