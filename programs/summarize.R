# 作成者: Sayuri Yamashita
# 作成日: 2021.09.10
# 対応フロー: JSHregistry集計用データ作成プログラム仕様書_20211013.xlsx
# コピー元: none
# 変更履歴: none
#######################################################################

library(tidyverse)

# 最下行に合計値を追加する（表を分割することによりgtのsummary_rowsが使えない表が対象）関数
# col_no：numeric 合計値を出す最初の列（ラベル列が1列であれば2、ラベル列が2列であれば3）
AddTotal <- function(df, col_no = 2){
    col_no <- as.numeric(col_no)
    total <- df %>% summarize(across(c(col_no:ncol(.)), sum))
    df <- bind_rows(df, total)
    df[is.na(df)] <- "合計"
    if (col_no == 3){
      df[nrow(df),1] <- ""
    } # とりあえずcol_noが2か3しか想定していない
    return(df)
}

RegistSum <- function(df, site_master){
    # 団体別の登録者数の算出
    study_summary <- df %>% group_by(STUDYID) %>% summarize(登録数 = n())
    # 団体・施設別の登録者数の算出
    registered_study_site <- df %>% group_by(STUDYID, SITEID) %>% summarize(siteN = n())
    study_site_summary <- site_master %>% left_join(registered_study_site, by = c("STUDYID"="STUDYID", "SITEID"="SITEID")) %>%
        mutate(is_registered = ifelse(!is.na(siteN), "登録あり", "登録なし")) %>% group_by(STUDYID, is_registered) %>%
        count() %>% group_by(STUDYID) %>% mutate(合計 = sum(n)) %>% spread(is_registered, n) %>% left_join(study_summary) %>% ungroup
    result <-  study_site_summary %>% rename(団体名 = STUDYID)
    return(result)
}

SiteRegistSum <- function(df){
    site_summary <- df %>% group_by(SITEID) %>% summarize(登録数 = n()) %>% left_join(distinct(sites[,1:2])) %>% select(施設名 = SITENAME, 登録数)
    result <- AddTotal(site_summary, 2)
    return(result)
}

DiseaseMajorSum <- function(df, dis_code){
    df <- df %>% group_by(group_code, group_ja, cat.age.diagnosis)
    result <- df %>% summarize(n = n()) %>% filter(group_code %in% dis_code) %>% mutate(合計 = sum(n)) %>%
      spread(cat.age.diagnosis, n) %>% rename_with( ~ paste0(., "歳"), matches("\\d-\\d")) %>%
      rename(診断名 = group_ja, `40歳以上` = `40-`) %>% ungroup %>% select(-group_code) %>%
      mutate(tmp = 合計) %>% select(-合計) %>% rename(合計 = tmp)　        #合計列を末尾に移動
    result[is.na(result)] <- 0
    return(result)
}

DiseaseMinorSum <- function(df, dis_code){
    # 中分類の分類不能型骨髄異形成症候群のうちMHDECODで分類される詳細名を括弧で追記したラベル
    label <- df %>% filter(grepl("1028[1-3]", MHDECOD)) %>% select(MHDECOD, MHTERM, abbr) %>% distinct() %>%
      mutate(MHTERM = paste0(MHTERM, " (", abbr, ")")) %>% pull(MHTERM)

    df <- df %>% group_by(group_code, group_ja, MHTERM, MHDECOD, cat.age.diagnosis)
    result <- df %>% summarize(n = n()) %>% filter(group_code %in% dis_code) %>% mutate(合計 = sum(n)) %>%
      spread(cat.age.diagnosis, n) %>% rename_with( ~ paste0(., "歳"), matches("\\d-\\d")) %>%
      rename(診断名 = group_ja, `40歳以上` = `40-`) %>% ungroup %>% select(-group_code, -MHDECOD) %>%
      mutate(tmp = 合計) %>% select(-合計) %>% rename(合計 = tmp)　        #合計列を末尾に移動
    result[is.na(result)] <- 0

    #ラベルを置き換えるステップ
    if (length(result[result$MHTERM == "分類不能型骨髄異形成症候群",]$MHTERM) != 0) {
      result[result$MHTERM == "分類不能型骨髄異形成症候群",]$MHTERM <- label
    }

    result <- AddTotal(result, 3)
    result[nrow(result),1] <- result[nrow(result)-1,1] #「合計」の左隣の列には一行上の値を代入する(gtフォーマット用)
    return(result)
}


# マスターに従って疾患詳細の集計対象を絞り込む関数
DfFiltering <- function(df, master){
    field_name <- rlang::sym(master$field)
    target_name <- rlang::sym(master$target)
    if (master$field == "-" || master$val == "-") {
        df <- df %>% filter(!(!!target_name) %in% c("取得なし", NA))
    } else if (master$val == "who2016.1005") {
        df <- df %>% filter(!!field_name == master$val) %>% filter(STUDYID == "NHOH")
    } else if (str_detect(master$val, ";")) {
        vals <- as.vector(str_split(master$val, ";", simplify = T))
        df <- df %>% filter(!!field_name %in% vals)
    } else {
        df <- df %>% filter(!!field_name == master$val)
    }
    return(df)
}

# マスターのcuration列に従って疾患詳細の項目名を修正し、該当項目を合算する関数
DfCurating <- function(df, master){
    if (!is.na(master$curation)) {
        replace_words <- as.vector(str_split(master$curation, ",", simplify = T))
        for(word in replace_words) {
            bf_af <- as.vector(str_split(word, ">", simplify = T))
            df[,master$target] <- gsub(bf_af[1], bf_af[2], df[,master$target])
        }
    } else if (master$target == "HL.Stage.Ann.Arbor") {
        idx <- grep("^I+$|^IV$", df[,master$target])
        df[,master$target][idx] <- paste0(df[,master$target][idx], "期")
    }
    df[,master$target] <- gsub("取得なし", NA, df[,master$target])
    return(df)
}

# すべての年代別カラムが表示されるように調整する関数
FormatDetailSum <- function(result){
    form <- data.frame(matrix(rep(0, 7), nrow=1))[numeric(0),]
    form$X1 <- as.character(form$X1)
    colnames(form) <- c(names(result)[1], "0-14歳", "15-19歳", "20-29歳", "30-39歳", "40歳以上", "合計")
    result <- bind_rows(form, result)
    result[is.na(result)] <- 0
    return(result)
}

DetailSum <- function(df, master){
    item <- rlang::sym(master$target)
    result <- df %>% select(!!item, cat.age.diagnosis) %>% group_by(!!item, cat.age.diagnosis) %>% summarize(n = n()) %>% mutate(合計 = sum(n)) %>%
        spread(cat.age.diagnosis, n)  %>% rename_with( ~ paste0(., "歳"), matches("\\d-\\d")) %>% rename(`40歳以上` = `40-`) %>% ungroup
    result[is.na(result[,1]),1] <- "未入力または未取得"
    result[is.na(result)] <- 0
    #result <- AddTotal(result, 2)
    result <- FormatDetailSum(result)
    return(result)
}

#######################################################################

# 入力データ、マスターの明示
input_ads <- "./input/JSH_NHOH_JSPHO_ads.csv"
sites_master <- "./input/SITEID.txt"
detail_diag_master <- "./input/detail_master.txt"
report_form <- "./input/report_form.txt"

# 結果を格納するためのlistを用意
registration <- list()
disease <- list()
disease_detail <- list()

# ADSとマスターのインポート実行
df <- read.delim(input_ads, sep=",", header=T, na.strings = "", fileEncoding = "Shift-JIS")
sites <- read.delim(sites_master, sep="\t", header=T,  na.strings = "", fileEncoding="UTF-8")
colnames(sites) <- c("SITEID", "SITENAME", "STUDYID")
detail_diag <- read.delim(detail_diag_master, sep="\t", header=T, na.strings = "", fileEncoding = "UTF-8")
report <- read.delim(report_form, sep="\t", header=T, na.strings = "", fileEncoding = "UTF-8")

#######################################################################

# 団体別かつ登録者有無別の施設数の集計
registration[["団体別施設登録数及び登録数"]] <- RegistSum(df, sites)

# 団体別の登録者数（登録者ありの施設に限る）の集計
registration[["施設別登録数"]] <- SiteRegistSum(df)

# 血液疾患（大分類・中分類）の集計
neo_group_code <- c(sprintf("who2016.10%02d", 1:12), sprintf("who2016.1007.%01d", 1:3), "who2016.1001.1", "who2016.1005.1")
neo_group_code <- neo_group_code[order(neo_group_code)]
nonN_group_code <- sprintf("who2016.11%02d", 1:14)

disease[["腫瘍性疾患・大分類"]] <- DiseaseMajorSum(df, neo_group_code)
disease[["非腫瘍性疾患・大分類"]] <- DiseaseMajorSum(df, nonN_group_code)
disease[["腫瘍性疾患・中分類"]] <- DiseaseMinorSum(df, neo_group_code)
disease[["非腫瘍性疾患・中分類"]] <- DiseaseMinorSum(df, nonN_group_code)

# 腫瘍性疾患・非腫瘍性疾患の詳細集計
errorOut <- "./output/【要確認】疾患詳細項目.txt"
cat(paste0(
  "下記の項目はreport_form.txtに含まれません。\n文字列を補正あるいは新規項目として追加の必要があります。\n\n"), file = errorOut)

disease_detail <- lapply(1:nrow(detail_diag), function(i){

    # マスターで一つ前の集計対象に絞り込んだ上で、さらに詳細を集計する場合の処理
    if (detail_diag$field[i] != "MHDECOD" && detail_diag$field[i] != "group_code" && detail_diag$field[i] != "-") {
        df <- DfFiltering(df, detail_diag[i-1,])
    }

    # デフォルトの疾患詳細の集計処理default totaling for detail
    df <- DfFiltering(df, detail_diag[i,])
    df <- DfCurating(df, detail_diag[i,])
    df_R <- DetailSum(df, detail_diag[i,])

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
        result <- df_L %>% left_join(df_R, by = c("id"="id", "detail"="detail")) %>% select(detail, everything()) %>% 
          select(-id)
        result[is.na(result)] <- 0
    }
    return(result)
})

report_title <- report %>% select(id, category, disease) %>% distinct()
names(disease_detail) <- report_title$disease

#######################################################################
