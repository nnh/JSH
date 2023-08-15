#######################################################################
# 作成者: Sayuri Yamashita
# 作成日: 2021.10.13
# 対応フロー: JSHregistry集計用データ作成プログラム仕様書_20211013.xlsx
# コピー元: none
# 変更履歴: 2022.08.18 Agata.K 2022年度集計に伴う、プログラム修正とコメント追加
#           2023.08.09 Agata.K 2023年度集計：パスを手動設定に変更、outputのディレクトリを無ければ生成するよう修正
#######################################################################

# 手動で設定する事項の定義
######################################################################################
# プログラムを保管しているフォルダのパス
prgpath <- "C:/Users/KumikoAgata/Box/Datacenter/Users/agata/100_R関連/JSH/programs"
# inputフォルダを保管しているフォルダのパス
prtpath <- "C:/Users/KumikoAgata/Box/Datacenter/Trials/JSH/Registry/10.03.10 データレビュー書/2022年診断/集計/2_集計"
######################################################################################

source(paste0(prgpath, "/summarize.R"), local = F, encoding = "UTF-8")

# libraryの読込が失敗する場合、インストールが出来ていない可能性あり、以下コマンドをconsole上ですれば解決する
# Rのバージョンアップ後は毎回必要な模様
# install.packages("gt", dependencies=TRUE)
# 上記以外にPhantomJSのインストールも必要コマンドは以下
# webshot::install_phantomjs()
library(gt)

#--------------------------------
# 関数：OutputCSV
# 内容：リストに格納した結果をCSVで出力する関数
# 引数：title(メインタイトル)
# 　　　out_file(出力パス＆ファイル名)
#       list_data(出力するリストデータ)
# 返値：なし
# 履歴：
# 備考：
#--------------------------------
OutputCSV <- function(title, out_file, list_data){
    
    # CSVファイル出力（タイトル）
    cat(paste0(title, "\n"), file = out_file, append = T)
    
    # CSVファイル出力（サブタイトルとリストデータ）
    lapply(1:length(list_data), function(i){
        subtitle <- names(list_data[i])
        print(subtitle)
        cat(paste0("\n【", subtitle, "】\n"), file = out_file, append = T)
        write.table(list_data[[i]], file = out_file, append = T, sep=",", row.names = F, col.names = T, quote = T)
    })
}


#--------------------------------
# 関数：Common_style
# 内容：集計表の共通書式を設定する関数
# 引数：data()
# 返値：なし
# 履歴：
# 備考：
#--------------------------------
Common_style <- function(data){
    
    # 図で出力する集計表の設定
    data %>%
        tab_options(
            table.font.names = c("Yu Mincho","Times new Roman", default_fonts()),　
            heading.border.bottom.color = "#000000",
            table.border.top.color = "#000000",
            table.border.bottom.color = "#000000",
            table.font.size = px(14),
            heading.padding = px(3),
            heading.title.font.size = px(14),
            heading.border.bottom.width = px(1),
            column_labels.border.top.width = 0,
            column_labels.font.size = px(14),
            column_labels.border.bottom.width = px(1),
            column_labels.padding = px(1),
            row_group.border.bottom.width = px(1),
            row_group.padding = px(0.5),
            row_group.font.size = px(14),
            table_body.hlines.width = 0,
            stub.border.width = 0,
            data_row.padding = px(0),
            summary_row.border.width = 0,
            summary_row.padding = px(1),
            grand_summary_row.border.width = 0,
            grand_summary_row.padding = px(2),
            footnotes.marks = "standard",
            footnotes.font.size = px(12),
        )
}

#--------------------------------
# 関数：OutputTopTable
# 内容：団体別施設別登録数の図の生成
# 引数：df(団体別施設登録数及び登録数のリスト)
# 　　　main_title(タイトル)
# 返値：なし
# 履歴：2023.08.09 Agata.K 修正
# 備考：
#--------------------------------
OutputTopTable <- function(df, main_title){
    
    # 出力するテーブルの体裁を整え
    gt_table <- df %>%
        gt(rowname_col = "団体名") %>%
        grand_summary_rows(
            columns = c(-団体名),
            fns = list(合計 = "sum"),
            formatter = fmt_integer
        ) %>%
        tab_stubhead(label = "団体名") %>%
        tab_header(
            title = main_title
        ) %>%
        tab_spanner(
            label = "施設数",
            columns = c(合計, 登録あり, 登録なし)
        ) %>%
        # tab_footnote(
        #   footnote = "JSH、JSPHOおよびNHOの施設は重複を含む",
        #   locations = cells_column_spanners()
        # ) %>%
        Common_style() %>%
        tab_style(
            style=cell_text(
                weight = "bold",
                size = px(14),
            ),
            locations = list(
                cells_grand_summary(rows = 1),
                cells_stub_grand_summary(rows = 1)
            )
        ) %>%
        text_transform(
            locations = cells_stub(
                rows = everything()
            ),
            fn = function(x) {
                dplyr::case_when(
                    x == "JSH"   ~ "日本血液学会（JSH）",
                    x == "JSPHO"  ~ "日本小児血液・がん学会（JSPHO）",
                    x == "NHOH"  ~ "国立病院機構（NHO）")
            }
        ) %>%
        # (2023.08.09 Agata.K 修正)
        # gtsave(filename = paste0("./output/img/Table1.png"))　　# imageの出力
        gtsave(filename = paste0(prtpath,"/output/img/Table1.png"))　　# imageの出力
}

#--------------------------------
# 関数：OutputSiteTable
# 内容：施設登録数の図の生成
# 引数：df(施設別登録数のリスト)
# 　　　split_num(1ページあたりの行数)
# 返値：なし
# 履歴：2023.08.09 Agata.K 修正
# 備考：
#--------------------------------
OutputSiteTable <- function(df, split_num){
    
    # dfの行数を1ページあたりの行数で割る（forの回数を算出）
    times <- ceiling(nrow(df)/split_num)
    
    # ページ数分回す
    for (i in 1:times) {
        
        # データの先頭からsplit_num分の情報を取り出す
        tmp <- df %>% head(n = split_num)
        
        # 出力するテーブルの体裁を整える
        gt_table <- tmp %>%
            gt(rowname_col = "施設名") %>%
            tab_stubhead(label = "施設名") %>%
            Common_style()
        
        if(i == 1) {
            gt_table <- gt_table %>%
                tab_header(title = "施設別登録数（症例登録あり施設のみ）")
        } else if (i == times) {
            gt_table <- gt_table %>%
                tab_style(
                    style=cell_text(
                        weight = "bold",
                        size = px(14),
                        align = "right"
                    ),
                    locations = list(
                        cells_body(rows = nrow(tmp))
                    )
                )
        }
        
        # imageの出力（2023.08.09 Agata.K 修正）
        # gtsave(gt_table, filename = paste0("./output/img/Table2.", i, ".png"))
        gtsave(gt_table, filename = paste0(prtpath,"/output/img/Table2.", i, ".png"))
        df <- setdiff(df, tmp)
    }
}

#--------------------------------
# 関数：OutputDiseaseMajorTable
# 内容：大分類の図（ver：ファイル名の採番）
# 引数：df(大分類のリスト取得)
# 　　　main_title(タイトル)
#       ver(ファイル名の採番)
# 返値：なし
# 履歴：2023.08.09 Agata.K 修正
# 備考：
#--------------------------------
OutputDiseaseMajorTable <- function(df, main_title, ver){
    
    df %>%
        gt(rowname_col = "診断名") %>%
        tab_header(
            title = main_title
        ) %>%
        grand_summary_rows(
            columns = c(-診断名),
            fns = list(合計 = "sum"),
            formatter = fmt_integer
        ) %>%
        Common_style() %>%
        tab_style(
            style=cell_text(
                weight = "bold",
                size = px(14),
            ),
            locations = list(
                cells_grand_summary(rows = 1),
                cells_stub_grand_summary(rows = 1),
                cells_body(columns = 合計)
            )
        ) %>%
        text_transform(
            locations = cells_stub(
                rows = everything()
            ),
            fn = function(x) {
                gsub("\\?", "-", x)
            }
        ) %>%
        fmt_number(columns = 合計, decimals = 0) %>%
        # (2023.08.09 Agata.K 修正)
        # gtsave(filename = paste0("./output/img/Table3.", ver, ".png"))　# imageの出力
        gtsave(filename = paste0(prtpath, "/output/img/Table3.", ver, ".png"))　# imageの出力
}

#--------------------------------
# 関数：OutputDiseaseMinorTable
# 内容：中分類の図の生成
# 引数：df(中分類のリスト)
# 　　　main_title(タイトル)
#       split_num(1ページあたりの行数)
#       ver(ファイル名の採番)
# 返値：なし
# 履歴：2022.08.18 Agata.K 修正
#       2023.08.09 Agata.K 修正
# 備考：
#--------------------------------
OutputDiseaseMinorTable <- function(df, main_title, split_num, ver){
    
    # dfの行数を1ページあたりの行数で割る（forの回数を算出）
    times <- ceiling(nrow(df)/split_num)
    
    # \\?を－へ置換して、文字化け対応
    df$診断名 <- gsub("\\?", "-", df$診断名)
    
    # ページ数分回す
    for (i in 1:times) {
        
        # データの先頭からsplit_num分の情報を取り出す
        tmp <- df %>% head(n = split_num)
        
        # 存在しない項目名をrenameしようとしているので、修正（2022.7.28 Agata.K）
        # gt_table <- tmp %>%
        #   rename(`疾患名・中分類` = 診断名, 疾患名 = MHTERM) %>%
        #   group_by(`疾患名・中分類`) %>%
        #   gt(rowname_col = "疾患名") %>%
        #   Common_style() %>%
        #   text_transform(
        #     locations = cells_stub(
        #       rows = everything()
        #     ),
        #     fn = function(x) {
        #       gsub("\\?", "-", x)
        #     }
        #   )
        
        # 項目名の変更と表の整え、文字化け対応など
        gt_table <- tmp %>% rename(疾患名 = 診断名) %>% group_by(`疾患名・中分類`) %>% gt(rowname_col = "疾患名") %>% Common_style() %>%
            text_transform(locations = cells_stub(rows = everything()), fn = function(x) { gsub("\\?", "-", x)})
        
        # 1行目なら以下設定
        if(i == 1) {
            gt_table <- gt_table %>% tab_header(title = main_title)
            
            # 2行目以降なら以下設定
        } else if (i == times) {
            gt_table <- gt_table %>%
                tab_style(
                    style=cell_text(
                        weight = "bold",
                        size = px(14),
                        align = "right"
                    ),
                    locations = list(
                        cells_body(rows = nrow(tmp))
                    )
                )
        }
        
        # imageの出力(2023.08.09 Agata.K 修正)
        # gtsave(gt_table, filename = paste0("./output/img/Table4.", ver, ".", i, ".png"))
        gtsave(gt_table, filename = paste0(prtpath,"/output/img/Table4.", ver, ".", i, ".png"))
        df <- setdiff(df, tmp)
        
    }
}

#--------------------------------
# 関数：OutputDetailTable
# 内容：疾患詳細の図の生成
# 引数：df(詳細のリスト)
# 　　　report_title(タイトル)
#       split_num(1ページあたりの行数)
# 返値：なし
# 履歴：2023.08.09 Agata.K 修正
# 備考：
#--------------------------------
OutputDetailTable <- function(df, report_title, split_num){
    
    # dfの行数を1ページあたりの行数で割る（forの回数を算出）
    times <- ceiling(nrow(report_title)/split_num)
    
    # タイトルはNull
    main_title <- ""
    
    # ページ数分回す
    for (i in 1:times) {
        
        # データの先頭からsplit_num分の情報を取り出す
        tmp <- report_title %>% head(n = split_num)　
        
        # メインタイトルとカテゴリが違うなら、カテゴリをタイトルにする(1行目のみ通る)
        if (main_title != tmp$category[1]) {
            main_title <- tmp$category[1]
            k <- i
        }
        
        # 表の整え
        gt_table <- df %>%
            filter(disease %in% tmp$disease) %>%
            #select(-id, -category) %>%
            select(-category) %>%
            group_by(disease) %>%
            gt(rowname_col = "detail") %>%
            summary_rows(
                groups = TRUE,
                columns = c(-disease),
                fns = list(合計 = "sum"),
                formatter = fmt_integer
            ) %>%
            Common_style() %>%
            tab_style(
                style=cell_text(
                    weight = "bold",
                    size = px(12),
                ),
                locations = list(
                    cells_summary(rows = 1),
                    cells_stub_summary(rows = 1),
                    cells_body(columns = 合計)
                )
            )
        
        # 1行目ならメインタイトルをヘッダにつける
        if (i == k) {
            gt_table <- gt_table %>%
                tab_header(title = main_title)
        }
        
        # imageの出力(2023.08.09 Agata.K 修正)
        # gtsave(gt_table, filename = paste0("./output/img/Table5.", i, ".png"))
        gtsave(gt_table, filename = paste0(prtpath, "/output/img/Table5.", i, ".png"))
        report_title <- setdiff(report_title, tmp)
        
    }
    
}

# ここからメインの処理-----------
# 履歴：2022.08.18 Agata.K 修正
#       2023.08.09 Agata.K 修正
#--------------------------------

#################################################################################################
# 出力先のdataとimgディレクトリが無ければ、生成する（2023.08.09 Agata.K 追加）
output_dir <- paste0(prtpath, "/output")
if(file.exists(paste0(output_dir, "/data")) == FALSE) dir.create(paste0(output_dir, "/data"))
if(file.exists(paste0(output_dir, "/img")) == FALSE) dir.create(paste0(output_dir, "/img"))
#################################################################################################

# 疾患詳細データの連結
for(i in 1:length(disease_detail)) {
    
    names(disease_detail[i]) <- report_title$disease[i]
    
    # 詳細集計データを連結する（1つ目なら代入、2つ目以降はバインド）
    if(i == 1) {
        detail_res <- disease_detail[[i]]
    } else {
        detail_res <- bind_rows(detail_res, disease_detail[[i]])
    }
    
    # 結合したら、id情報は不要になるので、削除（2022.7.28 Agata.K）
    disease_detail[[i]] <- disease_detail[[i]] %>% select(-id)
}

# 結合したら、id不要なので削除を追加（2022.7.28 Agata.K）
detail_res <- detail_res %>% left_join(report_title, by = c("id" = "id")) %>% select(id, category, disease, everything()) %>% select(-id)
#################################################################################################

# png出力実行
# 今日の日付け年-1の「xx年度」でタイトルに記載される。
main_title <- paste0(as.numeric(format(Sys.Date(), "%Y")) -1, "年診断例　団体別施設登録数及び登録数")
OutputTopTable(registration[["団体別施設登録数及び登録数"]], main_title)

OutputSiteTable(registration[["施設別登録数"]], split_num = 35)

OutputDiseaseMajorTable(disease[["腫瘍性疾患・大分類"]], "血液腫瘍性疾患", ver = 1)
OutputDiseaseMajorTable(disease[["非腫瘍性疾患・大分類"]], "非腫瘍性血液疾患", ver = 2)

OutputDiseaseMinorTable(disease[["腫瘍性疾患・中分類"]],"血液腫瘍性疾患", split_num = 35, ver = 1)
OutputDiseaseMinorTable(disease[["非腫瘍性疾患・中分類"]],"非血液腫瘍性疾患", split_num = 35, ver = 2)

OutputDetailTable(detail_res, report_title, split_num = 5)

#################################################################################################

# gtのsummary_rowsを使って合計値を出している表を対象に、csvでもやっぱり合計値を出したいので追加
# 団体別登録数の合計も追加(2022.8.18 Agata.K)
registration[["団体別施設登録数及び登録数"]] <-  AddTotal(registration[["団体別施設登録数及び登録数"]], 2)

# 大分類の合計値
disease[["腫瘍性疾患・大分類"]] <- AddTotal(disease[["腫瘍性疾患・大分類"]], 2)
disease[["非腫瘍性疾患・大分類"]] <- AddTotal(disease[["非腫瘍性疾患・大分類"]], 2)

# 詳細集計の合計値を追加（引数間違いがあり修正、3⇒2）(2022.8.18 Agata.K)
for(i in 1:length(disease_detail)) {
    disease_detail[[i]] <- AddTotal(disease_detail[[i]], 2)
}

# csv出力データファイルの明示
# 日付だけだと、同じファイル内に追加で出力していってしまうので、ファイル名に時間情報を追加する
#today <- format(Sys.Date(), "%Y%m%d")
nowdt <- format(Sys.time(), "%Y%m%d_%I%M%S")
output_registration <- paste0(prtpath, "/output/data/JSH_registry_", nowdt, ".csv")
output_disease <- paste0(prtpath, "/output/data/JSH_disease_", nowdt, ".csv")
output_disease_detail <- paste0(prtpath, "/output/data/JSH_disease_detail_", nowdt, ".csv")

# csv出力実行
OutputCSV("施設別登録数", output_registration, registration)
OutputCSV("疾患集計", output_disease, disease)
OutputCSV("疾患集計：詳細", output_disease_detail, disease_detail)
#################################################################################################
