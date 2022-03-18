# 作成者: Sayuri Yamashita
# 作成日: 2021.10.13
# 対応フロー: JSHregistry集計用データ作成プログラム仕様書_20211013.xlsx
# コピー元: none
# 変更履歴: none
#######################################################################

source("summarize.R", local = F, encoding = "UTF-8")

library(gt)

# リストに格納した結果をCSVで出力する関数
OutputCSV <- function(title, out_file, list_data){
  cat(paste0(title, "\n"), file = out_file, append = T)
  lapply(1:length(list_data), function(i){
    subtitle <- names(list_data[i])
    print(subtitle)
    cat(paste0("\n【", subtitle, "】\n"), file = out_file, append = T)
    write.table(list_data[[i]], file = out_file, append = T, sep=",", row.names = F, col.names = T, quote = T)
  })
}

# 集計表の共通書式を設定する関数
Common_style <- function(data){
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

# 団体別施設別登録数の図
OutputTopTable <- function(df, main_title){
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
    gtsave(filename = paste0("./output/img/Table1.png"))
}

# 施設登録数の図（split_num：ページあたりの行数を指定）
OutputSiteTable <- function(df, split_num){
  times <- ceiling(nrow(df)/split_num)
  for (i in 1:times) {
    tmp <- df %>% head(n = split_num)
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
    gtsave(gt_table, filename = paste0("./output/img/Table2.", i, ".png"))
    df <- setdiff(df, tmp)
  }
}

# 大分類の図（ver：ファイル名の採番）
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
    gtsave(filename = paste0("./output/img/Table3.", ver, ".png"))
}

# 中分類の図（split_num：ページあたりの行数を指定、ver：ファイル名の採番）
OutputDiseaseMinorTable <- function(df, main_title, split_num, ver){
  times <- ceiling(nrow(df)/split_num)
  df$診断名 <- gsub("\\?", "-", df$診断名)

  for (i in 1:times) {
    tmp <- df %>% head(n = split_num)
    gt_table <- tmp %>%
      rename(`疾患名・中分類` = 診断名, 疾患名 = MHTERM) %>%
      group_by(`疾患名・中分類`) %>%
      gt(rowname_col = "疾患名") %>%
      Common_style() %>%
      text_transform(
        locations = cells_stub(
          rows = everything()
        ),
        fn = function(x) {
          gsub("\\?", "-", x)
        }
      )

    if(i == 1) {
      gt_table <- gt_table %>%
        tab_header(title = main_title)
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

    gtsave(gt_table, filename = paste0("./output/img/Table4.", ver, ".", i, ".png"))
    df <- setdiff(df, tmp)

  }
}

# 疾患詳細の図
OutputDetailTable <- function(df, report_title, split_num){
  times <- ceiling(nrow(report_title)/split_num)
  main_title <- ""

  for (i in 1:times) {
    tmp <- report_title %>% head(n = split_num)

    if (main_title != tmp$category[1]) {
      main_title <- tmp$category[1]
      k <- i
    }

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

    if (i == k) {
      gt_table <- gt_table %>%
        tab_header(title = main_title)
    }

    gtsave(gt_table, filename = paste0("./output/img/Table5.", i, ".png"))
    report_title <- setdiff(report_title, tmp)

  }

}


#################################################################################################

# 疾患詳細データの連結

for(i in 1:length(disease_detail)) {
  names(disease_detail[i]) <- report_title$disease[i]
  if(i == 1) {
    detail_res <- disease_detail[[i]]
  } else {
    detail_res <- bind_rows(detail_res, disease_detail[[i]])
  }
}

detail_res <- detail_res %>% left_join(report_title, by = c("id" = "id")) %>% select(id, category, disease, everything())

#################################################################################################

# png出力実行
# 今日の日付け年-1の「xx年度」でタイトルに記載される。
main_title <- paste0(as.numeric(format(Sys.Date(), "%Y")) -1, "年診断例　団体別施設登録数及び登録数")
OutputTopTable(registration[["団体別施設登録数及び登録数"]], main_title)

OutputSiteTable(registration[["施設別登録数"]], split_num = 40)

OutputDiseaseMajorTable(disease[["腫瘍性疾患・大分類"]], "血液腫瘍性疾患", ver = 1)
OutputDiseaseMajorTable(disease[["非腫瘍性疾患・大分類"]], "非腫瘍性血液疾患", ver = 2)

OutputDiseaseMinorTable(disease[["腫瘍性疾患・中分類"]],"血液腫瘍性疾患", split_num = 40, ver = 1)
OutputDiseaseMinorTable(disease[["非腫瘍性疾患・中分類"]],"非血液腫瘍性疾患", split_num = 40, ver = 2)

OutputDetailTable(detail_res, report_title, split_num = 5)

#################################################################################################

# gtのsummary_rowsを使って合計値を出している表を対象に、csvでもやっぱり合計値を出したいので追加
disease[["腫瘍性疾患・大分類"]] <- AddTotal(disease[["腫瘍性疾患・大分類"]],2)
disease[["非腫瘍性疾患・大分類"]] <- AddTotal(disease[["非腫瘍性疾患・大分類"]],2)
for(i in 1:length(disease_detail)) {
  disease_detail[[i]] <- AddTotal(disease_detail[[i]], 3) 
  }

# csv出力データファイルの明示
today <- format(Sys.Date(), "%Y%m%d")
output_registration <- paste0("./output/data/JSH_registry_", today, ".csv")
output_disease <- paste0("./output/data/JSH_disease_", today, ".csv")
output_disease_detail <- paste0("./output/data/JSH_disease_detail_", today, ".csv")

# csv出力実行
OutputCSV("施設別登録数", output_registration, registration)
OutputCSV("疾患集計", output_disease, disease)
OutputCSV("疾患集計：詳細", output_disease_detail, disease_detail)

#################################################################################################
