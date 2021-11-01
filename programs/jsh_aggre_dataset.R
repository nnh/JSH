# JSH データ集計用　データカット
# 2021/06/07 Agata.K

###########################################
# 参考
###########################################
# 診断年:2012
# 固定日:20130731
# NAS1  ://192.168.200.222/Datacenter/学会事務/130_日本血液学会/04.03.02 データ集計/2013/130826/JSH_registration_130823_1708.csv

# 診断年:2013
# 固定日:20140630
# NAS1  ://192.168.200.222/Datacenter/学会事務/130_日本血液学会/04.03.02 データ集計/2014/140811/JSHダウンロードデータCSV/JSH_registration_140809_0100.csv

# 診断年:2014
# 固定日:20150630
# NAS1  ://192.168.200.222/Datacenter/学会事務/130_日本血液学会/04.03.02 データ集計/2015/150731/元データ/JSH_registration_150731_1031.csv

# 診断年:2015
# 固定日:20160623
# NAS1  ://192.168.200.222/Datacenter/学会事務/130_日本血液学会/04.03.02 データ集計/2016/JSH_sheets_160905_1920/JSH_registration_160905_1920.csv
# NAS2  ://192.168.200.222/Datacenter/学会事務/130_日本血液学会/04.03.02 データ集計/2016/JSH_sheets_160905_1920/JSH_report_160905_1920.csv

# 診断年:2016
# 固定日:20170531
# NAS1  ://192.168.200.222/Datacenter/学会事務/130_日本血液学会/04.03.02 データ集計/2017/Ptoshデータ/JSH_registration_170724_1911.csv
# NAS2  ://192.168.200.222/Datacenter/学会事務/130_日本血液学会/04.03.02 データ集計/2017/Ptoshデータ/JSH_report_170724_1911.csv

# 診断年:2017
# 固定日:20180531
# NAS1  ://192.168.200.222/Datacenter/学会事務/130_日本血液学会/04.03.02 データ集計/2018/201807/rawdata/JSH_registration_180717_1530.csv
# NAS2  ://192.168.200.222/Datacenter/学会事務/130_日本血液学会/04.03.02 データ集計/2018/201807/rawdata/JSH_report_180717_1530.csv

# 診断年:2018
# 固定日:20190531
# NAS1  ://192.168.200.222/Datacenter/学会事務/130_日本血液学会/04.03.02 データ集計/2019/集計/作業/20190806/rawdata/JSH_registration_190801_1100.csv
# NAS2  ://192.168.200.222/Datacenter/学会事務/130_日本血液学会/04.03.02 データ集計/2019/集計/作業/20190806/rawdata/JSH_report_190801_1100.csv

# 診断年:2019
# 固定日:20200621
# NAS1  ://192.168.200.222/Datacenter/学会事務/130_日本血液学会/04.03.02 データ集計/2020/集計/作業/20200826/rawdata/JSH_registration_200701_0938.csv
# NAS2  ://192.168.200.222/Datacenter/学会事務/130_日本血液学会/04.03.02 データ集計/2020/集計/作業/20200826/rawdata/JSH_report_200701_0938.csv
###########################################

  # データカット用設定
  # 診断年
  kyear_cut <- 2019
  # 固定日
  day_cut <- "20200621"
  # 疾患名
  hosp_cut <- "北見赤十字病院"

  # 入力パス・ファイル名と読込処理（診断年により）
  # 2012-2014年診断
  # input_path_o <- "//192.168.200.222/Datacenter/学会事務/130_日本血液学会/04.03.02 データ集計/2015/150731/元データ/JSH_registration_150731_1031.csv"
  # jsh_rgst_o <- read.csv(input_path_o, na.strings = c(""), as.is=T, fileEncoding="CP932")
  #2015年以降診断
  input_path_rgst_n <- "//192.168.200.222/Datacenter/学会事務/130_日本血液学会/04.03.02 データ集計/2020/集計/作業/20200826/rawdata/JSH_registration_200701_0938.csv"
  input_path_rept_n <- "//192.168.200.222/Datacenter/学会事務/130_日本血液学会/04.03.02 データ集計/2020/集計/作業/20200826/rawdata/JSH_report_200701_0938.csv"
  jsh_rgst_n <- read.csv(input_path_rgst_n, na.strings = c(""), as.is=T, fileEncoding="CP932")
  jsh_rept_n <- read.csv(input_path_rept_n, na.strings = c(""), as.is=T, fileEncoding="CP932")
  
  # 出力パス・ファイル名
  output_path <- "//192.168.200.222/Datacenter/Trials/JSH/Registry/09.03.01 第三者機関とのコミュニケーション/データ提出/JSH-BDR_20210521_北見赤十字病院/output/JSH_datacut_2019.csv"


  # 以降設定不要

  # 作成日カット
  if(kyear_cut <= 2014){
    jsh_rgst_cut <- subset(jsh_rgst_o, format(as.Date(jsh_rgst_o$作成日), "%Y%m%d") <= day_cut)
  }else if(kyear_cut >= 2015){
    jsh_rept_cut <- subset(jsh_rept_n, format(as.Date(jsh_rept_n$作成日), "%Y%m%d") <= day_cut)
  }

  # 診断年カット
  if(kyear_cut <= 2014){
    jsh_rgst_cut <- subset(jsh_rgst_cut, substr(jsh_rgst_cut$診断年月日, 1, 4) == kyear_cut)
  }else if(kyear_cut >= 2015){
    jsh_rept_cut <- subset(jsh_rept_cut, substr(jsh_rept_cut$診断年月日, 1, 4) == kyear_cut)
  }

  # 施設名カット
  if(kyear_cut <=2014){
    jsh_rgst_cut <- subset(jsh_rgst_cut, jsh_rgst_cut$シート作成時施設名 == hosp_cut)
  }else if(kyear_cut >= 2015){
    jsh_rept_cut <- subset(jsh_rept_cut, jsh_rept_cut$シート作成時施設名 == hosp_cut)
  }
  
  # 最終データ生成
  if(kyear_cut <=2014){　#2014年診断以前はマージ不要
    jsh_data <- jsh_rgst_cut
    jsh_data[is.na(jsh_data)] <- ""
    
  }else if(kyear_cut >= 2015){　# マージ要
    #registrationとreportが区分けできるようにする
    colnames(jsh_rgst_n) <- paste0("registration_", colnames(jsh_rgst_n))
    #registrationとreportを登録コードでマージする
    jsh_data <- merge(jsh_rgst_n, jsh_rept_cut, by.x = "registration_登録コード", by.y = "登録コード", all.y = T)
    jsh_data[is.na(jsh_data)] <- ""
  }

  #CSVファイル出力
  write.csv(jsh_data, output_path, row.names = F)

  
  
  