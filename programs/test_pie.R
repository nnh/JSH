library(openxlsx)
library(sas7bdat)
library(dplyr)
output_wb <- createWorkbook()
kALL_LBL <- "ALL, LBL"
kTarget_disease <- c(kALL_LBL, "AML", "B-NHL, MM", "HD", "LCH, DC", "MDS", "MDS/MPN", "mixed AL", "MPN", "PDGFR/FGFR/PCM1-JAK2",
                     "PTLD", "T,NK lymphoma")
test_sasdat <- read.sas7bdat("/Users/admin/Documents/GitHub/JSH/ADS/ads.sas7bdat")
for (i in 1:length(kTarget_disease)){
  assign(kTarget_disease[i], subset(test_sasdat, MHGRPTERM == kTarget_disease[i]))
  for (j in 1:4){
    temp <- paste0(kTarget_disease[i], "_gen", j)
    assign(temp, subset(get(kTarget_disease[i]), AGECAT2N==j))
    temp_table <- paste0("table_", kTarget_disease[i], "_gen", j)
    temp_dst <- get(temp)
    assign(temp_table, data.frame(t(table(temp_dst$AGECAT2N, temp_dst$MHTERM)), stringsAsFactors=F))
    assign(temp_table, subset(get(temp_table), Freq>0))
    assign(temp_table, arrange(get(temp_table), desc(Freq)))
    temp_temp_table <- get(temp_table)
    temp_temp_table$byoumei <- as.character(temp_temp_table$Var1)
    if (nrow(temp_temp_table) > 10){
      temp_1 <- temp_temp_table[1:10, ]
      temp_2 <- temp_temp_table[11:nrow(temp_temp_table), ]
      temp_2[1, 1] <- ""
      temp_2[1, 4] <- "!others"
      temp_2[1, 3] <- sum(temp_2$Freq)
      temp_3 <- temp_2[1, ]
      assign(temp_table, rbind(temp_1, temp_3))
    } else {
      assign(temp_table, temp_temp_table)
    }
    addWorksheet(output_wb, gsub("/", "_", temp_table))
    writeData(output_wb, sheet=gsub("/", "_", temp_table), x=get(temp_table))
  }
}
saveWorkbook(output_wb, "/Users/admin/Documents/GitHub/JSH/output/test/test.xlsx", overwrite=T)
# ALL_LBL
output_wb <- createWorkbook()
temp_all_1 <- subset(test_sasdat, MHGRPTERM=="B-ALL/LBL")
temp_all_2 <- subset(test_sasdat, MHGRPTERM=="NK-ALL/LBL")
temp_all_3 <- subset(test_sasdat, MHGRPTERM=="T-ALL/LBL")
temp_all <- rbind(temp_all_1, temp_all_2, temp_all_3)
for (j in 1:4){
  temp <- paste0("ALL_LBL_gen", j)
  temp <- subset(temp_all, AGECAT2N==j)
  temp_table <- paste0("table_", kTarget_disease[i], "_gen", j)
  temp_all_t <- data.frame(t(table(temp$AGECAT2N, temp$MHTERM)), stringsAsFactors=F)
  temp_all_t_2 <- subset(temp_all_t, Freq>0)
  temp_all_t_3 <- arrange(temp_all_t_2, desc(Freq))
  temp_all_t_3$byoumei <- as.character(temp_all_t_3$Var1)
  if (nrow(temp_all_t_3) > 10){
    temp_1 <- temp_all_t_3[1:10, ]
    temp_2 <- temp_all_t_3[11:nrow(temp_all_t_3), ]
    temp_2[1, 1] <- ""
    temp_2[1, 4] <- "!others"
    temp_2[1, 3] <- sum(temp_2$Freq)
    temp_3 <- temp_2[1, ]
    temp_all_t_4 <- rbind(temp_1, temp_3)
  }
  addWorksheet(output_wb, gsub("/", "_", temp_table))
  writeData(output_wb, sheet=gsub("/", "_", temp_table), x=temp_all_t_4)
}
saveWorkbook(output_wb, "/Users/admin/Documents/GitHub/JSH/output/test/test_alllbl.xlsx", overwrite=T)
# 病名ごとの全て
output_wb <- createWorkbook()
for (i in 1:length(kTarget_disease)){
  temp_allcase <- subset(rbind(test_sasdat, temp_all), MHGRPTERM == kTarget_disease[i])
  for (j in 1:1){
    temp <- temp_allcase
    temp_all_t <- data.frame(t(table(temp$MHTERM)), stringsAsFactors=F)
    temp_all_t_2 <- subset(temp_all_t, Freq>0)
    temp_all_t_3 <- arrange(temp_all_t_2, desc(Freq))
    addWorksheet(output_wb, i)
    if (i == 3){
      temp_all_t_3[47,2] <- ""
      temp_all_t_3[50,2] <- ""
      temp_all_t_3[51,2] <- ""
    }
    if (i == 12){
      temp_all_t_3[23,2] <- ""
    }
    writeData(output_wb, sheet=i, x=temp_all_t_3)
  }
  saveWorkbook(output_wb, "/Users/admin/Documents/GitHub/JSH/output/test/test_allcase.xlsx", overwrite=T)
}
# 世代ごとの全て
temp <- test_sasdat
temp_all_t <- data.frame(t(table(temp$AGECAT2N, temp$MHGRPTERM)), stringsAsFactors=F)
output_wb <- createWorkbook()
for (i in 1:4){
  addWorksheet(output_wb, i)
  temp_1 <- subset(temp_all_t, Var2==i)
  writeData(output_wb, sheet=i, x=temp_1)
}
# 総計
temp <- test_sasdat
addWorksheet(output_wb, "0")
temp_1 <- data.frame(t(table(temp$MHGRPTERM)), stringsAsFactors=F)
writeData(output_wb, sheet="0", x=temp_1)
addWorksheet(output_wb, "kTarget_disease")
writeData(output_wb, sheet="kTarget_disease", x=data.frame(c("B-ALL/LBL", "T-ALL/LBL", "NK-ALL/LBL", kTarget_disease)))
saveWorkbook(output_wb, "/Users/admin/Documents/GitHub/JSH/output/test/test_allgen.xlsx", overwrite=T)
