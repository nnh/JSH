library(sas7bdat)
library(dplyr)
kALL_LBL <- "ALL, LBL"
kTarget_disease <- c(kALL_LBL, "AML", "B-NHL, MM", "HD", "LCH, DC", "MDS", "MDS/MPN", "mixed AL", "MPN", "PDGFR/FGFR/PCM1-JAK2",
                     "PTLD", "T,NK lymphoma")
test_sasdat <- read.sas7bdat("/Users/admin/Documents/GitHub/JSH/ADS2019/ads.sas7bdat")
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
  }
}
