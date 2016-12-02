#JSPHO to WHO mapping
#Mamiko YOnejima
#2016/11/14

#reading csv
<<<<<<< HEAD
setwd("//Rinken-sv2/å­¦ä¼šäº‹å‹™/å€‹äºº/ç±³å³¶/GitHub/JSH2016/rawdata")
DF <- read.csv("JSPHO_registration_160720_1501.csv",as.is = T)
#2012å¹´è¨ºæ–­ä»¥é™
#è¨ºæ–­æ™‚å¹´é½¢20æ­³æœªæº€
=======
setwd("//Rinken-sv2/Šw‰ï––±/ŒÂl/•Ä“‡/GitHub/JSH2016/rawdata")
data <- read.csv("JSPHO_registration_160720_1501.csv",as.is = T)

#2012”Nf’fˆÈ~
data$year<-substr(data$f’f”NŒ“ú,1,4)
data<- data[data$year>=2012,]

#Cut data /age diagnosis is over@20
  data$¶”NŒ“ú <- as.Date(data$¶”NŒ“ú,format="%Y/%m/%d") 
  data$f’f”NŒ“ú <- as.Date(data$f’f”NŒ“ú,format="%Y/%m/%d")


  datedif <- function(starting, ending) {
 y <- as.integer((as.integer(format(ymd(ending),"%Y%m%d")) - as.integer(format(ymd(starting),"%Y%m%d")))/10000)
 months <- as.numeric((as.integer(format(ymd(ending),"%m%d")) - as.integer(format(ymd(starting),"%m%d")))/100)
 ym <- ifelse(months<0, as.integer(months + 12), as.integer(months))
 m <- ym + y*12
 d <- as.integer(ymd(ending) - ymd(starting))
 days <- as.integer(format(ymd(ending),"%d")) - as.integer(format(ymd(starting),"%d"))
 yd <- as.integer(ymd(ending) - (ymd(starting) %m+% months(12*y)))
 md <- ifelse(days<0, ymd(ending) - (ymd(starting) %m+% months(m)), days)
 return(data.frame(y,m,d,ym,yd,md))
}
  library(lubridate)@@#•K{
Sys.setlocale("LC_TIME", "C") #•K{F“ú–{ŠÔ‚ÉƒRƒ“ƒsƒ…[ƒ^İ’è‚ğ‡‚í‚¹‚éforwindows
 age <- datedif(data$¶”NŒ“ú,data$f’f”NŒ“ú)
data$age_diagnosis <- age$y
>>>>>>> dff81181f4bc457ecbc40b4d3debb539a77d5396

data<- data[data$age_diagnosis<20,]





<<<<<<< HEAD
for(i in 1:length(DF$ç™»éŒ²ã‚³ãƒ¼ãƒ‰)){
=======
#except nontumor
for(i in 1:length(data$“o˜^ƒR[ƒh)){
             ifelse(((data$field7[i]==2)|
                    (data$field37[i]==8 && data$field69[i]==2)),
                 data$MHDECOD[i] <- "non_tumor",
                 data$MHDECOD[i] <- ""
                        )
}

#Make a group of tumor
DF <- subset(data,data$MHDECOD=="")
        DF[is.na(DF)]<-"-"  #Replace NA to "-"
     
for(i in 1:length(DF$“o˜^ƒR[ƒh)){
>>>>>>> dff81181f4bc457ecbc40b4d3debb539a77d5396

ã€€ã€€strA = DF$field7[i]  ã€€ã€€#ç–¾æ‚£ç¨®åˆ¥
ã€€ã€€strB = DF$field37[i] ã€€ã€€#è¡€æ¶²è…«ç˜æ€§ç–¾æ‚£å
ã€€ã€€strC = DF$field10[i] ã€€ã€€#åŸºç¤ç–¾æ‚£

#ALL strB==1
#NHL srtB==5ã€€ã€€ã€€ã€€ã€€ã€€ã€€ã€€ã€€ã€€ã€€ã€€ã€€ã€€ã€€ã€€ã€€ã€€ã€€ã€€ã€€ã€€
#AML strB==2
#exceptCML strB==4 && DF$field159[i]==2
#ã¾ã‚Œãªç™½è¡€ç—… strB==3
#HL strB==6
#çµ„ç¹”çƒç—‡ strB==8
#ãã®ä»–ã®ãƒªãƒ³ãƒ‘å¢—æ®–æ€§ç–¾æ‚£ strB==7

<<<<<<< HEAD
ã€€ã€€strMHDECOD = ""


 if((strA==2)|(strB==8 && DF$field69[i]==2)){
         strMHDECOD <- "non_tumor"     
     }else if((strA==1 && strB==2 && strC==1)|(strA==1 && strB==2 && strC==2)){
=======
@@strMHDECOD = ""
 if((strA==1 && strB==2 && strC==1)|(strA==1 && strB==2 && strC==2)){
>>>>>>> dff81181f4bc457ecbc40b4d3debb539a77d5396
         strMHDECOD <- 53
     }else if(strA==1 && strB==10){
            strMHDECOD <- 52
     }else if((strB==1&&DF$field20[i]==6) | (strB==1&&DF$field20[i]==7) | (strB==1&&DF$field20[i]==8)){
            strMHDECOD <-65
     }else if((strB==1&&DF$field20[i]==1) | (strB==1&&DF$field20[i]==2) | (strB==1&&DF$field20[i]==3)){
            strMHDECOD <-66
     }else if((strB==1&&DF$field19[i]==2)){
            strMHDECOD <-62
     }else if((strB==1&&DF$field19[i]==4) | (strB==1&&DF$field19[i]==5) | (strB==1&&DF$field19[i]==6) | (strB==1&&DF$field19[i]==7)){
            strMHDECOD <-63
     }else if(strB==1&&DF$field19[i]==3){
            strMHDECOD <-64
     }else if(strB==1&&DF$field19[i]==14){
            strMHDECOD <- 67
     }else if(strB==1&&DF$field19[i]==8){
            strMHDECOD <- 68     
     }else if((strB==1&&DF$field17[i]==1) | (strB==5&&DF$field55[i]==2)){
            strMHDECOD <- 61     
     }else if((strB==1&&DF$field17[i]==3) | (strB==5&&DF$field55[i]==1)){
            strMHDECOD <- 69
     }else if(strB==1&&DF$field17[i]==2){
            strMHDECOD <- 70                                            #End Classification of ALL

     }else if ((strB==2 && DF$field26[i]==4)| (strB==2 && DF$field25[i]==7)){
            strMHDECOD <- 31
     }else if ((strB==2 && DF$field26[i]==3)| (strB==2 && DF$field25[i]==4)| (strB==2 && DF$field25[i]==5)){
            strMHDECOD <- 32
     }else if (strB==2 && DF$field26[i]==6){
            strMHDECOD <- 33
     }else if (strB==2 && DF$field26[i]==2){
            strMHDECOD <- 30
     }else if (strB==2 && DF$field26[i]==12){
            strMHDECOD <- 34
     }else if (strB==2 && DF$field26[i]==13){
            strMHDECOD <- 36
     }else if (strB==2 && DF$field26[i]==9){      
            strMHDECOD <- 57 
     }else if (strB==2 && DF$field25[i]==1){
            strMHDECOD <- 42  
     }else if (strB==2 && DF$field25[i]==2){
            strMHDECOD <- 43    
     }else if (strB==2 && DF$field25[i]==3){
            strMHDECOD <- 44     
     }else if (strB==2 && DF$field25[i]==6){
            strMHDECOD <- 45  
     }else if ((strB==2 && DF$field25[i]==8) | (strB==2 && DF$field25[i]==9)){
            strMHDECOD <- 46 
     }else if ((strB==2 && DF$field25[i]==10) | (strB==2 && DF$field25[i]==11)){
            strMHDECOD <- 47    
     }else if (strB==2 && DF$field25[i]==12){
            strMHDECOD <- 48   
     }else if (strB==2 && DF$field24[i]==5){
            strMHDECOD <- 51   ã€€ã€€ã€€ã€€ã€€ã€€ã€€ã€€ã€€ã€€ã€€ã€€ã€€ã€€ã€€ã€€
      }else if (strB==2 && DF$field24[i]==6){
            strMHDECOD <- 54  
      }else if (strB==2){
            strMHDECOD <- 41                                             #End Classification of AML
      }else if (strB==4 && DF$field159[i]==1){
<<<<<<< HEAD
            strMHDECOD <- 1ã€€ã€€ã€€ã€€ã€€ã€€ã€€ã€€ã€€ã€€ã€€ã€€ã€€##ã“ã“ã¾ã§å‹•ã
=======
            strMHDECOD <- 1@@@@@@@@@@@@@
>>>>>>> dff81181f4bc457ecbc40b4d3debb539a77d5396
      }else if (strB==4 && DF$field159[i]==2 && DF$field164[i]==1 && DF$field35[i]==2){
            strMHDECOD <- 5
      }else if (strB==4 && DF$field159[i]==2 && DF$field164[i]==1 && DF$field35[i]==3){
            strMHDECOD <- 3
      }else if (strB==4 && DF$field159[i]==2 && DF$field164[i]==1 && DF$field35[i]==7){
            strMHDECOD <- 4
      }else if (strB==4 && DF$field159[i]==2 && DF$field164[i]==1 && DF$field35[i]==5){
            strMHDECOD <- 7
      }else if (strB==4 && DF$field159[i]==2 && DF$field164[i]==1 && DF$field35[i]==6){
            strMHDECOD <- 9
      }else if (strB==4 && DF$field159[i]==2 && DF$field164[i]==1 && DF$field35[i]==4){
            strMHDECOD <- 12
      }else if (strB==4 && DF$field159[i]==2 && DF$field164[i]==2 && DF$field47[i]==1){
            strMHDECOD <- 16
      }else if (strB==4 && DF$field159[i]==2 && DF$field164[i]==2 && DF$field47[i]==2){
            strMHDECOD <- 17
      }else if (strB==4 && DF$field159[i]==2 && DF$field164[i]==2 && DF$field47[i]==3){
            strMHDECOD <- 18
      }else if (strB==4 && DF$field159[i]==2&& DF$field164[i]==2 && DF$field47[i]==4){
            strMHDECOD <- 19
      }else if (strB==4 && DF$field159[i]==2&& DF$field164[i]==3 && DF$field51[i]==5){
            strMHDECOD <- 27
      }else if (strB==4 && DF$field159[i]==2&& DF$field164[i]==3 && DF$field49[i]==1){
            strMHDECOD <- 21
      }else if (strB==4 && DF$field159[i]==2&& DF$field164[i]==3 && DF$field49[i]==2){
            strMHDECOD <- 24
      }else if ((strB==4 && DF$field159[i]==2&& DF$field164[i]==3 && DF$field49[i]==3)|(strB==4 && DF$field159[i]==2&& DF$field164[i]==3 && DF$field49[i]==4)){
            strMHDECOD <- 25
      }else if ((strB==4 && DF$field159[i]==2&& DF$field164[i]==3 && DF$field49[i]==5)|(strB==4 && DF$field159[i]==2&& DF$field164[i]==3 && DF$field49[i]==6)){
            strMHDECOD <- 26
      }else if (strB==4 && DF$field159[i]==2&& DF$field164[i]==3 && DF$field49[i]==7){
            strMHDECOD <- 28
      }else if (strB==4 && DF$field159[i]==2&& DF$field164[i]==3 && DF$field49[i]==9){
            strMHDECOD <- 29                                          #End Classification of MDS MPD
      }else if (strB==3 && DF$field32[i]==3){      
            strMHDECOD <- 57 
      }else if (strB==3 && DF$field32[i]==2){
            strMHDECOD <- 56      
      }else if (strB==3 && DF$field28[i]==1 &&  DF$field29[i]==1 ){
            strMHDECOD <- 58
      }else if (strB==3 && DF$field28[i]==1 &&  DF$field29[i]==2){
            strMHDECOD <- 59
      }else if ((strB==3 && DF$field28[i]==1 &&  DF$field29[i]==3)|(strB==3 && DF$field28[i]==6)){
            strMHDECOD <- 60
      }else if (strB==3 && DF$field28[i]==2){
            strMHDECOD <- 55                                         #End Classification of Rare leukemia
      }else if (strB==5 && DF$field55[i]==3){
            strMHDECOD <- 106
      }else if (strB==5 && DF$field55[i]==4){
            strMHDECOD <- 93
      }else if (strB==5 && DF$field55[i]==5){
            strMHDECOD <- 100
      }else if (strB==5 && DF$field55[i]==6){
            strMHDECOD <- 90
      }else if (strB==5 && DF$field55[i]==7 && DF$field67[i]==1){
            strMHDECOD <- 129
       }else if (strB==5 && DF$field55[i]==7 && DF$field67[i]==2){
            strMHDECOD <- 130
      }else if (strB==5 && DF$field55[i]==9){
            strMHDECOD <- 116
      }else if (strB==5 && DF$field55[i]==10){
            strMHDECOD <- 119
      }else if (strB==5 && DF$field55[i]==11){
            strMHDECOD <- 127
      }else if (strB==5 && DF$field55[i]==13){
            strMHDECOD <- 86
      }else if (strB==5 && DF$field55[i]==14){
            strMHDECOD <- 88
      }else if (strB==5 && DF$field55[i]==15){
            strMHDECOD <- 107
      }else if (strB==5 && DF$field55[i]==16){
            strMHDECOD <- 117
      }else if (strB==5 && DF$field55[i]==17){
            strMHDECOD <- 113
      }else if (strB==5 && DF$field55[i]==15){
            strMHDECOD <- 107                                         #End Classification of NHL
      }else if (strB==6 && DF$field61[i]==1){
            strMHDECOD <- 131
      }else if (strB==6 && DF$field61[i]==2){
            strMHDECOD <- 133
      }else if (strB==6 && DF$field61[i]==3){
            strMHDECOD <- 134
      }else if (strB==6 && DF$field61[i]==4){
            strMHDECOD <- 135
      }else if (strB==6 && DF$field61[i]==5){
            strMHDECOD <- 136
      }else if (strB==6 && DF$field61[i]==7){
            strMHDECOD <- 132                                         #End Classification of HL
      }else if (strB==8 && DF$field69[i]==1){
            strMHDECOD <- 138  
      }else if (strB==8 && DF$field69[i]==4){
            strMHDECOD <- 137
      }else if (strB==8 && DF$field69[i]==5){
            strMHDECOD <- 144
      }else if (strB==7 && DF$field77[i]==4){
            strMHDECOD <- 145
      }else if (strB==7 && DF$field77[i]==2){
            strMHDECOD <- 154
      }else if (strB==7 && DF$field77[i]==3){
            strMHDECOD <- 155    
      }else{
            strMHDECOD <- "" }         
                   
    DF$MHDECOD[i]=strMHDECOD
<<<<<<< HEAD
       }ã€€ã€€ã€€ã€€ã€€ã€€ã€€ã€€ã€€ã€€ã€€ã€€ã€€ã€€ã€€ã€€ã€€ã€€ã€€ã€€ #foræ–‡çµ‚ã‚ã‚Šã€€DF$MHDECODãŒç©ºå€¤ã¯ç–¾æ‚£åãŒå½“ã¦ã¯ã¾ã‚‰ãªã„case
=======
       }@@@@@@@@@@@@@@@@@@@@ #for•¶I‚í‚è@DF$MHDECOD‚ª‹ó’l‚Í¾Š³–¼‚ª“–‚Ä‚Í‚Ü‚ç‚È‚¢case


#SCSTRESC
DF$Œ§CD <-sub("^.*.-","",DF$‰”­ZŠ)  
DF$Œ§CD <-substr(DF$Œ§CD,1,2)
           
#Pick up some data using
WHOdata <- DF[,c("¶”NŒ“ú","f’f”NŒ“ú","“o˜^ƒR[ƒh","«•Ê","Œ§CD","¶€","€–S“ú","ÅIŠm”F“ú","field161","MHDECOD")]
colnames(WHOdata)[1:9] <- c("BRTHDTC","MHSTDTC","SUBJID","SEX","SCSTRESC","DTHFL","DTHDTC","DSSTDTC","SITEID")

#Replace Death or Alive CD to 01
for(i in 1:length(WHOdata$SUBJID)){
            if (WHOdata$DTHFL[i]=="true"){
                     WHOdata$DTHFL[i] <- 1
            }else if (WHOdata$DTHFL[i]=="false"){
                     WHOdata$DTHFL[i] <- 0
             }else{
            WHOdata$DTHFL[i] <- "" }
          }



setwd("../output")
write.csv(WHOdata,"who.csv",row.names=F)
>>>>>>> dff81181f4bc457ecbc40b4d3debb539a77d5396
