#JSPHO to WHO mapping
#Mamiko YOnejima
#2016/11/14

#reading csv
setwd("//Rinken-sv2/学会事務/個人/米島/GitHub/JSH2016/rawdata")
DF <- read.csv("JSPHO_registration_160720_1501.csv",as.is = T)
#2012年診断以降
#診断時年齢20歳未満






for(i in 1:length(DF$症例登録番号)){

　　strA = DF$field7[i]  　　#疾患種別
　　strB = DF$field37[i] 　　#血液腫瘍性疾患名
　　strC = DF$field10[i] 　　#基礎疾患

#ALL strB==1
#NHL srtB==5　　　　　　　　　　　　　　　　　　　　　　
#AML strB==2
#exceptCML strB==4 && DF$field159[i]==2
#まれな白血病 strB==3
#HL strB==6
#組織球症 strB==8
#その他のリンパ増殖性疾患 strB==7

　　strMHDECOD = ""


 if((strA==2)|(strB==8 && DF$field69[i]==2)){
         strMHDECOD <- "non_tumor"     
     }else if((strA==1 && strB==2 && strC==1)|(strA==1 && strB==2 && strC==2)){
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
     }else if (strB==2 && DF$field26[i]==12){
            strMHDECOD <- 30 
     }else if ((strB==2 && DF$field26[i]==4)| (strB==2 && DF$field25[i]==7)){
            strMHDECOD <- 31
     }else if ((strB==2 && DF$field26[i]==3)| (strB==2 && DF$field25[i]==4)| (strB==2 && DF$field25[i]==5)){
            strMHDECOD <- 32
     }else if (strB==2 && DF$field26[i]==6){
            strMHDECOD <- 33
     }else if (strB==2 && DF$field26[i]==12){
            strMHDECOD <- 34
     }else if (strB==2 && DF$field26[i]==13){
            strMHDECOD <- 36
     }else if ((strB==2 && DF$field26[i]==9) | (strB==3 && DF$field32[i]==3)){
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
     }else if (strB==2 && DF$field25[i]==6){
            strMHDECOD <- 48   
     }else if (strB==2 && DF$field24[i]==5){
            strMHDECOD <- 51   
      }else if (strB==2 && DF$field24[i]==6){
            strMHDECOD <- 54  
      }else if (strB==2){
            strMHDECOD <- 41                                             #End Classification of AML
      }else if (strB==4 && DF$field159[i]==1){
            strMHDECOD <- 1
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
            strMHDECOD <- ""          
                   }
    DF$MHDECOD[i]=strMHDECOD
       }　　　　　　　　　　　　　　　　　　　　 #for文終わり　DF$MHDECODが空値は疾患名が当てはまらないcase
