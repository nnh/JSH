
#2012年診断以降
#診断時年齢20歳未満






for(i in 1:length(DF$症例登録番号)){

　　strA = DF$field7[i]  　　#疾患種別
　　strB = DF$field37[i] 　　#血液腫瘍性疾患名
　　strC = DF$field10[i] 　　#基礎疾患

  strALL　<- (strB==1)  　　　　　　　　　　　#ALL
  strNHL  <- (srtB==5) 　　　　　　　　　　　#NHL　　　　　　　　　　　　　　　　　　　　　　
  strAML　<- (strA==2)  　　　　　　　　　　　#AML
  strexCML　<-  (strA==4 && DF$field159[i]==2) #exceptCML
  strRareLuke  <-  (strA==3) 　　　　　　　 #まれな白血病
  strHL　<-  (strA==6) 　　　　　　　　　　　#HL
  strHistiocyte  <-  (strA==8)   　　　　　　#組織球症
  strOtherLympo  <-  (strA==7)  　　　　　　 #その他のリンパ増殖性疾患

　　strMHDECOD = ""

 if((strA==2)|(strHistiocyte && DF$field69[i]==2)){
         strMHDECOD <- "non_tumor"     
     }else if((strA==1 && strB==2 && strC==1)|(strA==1 && strB==2 && strC==2)){
         strMHDECOD <- 53
     }else if(strA==1 && strB==10){
            strMHDECOD <- 52
     }else if((strALL&&DF$field20[i]==6) | (strALL&&DF$field20[i]==7) | (strALL&&DF$field20[i]==8)){
            strMHDECOD <-65
     }else if((strALL&&DF$field20[i]==1) | (strALL&&DF$field20[i]==2) | (strALL&&DF$field20[i]==3)){
            strMHDECOD <-66
     }else if((strALL&&DF$field19[i]==2)){
            strMHDECOD <-62
     }else if(strALL&&DF$field19[i]==4) | (strALL&&DF$field19[i]==5) | (strALL&&DF$field19[i]==6) | (strALL&&DF$field19[i]==7)){
            strMHDECOD <-63
     }else if(strALL&&DF$field19[i]==3){
            strMHDECOD <-64
     }else if(strALL&&DF$field19[i]==14){
            strMHDECOD <- 67
     }else if(strALL&&DF$field19[i]==8){
            strMHDECOD <- 68     
     }else if((strALL&&DF$field17[i]==1) | (strNHL&&DF$field55[i]==2)){
            strMHDECOD <- 61     
     }else if((strALL&&DF$field17[i]==3) | (strNHL&&DF$field55[i]==1)){
            strMHDECOD <- 69
     }else if(strALL&&DF$field17[i]==2){
            strMHDECOD <- 70                                            #End Classification of ALL
     }else if (strAML && DF$field26[i]==12){
            strMHDECOD <- 30 
     }else if ((strAML && DF$field26[i]==4)| (strAML && DF$field25[i]==7)){
            strMHDECOD <- 31
     }else if ((strAML && DF$field26[i]==3)| (strAML && DF$field25[i]==4)| (strAML && DF$field25[i]==5){
            strMHDECOD <- 32
     }else if (strAML && DF$field26[i]==6){
            strMHDECOD <- 33
     }else if (strAML && DF$field26[i]==12){
            strMHDECOD <- 34
     }else if (strAML && DF$field26[i]==13){
            strMHDECOD <- 36
     }else if ((strAML && DF$field26[i]==9) | (strRareLuke && DF$field32[i]==3)){
            strMHDECOD <- 57 
     }else if (strAML && DF$field25[i]==1){
            strMHDECOD <- 42  
     }else if (strAML && DF$field25[i]==2){
            strMHDECOD <- 43    
     }else if (strAML && DF$field25[i]==3){
            strMHDECOD <- 44     
     }else if (strAML && DF$field25[i]==6){
            strMHDECOD <- 45  
     }else if ((strAML && DF$field25[i]==8) | (strAML && DF$field25[i]==9)){
            strMHDECOD <- 46 
     }else if ((strAML && DF$field25[i]==10) | (strAML && DF$field25[i]==11)){
            strMHDECOD <- 47    
     }else if (strAML && DF$field25[i]==6){
            strMHDECOD <- 48   
     }else if (strAML && DF$field24[i]==5){
            strMHDECOD <- 51   
      }else if (strAML && DF$field24[i]==6){
            strMHDECOD <- 54  
      }else if (strAML){
            strMHDECOD <- 41                                             #End Classification of AML
      }else if (strA==4 && DF$field159[i]==1){
            strMHDECOD <- 1
      }else if (strexCML&& DF$field164[i]==1 && DF$field35[i]==2){
            strMHDECOD <- 5
      }else if (strexCML&& DF$field164[i]==1 && DF$field35[i]==3){
            strMHDECOD <- 3
      }else if (strexCML&& DF$field164[i]==1 && DF$field35[i]==7){
            strMHDECOD <- 4
      }else if (strexCML&& DF$field164[i]==1 && DF$field35[i]==5){
            strMHDECOD <- 7
      }else if (strexCML&& DF$field164[i]==1 && DF$field35[i]==6){
            strMHDECOD <- 9
      }else if (strexCML&& DF$field164[i]==1 && DF$field35[i]==4){
            strMHDECOD <- 12
      }else if (strexCML&& DF$field164[i]==2 && DF$field47[i]==1){
            strMHDECOD <- 16
      }else if (strexCML&& DF$field164[i]==2 && DF$field47[i]==2){
            strMHDECOD <- 17
      }else if (strexCML&& DF$field164[i]==2 && DF$field47[i]==3){
            strMHDECOD <- 18
      }else if (strexCML&& DF$field164[i]==2 && DF$field47[i]==4){
            strMHDECOD <- 19
      }else if (strexCML&& DF$field164[i]==3 && DF$field51[i]==5){
            strMHDECOD <- 27
      }else if (strexCML&& DF$field164[i]==3 && DF$field49[i]==1){
            strMHDECOD <- 21
      }else if (strexCML&& DF$field164[i]==3 && DF$field49[i]==2){
            strMHDECOD <- 24
      }else if ((strexCML&& DF$field164[i]==3 && DF$field49[i]==3)|(strexCML&& DF$field164[i]==3 && DF$field49[i]==4)){
            strMHDECOD <- 25
      }else if ((strexCML&& DF$field164[i]==3 && DF$field49[i]==5)|(strexCML&& DF$field164[i]==3 && DF$field49[i]==6)){
            strMHDECOD <- 26
      }else if (strexCML&& DF$field164[i]==3 && DF$field49[i]==7){
            strMHDECOD <- 28
      }else if (strexCML&& DF$field164[i]==3 && DF$field49[i]==9){
            strMHDECOD <- 29                                          #End Classification of MDS MPD
      }else if (strRareLuke && DF$field32[i]==2){
            strMHDECOD <- 56      
      }else if (strRareLuke && DF$field28[i]==1 &&  DF$field29[i]==1 ){
            strMHDECOD <- 58
      }else if (strRareLuke && DF$field28[i]==1 &&  DF$field29[i]==2){
            strMHDECOD <- 59
      }else if ((strRareLuke && DF$field28[i]==1 &&  DF$field29[i]==3)|(strRareLuke && DF$field28[i]==6){
            strMHDECOD <- 60
      }else if (strRareLuke && DF$field28[i]==2){
            strMHDECOD <- 55                                         #End Classification of Rare leukemia
      }else if (strNHL && DF$field55[i]==3){
            strMHDECOD <- 106
      }else if (strNHL && DF$field55[i]==4){
            strMHDECOD <- 93
      }else if (strNHL && DF$field55[i]==5){
            strMHDECOD <- 100
      }else if (strNHL && DF$field55[i]==6){
            strMHDECOD <- 90
      }else if ((strNHL && DF$field55[i]==7 && DF$field67[i]==1){
            strMHDECOD <- 129
       }else if ((strNHL && DF$field55[i]==7 && DF$field67[i]==2){
            strMHDECOD <- 130
      }else if (strNHL && DF$field55[i]==9){
            strMHDECOD <- 116
      }else if (strNHL && DF$field55[i]==10){
            strMHDECOD <- 119
      }else if (strNHL && DF$field55[i]==11){
            strMHDECOD <- 127
      }else if (strNHL && DF$field55[i]==13){
            strMHDECOD <- 86
      }else if (strNHL && DF$field55[i]==14){
            strMHDECOD <- 88
      }else if (strNHL && DF$field55[i]==15){
            strMHDECOD <- 107
      }else if (strNHL && DF$field55[i]==16){
            strMHDECOD <- 117
      }else if (strNHL && DF$field55[i]==17){
            strMHDECOD <- 113
      }else if (strNHL && DF$field55[i]==15){
            strMHDECOD <- 107                                         #End Classification of NHL
      }else if (strHL && DF$field61[i]==1){
            strMHDECOD <- 131
      }else if (strHL && DF$field61[i]==2){
            strMHDECOD <- 133
      }else if (strHL && DF$field61[i]==3){
            strMHDECOD <- 134
      }else if (strHL && DF$field61[i]==4){
            strMHDECOD <- 135
      }else if (strHL && DF$field61[i]==5){
            strMHDECOD <- 136
      }else if (strHL && DF$field61[i]==7){
            strMHDECOD <- 132                                         #End Classification of HL
      }else if (strHistiocyte && DF$field69[i]==1){
            strMHDECOD <- 138  
      }else if (strHistiocyte && DF$field69[i]==4){
            strMHDECOD <- 137
      }else if (strHistiocyte && DF$field69[i]==5){
            strMHDECOD <- 144
      }else if (strOtherLympo && DF$field77[i]==4){
            strMHDECOD <- 145
      }else if (strOtherLympo && DF$field77[i]==2){
            strMHDECOD <- 154
      }else if (strOtherLympo && DF$field77[i]==3){
            strMHDECOD <- 155    
      }else{
            strMHDECOD <- ""          
                   }
    DF$MHDECOD[i]=strMHDECOD
       }　　　　　　　　　　　　　　　　　　　　 #for文終わり　DF$MHDECODが空値は疾患名が当てはまらないcase
