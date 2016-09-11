﻿**********************************************************************;
* Project           : JSH201609
*
* Program name      : JSH201609_STAT_MH002.sas
*
* Author            : MATSUO YAMAMOTO
*
* Date created      : 20160908
*
* Purpose           :
*
* Revision History  :
*
* Date        Author           Ref    Revision (Date in YYYYMMDD format)
* YYYYMMDD    XXXXXX XXXXXXXX  1      XXXXXXXXXXXXXXXXXXXXXXXXXXXX
*
**********************************************************************;

/*** Initial setting ***/
%MACRO CURRENT_DIR;

    %LOCAL _FULLPATH _PATH;
    %LET   _FULLPATH = ;
    %LET   _PATH     = ;

    %IF %LENGTH(%SYSFUNC(GETOPTION(SYSIN))) = 0 %THEN
        %LET _FULLPATH = %SYSGET(SAS_EXECFILEPATH);
    %ELSE
        %LET _FULLPATH = %SYSFUNC(GETOPTION(SYSIN));

    %LET _PATH = %SUBSTR(   &_FULLPATH., 1, %LENGTH(&_FULLPATH.)
                          - %LENGTH(%SCAN(&_FULLPATH.,-1,'\')) -1 );
    &_PATH.

%MEND CURRENT_DIR;

%LET _PATH2 = %CURRENT_DIR;
%LET FILE = MH002;

%INCLUDE "&_PATH2.\JSH201609_STAT_LIBNAME.sas";

/*** Template Open ***/
%XLSOPEN(JSH201609_STAT_RES_&FILE..xlsx);

/*** ADS read ***/
%MACRO DS_READ(LIB,DS);
  DATA  &DS.;
    SET  &LIB..&DS.;
    FORMAT _ALL_;
    INFORMAT _ALL_;
  RUN ;
%MEND ;
%DS_READ(LIBADS,ADS);

DATA  MAIN;
  SET  ADS;
  CNT=1;
  TRTPN=SEX+1;
  OUTPUT;
  MHGRPCOD=0;
  MHGRPTERM="全体";
  OUTPUT;
RUN ;

%MACRO MH ( WHE , DS ) ;

  %DO I = 1 %TO 2 ;
    PROC SORT DATA = MAIN OUT = SRT %IF &I = 1 %THEN NODUPKEY ; ;
      BY &WHE SUBJID TRTPN;
    RUN ;

    PROC MEANS DATA = SRT NWAY NOPRINT ;
      CLASS &WHE TRTPN ;
      VAR CNT ;
      OUTPUT OUT = N&I N = N&I ;
    RUN ;
  %END ;

  DATA WORK.MRG ;
    MERGE WORK.N1
          WORK.N2 ;
    BY &WHE TRTPN ;
  RUN ;

  DATA WORK.OUT&DS ;
    FORMAT &WHE VAR1 - VAR2;
    MERGE MRG ( WHERE = ( TRTPN = 1 ) RENAME = ( N1 = VAR1  ) )
          MRG ( WHERE = ( TRTPN = 2 ) RENAME = ( N1 = VAR2  ) ) ;
    %IF &DS ^= 1 %THEN BY &WHE ; ;
    ARRAY BEF(*) VAR1-VAR2 ;
    DO I = 1 TO DIM( BEF ) ;
      IF BEF(I) = . THEN BEF(I) = 0 ;
    END ;
  RUN ;
%MEND ;

%MH( %STR( AGECAT1N MHGRPCOD MHGRPTERM ) , 2 )

DATA WK01;
  SET  OUT2;
  MALE = VAR1 * -1;
  FEMALE = VAR2;
RUN ; 

PROC FORMAT ; PICTURE _PCTF LOW - HIGH = "000009" ; RUN ;
PROC FORMAT;
 VALUE AGEF  19='0-4歳'
             18='5-9'
             17='10-14'
             16='15-19'
             15='20-24'
             14='25-29'
             13='30-34'
             12='35-39'
             11='40-44'
             10='45-49'
             9='50-54'
             8='55-59'
             7='60-64'
             6='65-69'
             5='70-74'
             4='75-79'
             3='80-84'
             2='85-89'
             1='90歳以上';
RUN ;

%MACRO BUTT(TIT,ID,LEN1,LEN2,LEN3);

  DATA BUTTFY&ID.;
    SET  WK01;
    IF  MHGRPCOD = &ID.;
    ZERO=0;
    FORMAT MALE FEMALE _PCTF. AGECAT1N AGEF.;
  RUN ;

  *** ALL;
  ODS GRAPHICS ON / HEIGHT = 9CM WIDTH = 12CM IMAGENAME = "&FILE.&ID."
    OUTPUTFMT = PNG RESET = INDEX   ANTIALIASMAX=96100;
  ODS LISTING GPATH = "&OUTG." IMAGE_DPI = 300 ;

  TITLE "&TIT.";
  PROC SGPLOT DATA=BUTTFY&ID.;
    HBARPARM CATEGORY=AGECAT1N RESPONSE=MALE /
      DATALABEL=MALE DATALABELATTRS=(SIZE=8);
    HBARPARM CATEGORY=AGECAT1N RESPONSE=FEMALE / 
      DATALABEL=FEMALE DATALABELATTRS=(SIZE=8);
    XAXIS VALUES=(&LEN2. TO &LEN3. BY &LEN1.) DISPLAY=(NOLABEL) /*GRID*/;
    YAXIS DISPLAY=(NOLABEL);
  RUN;

%MEND;
%BUTT(全体,0,1000,-10000,10000);
%BUTT(骨髄増殖性腫瘍,1,100,-1000,1000);
%BUTT(ＰＤＧＦＲＡ、ＰＤＧＦＲＢ、ＦＧＦＲ１異常症,2,10,-100,100);
%BUTT(骨髄異形成・骨髄増殖性腫瘍,3,50,-500,500);
%BUTT(骨髄異形成症候群,4,200,-2000,2000);
%BUTT(急性骨髄性白血病および関連腫瘍,5,100,-1000,1000);
%BUTT(系統不明急性白血病,6,10,-100,100);
%BUTT(前駆リンパ球系腫瘍,7,50,-500,500);
%BUTT(成熟Ｂ細胞腫瘍（リンパ腫・骨髄腫）,8,600,-6000,6000);
%BUTT(成熟Ｔ・ＮＫ細胞腫瘍,9,100,-1000,1000);
%BUTT(ホジキンリンパ腫,10,50,-500,500);
%BUTT(組織球・樹状細胞腫瘍,11,10,-100,100);
%BUTT(移植後リンパ増殖性疾患,12,10,-100,100);

/*** Excel Output ***/

FILENAME SYS DDE "EXCEL | SYSTEM " ;
DATA _NULL_;
  FILE SYS;
  PUT '[SELECT("R4C1")]';
  PUT "[INSERT.PICTURE(""&OUTG.\&FILE.0.png"")]";
  PUT '[SELECT("R27C1")]';
  PUT "[INSERT.PICTURE(""&OUTG.\&FILE.1.png"")]";
  PUT '[SELECT("R50C1")]';
  PUT "[INSERT.PICTURE(""&OUTG.\&FILE.2.png"")]";
  PUT '[SELECT("R73C1")]';
  PUT "[INSERT.PICTURE(""&OUTG.\&FILE.3.png"")]";
  PUT '[SELECT("R96C1")]';
  PUT "[INSERT.PICTURE(""&OUTG.\&FILE.4.png"")]";
  PUT '[SELECT("R119C1")]';
  PUT "[INSERT.PICTURE(""&OUTG.\&FILE.5.png"")]";
  PUT '[SELECT("R142C1")]';
  PUT "[INSERT.PICTURE(""&OUTG.\&FILE.6.png"")]";
  PUT '[SELECT("R165C1")]';
  PUT "[INSERT.PICTURE(""&OUTG.\&FILE.7.png"")]";
  PUT '[SELECT("R188C1")]';
  PUT "[INSERT.PICTURE(""&OUTG.\&FILE.8.png"")]";
  PUT '[SELECT("R211C1")]';
  PUT "[INSERT.PICTURE(""&OUTG.\&FILE.9.png"")]";
  PUT '[SELECT("R234C1")]';
  PUT "[INSERT.PICTURE(""&OUTG.\&FILE.10.png"")]";
  PUT '[SELECT("R257C1")]';
  PUT "[INSERT.PICTURE(""&OUTG.\&FILE.11.png"")]";
  PUT '[SELECT("R280C1")]';
  PUT "[INSERT.PICTURE(""&OUTG.\&FILE.12.png"")]";
RUN;

*** Font;
FILENAME SYS DDE 'EXCEL|SYSTEM';

DATA _NULL_;
   FILE SYS;
   PUT "[WORKBOOK.ACTIVATE(""[JSH201609_STAT_RES_&FILE..xlsx]&FILE."")]";
   PUT '[SELECT("R1")]';
   PUT '[FONT.PROPERTIES("ＭＳ 明朝",,11)]';
   PUT '[FONT.PROPERTIES("Times New Roman",,11)]';

   PUT '[SELECT("R2:R1048576")]';
   PUT '[FONT.PROPERTIES("ＭＳ 明朝",,9)]';
   PUT '[FONT.PROPERTIES("Times New Roman",,9)]';
RUN;

*** Footnote;
DATA TMP;
   RUNTIME = TRIM(TRANSLATE(PUT(DATE(),YYMMDD10.),"/","-"));
RUN;

DATA _NULL_;
   FILE SYS;
   SET TMP ;
   PUT "[WORKBOOK.ACTIVATE(%BQUOTE("[JSH201609_STAT_RES_&FILE..xlsx]&FILE."))]";
   PUT '[PAGE.SETUP(, "&C &""Times New Roman"" &8 &P/&N &R &""Times New Roman"" &8 ' RUNTIME '")]';
   PUT '[SELECT("R1C1")]';
RUN;

*** Close;
DATA _NULL_;
   FILE SYS;
   PUT "[WORKBOOK.ACTIVATE(""[JSH201609_STAT_RES_&FILE..xlsx]&FILE."")]";
   PUT '[SELECT("R1C1")]';
   PUT '[ERROR(FALSE)]';
   PUT "[SAVE.AS(""&OUT.\JSH201609_STAT_RES_&FILE..xlsx"")]";
   PUT '[CLOSE("FALSE")]';
   PUT '[QUIT()]';
RUN;

%STAT_FIN;

/*** END ***/
