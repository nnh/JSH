**********************************************************************;
* Project           : JSH201609
*
* Program name      : JSH201609_STAT_MH006b.sas
*
* Author            : MATSUO YAMAMOTO
*
* Date created      : 20160909
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
%LET FILE = MH006b;

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

PROC FORMAT;
 VALUE YRF  1='2012'
            2='2013'
            3='2014'
            4='2015'
            5='2016'
;
RUN ;

DATA  MAIN;
  SET  ADS;
  TRTPN=1;
  CNT=1;
  YEAR=STRIP(SCAN(PUT(MHSTDTC,YYMMDD10.),1,"-"));
  YEARCATN = INPUT(YEAR,BEST.)-2011;
  FORMAT YEARCATN YRF.;
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
    FORMAT &WHE VAR1;
    SET MRG ( WHERE = ( TRTPN = 1 ) RENAME = ( N1 = VAR1  ) );
    %IF &DS ^= 1 %THEN BY &WHE ; ;
    ARRAY BEF(*) VAR1 ;
    DO I = 1 TO DIM( BEF ) ;
      IF BEF(I) = . THEN BEF(I) = 0 ;
    END ;
  RUN ;
%MEND ;

%MH( %STR(YEARCATN MHGRPCOD MHGRPTERM ) , 2 )
%MH( %STR(YEARCATN MHGRPCOD MHGRPTERM MHDECOD MHTERM ) , 3 )

DATA  OUT1;
  MERGE  OUT2(RENAME=(VAR1=VAR2)) OUT3;
  BY  YEARCATN MHGRPCOD MHGRPTERM;
  PCT=(VAR1/VAR2)*100;
RUN ;

%MACRO VLINE(TIT,ID);
  DATA  VLINE&ID. ;
    LABEL MHTERM="疾患名(小分類)";
    SET  OUT1;
    IF  MHGRPCOD = &ID.;
  RUN ;

  ODS GRAPHICS ON / HEIGHT = 9CM WIDTH = 12CM IMAGENAME = "&FILE.&ID."
    OUTPUTFMT = PNG RESET = INDEX   ANTIALIASMAX=96100;
  ODS LISTING GPATH = "&OUTG." IMAGE_DPI = 300 ;

  TITLE "&TIT.";

  PROC SGPLOT DATA=VLINE&ID.;
    VLINE YEARCATN / RESPONSE =PCT GROUP =MHTERM
    GROUPDISPLAY=CLUSTER
    MARKERS ;
    XAXIS TYPE=DISCRETE OFFSETMIN=0.2 OFFSETMAX=0.2
    DISPLAY=(NOLABEL) ;
    YAXIS OFFSETMIN=0.2 OFFSETMAX=0.2
    LABEL="粗罹患率(%)";
    KEYLEGEND / LOCATION=OUTSIDE ;
  RUN ;

%MEND;

%VLINE(骨髄増殖性腫瘍,1);
%VLINE(ＰＤＧＦＲＡ、ＰＤＧＦＲＢ、ＦＧＦＲ１異常症,2);
%VLINE(骨髄異形成・骨髄増殖性腫瘍,3);
%VLINE(骨髄異形成症候群,4);
%VLINE(急性骨髄性白血病および関連腫瘍,5);
%VLINE(系統不明急性白血病,6);
%VLINE(前駆リンパ球系腫瘍,7);
%VLINE(成熟Ｂ細胞腫瘍（リンパ腫・骨髄腫）,8);
%VLINE(成熟Ｔ・ＮＫ細胞腫瘍,9);
%VLINE(ホジキンリンパ腫,10);
%VLINE(組織球・樹状細胞腫瘍,11);
%VLINE(移植後リンパ増殖性疾患,12);

/*** Excel Output ***/

FILENAME SYS DDE "EXCEL | SYSTEM " ;
DATA _NULL_;
  FILE SYS;
  PUT '[SELECT("R4C1")]';
  PUT "[INSERT.PICTURE(""&OUTG.\&FILE.1.png"")]";
  PUT '[SELECT("R27C1")]';
  PUT "[INSERT.PICTURE(""&OUTG.\&FILE.2.png"")]";
  PUT '[SELECT("R50C1")]';
  PUT "[INSERT.PICTURE(""&OUTG.\&FILE.3.png"")]";
  PUT '[SELECT("R73C1")]';
  PUT "[INSERT.PICTURE(""&OUTG.\&FILE.4.png"")]";
  PUT '[SELECT("R96C1")]';
  PUT "[INSERT.PICTURE(""&OUTG.\&FILE.5.png"")]";
  PUT '[SELECT("R119C1")]';
  PUT "[INSERT.PICTURE(""&OUTG.\&FILE.6.png"")]";
  PUT '[SELECT("R142C1")]';
  PUT "[INSERT.PICTURE(""&OUTG.\&FILE.7.png"")]";
  PUT '[SELECT("R165C1")]';
  PUT "[INSERT.PICTURE(""&OUTG.\&FILE.8.png"")]";
  PUT '[SELECT("R188C1")]';
  PUT "[INSERT.PICTURE(""&OUTG.\&FILE.9.png"")]";
  PUT '[SELECT("R211C1")]';
  PUT "[INSERT.PICTURE(""&OUTG.\&FILE.10.png"")]";
  PUT '[SELECT("R234C1")]';
  PUT "[INSERT.PICTURE(""&OUTG.\&FILE.11.png"")]";
  PUT '[SELECT("R257C1")]';
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
