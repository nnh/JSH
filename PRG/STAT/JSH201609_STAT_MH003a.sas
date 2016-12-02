**********************************************************************;
* Project           : JSH201609
*
* Program name      : JSH201609_STAT_MH003a.sas
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
%LET FILE = MH003a;

%INCLUDE "&_PATH2.\JSH201609_STAT_LIBNAME.sas";

/*** Template Open ***/
%XLSOPEN(JSH201609_STAT_RES_&FILE..xlsx);

/*** CSV read ***/
PROC IMPORT OUT= EXT
  DATAFILE="&EXT.\Japanese population - Data.csv"
  DBMS=CSV REPLACE;
  GETNAMES=YES;
  DATAROW=2;
  GUESSINGROWS=2000; 
RUN; 

DATA  EXT1;
  SET  EXT;
  TRTPN = 1;
  OUTPUT;
  TRTPN = SCSTRESC + 1;
  OUTPUT;
RUN ;

PROC SORT DATA=EXT1; BY TRTPN; RUN ;

DATA  EXT2;
  DROP SCSTRESC;
  RETAIN POPVAL;
  SET  EXT1;
  BY  TRTPN;
  IF  FIRST.TRTPN=1 THEN POPVAL = 0;
  POPVAL=POPVAL+POPULAT;
  IF  LAST.TRTPN=1 THEN OUTPUT;
RUN ;

%MACRO LOOP(YR);

  /*** ADS read ***/
  %MACRO DS_READ(LIB,DS);
    DATA  &DS.;
      SET  &LIB..&DS.;
      FORMAT _ALL_;
      INFORMAT _ALL_;
      %IF  &YR. = 2012 %THEN %DO;
        IF SCAN(MHSTDTC,1,"/") IN("2012");
      %END ;
      %IF  &YR. = 2013 %THEN %DO;
        IF SCAN(MHSTDTC,1,"/") IN("2013");
      %END ;
      %IF  &YR. = 2014 %THEN %DO;
        IF SCAN(MHSTDTC,1,"/") IN("2014");
      %END ;
      %IF  &YR. = 2015 %THEN %DO;
        IF SCAN(MHSTDTC,1,"/") IN("2015");
      %END ;
      %IF  &YR. = ALL %THEN %DO;
        IF SCAN(MHSTDTC,1,"/") IN("2012","2013","2014","2015");
      %END ;
    RUN ;
  %MEND ;
  %DS_READ(LIBADS,ADS);

  DATA  MH01;
    SET  ADS;
    TRTPN=1;
    OUTPUT;
    TRTPN=AREA+1;
    OUTPUT;
  RUN ;

  DATA  MAIN;
    SET  MH01;
    CNT=1;
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

    PROC SORT DATA=MRG; BY TRTPN; RUN ;

    DATA  MRG;
      MERGE  MRG EXT2;
      BY  TRTPN;
    RUN ;

    DATA  MRG;
      SET  MRG;
      N3=ROUND((N1/POPVAL*100000)/100000,0.01);
    RUN ;

    DATA WORK.OUT&DS ;
      FORMAT &WHE VAR1 - VAR47;
      MERGE MRG ( WHERE = ( TRTPN = 1  ) RENAME = ( N3 = VAR1  ) )
            MRG ( WHERE = ( TRTPN = 2  ) RENAME = ( N3 = VAR2  ) ) 
            MRG ( WHERE = ( TRTPN = 3  ) RENAME = ( N3 = VAR3  ) )
            MRG ( WHERE = ( TRTPN = 4  ) RENAME = ( N3 = VAR4  ) )
            MRG ( WHERE = ( TRTPN = 5  ) RENAME = ( N3 = VAR5  ) )
            MRG ( WHERE = ( TRTPN = 6  ) RENAME = ( N3 = VAR6  ) )
            MRG ( WHERE = ( TRTPN = 7  ) RENAME = ( N3 = VAR7  ) )
            MRG ( WHERE = ( TRTPN = 8  ) RENAME = ( N3 = VAR8  ) )
            MRG ( WHERE = ( TRTPN = 9  ) RENAME = ( N3 = VAR9  ) )
            MRG ( WHERE = ( TRTPN = 10 ) RENAME = ( N3 = VAR10 ) )
            MRG ( WHERE = ( TRTPN = 11 ) RENAME = ( N3 = VAR11 ) )
            MRG ( WHERE = ( TRTPN = 12 ) RENAME = ( N3 = VAR12 ) )
            MRG ( WHERE = ( TRTPN = 13 ) RENAME = ( N3 = VAR13 ) )
            MRG ( WHERE = ( TRTPN = 14 ) RENAME = ( N3 = VAR14 ) )
            MRG ( WHERE = ( TRTPN = 15 ) RENAME = ( N3 = VAR15 ) )
            MRG ( WHERE = ( TRTPN = 16 ) RENAME = ( N3 = VAR16 ) )
            MRG ( WHERE = ( TRTPN = 17 ) RENAME = ( N3 = VAR17 ) )
            MRG ( WHERE = ( TRTPN = 18 ) RENAME = ( N3 = VAR18 ) )
            MRG ( WHERE = ( TRTPN = 19 ) RENAME = ( N3 = VAR19 ) )
            MRG ( WHERE = ( TRTPN = 20 ) RENAME = ( N3 = VAR20 ) )
            MRG ( WHERE = ( TRTPN = 21 ) RENAME = ( N3 = VAR21 ) )
            MRG ( WHERE = ( TRTPN = 22 ) RENAME = ( N3 = VAR22 ) )
            MRG ( WHERE = ( TRTPN = 23 ) RENAME = ( N3 = VAR23 ) )
            MRG ( WHERE = ( TRTPN = 24 ) RENAME = ( N3 = VAR24 ) )
            MRG ( WHERE = ( TRTPN = 25 ) RENAME = ( N3 = VAR25 ) )
            MRG ( WHERE = ( TRTPN = 26 ) RENAME = ( N3 = VAR26 ) )
            MRG ( WHERE = ( TRTPN = 27 ) RENAME = ( N3 = VAR27 ) )
            MRG ( WHERE = ( TRTPN = 28 ) RENAME = ( N3 = VAR28 ) )
            MRG ( WHERE = ( TRTPN = 29 ) RENAME = ( N3 = VAR29 ) )
            MRG ( WHERE = ( TRTPN = 30 ) RENAME = ( N3 = VAR30 ) )
            MRG ( WHERE = ( TRTPN = 31 ) RENAME = ( N3 = VAR31 ) )
            MRG ( WHERE = ( TRTPN = 32 ) RENAME = ( N3 = VAR32 ) )
            MRG ( WHERE = ( TRTPN = 33 ) RENAME = ( N3 = VAR33 ) )
            MRG ( WHERE = ( TRTPN = 34 ) RENAME = ( N3 = VAR34 ) )
            MRG ( WHERE = ( TRTPN = 35 ) RENAME = ( N3 = VAR35 ) )
            MRG ( WHERE = ( TRTPN = 36 ) RENAME = ( N3 = VAR36 ) )
            MRG ( WHERE = ( TRTPN = 37 ) RENAME = ( N3 = VAR37 ) )
            MRG ( WHERE = ( TRTPN = 38 ) RENAME = ( N3 = VAR38 ) )
            MRG ( WHERE = ( TRTPN = 39 ) RENAME = ( N3 = VAR39 ) )
            MRG ( WHERE = ( TRTPN = 40 ) RENAME = ( N3 = VAR40 ) )
            MRG ( WHERE = ( TRTPN = 41 ) RENAME = ( N3 = VAR41 ) )
            MRG ( WHERE = ( TRTPN = 42 ) RENAME = ( N3 = VAR42 ) )
            MRG ( WHERE = ( TRTPN = 43 ) RENAME = ( N3 = VAR43 ) )
            MRG ( WHERE = ( TRTPN = 44 ) RENAME = ( N3 = VAR44 ) )
            MRG ( WHERE = ( TRTPN = 45 ) RENAME = ( N3 = VAR45 ) )
            MRG ( WHERE = ( TRTPN = 46 ) RENAME = ( N3 = VAR46 ) )
            MRG ( WHERE = ( TRTPN = 47 ) RENAME = ( N3 = VAR47 ) )
            MRG ( WHERE = ( TRTPN = 48 ) RENAME = ( N3 = VAR48 ) );
      %IF &DS ^= 1 %THEN BY &WHE ; ;
      ARRAY BEF(*) VAR1-VAR48 ;
      DO I = 1 TO DIM( BEF ) ;
        IF BEF(I) = . THEN BEF(I) = 0 ;
      END ;
    RUN ;
  %MEND ;

  %MH( %STR( MHGRPCOD MHGRPTERM ) , 2 )
  %MH( %STR( MHGRPCOD MHGRPTERM MHDECOD MHTERM ) , 3 )

  DATA  MH02;
    SET  OUT2 OUT3(DROP=MHGRPTERM);
    OUT1 =STRIP(MHGRPTERM);
    OUT2 =STRIP(MHTERM);
    OUT3 =PUT(VAR1 ,BEST8.);
    OUT4 =PUT(VAR2 ,BEST8.);
    OUT5 =PUT(VAR3 ,BEST8.);
    OUT6 =PUT(VAR4 ,BEST8.);
    OUT7 =PUT(VAR5 ,BEST8.);
    OUT8 =PUT(VAR6 ,BEST8.);
    OUT9 =PUT(VAR7 ,BEST8.);
    OUT10=PUT(VAR8 ,BEST8.);
    OUT11=PUT(VAR9 ,BEST8.);
    OUT12=PUT(VAR10,BEST8.);
    OUT13=PUT(VAR11,BEST8.);
    OUT14=PUT(VAR12,BEST8.);
    OUT15=PUT(VAR13,BEST8.);
    OUT16=PUT(VAR14,BEST8.);
    OUT17=PUT(VAR15,BEST8.);
    OUT18=PUT(VAR16,BEST8.);
    OUT19=PUT(VAR17,BEST8.);
    OUT20=PUT(VAR18,BEST8.);
    OUT21=PUT(VAR19,BEST8.);
    OUT22=PUT(VAR20,BEST8.);
    OUT23=PUT(VAR21,BEST8.);
    OUT24=PUT(VAR22,BEST8.);
    OUT25=PUT(VAR23,BEST8.);
    OUT26=PUT(VAR24,BEST8.);
    OUT27=PUT(VAR25,BEST8.);
    OUT28=PUT(VAR26,BEST8.);
    OUT29=PUT(VAR27,BEST8.);
    OUT30=PUT(VAR28,BEST8.);
    OUT31=PUT(VAR29,BEST8.);
    OUT32=PUT(VAR30,BEST8.);
    OUT33=PUT(VAR31,BEST8.);
    OUT34=PUT(VAR32,BEST8.);
    OUT35=PUT(VAR33,BEST8.);
    OUT36=PUT(VAR34,BEST8.);
    OUT37=PUT(VAR35,BEST8.);
    OUT38=PUT(VAR36,BEST8.);
    OUT39=PUT(VAR37,BEST8.);
    OUT40=PUT(VAR38,BEST8.);
    OUT41=PUT(VAR39,BEST8.);
    OUT42=PUT(VAR40,BEST8.);
    OUT43=PUT(VAR41,BEST8.);
    OUT44=PUT(VAR42,BEST8.);
    OUT45=PUT(VAR43,BEST8.);
    OUT46=PUT(VAR44,BEST8.);
    OUT47=PUT(VAR45,BEST8.);
    OUT48=PUT(VAR46,BEST8.);
    OUT49=PUT(VAR47,BEST8.);
    OUT50=PUT(VAR48,BEST8.);
    PROC SORT ;
    BY MHGRPCOD DESCENDING VAR1 MHDECOD ;
  RUN ;

  DATA  &FILE.;
    SET  MH02;
    KEEP MHGRPCOD VAR1 MHDECOD OUT1-OUT50;
  RUN ;

%MEND LOOP;
%LOOP(2012);
/*%LOOP(2013);*/
/*%LOOP(2014);*/
/*%LOOP(2015);*/
/*%LOOP(ALL);*/

PROC SORT DATA = &FILE.;
  BY MHGRPCOD DESCENDING VAR1 MHDECOD ;
RUN ;

/*** Excel Output ***/
%LET STROW = 6;                         
%LET PREROW = %EVAL(&STROW. -1);

/*OBS*/
PROC SQL NOPRINT;
   SELECT COUNT (*) INTO:OBS FROM &FILE.;
QUIT;

FILENAME SYS DDE 'EXCEL|SYSTEM';
DATA _NULL_;
   FILE SYS;
   PUT "[WORKBOOK.ACTIVATE(""[JSH201609_STAT_RES_&FILE..xlsx]&FILE."")]";
   PUT "[SELECT(%BQUOTE("R&STROW.:R99999"))]";
   PUT '[EDIT.DELETE(3)]';
   PUT '[SELECT("R1C1")]';
RUN;


%MACRO XLSOUT01(SHT,RANGE,DS,VAR,JDG);

   FILENAME XLS DDE "EXCEL |\\[JSH201609_STAT_RES_&FILE..xlsx]&FILE.!&RANGE";

   DATA _NULL_;
      FILE XLS NOTAB LRECL=10000 dsd dlm='09'x;
      SET &DS.;
      DMY = "";
      &JDG.;
      PUT &VAR.;
   RUN;

%MEND;

%XLSOUT01(&FILE.,R&STROW.C1:R%EVAL(&PREROW.+&OBS.)C50,&FILE.,OUT1-OUT50);

*** LINE;
FILENAME SYS DDE 'EXCEL|SYSTEM';
%MACRO LINE(LINEST=);
  DATA _NULL_;
    FILE SYS;
    SET &FILE.;
    PUT "[WORKBOOK.ACTIVATE(""[JSH201609_STAT_RES_&FILE..xlsx]&FILE."")]";
    ROW = _N_ + &STROW. - 2 ;
    IF OUT1 ^= "" THEN DO;
        PUT "[SELECT(""R" ROW  +(-1) "C1:R" ROW  +(-1) "C50"")]";
        PUT "[BORDER(,,,,1)]";
    END;
    PUT "[SELECT(""R&LINEST.C1:R&LINEST.C50"")]";
    PUT "[BORDER(,,,,1)]";
  RUN;
%MEND;
%LINE(LINEST=%EVAL(&PREROW.+(&OBS.)));

*** Font;
FILENAME SYS DDE 'EXCEL|SYSTEM';

DATA _NULL_;
   FILE SYS;
   PUT "[WORKBOOK.ACTIVATE(""[JSH201609_STAT_RES_&FILE..xlsx]&FILE."")]";
   PUT '[SELECT("R1")]';
   PUT '[FONT.PROPERTIES("‚l‚r –¾’©",,11)]';
   PUT '[FONT.PROPERTIES("Times New Roman",,11)]';

   PUT '[SELECT("R2:R1048576")]';
   PUT '[FONT.PROPERTIES("‚l‚r –¾’©",,9)]';
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
