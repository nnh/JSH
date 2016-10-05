**********************************************************************;
* Project           : JSH201609
*
* Program name      : JSH201609_STAT_MH003.sas
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
%LET FILE = MH003;

%INCLUDE "&_PATH2.\JSH201609_STAT_LIBNAME.sas";

/*** Template Open ***/
%XLSOPEN(JSH201609_STAT_RES_&FILE..xlsx);

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
    TRTPN=AGECAT2N+1;
    OUTPUT;
    TRTPN=SEX+6;
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

    DATA WORK.OUT&DS ;
      FORMAT &WHE VAR1 - VAR7;
      MERGE MRG ( WHERE = ( TRTPN = 1 ) RENAME = ( N1 = VAR1  ) )
            MRG ( WHERE = ( TRTPN = 2 ) RENAME = ( N1 = VAR2  ) ) 
            MRG ( WHERE = ( TRTPN = 3 ) RENAME = ( N1 = VAR3  ) )
            MRG ( WHERE = ( TRTPN = 4 ) RENAME = ( N1 = VAR4  ) )
            MRG ( WHERE = ( TRTPN = 5 ) RENAME = ( N1 = VAR5  ) )
            MRG ( WHERE = ( TRTPN = 6 ) RENAME = ( N1 = VAR6  ) )
            MRG ( WHERE = ( TRTPN = 7 ) RENAME = ( N1 = VAR7  ) );
      %IF &DS ^= 1 %THEN BY &WHE ; ;
      ARRAY BEF(*) VAR1-VAR7 ;
      DO I = 1 TO DIM( BEF ) ;
        IF BEF(I) = . THEN BEF(I) = 0 ;
      END ;
    RUN ;
  %MEND ;

  %MH( %STR( MHGRPCOD MHGRPTERM ) , 2 )
  %MH( %STR( MHGRPCOD MHGRPTERM MHDECOD MHTERM ) , 3 )

  DATA  MH02;
    SET  OUT2 OUT3(DROP=MHGRPTERM);
    OUT1=STRIP(MHGRPTERM);
    OUT2=STRIP(MHTERM);
    OUT3=PUT(VAR1,BEST8.);
    OUT4=PUT(VAR2,BEST8.);
    OUT5=PUT(VAR3,BEST8.);
    OUT6=PUT(VAR4,BEST8.);
    OUT7=PUT(VAR5,BEST8.);
    OUT8=PUT(VAR6,BEST8.);
    OUT9=PUT(VAR7,BEST8.);
    PROC SORT ;
    BY MHGRPCOD DESCENDING VAR1 MHDECOD ;
  RUN ;

  DATA  &FILE._&YR.;
    SET  MH02;
    KEEP MHGRPCOD VAR1 MHDECOD OUT1-OUT9;
  RUN ;

%MEND LOOP;
%LOOP(2012);
%LOOP(2013);
%LOOP(2014);
%LOOP(2015);
%LOOP(ALL);

PROC SORT DATA=&FILE._2012;  BY  MHGRPCOD MHDECOD; RUN ;
PROC SORT DATA=&FILE._2013;  BY  MHGRPCOD MHDECOD; RUN ;
PROC SORT DATA=&FILE._2014;  BY  MHGRPCOD MHDECOD; RUN ;
PROC SORT DATA=&FILE._2015;  BY  MHGRPCOD MHDECOD; RUN ;
PROC SORT DATA=&FILE._ALL;   BY  MHGRPCOD MHDECOD; RUN ;

DATA  &FILE.;
  MERGE  &FILE._2012(DROP=VAR1)
         &FILE._2013(DROP=VAR1 RENAME=(OUT3=OUT10 OUT4=OUT11 OUT5=OUT12 OUT6=OUT13 OUT7=OUT14 OUT8=OUT15 OUT9=OUT16))
         &FILE._2014(DROP=VAR1 RENAME=(OUT3=OUT17 OUT4=OUT18 OUT5=OUT19 OUT6=OUT20 OUT7=OUT21 OUT8=OUT22 OUT9=OUT23))
         &FILE._2015(DROP=VAR1 RENAME=(OUT3=OUT24 OUT4=OUT25 OUT5=OUT26 OUT6=OUT27 OUT7=OUT28 OUT8=OUT29 OUT9=OUT30))
         &FILE._ALL(RENAME=(OUT3=OUT31 OUT4=OUT32 OUT5=OUT33 OUT6=OUT34 OUT7=OUT35 OUT8=OUT36 OUT9=OUT37));
  BY  MHGRPCOD MHDECOD;
  ARRAY BEF(*) OUT3-OUT37 ;
  DO I = 1 TO DIM( BEF ) ;
    IF BEF(I) = "" THEN BEF(I) = "0" ;
  END ;
RUN ;

PROC SORT DATA = &FILE.;
  BY MHGRPCOD DESCENDING VAR1 MHDECOD ;
RUN ;

/*** Excel Output ***/
%LET STROW = 7;                         
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

%XLSOUT01(&FILE.,R&STROW.C1:R%EVAL(&PREROW.+&OBS.)C37,&FILE.,OUT1-OUT37);

*** LINE;
FILENAME SYS DDE 'EXCEL|SYSTEM';
%MACRO LINE(LINEST=);
  DATA _NULL_;
    FILE SYS;
    SET &FILE.;
    PUT "[WORKBOOK.ACTIVATE(""[JSH201609_STAT_RES_&FILE..xlsx]&FILE."")]";
    ROW = _N_ + &STROW. - 2 ;
    IF OUT1 ^= "" THEN DO;
        PUT "[SELECT(""R" ROW  +(-1) "C1:R" ROW  +(-1) "C37"")]";
        PUT "[BORDER(,,,,1)]";
    END;
    PUT "[SELECT(""R&LINEST.C1:R&LINEST.C37"")]";
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
