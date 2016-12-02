**********************************************************************;
* Project           : JSH201609
*
* Program name      : JSH201609_STAT_MH001.sas
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
%LET FILE = MH001;

%INCLUDE "&_PATH2.\JSH201609_STAT_LIBNAME.sas";

/*** Template Open ***/
/*%XLSOPEN(JSH201609_STAT_RES_&FILE..xlsx);*/
%MACRO LOOP(AGECAT,GRP);

  /*** ADS read ***/
  %MACRO DS_READ(LIB,DS);
    DATA  &DS.;
      SET  &LIB..&DS.;
      FORMAT _ALL_;
      INFORMAT _ALL_;
      IF SCAN(MHSTDTC,1,"/") IN("2012","2013","2014","2015");
      %IF  &AGECAT=1 %THEN %DO;
        IF  AGECAT2N IN(1,2,3,4);
      %END ;
      %IF  &AGECAT=2 %THEN %DO;
        IF  AGECAT2N = 1;
      %END ;
      %IF  &AGECAT=3 %THEN %DO;
        IF  AGECAT2N = 2;
      %END ;
      %IF  &AGECAT=4 %THEN %DO;
        IF  AGECAT2N = 3;
      %END ;
      %IF  &AGECAT=5 %THEN %DO;
        IF  AGECAT2N = 4;
      %END ;
    RUN ;
  %MEND ;
  %DS_READ(LIBADS,ADS);

  *** ALL;
  ODS GRAPHICS ON / HEIGHT = 9CM WIDTH = 12CM IMAGENAME = "&FILE.x0x&AGECAT.x&GRP."
    OUTPUTFMT = PNG RESET = INDEX   ANTIALIASMAX=96100 ;
  ODS LISTING GPATH = "&OUTG.\PIE" IMAGE_DPI = 300 STYLE=ANALYSIS; 

  PROC TEMPLATE;
    DEFINE STATGRAPH MYPIECHART;
      DYNAMIC _X;
      BEGINGRAPH;
      ENTRYTITLE "Total(&GRP.)";
        LAYOUT REGION;
          PIECHART CATEGORY=_X 
            /   CATEGORYDIRECTION=CLOCKWISE
                START=90
                NAME="MHGRPTERM"
                DATALABELCONTENT=(PERCENT)
                DATALABELLOCATION=INSIDE
                LABELFITPOLICY=DROP
                OTHERSLICE=TRUE
                OTHERSLICEOPTS=(TYPE=MAXSLICES MAXSLICES=12);
                DISCRETELEGEND 'MHGRPTERM' ;
        ENDLAYOUT;
      ENDGRAPH;
    END;
  RUN;

  PROC SGRENDER DATA=ADS TEMPLATE=MYPIECHART;
    DYNAMIC _X='MHGRPTERM';
  RUN;

  *** SUB;

  %MACRO PIE(ID,TIT=&TERM.);
    %LET TERM=;

    DATA  PIE&ID.;
      SET  ADS;
      IF MHGRPCOD=&ID.;
      CALL SYMPUT('TERM',STRIP(MHGRPTERM));
    RUN ;

    ODS GRAPHICS ON / HEIGHT = 9CM WIDTH = 12CM IMAGENAME = "&FILE.&ID.x&AGECAT.x&GRP."
      OUTPUTFMT = PNG RESET = INDEX   ANTIALIASMAX=96100;
    ODS LISTING GPATH = "&OUTG.\PIE" IMAGE_DPI = 300  STYLE=ANALYSIS;

    PROC TEMPLATE;
      DEFINE STATGRAPH MYPIECHART;
        DYNAMIC _X;
        BEGINGRAPH;
          ENTRYTITLE "&TIT.(&GRP.)";
          LAYOUT REGION;
            PIECHART CATEGORY=_X
              /   CATEGORYDIRECTION=CLOCKWISE
                  START=90
                  NAME="MHTERM"
                  DATALABELCONTENT=(PERCENT)
                  DATALABELLOCATION=INSIDE
                  LABELFITPOLICY=DROP
                  OTHERSLICE=TRUE
                  OTHERSLICEOPTS=(TYPE=MAXSLICES MAXSLICES=12);
                  DISCRETELEGEND 'MHTERM' ;
          ENDLAYOUT;
        ENDGRAPH;
      END;
    RUN;

    PROC SGRENDER DATA=PIE&ID. TEMPLATE=MYPIECHART;
      DYNAMIC _X='MHTERM';
    RUN;

  %MEND;

  %PIE(1);
  %PIE(2);
  %PIE(3);
  %PIE(4);
  %PIE(5);
  %PIE(6);
  %PIE(7);
  %PIE(8);
  %PIE(9);
  %PIE(10);
  %PIE(11);
  %PIE(12);

%MEND;

%LOOP(1,all);
%LOOP(2,child);
%LOOP(3,AYA);
%LOOP(4,adult);
%LOOP(5,old);

/*** Excel Output ***/

/*FILENAME SYS DDE "EXCEL | SYSTEM " ;*/
/*DATA _NULL_;*/
/*  FILE SYS;*/
/*  PUT '[SELECT("R4C1")]';*/
/*  PUT "[INSERT.PICTURE(""&OUTG.\PIE\&FILE..png"")]";*/
/*  PUT '[SELECT("R27C1")]';*/
/*  PUT "[INSERT.PICTURE(""&OUTG.\PIE\&FILE.1.png"")]";*/
/*  PUT '[SELECT("R50C1")]';PUT '[SET.PAGE.BREAK()]';*/
/*  PUT "[INSERT.PICTURE(""&OUTG.\PIE\&FILE.2.png"")]";*/
/*  PUT '[SELECT("R73C1")]';*/
/*  PUT "[INSERT.PICTURE(""&OUTG.\PIE\&FILE.3.png"")]";*/
/*  PUT '[SELECT("R96C1")]';PUT '[SET.PAGE.BREAK()]';*/
/*  PUT "[INSERT.PICTURE(""&OUTG.\PIE\&FILE.4.png"")]";*/
/*  PUT '[SELECT("R119C1")]';*/
/*  PUT "[INSERT.PICTURE(""&OUTG.\PIE\&FILE.5.png"")]";*/
/*  PUT '[SELECT("R142C1")]';PUT '[SET.PAGE.BREAK()]';*/
/*  PUT "[INSERT.PICTURE(""&OUTG.\PIE\&FILE.6.png"")]";*/
/*  PUT '[SELECT("R165C1")]';*/
/*  PUT "[INSERT.PICTURE(""&OUTG.\PIE\&FILE.7.png"")]";*/
/*  PUT '[SELECT("R188C1")]';PUT '[SET.PAGE.BREAK()]';*/
/*  PUT "[INSERT.PICTURE(""&OUTG.\PIE\&FILE.8.png"")]";*/
/*  PUT '[SELECT("R211C1")]';*/
/*  PUT "[INSERT.PICTURE(""&OUTG.\PIE\&FILE.9.png"")]";*/
/*  PUT '[SELECT("R234C1")]';PUT '[SET.PAGE.BREAK()]';*/
/*  PUT "[INSERT.PICTURE(""&OUTG.\PIE\&FILE.10.png"")]";*/
/*  PUT '[SELECT("R257C1")]';*/
/*  PUT "[INSERT.PICTURE(""&OUTG.\PIE\&FILE.11.png"")]";*/
/*  PUT '[SELECT("R280C1")]';PUT '[SET.PAGE.BREAK()]';*/
/*  PUT "[INSERT.PICTURE(""&OUTG.\PIE\&FILE.12.png"")]";*/
/*RUN;*/
/**/
/**** Font;*/
/*FILENAME SYS DDE 'EXCEL|SYSTEM';*/
/**/
/*DATA _NULL_;*/
/*   FILE SYS;*/
/*   PUT "[WORKBOOK.ACTIVATE(""[JSH201609_STAT_RES_&FILE..xlsx]&FILE."")]";*/
/*   PUT '[SELECT("R1")]';*/
/*   PUT '[FONT.PROPERTIES("‚l‚r –¾’©",,11)]';*/
/*   PUT '[FONT.PROPERTIES("Times New Roman",,11)]';*/
/**/
/*   PUT '[SELECT("R2:R1048576")]';*/
/*   PUT '[FONT.PROPERTIES("‚l‚r –¾’©",,9)]';*/
/*   PUT '[FONT.PROPERTIES("Times New Roman",,9)]';*/
/*RUN;*/
/**/
/**** Footnote;*/
/*DATA TMP;*/
/*   RUNTIME = TRIM(TRANSLATE(PUT(DATE(),YYMMDD10.),"/","-"));*/
/*RUN;*/
/**/
/*DATA _NULL_;*/
/*   FILE SYS;*/
/*   SET TMP ;*/
/*   PUT "[WORKBOOK.ACTIVATE(%BQUOTE("[JSH201609_STAT_RES_&FILE..xlsx]&FILE."))]";*/
/*   PUT '[PAGE.SETUP(, "&C &""Times New Roman"" &8 &P/&N &R &""Times New Roman"" &8 ' RUNTIME '")]';*/
/*   PUT '[SELECT("R1C1")]';*/
/*RUN;*/
/**/
/**** Close;*/
/*DATA _NULL_;*/
/*   FILE SYS;*/
/*   PUT "[WORKBOOK.ACTIVATE(""[JSH201609_STAT_RES_&FILE..xlsx]&FILE."")]";*/
/*   PUT '[SELECT("R1C1")]';*/
/*   PUT '[ERROR(FALSE)]';*/
/*   PUT "[SAVE.AS(""&OUT.\JSH201609_STAT_RES_&FILE..xlsx"")]";*/
/*   PUT '[CLOSE("FALSE")]';*/
/*   PUT '[QUIT()]';*/
/*RUN;*/

%STAT_FIN;

/*** END ***/
