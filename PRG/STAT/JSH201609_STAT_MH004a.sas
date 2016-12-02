**********************************************************************;
* Project           : JSH201609
*
* Program name      : JSH201609_STAT_MH004a.sas
*
* Author            : MATSUO YAMAMOTO
*
* Date created      : 20160912
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
%LET FILE = MH004a;

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

/*** CSV read ***/
PROC IMPORT OUT= EXT
  DATAFILE="&EXT.\Japanese population - Data.csv"
  DBMS=CSV REPLACE;
  GETNAMES=YES;
  DATAROW=2;
  GUESSINGROWS=2000; 
RUN; 

/* Create a data set of the boundaries for the states */
DATA JAPAN;
   SET MAPSGFK.JAPAN;
   BY ID SEGMENT;
   RETAIN X1 Y1;

   /* Close each polygon */
   IF FIRST.SEGMENT THEN DO;
      X1=X; Y1=Y;
   END;
   IF LAST.SEGMENT THEN DO;
      OUTPUT;
      X=X1; Y=Y1;
      OUTPUT;
      X=.; Y=.;
   END;
   OUTPUT;
   DROP X1 Y1;
RUN;

/* Create a response data to represent in the map areas */
DATA  MAIN;
  SET  ADS;
  CNT=1;
  TRTPN=1;
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
    FORMAT &WHE VAR1 ;
    SET MRG ( WHERE = ( TRTPN = 1 ) RENAME = ( N1 = VAR1 ) );
    %IF &DS ^= 1 %THEN BY &WHE ; ;
    ARRAY BEF(*) VAR1 ;
    DO I = 1 TO DIM( BEF ) ;
      IF BEF(I) = . THEN BEF(I) = 0 ;
    END ;
  RUN ;
%MEND ;

%MH( %STR(MHGRPCOD MHGRPTERM ) , 2 )
%MH( %STR(AREA AREA2 MHGRPCOD MHGRPTERM ) , 3 )
%MH( %STR(AREA2 MHGRPCOD MHGRPTERM ) , 4 )

PROC SORT DATA=OUT3 ; BY MHGRPCOD; RUN ;

DATA  OUT5;
  MERGE  OUT2(RENAME=(VAR1=VAR2)) OUT3 ;
  BY  MHGRPCOD ;
RUN ;

PROC SORT DATA=OUT5 ; BY AREA2 MHGRPCOD; RUN ;

DATA  OUT1;
  MERGE  OUT5 OUT4(RENAME=(VAR1=VAR3));
  BY  AREA2 MHGRPCOD ;
/*  PCT=ROUND((VAR3/VAR2)*100,0.1);*/
RUN ;

DATA  RESPONSE;
  LENGTH ID1 $15.;
  SET  OUT1;
  ID1=CAT('JP-',PUT(AREA,Z2.));
  IF  AREA=0 THEN DELETE;
RUN ;

/* add 20161009*/
DATA  POP;
  SET  EXT;
  RENAME SCSTRESC = AREA ;
RUN ;

DATA  POP;
  SET  POP;
  IF  AREA = 1 THEN AREA2 = 1;
  ELSE IF 2 <= AREA <= 7 THEN AREA2 = 2;
  ELSE IF 8 <= AREA <=15 OR 19<= AREA <=20 THEN AREA2 = 3;
  ELSE IF 16<= AREA <=18 OR 21<= AREA <=24 THEN AREA2 = 4;
  ELSE IF 25<= AREA <=30 THEN AREA2 = 5;
  ELSE IF 31<= AREA <=39 THEN AREA2 = 6;
  ELSE IF 40<= AREA <=47 THEN AREA2 = 7;
RUN ;

PROC SORT DATA=POP; BY AREA2; RUN ;

DATA  POP; 
  DROP POPULAT AREA;
  RETAIN POPULAT2;
  SET  POP;
  BY  AREA2;
  IF  FIRST.AREA2=1 THEN POPULAT2=0;
  POPULAT2=POPULAT2+POPULAT;
  IF  LAST.AREA2=1 THEN OUTPUT;
RUN ;

PROC SORT DATA=POP; BY AREA2; RUN ;
PROC SORT DATA=RESPONSE; BY AREA2; RUN ;

DATA  RESPONSE;
  MERGE  POP RESPONSE;
  BY  AREA2;
  PCT=ROUND((VAR3/POPULAT2*100000)/100000,0.1);
RUN ;

/* end */

PROC SORT DATA=RESPONSE;
   BY ID1;
RUN;

/* Define a format for the response data */
PROC FORMAT;
   VALUE MAPFMT
        .='No Data'
  low-5  ='5 and under'
  5.1-10 ='Between 5 and 10'
  10.1-15='Between 10 and 15'
  15.1-20='Between 15 and 20'
  20.1-high='Over 20';
RUN;

/* Define an attribute map for the response data */
DATA ATTRMAP;
   ID = 'maparea';
   TEXTCOLOR='black';
   INPUT VALUE $20. @22 FILLCOLOR $;
   DATALINES;
No Data               beige
5 and under           blue
Between 5 and 10      green
Between 10 and 15     yellow
Between 15 and 20     orange
Over 20               red
;
run;

/* Calculate the center of each polgyon
   to be used to place a label */
%CENTROID(JAPAN,CENTERS,ID1);

%MACRO MAP(TIT,ID);

  /* Combine the response data with the map data */
  DATA MAP&ID.;
     MERGE JAPAN RESPONSE(WHERE=(MHGRPCOD = &ID.));
     BY ID1;
  RUN;

  PROC SORT DATA=MAP&ID.; 
     BY PCT;
  RUN;

  /* Define a label to be placed at the center of each polygon */
  DATA MAP&ID.;
     SET MAP&ID. CENTERS(RENAME=(X=XCEN Y=YCEN) IN=A);
     /* Define the variable to contain the label for each polygon */
     IF A THEN LABEL="";
  RUN;

  ODS GRAPHICS ON / HEIGHT = 9CM WIDTH = 12CM IMAGENAME = "&FILE.&ID."
    OUTPUTFMT = PNG RESET = INDEX   ANTIALIASMAX=227600;
  ODS LISTING GPATH = "&OUTG." IMAGE_DPI = 300 ;


  TITLE "&TIT.";
  PROC SGPLOT DATA=MAP&ID. DATTRMAP=ATTRMAP ;
     FORMAT PCT MAPFMT.;
     /* Draw each polygon */
     POLYGON X=X Y=Y ID=ID1 / GROUP=PCT ATTRID=MAPAREA
             FILL FILLATTRS=(TRANSPARENCY=0.5)
             DATASKIN=MATTE NAME='poly';
    /* Label each polygon with the LABEL variable value */
     SCATTER X=XCEN Y=YCEN / MARKERCHAR=LABEL;
     KEYLEGEND 'poly' / TITLE='per 100,000 population: ';
     XAXIS OFFSETMIN=0.01 OFFSETMAX=0 DISPLAY=NONE;
     YAXIS OFFSETMIN=0.01 OFFSETMAX=0 DISPLAY=NONE;
  RUN;

%MEND;
%MAP(œ‘‘B«Žîá‡,1);
%MAP(‚o‚c‚f‚e‚q‚`A‚o‚c‚f‚e‚q‚aA‚e‚f‚e‚q‚PˆÙíÇ,2);
%MAP(œ‘ˆÙŒ`¬Eœ‘‘B«Žîá‡,3);
%MAP(œ‘ˆÙŒ`¬ÇŒóŒQ,4);
%MAP(‹}«œ‘«”’ŒŒ•a‚¨‚æ‚ÑŠÖ˜AŽîá‡,5);
%MAP(Œn“•s–¾‹}«”’ŒŒ•a,6);
%MAP(‘O‹ìƒŠƒ“ƒp‹…ŒnŽîá‡,7);
%MAP(¬n‚a×–EŽîá‡iƒŠƒ“ƒpŽîEœ‘Žîj,8);
%MAP(¬n‚sE‚m‚j×–EŽîá‡,9);
%MAP(ƒzƒWƒLƒ“ƒŠƒ“ƒpŽî,10);
%MAP(‘gD‹…EŽ÷ó×–EŽîá‡,11);
%MAP(ˆÚAŒãƒŠƒ“ƒp‘B«Ž¾Š³,12);

/*** Excel Output ***/

FILENAME SYS DDE "EXCEL | SYSTEM " ;
DATA _NULL_;
  FILE SYS;
  PUT '[SELECT("R4C1")]';
  PUT "[INSERT.PICTURE(""&OUTG.\&FILE.1.png"")]";
  PUT '[SELECT("R27C1")]';
  PUT "[INSERT.PICTURE(""&OUTG.\&FILE.2.png"")]";
  PUT '[SELECT("R50C1")]';PUT '[SET.PAGE.BREAK()]';
  PUT "[INSERT.PICTURE(""&OUTG.\&FILE.3.png"")]";
  PUT '[SELECT("R73C1")]';
  PUT "[INSERT.PICTURE(""&OUTG.\&FILE.4.png"")]";
  PUT '[SELECT("R96C1")]';PUT '[SET.PAGE.BREAK()]';
  PUT "[INSERT.PICTURE(""&OUTG.\&FILE.5.png"")]";
  PUT '[SELECT("R119C1")]';
  PUT "[INSERT.PICTURE(""&OUTG.\&FILE.6.png"")]";
  PUT '[SELECT("R142C1")]';PUT '[SET.PAGE.BREAK()]';
  PUT "[INSERT.PICTURE(""&OUTG.\&FILE.7.png"")]";
  PUT '[SELECT("R165C1")]';
  PUT "[INSERT.PICTURE(""&OUTG.\&FILE.8.png"")]";
  PUT '[SELECT("R188C1")]';PUT '[SET.PAGE.BREAK()]';
  PUT "[INSERT.PICTURE(""&OUTG.\&FILE.9.png"")]";
  PUT '[SELECT("R211C1")]';
  PUT "[INSERT.PICTURE(""&OUTG.\&FILE.10.png"")]";
  PUT '[SELECT("R234C1")]';PUT '[SET.PAGE.BREAK()]';
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
