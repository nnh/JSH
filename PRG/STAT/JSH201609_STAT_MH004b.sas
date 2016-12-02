+**********************************************************************;
* Project           : JSH201609
*
* Program name      : JSH201609_STAT_MH004b.sas
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
%LET FILE = MH004b;

%INCLUDE "&_PATH2.\JSH201609_STAT_LIBNAME.sas";

/*** Template Open ***/
/*%XLSOPEN(JSH201609_STAT_RES_&FILE..xlsx);*/

/*** CSV read ***/
PROC IMPORT OUT= EXT
  DATAFILE="&EXT.\Japanese population - Data.csv"
  DBMS=CSV REPLACE;
  GETNAMES=YES;
  DATAROW=2;
  GUESSINGROWS=2000; 
RUN; 

/* Define an attribute map for the response data */
DATA ATTRMAP;
   ID = 'maparea';
   TEXTCOLOR='black';
   INPUT VALUE $30. @30 FILLCOLOR $;
   DATALINES;
No Data                       cx000000
Max*1/5 and under             cx0000ff
Between Max*1/5 and Max*2/5   cx00ff00
Between Max*2/5 and Max*3/5   cxffff00
Between Max*3/5 and Max*4/5   cxff9900
Over Max*4/5                  cxff0000
;
run;


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
  %MEND DS_READ;
  %DS_READ(LIBADS,ADS);

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
  %MEND MH ;

  %MH( %STR(MHGRPCOD MHGRPTERM MHDECOD MHTERM ) , 2 )
  %MH( %STR(AREA MHGRPCOD MHGRPTERM MHDECOD MHTERM ) , 3 )

  PROC SORT DATA=OUT2 ; BY MHDECOD; RUN ;
  PROC SORT DATA=OUT3 ; BY MHDECOD; RUN ;

  DATA  OUT1;
    MERGE  OUT2(RENAME=(VAR1=VAR2)) OUT3;
    BY  MHDECOD ;
  /*  PCT=ROUND((VAR1/VAR2)*100,0.1);*/
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

  PROC SORT DATA=POP; BY AREA; RUN ;
  PROC SORT DATA=RESPONSE; BY AREA; RUN ;

  DATA  RESPONSE;
    MERGE  POP RESPONSE;
    BY  AREA;
    PCT=(VAR1/POPULAT*100000)/100000;
  RUN ;

  PROC SORT DATA=RESPONSE; BY MHDECOD DECENDING PCT; RUN ; 

  DATA  RESPONSE;
    RETAIN REFVAL;
    SET  RESPONSE;
    BY  MHDECOD;
    IF  FIRST.MHDECOD=1 THEN REFVAL=PCT;
  RUN ;

  DATA  RESPONSE;
    SET RESPONSE;
    VAL = ROUND(PCT/REFVAL*100,0.1);
  RUN ;

  /* end */

  PROC SORT DATA=RESPONSE;
     BY ID1;
  RUN;

  /* Define a format for the response data */
  PROC FORMAT;
     VALUE MAPFMT
          .='No Data'
    low-20  ='Max*1/5 and under'
    20.1-40 ='Between Max*1/5 and Max*2/5'
    40.1-60='Between Max*2/5 and Max*3/5'
    60.1-80='Between Max*3/5 and Max*4/5'
    80.1-high='Over Max*4/5';
  RUN;

  /* Define an attribute map for the response data */
/*  DATA ATTRMAP;*/
/*     ID = 'maparea';*/
/*     TEXTCOLOR='black';*/
/*     INPUT VALUE $30. @30 FILLCOLOR $;*/
/*     DATALINES;*/
/*  No Data                       cx000000*/
/*  Max*1/5 and under             cx0000ff*/
/*  Between Max*1/5 and Max*2/5   cx00ff00*/
/*  Between Max*2/5 and Max*3/5   cxffff00*/
/*  Between Max*3/5 and Max*4/5   cxff9900*/
/*  Over Max*4/5                  cxff0000*/
/*  ;*/
/*  run;*/

  /* Calculate the center of each polgyon
     to be used to place a label */
  %CENTROID(JAPAN,CENTERS,ID1);

  %MACRO MAP(ID,TIT=&TERM.);
    %LET TERM=;

    DATA  RESP;
      SET  RESPONSE;
      IF  MHDECOD = &ID.;
      CALL SYMPUT('TERM',MHTERM);
      %PUT &TERM.;
    RUN ;

    /* Combine the response data with the map data */
    DATA MAP&ID.;
       MERGE JAPAN RESP;
       BY ID1;
    RUN;

    PROC SORT DATA=MAP&ID.; 
       BY VAL;
    RUN;

    /* Define a label to be placed at the center of each polygon */
    DATA MAP&ID.;
       SET MAP&ID. CENTERS(RENAME=(X=XCEN Y=YCEN) IN=A);
       /* Define the variable to contain the label for each polygon */
       IF A THEN LABEL="";
    RUN;

    ODS GRAPHICS ON / HEIGHT = 9CM WIDTH = 12CM IMAGENAME = "&FILE.x&ID.x&YR."
      OUTPUTFMT = PNG RESET = INDEX   ANTIALIASMAX=227600;
    ODS LISTING GPATH = "&OUTG.\MAP" IMAGE_DPI = 300 ;


    TITLE &TIT.;
    PROC SGPLOT DATA=MAP&ID. DATTRMAP=ATTRMAP ;
       FORMAT VAL MAPFMT.;
       /* Draw each polygon */
       POLYGON X=X Y=Y ID=ID1 / GROUP=VAL ATTRID=MAPAREA
               FILL FILLATTRS=(TRANSPARENCY=0.5)
               DATASKIN=MATTE NAME='poly';
      /* Label each polygon with the LABEL variable value */
       SCATTER X=XCEN Y=YCEN / MARKERCHAR=LABEL;
       KEYLEGEND 'poly' / TITLE='Relatrve Value of Maximum Value: ';
       XAXIS OFFSETMIN=0.01 OFFSETMAX=0 DISPLAY=NONE;
       YAXIS OFFSETMIN=0.01 OFFSETMAX=0 DISPLAY=NONE;
    RUN;

  %MEND MAP;
  %MAP(1);
  %MAP(2);
  %MAP(3);
  %MAP(4);
  %MAP(5);
  %MAP(6);
  %MAP(7);
  %MAP(8);
  %MAP(9);
  %MAP(10);
  %MAP(11);
  %MAP(12);
  %MAP(13);
  %MAP(14);
  %MAP(15);
  %MAP(16);
  %MAP(17);
  %MAP(18);
  %MAP(19);
  %MAP(20);
  %MAP(21);
  %MAP(22);
  %MAP(23);
  %MAP(24);
  %MAP(25);
  %MAP(26);
  %MAP(27);
  %MAP(28);
  %MAP(29);
  %MAP(30);
  %MAP(31);
  %MAP(32);
  %MAP(33);
  %MAP(34);
  %MAP(35);
  %MAP(36);
  %MAP(37);
  %MAP(38);
  %MAP(39);
  %MAP(40);
  %MAP(41);
  %MAP(42);
  %MAP(43);
  %MAP(44);
  %MAP(45);
  %MAP(46);
  %MAP(47);
  %MAP(48);
  %MAP(49);
  %MAP(50);
  %MAP(51);
  %MAP(52);
  %MAP(53);
  %MAP(54);
  %MAP(55);
  %MAP(56);
  %MAP(57);
  %MAP(58);
  %MAP(59);
  %MAP(60);
  %MAP(61);
  %MAP(62);
  %MAP(63);
  %MAP(64);
  %MAP(65);
  %MAP(66);
  %MAP(67);
  %MAP(68);
  %MAP(69);
  %MAP(70);
  %MAP(71);
  %MAP(72);
  %MAP(73);
  %MAP(74);
  %MAP(75);
  %MAP(76);
  %MAP(77);
  %MAP(78);
  %MAP(79);
  %MAP(80);
  %MAP(81);
  %MAP(82);
  %MAP(83);
  %MAP(84);
  %MAP(85);
  %MAP(86);
  %MAP(87);
  %MAP(88);
  %MAP(89);
  %MAP(90);
  %MAP(91);
  %MAP(92);
  %MAP(93);
  %MAP(94);
  %MAP(95);
  %MAP(96);
  %MAP(97);
  %MAP(98);
  %MAP(99);
  %MAP(100);
  %MAP(101);
  %MAP(102);
  %MAP(103);
  %MAP(104);
  %MAP(105);
  %MAP(106);
  %MAP(107);
  %MAP(108);
  %MAP(109);
  %MAP(110);
  %MAP(111);
  %MAP(112);
  %MAP(113);
  %MAP(114);
  %MAP(115);
  %MAP(116);
  %MAP(117);
  %MAP(118);
  %MAP(119);
  %MAP(120);
  %MAP(121);
  %MAP(122);
  %MAP(123);
  %MAP(124);
  %MAP(125);
  %MAP(126);
  %MAP(127);
  %MAP(128);
  %MAP(129);
  %MAP(130);
  %MAP(131);
  %MAP(132);
  %MAP(133);
  %MAP(134);
  %MAP(135);
  %MAP(136);
  %MAP(137);
  %MAP(138);
  %MAP(139);
  %MAP(140);
  %MAP(141);
  %MAP(142);
  %MAP(143);
  %MAP(144);
  %MAP(145);
  %MAP(146);
  %MAP(147);
  %MAP(148);
  %MAP(149);
  %MAP(150);
  %MAP(151);
  %MAP(152);
  %MAP(153);
  %MAP(154);
  %MAP(155);
%MEND LOOP;
%LOOP(2012);
%LOOP(2013);
%LOOP(2014);
%LOOP(2015);
%LOOP(ALL);

/*** Excel Output ***/

/*FILENAME SYS DDE "EXCEL | SYSTEM " ;*/
/*DATA _NULL_;*/
/*  FILE SYS;*/
/*  PUT '[SELECT("R4C1")]';*/
/*  PUT "[INSERT.PICTURE(""&OUTG.\&FILE.1.png"")]";*/
/*  PUT '[SELECT("R27C1")]';*/
/*  PUT "[INSERT.PICTURE(""&OUTG.\&FILE.2.png"")]";*/
/*  PUT '[SELECT("R50C1")]';PUT '[SET.PAGE.BREAK()]';*/
/*  PUT "[INSERT.PICTURE(""&OUTG.\&FILE.3.png"")]";*/
/*  PUT '[SELECT("R73C1")]';*/
/*  PUT "[INSERT.PICTURE(""&OUTG.\&FILE.4.png"")]";*/
/*  PUT '[SELECT("R96C1")]';PUT '[SET.PAGE.BREAK()]';*/
/*  PUT "[INSERT.PICTURE(""&OUTG.\&FILE.5.png"")]";*/
/*  PUT '[SELECT("R119C1")]';*/
/*  PUT "[INSERT.PICTURE(""&OUTG.\&FILE.6.png"")]";*/
/*  PUT '[SELECT("R142C1")]';PUT '[SET.PAGE.BREAK()]';*/
/*  PUT "[INSERT.PICTURE(""&OUTG.\&FILE.7.png"")]";*/
/*  PUT '[SELECT("R165C1")]';*/
/*  PUT "[INSERT.PICTURE(""&OUTG.\&FILE.8.png"")]";*/
/*  PUT '[SELECT("R188C1")]';PUT '[SET.PAGE.BREAK()]';*/
/*  PUT "[INSERT.PICTURE(""&OUTG.\&FILE.9.png"")]";*/
/*  PUT '[SELECT("R211C1")]';*/
/*  PUT "[INSERT.PICTURE(""&OUTG.\&FILE.10.png"")]";*/
/*  PUT '[SELECT("R234C1")]';PUT '[SET.PAGE.BREAK()]';*/
/*  PUT "[INSERT.PICTURE(""&OUTG.\&FILE.11.png"")]";*/
/*  PUT '[SELECT("R257C1")]';*/
/*  PUT "[INSERT.PICTURE(""&OUTG.\&FILE.12.png"")]";*/
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
/**/
%STAT_FIN;

/*** END ***/
