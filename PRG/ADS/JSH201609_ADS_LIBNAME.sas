**********************************************************************;
* Project           : JSH201609
*
* Program name      : JSH201609_ADS_LIBNAME.sas
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
PROC DATASETS LIBRARY = WORK KILL NOLIST; QUIT;

%MACRO WORKING_DIR;

  %LOCAL _FULLPATH _PATH;
  %LET   _FULLPATH = ;
  %LET   _PATH     = ;

  %IF &sysscp=WIN %THEN %DO;
    %IF %LENGTH(%SYSFUNC(GETOPTION(SYSIN))) = 0 %THEN
        %LET _FULLPATH = %SYSGET(SAS_EXECFILEPATH);
    %ELSE
        %LET _FULLPATH = %SYSFUNC(GETOPTION(SYSIN));
  %END;
  %IF &sysscp=LIN X64 %THEN %DO;
    %LET _FULLPATH = &_SASPROGRAMFILE;
  %END;

  %LET _PATH = %SUBSTR(   &_FULLPATH., 1, %LENGTH(&_FULLPATH.)
                        - %LENGTH(%SCAN(&_FULLPATH.,-1,&_FS.))
                        - %LENGTH(%SCAN(&_FULLPATH.,-2,&_FS.))
                        - %LENGTH(%SCAN(&_FULLPATH.,-3,&_FS.))
                        - 3 );

  &_PATH.
%MEND WORKING_DIR;

%LET _WK_PATH = %WORKING_DIR;

LIBNAME LIBRAW  "&_WK_PATH.&_FS.RAWDATA"          access = readonly;
LIBNAME LIBEXT  "&_WK_PATH.&_FS.RAWDATA&_FS.EXT"      access = readonly;
LIBNAME LIBADS  "&_WK_PATH.&_FS.ADS";

%LET OUT = &_WK_PATH.&_FS.ADS ;
%LET LOG = &_WK_PATH.&_FS.LOG&_FS.ADS;
%LET EXT = &_WK_PATH.&_FS.RAWDATA&_FS.EXT;
%LET RAW = &_WK_PATH.&_FS.RAWDATA;

OPTIONS  VALIDVARNAME=V7
         FMTSEARCH = (LIBADS WORK)
         SASAUTOS = ("&_WK_PATH.&_FS.PRG&_FS.ADS&_FS.Macro") CMDMAC
         NOFMTERR
         NOMLOGIC NOSYMBOLGEN NOMPRINT
         LS = 100 MISSING = "" PAGENO = 1;

/*** END ***/
