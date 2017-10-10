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

    %IF %LENGTH(%SYSFUNC(GETOPTION(SYSIN))) = 0 %THEN
        %LET _FULLPATH = %SYSGET(SAS_EXECFILEPATH);
    %ELSE
        %LET _FULLPATH = %SYSFUNC(GETOPTION(SYSIN));

    %LET _PATH = %SUBSTR(   &_FULLPATH., 1, %LENGTH(&_FULLPATH.)
                          - %LENGTH(%SCAN(&_FULLPATH.,-1,'\'))
                          - %LENGTH(%SCAN(&_FULLPATH.,-2,'\'))
                          - %LENGTH(%SCAN(&_FULLPATH.,-3,'\'))
                          - 3 );

    &_PATH.

%MEND WORKING_DIR;

%LET _WK_PATH = %WORKING_DIR;

LIBNAME LIBRAW  "&_WK_PATH.\RAWDATA"          access = readonly;
LIBNAME LIBEXT  "&_WK_PATH.\RAWDATA\EXT"      access = readonly;
LIBNAME LIBADS  "&_WK_PATH.\ADS";

%LET OUT = &_WK_PATH.\ADS ;
%LET LOG = &_WK_PATH.\LOG\ADS;
%LET EXT = &_WK_PATH.\RAWDATA\EXT;
%LET RAW = &_WK_PATH.\RAWDATA;

OPTIONS  VALIDVARNAME=V7
         FMTSEARCH = (LIBADS WORK)
         SASAUTOS = ("&_WK_PATH.\PRG\ADS\Macro") CMDMAC
         NOFMTERR
         NOMLOGIC NOSYMBOLGEN NOMPRINT
         LS = 100 MISSING = "" PAGENO = 1;

/*** END ***/
