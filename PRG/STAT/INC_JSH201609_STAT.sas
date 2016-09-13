**********************************************************************;
* Project           : JSH201609
*
* Program name      : INC_JSH201609_STAT.sas
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

%LET PROJ = %CURRENT_DIR;
%MACRO CLE;
  dm 'output; clear; log; clear;';
%MEND ;

%CLE;%inc "&PROJ.\JSH201609_STAT_MH001.sas" / source2 ;
%CLE;%inc "&PROJ.\JSH201609_STAT_MH001a.sas" / source2 ;
%CLE;%inc "&PROJ.\JSH201609_STAT_MH001b.sas" / source2 ;
%CLE;%inc "&PROJ.\JSH201609_STAT_MH001c.sas" / source2 ;
%CLE;%inc "&PROJ.\JSH201609_STAT_MH001d.sas" / source2 ;
%CLE;%inc "&PROJ.\JSH201609_STAT_MH002.sas" / source2 ;
%CLE;%inc "&PROJ.\JSH201609_STAT_MH003.sas" / source2 ;
%CLE;%inc "&PROJ.\JSH201609_STAT_MH004a.sas" / source2 ;
%CLE;%inc "&PROJ.\JSH201609_STAT_MH004b.sas" / source2 ;
%CLE;%inc "&PROJ.\JSH201609_STAT_MH006a.sas" / source2 ;
%CLE;%inc "&PROJ.\JSH201609_STAT_MH006b.sas" / source2 ;
