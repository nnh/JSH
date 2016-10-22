**********************************************************************;
* Author            : Toshiki Saito
* Purpose           : Create MHCOD DataSet
**********************************************************************;

/*** Initial setting ***/
%MACRO FILE_SEPARATOR;
  %LOCAL _separator;
  %LET   _separator = ;
  %IF &sysscp=LIN X64 
    %THEN %LET _separator=/;
    %ELSE %LET _separator=\;
  &_separator.
%MEND FILE_SEPARATOR;
%LET _FS = %FILE_SEPARATOR;
%MACRO CURRENT_DIR;
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
                          - %LENGTH(%SCAN(&_FULLPATH.,-1,&_FS.)) -1 );
  &_PATH.
%MEND CURRENT_DIR;

%LET _PATH2 = %CURRENT_DIR;
%LET FILE = ADS;

%INCLUDE "&_PATH2.&_FS.JSH201609_ADS_LIBNAME.sas";

/*** CSV read ***/
FILENAME IN "&EXT.&_FS.disease_161021.csv" Encoding="utf-8";
PROC IMPORT OUT= MHCOD
  DATAFILE=IN
  DBMS=CSV REPLACE;
  GETNAMES=NO;
  DATAROW=1;
  GUESSINGROWS=2000; 
RUN;
PROC DATASETS;
  MODIFY MHCOD;
  RENAME VAR1=category VAR2=code VAR3=abbr VAR4=name_ja VAR5=name_en VAR6=order VAR7=group_code VAR8=group_abbr VAR9=group_name_ja VAR10=group_name_en VAR11=group_type VAR12=group_order;
  RENAME code=MHDECOD group_code=MHGRPCOD name_ja=MHTERM_J group_name_ja=MHGRPTERM_J abbr=MHTERM group_abbr=MHGRPTERM order=MHREFID;
QUIT;
DATA  MHCOD;
  SET MHCOD;
  IF  category="epilepsy" THEN DELETE;
RUN;
