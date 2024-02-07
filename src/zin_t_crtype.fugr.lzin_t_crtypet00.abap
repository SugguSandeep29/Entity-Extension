*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZIN_T_CRTYPE....................................*
DATA:  BEGIN OF STATUS_ZIN_T_CRTYPE                  .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZIN_T_CRTYPE                  .
CONTROLS: TCTRL_ZIN_T_CRTYPE
            TYPE TABLEVIEW USING SCREEN '0001'.
*.........table declarations:.................................*
TABLES: *ZIN_T_CRTYPE                  .
TABLES: ZIN_T_CRTYPE                   .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
