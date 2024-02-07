*&---------------------------------------------------------------------*
*& Report ZIN_PRG_STRUC_INSIDE_STRUC
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zin_prg_struc_inside_struc.

PARAMETERS: p_trg TYPE ddobjname,
            p_src TYPE ddobjname.

DATA : lt_dd03p TYPE TABLE OF dd03p,
       ls_head  TYPE dd02v.

CALL FUNCTION 'DDIF_TABL_GET'
  EXPORTING
    name          = p_trg  " ZIN_STRUC   " ZIN_S_DEMO             " Name of the Table to be Read
  IMPORTING
    dd02v_wa      = ls_head                " Table Header
  TABLES
    dd03p_tab     = lt_dd03p                 " Table Fields
  EXCEPTIONS
    illegal_input = 1                " Value not Allowed for Parameter
    OTHERS        = 2.
IF sy-subrc <> 0.
  MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
    WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
ENDIF.

** Adding .include to structure

lt_dd03p = VALUE #(
                  BASE lt_dd03p
                (
                 tabname = p_trg   " ZIN_STRUC  " ZIN_S_DEMO
                 fieldname = '.INCLUDE'
                 position = CONV tabfdpos( lines( lt_dd03p ) + 1 )
                 precfield = p_src  " ZIN_S_DEMO  " ZIN_STRUC
                   ) ).


CALL FUNCTION 'DDIF_TABL_PUT'
  EXPORTING
    name              = p_trg   " ZIN_S_DEMO               " Name of the Table to be Written
    dd02v_wa          = ls_head             " Table Header
  TABLES
    dd03p_tab         = lt_dd03p                 " Table Fields
  EXCEPTIONS
    tabl_not_found    = 1                " Table Header could not be Found
    name_inconsistent = 2                " Name in Sources Inconsistent with NAME
    tabl_inconsistent = 3                " Inconsistent Sources
    put_failure       = 4                " Write Error (ROLLBACK Recommended)
    put_refused       = 5                " Write not Allowed
    OTHERS            = 6.
IF sy-subrc <> 0.
  MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
    WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
ENDIF.


CALL FUNCTION 'DDIF_TABL_ACTIVATE'
  EXPORTING
    name        = p_trg  " ZIN_S_DEMO
  EXCEPTIONS
    not_found   = 1
    put_failure = 2
    OTHERS      = 3.


IF sy-subrc <> 0.
  WRITE: / 'Table could not be activated'.
ELSE.
  WRITE: / 'Table Activated Successfully with .INCLUDE'.
ENDIF.
