*&---------------------------------------------------------------------*
*& Report ZIN_PRG_CREATE_MAT_BDC
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zin_prg_bdcc_fi01_bank_cre.

TYPES: BEGIN OF ty_bnka,
         banks TYPE bnka-banks,     " Country
         bankl TYPE bnka-bankl,     " Bank Key
         banka TYPE bnka-banka,     " Bank Name
         provz TYPE bnka-provz,     " Region
         stras TYPE bnka-stras,     " Street
         ort01 TYPE bnka-ort01,     " City
         brnch TYPE bnka-brnch,     " Branch
       END OF ty_bnka.

DATA:
  lt_bnka    TYPE TABLE OF ty_bnka,
  ls_bnka    TYPE ty_bnka,
  lt_bdcdata TYPE TABLE OF bdcdata,
  lt_messtab TYPE TABLE OF bdcmsgcoll,
  ls_bdcdata TYPE bdcdata,
  ls_messtab TYPE bdcmsgcoll,
  lv_msg     TYPE string.

START-OF-SELECTION.

  PERFORM get_data_from_file_into_table.
  PERFORM bdc_process.

*&---------------------------------------------------------------------*
*& Form get_data_from_file_into_table
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM get_data_from_file_into_table .
** To upload file from text file.

  CALL FUNCTION 'GUI_UPLOAD'
    EXPORTING
      filename            = `D:\Imp Data\SAP\MDG Notes\Data Migration\BDC\Bank_Flat_File.txt`          " Name of file
      filetype            = 'ASC'            " File Type (ASC or BIN)
      has_field_separator = 'X'              " Columns Separated by Tabs in Case of ASCII Upload
    TABLES
      data_tab            = lt_bnka.     " Transfer table for file contents

ENDFORM.
*&---------------------------------------------------------------------*
*& Form bdc_process
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM bdc_process .
  LOOP AT lt_bnka INTO ls_bnka.

    PERFORM bdc_dynprog USING 'SAPMF02B' '0100'.
    PERFORM bdc_field USING 'BNKA-BANKS' ls_bnka-banks.
    PERFORM bdc_field USING 'BNKA-BANKL' ls_bnka-bankl.
    PERFORM bdc_field USING 'BDC_OKCODE' '/00'.
    PERFORM bdc_dynprog USING 'SAPMF02B' '0110'.
    PERFORM bdc_field USING 'BNKA-BANKA' ls_bnka-banka.
    PERFORM bdc_field USING 'BNKA-PROVZ' ls_bnka-provz.
    PERFORM bdc_field USING 'BNKA-STRAS' ls_bnka-stras.
    PERFORM bdc_field USING 'BNKA-ORT01' ls_bnka-ort01.
    PERFORM bdc_field USING 'BNKA-BRNCH' ls_bnka-brnch.
    PERFORM bdc_field USING 'BDC_OKCODE' '=UPDA'.

    CALL TRANSACTION 'FI01' USING lt_bdcdata MODE 'N' UPDATE 'A' MESSAGES INTO lt_messtab.
    REFRESH lt_bdcdata.

  ENDLOOP.

  LOOP AT lt_messtab INTO ls_messtab.
    CALL FUNCTION 'FORMAT_MESSAGE'
      EXPORTING
        id        = ls_messtab-msgid         " Application Area
        lang      = ls_messtab-msgspra
        no        = ls_messtab-msgnr
        v1        = ls_messtab-msgv1         " 1st parameter
        v2        = ls_messtab-msgv2         " 2nd parameter
        v3        = ls_messtab-msgv3         " 3rd parameter
        v4        = ls_messtab-msgv4         " 4th Parameter
      IMPORTING
        msg       = lv_msg
      EXCEPTIONS
        not_found = 1
        OTHERS    = 2.
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
        WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ELSE.
      WRITE: lv_msg.
    ENDIF.

  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form bdc_dynprog
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> P_
*&      --> P_
*&---------------------------------------------------------------------*
FORM bdc_dynprog  USING pg_name sc_no.
  CLEAR ls_bdcdata.
  ls_bdcdata-program = pg_name.
  ls_bdcdata-dynpro = sc_no.
  ls_bdcdata-dynbegin = 'X'.
  APPEND ls_bdcdata TO lt_bdcdata.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form bdc_field
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> P_
*&      --> LS_BNKA_ORT01
*&---------------------------------------------------------------------*
FORM bdc_field  USING fd_nam fd_val.
  CLEAR ls_bdcdata.
  ls_bdcdata-fnam = fd_nam.
  ls_bdcdata-fval = fd_val.
  APPEND ls_bdcdata TO lt_bdcdata.

ENDFORM.
