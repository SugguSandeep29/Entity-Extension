*&---------------------------------------------------------------------*
*& Report ZIN_PRG_CREATE_MAT_BDC
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zin_prg_create_mat_bdc.


TYPES: BEGIN OF ty_material,
         matnr(40),                 " material no
         mbrsh(1),                  " industry sector
         mtart(4),                  " material type
         maktx(40),                 " material decription
         meins(3),                  " base unit of measures
         msg       TYPE string,
       END OF ty_material.


DATA: lt_material TYPE TABLE OF ty_material,
      lt_bdcdata  TYPE TABLE OF bdcdata,
      lt_messtab  TYPE TABLE OF bdcmsgcoll,
      ls_material TYPE ty_material,
      ls_bdcdata  TYPE bdcdata,
      ls_messtab  TYPE bdcmsgcoll,
      lv_file     TYPE string,
      lt_raw      TYPE truxs_t_text_data.

SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME.
  PARAMETERS: p_file TYPE ibipparms-path,
              p_mode TYPE ctu_params-dismode DEFAULT 'A'.       " A-Display all dynpro
SELECTION-SCREEN END OF BLOCK b1.                               " E-Show dynpro on error only
" N-Do not display dynpro

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_file.

  CALL FUNCTION 'F4_FILENAME'
    EXPORTING
      program_name  = syst-cprog       " Module pool program name for screen field
      dynpro_number = syst-dynnr       " Dynpro number where F4 help is needed
*     field_name    = space            " name of field where path is to be entered
    IMPORTING
      file_name     = p_file.                " Path name selected by user with help of Filemngr

START-OF-SELECTION.
  FREE: lt_material,lv_file.

  lv_file = p_file.

** To upload file from text file.

*  CALL FUNCTION 'GUI_UPLOAD'
*    EXPORTING
*      filename            = lv_file          " Name of file
*      filetype            = 'ASC'            " File Type (ASC or BIN)
*      has_field_separator = 'X'              " Columns Separated by Tabs in Case of ASCII Upload
*    TABLES
*      data_tab            = lt_material.     " Transfer table for file contents


** To Upload the file from Excel

  CALL FUNCTION 'TEXT_CONVERT_XLS_TO_SAP'
    EXPORTING
*     i_field_seperator    =                  " Character Field Length 1
*     i_line_header        =                  " Character Field Length 1
      i_tab_raw_data       = lt_raw
      i_filename           = p_file                 " Local file for upload/download
*     i_step               = 1                " Steps for progress indicator
    TABLES
      i_tab_converted_data = lt_material                  " Predefined Type
  EXCEPTIONS
     conversion_failed    = 1
     others               = 2
    .
  IF sy-subrc <> 0.
** MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
**   WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.

*  LOOP AT lt_material INTO ls_material.
*    FREE: lt_bdcdata, lt_messtab.
*
*    PERFORM bdc_dynpro      USING 'SAPLMGMM' '0060'.
*    PERFORM bdc_field       USING 'BDC_CURSOR'
*                                  'RMMG1-MTART'.
*    PERFORM bdc_field       USING 'BDC_OKCODE'
*                                  '=ENTR'.
*    PERFORM bdc_field       USING 'RMMG1-MATNR'
*                                  ls_material-matnr. "'Test-3944'.
*    PERFORM bdc_field       USING 'RMMG1-MBRSH'
*                                  ls_material-mbrsh. "'C'.
*    PERFORM bdc_field       USING 'RMMG1-MTART'
*                                  ls_material-mtart. "'FGTR'.
*    PERFORM bdc_dynpro      USING 'SAPLMGMM' '0070'.
*    PERFORM bdc_field       USING 'BDC_CURSOR'
*                                  'MSICHTAUSW-DYTXT(01)'.
*    PERFORM bdc_field       USING 'BDC_OKCODE'
*                                  '=ENTR'.
*    PERFORM bdc_field       USING 'MSICHTAUSW-KZSEL(01)'
*                                  'X'.
*    PERFORM bdc_dynpro      USING 'SAPLMGMM' '4004'.
*    PERFORM bdc_field       USING 'BDC_OKCODE'
*                                  '=BU'.
*    PERFORM bdc_field       USING 'MAKT-MAKTX'
*                                  ls_material-maktx. "'ZIN: Demo Material'.
*    PERFORM bdc_field       USING 'BDC_CURSOR'
*                                  'MARA-MEINS'.
*    PERFORM bdc_field       USING 'MARA-MEINS'
*                                  ls_material-meins. "'PC'.
*    PERFORM bdc_field       USING 'MARA-MTPOS_MARA'
*                                  'NORM'.
*
*    CALL TRANSACTION 'MM01' USING lt_bdcdata
*                 MODE   p_mode
*                 UPDATE 'A'
*                 MESSAGES INTO lt_messtab.
*
*    IF lt_messtab IS NOT INITIAL.
*      LOOP AT lt_messtab INTO ls_messtab.
*
*        CALL FUNCTION 'MESSAGE_TEXT_BUILD'
*          EXPORTING
*            msgid               = ls_messtab-msgid                 " Message ID
*            msgnr               = ls_messtab-msgnr                 " Number of message
*            msgv1               = ls_messtab-msgv1                 " Parameter 1
*            msgv2               = ls_messtab-msgv2                 " Parameter 2
*            msgv3               = ls_messtab-msgv3                 " Parameter 3
*            msgv4               = ls_messtab-msgv4                 " Parameter 4
*          IMPORTING
*            message_text_output = ls_material-msg.                 " Output message text
*
*        MODIFY lt_material FROM ls_material TRANSPORTING msg.
*
*      ENDLOOP.
*
*    ENDIF.
*
*    CLEAR ls_material.
*  ENDLOOP.
*
*  cl_demo_output=>display( lt_material ).
**----------------------------------------------------------------------*
**        Start new screen                                              *
**----------------------------------------------------------------------*
*FORM bdc_dynpro USING program dynpro.
*  ls_bdcdata-program  = program.
*  ls_bdcdata-dynpro   = dynpro.
*  ls_bdcdata-dynbegin = 'X'.
*  APPEND ls_bdcdata TO lt_bdcdata.
*  CLEAR ls_bdcdata.
*ENDFORM.
*
**----------------------------------------------------------------------*
**        Insert field                                                  *
**----------------------------------------------------------------------*
*FORM bdc_field USING fnam fval.
*  IF fval <> space.
*    ls_bdcdata-fnam = fnam.
*    ls_bdcdata-fval = fval.
*    APPEND ls_bdcdata TO lt_bdcdata.
*    CLEAR ls_bdcdata.
*  ENDIF.
*ENDFORM.
