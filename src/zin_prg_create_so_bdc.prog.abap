REPORT z1014309_sales_bdc
       NO STANDARD PAGE HEADING LINE-SIZE 255.

* Include bdcrecx1_s:
* The call transaction using is called WITH AUTHORITY-CHECK!
* If you have own auth.-checks you can use include bdcrecx1 instead.
*include bdcrecx1_s.
TYPES : BEGIN OF ty_sales,
          auart      TYPE vbak-auart,
          vkorg      TYPE vbak-vkorg,
          vtweg      TYPE vbak-vtweg,
          spart      TYPE vbak-spart,
          bstkd      TYPE vbkd-bstkd,
          kunnr      TYPE kuagv-kunnr,
          kunnr1     TYPE kuwev-kunnr,
          matnr      TYPE vbap-matnr,
*          posnr  TYPE vbap-posnr,
          kwmeng(10), "TYPE vbap-kwmeng,
          vrkme      TYPE vbap-vrkme,
        END OF ty_sales.

DATA : lt_sales   TYPE STANDARD TABLE OF ty_sales,
       ls_sales   TYPE ty_sales,
       lt_bdcdata TYPE STANDARD TABLE OF  bdcdata,
       ls_bdcdata TYPE bdcdata,
       lv_file    TYPE string.

DATA : lt_bdcmsgcoll TYPE TABLE OF bdcmsgcoll,
       ls_bdcmsgcoll TYPE bdcmsgcoll,
       lv_msg        TYPE string.

DATA : lv_ind  TYPE numc2,
       lv_fnam TYPE char20,
       lv_bstkd type vbkd-bstkd.

PARAMETERS p_file TYPE localfile.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_file.
  CALL FUNCTION 'F4_FILENAME'
    EXPORTING
      program_name  = syst-cprog
      dynpro_number = syst-dynnr
*     FIELD_NAME    = ' '
    IMPORTING
      file_name     = p_file.



START-OF-SELECTION.

  lv_file = p_file.

  CALL FUNCTION 'GUI_UPLOAD'
    EXPORTING
      filename                = lv_file
      filetype                = 'ASC'
      has_field_separator     = 'X'
*     HEADER_LENGTH           = 0
*     READ_BY_LINE            = 'X'
*     DAT_MODE                = ' '
*     CODEPAGE                = ' '
*     IGNORE_CERR             = ABAP_TRUE
*     REPLACEMENT             = '#'
*     CHECK_BOM               = ' '
*     VIRUS_SCAN_PROFILE      =
*     NO_AUTH_CHECK           = ' '
* IMPORTING
*     FILELENGTH              =
*     HEADER                  =
    TABLES
      data_tab                = lt_sales
* CHANGING
*     ISSCANPERFORMED         = ' '
    EXCEPTIONS
      file_open_error         = 1
      file_read_error         = 2
      no_batch                = 3
      gui_refuse_filetransfer = 4
      invalid_type            = 5
      no_authority            = 6
      unknown_error           = 7
      bad_data_format         = 8
      header_not_allowed      = 9
      separator_not_allowed   = 10
      header_too_long         = 11
      unknown_dp_error        = 12
      access_denied           = 13
      dp_out_of_memory        = 14
      disk_full               = 15
      dp_timeout              = 16
      OTHERS                  = 17.
  IF sy-subrc <> 0.
* Implement suitable error handling here
  ENDIF.
*  SORT lt_sales BY bstkd.
  LOOP AT lt_sales INTO ls_sales.
    REFRESH lt_bdcdata.
*perform open_group.
    DATA(lv_kunnr) = ls_sales-kunnr.
    DATA(lv_kunnr1) = ls_sales-kunnr1.

*    AT NEW bstkd.
    IF lv_bstkd <> ls_sales-bstkd.

      ls_sales-kunnr = lv_kunnr.
      ls_sales-kunnr1 = lv_kunnr1.

      PERFORM bdc_dynpro      USING 'SAPMV45A' '0101'.
      PERFORM bdc_field       USING 'BDC_CURSOR'
                                    'VBAK-SPART'.
      PERFORM bdc_field       USING 'BDC_OKCODE'
                                    '=ENT2'.
      PERFORM bdc_field       USING 'VBAK-AUART'
                                    ls_sales-auart.        "'or'.
      PERFORM bdc_field       USING 'VBAK-VKORG'
                                    ls_sales-vkorg.   "'1710'.
      PERFORM bdc_field       USING 'VBAK-VTWEG'
                                    ls_sales-vtweg.       "'10'.
      PERFORM bdc_field       USING 'VBAK-SPART'
                                    ls_sales-spart.     "'00'.
      PERFORM bdc_dynpro      USING 'SAPMV45A' '4001'.
      PERFORM bdc_field       USING 'BDC_OKCODE'
                                    '=SICH'.
      PERFORM bdc_field       USING 'VBKD-BSTKD'
                                    ls_sales-bstkd.     "'Likitha'.
      PERFORM bdc_field       USING 'KUAGV-KUNNR'
                                    ls_sales-kunnr.       "'17100006'.
      PERFORM bdc_field       USING 'KUWEV-KUNNR'
                                    ls_sales-kunnr1.          "'17100007'.
      PERFORM bdc_field       USING 'RV45A-KETDAT'
                                    '06.04.2023'.
      PERFORM bdc_field       USING 'RV45A-KPRGBZ'
                                    'D'.
      PERFORM bdc_field       USING 'VBKD-PRSDT'
                                    '06.04.2023'.

*  ENDAT.
      lv_ind += 1.

      CONCATENATE 'VBAP-VRKME(' lv_ind ')' INTO lv_fnam.

      PERFORM bdc_field       USING 'BDC_CURSOR'
                                    lv_fnam.          "'VBAP-VRKME(01)'.

      CLEAR lv_fnam.

      CONCATENATE 'RV45A-MABNR(' lv_ind ')' INTO lv_fnam.

      PERFORM bdc_field       USING lv_fnam           "'RV45A-MABNR(01)'
                                    ls_sales-matnr.    "  'TG11'.

      CONCATENATE 'RV45A-KWMENG(' lv_ind ')' INTO lv_fnam.

      PERFORM bdc_field       USING lv_fnam         "'RV45A-KWMENG(01)'
                                    ls_sales-kwmeng.   "'                 20'.

      CONCATENATE 'VBAP-VRKME(' lv_ind ')' INTO lv_fnam.

      PERFORM bdc_field       USING lv_fnam         "'VBAP-VRKME(01)'
                                    ls_sales-vrkme. "'PC'.

*perform bdc_dynpro      using 'SAPMSSY0' '0120'.
*perform bdc_field       using 'BDC_CURSOR'
*                              '04/03'.
*perform bdc_field       using 'BDC_OKCODE'
*                              '=CANC'.
*perform bdc_dynpro      using 'SAPMV45A' '4001'.
*perform bdc_field       using 'BDC_OKCODE'
*                              'CANC'.
*perform bdc_field       using 'VBKD-BSTKD'
*                              ls_sales-bstkd."'Aditya'.
*perform bdc_field       using 'VBKD-BSTDK'
*                              '06.04.2023'.
*perform bdc_field       using 'KUAGV-KUNNR'
*                              ls_sales-kunnr."'17100006'.
*perform bdc_field       using 'KUWEV-KUNNR'
*                              ls_sales-kunnr1."'17100007'.
*perform bdc_field       using 'RV45A-KETDAT'
*                              '06.04.2023'.
*perform bdc_field       using 'RV45A-KPRGBZ'
*                              'D'.
*perform bdc_field       using 'VBKD-PRSDT'
*                              '06.04.2023'.
*perform bdc_field       using 'VBKD-ZTERM'
*                              '0004'.
*perform bdc_field       using 'VBKD-INCO1'
*                              'EXW'.
*perform bdc_field       using 'VBKD-INCO2_L'
*                              'Palo Alto'.
*perform bdc_field       using 'BDC_CURSOR'
*                              'VBAP-VRKME(02)'.
*perform bdc_field       using 'RV45A-MABNR(02)'
*                              'TG11'.
*perform bdc_field       using 'RV45A-KWMENG(02)'
*                              '                 30'.
*perform bdc_field       using 'VBAP-VRKME(02)'
*                              'PC'.
*    AT NEW bstkd.
*      PERFORM bdc_dynpro      USING 'SAPMV45A' '4001'.
*      PERFORM bdc_field       USING 'BDC_OKCODE'
*                                    '=SICH'.
*    ENDAT.
*perform bdc_field       using 'VBKD-BSTKD'
*                              'Aditya'.
*perform bdc_field       using 'VBKD-BSTDK'
*                              '06.04.2023'.
*perform bdc_field       using 'KUAGV-KUNNR'
*                              '17100006'.
*perform bdc_field       using 'KUWEV-KUNNR'
*                              '17100007'.
*perform bdc_field       using 'RV45A-KETDAT'
*                              '06.04.2023'.
*perform bdc_field       using 'RV45A-KPRGBZ'
*                              'D'.
*perform bdc_field       using 'VBKD-PRSDT'
*                              '06.04.2023'.
*perform bdc_field       using 'VBKD-ZTERM'
*                              '0004'.
*perform bdc_field       using 'VBKD-INCO1'
*                              'EXW'.
*perform bdc_field       using 'VBKD-INCO2_L'
*                              'Palo Alto'.
*perform bdc_field       using 'BDC_CURSOR'
*                              'RV45A-MABNR(02)'.
      PERFORM bdc_dynpro      USING 'SAPLSPO2' '0101'.
      PERFORM bdc_field       USING 'BDC_OKCODE'
                                    '=OPT1'.

    ELSE.
      lv_ind += 1.

      CONCATENATE 'VBAP-VRKME(' lv_ind ')' INTO lv_fnam.

      PERFORM bdc_field       USING 'BDC_CURSOR'
                                    lv_fnam.          "'VBAP-VRKME(01)'.

      CLEAR lv_fnam.

      CONCATENATE 'RV45A-MABNR(' lv_ind ')' INTO lv_fnam.

      PERFORM bdc_field       USING lv_fnam           "'RV45A-MABNR(01)'
                                    ls_sales-matnr.    "  'TG11'.

      CONCATENATE 'RV45A-KWMENG(' lv_ind ')' INTO lv_fnam.

      PERFORM bdc_field       USING lv_fnam         "'RV45A-KWMENG(01)'
                                    ls_sales-kwmeng.   "'                 20'.

      CONCATENATE 'VBAP-VRKME(' lv_ind ')' INTO lv_fnam.

      PERFORM bdc_field       USING lv_fnam         "'VBAP-VRKME(01)'
                                    ls_sales-vrkme. "'PC'.

    ENDIF.
    CALL TRANSACTION 'VA01' USING lt_bdcdata MODE 'A' UPDATE 'A' MESSAGES INTO lt_bdcmsgcoll.

    lv_bstkd = ls_sales-bstkd.
  ENDLOOP.
*perform bdc_transaction using 'VA01'.

*perform close_group.

  LOOP AT lt_bdcmsgcoll INTO ls_bdcmsgcoll.

    CALL FUNCTION 'MESSAGE_TEXT_BUILD'
      EXPORTING
        msgid               = ls_bdcmsgcoll-msgid
        msgnr               = ls_bdcmsgcoll-msgnr
        msgv1               = ls_bdcmsgcoll-msgv1
        msgv2               = ls_bdcmsgcoll-msgv2
        msgv3               = ls_bdcmsgcoll-msgv3
        msgv4               = ls_bdcmsgcoll-msgv4
      IMPORTING
        message_text_output = lv_msg.
    WRITE : / lv_msg.
  ENDLOOP.
*----------------------------------------------------------------------*
*        Start new screen                                              *
*----------------------------------------------------------------------*
FORM bdc_dynpro USING program dynpro.
  CLEAR ls_bdcdata.
  ls_bdcdata-program  = program.
  ls_bdcdata-dynpro   = dynpro.
  ls_bdcdata-dynbegin = 'X'.
  APPEND ls_bdcdata TO lt_bdcdata.
ENDFORM.

*----------------------------------------------------------------------*
*        Insert field                                                  *
*----------------------------------------------------------------------*
FORM bdc_field USING fnam fval.
  IF fval <> space. "NODATA.
    CLEAR ls_bdcdata.
    ls_bdcdata-fnam = fnam.
    ls_bdcdata-fval = fval.
*    SHIFT ls_bdcdata-fval LEFT DELETING LEADING space.
    APPEND ls_bdcdata TO lt_bdcdata.
  ENDIF.
ENDFORM.
