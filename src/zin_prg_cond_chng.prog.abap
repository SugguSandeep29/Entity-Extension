*&---------------------------------------------------------------------*
*& Report ZIN_PRG_COND_CHNG
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zin_prg_cond_chng.

TYPES: BEGIN OF ty_final,
         date    TYPE if_sd_cond_chg_report=>ty_cdhdr-udate,
         time    TYPE if_sd_cond_chg_report=>ty_cdhdr-utime,
         user    TYPE if_sd_cond_chg_report=>ty_cdhdr-username,
         tcode   TYPE if_sd_cond_chg_report=>ty_cdhdr-tcode,
         cond    TYPE if_sd_cond_chg_report=>ty_nach-kschl,
         table   TYPE if_sd_cond_chg_report=>ty_nach-kotabnr,
         mat_ent TYPE if_sd_cond_chg_report=>ty_header-varkey,
         val_per TYPE if_sd_cond_chg_report=>ty_header-validity,
         desc    TYPE if_sd_cond_chg_report=>ty_item-ftext,
         oldval  TYPE if_sd_cond_chg_report=>ty_item-f_old,
         newval  TYPE if_sd_cond_chg_report=>ty_item-f_new,
         field   TYPE if_sd_cond_chg_report=>ty_item-fname,
       END OF ty_final.

DATA:
  gr_report         TYPE REF TO if_sd_cond_chg_report,
  gr_cx_sd_cond_chg TYPE REF TO cx_sd_cond_chg,
  lt_final          TYPE TABLE OF ty_final,
  ls_final          TYPE ty_final,
  lv_change_no      TYPE if_sd_cond_chg_report=>ty_item-changenr,
  lv_date           TYPE sy-datum.


DATA:
  gs_cdhdr  TYPE if_sd_cond_chg_report=>ty_cdhdr,
  gs_nach   TYPE if_sd_cond_chg_report=>ty_nach,
  gs_item   TYPE if_sd_cond_chg_report=>ty_item,
  gs_header TYPE if_sd_cond_chg_report=>ty_header,
  gs_objid  TYPE if_sd_cond_chg_report=>ty_cdobjid.


SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-s01.
  SELECT-OPTIONS:
     so_date   FOR gs_cdhdr-udate NO-EXTENSION DEFAULT '20230101' TO sy-datum, " Date
     so_time   FOR gs_cdhdr-utime NO-EXTENSION                , " Time
     so_user   FOR gs_cdhdr-username                          , " User
     so_tcode  FOR gs_cdhdr-tcode                             . " Transaction
SELECTION-SCREEN END OF BLOCK b1                            .

SELECTION-SCREEN BEGIN OF BLOCK b2 WITH FRAME TITLE TEXT-s02.
  PARAMETERS: p_kvewe TYPE t681v-kvewe OBLIGATORY MODIF ID d1 DEFAULT 'D' . " Condition Table Usage

  SELECT-OPTIONS:
     so_kschl  FOR gs_nach-kschl                              , " Condition Type
     so_kotab  FOR gs_nach-kotabnr                            , " Condition Table
     so_datab  FOR gs_nach-datab MODIF ID d0                  , " Valid from
     so_datbi  FOR gs_nach-datbi MODIF ID d0                  . " Valid to
SELECTION-SCREEN END OF BLOCK b2                            .

SELECTION-SCREEN BEGIN OF BLOCK b3 WITH FRAME TITLE TEXT-s03.
  PARAMETERS: p_drill AS CHECKBOX DEFAULT abap_true           . " Detailed display
SELECTION-SCREEN END OF BLOCK b3                            .


START-OF-SELECTION.

  DATA ls_change_document_header TYPE if_sd_cond_chg=>ty_change_document_header.

*  ls_change_document_header-objid    = so_objid[].
  ls_change_document_header-date     = so_date[].
  ls_change_document_header-time     = so_time[].
  ls_change_document_header-username = so_user[].
  ls_change_document_header-tcode    = so_tcode[].

  DATA ls_condition_data TYPE if_sd_cond_chg=>ty_condition_data.

  ls_condition_data-kvewe = p_kvewe.
  ls_condition_data-kschl = so_kschl[].
  ls_condition_data-kotab = so_kotab[].
  ls_condition_data-datab = so_datab[].
  ls_condition_data-datbi = so_datbi[].
*  ls_condition_data-valid_on = p_valon.


  DATA: lr_context         TYPE REF TO if_sd_cond_chg_context,
        lt_chg_doc_details TYPE if_sd_cond_chg=>tt_chg_doc_details,
        lr_cx_sd_cond_chg  TYPE REF TO cx_sd_cond_chg,
        lt_cdhdr_tab       TYPE cdhdr_tab.
  TRY.
      DATA(lo_strategy) = cl_sd_cond_chg_factory=>get_strategy( is_change_document_header = ls_change_document_header
                                                                      is_condition_data         = ls_condition_data
                                                                      iv_new_instance           = abap_true ).
    CATCH cx_sd_cond_chg INTO lr_cx_sd_cond_chg.
      RAISE EXCEPTION TYPE cx_sd_cond_chg
        EXPORTING
          previous = lr_cx_sd_cond_chg.
  ENDTRY.

  lo_strategy->set_valid_applications( iv_application = '' ).

  TRY.
      lo_strategy->get_changed_documents_log(
        RECEIVING
          rt_chg_doc_details = lt_chg_doc_details
      ).
    CATCH cx_sd_cond_chg INTO lr_cx_sd_cond_chg.
      RAISE EXCEPTION TYPE cx_sd_cond_chg
        EXPORTING
          previous = lr_cx_sd_cond_chg.
  ENDTRY.

  SORT lt_chg_doc_details BY date DESCENDING.

  LOOP AT lt_chg_doc_details INTO DATA(ls_chg).
    ls_final-date = ls_chg-date.
    ls_final-time = ls_chg-time.
    ls_final-user = ls_chg-user.
    ls_final-tcode = ls_chg-transaction.
    ls_final-cond = ls_chg-condition.
    ls_final-table = ls_chg-cond_table.
    ls_final-mat_ent = ls_chg-varkey.
    ls_final-desc = 'Condition record has been updated'.

    PERFORM convert_date USING ls_chg-validity_start.
    PERFORM convert_date USING ls_chg-validity_end.

    ls_final-val_per = |{ ls_chg-validity_start }-{ ls_chg-validity_end }|.

    IF lv_change_no <> ls_chg-changenr.
      CLEAR: ls_final-oldval,
             ls_final-newval,
             ls_final-field .
      APPEND ls_final TO lt_final.
    ENDIF.
    LOOP AT ls_chg-item INTO DATA(ls_chg_item).
      CLEAR: ls_final-date,
             ls_final-time,
             ls_final-user,
             ls_final-tcode,
             ls_final-cond,
             ls_final-table,
             ls_final-mat_ent,
             ls_final-val_per.

      ls_final-field = ls_chg_item-field_name.
      ls_final-desc = ls_chg_item-description.
      ls_final-oldval = ls_chg_item-old_value.
      ls_final-newval = ls_chg_item-new_value.

      IF ( ls_final-field = 'DATAB' OR ls_final-field = 'DATBI' ).
        IF ls_final-oldval NE '00000000'.
          PERFORM convert_date USING ls_final-oldval.
        ENDIF.
        IF ls_final-newval NE '00000000'.
          PERFORM convert_date USING ls_final-newval.
        ENDIF.
      ENDIF.

      APPEND ls_final TO lt_final.
      lv_change_no = ls_chg_item-changenr.
      CLEAR: ls_chg_item.
    ENDLOOP.
  ENDLOOP.


  TRY.
      cl_salv_table=>factory(
        EXPORTING
          list_display   = if_salv_c_bool_sap=>false " ALV Displayed in List Mode
        IMPORTING
          r_salv_table   = DATA(lr_tab)                         " Basis Class Simple ALV Tables
        CHANGING
          t_table        = lt_final
      ).
    CATCH cx_salv_msg. " ALV: General Error Class with Message
  ENDTRY.

  DATA(lo_column) = lr_tab->get_columns( ).
  lo_column->set_optimize( abap_true ).

    TRY.
      DATA(lo_col) = lo_column->get_column( 'TIME' ).
      lo_col->set_short_text( 'Time' ).
      lo_col->set_medium_text( 'Time' ).
      lo_col->set_zero( abap_false ).
    CATCH cx_salv_not_found.
  ENDTRY.


  TRY.
      lo_col = lo_column->get_column( 'MAT_ENT' ).
      lo_col->set_short_text( 'Matarial' ).
      lo_col->set_medium_text( 'Material Entered' ).
    CATCH cx_salv_not_found.
  ENDTRY.

  TRY.
      lo_col = lo_column->get_column( 'VAL_PER' ).
      lo_col->set_short_text( 'Validity' ).
      lo_col->set_medium_text( 'Validity period' ).
    CATCH cx_salv_not_found.
  ENDTRY.

  TRY.
      lo_col = lo_column->get_column( 'DESC' ).
      lo_col->set_short_text( 'Desc.' ).
      lo_col->set_medium_text( 'Description' ).
    CATCH cx_salv_not_found.
  ENDTRY.

  TRY.
      lo_col = lo_column->get_column( 'OLDVAL' ).
      lo_col->set_short_text( 'Old val' ).
      lo_col->set_medium_text( 'Old value' ).
      lo_col->set_zero( abap_false ).
    CATCH cx_salv_not_found.
  ENDTRY.

  TRY.
      lo_col = lo_column->get_column( 'NEWVAL' ).
      lo_col->set_short_text( 'New val' ).
      lo_col->set_medium_text( 'New value' ).
      lo_col->set_long_text( 'New value' ).
    CATCH cx_salv_not_found.
  ENDTRY.

  TRY.
      lo_col = lo_column->get_column( 'FIELD' ).
      lo_col->set_short_text( 'Field' ).
      lo_col->set_medium_text( 'Field' ).
    CATCH cx_salv_not_found.
  ENDTRY.

  lr_tab->display( ).
*&---------------------------------------------------------------------*
*& Form CONVERT_DATE
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> LS_CHG_VALIDITY_START
*&---------------------------------------------------------------------*
FORM convert_date  USING validity.
  lv_date = validity.

  CALL FUNCTION 'CONVERT_DATE_TO_EXTERNAL'
    EXPORTING
      date_internal            = lv_date        " internal date formatting
    IMPORTING
      date_external            = validity                " external date formatting
    EXCEPTIONS
      date_internal_is_invalid = 1                " the internal date is invalid
      OTHERS                   = 2.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
      WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

ENDFORM.
