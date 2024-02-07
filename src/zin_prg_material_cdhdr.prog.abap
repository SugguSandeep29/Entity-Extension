*&---------------------------------------------------------------------*
*& Report ZIN_PRG_MATERIAL_CDHDR
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zin_prg_material_cdhdr.

TYPES: BEGIN OF ty_final,
         oldval  TYPE if_sd_cond_chg_report=>ty_item-f_old,
         val_per TYPE if_sd_cond_chg_report=>ty_header-validity,
         newval  TYPE if_sd_cond_chg_report=>ty_item-f_new,
       END OF ty_final.

DATA:
  gs_cdhdr     TYPE if_sd_cond_chg_report=>ty_cdhdr,
  lv_matnr     TYPE matnr,
  lt_final     TYPE TABLE OF ty_final,
  ls_final     TYPE ty_final,
  lv_change_no TYPE if_sd_cond_chg_report=>ty_item-changenr,
  lv_date      TYPE sy-datum,
  lv_field     TYPE if_sd_cond_chg_report=>ty_item-fname.




SELECT-OPTIONS:
   so_date   FOR gs_cdhdr-udate NO-EXTENSION DEFAULT '20230101' TO sy-datum,
   so_mat    FOR lv_matnr. " Date


DATA: ls_change_document_header TYPE if_sd_cond_chg=>ty_change_document_header,
      ls_condition_data         TYPE if_sd_cond_chg=>ty_condition_data,
      lr_cx_sd_cond_chg         TYPE REF TO cx_sd_cond_chg,
      lv_date_start             TYPE char10,
      lv_date_end               TYPE char10.
ls_condition_data-kvewe = 'D'.

*  ls_change_document_header-objid    = so_objid[].
ls_change_document_header-date     = so_date[].

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
        rt_chg_doc_details = DATA(lt_chg_doc_details)
    ).
  CATCH cx_sd_cond_chg INTO lr_cx_sd_cond_chg.
    RAISE EXCEPTION TYPE cx_sd_cond_chg
      EXPORTING
        previous = lr_cx_sd_cond_chg.
ENDTRY.

SORT lt_chg_doc_details BY date DESCENDING.

LOOP AT lt_chg_doc_details INTO DATA(ls_chg_doc).
  LOOP AT ls_chg_doc-item INTO DATA(ls_chg_item).
    IF ls_chg_item-field_name = 'SMATN' AND ls_chg_item-old_value IN so_mat.
      ls_final-oldval = ls_chg_item-old_value.
      ls_final-newval = ls_chg_item-new_value.

      IF lv_date_start NE ls_chg_doc-validity_start AND lv_date_end NE ls_chg_doc-validity_end.
        PERFORM convert_date USING ls_chg_doc-validity_start.
        PERFORM convert_date USING ls_chg_doc-validity_end.
      ENDIF.
      ls_final-val_per = |{ ls_chg_doc-validity_start }-{ ls_chg_doc-validity_end }|.
      lv_date_start = ls_chg_doc-validity_start.
      lv_date_end   = ls_chg_doc-validity_end.

      APPEND ls_final TO lt_final.
      CLEAR ls_final.
    ENDIF.

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
    DATA(lo_col) = lo_column->get_column( 'VAL_PER' ).
    lo_col->set_short_text( 'Validity' ).
    lo_col->set_medium_text( 'Validity period' ).
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
