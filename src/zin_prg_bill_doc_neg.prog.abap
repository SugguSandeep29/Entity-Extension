*&---------------------------------------------------------------------*
*& Report ZIN_PRG_BILL_DOC_NEG
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zin_prg_bill_doc_neg.



TYPES: BEGIN OF ty_vbrp,
         vbeln TYPE vbrp-vbeln,
         posnr TYPE vbrp-posnr,
         fklmg TYPE vbrp-fklmg,
         mwsbp TYPE vbrp-mwsbp,
         netwr TYPE vbrp-mwsbp,
         brtwr TYPE vbrp-brtwr,
       END OF ty_vbrp.


DATA: lt_vbrp  TYPE TABLE OF ty_vbrp,
      ls_vbrp  TYPE ty_vbrp,
      lv_netwr TYPE string,
      lt_fcat  TYPE slis_t_fieldcat_alv,
      ls_fcat  LIKE LINE OF lt_fcat.

SELECT vbeln posnr fklmg mwsbp netwr brtwr FROM vbrp INTO TABLE lt_vbrp UP TO 20 ROWS.

LOOP AT lt_vbrp INTO ls_vbrp.

  ls_vbrp-netwr = ls_vbrp-netwr * -1.

  lv_netwr = ls_vbrp-netwr.

*  call function 'CLOI_PUT_SIGN_IN_FRONT'
*    CHANGING
*      value = lv_netwr.
*    .

  SHIFT lv_netwr BY strlen( lv_netwr ) - 1 PLACES CIRCULAR.
  ls_vbrp-netwr = CONV #( lv_netwr ).

  MODIFY lt_vbrp FROM ls_vbrp TRANSPORTING netwr.

ENDLOOP.


PERFORM build_cat USING: 'VBELN' 'VBRP' 'VBELN',
                         'POSNR' 'VBRP' 'POSNR',
                         'FKLMG' 'VBRP' 'FKLMG',
                         'MWSBP' 'VBRP' 'MWSBP',
                         'NETWR' 'VBRP' 'NETWR',
                         'BRTWR' 'VBRP' 'BRTWR'.

FORM build_cat USING lv_field lv_tab lv_text.

*  ls_fcat-ref_fieldname = lv_field.
*  ls_fcat-ref_tabname = lv_tab.
*  ls_fcat-datatype = 'CURR'.
  ls_fcat-edit_mask = '_______'.
  ls_fcat-fieldname = lv_field.
  ls_fcat-tabname = lv_tab.
  ls_fcat-seltext_m = lv_text.

  APPEND ls_fcat TO lt_fcat.
  CLEAR ls_fcat.

ENDFORM.

START-OF-SELECTION.

  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
    EXPORTING
      i_callback_program = sy-repid           " Name of the calling program
      it_fieldcat        = lt_fcat                 " Field catalog with field descriptions
    TABLES
      t_outtab           = lt_vbrp.                " Table with data to be displayed
