*&---------------------------------------------------------------------*
*& Report ZIN_PRG_CRTYP_IDOC_OUTBOUND
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zin_prg_crtyp_idoc_outbound.

DATA: lt_crtype     TYPE STANDARD TABLE OF zin_t_crtype,
      lt_edidc      TYPE STANDARD TABLE OF edidc,
      lt_edidd      TYPE  STANDARD TABLE OF edidd,
      ls_cust       TYPE zin_t_crtype,
      ls_crtype_seg TYPE zin_crtype_segment,
      ls_edidc      TYPE edidc,
      ls_edidd      TYPE edidd.

SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-001.

  PARAMETERS: p_idoctp TYPE edidc-idoctp DEFAULT 'ZIN_CRTYPE_BASIC',
              p_mestyp TYPE edidc-mestyp DEFAULT 'ZIN_CRTYPE_MSG_TYPE',
              p_rcvprt TYPE edidc-rcvprt DEFAULT 'LS',
              p_rcvprn TYPE edidc-rcvprn DEFAULT 'S4XCLNT100',
              p_rcvpor TYPE edidc-rcvpor DEFAULT 'ZIN_CRPORT',
              p_sndprt TYPE edidc-sndprt DEFAULT 'LS',
              p_sndprn TYPE edidc-sndprn DEFAULT 'S4XCLNT100'.

SELECTION-SCREEN END OF BLOCK b1.

SELECTION-SCREEN BEGIN OF BLOCK b2 WITH FRAME.
  PARAMETERS: p_crtype TYPE usmd_crequest_type,
              p_txtmi  TYPE usmd_txtmi.
SELECTION-SCREEN END OF BLOCK b2.


START-OF-SELECTION.

  PERFORM control_data_record.

  PERFORM generate_idoc.


FORM control_data_record .
  CLEAR ls_edidc.
  REFRESH lt_edidc.

  ls_edidc-idoctp = p_idoctp.
  ls_edidc-mestyp = p_mestyp.
  ls_edidc-rcvprt = p_rcvprt .
  ls_edidc-rcvprn = p_rcvprn.
  ls_edidc-rcvpor = p_rcvpor.
  ls_edidc-sndprt = p_sndprt.
  ls_edidc-sndprn = p_sndprn.

  APPEND ls_edidc TO lt_edidc.
*  data record

  CLEAR ls_crtype_seg.
  ls_crtype_seg = VALUE #( usmd_creq_type = p_crtype txtmi = p_txtmi ).

  CLEAR ls_edidd.
  REFRESH lt_edidd.

  ls_edidd-segnam = 'ZIN_CRTYPE_SEGMENT'.
  ls_edidd-sdata = ls_crtype_seg.
  APPEND ls_edidd TO lt_edidd.

ENDFORM.

FORM generate_idoc .

  CALL FUNCTION 'MASTER_IDOC_DISTRIBUTE'
    EXPORTING
      master_idoc_control            = ls_edidc
    TABLES
      communication_idoc_control     = lt_edidc
      master_idoc_data               = lt_edidd
    EXCEPTIONS
      error_in_idoc_control          = 1
      error_writing_idoc_status      = 2
      error_in_idoc_data             = 3
      sending_logical_system_unknown = 4
      OTHERS                         = 5.
  IF sy-subrc <> 0.
* Implement suitable error handling here

  ELSE.
    CALL FUNCTION 'DEQUEUE_ALL'.
    COMMIT WORK.

    LOOP AT lt_edidc INTO ls_edidc.
      MESSAGE 'Idoc generated successfully' && ls_edidc-docnum TYPE 'S'.

    ENDLOOP.


  ENDIF.

ENDFORM.
