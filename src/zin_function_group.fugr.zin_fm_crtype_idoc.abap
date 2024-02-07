FUNCTION zin_fm_crtype_idoc.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     REFERENCE(INPUT_METHOD) LIKE  BDWFAP_PAR-INPUTMETHD
*"     REFERENCE(MASS_PROCESSING) LIKE  BDWFAP_PAR-MASS_PROC
*"  EXPORTING
*"     REFERENCE(WORKFLOW_RESULT) LIKE  BDWF_PARAM-RESULT
*"     REFERENCE(APPLICATION_VARIABLE) LIKE  BDWF_PARAM-APPL_VAR
*"     REFERENCE(IN_UPDATE_TASK) LIKE  BDWFAP_PAR-UPDATETASK
*"     REFERENCE(CALL_TRANSACTION_DONE) LIKE  BDWFAP_PAR-CALLTRANS
*"  TABLES
*"      IDOC_CONTRL STRUCTURE  EDIDC
*"      IDOC_DATA STRUCTURE  EDIDD
*"      IDOC_STATUS STRUCTURE  BDIDOCSTAT
*"      RETURN_VARIABLES STRUCTURE  BDWFRETVAR
*"      SERIALIZATION_INFO STRUCTURE  BDI_SER
*"  EXCEPTIONS
*"      WRONG_FUNCTION_CALLED
*"----------------------------------------------------------------------

  INCLUDE mbdconwf.

  DATA: lt_seg   TYPE TABLE OF zin_crtype_segment,
        ls_crtyp TYPE zin_t_crtype_cpy,
        ls_seg   TYPE zin_crtype_segment.

  workflow_result = c_wf_result_ok.

  LOOP AT idoc_contrl.
    IF idoc_contrl-mestyp = 'ZIN_CRTYPE_MSG_TYPE'.
      LOOP AT idoc_data.
        ls_seg = idoc_data-sdata.
      ENDLOOP.

      ls_crtyp = CORRESPONDING #( ls_seg ).
      IF ls_seg-usmd_creq_type IS INITIAL.
        DELETE FROM zin_t_crtype_cpy.

        INSERT zin_t_crtype_cpy FROM ( SELECT * FROM zin_t_crtype ).

        APPEND VALUE #( docnum = idoc_data-docnum
                        status = '51'
                        msgty = 'E'
                        msgid = 'ZIN_MESSAGE_CLS'
                        msgno = '024'
                        msgv1 = ls_seg-usmd_creq_type ) TO idoc_status[].
        CLEAR idoc_status.

      ELSE.

        MODIFY zin_t_crtype_cpy FROM ls_crtyp.
        APPEND VALUE #( docnum = idoc_data-docnum
                        status = '53'
                        msgv1 = ls_seg-usmd_creq_type ) TO idoc_status[].
        CLEAR idoc_status.
      ENDIF.
    ENDIF.

  ENDLOOP.




ENDFUNCTION.
