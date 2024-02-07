class ZIN_CL_PARALLEL_HANDLING definition
  public
  final
  create public .

public section.

  interfaces IF_BADI_INTERFACE .
  interfaces IF_USMD_SSW_PARA_RSLT_HANDLER .
protected section.
private section.
ENDCLASS.



CLASS ZIN_CL_PARALLEL_HANDLING IMPLEMENTATION.


  METHOD if_usmd_ssw_para_rslt_handler~handle_parallel_result.

    CONSTANTS: lc_mergestep    TYPE usmd_crequest_appstep VALUE 'DG',
               lc_finalize     TYPE usmd_crequest_action VALUE '05',
               lc_sendrivision TYPE usmd_crequest_action VALUE '06',
               lc_crtype       TYPE usmd_crequest_type VALUE 'ZINISUP1'.

    CLEAR: ev_merge_action, et_message, ev_merge_step.

    CALL METHOD cl_usmd_crequest_api=>get_instance
      EXPORTING
        iv_crequest          = iv_cr_number
      IMPORTING
        re_inst_crequest_api = DATA(lo_crequest).

    lo_crequest->read_crequest(
      IMPORTING
        es_crequest   = DATA(ls_request)                 " Change Request
    ).

    IF ls_request-usmd_creq_type EQ lc_crtype.

      LOOP AT it_step_action ASSIGNING FIELD-SYMBOL(<ls_step_action>).
        IF <ls_step_action>-action = lc_sendrivision.
          ev_merge_action = lc_sendrivision.
          ev_merge_step = lc_mergestep.
          RETURN.
        ENDIF.
      ENDLOOP.

      ev_merge_action = lc_finalize.
      ev_merge_step   = lc_mergestep.

    ENDIF.

  ENDMETHOD.
ENDCLASS.
