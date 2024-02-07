class ZIN_CL_MDQ_DERIVATION definition
  public
  final
  create public .

public section.

  interfaces IF_BADI_INTERFACE .
  interfaces IF_USMD_SSW_SYST_METHOD_CALLER .
protected section.
private section.
ENDCLASS.



CLASS ZIN_CL_MDQ_DERIVATION IMPLEMENTATION.


  METHOD if_usmd_ssw_syst_method_caller~call_system_method.
    DATA(lv_cr_number) = iv_cr_number.

    DATA(lo_derivation_controller) = cl_mdg_mdq_rbwf_derive=>get_instance( ).

    DATA(lo_result) = lo_derivation_controller->if_mdg_mdq_rbwf_derive~derive( change_request = lv_cr_number ).

    ev_action = SWITCH #( lo_result
                          WHEN lo_derivation_controller->if_mdg_mdq_rbwf_derive~derivation_failure
                            THEN 'D2'
                          WHEN lo_derivation_controller->if_mdg_mdq_rbwf_derive~derivation_success_w_messages
                            THEN 'D5'
                          ELSE 'D1' ).

  ENDMETHOD.
ENDCLASS.
