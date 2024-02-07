class ZIN_CL_DYN_AGENT_BADI definition
  public
  final
  create public .

public section.

  interfaces IF_BADI_INTERFACE .
  interfaces IF_USMD_SSW_DYNAMIC_AGT_SELECT .
protected section.
private section.
ENDCLASS.



CLASS ZIN_CL_DYN_AGENT_BADI IMPLEMENTATION.


  method IF_USMD_SSW_DYNAMIC_AGT_SELECT~GET_DYNAMIC_AGENTS.
    DATA: lo_model      TYPE REF TO if_usmd_model_ext,
          lo_context    TYPE REF TO if_usmd_app_context,
          lt_entity     TYPE usmd_t_entity,
          lt_user_agent TYPE usmd_s_user_agent,
          ls_user_group TYPE usmd_t_user_agent_group,
          lv_country    TYPE land1.


    FIELD-SYMBOLS: <lt_data> TYPE ANY TABLE,
                   <ls_data> TYPE any.

    cl_usmd_crequest_util=>get_model_by_cr(
      EXPORTING
        i_crequest = iv_cr_number                 " Change Request
      IMPORTING
        e_model    = DATA(lv_model)                 " Data Model of Change Request
    ).

    IF lv_model IS NOT INITIAL.
      cl_usmd_model_ext=>get_instance(
        EXPORTING
          i_usmd_model = lv_model                 " Data model
        IMPORTING
          eo_instance  = DATA(lo_inst_model)                 " MDM Data Model for Access from Non-SAP Standard
          et_message   = DATA(lt_message)                 " Messages
      ).

      IF lo_inst_model IS BOUND.
        lt_entity = VALUE #( ( 'AD_POSTAL' ) ).
*      IF lo_model IS BOUND.
        lo_inst_model->read_entity_data_all(
          EXPORTING
            i_fieldname      = 'BP_HEADER'                  " Financial MDM: Field Name
            if_active        = abap_false                   " Financial MDM: General Indicator
            i_crequest       = iv_cr_number                 " Change Request
*           it_sel           =                              " Sorted Table: Selection Condition (Range per Field)
            it_entity_filter = lt_entity                 " Ent.Types for Which Data Is Expected; Default: All Ent.Types
          IMPORTING
            et_message       = lt_message                " Messages
            et_data_entity   = DATA(lt_entity_data)                 " Data for Entity Types
        ).


    IF lt_entity_data IS NOT INITIAL.
      DATA(ls_entity_data) = lt_entity_data[ usmd_entity = 'BP_HEADER' usmd_entity_cont = 'AD_POSTAL' struct = 'KATTR' ].

      ASSIGN ls_entity_data-r_t_data->* TO FIELD-SYMBOL(<lt_ent_data>).

      LOOP AT <lt_ent_data> ASSIGNING <ls_data>.
        ASSIGN COMPONENT 'REF_POSTA' OF STRUCTURE <ls_data> TO FIELD-SYMBOL(<lv_field>).
        lv_country = <lv_field>.
      ENDLOOP.

      ct_user_agent_group = VALUE #( ( agent_group = 001
                                       step_type = 3
                                       user_agent = COND #( WHEN lv_country = 'CA'
                                                            THEN VALUE #( ( user_type = 'US'
                                                                            user_value = 'INGALEA' ) )

                                                            WHEN lv_country = 'US'
                                                            THEN VALUE #( ( user_type = 'US'
                                                                            user_value = 'SUREKHAS' ) )

                                                            ELSE VALUE #( ( user_type = 'US'
                                                                            user_value = 'SANDEEPS' ) ) ) ) ).
    ENDIF.
  ENDIF.
ENDIF.



  endmethod.
ENDCLASS.
