class ZIN_CL_BADI_BANK_CONTEXT definition
  public
  final
  create public .

public section.

  interfaces IF_BADI_INTERFACE .
  interfaces IF_USMD_SSW_RULE_CNTX_PREPARE .
protected section.
private section.

  methods READ_CREQUEST
    importing
      !IV_CR_NUMBER type USMD_CREQUEST
    exporting
      !ES_CREQUEST type USMD_S_CREQUEST .
  methods GET_ELEMENT_ID
    importing
      !IV_CR_TYPE type USMD_CREQUEST_TYPE
      !IV_NAME type IF_FDT_TYPES=>NAME
    exporting
      !EV_BRF_EXPR_ID type IF_FDT_TYPES=>ID .
  methods IS_BP_BKDTL_INCLUDED
    importing
      !IV_CR_NUMBER type USMD_CREQUEST
    exporting
      !EV_INCLUDED type USMD_FLG .
  methods READ_DATA
    importing
      !IV_CR_NUMBER type USMD_CREQUEST
    exporting
      !ET_ENTITY type USMD_TS_DATA_ENTITY .
ENDCLASS.



CLASS ZIN_CL_BADI_BANK_CONTEXT IMPLEMENTATION.


  METHOD get_element_id.

    DATA: LT_DATA TYPE IF_FDT_QUERY=>ts_name.

    DATA(lv_app_id) = cl_usmd_wf_ssw_rule_service=>get_cr_type_app_id( iv_cr_type = iv_cr_type ).

    DATA(lo_brf_factory) = cl_fdt_factory=>if_fdt_factory~get_instance( ).

    DATA(lo_brf_query) = lo_brf_factory->get_query(
                           iv_object_type        = if_fdt_constants=>gc_object_type_data_object                 " Objekttyp
                           iv_data_object_type   = if_fdt_constants=>gc_data_object_type_element                 " Data Object Type
                         ).

    DATA(lt_selection) = VALUE if_fdt_query=>ts_selection( (
                                                       queryfield = if_fdt_admin_data_query=>gc_fn_name
                                                       sign = 'I'
                                                       option = 'EQ'
                                                       low = iv_name )
                                                     (
                                                     queryfield = if_fdt_admin_data_query=>gc_fn_application_id
                                                       sign = 'I'
                                                       option = 'EQ'
                                                       low = lv_app_id
                                                      ) ).

    DATA(ls_object_catagory) = VALUE if_fdt_query=>s_object_category_sel( system_objects = abap_true
                                                                          customizing_objects = abap_true
                                                                          masterdata_objects = abap_false  ).

    lo_brf_query->select_data(
      EXPORTING
        its_selection          = lt_selection                    " List of Selections
        is_object_category_sel = ls_object_catagory                    " Object categories - Default: Only Customizing and Syst. Obj.
      IMPORTING
        eta_data               = lt_data                    " Selected Data
    ).

    ev_brf_expr_id = COND #( WHEN lt_data IS NOT INITIAL THEN lt_data[ 1 ]-id ).

  ENDMETHOD.


  METHOD if_usmd_ssw_rule_cntx_prepare~prepare_rule_context.
    DATA:
          ls_context    TYPE usmd_s_fdt_context_value.

    CLEAR: et_message, et_rule_context_value.

    CALL METHOD me->read_crequest
      EXPORTING
        iv_cr_number = iv_cr_number                  " Change Request
      IMPORTING
        es_crequest  = DATA(ls_crequest).                 " Change Request

    me->get_element_id(
      EXPORTING
        iv_cr_type     = ls_crequest-usmd_creq_type                 " Type of Change Request
        iv_name        =  'ZIN_BANK_CHANGED'                " Beschreibung
      IMPORTING
        ev_brf_expr_id = DATA(lv_brf_exp_id)                " Universal Unique Identifier
    ).

    ls_context-id = lv_brf_exp_id.
    CREATE DATA ls_context-value TYPE usmd_flg.
    ASSIGN ls_context-value->* TO FIELD-SYMBOL(<lv_value>).

    me->is_bp_bkdtl_included(
      EXPORTING
        iv_cr_number = iv_cr_number                 " Change Request
      IMPORTING
        ev_included  = <lv_value>                 " MDGAF: General Indicator
    ).

    APPEND ls_context TO et_rule_context_value.


  ENDMETHOD.


  METHOD is_bp_bkdtl_included.
    me->read_data(
      EXPORTING
        iv_cr_number = iv_cr_number                 " Change Request
      IMPORTING
        et_entity    = DATA(lt_entity)                 " Data for Entity Types
    ).

    LOOP AT lt_entity ASSIGNING FIELD-SYMBOL(<fs_entity>).
      IF <fs_entity>-usmd_entity_cont = 'BP_BKDTL'.
        ev_included = abap_true.
        RETURN.
      ENDIF.
    ENDLOOP.
    ev_included = abap_false.

ENDMETHOD.


  METHOD read_crequest.

FIELD-SYMBOLS: <LT_DATA> TYPE SORTED TABLE.

    CLEAR es_crequest.

    CALL METHOD cl_usmd_model_ext=>get_instance
      EXPORTING
        i_usmd_model = 'BP'                " Data model
      IMPORTING
        eo_instance  = DATA(lr_model)                " MDM Data Model for Access from Non-SAP Standard
        et_message   = DATA(lt_message).                " Messages

    DATA(lt_sel) = VALUE usmd_ts_sel( ( sign = 'I' option = 'EQ' fieldname = usmd0_cs_fld-crequest low = iv_cr_number ) ).


    CALL METHOD lr_model->read_char_value
      EXPORTING
        i_fieldname       = usmd0_cs_fld-crequest                    " Field Name
        it_sel            = lt_sel                    " Sorted Table: Selection Condition (Range per Field)
        if_use_edtn_slice = abap_false                 " 'X'=Do Not Read Any Other Data Slices
      IMPORTING
        et_data           = <LT_DATA>
        et_message        = lt_message.                    " Messages

    es_crequest = COND #( WHEN <LT_DATA> IS NOT INITIAL THEN <LT_DATA>[ 1 ]
                          ELSE abap_false ).

  ENDMETHOD.


  METHOD read_data.

    cl_usmd_model_ext=>get_instance(
      EXPORTING
        i_usmd_model = 'BP'                " Data model
      IMPORTING
        eo_instance  = DATA(lo_model_ext)                 " MDM Data Model for Access from Non-SAP Standard
        et_message   = DATA(lt_message)                 " Messages
    ).

    lo_model_ext->read_entity_data_all(
      EXPORTING
        i_fieldname      = 'BP_HEADER'                 " Financial MDM: Field Name
        if_active        = abap_false                 " Financial MDM: General Indicator
        i_crequest       =  iv_cr_number                " Change Request
      IMPORTING
        et_message       = lt_message                 " Messages
        et_data_entity   = et_entity                 " Data for Entity Types
    ).


  ENDMETHOD.
ENDCLASS.
