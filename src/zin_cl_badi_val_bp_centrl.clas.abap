class ZIN_CL_BADI_VAL_BP_CENTRL definition
  public
  final
  create public .

public section.

  interfaces IF_BADI_INTERFACE .
  interfaces IF_EX_USMD_RULE_SERVICE .
protected section.
private section.
ENDCLASS.



CLASS ZIN_CL_BADI_VAL_BP_CENTRL IMPLEMENTATION.


  method IF_EX_USMD_RULE_SERVICE~CHECK_CREQUEST.
  endmethod.


  method IF_EX_USMD_RULE_SERVICE~CHECK_CREQUEST_FINAL.
  endmethod.


  method IF_EX_USMD_RULE_SERVICE~CHECK_CREQUEST_HIERARCHY.
  endmethod.


  method IF_EX_USMD_RULE_SERVICE~CHECK_CREQUEST_START.
  endmethod.


  method IF_EX_USMD_RULE_SERVICE~CHECK_EDITION.
  endmethod.


  method IF_EX_USMD_RULE_SERVICE~CHECK_EDITION_FINAL.
  endmethod.


  method IF_EX_USMD_RULE_SERVICE~CHECK_EDITION_HIERARCHY.
  endmethod.


  method IF_EX_USMD_RULE_SERVICE~CHECK_EDITION_START.
  endmethod.


  METHOD if_ex_usmd_rule_service~check_entity.
    IF it_data IS NOT INITIAL AND id_entitytype IS NOT INITIAL.
      io_model->create_data_reference(
        EXPORTING
          i_fieldname          = CONV #( id_entitytype )                  " Field Name
          i_struct             = if_usmd_model_ext=>gc_struct_key     " Structure
          if_table             = abap_false
        IMPORTING
          er_data              = DATA(lr_data)
      ).
      IF lr_data IS BOUND.

        io_model->read_entity_data_all(
          EXPORTING
            i_fieldname      = 'BP_HEADER'                 " Financial MDM: Field Name
            if_active        = abap_false                 " Financial MDM: General Indicator
            i_crequest       = id_crequest                 " Change Request
          IMPORTING
            et_data_entity   = DATA(lt_header)                 " Data for Entity Types
        ).

        DATA(ls_header) = lt_header[ usmd_entity = 'BP_HEADER' usmd_entity_cont = id_entitytype struct = 'KATTR'  ].

        ASSIGN ls_header-r_t_data->* TO FIELD-SYMBOL(<lt_header>).
        LOOP AT <lt_header> ASSIGNING FIELD-SYMBOL(<ls_header>).
          ASSIGN lr_data->* TO FIELD-SYMBOL(<ls_ref_data>).
          MOVE-CORRESPONDING <ls_header> TO <ls_ref_data>.
        ENDLOOP.

        LOOP AT it_data ASSIGNING FIELD-SYMBOL(<ls_data>).
          ASSIGN COMPONENT 'BU_SORT1' OF STRUCTURE <ls_data> TO FIELD-SYMBOL(<lv_sort1>).
          IF <lv_sort1> IS ASSIGNED AND <lv_sort1> IS INITIAL.
            et_message = VALUE #( ( msgid = 'ZIN_CL_MSG'
                                    msgno = '002'
                                    msgty = 'E'
                                    msgv1 = 'Sort Term1'
                                    row  = cl_mdg_bs_key_row_map_service=>set_key(
                                                                                        is_key       = lr_data
                                                                                        iv_entity    = id_entitytype
                                                                                        iv_fieldname = 'BU_SORT1'

                                                                                        ) ) ).
          ENDIF.
        ENDLOOP.
      ENDIF.
    ENDIF.
  ENDMETHOD.


  method IF_EX_USMD_RULE_SERVICE~CHECK_ENTITY_HIERARCHY.
  endmethod.


  method IF_EX_USMD_RULE_SERVICE~DERIVE_ENTITY.
  endmethod.
ENDCLASS.
