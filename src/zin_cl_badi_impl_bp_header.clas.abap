class ZIN_CL_BADI_IMPL_BP_HEADER definition
  public
  final
  create public .

public section.

  interfaces IF_BADI_INTERFACE .
  interfaces IF_EX_USMD_RULE_SERVICE .
protected section.
private section.
ENDCLASS.



CLASS ZIN_CL_BADI_IMPL_BP_HEADER IMPLEMENTATION.


  method IF_EX_USMD_RULE_SERVICE~CHECK_ENTITY.

     CALL METHOD io_model->create_data_reference
      EXPORTING
        i_fieldname = CONV #( id_entitytype )                  " Field Name
        i_struct    = if_usmd_model_ext=>gc_struct_key         " Structure
        if_table    = ''                                       " Financial MDM: General Indicator
      IMPORTING
        er_data     = DATA(lr_key).

    IF lr_key IS BOUND.

      CALL METHOD io_model->read_entity_data_all
        EXPORTING
          i_fieldname    = CONV #( id_entitytype )    " Financial MDM: Field Name
          if_active      = abap_false                 " Financial MDM: General Indicator
          i_crequest     = id_crequest                " Change Request
        IMPORTING
          et_data_entity = DATA(lt_data).             " Data for Entity Types

      IF lt_data IS NOT INITIAL.
        READ TABLE lt_data WITH KEY usmd_entity = 'BP_HEADER' usmd_entity_cont = id_entitytype struct = 'KATTR' INTO DATA(ls_data).

        ASSIGN ls_data-r_t_data->* TO FIELD-SYMBOL(<lt_data>).
        LOOP AT <lt_data> ASSIGNING FIELD-SYMBOL(<ls_data>).

          ASSIGN lr_key->* TO FIELD-SYMBOL(<ls_key>).
          MOVE-CORRESPONDING <ls_data> TO <ls_key>.

        ENDLOOP.

        IF lines( it_data ) NE 0.
          LOOP AT it_data ASSIGNING FIELD-SYMBOL(<ls_itdata>).
            IF <ls_itdata> IS ASSIGNED.
              ASSIGN COMPONENT 'BU_GROUP' OF STRUCTURE <ls_itdata> TO FIELD-SYMBOL(<lv_group>).
              IF <lv_group> IS ASSIGNED.
                IF <lv_group> IS INITIAL.

                  et_message =  VALUE #( ( msgid = 'ZIN_CL_MSG'
                                           msgty = 'E'
                                           msgno = '003'
                                           row = cl_mdg_bs_key_row_map_service=>set_key(
                                                                                        is_key       = lr_key
                                                                                        iv_entity    = id_entitytype
                                                                                        iv_fieldname = 'BU_GROUP'
                                                                                        ) ) ).
                ENDIF.
              ENDIF.

            ENDIF.
          ENDLOOP.
        ENDIF.

      ENDIF.

    ENDIF.


  endmethod.
ENDCLASS.
