class ZIN_CL_BADI_UI_FLD_PROP definition
  public
  final
  create public .

public section.

  interfaces IF_BADI_INTERFACE .
  interfaces IF_EX_USMD_ACC_FLD_PROP_CDS .
protected section.
private section.
ENDCLASS.



CLASS ZIN_CL_BADI_UI_FLD_PROP IMPLEMENTATION.


  method IF_EX_USMD_ACC_FLD_PROP_CDS~IS_FIELD_PROP_HIDDEN_SUPPORTED.
  endmethod.


  method IF_EX_USMD_ACC_FLD_PROP_CDS~MODIFY_ENTITY_PROPERTIES.
  endmethod.


  METHOD if_ex_usmd_acc_fld_prop_cds~modify_fld_prop_attr.

    IF iv_entity EQ 'BP_CENTRL'.
      ASSIGN ct_fld_prop TO FIELD-SYMBOL(<lt_prop>).
      LOOP AT <lt_prop> ASSIGNING FIELD-SYMBOL(<ls_prop>).
        ASSIGN COMPONENT 'USMD_FP' OF STRUCTURE <ls_prop> TO FIELD-SYMBOL(<ls_prop_fld>).
        ASSIGN COMPONENT 'BU_SORT1' OF STRUCTURE <ls_prop_fld> to FIELD-SYMBOL(<lv_field>).
        <lv_field> = 'M'.
      ENDLOOP.
    ENDIF.

  ENDMETHOD.
ENDCLASS.
