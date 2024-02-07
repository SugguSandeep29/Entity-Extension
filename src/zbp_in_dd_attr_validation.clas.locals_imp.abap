CLASS lhc_zin_dd_attr_validation DEFINITION INHERITING FROM cl_abap_behavior_handler.
  PRIVATE SECTION.

    METHODS get_instance_authorizations FOR INSTANCE AUTHORIZATION
      IMPORTING keys REQUEST requested_authorizations FOR zin_dd_attr_validation RESULT result.

ENDCLASS.

CLASS lhc_zin_dd_attr_validation IMPLEMENTATION.

  METHOD get_instance_authorizations.
  ENDMETHOD.

ENDCLASS.
