class ZIN_CL_BADI_CRTYPE_INT definition
  public
  final
  create public .

public section.

  interfaces IF_BADI_INTERFACE .
  interfaces IF_EX_USMD_CREQUEST_INTEGR .
protected section.
private section.

  methods GET_BUS_ACTIVITY
    importing
      !IT_CREQUEST_TYPE type USMD_TS_CREQUEST_TYPE
    returning
      value(RV_USMD_PROCESS) type USMD_PROCESS .
  methods FILTER_CUSTOMER_CRTYPE
    changing
      !CT_CREQUEST_TYPE type USMD_TS_CREQUEST_TYPE .
  methods FILTER_SUPPLIER_CRTYPE
    changing
      !CT_CREQUEST_TYPE type USMD_TS_CREQUEST_TYPE .
ENDCLASS.



CLASS ZIN_CL_BADI_CRTYPE_INT IMPLEMENTATION.


  METHOD filter_customer_crtype.
    DELETE ct_crequest_type WHERE table_line NE 'ZCUST1P1' AND table_line NE 'ZCUST2P1'.
  ENDMETHOD.


  method FILTER_SUPPLIER_CRTYPE.
    DELETE ct_crequest_type WHERE table_line NE 'ZINSUP01' AND table_line NE 'ZBPHSUP1'.
  endmethod.


  METHOD get_bus_activity.

    SELECT SINGLE usmd_process FROM usmd1601 WHERE usmd_creq_type = @( it_crequest_type[ 1 ] ) INTO @rv_usmd_process.

  ENDMETHOD.


  method IF_EX_USMD_CREQUEST_INTEGR~CHECK_CREQUEST_SUBMIT_FOR_NOTE.
  endmethod.


  METHOD if_ex_usmd_crequest_integr~filter_crequest_type.
    IF sy-uname = 'INGALEA'.

      DATA(lv_usmd_process) =  me->get_bus_activity(
                                EXPORTING
                                it_crequest_type = ct_crequest_type ).

      IF lv_usmd_process = 'CUP1'.
        me->filter_customer_crtype(
          CHANGING
            ct_crequest_type = ct_crequest_type                 " Type of Change Request
        ).

      ELSEIF lv_usmd_process = 'SUP1'.
        me->filter_supplier_crtype(
          CHANGING
            ct_crequest_type = ct_crequest_type                 " Type of Change Request
        ).

      ENDIF.
    ENDIF.
  ENDMETHOD.


  method IF_EX_USMD_CREQUEST_INTEGR~GET_CREQUEST_ATTRIBUTES.
  endmethod.


  method IF_EX_USMD_CREQUEST_INTEGR~GET_CREQUEST_FLD_PROP.
  endmethod.
ENDCLASS.
