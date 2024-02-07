class ZIN_CL_SMT_FLD_CHK definition
  public
  final
  create public .

public section.

  interfaces IF_SMT_CHECK .
protected section.
private section.
ENDCLASS.



CLASS ZIN_CL_SMT_FLD_CHK IMPLEMENTATION.


  METHOD if_smt_check~check.
    IF i_source IS INITIAL.
      DATA(l_protocol) = cl_smt_protocol=>create_protocol_for_message( i_msgid = 'ZIN_CL_MSG'
                       i_msgty = 'E'
                       i_msgno = 003
                       i_msgv1 = 'Nickname is Initial'
                       i_msgv2 = 'Please Provide the value in Nickname'
                       i_msgv3 = ''
                       i_msgv4 = '' ).
      RAISE EXCEPTION TYPE cx_smt_transformation_error EXPORTING a_protocol = l_protocol.
    ELSE.
      RETURN.
    ENDIF.

  ENDMETHOD.
ENDCLASS.
