class ZIN_CL_MM_ZBUPA01_TRAN definition
  public
  final
  create public .

public section.

  interfaces IF_SMT_PREFETCH .
  interfaces IF_SMT_TRANSFORMATION .

  class-methods MAP_NICKNAME
    importing
      !I_NICKNAME type NICKNAME
      !I_PREFETCH type SMT_BOOLEAN
    exporting
      !E_NICKNAME type NICKNAME .
protected section.
private section.
ENDCLASS.



CLASS ZIN_CL_MM_ZBUPA01_TRAN IMPLEMENTATION.


  METHOD if_smt_prefetch~end_of_prefetch.
    RETURN.
  ENDMETHOD.


  METHOD if_smt_prefetch~start_of_prefetch.
    RETURN.
  ENDMETHOD.


  METHOD map_nickname.
    IF i_nickname = 'Hero Honda'.
      e_nickname = 'Hero'.
    ELSE.
      RETURN.
    ENDIF.
  ENDMETHOD.
ENDCLASS.
