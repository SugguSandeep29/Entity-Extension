class ZIN_CL_DUMMY_ACTION definition
  public
  final
  create public .

public section.

  interfaces IF_BADI_INTERFACE .
  interfaces IF_USMD_SSW_SYST_METHOD_CALLER .
protected section.
private section.
ENDCLASS.



CLASS ZIN_CL_DUMMY_ACTION IMPLEMENTATION.


  method IF_USMD_SSW_SYST_METHOD_CALLER~CALL_SYSTEM_METHOD.
  endmethod.
ENDCLASS.
