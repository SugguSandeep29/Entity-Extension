class ZIN_CL_SHMA_WEB_VARIANT definition
  public
  final
  create public
  shared memory enabled .

public section.

  interfaces IF_SHM_BUILD_INSTANCE .

  data GT_SEL type FPMGB_T_SEARCH_CRITERIA .
protected section.
private section.
ENDCLASS.



CLASS ZIN_CL_SHMA_WEB_VARIANT IMPLEMENTATION.


  method IF_SHM_BUILD_INSTANCE~BUILD.
  endmethod.
ENDCLASS.
