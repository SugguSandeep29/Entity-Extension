class ZIN_CL_SHM_AREAHANDLE_ROOT definition
  public
  final
  create public
  shared memory enabled .

public section.

  interfaces IF_SHM_BUILD_INSTANCE .

  data GT_VBAK type VBAK_T .

  methods GET_VBAK
    returning
      value(RT_VBAK) type VBAK_T .
  methods SET_VBAK
    importing
      !IT_SO_VBAK type SHP_VBELN_RANGE_T .
protected section.
private section.
ENDCLASS.



CLASS ZIN_CL_SHM_AREAHANDLE_ROOT IMPLEMENTATION.


  method GET_VBAK.
    rt_vbak[] = gt_vbak[].
  endmethod.


  method IF_SHM_BUILD_INSTANCE~BUILD.

  endmethod.


  METHOD set_vbak.
    SELECT * FROM vbak
      INTO TABLE gt_vbak
      UP TO 10 ROWS
      WHERE vbeln IN it_so_vbak.
  ENDMETHOD.
ENDCLASS.
