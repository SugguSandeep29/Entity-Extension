*&---------------------------------------------------------------------*
*& Report ZIN_PRG_SHMA_GET_VBAK
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zin_prg_shma_get_vbak.

DATA: lr_area TYPE REF TO zin_cl_areahandle_area.

TRY.

    lr_area = zin_cl_areahandle_area=>attach_for_read( ).

    lr_area->root->get_vbak(
      RECEIVING
        rt_vbak = DATA(lt_vbak)                 " Table Type for Structure VBAK
    ).

    cl_demo_output=>display( lt_vbak ).

  CATCH cx_shm_no_active_version.

ENDTRY.
