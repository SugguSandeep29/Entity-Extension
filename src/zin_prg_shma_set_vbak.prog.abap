*&---------------------------------------------------------------------*
*& Report ZIN_PRG_SHMA_SET_VBAK
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zin_prg_shma_set_vbak.

DATA: lr_area TYPE REF TO zin_cl_areahandle_area,
      lr_root TYPE REF TO zin_cl_shm_areahandle_root.

TRY.
    lr_area = zin_cl_areahandle_area=>attach_for_write( ).

    CREATE OBJECT lr_root AREA HANDLE lr_area.

    lr_root->set_vbak( ).

    lr_area->set_root( root = lr_root ).

    lr_area->detach_commit( ).


    CATCH cx_shm_wrong_handle.           " Incorrect Handle
    CATCH cx_shm_already_detached.       " Handle Already Released
    CATCH cx_shm_secondary_commit.       " Second Commit After Failed Commit
    CATCH cx_shm_event_execution_failed. " Incorrect Method Registered Under SHM_COMMIT/ROLLBACK_EVENT
    CATCH cx_shm_completion_error.       " Exception with Commit Preparation

  ENDTRY.
