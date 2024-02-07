CLASS lcl_zin_enh_impl_creq_wd DEFINITION DEFERRED.
CLASS cl_usmd_edition_creq_gui_wd DEFINITION LOCAL FRIENDS lcl_zin_enh_impl_creq_wd.
CLASS lcl_zin_enh_impl_creq_wd DEFINITION.
  PUBLIC SECTION.
    CLASS-DATA obj TYPE REF TO lcl_zin_enh_impl_creq_wd.    "#EC NEEDED
    DATA core_object TYPE REF TO cl_usmd_edition_creq_gui_wd . "#EC NEEDED
 INTERFACES  IPR_ZIN_ENH_IMPL_CREQ_WD.
    METHODS:
      constructor IMPORTING core_object
                              TYPE REF TO cl_usmd_edition_creq_gui_wd OPTIONAL.
ENDCLASS.
CLASS lcl_zin_enh_impl_creq_wd IMPLEMENTATION.
  METHOD constructor.
    me->core_object = core_object.
  ENDMETHOD.

  METHOD ipr_zin_enh_impl_creq_wd~load_crequest.
*"------------------------------------------------------------------------*
*" Declaration of PRE-method, do not insert any comments here please!
*"
*"methods LOAD_CREQUEST
*"  importing
*"    !IO_CONTEXT type ref to IF_WD_CONTEXT_NODE .
*"------------------------------------------------------------------------*
    DATA: lt_fpm_search_criteria TYPE fpmgb_t_search_criteria,
          lv_option              TYPE char2 VALUE 'EQ',
          lo_area                TYPE REF TO zin_cl_areahandle_wd_var.

    TRY.

        lo_area = zin_cl_areahandle_wd_var=>attach_for_read( ).

        lt_fpm_search_criteria = lo_area->root->gt_sel.

        lo_area->detach( ).


      CATCH cx_shm_wrong_handle.     " Incorrect Handle
      CATCH cx_shm_already_detached. " Handle Already Released
      CATCH cx_shm_no_active_version.
      CATCH cx_shm_change_lock_active.
      CATCH cx_shm_exclusive_lock_active.
      CATCH cx_shm_inconsistent.
      CATCH cx_shm_read_lock_active.


    ENDTRY.

    LOOP AT lt_fpm_search_criteria INTO DATA(ls_fpm).
      core_object->dt_sel = VALUE #( BASE core_object->dt_sel ( fieldname = ls_fpm-search_attribute
                                                        option = 'EQ'
                                                        low = ls_fpm-low
                                                        high = ls_fpm-high
                                                        sign = ls_fpm-sign ) ).
    ENDLOOP.

  ENDMETHOD.
ENDCLASS.
