*&---------------------------------------------------------------------*
*& Report zin_prg_create_ddic_tab
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zin_prg_create_ddic_tab.

***Selection Screen
PARAMETERS: p_tab  TYPE ddobjname,
            p_desc TYPE ddtext.

** Data Declaration
DATA: lt_fields     TYPE TABLE OF dd03p,
      ls_tab_header TYPE dd02v.

** Assigning values to the workarea of table header.

ls_tab_header = VALUE #(
                         tabname    = p_tab             " Table name
                         ddlanguage = sy-langu          " Language
                         tabclass   = 'INTTAB'          " TRANSP = Table ; INTTAB = Structure
                         ddtext     = p_desc            " Description of Table
                         masterlang = sy-langu
                                           ).

** Inserting records to the fields table

lt_fields = VALUE #(
                     ( tabname = p_tab
                       fieldname = 'EMP_ID'
                       ddlanguage = sy-langu
                       position = 0001
                       rollname = 'PERSNO' )

                     ( tabname = p_tab
                       fieldname = 'FIRST_NAME'
                       ddlanguage = sy-langu
                       position = 0002
                       rollname = 'PAD_VORNA' )

                     ( tabname = p_tab
                       fieldname = 'SECOND_NAME'
                       ddlanguage = sy-langu
                       position = 0003
                       rollname = 'PAD_NACH2' )

                     ( tabname = p_tab
                       fieldname = 'LAST_NAME'
                       ddlanguage = sy-langu
                       position = 0004
                       rollname = 'PAD_NACHN' )

                     ( tabname = p_tab
                       fieldname = 'GENDER'
                       ddlanguage = sy-langu
                       position = 0005
                       rollname = 'HRPAD_GENDER' )

                     ( tabname = p_tab
                       fieldname = 'DOB'
                       ddlanguage = sy-langu
                       position = 0006
                       rollname = 'GBDAT' )
                                       ) .

** This function will crate database table/structure

CALL FUNCTION 'DDIF_TABL_PUT'
  EXPORTING
    name              = p_tab
    dd02v_wa          = ls_tab_header
*   dd09l_wa          = ls_tab_setting
  TABLES
    dd03p_tab         = lt_fields
  EXCEPTIONS
    tabl_not_found    = 1
    name_inconsistent = 2
    tabl_inconsistent = 3
    put_failure       = 4
    put_refused       = 5
    OTHERS            = 6.

IF sy-subrc = 0.
  WRITE: 'Table Created Successfully'.
ENDIF.

** This function will activate the table/structure

CALL FUNCTION 'DDIF_TABL_ACTIVATE'
  EXPORTING
    name        = p_tab
  EXCEPTIONS
    not_found   = 1
    put_failure = 2
    OTHERS      = 3.

IF sy-subrc <> 0.
  WRITE: / 'Table could not be activated'.
ELSE.
  WRITE: / 'Table Activated Successfully'.
ENDIF.

** This function will assign above table into package

** To save in $TMP
*CALL FUNCTION 'TR_TADIR_INTERFACE'
*  EXPORTING
*    wi_test_modus                  = abap_false
*    wi_tadir_pgmid                 = 'R3TR'
*    wi_tadir_object                = 'TABL'
*    wi_tadir_obj_name              = CONV sobj_name( p_tab )
*    wi_set_genflag                 = abap_true
*    wi_tadir_masterlang            = sy-langu
*  EXCEPTIONS
*    tadir_entry_not_existing       = 1
*    tadir_entry_ill_type           = 2
*    no_systemname                  = 3
*    no_systemtype                  = 4
*    original_system_conflict       = 5
*    object_reserved_for_devclass   = 6
*    object_exists_global           = 7
*    object_exists_local            = 8
*    object_is_distributed          = 9
*    obj_specification_not_unique   = 10
*    no_authorization_to_delete     = 11
*    devclass_not_existing          = 12
*    simultanious_set_remove_repair = 13
*    order_missing                  = 14
*    no_modification_of_head_syst   = 15
*    pgmid_object_not_allowed       = 16
*    masterlanguage_not_specified   = 17
*    devclass_not_specified         = 18
*    specify_owner_unique           = 19
*    loc_priv_objs_no_repair        = 20
*    gtadir_not_reached             = 21
*    object_locked_for_order        = 22
*    change_of_class_not_allowed    = 23
*    no_change_from_sap_to_tmp      = 24
*    OTHERS                         = 25.
*IF sy-subrc = 0.
*  WRITE: / 'The object saved in package successfully'.
*ENDIF.

**********************************************************************
*DATA: ls_tadir    TYPE tadir,
*      ls_tdevc    TYPE tdevc,
*      lv_obj_name TYPE e071-obj_name.
*
*lv_obj_name = p_tab.

*CALL FUNCTION 'TR_TADIR_POPUP_ENTRY_E071'
*  EXPORTING
*    wi_e071_pgmid             = 'R3TR'                                  " E071-PGMID of requested TADIR entry
*    wi_e071_object            = 'TABL'                                  " E071-OBJECT of requested TADIR entry
*    wi_e071_obj_name          = lv_obj_name                 " E071-OBJ_NAME of requested TADIR entry
**   wi_e071_gennum            = ' '              " E071-GENNUM of the requested TADIR entry
**   wi_message_enter_devclass = ' '              " Send message 'Maintain package'
**   wi_read_only              = ' '              " TADIR in display mode
*    wi_tadir_devclass         = 'ZIN_MDG'            " TADIR_DEVCLASS (default for creating new TADIR)
**   wi_tadir_genflag          = ' '              " TADIR-GENFLAG (generated object) (X/T/U/ )
*    wi_tadir_masterlang       = sy-langu              " TADIR-MASTERLANG (master language)
**   iv_suppress_mod_show      = ' '              " Suppress display-change function
*  IMPORTING
*    we_tadir                  = ls_tadir                 " Output string for TADIR
*    es_tdevc                  = ls_tdevc
**   ev_deleted                =                  " General Indicator
*  EXCEPTIONS
*    display_mode              = 1                " Only display object
*    exit                      = 2                " Function canceled
*    global_tadir_insert_error = 3                " Error inserting into global TADIR
*    no_repair_selected        = 4                " No repair request selected
*    no_systemname             = 5                " System name cannot be determined or invalid length
*    no_systemtype             = 6                " System type cannot be determined
*    no_tadir_type             = 7                " Object type cannot be included in TADIR
*    reserved_name             = 8                " Violation of reserved TRESN names
*    tadir_enqueue_failed      = 9                " TADIR locked by short-term lock
*    devclass_not_found        = 10               " Package not in TDEVC
*    tadir_not_exist           = 11               " TADIR does not exist, deletion rejected
*    object_exists             = 12               " Object exists, deletion rejected
*    internal_error            = 13               " Internal program error
*    object_append_error       = 14               " Error when locking and saving the E071 entries
*    tadir_modify_error        = 15               " Error when saving TADIR entry
*    object_locked             = 16               " Object locked by external request lock
*    no_object_authority       = 17               " No authorization to execute the function
*    OTHERS                    = 18.
*IF sy-subrc <> 0.
**   MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
**     WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
*ELSE.
*  cl_demo_output=>write( ls_tadir ).
*  cl_demo_output=>display( ls_tdevc ).
*
*ENDIF.


** To save in package

*CALL FUNCTION 'TR_TADIR_INTERFACE'
*  EXPORTING
*   wi_delete_tadir_entry          = ' '              " X - delete object directory entry
*   wi_remove_repair_flag          = ' '              " X - reset repair flag
*   wi_set_repair_flag             = ' '              " X - set repair flag
*    wi_test_modus                  = abap_false             " X - simulation mode (no update)
*    wi_tadir_pgmid                 = 'R3TR'                 " Input for TADIR field PGMID
*    wi_tadir_object                = 'TABL'                 " Input for TADIR field OBJECT
*    wi_tadir_obj_name              = CONV sobj_name( p_tab )                 " Input for TADIR field OBJ_NAME
*    wi_tadir_korrnum               = 'S4XK913881'              " Transport Organizer internal
*   wi_tadir_srcsystem             = sy-sysid              " Input for TADIR field SRCSYSTEM
*    wi_tadir_author                = 'INGALEA'              " Input for TADIR field AUTHOR
*    wi_tadir_devclass              = 'ZIN_MDG'              " Input for TADIR field DEVCLASS
*    wi_tadir_masterlang            = sy-langu              " Input for TADIR field MASTERLANG
*   wi_tadir_cproject              = ' '              " Transport Organizer internal
*   wi_tadir_versid                = ' '              " Transport Organizer internal
*   wi_remove_genflag              = ' '              " X - delete generation flag
*    wi_set_genflag                 = abap_true              " Set generation flag (see documentation)
*   wi_read_only                   = ' '              " Read object directory entry
*   iv_set_edtflag                 = ' '              " Editing lock for standard editors
*   iv_transl_tech_text            = ' '              " Translate Technical Texts into Development Language
*   iv_delflag                     = ' '              " Deletion Flag
*   iv_no_pak_check                = ' '              " Exception Indicator for Package Check
*   iv_obj_stability               = ' '              " Release Status of a Development Object
*   wi_tadir_check_date            = ''               " SCC: Check Date
*   wi_tadir_check_cfg             = ' '              " SCC: Check Configuration
*  IMPORTING
*   new_gtadir_entry               =                  " Entry in the global TADIR
*   new_tadir_entry                =                  " Modified TADIR entry
*  EXCEPTIONS
*    tadir_entry_not_existing       = 1                " Object directory entry does not exist
*    tadir_entry_ill_type           = 2                " Transferred TADIR key not compatible with E071
*    no_systemname                  = 3                " System name not found
*    no_systemtype                  = 4                " System type not defined
*    original_system_conflict       = 5                " Object already exists in another system
*    object_reserved_for_devclass   = 6                " Object reserved for name range
*    object_exists_global           = 7                " Object exists globally
*    object_exists_local            = 8                " Object exists locally
*    object_is_distributed          = 9                " Object is distributed
*    obj_specification_not_unique   = 10               " Object specification for import is not sufficient
*    no_authorization_to_delete     = 11               " No permission to delete
*    devclass_not_existing          = 12               " Package unknown
*    simultanious_set_remove_repair = 13               " Repair flag set/reset simultaneously
*    order_missing                  = 14               " Repair request was not transferred
*    no_modification_of_head_syst   = 15               " Modification of HEAD-SYST entry not allowed
*    pgmid_object_not_allowed       = 16               " PGMID entry not permitted
*    masterlanguage_not_specified   = 17               " Master language not specified
*    devclass_not_specified         = 18               " Package not specified
*    specify_owner_unique           = 19
*    loc_priv_objs_no_repair        = 20               " No repair to local-private objects
*    gtadir_not_reached             = 21               " The GTADIR cannot be accessed
*    object_locked_for_order        = 22
*    change_of_class_not_allowed    = 23
*    no_change_from_sap_to_tmp      = 24               " Do not switch SAP objects to customer development class
*    OTHERS                         = 25.
*IF sy-subrc <> 0.
*  MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
*    WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
*ELSE.
*
*  WRITE: / 'The object saved in package successfully'.
*
*
*ENDIF.

**********************************************************************

DATA: lt_ko200 TYPE TABLE OF ko200.

APPEND VALUE #(
                trkorr = 'S4XK913881'
                pgmid = 'R3TR'
                object = 'TABL'
                obj_name = p_tab
                objfunc = ' '  ) TO lt_ko200.

CALL FUNCTION 'TR_OBJECTS_CHECK'
  TABLES
    wt_ko200                = lt_ko200                 " Input table for objects to be edited
  EXCEPTIONS
    cancel_edit_other_error = 1                " Cancel
    show_only_other_error   = 2                " Cancel, user wants to go to display mode
    OTHERS                  = 3.
IF sy-subrc <> 0.
  MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
    WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
ENDIF.

DATA: lv_request TYPE char20.
CALL FUNCTION 'TR_OBJECTS_INSERT'
  EXPORTING
    wi_order                = 'S4XK913881'
  IMPORTING
    we_order                = lv_request
  TABLES
    wt_ko200                = lt_ko200                 " Input table of edited objects
  EXCEPTIONS
    cancel_edit_other_error = 1                " Cancel
    show_only_other_error   = 2                " Cancel, user wants to go to display mode
    OTHERS                  = 3.
IF sy-subrc <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*   WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
ELSE.
  WRITE: / 'Passed to TR', lv_request.
ENDIF.
