*&---------------------------------------------------------------------*
*& Report zin_prg_create_ddic_tab
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zin_prg_create_ddic_struc.

***Selection Screen
PARAMETERS: p_tab  TYPE ddobjname,
            p_desc TYPE ddtext.

TYPES: BEGIN OF ty_table,
         fieldname TYPE fieldname,
         rollname  TYPE rollname,
       END OF ty_table.

** Data Declaration
DATA: lt_fields     TYPE TABLE OF dd03p,
      lt_table      TYPE TABLE OF ty_table,
      ls_tab_header TYPE dd02v,
      lv_position   TYPE numc4.


lt_table = VALUE #(
                    ( fieldname = 'EMP_ID'      rollname = 'PERSNO' )
                    ( fieldname = 'FIRST_NAME'  rollname = 'PAD_VORNA' )
                    ( fieldname = 'SECOND_NAME' rollname = 'PAD_NACH2' )
                    ( fieldname = 'LAST_NAME'   rollname = 'PAD_NACHN' )
                    ( fieldname = 'GENDER'      rollname = 'HRPAD_GENDER' )
                    ( fieldname = 'DOB'         rollname = 'GBDAT' )
                                     ).

** Assigning values to the workarea of table header.

ls_tab_header = VALUE #(
                         tabname    = p_tab             " Table name
                         ddlanguage = sy-langu          " Language
                         tabclass   = 'INTTAB'          " TRANSP = Table ; INTTAB = Structure
                         ddtext     = p_desc            " Description of Table
                         masterlang = sy-langu
                         exclass = '3'
                                           ).

** Inserting records to the fields table
LOOP AT lt_table ASSIGNING FIELD-SYMBOL(<ls_table>).
  lv_position += 1.
  lt_fields = VALUE #( BASE lt_fields
                       ( tabname = p_tab
                         fieldname = <ls_table>-fieldname
                         ddlanguage = sy-langu
                         position = lv_position
                         rollname = <ls_table>-rollname )
                        ) .

ENDLOOP.
** This function will crate database table/structure

CALL FUNCTION 'DDIF_TABL_PUT'
  EXPORTING
    name              = p_tab
    dd02v_wa          = ls_tab_header
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
  WRITE: / 'Table Activation failed'.
ELSE.
  WRITE: / 'Table Activated Successfully'.
ENDIF.

**********************************************************************
** To get Package list

DATA: lv_choise TYPE sy-tabix,
      lv_title  TYPE char80.

SELECT tdevc~devclass,
       tdevct~ctext
      FROM tdevc
      INNER JOIN tdevct
      ON tdevc~devclass = tdevct~devclass
      INTO TABLE @DATA(lt_pack)
      WHERE created_by = @sy-uname AND
            spras = @sy-langu.

APPEND VALUE #( devclass = '$TMP' ctext = 'Local Package' ) TO lt_pack.

lv_title = 'Select Package'.

CALL FUNCTION 'POPUP_TABLE_DISPLAY'
  EXPORTING
    endpos_col   = 80                 " Ending position of popup
    endpos_row   = 20                 " Ending position of popup
    startpos_col = 10                 " Starting position of popup
    startpos_row = 10                 " Starting position of popup
    titletext    = lv_title                 " Text in title bar of popup
  IMPORTING
    choise       = lv_choise                " Number of table entry
  TABLES
    valuetab     = lt_pack                 " Table with possible values
  EXCEPTIONS
    break_off    = 1
    OTHERS       = 2.
IF sy-subrc <> 0.
  MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
    WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
ELSE.
  DATA(lv_pkg) = lt_pack[ lv_choise ]-devclass.
ENDIF.

CLEAR: lv_choise, lt_pack, lv_title.
*
***********************************************************************
*** To get Trnasport Requests
*
SELECT e070~strkorr,
       e07t~as4text
   FROM e070
  INNER JOIN e07t
  ON e070~trkorr = e07t~trkorr
  INTO TABLE @DATA(lt_req)
  WHERE
  e070~korrdev = 'SYST' AND
  e070~as4user = @sy-uname AND
  e070~strkorr NE ''.

lv_title = 'Select Transport Request'.

CALL FUNCTION 'POPUP_TABLE_DISPLAY'
  EXPORTING
    endpos_col   = 80                 " Ending position of popup
    endpos_row   = 20                 " Ending position of popup
    startpos_col = 10                 " Starting position of popup
    startpos_row = 10                 " Starting position of popup
    titletext    = lv_title                " Text in title bar of popup
  IMPORTING
    choise       = lv_choise                " Number of table entry
  TABLES
    valuetab     = lt_req                 " Table with possible values
  EXCEPTIONS
    break_off    = 1
    OTHERS       = 2.
IF sy-subrc <> 0.
  MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
    WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
ELSE.
  DATA(lv_req) = lt_req[ lv_choise ]-strkorr.
ENDIF.

CLEAR: lv_choise, lt_pack.

**********************************************************************
** To save in package

CALL FUNCTION 'TR_TADIR_INTERFACE'
  EXPORTING
    wi_test_modus                  = abap_false             " X - simulation mode (no update)
    wi_tadir_pgmid                 = 'R3TR'                 " Input for TADIR field PGMID
    wi_tadir_object                = 'TABL'                 " Input for TADIR field OBJECT
    wi_tadir_obj_name              = CONV sobj_name( p_tab )                 " Input for TADIR field OBJ_NAME
    wi_tadir_author                = 'INGALEA'              " Input for TADIR field AUTHOR
    wi_tadir_devclass              = lv_pkg               " Input for TADIR field DEVCLASS
    wi_tadir_masterlang            = sy-langu              " Input for TADIR field MASTERLANG
    wi_set_genflag                 = abap_true              " Set generation flag (see documentation)
  EXCEPTIONS
    tadir_entry_not_existing       = 1                " Object directory entry does not exist
    tadir_entry_ill_type           = 2                " Transferred TADIR key not compatible with E071
    no_systemname                  = 3                " System name not found
    no_systemtype                  = 4                " System type not defined
    original_system_conflict       = 5                " Object already exists in another system
    object_reserved_for_devclass   = 6                " Object reserved for name range
    object_exists_global           = 7                " Object exists globally
    object_exists_local            = 8                " Object exists locally
    object_is_distributed          = 9                " Object is distributed
    obj_specification_not_unique   = 10               " Object specification for import is not sufficient
    no_authorization_to_delete     = 11               " No permission to delete
    devclass_not_existing          = 12               " Package unknown
    simultanious_set_remove_repair = 13               " Repair flag set/reset simultaneously
    order_missing                  = 14               " Repair request was not transferred
    no_modification_of_head_syst   = 15               " Modification of HEAD-SYST entry not allowed
    pgmid_object_not_allowed       = 16               " PGMID entry not permitted
    masterlanguage_not_specified   = 17               " Master language not specified
    devclass_not_specified         = 18               " Package not specified
    specify_owner_unique           = 19
    loc_priv_objs_no_repair        = 20               " No repair to local-private objects
    gtadir_not_reached             = 21               " The GTADIR cannot be accessed
    object_locked_for_order        = 22
    change_of_class_not_allowed    = 23
    no_change_from_sap_to_tmp      = 24               " Do not switch SAP objects to customer development class
    OTHERS                         = 25.
IF sy-subrc <> 0.
  MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
    WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
ELSE.
  WRITE: / 'The object saved in package successfully'.
ENDIF.

**********************************************************************
*** To get Transport request number.

DATA: lt_ko200 TYPE TABLE OF ko200.

CLEAR lt_ko200.

APPEND VALUE #(
*                trkorr = 'S4XK913881'    " S4XK913881   S4XK911213
                pgmid = 'R3TR'
                object = 'TABL'
                obj_name = p_tab
*                devclass = '$TMP'
                ) TO lt_ko200.

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
    wi_order                = lv_req
  IMPORTING
    we_order                = lv_request
  TABLES
    wt_ko200                = lt_ko200                 " Input table of edited objects
  EXCEPTIONS
    cancel_edit_other_error = 1                " Cancel
    show_only_other_error   = 2                " Cancel, user wants to go to display mode
    OTHERS                  = 3.
IF sy-subrc <> 0.
  MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
    WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
ELSE.
  WRITE: / 'Passed to TR', lv_request.
ENDIF.

**********************************************************************

**********************************************************************
** To get the popup for selecting Package ( Standard Popup ).

*DATA: ls_tadir TYPE tadir.
*
*CALL FUNCTION 'TR_TADIR_POPUP_ENTRY_E071'
*  EXPORTING
*    wi_e071_pgmid             = 'R3TR'                                  " E071-PGMID of requested TADIR entry
*    wi_e071_object            = 'TABL'                                  " E071-OBJECT of requested TADIR entry
*    wi_e071_obj_name          = CONV trobj_name( p_tab )                " E071-OBJ_NAME of requested TADIR entry
*    wi_tadir_masterlang       = sy-langu                                " TADIR-MASTERLANG (master language)
*  IMPORTING
*    we_tadir                  = ls_tadir                                 " Output string for TADIR
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
*
*
*IF sy-subrc <> 0.
*   MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*     WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
*ENDIF.
**********************************************************************
**********************************************************************
**DATA: lt_e071      TYPE TABLE OF e071,
**      ls_tadir     TYPE tadir,
**      ls_tadir_old TYPE tadir,
**      ls_tadir_new TYPE tadir,
**      lv_obj_name  TYPE e071-obj_name.
**
**lv_obj_name = p_tab.
**
**ls_tadir = VALUE #(
**                    pgmid = 'R3TR'
**                    object = 'TABL'
**                    obj_name = lv_obj_name
**                    devclass = 'ZIN_MDG'
**                    genflag = 'X'
**                    ).
**
**APPEND VALUE #(
**                trkorr = 'S4XK913881'
**                pgmid = 'R3TR'
**                obj_name = 'TABL'
**                objfunc = 'lv_obj_name'
**                 ) TO lt_e071.
**
**
**CALL FUNCTION 'TRINT_TADIR_POPUP'
**  EXPORTING
***   wi_existence_check           = 'X'              " X with/ ' ' without existence check
***   wi_masterflag                = ' '              " X - behavior as for DDIC
***   wi_message_enter_devclass    = ' '              " Send message 'Maintain development class'
***   wi_read_only                 = ' '              " TADIR in display mode
**    wi_tadir                     = ls_tadir                 " Input string TADIR (database version)
***   wi_tdevc                     =                  " Input string TDEVC (database version)
***   wi_no_tadir                  = ' '              " X - no TADIR exists in the database
***   wi_no_tdevc                  = ' '              " X - no TDEVC exists in the database
***   wi_no_delete_function        = ' '              " X - do not offer TADIR deletion function
***   iv_name_too_long_for_old     = ' '
***   iv_namespace                 = ' '
***   iv_wbo_internal              = ' '
***   iv_rep_call                  = ' '
***   iv_suppress_mod_show         = ' '              " General Indicator
***   iv_nonstandard_chars         = ' '
***   iv_show_only_check_like_ins  = ' '              " General Indicator
**  IMPORTING
**    we_tadir_old                 = ls_tadir_old                 " Old TADIR contents
**    we_tadir_new                 = ls_tadir_new                 " New TADIR contentgs
***   ev_tdevc                     =                 " new development class (TDEVC)
***   we_delete_tadir              =                  " Flag for deletion of TADIR entry
***   we_repair_flag_set           =                  " "Object in repair" flag set
***   we_repair_flag_removed       =                  " "Object in repair" flag reset
***   we_generated_flag_set        =                  " Generated flag set
***   we_generate_flag_removed     =                  " "Generated object" flag reset
***   ev_object_transport          =                  " X - transport of TT_E071 required
***   ev_edit_task                 =                  " possibly edit/share task
**  TABLES
**    tt_e071                      = lt_e071                  " Table of objects to be transported
**  EXCEPTIONS
**    display_mode                 = 1                " Only display object
**    exit                         = 2                " Function was cancelled
**    global_tadir_insert_error    = 3                " Error occurred inserting into global TADIR
**    no_systemname                = 4                " System name cannot be determined or invalid length
**    no_systemtype                = 5                " System type cannot be determined
**    no_tadir_type                = 6                " Object type cannot be included in TADIR
**    reserved_name                = 7                " Violation of reserved TRESN names
**    tadir_not_exist              = 8                " TADIR does not exist, display not possible
**    close                        = 9                " Display window closed (continue)
**    no_object_authority          = 10               " no authorization to execute the function
**    no_modification_of_head_syst = 11
**    obj_specification_not_unique = 12
**    pgmid_object_not_allowed     = 13
**    object_reserved_for_devclass = 14
**    OTHERS                       = 15.
**IF sy-subrc <> 0.
**  MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
**    WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
**ELSE.
**  cl_demo_output=>write( ls_tadir_old ).
**  cl_demo_output=>write( ls_tadir_new ).
**  cl_demo_output=>display( lt_e071 ).
**ENDIF.
** This function will assign above table into package

**********************************************************************
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



**********************************************************************

*
*CALL FUNCTION 'TRINT_OBJECTS_CHECK_AND_INSERT'
*    EXPORTING
*      iv_order                     = wi_order
*      iv_with_dialog               = 'X'
*      iv_send_message              = 'X'
*      iv_no_show_option            = iv_no_show_option
*      iv_no_standard_editor        = iv_no_standard_editor
*      iv_externalps                = iv_externalps
*      iv_externalid                = iv_externalid
*      iv_no_ps                     = iv_no_ps
*      iv_read_activity_from_memory = iv_read_activity_from_memory
*      it_obj_entries               = it_obj_entries
*    IMPORTING
*      ev_order                     = we_order
*      ev_task                      = we_task
*      et_tadir                     = tt_tadir[]
*    CHANGING
*      ct_ko200                     = wt_ko200[]
*      ct_e071k                     = wt_e071k[]
*      ct_e071k_str                 = it_e071k_str
*    EXCEPTIONS
*      show_only_user_after_error   = 1
*      cancel_edit_user_after_error = 2
*      OTHERS                       = 2.
