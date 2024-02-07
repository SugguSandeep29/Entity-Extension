class ZIN_CL_CUST_ECC_HANDLER definition
  public
  inheriting from CL_MDG_BS_ECC_HANDLER
  final
  create public .

public section.

  methods CONSTRUCTOR .

  methods IF_MDG_BS_BP_ACCESS_HANDLER~CHECK_ENTITY_DATA
    redefinition .
  methods IF_MDG_BS_BP_ACCESS_HANDLER~GET_OBJECT_DATA_BY_ENTITY
    redefinition .
  methods IF_MDG_BS_BP_ACCESS_HANDLER~MAP_DATA_2API
    redefinition .
  methods IF_MDG_BS_BP_ACCESS_HANDLER~MAP_DATA_2STA
    redefinition .
  methods IF_MDG_BS_BP_ACCESS_HANDLER~READ_OBJECT_DATA
    redefinition .
  methods IF_MDG_BS_BP_ACCESS_HANDLER~SAVE_ADDITIONAL_OBJECT_DATA
    redefinition .
  methods IF_MDG_BS_BP_ACCESS_HANDLER~SORT_ENTITIES
    redefinition .
  methods IF_MDG_BS_BP_ACCESS_HANDLER~PREPARE_EI_HEADER_MAP_2API
    redefinition .
protected section.

  types:
    BEGIN OF ty_suppl_link,
      bpartner TYPE bu_partner,
      supplier TYPE lifnr,
    END OF ty_suppl_link .
  types TY_BP_SUPPL_LINK type TY_SUPPL_LINK .
  types:
    tty_bp_suppl_link TYPE TABLE OF ty_suppl_link .

  data GV_WF_SERVICES type ref to CL_MDG_BS_SUPPL_WF_SERVICES .
private section.

  data GT_ENT_FLD_TAB type ZIN_TT_BP_RULE .
  class-data GT_SUPPL_FLDGRP type MDG_BS_SUPPL_FG_T .
  class-data GT_BP_SUPPL_LINK type TTY_BP_SUPPL_LINK .

  methods READ_VENDOR_BY_LIFNR
    importing
      !IV_LIFNR type LIFNR
    exporting
      !ES_SUPPLIER type CVIS_EI_EXTERN .
  methods READ_VENDOR
    importing
      !IT_IDLIST type BUS_EI_INSTANCE_T
    exporting
      !ET_SUPPLIER type CVIS_EI_EXTERN_T .
  methods MAP_BP_ZSECRET_N_2API
    importing
      !IS_DATA type ANY
      !IV_TASK type BUS_EI_OBJECT_TASK
    changing
      !CS_SUPPL type CVIS_EI_EXTERN
      !CV_XCHANGE type BOOLE .
  methods MAP_BP_ZSECRET_N_2STA
    importing
      !IV_ASSIGNMENT_ID type MDG_BP_ASSIGNMENT_ID optional
      !IS_SUPPL type CVIS_EI_EXTERN
    changing
      !CT_DATA type ANY TABLE .
ENDCLASS.



CLASS ZIN_CL_CUST_ECC_HANDLER IMPLEMENTATION.


  METHOD constructor.
    DATA: ls_bp_fldgrp         TYPE mdg_bs_bp_fldgrp,
          ls_suppl_fldgrp      TYPE mdg_bs_suppl_fg,
          ls_usmd_entity       TYPE usmd_entity,
          lt_bp_fldgrp         TYPE TABLE OF mdg_bs_bp_fldgrp,
          gt_bp_fldgrp         TYPE TABLE OF mdg_bs_bp_fldgrp,
          ls_strucname         TYPE bu_bapistrc,
          ls_mdg_ecc_bpfldmap  TYPE mdg_ecc_bpfldmap,
          lt_mdg_ecc_bpfldmap  TYPE TABLE OF mdg_ecc_bpfldmap,
          gt_mdg_ecc_bpfldmap  TYPE TABLE OF mdg_ecc_bpfldmap,
          ls_field_mapping_ecc TYPE usmd_s_map_struc,
          lt_field_mapping_ecc TYPE usmd_ts_map_struc,
          lt_strucnames_ecc    TYPE mdg_bs_bp_strucnames_t,
          lt_usmd_entity       TYPE TABLE OF usmd_entity.

    super->constructor( ).
    me->gv_wf_services = cl_mdg_bs_suppl_wf_services=>get_instance( ).
* 1. get all included entities
*    SELECT usmd_entity FROM usmd0022 INTO ls_usmd_entity
*                                     WHERE usmd_model = 'BP'
*                                     AND   usmd_objstat = 'A'.
*      COLLECT ls_usmd_entity INTO lt_usmd_entity.
*    ENDSELECT.
** 2. get mapping information for each entity
*    LOOP AT lt_usmd_entity INTO ls_usmd_entity.
*      REFRESH lt_bp_fldgrp.
*      REFRESH lt_strucnames_ecc.
*      REFRESH lt_field_mapping_ecc.
*      CALL METHOD cl_mdg_bs_fnd_bp_services=>get_fieldmapping_for_entity
*        EXPORTING
**         IO_MODEL             =
*          iv_entity            = ls_usmd_entity
*        IMPORTING
*          et_strucnames_ecc    = lt_strucnames_ecc
*          et_field_mapping_ecc = lt_field_mapping_ecc
*          et_bp_fldgrp         = lt_bp_fldgrp.
*      APPEND LINES OF lt_bp_fldgrp TO gt_bp_fldgrp.
** map ecc structures
*      LOOP AT lt_strucnames_ecc INTO ls_strucname.
*        REFRESH lt_mdg_ecc_bpfldmap.
*        SELECT * FROM mdg_ecc_bpfldmap INTO TABLE lt_mdg_ecc_bpfldmap
*        WHERE bapistrucname = ls_strucname.
*        CHECK sy-subrc = 0.
*        APPEND LINES OF lt_mdg_ecc_bpfldmap TO gt_mdg_ecc_bpfldmap.
*      ENDLOOP.
*      LOOP AT lt_field_mapping_ecc INTO ls_field_mapping_ecc.
*        READ TABLE gt_mdg_ecc_bpfldmap WITH KEY bapistrucname = ls_field_mapping_ecc-tab_source
*                                                    bapifldnm = ls_field_mapping_ecc-fld_source
*                                                    INTO ls_mdg_ecc_bpfldmap.
*        CHECK sy-subrc = 0.
*        ls_suppl_fldgrp-model = 'BP'.
*        ls_suppl_fldgrp-entity = ls_usmd_entity.
*        ls_suppl_fldgrp-attribute = ls_field_mapping_ecc-fieldname.
*        ls_suppl_fldgrp-object_type_code = '147'.
*        ls_suppl_fldgrp-modif = ls_mdg_ecc_bpfldmap-modif.
*        ls_suppl_fldgrp-tabnm = ls_mdg_ecc_bpfldmap-tabnm.
*        ls_suppl_fldgrp-feldn = ls_mdg_ecc_bpfldmap-feldn.
*        APPEND ls_suppl_fldgrp TO gt_suppl_fldgrp.
*      ENDLOOP.
*    ENDLOOP.
** 3. map BP entities to gt_suppl_fldgrp
*    LOOP AT gt_bp_fldgrp INTO ls_bp_fldgrp.
*      MOVE-CORRESPONDING ls_bp_fldgrp TO ls_suppl_fldgrp.
*      ls_suppl_fldgrp-feldn = ls_bp_fldgrp-feldn.
*      ls_suppl_fldgrp-tabnm = ls_bp_fldgrp-tabnm.
*      ls_suppl_fldgrp-model = 'BP'.
*      ls_suppl_fldgrp-object_type_code = '147'.
*      COLLECT ls_suppl_fldgrp INTO gt_suppl_fldgrp.
*    ENDLOOP.
*    DELETE ADJACENT DUPLICATES FROM gt_bp_fldgrp.
*    DELETE ADJACENT DUPLICATES FROM gt_suppl_fldgrp.
* get instance of multiple assignment memory
    go_mlt_as_api = cl_mdg_bs_bp_mlt_assgnmnt_api=>get_instance( ).
  ENDMETHOD.


  METHOD if_mdg_bs_bp_access_handler~check_entity_data.

*    FIELD-SYMBOLS: <lt_r_data> TYPE INDEX TABLE.
*
*    IF line_exists( gt_ent_fld_tab[ entity = iv_entity ] ).     " validating
*
*      DATA(lo_context) = cl_usmd_app_context=>get_context( ).   " Get context for reading current CR data
*
*      lo_context->get_attributes(
*        IMPORTING
*          ev_crequest_id    = DATA(lv_crequest)                 " Change Request
*          ev_crequest_type  = DATA(lv_creqtype)                 " Type of Change Request
*      ).
*
** Checking for the iv_entity SU type
** We have to always pass Root Entity to the Read_entity_data_all method for reading CR data
*
*      DATA(ls_ent_prop) = io_model->if_usmd_model_metadata_ext~dt_entity_prop[ usmd_entity = iv_entity ].
*      IF ls_ent_prop-usage_type NE 1.
*        DATA(ls_root_ent) = io_model->if_usmd_model_metadata_ext~dt_entity_cont[ usmd_entity_cont = iv_entity ].
*        DATA(lv_entity) = ls_root_ent-usmd_entity.
*      ELSE.
*        lv_entity = iv_entity.
*      ENDIF.
*
** Creating data referenace for to hold the key values
*      CALL METHOD io_model->create_data_reference
*        EXPORTING
*          i_fieldname = CONV #( lv_entity )                      " Field Name
*          i_struct    = if_usmd_model_ext=>gc_struct_key         " Structure
*          if_table    = ''                                       " Financial MDM: General Indicator
*        IMPORTING
*          er_data     = DATA(lr_key).
*
*      IF lr_key IS BOUND.
*        ASSIGN lr_key->* TO FIELD-SYMBOL(<ls_key>).              " Dereferencing the object
*
** Reading Current CR data
*        io_model->read_entity_data_all(
*          EXPORTING
*            i_fieldname      = CONV #( lv_entity )                 " Financial MDM: Field Name
*            if_active        = abap_false                          " Financial MDM: General Indicator
*            i_crequest       = lv_crequest                         " Change Request
*          IMPORTING
*            et_message       = DATA(lt_message)                    " Messages
*            et_data_entity   = DATA(lt_data_entity)                " Data for Entity Types
*        ).
*
*
*        IF ls_ent_prop-usage_type = 1.
*          DATA(ls_data_entity) = VALUE #( lt_data_entity[ usmd_entity = iv_entity  struct = 'KATTR' ] OPTIONAL ).
*        ELSE.
*          ls_data_entity = VALUE #( lt_data_entity[ usmd_entity = lv_entity usmd_entity_cont = iv_entity struct = 'KATTR' ] OPTIONAL ).
*        ENDIF.
*
*
*        IF ls_data_entity IS NOT INITIAL.
*          ASSIGN ls_data_entity-r_t_data->* TO <lt_r_data>.        " Dereferencing data object
*
*          READ TABLE <lt_r_data> ASSIGNING FIELD-SYMBOL(<ls_data>) INDEX 1.
*          <ls_key> = CORRESPONDING #( <ls_data> ).                 " Moving corresponding values from <ls_data> to <ls_key>
*
*          LOOP AT gt_ent_fld_tab ASSIGNING FIELD-SYMBOL(<ls_field>) WHERE entity = iv_entity.
*            ASSIGN COMPONENT <ls_field>-attribute OF STRUCTURE <ls_data> TO FIELD-SYMBOL(<lv_field>).
*
** For Reading Description of attributes
*            DATA(ls_ent_attr_prop) = io_model->if_usmd_model_metadata_ext~dt_entity_attr_prop[ attr_res = <ls_field>-attribute ].
*            ASSIGN ls_ent_attr_prop-r_fprop->* TO FIELD-SYMBOL(<ls_fprop>).
*
*            IF <lv_field> IS INITIAL.
*              ct_return = VALUE #( BASE ct_return (
*                                                    type       = 'E'
*                                                    id         = 'ZIN_CL_MSG'
*                                                    number     = '003'
*                                                    message_v1 = `Enter a value `
*                                                    message_v2 = `for field `
*                                                    message_v3 = <ls_fprop>-scrtext_m
*                                                    log_msg_no = cl_mdg_bs_key_row_map_service=>set_key(
*                                                                                                  is_key       = lr_key
*                                                                                                  iv_entity    = iv_entity
*                                                                                                  iv_fieldname = <ls_field>-attribute )
*                                                                                                   ) ).
*            ENDIF.
*          ENDLOOP.
*        ENDIF.
*      ENDIF.
*    ENDIF.
  ENDMETHOD.


  METHOD if_mdg_bs_bp_access_handler~get_object_data_by_entity.
*CALL METHOD SUPER->IF_MDG_BS_BP_ACCESS_HANDLER~GET_OBJECT_DATA_BY_ENTITY
*  EXPORTING
*    IV_ENTITY              =
*    IV_PARTNER             =
*    iv_enforce_update_task = ABAP_FALSE
*  IMPORTING
*    er_data                =
*    ef_not_found           =
*    .
    CASE iv_entity.
      WHEN 'ZSECRET_N'.
        FIELD-SYMBOLS: <ls_secret_num> TYPE zin_t_secret_num.
        DATA: lo_line TYPE REF TO data.
        SELECT SINGLE * FROM zin_t_secret_num INTO @DATA(ls_secret_num) WHERE lifnr = @iv_partner.
        CREATE DATA lo_line TYPE zin_t_secret_num.
        ASSIGN lo_line->* TO <ls_secret_num>.
        <ls_secret_num> = ls_secret_num.
        er_data = lo_line.
    ENDCASE.
  ENDMETHOD.


METHOD if_mdg_bs_bp_access_handler~map_data_2api.

  DATA: ls_cvi_ei_extern TYPE cvis_ei_extern.
* ls_suppl_ext TYPE mdg_bs_bp_s_mlt_as_suppl .
  FIELD-SYMBOLS:
    <lt_mlt_as>    TYPE mdg_bs_bp_tt_mlt_as,
    <ls_mlt_as>    TYPE mdg_bs_bp_s_mlt_as,
    <lt_data>      TYPE mdg_bs_bp_tt_mlt_as_suppl,
    <ls_data>      TYPE vmds_ei_extern,
    <ls_ext_suppl> TYPE vmds_ei_extern,
    <ls_bp_header> TYPE bus_ei_extern,
    <ls_as_id>     TYPE mdg_bp_assignment_id,
    <ls_ext_as_id> TYPE mdg_bp_assignment_id,
    <ls_suppl>     TYPE mdg_bs_bp_s_mlt_as_suppl. " vmds_ei_extern.
  ASSIGN COMPONENT 'ASSGNM_ID' OF STRUCTURE is_data TO <ls_as_id>.
  ASSIGN COMPONENT 'VENDOR' OF STRUCTURE cs_data_ext TO <ls_ext_suppl> .
  CHECK <ls_ext_suppl> IS ASSIGNED.
* ls_cvi_ei_extern-partner-header-object_instance_bpartner = not available here...
  ls_cvi_ei_extern-vendor = <ls_ext_suppl> .
  CASE iv_entity.
    WHEN 'ZSECRET_N'.
      CALL METHOD me->map_bp_zsecret_n_2api
        EXPORTING
          is_data    = is_data
          iv_task    = iv_task
*          iv_fname   = iv_fname
        CHANGING
          cs_suppl   = ls_cvi_ei_extern
*          ct_return  = ct_return
          cv_xchange = cv_xchange.
    WHEN OTHERS.
  ENDCASE.
  IF <ls_ext_suppl> IS ASSIGNED.
    <ls_ext_suppl> = ls_cvi_ei_extern-vendor.
    ASSIGN COMPONENT 'ASSIGNMENT_ID' OF STRUCTURE cs_data_ext TO <ls_ext_as_id>.
    IF sy-subrc = 0.
      <ls_ext_as_id> = <ls_as_id>.
    ENDIF.
  ENDIF.
ENDMETHOD.


METHOD if_mdg_bs_bp_access_handler~map_data_2sta.

  DATA: ls_cvi_ei_extern TYPE cvis_ei_extern,
        lv_as_id         TYPE mdg_bp_assignment_id.

  FIELD-SYMBOLS:
    <lt_mlt_as>    TYPE ANY TABLE,
    <ls_mlt_as>    TYPE any,
    <lt_data>      TYPE mdg_bs_bp_tt_mlt_as_suppl,
    <ls_data>      TYPE mdg_bs_bp_s_mlt_as_suppl,
    <ls_bp_header> TYPE bus_ei_extern,
    <ls_data_ret>  TYPE any,
    <lv_as_id_ret> TYPE any,
    <lv_as_id>     TYPE any.

* special handling for multiple assignments:
* in case a multiple assignment record for standard vendor was simulated (ifcvi_vend_link exist but no entry
* in table MDG_MLT_ASSGNMNT) then only this record is bufferd by customer handler and has to be mapped here, that's why entity 'BP_MLT_AS'
* has to be processed here additionally
*  IF iv_entity = 'BP_MLT_AS'.
*    ASSIGN COMPONENT 'MLT_ASSIGNMENTS' OF STRUCTURE is_data TO <lt_mlt_as>.
*    CALL METHOD me->map_bp_mlt_as_2sta
*      EXPORTING
*        it_mlt_as = <lt_mlt_as>
*      CHANGING
*        ct_data   = ct_data
*        ct_return = ct_return.
*    RETURN.
*  ENDIF.
** handling of all other entities
*  ASSIGN COMPONENT 'PARTNER' OF STRUCTURE is_data TO <ls_bp_header>.
*  ASSIGN COMPONENT 'VENDORS' OF STRUCTURE is_data TO <lt_data>.
*  IF <lt_data> IS ASSIGNED.
*    LOOP AT <lt_data> ASSIGNING <ls_data>.
** build CVI structure for mapping
*      ls_cvi_ei_extern-partner = <ls_bp_header>.
*      ls_cvi_ei_extern-vendor = <ls_data>-vendor.
*      ASSIGN COMPONENT 'ASSIGNMENT_ID' OF STRUCTURE <ls_data> TO <lv_as_id>.
*      IF sy-subrc = 0.
*        lv_as_id = <lv_as_id>.
*      ENDIF.
      CASE iv_entity.
        WHEN 'ZSECRET_N'.
          CALL METHOD me->map_bp_zsecret_n_2sta
            EXPORTING
              is_suppl         = ls_cvi_ei_extern
              iv_assignment_id = lv_as_id
            CHANGING
              ct_data          = ct_data.
*              ct_return        = ct_return.
      ENDCASE.
*    ENDLOOP.
*  ENDIF.
ENDMETHOD.


  METHOD if_mdg_bs_bp_access_handler~read_object_data.
    DATA: lt_supplier   TYPE cvis_ei_extern_t,
          ls_as_vendor  TYPE cvis_ei_extern,
          lt_mlt_as     TYPE mdg_bs_bp_tt_mlt_as,
          lt_mlt_as_old TYPE mdg_bs_bp_tt_mlt_as_mem,
          lt_mlt_as_ret TYPE mdg_bs_bp_tt_mlt_as_mem,
          ls_mlt_as     TYPE mdg_bs_bp_s_mlt_as,
          ls_mlt_as_old TYPE mdg_bs_bp_s_mlt_as_mem,
          ls_mlt_as_db  TYPE mdg_bs_bp_s_mlt_as_target,
          lt_return     TYPE bapiret2_t,
          lv_lifnr      TYPE lifnr.

    FIELD-SYMBOLS:
      <ls_cvis_data_db> TYPE cvis_ei_extern,
      <ls_supplier>     TYPE cvis_ei_extern,
      <ls_as_supplier>  TYPE cvis_ei_extern,
      <ls_mlt_as_db>    TYPE mdg_bs_bp_s_mlt_as_target.

    DATA: lt_ecc_extern_db TYPE mdg_bs_bp_tt_ecc_extern,
          ls_ecc_extern_db TYPE mdg_bs_bp_s_ecc_extern,
          ls_vendor        TYPE mdg_bs_bp_s_mlt_as_suppl,
          lv_kunnr         TYPE kunnr.

    FIELD-SYMBOLS:
      <ls_ecc_extern_db> TYPE mdg_bs_bp_s_ecc_extern,
      <ls_mlt_as_old>    TYPE mdg_bs_bp_s_mlt_as_mem,
      <ls_idlist>        TYPE bus_ei_instance.

    DATA: lt_zin_t_secret_num TYPE TABLE OF zin_t_secret_num,
          ls_zin_t_secret_num LIKE LINE OF lt_zin_t_secret_num,
          ls_secret_number    TYPE zin_s_secret_number.

    FIELD-SYMBOLS: <ls_extern_db>     LIKE LINE OF gt_ecc_extern_db,
                   <ls_vendor>        TYPE mdg_bs_bp_s_mlt_as_suppl,
                   <ls_company>       TYPE LINE OF vmds_ei_company_t,
                   <ls_secret_number> TYPE zin_s_secret_number.

* global DB tables are filled here
* EHP5: gt_cvis_data_db -> contains data of standard supplier
* EHP6: gt_ecc_extern_db -> contains all assigned supplier (incl. multiple assignment data)
* (every handler has its own instance of these tables)
* NOTE: Multiple Assignment Handler reads all assignments, but in case a standard supplier is assigned to bp
* and no record exists in table MDG_MLT_ASSGNMNT, this record has to be simulated here and will be appended to db state
* flag controls if data for all entities have to be read
    CHECK iv_read_all IS NOT INITIAL.
** assigned vendor to BP is read (can be only one per BP)
*    CALL METHOD me->read_vendor
*      EXPORTING
*        it_idlist   = it_idlist
*      IMPORTING
*        et_supplier = lt_supplier.
*    note: lt_supplier may have an entry even if no supplier exists (in case it_idlist has mor than one entry),
*    vendor strcuture is empty then
*
*    LOOP AT it_idlist ASSIGNING <ls_idlist> .
* read all supplier assignments for BP from DB (standard and additional assignments)
* buffer table of each handler has to store ALL assignments (supplier & customer)
* in method 'read_object_data_by_entitythe entity BP_MLT_AS is handled byall handlers
*      CALL METHOD go_mlt_as_api->get_list_mlt_assgnmnt
*        EXPORTING
*          iv_partner        = <ls_idlist>-bpartner
*          iv_assignment_cat = gc_as_cat_suppl
*        IMPORTING
*          et_mlt_as         = lt_mlt_as " supplier assignments only
*          et_message        = lt_return.
*      lt_mlt_as_old = lt_mlt_as.
* get data of standard customer
*      READ TABLE lt_supplier WITH KEY partner-header-object_instance-bpartner = <ls_idlist>-bpartner
*      ASSIGNING <ls_supplier>.
*      IF <ls_supplier> IS NOT ASSIGNED.
*        CONTINUE.
*      ELSE.
*        IF <ls_supplier>-vendor-header IS INITIAL.
*          CONTINUE.
*        ENDIF.
*      ENDIF.
* get corresponding assignment_id for standard supplier
*      READ TABLE lt_mlt_as_old ASSIGNING <ls_mlt_as_old>
*      WITH KEY partner = <ls_idlist>-bpartner
*      standard = abap_true.
*      IF <ls_mlt_as_old> IS ASSIGNED. "should always be assigned
*        ls_vendor-assignment_id = <ls_mlt_as_old>-assignment_id.
*        ls_vendor-vendor = <ls_supplier>-vendor.
* vendor does not have standard assigment/we simulate one/will be writtenwhen activating/assignment id of standard supplieris always '1'
*      ELSE.
*        ls_vendor-assignment_id = gc_standard_as_id_suppl. " '1'
*        ls_vendor-vendor = <ls_supplier>-vendor.
* create multiple assignment record
*        ls_mlt_as_old-partner = <ls_idlist>-bpartner.
*        ls_mlt_as_old-assignment_id = gc_standard_as_id_suppl.
*        ls_mlt_as_old-assignment_cat = gc_as_cat_suppl.
*        ls_mlt_as_old-object_id = ls_vendor-vendor-header-object_instance-lifnr.
*        ls_mlt_as_old-standard = abap_true.
*        ls_mlt_as_old-updateflag = gc_simulated_db_record. "Simulated DB record/has to be written in save later on
*        APPEND ls_mlt_as_old TO lt_mlt_as_ret . " no append necessary for lt_mlt_as_old
*      ENDIF.
* check if there is already a record for BP in DB buffer table
*      UNASSIGN <ls_ecc_extern_db>.                          "1618668
*      READ TABLE gt_ecc_extern_db ASSIGNING <ls_ecc_extern_db>
*      WITH KEY partner-header-object_instance-bpartner = <ls_supplier>-partner-header-object_instance-bpartner.
*      IF <ls_ecc_extern_db> IS ASSIGNED.
* check if vendor is already in DB buffer table
*        READ TABLE <ls_ecc_extern_db>-vendors
*        WITH KEY assignment_id = ls_vendor-assignment_id
*        vendor-header-object_instance-lifnr = <ls_supplier>-vendor-header-object_instance-lifnr TRANSPORTING NO FIELDS.
*        IF sy-subrc <> 0.
*          APPEND ls_vendor TO <ls_ecc_extern_db>-vendors.
*        ENDIF.
*      ELSE.
* create complete record
*        ls_ecc_extern_db-partner-header-object_instance = <ls_idlist>.
*        APPEND ls_vendor TO ls_ecc_extern_db-vendors.
*        APPEND ls_ecc_extern_db TO gt_ecc_extern_db ASSIGNING <ls_ecc_extern_db>.
*      ENDIF.
*      IF <ls_ecc_extern_db> IS NOT ASSIGNED.
* error
*      ENDIF.
* special handling for multiple assignments:
* in case a multiple assignment record for standard vendor was simulated (if cvi_vend_link exist but no entry in table MDG_MLT_ASSGNMNT)
* then only this record is bufferd by customer handler and has to be stored here
*      <ls_ecc_extern_db>-mlt_assignments = lt_mlt_as_ret.
* read all additional suppliers
*      LOOP AT lt_mlt_as_old INTO ls_mlt_as_old WHERE assignment_cat = gc_as_cat_suppl.
* data of standard supplier has been already appended to table gt_ecc_extern_db, here only the additional assignments are treated
*        IF ls_mlt_as_old-standard = abap_true.
*          CONTINUE.
*        ENDIF.
* read data of assigned supplier
*        CLEAR ls_as_vendor .
*        lv_lifnr = ls_mlt_as_old-object_id.
*        CALL METHOD me->read_vendor_by_lifnr
*          EXPORTING
*            iv_lifnr    = lv_lifnr
*          IMPORTING
*            es_supplier = ls_as_vendor.
*
*        IF ls_as_vendor IS NOT INITIAL.
*          READ TABLE <ls_ecc_extern_db>-vendors
*          WITH KEY assignment_id = ls_mlt_as_old-assignment_id
*          vendor-header-object_instance-lifnr = ls_as_vendor-vendor-header-object_instance-lifnr TRANSPORTING NO FIELDS.
*          IF sy-subrc <> 0.
*            ls_vendor-vendor = ls_as_vendor-vendor.
*            ls_vendor-assignment_id = ls_mlt_as_old-assignment_id.
*            APPEND ls_vendor TO <ls_ecc_extern_db>-vendors.
*          ENDIF.
*        ENDIF.
*      ENDLOOP. " LOOP AT lt_mlt_as_old
* ENDLOOP. " LOOP AT lt_supplier
*    ENDLOOP. " LOOP AT it_id_list
***********************************************************************
** Part for Customer-own tables
    LOOP AT gt_ecc_extern_db ASSIGNING <ls_extern_db>.
      LOOP AT <ls_extern_db>-vendors ASSIGNING <ls_vendor>.
        LOOP AT <ls_vendor>-vendor-company_data-company ASSIGNING <ls_company>.
          CLEAR: lt_zin_t_secret_num,
          ls_zin_t_secret_num.
          SELECT * FROM zin_t_secret_num APPENDING TABLE lt_zin_t_secret_num
                                                          WHERE bukrs = <ls_company>-data_key-bukrs
                                                            AND lifnr = <ls_vendor>-vendor-header-object_instance-lifnr.
          IF NOT lt_zin_t_secret_num IS INITIAL.
            <ls_company>-zsecret_number-zcurrent_state = 'X'.
          ENDIF.

          LOOP AT lt_zin_t_secret_num INTO ls_zin_t_secret_num.
*            APPEND INITIAL LINE TO <ls_company>-zsecret_number-zsecret_number ASSIGNING <ls_secret_number>.
*            <ls_secret_number>-data_key-secret_number = ls_zin_t_secret_num-secret_number.
*            <ls_secret_number>-data-sn_comment = ls_zin_t_secret_num-sn_comment.
            APPEND VALUE #( data_key-secret_number = ls_zin_t_secret_num-secret_number
                           data-sn_comment = ls_zin_t_secret_num-sn_comment ) TO <ls_company>-zsecret_number-zsecret_number.
*            APPEND <ls_secret_number> TO <ls_company>-zsecret_number-zsecret_number.
          ENDLOOP.
        ENDLOOP.
      ENDLOOP.
    ENDLOOP.
  ENDMETHOD.


  METHOD if_mdg_bs_bp_access_handler~sort_entities.
    DATA: lt_entity TYPE TABLE OF ty_usmd_entity,
          lv_entity TYPE ty_usmd_entity.

    lt_entity = it_entities_all.

    READ TABLE lt_entity WITH KEY entity = 'ZSECRET_N' INTO lv_entity.
    IF sy-subrc = 0.
      APPEND lv_entity TO ct_entities.
    ENDIF.

  ENDMETHOD.


  METHOD if_mdg_bs_bp_access_handler~prepare_ei_header_map_2api.
    FIELD-SYMBOLS:
      <ls_suppl_ext>  TYPE cvis_ei_extern,
      <bp_suppl_link> TYPE ty_bp_suppl_link.
    DATA:
    ls_suppl_ext TYPE cvis_ei_extern.
    FIELD-SYMBOLS:
      <lv_partner>       TYPE bu_partner,
      <ls_ecc_ext>       TYPE mdg_bs_bp_s_ecc_extern,
      <ls_data_ext_db>   TYPE mdg_bs_bp_s_ecc_extern,
      <ls_vendor_db>     TYPE mdg_bs_bp_s_mlt_as_suppl,
      <lv_kunnr>         TYPE kunnr,
      <ls_ecc_ext_suppl> TYPE mdg_bs_bp_s_mlt_as_suppl,
      <lv_assignment_id> TYPE mdg_bp_assignment_id,
      <ls_ecc_mlt_as>    TYPE mdg_bs_bp_s_mlt_as_mem,
      <ls_ext>           TYPE mdg_bs_bp_s_mlt_as_suppl.
    DATA:
      ls_cust_ext      TYPE cvis_ei_extern,
      ls_ecc_ext       TYPE mdg_bs_bp_s_ecc_extern,
      lr_data          TYPE REF TO data,
      ls_ecc_ext_suppl TYPE mdg_bs_bp_s_mlt_as_suppl,
      ls_ecc_mlt_as    TYPE mdg_bs_bp_s_mlt_as_mem.

    CHECK iv_entity = 'ZSECRET_N'.
* Supplier Switch for EHP6 has to be active
    IF cl_mdg_bs_suppl_switch_check=>mdg_bs_ecc_supplier_switch_02( ) EQ abap_true
    .
      ASSIGN COMPONENT 'BP_HEADER' OF STRUCTURE is_data TO <lv_partner>.
      ASSIGN COMPONENT 'ASSGNM_ID' OF STRUCTURE is_data TO <lv_assignment_id>.
* check whether record with bp number already exists, if not create one
      READ TABLE ct_data_ext
      WITH KEY ('PARTNER-HEADER-OBJECT_INSTANCE-BPARTNER') = <lv_partner>
      ASSIGNING <ls_ecc_ext>.
* should always be provided by BP_Handler
      IF sy-subrc <> 0.
* create BP record
        CLEAR ls_ecc_ext.
        ls_ecc_ext-partner-header-object_instance-bpartner = <lv_partner>.
        READ TABLE gt_ecc_extern_db
        ASSIGNING <ls_data_ext_db>
        WITH KEY partner-header-object_instance-bpartner = <lv_partner>.
        IF sy-subrc = 0.
          ls_ecc_ext-partner-header-object_task = gc_upd.
        ELSE.
          ls_ecc_ext-partner-header-object_task = gc_ins.
        ENDIF.
        INSERT ls_ecc_ext INTO TABLE ct_data_ext ASSIGNING <ls_ecc_ext>.
      ENDIF.

* check if vendor record was already mapped
      READ TABLE <ls_ecc_ext>-vendors
      WITH KEY assignment_id = <lv_assignment_id> ASSIGNING <ls_ecc_ext_suppl>.
* create new supplier entry
      IF sy-subrc <> 0.
        ls_ecc_ext_suppl-assignment_id = <lv_assignment_id>.
* determine LIFNR (may already exist on DB)
        READ TABLE gt_ecc_extern_db
        ASSIGNING <ls_data_ext_db>
        WITH KEY partner-header-object_instance-bpartner = <lv_partner>.
* Partner already exist on DB
        IF sy-subrc = 0.
          READ TABLE <ls_data_ext_db>-vendors ASSIGNING <ls_vendor_db>
          WITH KEY assignment_id = <lv_assignment_id>.
* supplier already exist on DB
          IF sy-subrc = 0.
* fill suplier ID
            ls_ecc_ext_suppl-vendor-header-object_instance-lifnr = <ls_vendor_db>-vendor-header-object_instance-lifnr.
            ls_ecc_ext_suppl-vendor-header-object_task = gc_upd.
          ELSE.
* supplier doesn't exist on DB, must be Insert
            ls_ecc_ext_suppl-vendor-header-object_task = gc_ins.
          ENDIF.
        ENDIF.
        INSERT ls_ecc_ext_suppl INTO TABLE <ls_ecc_ext>-vendors ASSIGNING <ls_ecc_ext_suppl>.
      ENDIF. "create new supplier target record
      GET REFERENCE OF <ls_ecc_ext_suppl> INTO er_data_ext.
    ENDIF.
  ENDMETHOD.


  METHOD if_mdg_bs_bp_access_handler~save_additional_object_data.
    FIELD-SYMBOLS:
      <lt_data_new> TYPE mdg_bs_bp_s_ecc_extern,
      <lt_data_db>  TYPE mdg_bs_bp_s_ecc_extern,
      <ls_partner>  TYPE bus_ei_extern,
      <ls_data>     TYPE zin_t_secret_num,
      <ls_data_db>  TYPE zin_t_secret_num,
      <ls_company>  TYPE vmds_ei_company,
      <ls_secret>   TYPE zin_s_secret_number,
      <lv_vendor>   TYPE mdg_bs_bp_s_mlt_as_suppl,
      <ls_data_ext> TYPE mdg_bs_bp_s_ecc_extern.

    DATA: lt_current_secret  TYPE TABLE OF zin_t_secret_num,
          lt_database_secret TYPE TABLE OF zin_t_secret_num,
          ls_current_secret  TYPE zin_t_secret_num,
          ls_database_secret TYPE zin_t_secret_num,
          lt_ins             TYPE TABLE OF zin_t_secret_num,
          lt_upd             TYPE TABLE OF zin_t_secret_num,
          lt_del             TYPE TABLE OF zin_t_secret_num,
          ls_data            TYPE zin_t_secret_num,
          lr_ecc_extern_db   TYPE REF TO data.

* Note: IS_DATA_DB does not contain DB state of customer specific data, use GT_ECC_EXTERN_DB forcurrent db state of handler
    ASSIGN is_data TO <lt_data_new>.
    ASSIGN COMPONENT 'PARTNER' OF STRUCTURE is_data TO <ls_partner>.
    CHECK <ls_partner> IS ASSIGNED.
    READ TABLE gt_ecc_extern_db
    ASSIGNING <ls_data_ext>
    WITH KEY partner-header-object_instance-bpartner = <ls_partner>-header-object_instance-bpartner.
    IF <ls_data_ext> IS ASSIGNED.
      ASSIGN <ls_data_ext> TO <lt_data_db>.
    ELSE.
      CREATE DATA lr_ecc_extern_db TYPE mdg_bs_bp_s_ecc_extern.
      ASSIGN lr_ecc_extern_db->* TO <lt_data_db>.
    ENDIF.
* current data
    LOOP AT <lt_data_new>-vendors ASSIGNING <lv_vendor>.
      LOOP AT <lv_vendor>-vendor-company_data-company ASSIGNING <ls_company>.
        LOOP AT <ls_company>-zsecret_number-zsecret_number ASSIGNING <ls_secret>.
          CLEAR: ls_data.
          ASSERT NOT <lv_vendor>-vendor-header-object_instance-lifnr IS INITIAL.
          ls_data-lifnr = <lv_vendor>-vendor-header-object_instance-lifnr.
          ls_data-bukrs = <ls_company>-data_key-bukrs.
          ls_data-secret_number = <ls_secret>-data_key-secret_number.
          ls_data-sn_comment = <ls_secret>-data-sn_comment.
          CASE <ls_secret>-task.
            WHEN gc_del.
              APPEND ls_data TO lt_del.
            WHEN gc_ins.
              APPEND ls_data TO lt_ins.
            WHEN gc_upd.
              APPEND ls_data TO lt_upd.
          ENDCASE.
        ENDLOOP.
      ENDLOOP.
    ENDLOOP.

* database data
    LOOP AT <lt_data_db>-vendors ASSIGNING <lv_vendor>.
      LOOP AT <lv_vendor>-vendor-company_data-company ASSIGNING <ls_company>.
        LOOP AT <ls_company>-zsecret_number-zsecret_number ASSIGNING <ls_secret>.
          ASSERT NOT <lv_vendor>-vendor-header-object_instance-lifnr IS INITIAL.
          ls_database_secret-lifnr = <lv_vendor>-vendor-header-object_instance-lifnr.
          ls_database_secret-bukrs = <ls_company>-data_key-bukrs.
          ls_database_secret-secret_number = <ls_secret>-data_key-secret_number.
          ls_database_secret-sn_comment = <ls_secret>-data-sn_comment.
          APPEND ls_database_secret TO lt_database_secret.
        ENDLOOP.
      ENDLOOP.
    ENDLOOP.

    lt_current_secret = lt_database_secret.
* merge
* Deletes
    LOOP AT lt_del ASSIGNING <ls_data>.
      READ TABLE lt_current_secret WITH KEY bukrs = <ls_data>-bukrs
                                            lifnr = <ls_data>-lifnr
                                            secret_number = <ls_data>-secret_number
                                            ASSIGNING <ls_data_db>.
      IF sy-subrc = 0.
* delete record in target table
        DELETE lt_current_secret INDEX sy-tabix.
      ELSE.
* record doesn't exist on DB -> must be a new one, keep data to be checked consistent
        READ TABLE lt_ins WITH KEY bukrs = <ls_data>-bukrs
                                   lifnr = <ls_data>-lifnr
                                   secret_number = <ls_data>-secret_number ASSIGNING <ls_data_db>.
        IF sy-subrc = 0.
          DELETE lt_current_secret INDEX sy-tabix.
        ENDIF.
      ENDIF.
    ENDLOOP.

* Inserts
    LOOP AT lt_ins ASSIGNING <ls_data>.
      READ TABLE lt_current_secret WITH KEY bukrs = <ls_data>-bukrs
                                            lifnr = <ls_data>-lifnr
                                            secret_number = <ls_data>-secret_number
      ASSIGNING <ls_data_db>.
* insert record into target table
      IF sy-subrc NE 0.
        INSERT <ls_data> INTO TABLE lt_current_secret.
      ENDIF.
    ENDLOOP.

* Updates
    LOOP AT lt_upd ASSIGNING <ls_data>.
      READ TABLE lt_current_secret WITH KEY bukrs = <ls_data>-bukrs
                                            lifnr = <ls_data>-lifnr
                                            secret_number = <ls_data>-secret_number
                                            ASSIGNING <ls_data_db>.
* insert record into target table
      IF sy-subrc = 0.
        <ls_data_db> = <ls_data>.
      ENDIF.
    ENDLOOP.

    CALL FUNCTION 'ZIN_FM_SECRET_NUMBER_UPDATES' IN UPDATE TASK
      TABLES
        x_secret_numbers = lt_current_secret
        y_secret_numbers = lt_database_secret.

  ENDMETHOD.


  METHOD map_bp_zsecret_n_2api.

    DATA: lv_dummy   TYPE string,
          ls_secret  TYPE zxx_s_bp_pp_secret_n,             "#EC NEEDED
          ls_dunn_x  TYPE mdg_bs_suppl_bp_dunn_x,
          ls_target  TYPE zin_s_secret_number,
          ls_comp    TYPE vmds_ei_company,
          ls_comp_db TYPE vmds_ei_company.

    FIELD-SYMBOLS:
      <ls_data_x>         TYPE any,
      <ls_comp>           TYPE vmds_ei_company,
      <lv_assgnm_id>      TYPE mdg_bp_assignment_id,
      <lv_partner>        TYPE bu_partner,
      <ls_ecc_extern_db>  TYPE mdg_bs_bp_s_ecc_extern,
      <ls_cvis_ei_extern> TYPE cvis_ei_extern,
      <ls_vendors>        TYPE mdg_bs_bp_s_mlt_as_suppl,
      <ls_vendor>         TYPE vmds_ei_extern.
    MOVE-CORRESPONDING is_data TO ls_secret.
* Company Data key
    READ TABLE cs_suppl-vendor-company_data-company WITH KEY data_key-bukrs = ls_secret-company ASSIGNING <ls_comp>.
    IF <ls_comp> IS NOT ASSIGNED.
* create entry as entities bp_compny and bp_dunn may map to the same target record as dunning data are company dependent
* CAUTION if no inactive changes for the company code exist: check if company data for the relevant
*comp. code is saved. If DB data exists, the company code data must be insertedas well as otherwise
* check would determine initial company code data when checking entity BP_COMPNY
      ls_comp-data_key-bukrs = ls_secret-company.
      ls_comp-task = gc_upd. "if insert of entity comes later on task is overwritten
* check if comp. code is saved
      ASSIGN COMPONENT 'BP_HEADER' OF STRUCTURE is_data TO <lv_partner>.
      IF sy-subrc = 0.
        IF <lv_partner> IS NOT INITIAL.
          ASSIGN COMPONENT 'ASSGNM_ID' OF STRUCTURE is_data TO <lv_assgnm_id>.
          IF sy-subrc = 0.
            IF <lv_assgnm_id> IS NOT INITIAL.
              READ TABLE gt_ecc_extern_db ASSIGNING <ls_ecc_extern_db>
                                          WITH KEY partner-header-object_instance-bpartner = <lv_partner>.
              IF sy-subrc = 0.
                READ TABLE <ls_ecc_extern_db>-vendors ASSIGNING <ls_vendors>
                                                      WITH KEY assignment_id = <lv_assgnm_id>.
                IF sy-subrc = 0.
                  READ TABLE <ls_vendors>-vendor-company_data-company INTO ls_comp_db
                                            WITH KEY data_key-bukrs = ls_secret-company.

                  IF sy-subrc = 0.
                    ls_comp-data = ls_comp_db-data.
                  ENDIF.
                ENDIF.
              ENDIF.
            ENDIF.
          ENDIF.
        ENDIF.
      ENDIF.
      INSERT ls_comp INTO TABLE cs_suppl-vendor-company_data-company ASSIGNING <ls_comp>.
    ENDIF.
* Secret Number Data Key
    ls_target-data_key-secret_number = ls_secret-zsecret.
* Secret Number Data
    IF iv_task = gc_ins OR iv_task = gc_upd.
      ls_target-data-sn_comment = ls_secret-sncomment .
    ENDIF.
    ls_target-task = iv_task.
    APPEND ls_target TO <ls_comp>-zsecret_number-zsecret_number.
    cv_xchange = abap_true.

  ENDMETHOD.


  METHOD map_bp_zsecret_n_2sta.

    FIELD-SYMBOLS: <ls_extern_db>     LIKE LINE OF gt_ecc_extern_db,
                   <ls_vendor>        TYPE mdg_bs_bp_s_mlt_as_suppl,
                   <ls_company>       TYPE LINE OF vmds_ei_company_t,
                   <ls_secret_number> TYPE zin_s_secret_number,
                   <lv_bp_header>     TYPE bu_partner,
                   <lv_company>       TYPE bukrs,
                   <lv_secret_number> TYPE zin_de_secret_number,
                   <lv_comment>       TYPE zin_de_secret_number_comment.

    DATA: lr_data  TYPE REF TO data,
          lv_dummy TYPE string.

    FIELD-SYMBOLS:
      <ls_data>   TYPE data,
      <ls_comp>   TYPE vmds_ei_company,
      <ls_secret> TYPE zin_s_secret_number,
      <lv_as_id>  TYPE mdg_bp_assignment_id.

    LOOP AT gt_ecc_extern_db ASSIGNING <ls_extern_db>.
      LOOP AT <ls_extern_db>-vendors ASSIGNING <ls_vendor>.
        LOOP AT <ls_vendor>-vendor-company_data-company ASSIGNING <ls_company>.
          LOOP AT <ls_company>-zsecret_number-zsecret_number ASSIGNING <ls_secret>.
            CREATE DATA lr_data LIKE LINE OF ct_data.
            ASSIGN lr_data->* TO <ls_data>.

            ASSIGN COMPONENT:
            'BP_HEADER' OF STRUCTURE <ls_data> TO <lv_bp_header>,
            'COMPANY' OF STRUCTURE <ls_data> TO <lv_company>,
            'ZSECRET' OF STRUCTURE <ls_data> TO <lv_secret_number>,
            'SNCOMMENT' OF STRUCTURE <ls_data> TO <lv_comment>,
            'ASSGNM_ID' OF STRUCTURE <ls_data> TO <lv_as_id>.

            <lv_as_id>  = <ls_vendor>-assignment_id.
            <lv_bp_header> = <ls_extern_db>-partner-header-object_instance-bpartner.
            <lv_company> = <ls_company>-data_key-bukrs.
            <lv_secret_number> = <ls_secret>-data_key-secret_number.
            <lv_comment> = <ls_secret>-data-sn_comment.
            INSERT <ls_data> INTO TABLE ct_data.
          ENDLOOP.
        ENDLOOP.
        ENDLOOP.
        ENDLOOP.

      ENDMETHOD.


  METHOD read_vendor.
    DATA:
      lo_ka_bp_vendor    TYPE REF TO cvi_ka_bp_vendor,
      ls_bp_suppl_link   TYPE ty_bp_suppl_link,
      ls_vmds_ei_main    TYPE vmds_ei_main,
      ls_vmds_ei_extern  TYPE vmds_ei_extern,
      ls_vmds_ei_main_in TYPE vmds_ei_main,
      ls_cvis_extern     TYPE cvis_ei_extern,
      lv_partner_guid    TYPE bu_partner_guid.
    FIELD-SYMBOLS:
    <ls_idlist> TYPE bus_ei_instance.
* Read Connection Between Vendor and Business Partner
    lo_ka_bp_vendor = cvi_ka_bp_vendor=>get_instance( ).
    LOOP AT it_idlist ASSIGNING <ls_idlist>.
* existenz in globaler Tabelle noch prüfen
      ls_bp_suppl_link-bpartner = <ls_idlist>-bpartner.
      lv_partner_guid = <ls_idlist>-bpartnerguid.
      IF lv_partner_guid IS INITIAL.
        CALL FUNCTION 'BUPA_NUMBERS_GET'
          EXPORTING
            iv_partner      = ls_bp_suppl_link-bpartner
          IMPORTING
            ev_partner_guid = lv_partner_guid.
      ENDIF.
      lo_ka_bp_vendor->get_assigned_vendor_for_bp(
                           EXPORTING
                             i_partner = lv_partner_guid
                           RECEIVING
                             r_vendor = ls_bp_suppl_link-supplier
                           EXCEPTIONS
                             OTHERS = 1 ).

      IF sy-subrc = 0 AND ls_bp_suppl_link-supplier IS NOT INITIAL.
        APPEND ls_bp_suppl_link TO gt_bp_suppl_link.
        ls_vmds_ei_extern-header-object_task = 'M'.
        ls_vmds_ei_extern-header-object_instance-lifnr = ls_bp_suppl_link-supplier.
        APPEND ls_vmds_ei_extern TO ls_vmds_ei_main_in-vendors.
        CLEAR: ls_vmds_ei_extern.
      ENDIF.
    ENDLOOP.

    IF NOT ls_vmds_ei_main_in IS INITIAL.
* Extract Vendors
      vmd_ei_api_extract=>get_data(
                            EXPORTING
                              is_master_data = ls_vmds_ei_main_in
                            IMPORTING
                              es_master_data = ls_vmds_ei_main
                            EXCEPTIONS
                              OTHERS = 1 ).

      LOOP AT it_idlist ASSIGNING <ls_idlist>.
        ls_cvis_extern-partner-header-object_instance-bpartner = <ls_idlist>-bpartner.
        CLEAR ls_cvis_extern-vendor.
* Vendor
        READ TABLE gt_bp_suppl_link
                      INTO ls_bp_suppl_link
                      WITH KEY bpartner = <ls_idlist>-bpartner.
        IF sy-subrc = 0.

          READ TABLE ls_vmds_ei_main-vendors
                     INTO ls_vmds_ei_extern
                     WITH KEY header-object_instance-lifnr = ls_bp_suppl_link-supplier.

          IF sy-subrc = 0.
            ls_cvis_extern-vendor = ls_vmds_ei_extern.
          ENDIF.
        ENDIF.
        APPEND ls_cvis_extern TO et_supplier.
      ENDLOOP.
    ENDIF.
  ENDMETHOD.


  METHOD read_vendor_by_lifnr.
    DATA:
      ls_vmds_ei_main    TYPE vmds_ei_main,
      ls_vmds_ei_extern  TYPE vmds_ei_extern,
      ls_vmds_ei_main_in TYPE vmds_ei_main,
      ls_cvis_extern     TYPE cvis_ei_extern,
      lv_partner_guid    TYPE bu_partner_guid.

    FIELD-SYMBOLS:
                   <ls_idlist> TYPE bus_ei_instance.

    IF iv_lifnr IS NOT INITIAL.
      ls_vmds_ei_extern-header-object_task = 'M'.
      ls_vmds_ei_extern-header-object_instance-lifnr = iv_lifnr.
      APPEND ls_vmds_ei_extern TO ls_vmds_ei_main_in-vendors.
    ENDIF.

    IF NOT ls_vmds_ei_main_in IS INITIAL.
* Extract Vendors
      vmd_ei_api_extract=>get_data(
                            EXPORTING
                              is_master_data = ls_vmds_ei_main_in
                            IMPORTING
                              es_master_data = ls_vmds_ei_main
                            EXCEPTIONS
                              OTHERS = 1 ).

      LOOP AT ls_vmds_ei_main-vendors INTO DATA(ls_vendors). "only one entry expected
        es_supplier-vendor = ls_vendors.
        EXIT.
      ENDLOOP.
    ENDIF.

  ENDMETHOD.
ENDCLASS.
