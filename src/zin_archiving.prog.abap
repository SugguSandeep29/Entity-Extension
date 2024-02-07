REPORT zin_archiving.

TYPES:
  BEGIN OF ty_output,
    table_name TYPE tabname,
    used_size  TYPE p DECIMALS 2,
    arch_obj   TYPE objct_tr01,
    appli_tr01 TYPE ufps_posid,
    appli_desc TYPE ddtext,
  END OF ty_output,

  BEGIN OF ty_output_col,
    table_name                     TYPE tabname,
    estim_max_memory_size_in_total TYPE p DECIMALS 2,
    arch_obj                       TYPE objct_tr01,
    appli_tr01                     TYPE ufps_posid,
    appli_desc                     TYPE ddtext,
  END OF ty_output_col,

  BEGIN OF ty_output_row,
    table_name          TYPE tabname,
    allocated_part_size TYPE p DECIMALS 2,
    arch_obj            TYPE objct_tr01,
    appli_tr01          TYPE ufps_posid,
    appli_desc          TYPE ddtext,
  END OF ty_output_row.


DATA: fs_output_col TYPE ty_output_col,
      it_output_col TYPE TABLE OF ty_output_col,
      fs_output_row TYPE ty_output_row,
      it_output_row TYPE TABLE OF ty_output_row,
      fs_output     TYPE ty_output,
      it_output     TYPE TABLE OF ty_output.

DATA rdi           TYPE REF TO cl_dba_rdi.
DATA db6           TYPE REF TO cl_db6_sys.
DATA lv_to_date    TYPE sydatum.
DATA lv_to_time    TYPE syuzeit.
DATA lv_schema     TYPE char128.
DATA lv_sort_field_col TYPE string VALUE 'ESTIM_MAX_MEMORY_SIZE_IN_TOTAL'. "'DISK_SIZE'.
DATA lv_sort_field_row TYPE string VALUE 'ALLOCATED_PART_SIZE'."'DISK_SIZE'.
DATA system_id     TYPE sysysid VALUE 'S4X'.
DATA view_data     TYPE hdb_column_tables_part_sizealv.
DATA lv_disksize   TYPE p LENGTH 16.

DATA: lr_events TYPE REF TO cl_salv_events_table.

SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME.
  SELECT-OPTIONS s_schema FOR lv_schema.
  PARAMETERS p_disk TYPE int4.
SELECTION-SCREEN END OF BLOCK b1.

INITIALIZATION.
  s_schema-option = 'EQ'.
  s_schema-sign = 'I'.
  s_schema-low = 'SAPHANADB'.
  APPEND s_schema.


**********************************************************************
CLASS lcl_double_click DEFINITION.
  PUBLIC SECTION.
    METHODS:
      on_double_click FOR EVENT double_click OF cl_salv_events_table
        IMPORTING row column.
ENDCLASS.

CLASS lcl_double_click IMPLEMENTATION.
  METHOD on_double_click.

  ENDMETHOD.
ENDCLASS.
**********************************************************************
START-OF-SELECTION.

  IF p_disk IS NOT INITIAL.
    lv_disksize = p_disk * 1073741824.
  ENDIF.

  db6 = cl_db6_sys=>get_sys_ref( system_id   = system_id
                                 synchronize = abap_false ).

  TRY.
      CALL METHOD cl_dba_rdi=>get_instance
        EXPORTING
          sys_ref = db6
        RECEIVING
          rdi_ref = rdi.
    CATCH cx_dba_rdi.
  ENDTRY.

  rdi->query->reset( ).

  rdi->query->get_history( EXPORTING ddic_src  = cl_hdb_rdi_meta=>co_ddic_column_tables_part_siz
                           IMPORTING to_date   = lv_to_date
                                     to_time   = lv_to_time ).

  rdi->query->reset( ).
  rdi->query->set_history( from_time = lv_to_time
                           from_date = lv_to_date ).

  rdi->query->set_filter_from_range_tab( ddic_field = 'SCHEMA_NAME'
                                         range_tab  = s_schema[] ).

  rdi->query->set_sort( order      = cl_dba_rdi_query=>co_descending
                        seq_no     = 1
                        ddic_field = lv_sort_field_col ).

  rdi->query->get_snapshot( EXPORTING ddic_src = cl_hdb_rdi_meta=>co_ddic_column_tables_part_siz
                            IMPORTING data     = view_data-host_column_tables_part_size ).


  DELETE view_data-host_column_tables_part_size WHERE memory_size_in_total < lv_disksize.

  SELECT object,
         son
    FROM arch_def AS arch_def
    INNER JOIN @view_data-host_column_tables_part_size AS data
       ON data~table_name EQ arch_def~son
    INTO TABLE @DATA(it_arch_def).

  SELECT tadir~obj_name,
         df14l~ps_posid,
         df14t~name
    FROM tdevc
   INNER JOIN tadir
      ON tadir~devclass EQ tdevc~devclass
   INNER JOIN @view_data-host_column_tables_part_size AS data
      ON tadir~obj_name EQ data~table_name
   INNER JOIN df14l
      ON df14l~fctr_id EQ tdevc~component
   INNER JOIN df14t
      ON df14t~langu EQ @sy-langu
     AND df14t~fctr_id EQ tdevc~component
    INTO TABLE @DATA(it_comp).

  LOOP AT view_data-host_column_tables_part_size INTO DATA(fs_data).
    CLEAR fs_output_col.
    MOVE-CORRESPONDING fs_data TO fs_output_col.
    fs_output_col-estim_max_memory_size_in_total = fs_output_col-estim_max_memory_size_in_total / 1073741824.

    READ TABLE it_comp INTO DATA(fs_comp) WITH KEY obj_name = fs_data-table_name.
    IF sy-subrc EQ 0.
      MOVE fs_comp-ps_posid TO fs_output_col-appli_tr01.
      MOVE fs_comp-name TO fs_output_col-appli_desc.
    ENDIF.

    LOOP AT it_arch_def INTO DATA(fs_arch_def) WHERE son = fs_data-table_name.
      MOVE fs_arch_def-object TO fs_output_col-arch_obj.
*      APPEND fs_output_col TO it_output_col.
      APPEND fs_output_col TO it_output.
    ENDLOOP.
    IF sy-subrc NE 0.
*      APPEND fs_output_col TO it_output_col.
      APPEND fs_output_col TO it_output.
    ENDIF.

  ENDLOOP.

*******************************Row type table*****************************
  CLEAR view_data.

  rdi->query->reset( ).

  rdi->query->get_history( EXPORTING ddic_src  = cl_hdb_rdi_meta=>co_ddic_global_rowstore_tbl_sz"cl_hdb_rdi_meta=>co_ddic_glob_persistence_stat
                           IMPORTING to_date   = lv_to_date
                                     to_time   = lv_to_time ).

  rdi->query->reset( ).
  rdi->query->set_history( from_time = lv_to_time
                           from_date = lv_to_date ).

  rdi->query->set_filter_from_range_tab( ddic_field = 'SCHEMA_NAME'
                                         range_tab  = s_schema[] ).

  rdi->query->set_sort( order      = cl_dba_rdi_query=>co_descending
                        seq_no     = 1
                        ddic_field = lv_sort_field_row ).

  rdi->query->get_snapshot( EXPORTING ddic_src = cl_hdb_rdi_meta=>co_ddic_global_rowstore_tbl_sz"cl_hdb_rdi_meta=>co_ddic_glob_persistence_stat
                            IMPORTING data     = view_data-hdb_global_rowstore_table_size )."view_data-hdb_global_table_persist_stat ).

  DELETE view_data-hdb_global_rowstore_table_size WHERE allocated_part_size < lv_disksize.

  SELECT object,
         son
    FROM arch_def AS arch_def
    INNER JOIN @view_data-hdb_global_rowstore_table_size AS data
       ON data~table_name EQ arch_def~son
    INTO TABLE @DATA(it_arch_def_row).

  SELECT tadir~obj_name,
         df14l~ps_posid,
         df14t~name
    FROM tdevc
   INNER JOIN tadir
      ON tadir~devclass EQ tdevc~devclass
   INNER JOIN @view_data-hdb_global_rowstore_table_size AS data
      ON tadir~obj_name EQ data~table_name
   INNER JOIN df14l
      ON df14l~fctr_id EQ tdevc~component
   INNER JOIN df14t
      ON df14t~langu EQ @sy-langu
     AND df14t~fctr_id EQ tdevc~component
    INTO TABLE @DATA(it_comp_row).

  LOOP AT view_data-hdb_global_rowstore_table_size INTO DATA(fs_data_row).
    CLEAR fs_output_row.
    MOVE-CORRESPONDING fs_data_row TO fs_output_row.
    fs_output_row-allocated_part_size = fs_output_row-allocated_part_size / 1073741824.

    READ TABLE it_comp_row INTO DATA(fs_comp_row) WITH KEY obj_name = fs_data_row-table_name.
    IF sy-subrc EQ 0.
      MOVE fs_comp_row-ps_posid TO fs_output_row-appli_tr01.
      MOVE fs_comp_row-name TO fs_output_row-appli_desc.
    ENDIF.

    LOOP AT it_arch_def_row INTO DATA(fs_arch_def_row) WHERE son = fs_data_row-table_name.
      MOVE fs_arch_def_row-object TO fs_output_row-arch_obj.
*      APPEND fs_output_row TO it_output_row.
      APPEND fs_output_row TO it_output.
    ENDLOOP.
    IF sy-subrc NE 0.
*      APPEND fs_output_row TO it_output_row.
      APPEND fs_output_row TO it_output.
    ENDIF.
*    APPEND fs_output_row TO it_output.
  ENDLOOP.

  TRY.

      CALL METHOD cl_salv_table=>factory
        IMPORTING
          r_salv_table = DATA(lcl_salv)
        CHANGING
          t_table      = it_output.
    CATCH cx_salv_msg.
  ENDTRY.

  CALL METHOD lcl_salv->get_columns
    RECEIVING
      value = DATA(lcl_columns).

**********************************************************************
  lr_events = lcl_salv->get_event( ).
  DATA(lr_lcl_event) = NEW lcl_double_click( ).

  SET HANDLER lr_lcl_event->on_double_click FOR lr_events.
**********************************************************************

  lcl_columns->set_optimize( 'X' ).
  DATA(lcl_column) = lcl_columns->get_column( `USED_SIZE` ).
  lcl_column->set_long_text( `Used Size in GB`).
  DATA(lcl_column_desc) = lcl_columns->get_column( `APPLI_DESC` ).
  lcl_column_desc->set_long_text( `Appli. Area Description`).

  DATA(lcl_func) = lcl_salv->get_functions( ).
  lcl_func->set_all( if_salv_c_bool_sap=>true ).

  CALL METHOD lcl_salv->display.


  DATA it_table TYPE TABLE OF vbeln.
  DATA ls_cannotarch TYPE i.


  SELECT *
    FROM zarchobj_tables
    INTO TABLE @DATA(it_arch_tables).
  IF sy-subrc EQ 0.
    LOOP AT it_arch_tables INTO DATA(ls_arch_tables).
      SELECT (ls_arch_tables-fields)
        FROM (ls_arch_tables-table_name)
        INTO TABLE it_table.
      IF sy-subrc EQ 0.
        WRITE:/ 'Total Documents', sy-dbcnt.
      ENDIF.

      SELECT (ls_arch_tables-fields)
        FROM (ls_arch_tables-table_name)
        INTO TABLE it_table
       WHERE (ls_arch_tables-zwhere).
      IF sy-subrc EQ 0.
        WRITE:/ 'Data that can be archived', sy-dbcnt.
      ENDIF.
    ENDLOOP.
  ENDIF.
