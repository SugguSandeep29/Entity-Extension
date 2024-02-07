*&---------------------------------------------------------------------*
*& Report ZIN_PRG_DYN_PRG
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zin_prg_dyn_prg.

CONSTANTS lc_left TYPE tabname VALUE 'SFLIGHT'.

TYPES ty_include_list TYPE HASHED TABLE OF string
                   WITH UNIQUE KEY table_line.

DATA: lv_right    TYPE tabname VALUE 'SPFLI',
      lt_select   TYPE TABLE OF edpline,
      lv_sublist  TYPE edpline,
      lv_from     TYPE string,
      lv_first_on TYPE abap_bool VALUE abap_true,
      lr_first_on TYPE REF TO data.

cl_demo_input=>request(
  EXPORTING text  = `lv_right Table of Join`
  CHANGING  field = lv_right ).

FINAL(ty_include_list) = VALUE ty_include_list( ( `SPFLI` )
                                          ( `SCARR` )
                                          ( `SAPLANE` ) ).

TRY.
    lv_right = cl_abap_dyn_prg=>check_whitelist_tab(
              val = to_upper( lv_right )
              whitelist = ty_include_list ).
  CATCH cx_abap_not_in_whitelist.
    cl_demo_output=>write(
      `Only the following tables are allowed:` ).
    cl_demo_output=>display( ty_include_list ).
    LEAVE PROGRAM.
ENDTRY.

lv_first_on = abap_true.
CLEAR lt_select.
CLEAR lv_sublist.
CLEAR lv_from.
READ CURRENT LINE LINE VALUE INTO lv_right.

DATA(comp_tab1) =
  CAST cl_abap_structdescr( cl_abap_typedescr=>describe_by_name(
                              lc_left ) )->get_components( ).
DATA(comp_tab2) =
  CAST cl_abap_structdescr( cl_abap_typedescr=>describe_by_name(
                              lv_right ) )->get_components( ).

DELETE comp_tab1 WHERE name = 'MANDT'.
DELETE comp_tab2 WHERE name = 'MANDT'.

lv_from = lc_left && ` join ` && lv_right && ` on `.

LOOP AT comp_tab1 INTO DATA(comp1) WHERE name IS NOT INITIAL.
  lv_sublist = lc_left && '~' && comp1-name && ','.
  APPEND lv_sublist TO lt_select.
ENDLOOP.

LOOP AT comp_tab2 INTO FINAL(comp2) WHERE name IS NOT INITIAL.
  TRY.
      comp1 = comp_tab1[ KEY primary_key name = comp2-name ].
      IF lv_first_on = abap_false.
        lv_from = lv_from && ` and `.
      ELSE.
        lv_first_on = abap_false.
      ENDIF.
      lv_from = lv_from && lc_left  && `~` && comp2-name &&
            ` = ` && lv_right && `~` && comp2-name.
    CATCH cx_sy_itab_line_not_found.
      APPEND comp2 TO comp_tab1.
      lv_sublist = lv_right && '~' && comp2-name && ','.
      APPEND lv_sublist TO lt_select.
  ENDTRY.
ENDLOOP.

FINAL(struct_type) = cl_abap_structdescr=>create( comp_tab1 ).
FINAL(table_type) = cl_abap_tabledescr=>create( struct_type ).
CREATE DATA lr_first_on TYPE HANDLE table_type.

ASSIGN lt_select[ lines( lt_select ) ] TO FIELD-SYMBOL(<comp>).
REPLACE `,` IN <comp>  WITH ``.
TRY.
    SELECT (lt_select) FROM (lv_from) INTO TABLE @lr_first_on->* UP TO 1000 ROWS.
    cl_demo_output=>display( lr_first_on->* ).
  CATCH cx_sy_dynamic_osql_syntax
        cx_sy_dynamic_osql_semantics INTO FINAL(exc).
    cl_demo_output=>display( exc->get_text( ) ).
ENDTRY.
