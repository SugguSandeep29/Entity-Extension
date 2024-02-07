*&---------------------------------------------------------------------*
*& Report ZIN_PRG_PRACTICE
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zin_prg_practice MESSAGE-ID zin_cl_msg.

**********************************************************************
*TABLES sscrfields.
*
*SELECTION-SCREEN BEGIN OF SCREEN 1100.
*PARAMETERS: p_carrid TYPE s_carr_id,
*            p_cityfr TYPE s_from_cit.
*SELECTION-SCREEN: FUNCTION KEY 1,
*                  FUNCTION KEY 2.
*SELECTION-SCREEN END OF SCREEN 1100.
*
*AT SELECTION-SCREEN.
*  CASE sscrfields-ucomm.
*      WHEN'FC01'.
*      p_carrid = 'LH'.
*      p_cityfr = 'Frankfurt'.
*    WHEN 'FC02'.
*      p_carrid = 'UA'.
*      p_cityfr = 'Chicago'.
*  ENDCASE.
*
*CLASS start DEFINITION.
*  PUBLIC SECTION.
*    CLASS-METHODS main.
*ENDCLASS.
*
*CLASS start IMPLEMENTATION.
*  METHOD main.
*
*    sscrfields-functxt_01 = 'LH'.
*    sscrfields-functxt_02 = 'UA'.
*
*    CALL SELECTION-SCREEN 1100 STARTING AT 10 10.
*
*  ENDMETHOD.
*ENDCLASS.
*
*START-OF-SELECTION.
*  start=>main( ).
**********************************************************************

*DATA(lv_find) = count( val = 'tapper' sub = 'p' ).
*
*WRITE lv_find.
*TYPES: BEGIN OF ty_val,
*         value TYPE c,
*         count TYPE i,
*       END OF ty_val.
*
*DATA: lt_count_tab TYPE TABLE OF ty_val,
*      lt_candidate TYPE TABLE OF string.

**********************************************************************
*DATA: lt_tab       TYPE TABLE OF alsmex_tabline,
*      lt_res       TYPE TABLE OF string,
*      lt_res_final TYPE TABLE OF string.

*CALL FUNCTION 'ALSM_EXCEL_TO_INTERNAL_TABLE'
*  EXPORTING
*    filename                = 'C:\USERS\ADITYA.INGALE\DOWNLOADS\ROLES IN Q17.XLSX'
*    i_begin_col             = 1
*    i_begin_row             = 1
*    i_end_col               = 100
*    i_end_row               = 9999
*  TABLES
*    intern                  = lt_tab
*  EXCEPTIONS
*    inconsistent_parameters = 1
*    upload_ole              = 2
*    OTHERS                  = 3.
*IF sy-subrc <> 0.
*  MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
*    WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
*ENDIF.
*
*LOOP AT lt_tab ASSIGNING FIELD-SYMBOL(<ls_tab>).
*  IF sy-tabix EQ 1.
*    CONTINUE.
*  ELSE.
*    IF <ls_tab>-col = '0001'.
*      APPEND <ls_tab>-value TO lt_res.
*    ENDIF.
*  ENDIF.
*
*ENDLOOP.
*
*DATA(lt_res_q17) = lt_res.
*REFRESH lt_res.

*CALL FUNCTION 'ALSM_EXCEL_TO_INTERNAL_TABLE'
*  EXPORTING
*    filename                = 'C:\USERS\ADITYA.INGALE\DOWNLOADS\ROLES IN C17.XLSX'
*    i_begin_col             = 1
*    i_begin_row             = 1
*    i_end_col               = 100
*    i_end_row               = 9999
*  TABLES
*    intern                  = lt_tab
*  EXCEPTIONS
*    inconsistent_parameters = 1
*    upload_ole              = 2
*    OTHERS                  = 3.
*IF sy-subrc <> 0.
*  MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
*    WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
*ENDIF.
*
*LOOP AT lt_tab ASSIGNING FIELD-SYMBOL(<ls_tab>).
*  IF sy-tabix EQ 1.
*    CONTINUE.
*  ELSE.
*    IF <ls_tab>-col = '0001'.
*      APPEND <ls_tab>-value TO lt_res.
*    ENDIF.
*  ENDIF.
*
*ENDLOOP.
*
*DATA(lt_res_c17) = lt_res.
*REFRESH lt_res.
*
*CALL FUNCTION 'ALSM_EXCEL_TO_INTERNAL_TABLE'
*  EXPORTING
*    filename                = 'C:\Users\aditya.ingale\Downloads\Christina_c17.XLSX'
*    i_begin_col             = 1
*    i_begin_row             = 1
*    i_end_col               = 100
*    i_end_row               = 9999
*  TABLES
*    intern                  = lt_tab
*  EXCEPTIONS
*    inconsistent_parameters = 1
*    upload_ole              = 2
*    OTHERS                  = 3.
*IF sy-subrc <> 0.
*  MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
*    WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
*ENDIF.
*
*LOOP AT lt_tab ASSIGNING <ls_tab>.
*  IF sy-tabix EQ 1.
*    CONTINUE.
*  ELSE.
*    IF <ls_tab>-col = '0001'.
*      APPEND <ls_tab>-value TO lt_res.
*    ENDIF.
*  ENDIF.
*
*ENDLOOP.
*
*DATA(lt_res_d17) = lt_res.
*
*cl_demo_output=>write( lt_res_c17 ).
*cl_demo_output=>write( lines( lt_res_c17 ) ).
**
*cl_demo_output=>write( lt_res_d17 ).
*cl_demo_output=>write( lines( lt_res_d17 ) ).
*
*cl_demo_output=>write( lt_res_q17 ).
*cl_demo_output=>display( lines( lt_res_q17 ) ).
*
*LOOP AT lt_res_c17 ASSIGNING FIELD-SYMBOL(<ls_res_c17>).
*  IF not line_exists( lt_res_d17[ table_line = <ls_res_c17> ] ).
*    APPEND <ls_res_c17> TO lt_res_final.
*  ENDIF.
*ENDLOOP.
*
*cl_demo_output=>display( lt_res_final ).

*********************************************************************
*DATA(lo_json) = NEW /ui2/cl_json( ).
*
*SELECT * FROM scarr INTO TABLE @DATA(lt_scarr).
*
*lo_json->serialize(
*  EXPORTING
*    data             = lt_scarr                 " Data to serialize
**    compress         =                  " Skip empty elements
**    name             =                  " Object name
**    pretty_name      =                  " Pretty Print property names
**    type_descr       =                  " Data descriptor
**    assoc_arrays     =                  " Serialize tables with unique keys as associative array
**    ts_as_iso8601    =                  " Dump timestamps as string in ISO8601 format
**    expand_includes  =                  " Expand named includes in structures
**    assoc_arrays_opt =                  " Optimize rendering of name value maps
**    numc_as_string   =                  " Serialize NUMC fields as strings
**    name_mappings    =                  " ABAP<->JSON Name Mapping Table
**    conversion_exits =                  " Use DDIC conversion exits on serialize of values
**    format_output    =                  " Indent and split in lines serialized JSON
**    hex_as_base64    =                  " Serialize hex values as base64
*  RECEIVING
*    r_json           = DATA(ls_json_data)                 " JSON string
*).
*
*
*cl_demo_output=>display( ls_json_data ).

**********************************************************************

*PARAMETERS: p_n1 TYPE int3,
*            p_n2 TYPE int3,
*            r_b1 RADIOBUTTON GROUP g1,
*            r_b2 RADIOBUTTON GROUP g1,
*            r_b3 RADIOBUTTON GROUP g1,
*            r_b4 RADIOBUTTON GROUP g1.
*
*DATA: lv_result TYPE p DECIMALS 3.
*
*CASE abap_true.
*  WHEN r_b1.
*    lv_result = p_n1 + p_n2.
*  WHEN r_b2.
*    lv_result = p_n1 - p_n2.
*  WHEN r_b3.
*    lv_result = p_n1 * p_n2.
*  WHEN r_b4.
*    lv_result = p_n1 / p_n2.
*  WHEN OTHERS.
*    MESSAGE 'No Option has selected' TYPE 'E'.
*ENDCASE.
*WRITE: 'Result:', lv_result.

**********************************************************************

*DATA: lv_string TYPE string,
*      lv_result TYPE string,
*      lv_var1   TYPE string,
*      lv_var2   TYPE string,
*      lv_var3   TYPE string.
*
*DATA: lt_result TYPE TABLE OF string.
*
*lv_string = '(240)1007712(10)0001212585(21)10'.
*
*REPLACE ALL OCCURRENCES OF PCRE '\(\d+\)' IN lv_string WITH '-'.
*
*DO.
*  SPLIT lv_string AT '-' INTO lv_var1 lv_var2.
*  APPEND lv_var1 TO lt_result.
*  lv_string = lv_var2.
*  IF lv_var2 IS INITIAL.
*    EXIT.
*  ENDIF.
*ENDDO.
*
*cl_demo_output=>display( lt_result ).



**********************************************************************

*DATA: lv_string TYPE string VALUE '(240)1007712',
*      lv_var1,
*      n.
*
*DATA(lv_len) = strlen( lv_string ).
*
*DO.
*  IF lv_len = n - 1.
*    EXIT.
*  ENDIF.
*  lv_var1 = lv_string+n(1).
*  WRITE: / lv_var1.
*  n += 1.
*ENDDO.

**********************************************************************
*SELECT-OPTIONS: s_dat FOR sy-datum.
*
*INITIALIZATION.
*
*  CALL FUNCTION 'CCM_GO_BACK_MONTHS'
*    EXPORTING
*      currdate   = sy-datum
*      backmonths = 6
*    IMPORTING
*      newdate    = s_dat-low.
*
*  s_dat-high = sy-datum.
*
*  APPEND s_dat.
*
*s_dat-low = sy-datum.


**********************************************************************
*types: begin of ty_num,
*         num1 type num10,
*         num2 type char10,
*       end of ty_num.
*
*
*DATA: ls_num type ty_num.
*
*DATA(lv_text) = '1234567890,abcde'.
*
*SPLIT lv_text at ',' into ls_num-num1 ls_num-num2.
*
*WRITE: ls_num-num1, / ls_num-num2.

**********************************************************************
*DATA: lt_table type table of spfli.
*
*SELECT * FROM spfli
*   WHERE
*      like_regexpr( pcre = '\bBEL\b|\bTOKYO\b', value = cityfrom ) = '1'
*   INTO @DATA(ls_table).
*   APPEND ls_table TO lt_table.
*ENDSELECT.
*
*
*cl_demo_output=>display( lt_table ).

**********************************************************************


*DATA: lv_matnr TYPE matnr.
**      lv_mat type matnr.
*
*
*
*lv_matnr = '18'.
*
*CALL FUNCTION 'CONVERSION_EXIT_MATN1_INPUT'
*  EXPORTING
*    input  = lv_matnr
*  IMPORTING
*    output = lv_matnr.
*
*DATA(lv_mat) = |{ lv_matnr }%|.

*SELECT matnr, mtart, mbrsh FROM mara
*  WHERE like_regexpr( pcre = '[0]18.', value = matnr  ) = '1'
*  INTO TABLE @DATA(lt_mara) .
*
*SORT lt_mara by matnr.
*
*cl_demo_output=>display( lt_mara ).



**********************************************************************
*GET TIME STAMP FIELD DATA(lv_tmstmp).
*
*write lv_tmstmp.
*
*DATA(lv_result) = |{ lv_tmstmp TIMESTAMP = RAW }|.
*
*WRITE / lv_result.

*DATA: lv_dat TYPE dats.
*
*lv_dat = '23/06/1999'.
*
*WRITE lv_dat.
*
*DATA(lv_date) = sy-datum.
*WRITE / lv_date.
*
*DATA(lv_result) = |{ lv_date DATE = RAW }|.
*WRITE / lv_result.
**********************************************************************

*TABLES: bkpf.
*SELECTION-SCREEN BEGIN OF BLOCK blk1.
*  SELECT-OPTIONS: s_postdy FOR bkpf-budat OBLIGATORY.
*SELECTION-SCREEN END OF BLOCK blk1.
*
*TYPES: lrngty_yr_of_posting_dy TYPE RANGE OF bkpf-gjahr.
*
*DATA(lrng_yr_of_posting_dy) = VALUE lrngty_yr_of_posting_dy(
*                              FOR ls_postdy IN s_postdy[]
*                              LET lv_low_yr = ls_postdy-low(4)
*                              lv_high_yr = ls_postdy-high(4)
*                              IN ( low = lv_low_yr
*                                   high = lv_high_yr
*                                   sign = ls_postdy-sign
*                                   option = ls_postdy-option ) ).
*
*DATA(lrng_yr_of_posting_dy) = VALUE lrngty_yr_of_posting_dy(
*                              FOR ls_postdy IN s_postdy[]
*                             ( low = ls_postdy-low(4)
*                              high = ls_postdy-high(4)
*                              sign = ls_postdy-sign
*                              option = ls_postdy-option ) ).
*
*cl_demo_output=>display( lrng_yr_of_posting_dy ).
**********************************************************************

*
*SELECT * FROM vbsegk INTO TABLE @DATA(lt_tab) UP TO 10 ROWS WHERE bukrs = 'CC01' .
*
*  cl_demo_output=>display( lt_tab ).

**********************************************************************
*
*DATA: lv_val TYPE hoehe,
*      lv_int TYPE i.
*
*SELECT matnr, laeng, breit, hoehe
*  FROM marm
*  WHERE laeng IS NOT INITIAL
*  INTO TABLE @DATA(lt_marm)
*    UP TO 10 ROWS .
*
*
*LOOP AT lt_marm INTO DATA(ls_marm).
*  WRITE / ls_marm-laeng.
*  lv_int = ls_marm-laeng / '30.48'.
*  WRITE lv_int.
*ENDLOOP.



**********************************************************************

*DATA: lv_langu TYPE string.
*
*CALL FUNCTION 'CONVERSION_EXIT_ISOLA_OUTPUT'
*  EXPORTING
*    input  = 'DE'
*  IMPORTING
*    output = lv_langu.
*
**DATA(lv_langu) = |{ is_gen-langu ALPHA = OUT }|.
*
*ASSERT 1 = 1.

**********************************************************************
*TYPES: BEGIN OF ty_fina,
*         intad    TYPE intad,
*         intad_cc TYPE intad,
*       END OF ty_fina,
*
*       BEGIN OF ty_email,
*         e_mail TYPE char255,
*       END OF ty_email,
*
*       BEGIN OF ty_test,
*         mail TYPE char255,
*       END OF ty_test.
*
*DATA: c_finaa  TYPE ty_fina,
*      lt_add   TYPE TABLE OF ty_email,
*      lt_email TYPE TABLE OF ty_test.
*
*
*c_finaa-intad = 'test@test.com best@test.com cool@bool.com fool@cool.com wrong@cc'.
*
*lt_add = VALUE #( ( e_mail = 'test@test.com' )
*                  ( e_mail = 'best@test.com' )
*                  ( e_mail = 'cool@bool.com' )
*                  ( e_mail = 'fool@cool.com' )
*                  ( e_mail = 'apple@cool.com')
*                  ( e_mail = 'ball@cool.com' ) ).
*
*
*DATA(lv_intad) = c_finaa-intad.

*DO.
*  SPLIT lv_intad AT ' ' INTO DATA(lv_str1) DATA(lv_str2).
*  APPEND lv_str1 TO lt_email.
*  IF lv_str2 IS INITIAL.
*    EXIT.
*  ENDIF.
*  lv_intad = lv_str2.
*ENDDO.


*LOOP AT lt_add ASSIGNING FIELD-SYMBOL(<fs_add>).
*
*  DATA(lv_count) = count( val = c_finaa-intad sub = <fs_add>-e_mail ).
*
*  IF lv_count NE 0.
*    CONTINUE.
*  ELSE.
*    c_finaa-intad_cc = c_finaa-intad_cc && ` ` && <fs_add>-e_mail.
*    CONDENSE c_finaa-intad_cc.
*  ENDIF.
*
*  CLEAR lv_count.
*
*ENDLOOP.
*
*cl_demo_output=>display( c_finaa ).

**********************************************************************
*DATA: o_dock TYPE REF TO cl_gui_docking_container.
*DATA: o_salv_ida TYPE REF TO if_salv_gui_table_ida.
*
*PARAMETERS: p_name TYPE string. " Dummy-Parameter zum erzwingen des Selektionsbildes
*
*AT SELECTION-SCREEN OUTPUT.
*
*  IF NOT o_dock IS BOUND.
** maximierten Dockingcontainer auf dem Selektionsbild erzeugen
*    o_dock = NEW #( repid     = sy-repid
*                    dynnr     = sy-dynnr
*                    side      = cl_gui_docking_container=>dock_at_bottom
*                    extension = cl_gui_docking_container=>ws_maximizebox
*                    ratio     = 90
*                  ).

* Testweise ein SALV TABLE IDA im Container einbetten
*    o_salv_ida = cl_salv_gui_table_ida=>create( iv_table_name    = 'SCARR'
*                                                io_gui_container = o_dock ).


*  ENDIF.

**********************************************************************
*DATA: lv_final, lv_str1, lv_str2.
*
*DATA(lv_string) = 'FOUR LAKH EIGHTEEN Rupees NINETY TWO Paise'.
*
*
*Do.
*  SPLIT lv_string AT ` ` INTO lv_str1 lv_str2.
*
*  DATA(lv_result) = to_mixed( lv_str1 ).
**DATA(lv_result) = from_mixed( VAL = lv_string sep = `_ ` ).
*
*  lv_final = |{ lv_final } { lv_result }|.
*
*  lv_string = lv_str2.
*ENDDO.
*
*  WRITE: lv_final.

**********************************************************************
*SELECT SINGLE MAX( obknr ), MAX( datum )
*  INTO ( @DATA(wa_obknr) , @DATA(wa_datum) )
*  FROM objk
*  WHERE equnr = '000000000010000241' AND
*        taser = 'SER01'.
*       GROUP BY obknr, datum.

*SELECT SINGLE obknr, datum
*    FROM objk
*    WHERE equnr = '000000000010000241' AND taser = 'SER01'
*      AND obknr IN ( SELECT MAX( obknr )
*                    FROM objk AS objk_2
*                    WHERE equnr = objk~equnr
*                      AND taser = objk~taser )
*    INTO (@DATA(wa_obknr), @DATA(wa_datum)).

*  cl_demo_output=>write( wa_obknr ).
*  cl_demo_output=>display( wa_datum ).

**********************************************************************
*DATA: lv_budat TYPE budat.
*
*SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE lv_text.
*  SELECT-OPTIONS: s_budat FOR lv_budat OBLIGATORY.
*SELECTION-SCREEN END OF BLOCK b1.
*
*INITIALIZATION.
*  MESSAGE E000 INTO lv_text.
*
*AT SELECTION-SCREEN.
*
*  IF s_budat-high - s_budat-low > 60.
*    MESSAGE 'You have Entered Date Range more than 60 Days' TYPE 'E'.
*  ENDIF.


**********************************************************************

*TYPES: BEGIN OF ty_tab,
*         vkorg TYPE vkorg,
*         vtweg TYPE vtweg,
*         spart TYPE spart,
*         boart TYPE boart,
*       END OF ty_tab.
*
*DATA: lt_tab TYPE TABLE OF ty_tab,
*      ls_tab TYPE ty_tab.
*
*DATA(lv_string) = 'AE01#10#1#ZF01'.
*
*  DO 4 TIMES.
*
*    SPLIT lv_string AT '#' INTO DATA(lv_str1) DATA(lv_str2).
*    ASSIGN COMPONENT sy-index OF STRUCTURE ls_tab TO FIELD-SYMBOL(<lv_data>).
*    <lv_data> = lv_str1.
*    lv_string = lv_str2.
*
*  ENDDO.
*
*  APPEND ls_tab to lt_tab.
*
*cl_demo_output=>display( lt_tab ).

**********************************************************************

*delete from zin_t_crtype_cpy.

*DATA: lt_table TYPE TABLE OF zin_t_demo.
*
*
*
*cl_demo_output=>display( lt_table ).

**********************************************************************

*DATA: lt_sales TYPE TABLE OF zin_t_vbak.
*
*SELECT vbeln erdat ernam FROM vbak INTO CORRESPONDING FIELDS OF TABLE lt_sales UP TO 10 ROWS ORDER BY  vbeln.
*
*cl_demo_output=>display( lt_sales ).
*
*DELETE FROM zin_t_vbak.
*MODIFY zin_t_vbak FROM TABLE lt_sales.

**********************************************************************
*DATA: lt_cr TYPE TABLE OF zin_t_crtype,
*      ls_cr TYPE zin_t_crtype.
*
*
*SELECT crtype~usmd_creq_type
*        text~txtmi
*        FROM usmd110c AS crtype
*        INNER JOIN usmd110t AS text
*        ON crtype~usmd_creq_type = text~usmd_creq_type
*        INTO CORRESPONDING FIELDS OF TABLE lt_cr
*        WHERE crtype~usmd_creq_type LIKE 'Z%' AND
*              text~langu = sy-langu
*        ORDER BY crtype~usmd_creq_type.
*
*cl_demo_output=>display( lt_cr ).
*MODIFY zin_t_crtype FROM TABLE lt_cr.
**DELETE FROM zin_t_crtype.
*
*IF sy-subrc = 0.
*  WRITE : 'Data Loaded successfully'.
*ENDIF.

**********************************************************************
*SELECT mara~matnr,
*       mara~mtart,
*       mara~mbrsh,
*       makt~maktx
*         FROM mara
*  inner join makt
*  ON mara~matnr = makt~matnr
*  INTO TABLE @DATA(lt_mara)
*  UP TO 200 ROWS
**  WHERE makt~spras EQ @sy-langu
*  ORDER BY mara~matnr.
*
*cl_demo_output=>display( lt_mara ).

**********************************************************************
*CALL METHOD zmdg_cl_utilities=>get_transport_requests
*  IMPORTING
*    et_transports = DATA(lt_tr).                 " MDG: Table Type for Package/Request Structure
*
*
*cl_demo_output=>display( lt_tr ).

**********************************************************************
** To get the whare used list of perticular structure

*PARAMETERS: p_struc TYPE precfield.
*:
*zmdg_cl_utilities=>get_where_used_list_struc(
*  EXPORTING
*    iv_structure       = p_struc                 " Table Name
*  IMPORTING
*    et_where_used_list = DATA(lt_list)                 " DD03L (standard)
*).
*
*LOOP AT lt_list ASSIGNING FIELD-SYMBOL(<ls_field>).
*  WRITE:/ <ls_field>-tabname.
*ENDLOOP.

**********************************************************************
** Final program to extend the structure by creating new stucture

*PARAMETERS: p_trg  TYPE ddobjname,
*            p_src  TYPE ddobjname,
*            p_desc TYPE ddtext.
*
*DATA: lt_fields TYPE TABLE OF dd03p,
*      ls_choise TYPE sy-index.
*
*lt_fields = VALUE #(
*                    ( fieldname = 'EMP_ID'      rollname = 'PERSNO' )
*                    ( fieldname = 'FIRST_NAME'  rollname = 'PAD_VORNA' )
**                    ( fieldname = 'SECOND_NAME' rollname = 'PAD_NACH2' )
*                    ( fieldname = 'LAST_NAME'   rollname = 'PAD_NACHN' )
*                    ( fieldname = 'GENDER'      rollname = 'HRPAD_GENDER' )
*                    ( fieldname = 'DOB'         rollname = 'GBDAT' )
*                                     ).
*** Getting TR's
**
*CALL METHOD zmdg_cl_utilities=>get_transport_requests
*  IMPORTING
*    et_transports = DATA(lt_tr).                 " MDG: Table Type for Package/Request Structure
*
*** Getting Packages
*
*zmdg_cl_utilities=>get_packages(
*  IMPORTING
*    et_packages = DATA(lt_pkg)                 " MDG: Table Type for Package/Request Structure
*).
*
*APPEND VALUE #( id = '$TMP' description = 'Local Package' ) TO lt_pkg.
**
*CALL FUNCTION 'POPUP_TABLE_DISPLAY'
*  EXPORTING
*    endpos_col   = 80                " Ending position of popup
*    endpos_row   = 20                " Ending position of popup
*    startpos_col = 10                " Starting position of popup
*    startpos_row = 10                " Starting position of popup
*    titletext    = 'Package'                " Text in title bar of popup
*  IMPORTING
*    choise       = ls_choise                " Number of table entry
*  TABLES
*    valuetab     = lt_pkg                 " Table with possible values
*  EXCEPTIONS
*    break_off    = 1
*    OTHERS       = 2.
*
*IF sy-subrc = 0.
*  DATA(lv_package) = lt_pkg[ ls_choise ]-id.
*ENDIF.
*
*CLEAR ls_choise.
*IF lv_package+0(1) NE '$'.
*
*  CALL FUNCTION 'POPUP_TABLE_DISPLAY'
*    EXPORTING
*      endpos_col   = 80                " Ending position of popup
*      endpos_row   = 20                " Ending position of popup
*      startpos_col = 10                " Starting position of popup
*      startpos_row = 10                " Starting position of popup
*      titletext    = 'Transport Requests'                " Text in title bar of popup
*    IMPORTING
*      choise       = ls_choise                " Number of table entry
*    TABLES
*      valuetab     = lt_tr                " Table with possible values
*    EXCEPTIONS
*      break_off    = 1
*      OTHERS       = 2.
*
*  IF sy-subrc = 0.
*    DATA(lv_transport) = lt_tr[ ls_choise ]-id.
*  ENDIF.
*ENDIF.
*
*** Extending structure
*
*zmdg_cl_utilities=>extend_structure(
*  EXPORTING
*    iv_target_struc = p_trg                 " Name of ABAP Dictionary Object
*    iv_source_struc = p_src                 " Name of ABAP Dictionary Object
*    it_struc_fields = lt_fields
*    iv_struc_desc   = p_desc
*    iv_package      = lv_package
*    iv_tr_id        = CONV trkorr( lv_transport )
*  IMPORTING
*    ev_success_flg  = DATA(lv_success_flg)
*).
*
*IF lv_success_flg = abap_true.
*  WRITE: 'Structure Extended Successfully'.
*ELSE.
*  WRITE: 'Structure not Extended'.
*ENDIF.
*
*CLEAR: p_trg,
*       p_src,
*       p_desc.

**********************************************************************


**********************************************************************
*** Include Structure in Target Structure


*** Standard POPUP Screen

*DATA: ls_req     TYPE trwbo_request_header,
*      ls_task    TYPE trwbo_request_header,
*      ls_sel     TYPE trwbo_selection,
*      ls_new_req TYPE trwbo_new_req_props.
*
*CALL FUNCTION 'TR_PRESENT_REQUESTS_SEL_POPUP'
*  EXPORTING
*    iv_organizer_type    = 'D'
*    iv_username          = sy-uname
*    is_selection         = ls_sel
*    iv_title             = 'Select Transport Request'
*    is_new_request_props = ls_new_req
**   iv_prog_top_of_page  = ' '
**   iv_form_top_of_page  =
*  IMPORTING
*    es_selected_request  = ls_req
*    es_selected_task     = ls_task.
*
*IF sy-subrc = 0 AND ls_req IS NOT INITIAL.
*  cl_demo_output=>write( ls_req ).
*  cl_demo_output=>display( ls_task ).
*ENDIF.

*** Custom POPUP Screen
*
*DATA: lv_choise TYPE sy-tabix,
*      lv_heading type CHAR80.
*
*lv_heading = 'Select Transport Request'.
*
*SELECT e070~trkorr,
*       e07t~as4text
*   FROM e070
*  INNER JOIN e07t
*  ON e070~trkorr = e07t~trkorr
*  INTO TABLE @DATA(lt_req)
*  WHERE
*  TRFUNCTION IN ( 'W' ,
*   'X' ,
*   'K' )
* AND AS4USER = @sy-uname.
*
*CALL FUNCTION 'POPUP_TABLE_DISPLAY'
*  EXPORTING
*    endpos_col   = 80                 " Ending position of popup
*    endpos_row   = 20                 " Ending position of popup
*    startpos_col = 10                 " Starting position of popup
*    startpos_row = 10                 " Starting position of popup
*    titletext    = lv_heading                 " Text in title bar of popup
*  IMPORTING
*    choise       = lv_choise                " Number of table entry
*  TABLES
*    valuetab     = lt_req                 " Table with possible values
*  EXCEPTIONS
*    break_off    = 1
*    OTHERS       = 2.
*IF sy-subrc <> 0.
** MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
**   WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
*ENDIF.
*
*IF line_exists( lt_req[ lv_choise ] ).
*  WRITE: / lt_req[ lv_choise ].
*ENDIF.
**********************************************************************
** CDS Table function
**********************************************************************
*TYPES: BEGIN OF ty_s_customer,
*         id      TYPE scustom-id,
*         name    TYPE scustom-name,
*         city    TYPE scustom-city,
*         country TYPE scustom-country,
*       END OF ty_s_customer.
*
*TYPES ty_t_customers TYPE STANDARD TABLE OF ty_s_customer
* WITH NON-UNIQUE KEY id.
*
*
*DATA: lt_customers   TYPE ty_t_customers,
*      lt_feature_set TYPE cl_abap_dbfeatures=>features_set_t,
*      lo_salv        TYPE REF TO cl_salv_table,
*      lx_excp        TYPE REF TO cx_salv_error.
*
** Selection Screen
*PARAMETERS pa_nam TYPE s_custname DEFAULT 'ragnar' LOWER CASE.
*
*
*START-OF-SELECTION.
*
** Optional: Check if table functions can be used on this DB
*IF cl_abap_dbfeatures=>use_features(
* VALUE #( ( cl_abap_dbfeatures=>amdp_table_function ) )
* ) = abap_false.
* MESSAGE e030(S4D430).
*ENDIF.


* Data Retrieval
*************************************************************
*  SELECT
*  FROM zin_flight_table_function( name_in = @pa_nam )
*  FIELDS id,
*  name,
*  city,
*  country
*  ORDER BY country, city, name
* INTO TABLE @lt_customers.

* output
*************************************************************
*  TRY.
*      cl_salv_table=>factory(
*        IMPORTING
*          r_salv_table = lo_salv
*        CHANGING
*          t_table      = lt_customers
*      ).

* Display
*-------------------------*
*      lo_salv->display( ).
*    CATCH cx_salv_error INTO lx_excp. "
*      MESSAGE lx_excp TYPE 'I'.
*  ENDTRY.


**********************************************************************
** CDS View

*SELECT * FROM zin_flight_cds INTO TABLE @DATA(lt_flight).
*cl_demo_output=>display_data( lt_flight ).

**********************************************************************
** There is internal table with 5 to 10 sales order values.
** We need to display the first and last record of the internal table
** Without using loop statment, Sord statment.


*DATA: lt_vbeln TYPE TABLE OF vbak-vbeln.
*
*lt_vbeln = VALUE #(
*                    ( '10001' )
*                    ( '10003' )
*                    ( '10002' )
*                    ( '10004' )
*                     ).
*
*WRITE: lt_vbeln[ 1 ].
*
*WRITE: / lt_vbeln[ lines( lt_vbeln ) ].


**********************************************************************
** Dynamic Internal table creation
**********************************************************************
*tables: dd03l.
*
*PARAMETERS:
*  p_table TYPE tabname,
*  p_row   TYPE sy-dbcnt.
*
*SELECT-OPTIONS: s_field FOR dd03l-fieldname NO INTERVALS.
**********************************************************************
*                  Data Declaration
**********************************************************************

*DATA:
*  lo_dyn_data  TYPE REF TO data,
*  lv_field     TYPE char24,
*  lt_tab       TYPE TABLE OF char24,
*  lv_success   TYPE abap_bool,
*  lo_structure TYPE REF TO cl_abap_structdescr,
*  lo_tabletype TYPE REF TO cl_abap_tabledescr,
*  lt_comp      TYPE cl_abap_structdescr=>component_table,
*  ls_comp      LIKE LINE OF lt_comp.
*
*FIELD-SYMBOLS:
*  <lt_tab>   TYPE ANY TABLE,
*  <lt_table> TYPE ANY TABLE.
*
** Looping on select-options range and getting the low range to internal table.
*LOOP AT s_field.
*  SELECT tabname, fieldname FROM dd03l INTO TABLE @DATA(lt_field) WHERE tabname = @p_table AND fieldname = @s_field-low.
*  IF sy-subrc = 0.
*    lv_field = s_field-low.
*    APPEND lv_field TO lt_tab.
*
*    ls_comp-name = lv_field.
*    ls_comp-type ?= cl_abap_datadescr=>describe_by_name( lv_field ).
*    APPEND ls_comp TO lt_comp.
*
*    CLEAR lv_field.
*    CLEAR ls_comp.
*  ENDIF.
*ENDLOOP.
*
*FIELD-SYMBOLS: <fs_table> TYPE ANY TABLE.
*
*
*IF p_row < 0.
*  MESSAGE s015(zin_message_cls) WITH p_row DISPLAY LIKE 'E'.
*ELSE.
*
*    lo_structure = cl_abap_structdescr=>create( lt_comp ).
*
*    lo_tabletype = cl_abap_tabledescr=>create( p_line_type = lo_structure ).
*
*    CREATE DATA lo_dyn_data TYPE HANDLE lo_tabletype.
*    ASSIGN lo_dyn_data->* TO <fs_table>.
*
*    IF <fs_table> IS ASSIGNED.
*
*      SELECT (lt_tab) FROM (p_table) INTO CORRESPONDING FIELDS OF TABLE <fs_table> UP TO p_row ROWS.
*
*      IF sy-subrc = 0.
*
*         cl_demo_output=>display_data(
*           EXPORTING
*             value   = <fs_table>
*             name    = |Table: { p_table }| ).
*
*      ENDIF.
*    ENDIF.
*ENDIF.

**********************************************************************
*PARAMETERS: p_tab   TYPE tabname,
*            p_field TYPE string,
*            p_row   TYPE int4.
*
*DATA: lt_where    TYPE TABLE OF edpline,
*      lt_field    TYPE TABLE OF edpline,
*      ls_where    TYPE edpline,
*      lr_itab     TYPE REF TO cl_abap_tabledescr,
*      lr_struc    TYPE REF TO cl_abap_structdescr,
*      lt_comp_tab TYPE cl_abap_structdescr=>component_table,
*      lr_tab      TYPE REF TO data,
*      lt_fields   TYPE TABLE OF string,
*      lv_field    TYPE string,
*      lv_field1   TYPE string.

*DO.
*  IF p_field IS NOT INITIAL.
*    SPLIT p_field AT ',' INTO lv_field lv_field1.
*    APPEND lv_field TO lt_fields.
*    p_field = lv_field1.
*  ELSE.
*    EXIT.
*  ENDIF.
*ENDDO.

*CREATE DATA lr_tab TYPE TABLE OF (p_tab).
*ASSIGN lr_tab->* TO FIELD-SYMBOL(<lt_tab>).
*
*SELECT (p_field) FROM (p_tab) INTO CORRESPONDING FIELDS OF TABLE @<lt_tab> UP TO @p_row ROWS.
*
*cl_demo_output=>display( <lt_tab> ).


**********************************************************************

*DATA: lv_netwr TYPE vbak-netwr,
*      lv_price TYPE char13.
*
**lv_price = '17.55'.
*lv_price = '15.000,00'.
*
*WRITE: lv_price.
*
*REPLACE ALL OCCURRENCES OF '.' IN lv_price WITH ' '.
*
*REPLACE ALL OCCURRENCES OF ',' IN lv_price WITH '.'.
*
*WRITE: / lv_price.
*lv_netwr = lv_price.
*
*WRITE: / lv_netwr.

**********************************************************************
*DATA: lv_date  TYPE char10,
*      lv_erdat TYPE vbak-erdat.
*
*lv_date = '23.03.2023'.
*
*WRITE: lv_date.
*
*REPLACE ALL OCCURRENCES OF '.' IN lv_date WITH ' '.
*CONCATENATE lv_date+4(4) lv_date+2(2) lv_date+0(2) INTO DATA(lv_con_date).
*
*lv_erdat = lv_con_date.
*
*WRITE: / lv_erdat.
*
*ULINE.
*
*lv_date = '20230323'.
*
*WRITE: / lv_date.
*
*lv_erdat = lv_date.
*
*WRITE: / lv_erdat.

**********************************************************************
** Excel IMP
**********************************************************************

*SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME.
*
*  PARAMETERS: p_file TYPE ibipparms-path.
*
*  SELECTION-SCREEN SKIP.
*
*  SELECTION-SCREEN BEGIN OF LINE.
*    PARAMETERS: r1 RADIOBUTTON GROUP g1 DEFAULT 'X'.
*    SELECTION-SCREEN COMMENT 5(30) TEXT-000 FOR FIELD r1.
*
*    PARAMETERS: r2 RADIOBUTTON GROUP g1.
*    SELECTION-SCREEN COMMENT 5(30) TEXT-001 FOR FIELD r2.
*  SELECTION-SCREEN END OF LINE.
*SELECTION-SCREEN END OF BLOCK b1.

**********************************************************************
*** Send mail
**********************************************************************

* Create send Request
*DATA(lo_send_req) = cl_bcs=>create_persistent( ).
*
** Create Mail Body
*DATA(lt_body) = VALUE bcsy_text(
*                                  ( line = 'Dear Recipient, ' ) ( )
*                                  ( line = 'Material Document File.') ( )
*                                  ( line = 'Thank & Regards') ( )
*                                  ( line = 'Aditya Vijay Ingale')
*                                  ).
*
** Set up Document Object
*DATA(lo_document) = cl_document_bcs=>create_document(
*  i_type    = 'RAW'                " Code for Document Class
*  i_subject = 'Material Document'                " Short Description of Contents
*  i_text    = lt_body                " Content (Text-Like)
*).
*
*lo_document->add_attachment(
*  i_attachment_type     = 'xls'                 " Document Class for Attachment
*  i_attachment_subject  = 'Material Details'                 " Attachment Title
*).
*CATCH cx_document_bcs. " BCS: Document Exceptions


*DATA: input_field1 TYPE c LENGTH 10,
*      input_field2 TYPE c LENGTH 10,
*      line_num     TYPE i.
*
*START-OF-SELECTION.
*  WRITE 'Number 1:'.
*  SET BLANK LINES ON.
*  FORMAT INPUT.
*  WRITE / input_field1.
*  FORMAT INPUT OFF.
*
*  NEW-LINE.
*
*  WRITE 'Number 2:'.
*  SET BLANK LINES ON.
*  FORMAT INPUT.
*  WRITE / input_field2.
*  FORMAT INPUT OFF.
*
*  WRITE / '>>>SUM<<<' COLOR 5 HOTSPOT.
*
*  DATA(lv_sum) = INPUT_FIELd1 + input_field2.
*
*AT LINE-SELECTION.
*  IF sy-lisel = '>>>SUM<<<'.
**    line_num = sy-lilli - 1.
**    READ LINE line_num FIELD VALUE lv_sum.
*    WRITE: 'The sum is:', lv_sum.
*  ENDIF.

*START-OF-SELECTION.                  " Creation of Hotspot
*  FORMAT HOTSPOT ON.
*  WRITE 'Save' COLOR 5.
*  FORMAT HOTSPOT OFF.
*
*AT LINE-SELECTION.
*  WRITE 'Data Saved' COLOR 3.


*  DATA: id(2)    TYPE n,
*        name(10) TYPE c,
*        age(2)   TYPE c,
*        color_1  TYPE c LENGTH 20.
*
*  color_1 =   'Gray-Blue COLOR_1'.
*
*  WRITE: 'INTENSIFIED ON' COLOR 1 INTENSIFIED ON.
*
*  NEW-LINE.
*  ULINE.
*
*  WRITE: 'INTENSIFIED OFF' COLOR 1 INTENSIFIED OFF.
*
*  NEW-LINE.
*  ULINE.
*
*  WRITE: 'INVERSE ON' COLOR 1 INVERSE ON .
*
*  NEW-LINE.
*  ULINE.
*
*  WRITE: 'INVERSE OFF' COLOR 1 INVERSE OFF .
*
***********************************************************************
*  NEW-LINE.
*  ULINE.
*
*  WRITE: 'INTENSIFIED ON' COLOR 2 INTENSIFIED ON.
*
*  NEW-LINE.
*  ULINE.
*
*  WRITE: 'INTENSIFIED OFF' COLOR 2 INTENSIFIED OFF.
*
*  NEW-LINE.
*  ULINE.
*
*  WRITE: 'INVERSE ON' COLOR 2 INVERSE ON .
*
*  NEW-LINE.
*  ULINE.
*
*  WRITE: 'INVERSE OFF' COLOR 2 INVERSE OFF .

**********************************************************************



*id = 03.
*name = 'Aditya'.
*age = 23.
*
*WRITE id.
*
*NEW-LINE.
*
*WRITE:1 id,
*      20 name,
*      40 age.
