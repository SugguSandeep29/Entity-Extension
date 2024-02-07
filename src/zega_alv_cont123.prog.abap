***&---------------------------------------------------------------------*
**& Report ZANVIT_ALV_CONT
***&---------------------------------------------------------------------*
***&
***&---------------------------------------------------------------------*
REPORT zega_alv_cont123.
TYPE-POOLS : slis.
TABLES : vbak.


**
******************Selection Screen*****************
SELECT-OPTIONS : s_kunnr FOR vbak-kunnr.

************Internal table & Workarea*************8
DATA : it_data      TYPE TABLE OF   zcontainer.
DATA : lS_DATA TYPE  zcontainer.
DATA : it_mara TYPE TABLE OF zcontainer2,
       wa_mara TYPE zcontainer2,
       i_fcat  TYPE  slis_t_fieldcat_alv,
       wa_fcat LIKE LINE OF  i_fcat.




DATA: g_alv_tree         TYPE REF TO cl_gui_alv_tree,
      g_custom_container TYPE REF TO cl_gui_custom_container,
      custom_container2  TYPE REF TO cl_gui_custom_container,
      ls_header          TYPE treev_hhdr.

DATA : ok_code LIKE sy-ucomm,
       save_ok LIKE sy-ucomm,           "OK-Code
       g_max   TYPE i VALUE 255.

END-OF-SELECTION.
CLASS lcl_tree_event_receiver DEFINITION.

  PUBLIC SECTION.
*§2. Define an event handler method for each event you want to react to.
    METHODS handle_item_double_click
      FOR EVENT item_double_click OF cl_gui_alv_tree
      IMPORTING node_key  sender.

ENDCLASS.

CLASS lcl_tree_event_receiver IMPLEMENTATION.
*§3. Implement your event handler methods.

  METHOD handle_ITEM_double_click.
    DATA: lt_children TYPE lvc_t_nkey.
*    DATA : it_mara TYPE TABLE OF ty_mara,
*           wa_mara TYPE ty_mara.
*if node_key = 2.


SET PARAMETER ID 'AUN' FIELD LS_DATA-VBELN..
  call TRANSACTION 'VA03' AND SKIP FIRST SCREEN.



*    CALL METHOD sender->get_children
*      EXPORTING
*        i_node_key  = node_key
*      IMPORTING
*        et_children = lt_children.

*    else.
*      IF NODE_KEY = 3.
*
*        MESSAGE 'DONE' TYPE 'I'.
*      INCLUDE ZCONTAINER2.
*    PERFORM item_data.

*ENDIF.
*ENDIF.
  ENDMETHOD.

ENDCLASS.





START-OF-SELECTION.
  CALL SCREEN 200.


*&---------------------------------------------------------------------*
*& Module STATUS_0200 OUTPUT
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
MODULE status_0200 OUTPUT.
  SET PF-STATUS 'ZDEMO_A'.

  IF g_alv_tree IS INITIAL.
    PERFORM init_tree.

    CALL METHOD cl_gui_cfw=>flush
      EXCEPTIONS
        cntl_system_error = 1
        cntl_error        = 2.
    IF sy-subrc NE 0.
      CALL FUNCTION 'POPUP_TO_INFORM'
        EXPORTING
          titel = 'Automation Queue failure'(001)
          txt1  = 'Internal error:'(002)
          txt2  = 'A method in the automation queue'
          txt3  = 'caused a failure.'.
    ENDIF.
  ENDIF.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0200  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0200 INPUT.
  save_ok = ok_code.
  CLEAR ok_code.
  CASE  save_ok.
    WHEN 'BACK' OR 'EXIT' OR 'CANC'.

      PERFORM exit_program.
    WHEN OTHERS.

      CALL METHOD cl_gui_cfw=>dispatch.

  ENDCASE.
  CALL METHOD cl_gui_cfw=>flush.
ENDMODULE.

"**************************************************************************************************************************



***********************************************************************************************************************************8











*&---------------------------------------------------------------------*
*& Form init_tree
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM init_tree .
  DATA: l_tree_container_name(30) TYPE c.
  l_tree_container_name = 'CONTAINER'.
  IF cl_gui_alv_grid=>offline( ) IS INITIAL.
    CREATE OBJECT g_custom_container
      EXPORTING
        container_name              = l_tree_container_name
      EXCEPTIONS
        cntl_error                  = 1
        cntl_system_error           = 2
        create_error                = 3
        lifetime_error              = 4
        lifetime_dynpro_dynpro_link = 5.
    IF sy-subrc <> 0.
      MESSAGE 'Invalid' TYPE 'E'.
    ENDIF.
  ENDIF.



  CREATE OBJECT g_alv_tree
    EXPORTING
      parent                      = g_custom_container
      node_selection_mode         = cl_gui_column_tree=>node_sel_mode_single
      item_selection              = 'X'
      no_html_header              = 'X'
      no_toolbar                  = ''
    EXCEPTIONS
      cntl_error                  = 1
      cntl_system_error           = 2
      create_error                = 3
      lifetime_error              = 4
      illegal_node_selection_mode = 5
      failed                      = 6
      illegal_column_name         = 7.
  IF sy-subrc <> 0.
    MESSAGE x208(00) WITH 'ERROR'.
  ENDIF.


  DATA I_hierarchy_header TYPE treev_hhdr.
  PERFORM build_hierarchy_header CHANGING I_hierarchy_header.
  CALL METHOD g_alv_tree->set_table_for_first_display
    EXPORTING
      i_structure_name    = 'ZCONTAINER'
      is_hierarchy_header = I_hierarchy_header
    CHANGING
      it_outtab           = it_data. "table must be empty !

* §4. Create hierarchy (nodes and leaves)
  PERFORM create_hierarchy1.

  PERFORM register_events.

* §5. Send data to frontend.


  IF cl_gui_alv_grid=>offline( ) IS INITIAL.
    CALL METHOD g_alv_tree->frontend_update.
  ENDIF.
ENDFORM.




*&---------------------------------------------------------------------*
*& Form build_hierarchy_header
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      <-- L_HIERARCHY_HEADER
*&---------------------------------------------------------------------*
FORM build_hierarchy_header  CHANGING p_hierarchy_header TYPE treev_hhdr..
  p_hierarchy_header-heading = 'Customer/Document/Item     '.
  p_hierarchy_header-tooltip = 'Sales Order Details'(400).
  p_hierarchy_header-width = 60.
  p_hierarchy_header-width_pix = ' '.


ENDFORM.



*&---------------------------------------------------------------------*
*& Form EXIT_PROGRAM
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM exit_program .
  CALL METHOD g_custom_container->free.
  LEAVE PROGRAM.

ENDFORM.

.

*&---------------------------------------------------------------------*
*& Form create_hierarchy1
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM create_hierarchy1 .

  DATA : lS_DATA TYPE  zcontainer,
         it_data TYPE TABLE OF  zcontainer. " OCCURS 0.

  SELECT
           a~vbeln
           a~kunnr
           a~erdat
           a~ernam
           b~posnr
           b~matnr
           b~netwr
          b~waerk
      FROM vbak AS a INNER JOIN
           vbap AS b ON a~vbeln = b~vbeln
     INTO  CORRESPONDING FIELDS OF TABLE  it_data
  WHERE a~kunnr IN s_kunnr.


  DATA : lvc_node   TYPE lvc_nkey,
         lvc_node1  TYPE lvc_nkey,
         lvc_node12 TYPE lvc_nkey,
         lvc_text   TYPE lvc_value,
         lvc_text1  TYPE lvc_value,
         lvc_text2  TYPE lvc_value.

  DATA : i_start_column TYPE lvc_fname,
         i_end_column   TYPE lvc_fname.

  SORT it_data BY kunnr vbeln.
  LOOP AT it_data INTO ls_data.

    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
      EXPORTING
        input  = ls_data-kunnr
      IMPORTING
        output = ls_data-kunnr.



*******************First node*************************

    ON CHANGE OF ls_data-kunnr.
      CONCATENATE 'CUSTOMER' ls_data-kunnr INTO lvc_text SEPARATED BY space.


      CALL METHOD g_alv_tree->add_node
        EXPORTING
          i_relat_node_key = ' '
          i_relationship   = cl_gui_column_tree=>relat_First_CHILD
          i_node_text      = lvc_text
        IMPORTING
          e_new_node_key   = lvc_node.


      IF sy-subrc <> 0.
*     Implement suitable error handling here
      ENDIF.
    ENDON.

    CONCATENATE 'Sales Document' ls_data-vbeln INTO lvc_text1 SEPARATED BY space.

    ON CHANGE OF ls_data-vbeln.

      CALL METHOD g_alv_tree->add_node
        EXPORTING
          i_relat_node_key = lvc_node
          i_relationship   = cl_gui_column_tree=>relat_last_CHILD
*         is_outtab_line   = ls_data-vbeln
          i_node_text      = lvc_text1
        IMPORTING
          e_new_node_key   = lvc_node1.
    ENDON.




    lvC_text2 = ls_data-matnr.
    ON CHANGE OF ls_data-vbeln.

      CALL METHOD g_alv_tree->add_node
        EXPORTING
          i_relat_node_key = lvc_node1
          i_relationship   = cl_gui_column_tree=>relat_last_CHILD
          is_outtab_line   = ls_data
          i_node_text      = lvc_text2
        IMPORTING
          e_new_node_key   = lvc_node12.
    ENDON.
*    ENDON.
    CLEAR ls_data-kunnr.
  ENDLOOP.


********************Column Optimisation***********************
  i_start_column = ls_data-kunnr.
  i_end_column   = ls_data-netwr.
  CALL METHOD g_alv_tree->column_optimize
    EXPORTING
      i_start_column    = i_start_column
      i_end_column      = i_start_column
      i_include_heading = 'X'
*  EXCEPTIONS
*     start_column_not_found = 1
*     end_column_not_found   = 2
*     others            = 3
    .
  IF sy-subrc <> 0.
* Implement suitable error handling here
  ENDIF.



ENDFORM.
*&---------------------------------------------------------------------*
*& Form register_events
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM register_events .
  DATA: lt_events        TYPE cntl_simple_events,
        l_event          TYPE cntl_simple_event,
        l_event_receiver TYPE REF TO lcl_tree_event_receiver.


  CALL METHOD g_alv_tree->get_registered_events
    IMPORTING
      events = lt_events.

  l_event-eventid = cl_gui_column_tree=>eventid_item_double_click.
  APPEND l_event TO lt_events.

  CALL METHOD g_alv_tree->set_registered_events
    EXPORTING
      events                    = lt_events
    EXCEPTIONS
      cntl_error                = 1
      cntl_system_error         = 2
      illegal_event_combination = 3.
  IF sy-subrc <> 0.
    MESSAGE x208(00) WITH 'ERROR'.                          "#EC NOTEXT
  ENDIF.
*--------------------
*§4d. Register events on backend (ABAP Objects event handling)
  CREATE OBJECT l_event_receiver.
  SET HANDLER l_event_receiver->handle_ITEM_double_click FOR g_alv_tree.

ENDFORM.

*ENDMODULE.
*&---------------------------------------------------------------------*
*& Form ITEM_DATA
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*& Module STATUS_0300 OUTPUT
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
MODULE status_0300 OUTPUT.
  IF custom_container2 IS INITIAL.
    DATA: l_tree_container_name(30) TYPE c.
    l_tree_container_name = 'CONTAINER2'.
    IF cl_gui_alv_grid=>offline( ) IS INITIAL.
      CREATE OBJECT g_custom_container
        EXPORTING
          container_name              = l_tree_container_name
        EXCEPTIONS
          cntl_error                  = 1
          cntl_system_error           = 2
          create_error                = 3
          lifetime_error              = 4
          lifetime_dynpro_dynpro_link = 5.
      IF sy-subrc <> 0.
        MESSAGE 'Invalid' TYPE 'E'.
      ENDIF.
    ENDIF.
  ENDIF.



ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0300  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0300 INPUT.
  SET PF-STATUS 'Z101_ST'.
  save_ok = ok_code.
  CLEAR ok_code.
  CASE  save_ok.
    WHEN 'BO'.
      LEAVE TO SCREEN 0.
  ENDCASE.
ENDMODULE.
