*&---------------------------------------------------------------------*
*& Report ZIN_PRG_OOALV_CONTAINER
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zin_prg_ooalv_container.

SELECT * FROM vbak INTO TABLE @DATA(lt_vbak) UP TO 10 ROWS.



*DATA: lo_cust TYPE REF TO cl_gui_custom_container,
*      lo_alv  TYPE REF TO cl_gui_alv_grid,
*      lt_vbak TYPE TABLE OF vbak,
*      ls_vbak TYPE vbak,
*      ok_code TYPE sy-ucomm.
*
*
*SELECT-OPTIONS: s_vbeln FOR ls_vbak-vbeln.
*
*SELECT * FROM vbak INTO TABLE lt_vbak UP TO 30 ROWS WHERE vbeln IN s_vbeln.
*
*CALL SCREEN 100.
**&---------------------------------------------------------------------*
**& Module STATUS_0100 OUTPUT
**&---------------------------------------------------------------------*
**&
**&---------------------------------------------------------------------*
*MODULE status_0100 OUTPUT.
*  SET PF-STATUS 'ZIN_PF'.
*  SET TITLEBAR 'TITLE'.
*ENDMODULE.
**&---------------------------------------------------------------------*
**&      Module  USER_COMMAND_0100  INPUT
**&---------------------------------------------------------------------*
**       text
**----------------------------------------------------------------------*
*MODULE user_command_0100 INPUT.
*  CASE ok_code.
*    WHEN 'EXIT' OR 'BACK'.
*      MESSAGE 'Leaving Program' TYPE 'I'.
*      LEAVE PROGRAM.
*  ENDCASE.
*ENDMODULE.
**&---------------------------------------------------------------------*
**& Module DISPLAY_ALV OUTPUT
**&---------------------------------------------------------------------*
**&
**&---------------------------------------------------------------------*
*MODULE display_alv OUTPUT.
*
*  CREATE OBJECT lo_cust
*    EXPORTING
*      container_name = 'CUSTOM'.              " Name of the Screen CustCtrl Name to Link Container To
*
*  CREATE OBJECT lo_alv
*    EXPORTING
*      i_parent = lo_cust.
*
*  CALL METHOD lo_alv->set_table_for_first_display
*    EXPORTING
*      i_structure_name              = 'VBAK'
*    CHANGING
*      it_outtab                     = lt_vbak.                 " Output Table
*
*ENDMODULE.
