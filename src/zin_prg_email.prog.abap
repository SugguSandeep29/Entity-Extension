*&---------------------------------------------------------------------*
*& Report ZIN_PRG_EMAIL
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zin_prg_email.

SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-000.
  PARAMETERS: p_email TYPE bcs_address,
              p_sub   TYPE bcs_subject.
SELECTION-SCREEN END OF BLOCK b1.

SELECTION-SCREEN BEGIN OF BLOCK b2 WITH FRAME TITLE TEXT-001.
  PARAMETERS:
    p_table  TYPE tabname,
    p_fields TYPE string,
    p_row    TYPE sy-dbcnt.
SELECTION-SCREEN END OF BLOCK b2.

DATA :
  lv_filename    TYPE bcs_filename,
  lv_filelength  TYPE i,
  lv_header      TYPE xstring,
  lt_data        TYPE solix_tab,
  lo_spreadsheet TYPE REF TO cl_fdt_xl_spreadsheet,
  lv_xstring     TYPE xstring.

DATA:
  lo_dyn_data  TYPE REF TO data,
  lt_fields    TYPE TABLE OF char24,
  lo_structure TYPE REF TO cl_abap_structdescr,
  lo_tabletype TYPE REF TO cl_abap_tabledescr,
  lt_comp      TYPE cl_abap_structdescr=>component_table,
  ls_comp      LIKE LINE OF lt_comp,
  lv_field1    TYPE fieldname,
  lv_field2    TYPE fieldname.

FIELD-SYMBOLS:
  <lt_table> TYPE ANY TABLE.

* Validating the mail

DATA(lo_email_check) = NEW cl_abap_regex( pattern     = '\w+(\.\w+)*@(\w+\.)+(\w{2,4})'
                                          ignore_case = abap_true ).


DATA(lo_matcher) = lo_email_check->create_matcher( text = p_email ).

IF lo_matcher->match( ) IS INITIAL.
  MESSAGE i000(zin_cl_msg).
ELSE.

  DO.
*    Creating selection fields from table
    IF p_fields IS NOT INITIAL.
      SPLIT p_fields AT ',' INTO lv_field1 lv_field2.
      APPEND lv_field1 TO lt_fields.
      p_fields = lv_field2.

      ls_comp-name = lv_field1.
      ls_comp-type ?= cl_abap_datadescr=>describe_by_name( lv_field1 ).
      APPEND ls_comp TO lt_comp.

      CLEAR lv_field1.
      CLEAR ls_comp.

    ELSE.
      EXIT.
    ENDIF.
  ENDDO.

*  Creating internal table dynamically
  lo_structure = cl_abap_structdescr=>create( lt_comp ).

  lo_tabletype = cl_abap_tabledescr=>create( p_line_type = lo_structure ).

  CREATE DATA lo_dyn_data TYPE HANDLE lo_tabletype.
  ASSIGN lo_dyn_data->* TO <lt_table>.

*  Dynamic Select Query
  SELECT (lt_fields)
    FROM (p_table)
    INTO CORRESPONDING FIELDS OF TABLE <lt_table>
    UP TO p_row ROWS.

* Calling factory method
  TRY.
      cl_salv_table=>factory(
        IMPORTING
          r_salv_table = DATA(lo_table)                          " Basis Class Simple ALV Tables
        CHANGING
          t_table      = <lt_table>
      ).
    CATCH cx_salv_msg.                                            " ALV: General Error Class with Message
  ENDTRY.

* Converting table to binary
  lv_xstring = lo_table->to_xml( if_salv_bs_xml=>c_type_xlsx ).

* Getting instance of cl_bcs
  DATA(lo_bcs_msg) = NEW cl_bcs_message( ).

* Body of the mail
  CONCATENATE: 'Hi team'
               'Please find the attachment'



               'Thanks & Regards'
                sy-uname
                INTO DATA(lv_body) SEPARATED BY CL_ABAP_CHAR_UTILITIES=>NEWLINE.

* Setting subject
  lo_bcs_msg->set_subject( p_sub ) .

*  Setting Body
  lo_bcs_msg->set_main_doc( iv_contents_txt = lv_body ) .

*  Setting Recipient
  lo_bcs_msg->add_recipient( p_email ).

*  Adding attachments
  lo_bcs_msg->add_attachment(
    iv_doctype      = 'XLS'                 " Document Type
    iv_filename     = |{ p_sub }.xlsx|    " File Name (with Extension)
    iv_contents_bin = lv_xstring            " Binary Document Content
  ).

*  Sending mail
  TRY.
      lo_bcs_msg->send( ).
      IF sy-subrc = 0.
        MESSAGE i001(zin_cl_msg).
      ENDIF.
    CATCH cx_bcs_send.
  ENDTRY.
ENDIF.
