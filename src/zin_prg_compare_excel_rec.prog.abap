*&---------------------------------------------------------------------*
*& Report ZIN_PRG_BDC_RAJNI
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zin_prg_compare_excel_rec.

TYPES : BEGIN OF record,
* data element: DZBUKR
          zbukr_001(004),
* data element: HBKID
          hbkid_002(005),
* data element: HKTID
          hktid_003(005),
* data element: CHECT
          chect_004(013),
* data element: BANCD
          bancd_005(010),
*        msg type string,
        END OF record.
*** End generated data section ***

TYPES : BEGIN OF ty_msg1,
          check_no   TYPE chect,
          status(10) TYPE c,
          icon       TYPE c,
          zbukr      TYPE dzbukr,
          vblnr      TYPE vblnr,
          gjahr      TYPE gjahr,
        END OF ty_msg1.

DATA : it_record1 TYPE solix_tab,
       it_record  TYPE TABLE OF record,
       wa_record  TYPE record,
       it_tab     TYPE truxs_t_text_data,
       it_bdc     TYPE TABLE OF bdcdata,
       wa_bdc     TYPE bdcdata,
       it_fcat    TYPE slis_t_fieldcat_alv,
       wa_fcat    TYPE  slis_fieldcat_alv,
       wa_layout  TYPE slis_layout_alv,

       it_msg1    TYPE TABLE OF ty_msg1,
       wa_msg1    TYPE ty_msg1,

       bdcdata    LIKE bdcdata    OCCURS 0 WITH HEADER LINE,
       messtab    LIKE bdcmsgcoll OCCURS 0 WITH HEADER LINE,

       it_msg     TYPE TABLE OF bdcmsgcoll,
       wa_msg     TYPE bdcmsgcoll.
DATA: gv_header TYPE xstring.
PARAMETERS: dataset TYPE ibipparms-path.
FIELD-SYMBOLS : <gt_data> TYPE ANY TABLE,
                <lv_data> TYPE any.

DATA : lv_filename TYPE string,
       lv_length   TYPE i,
       lv_header   TYPE xstring,
       lo_record   TYPE REF TO data.
DATA : lo_excel_ref TYPE REF TO cl_fdt_xl_spreadsheet.
DATA : gv_file  TYPE string.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR dataset.
  CALL FUNCTION 'F4_FILENAME'
    EXPORTING
      program_name  = syst-cprog
      dynpro_number = syst-dynnr
*     FIELD_NAME    = ' '
    IMPORTING
      file_name     = dataset.

START-OF-SELECTION.

      CALL FUNCTION 'TEXT_CONVERT_XLS_TO_SAP'
  EXPORTING
*   I_FIELD_SEPERATOR          =
*   I_LINE_HEADER              =
    i_tab_raw_data             = it_tab
    i_filename                 = dataset
  TABLES
    i_tab_converted_data       = it_record[]
 EXCEPTIONS
   conversion_failed          = 1
   OTHERS                     = 2
          .
IF sy-subrc <> 0.
* Implement suitable error handling here
ENDIF.


  CALL FUNCTION 'SCMS_BINARY_TO_XSTRING'
    EXPORTING
      input_length = lv_length
    IMPORTING
      buffer       = lv_header
    TABLES
      binary_tab   = it_record1
    EXCEPTIONS
      failed       = 1
      OTHERS       = 2.

  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
      WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ELSE.
    gv_header = lv_header.   "add SD1K908280
  ENDIF.

  TRY .
      lo_excel_ref = NEW cl_fdt_xl_spreadsheet(
        document_name = lv_filename
        xdocument     = lv_header ).
    CATCH cx_fdt_excel_core.
      "Implement suitable error handling here    ##NO_HANDLER
  ENDTRY .

*& --- Get List of Worksheets
  lo_excel_ref->if_fdt_doc_spreadsheet~get_worksheet_names(
    IMPORTING
      worksheet_names = DATA(lt_worksheets) ).

  IF NOT lt_worksheets IS INITIAL.
    READ TABLE lt_worksheets INTO DATA(lv_woksheetname) INDEX 1.

    IF sy-subrc EQ 0.
      DATA(lo_data_ref) = lo_excel_ref->if_fdt_doc_spreadsheet~get_itab_from_worksheet(
        lv_woksheetname ).
*& --- now you have excel work sheet data in dyanmic internal table
      ASSIGN lo_data_ref->* TO <gt_data>.
    ENDIF.

  ENDIF.
