*&---------------------------------------------------------------------*
*& Report ZIN_PRG_INCOTERM_FROM_EXCEL
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zin_prg_incoterm_from_excel.


TYPES: BEGIN OF ty_inco,
         lifnr TYPE lifnr,
         inco  TYPE inco2_l,
       END OF ty_inco.

DATA: lt_inco       TYPE TABLE OF ty_inco,
      ls_inco       TYPE ty_inco,
      lv_filelength TYPE i,
      lv_header     TYPE xstring,
      lt_data       TYPE solix_tab,
      lv_filename   TYPE string.

lv_filename = 'C:\Users\aditya.ingale\Downloads\Incoterm location.xlsx'.
* Convert file to Binary
CALL FUNCTION 'GUI_UPLOAD'
  EXPORTING
    filename   = lv_filename              " Name of file
    filetype   = 'BIN'                    " File Type (ASC or BIN)
  IMPORTING
    filelength = lv_filelength                " File Length
    header     = lv_header                 " File Header in Case of Binary Upload
  TABLES
    data_tab   = lt_data.                 " Transfer table for file contents


CALL FUNCTION 'SCMS_BINARY_TO_XSTRING'
  EXPORTING
    input_length = lv_filelength
  IMPORTING
    buffer       = lv_header
  TABLES
    binary_tab   = lt_data
  EXCEPTIONS
    failed       = 1
    OTHERS       = 2.


* Get Instance of cl_fdt_xl_spreadsheet class.
DATA(lo_spreadsheet) = NEW cl_fdt_xl_spreadsheet( document_name = lv_filename
                                            xdocument     = lv_header ).

* Get list of worksheets
lo_spreadsheet->if_fdt_doc_spreadsheet~get_worksheet_names(
  IMPORTING
    worksheet_names = DATA(lt_worksheet)
).

DATA(lv_inco) = lt_worksheet[ 1 ].
DATA(lo_tab_inco) = lo_spreadsheet->if_fdt_doc_spreadsheet~get_itab_from_worksheet( lv_inco ).
ASSIGN lo_tab_inco->* TO FIELD-SYMBOL(<lt_inco>).

LOOP AT <lt_inco> ASSIGNING FIELD-SYMBOL(<ls_inco>).
  DO 2 TIMES.
    ASSIGN COMPONENT sy-index OF STRUCTURE <ls_inco> TO FIELD-SYMBOL(<lv_ind>).

    CASE sy-index.
      WHEN 1.
        ls_inco-lifnr = <lv_ind>.
      WHEN 2.
        ls_inco-inco = <lv_ind>.
    ENDCASE.
  ENDDO.
  IF strlen( ls_inco-inco ) > 28.
    APPEND ls_inco TO lt_inco.
    CLEAR ls_inco.
  ENDIF.

ENDLOOP.

cl_demo_output=>display( lt_inco ).
WRITE: lines( lt_inco ).
