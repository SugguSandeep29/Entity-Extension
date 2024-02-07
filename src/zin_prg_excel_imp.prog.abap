*&---------------------------------------------------------------------*
*& Report ZIN_PRG_EXCEL_IMP
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zin_prg_excel_imp.

* Selection Screen
SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME.

  PARAMETERS: p_file TYPE ibipparms-path.

  SELECTION-SCREEN SKIP.

  SELECTION-SCREEN BEGIN OF LINE.
    SELECTION-SCREEN COMMENT 5(20) TEXT-000 FOR FIELD r_sales.
    SELECTION-SCREEN POSITION 40.
    PARAMETERS: r_sales RADIOBUTTON GROUP g1 DEFAULT 'X'.

    SELECTION-SCREEN COMMENT 50(20) TEXT-001 FOR FIELD r_purch.
    SELECTION-SCREEN POSITION 70.
    PARAMETERS: r_purch RADIOBUTTON GROUP g1.
  SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN END OF BLOCK b1.


* Defining Structure

TYPES : BEGIN OF ty_vbak,
          vbeln TYPE vbak-vbeln,
          erdat TYPE vbak-erdat,
          ernam TYPE vbak-ernam,
          netwr TYPE vbak-netwr,
          waerk TYPE vbak-waerk,
        END OF ty_vbak,


        BEGIN OF ty_ekko,
          ebeln TYPE ekko-ebeln,
          bukrs TYPE ekko-bukrs,
          aedat TYPE ekko-aedat,
          ernam TYPE ekko-ernam,
          lifnr TYPE ekko-lifnr,
        END OF ty_ekko.

* Data Defination

DATA : lt_vbak        TYPE TABLE OF ty_vbak,
       lt_ekko        TYPE TABLE OF ty_ekko,
       ls_vbak        TYPE ty_vbak,
       ls_ekko        TYPE ty_ekko,
       lv_filename    TYPE string,
       lv_filelength  TYPE i,
       lv_header      TYPE xstring,
       lt_data        TYPE solix_tab,
       lo_spreadsheet TYPE REF TO cl_fdt_xl_spreadsheet.


* Getting search help on parameter p_file
AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_file.

  CALL FUNCTION 'F4_FILENAME'
    EXPORTING
      program_name  = syst-cprog       " Module pool program name for screen field
      dynpro_number = syst-dynnr       " Dynpro number where F4 help is needed
      field_name    = space            " name of field where path is to be entered
    IMPORTING
      file_name     = p_file.                 " Path name selected by user with help of Filemngr


  lv_filename = p_file.

START-OF-SELECTION.

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

*  Convert file from binay to xstring

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
  lo_spreadsheet = NEW cl_fdt_xl_spreadsheet( document_name = lv_filename
                                              xdocument     = lv_header ).

* Get list of worksheets
  lo_spreadsheet->if_fdt_doc_spreadsheet~get_worksheet_names(
    IMPORTING
      worksheet_names = DATA(lt_worksheet)
  ).

  IF r_sales = abap_true.

    DATA(lv_vbak) = lt_worksheet[ 1 ].
    DATA(lo_tab_vbak) = lo_spreadsheet->if_fdt_doc_spreadsheet~get_itab_from_worksheet( lv_vbak ).
    ASSIGN lo_tab_vbak->* TO FIELD-SYMBOL(<lt_vbak>).

    LOOP AT <lt_vbak> ASSIGNING FIELD-SYMBOL(<ls_vbak>).
      DO 5 TIMES.
        ASSIGN COMPONENT sy-index OF STRUCTURE <ls_vbak> TO FIELD-SYMBOL(<lv_vbak>).

        CASE sy-index.
          WHEN 1.
            ls_vbak-vbeln = <lv_vbak>.
          WHEN 2.
            REPLACE ALL OCCURRENCES OF '.' IN <lv_vbak> WITH ' '.
            CONCATENATE <lv_vbak>+4(4) <lv_vbak>+2(2) <lv_vbak>+0(2) INTO DATA(lv_con_date).
            ls_vbak-erdat = lv_con_date.
          WHEN 3.
            ls_vbak-ernam = <lv_vbak>.
          WHEN 4.
            REPLACE ALL OCCURRENCES OF '.' IN <lv_vbak> WITH ' '.
            REPLACE ALL OCCURRENCES OF ',' IN <lv_vbak> WITH '.'.
            CONDENSE <lv_vbak>.
            ls_vbak-netwr = <lv_vbak>.
          WHEN 5.
            ls_vbak-waerk = <lv_vbak>.
        ENDCASE.

      ENDDO.
      APPEND ls_vbak TO lt_vbak.
      CLEAR ls_vbak.
    ENDLOOP.

*   Dispay ALV
    TRY.
        cl_salv_table=>factory(
          IMPORTING
            r_salv_table = DATA(lo_vbak)                          " Basis Class Simple ALV Tables
          CHANGING
            t_table      = lt_vbak
        ).
      CATCH cx_salv_msg. " ALV: General Error Class with Message
    ENDTRY.

    lo_vbak->display( ).

  ELSEIF r_purch = abap_true.

    DATA(lv_ekko) = lt_worksheet[ 2 ].
    DATA(lo_tab_ekko) = lo_spreadsheet->if_fdt_doc_spreadsheet~get_itab_from_worksheet( lv_ekko ).
    ASSIGN lo_tab_ekko->* TO FIELD-SYMBOL(<lt_ekko>).

    LOOP AT <lt_ekko> ASSIGNING FIELD-SYMBOL(<ls_ekko>).
      DO 5 TIMES.
        ASSIGN COMPONENT sy-index OF STRUCTURE <ls_ekko> TO FIELD-SYMBOL(<lv_ekko>).

        CASE sy-index.
          WHEN 1.
            ls_ekko-ebeln = <lv_ekko>.
          WHEN 2.
            ls_ekko-bukrs = <lv_ekko>.
          WHEN 3.
            REPLACE ALL OCCURRENCES OF '.' IN <lv_ekko> WITH ' '.
            CONCATENATE <lv_ekko>+4(4) <lv_ekko>+2(2) <lv_ekko>+0(2) INTO lv_con_date.
            ls_ekko-aedat = lv_con_date.
          WHEN 4.
            ls_ekko-ernam = <lv_ekko>.
          WHEN 5.
            ls_ekko-lifnr = <lv_ekko>.
        ENDCASE.

      ENDDO.
      APPEND ls_ekko TO lt_ekko.
      CLEAR ls_ekko.
    ENDLOOP.

*   Dispay ALV
    TRY.
        cl_salv_table=>factory(
          IMPORTING
            r_salv_table = DATA(lo_ekko)                          " Basis Class Simple ALV Tables
          CHANGING
            t_table      = lt_ekko
        ).
      CATCH cx_salv_msg. " ALV: General Error Class with Message
    ENDTRY.

    lo_ekko->display( ).

  ENDIF.
