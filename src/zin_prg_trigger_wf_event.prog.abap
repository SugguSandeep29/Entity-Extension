*&---------------------------------------------------------------------*
*& Report ZIN_PRG_TRIGGER_WF_EVENT
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zin_prg_trigger_wf_event.

DATA: lv_retcode TYPE sy-subrc.

CALL FUNCTION 'SAP_WAPI_CREATE_EVENT'
  EXPORTING
    object_type    = 'ZIN_BO_1'                 " Business Object Repository object type
    object_key     = '0000000001'                 " Business Object Repository object key
    event          = 'WF_START'                 " Event
    commit_work    = 'X'              " Do not trigger "Commit Work" at end
    event_language = sy-langu         " Event language
    language       = sy-langu         " SAP System, Current Language
    user           = sy-uname         " R/3 system (logon name of user)
*   ifs_xml_container =                  " Event Container as XML Data Stream
  IMPORTING
    return_code    = lv_retcode                " Return code (0 - 3, 999)
*   event_id       =                  " Event ID
*  TABLES
*   input_container   =                  " Input container (name-value pairs)
*   message_lines  =                  " Message Lines
*   message_struct =                  " Message Structure
  .


IF lv_retcode IS INITIAL.
  WRITE: 'WF Triggered Successfully'.
ELSE.
  WRITE: 'WF Trigger Failed'.
ENDIF.
