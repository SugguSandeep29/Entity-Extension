*&---------------------------------------------------------------------*
*& Report ZIN_PRG_CREATE_CHANGE_REQ
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zin_prg_create_change_req.

PARAMETERS: p_crtype TYPE usmd_crequest_type,
            p_model  TYPE usmd_model,
            p_bpgrp  TYPE bu_group,
            p_bptyp  TYPE bu_type.

DATA: ls_bp_header type /MDGBP/_S_BP_PP_BP_HEADER.

ls_bp_header = VALUE #( bu_group = 'BP02' bu_type = 2 ).

*CALL METHOD zmdg_cl_create_crequest_gov=>create_change_request
*  EXPORTING
*    iv_cr_type    = p_crtype                 " Change Request
*    iv_data_model = p_model                 " Data Model
*    iv_bu_group   = p_bpgrp                 " Business Partner Grouping
*    iv_bu_type    = p_bptyp                 " Business Partner Category
*    is_bp_header  = ls_bp_header
*    is_bp_centrl  =
*    is_ad_postal  =
*  IMPORTING
*    et_message    = DATA(lt_message)                 " Messages
*    ev_cr_id      = DATA(lv_cr_id).                 " Change Request

IF lt_message IS INITIAL.
  cl_demo_output=>display( | Change Request { lv_cr_id } has been created successfully | ).
ELSE.
  cl_demo_output=>display( lt_message ).
ENDIF.
