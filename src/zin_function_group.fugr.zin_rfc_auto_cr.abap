FUNCTION zin_rfc_auto_cr.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(IV_CR_TYPE) TYPE  USMD_CREQUEST_TYPE
*"     VALUE(IV_DATA_MODEL) TYPE  USMD_MODEL
*"     VALUE(IS_BP_CENTRL) TYPE  /MDGBP/_S_BP_PP_BP_CENTRL
*"     VALUE(IS_BP_HEADER) TYPE  /MDGBP/_S_BP_PP_BP_HEADER
*"     VALUE(IS_AD_POSTAL) TYPE  /MDGBP/_S_BP_PP_AD_POSTAL
*"     VALUE(IV_CR_DESC) TYPE  USMD_TXTLG
*"  CHANGING
*"     VALUE(CV_CR_ID) TYPE  USMD_CREQUEST
*"----------------------------------------------------------------------

  CALL METHOD zmdg_cl_create_crequest=>create_change_request
    EXPORTING
      iv_cr_type    = iv_cr_type              " Change Request
      iv_data_model = iv_data_model                 " Data Model
      is_bp_centrl  = is_bp_centrl                  " Source Structure for PP Mapping
      is_bp_header  = is_bp_header                  " Source Structure for PP Mapping
      is_ad_postal  = is_ad_postal                  " Source Structure for PP Mapping
      iv_cr_desc    = iv_cr_desc                    " Description (long text)
    IMPORTING
      ev_cr_id      = cv_cr_id.                   " Change Request



ENDFUNCTION.
