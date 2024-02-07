FUNCTION zin_fm_secret_number_updates.
*"----------------------------------------------------------------------
*"*"Update Function Module:
*"
*"*"Local Interface:
*"  TABLES
*"      X_SECRET_NUMBERS STRUCTURE  ZIN_T_SECRET_NUM
*"      Y_SECRET_NUMBER STRUCTURE  ZIN_T_SECRET_NUM
*"----------------------------------------------------------------------
  DATA: ls_y_secret_number TYPE zin_t_secret_num.

  LOOP AT y_secret_number INTO ls_y_secret_number.
    READ TABLE x_secret_numbers WITH KEY bukrs = ls_y_secret_number-bukrs
                                         lifnr = ls_y_secret_number-lifnr
                                         secret_number = ls_y_secret_number-secret_number.

    IF sy-subrc <> 0.
      DELETE FROM zin_t_secret_num WHERE bukrs = ls_y_secret_number-bukrs
                                      AND lifnr = ls_y_secret_number-lifnr
                                      AND secret_number = ls_y_secret_number-secret_number.

      IF sy-subrc <> 0.
        MESSAGE x003(zin_cl_msg).
      ENDIF.
    ENDIF.
  ENDLOOP.

  MODIFY zin_t_secret_num FROM TABLE x_secret_numbers. "Current State
  IF sy-subrc <> 0.
    MESSAGE x003(zin_cl_msg).
  ENDIF.




ENDFUNCTION.
