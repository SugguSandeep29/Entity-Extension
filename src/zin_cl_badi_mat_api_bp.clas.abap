class ZIN_CL_BADI_MAT_API_BP definition
  public
  final
  create public .

public section.

  interfaces IF_BADI_INTERFACE .
  interfaces IF_MDG_BS_MAT_API_SEGMENTS_EXT .
protected section.
private section.
ENDCLASS.



CLASS ZIN_CL_BADI_MAT_API_BP IMPLEMENTATION.


  METHOD if_mdg_bs_mat_api_segments_ext~check_and_save.
    DATA:ls_data    TYPE zin_s_mat_but,
         ls_data_x  TYPE zin_s_mat_but_x,
         ls_message TYPE mdg_bs_mat_s_mat_msg,
         lt_modify  TYPE STANDARD TABLE OF zin_t_mat_but,
         ls_modify  TYPE zin_t_mat_but,
         lt_delete  TYPE STANDARD TABLE OF zin_t_mat_but,
         ls_delete  TYPE zin_t_mat_but.

    CLEAR et_message.
*   Validation
    LOOP AT is_data-zin_t_mat_but_tab INTO ls_data.
*    All fields must be filled otherwise display error message
      IF ls_data-matnr IS INITIAL OR ls_data-bp_id IS INITIAL OR ls_data-nickname IS INITIAL.
        ls_message-msgv1 = 'Empty Value detected in: '.
        ls_message-msgv2 = ls_data-matnr.
        ls_message-msgv3 = ls_data-bp_id.
        ls_message-msgv4 = ls_data-nickname.
        ls_message-msgty = 'E'.
        INSERT ls_message INTO TABLE et_message.
      ENDIF.
    ENDLOOP.

    CHECK iv_test_mode = abap_false.
*    Determine which record to delete or insert/modify
    LOOP AT is_data-zin_t_mat_but_x_tab INTO ls_data_x.
      IF ls_data_x-delete_row = abap_true.
        ls_delete-matnr = ls_data_x-matnr.
        ls_delete-bp_id = ls_data_x-bp_id.
        INSERT ls_delete INTO TABLE lt_delete.
      ELSE.
        READ TABLE is_data-zin_t_mat_but_tab INTO ls_data
                                             WITH TABLE KEY matnr = ls_data_x-matnr
                                                            bp_id = ls_data_x-bp_id.
        ASSERT sy-subrc = 0.
        MOVE-CORRESPONDING ls_data TO ls_modify.
        INSERT ls_modify INTO TABLE lt_modify.

      ENDIF.

    ENDLOOP.

*    Change DB
*    Note: In Productive Implementation, these DB changes must be done on COMMIT ( Perform XXX ON COMMIT or CALL FUNCTION XXX in UPDATE TASK )
*    Note: We are ignoring the X Structure here.
*    If data gets changes, all fields get updated
    IF lt_delete IS NOT INITIAL.
      DELETE zin_t_mat_but FROM TABLE lt_delete.
    ENDIF.
    IF lt_modify IS NOT INITIAL.
      MODIFY zin_t_mat_but FROM TABLE lt_modify.
    ENDIF.
  ENDMETHOD.


  method IF_MDG_BS_MAT_API_SEGMENTS_EXT~GET_ES_NODEINFO.
  endmethod.


  METHOD if_mdg_bs_mat_api_segments_ext~read.
    DATA: lt_mdgm_bupa TYPE zin_tt_mat_but.
    CLEAR: et_data,et_message.
    SELECT * FROM zin_t_mat_but
      INTO CORRESPONDING FIELDS OF TABLE lt_mdgm_bupa
      WHERE matnr IN is_selection-matnr_range
      AND bp_id IN is_selection-bp_id_range.
    INSERT LINES OF lt_mdgm_bupa INTO TABLE et_data.
  ENDMETHOD.
ENDCLASS.
