*----------------------------------------------------------------------*
***INCLUDE LZIN_T_CRTYPEF01.
*----------------------------------------------------------------------*

FORM save.
  BREAK-POINT.
  DATA ls_crtype TYPE zin_t_crtype.

  LOOP AT total.

    MOVE-CORRESPONDING <vim_total_struc> TO ls_crtype.

    MODIFY zin_t_crtype_cpy FROM ls_crtype.

  ENDLOOP.
ENDFORM.
