report ZIN_PRG_CREMAT042023
       no standard page heading line-size 255.

* Include bdcrecx1_s:
* The call transaction using is called WITH AUTHORITY-CHECK!
* If you have own auth.-checks you can use include bdcrecx1 instead.
include bdcrecx1_s.

start-of-selection.

perform open_group.

perform bdc_dynpro      using 'SAPLMGMM' '0060'.
perform bdc_field       using 'BDC_CURSOR'
                              'RMMG1-MTART'.
perform bdc_field       using 'BDC_OKCODE'
                              '=ENTR'.
perform bdc_field       using 'RMMG1-MATNR'
                              'Test-3944'.
perform bdc_field       using 'RMMG1-MBRSH'
                              'C'.
perform bdc_field       using 'RMMG1-MTART'
                              'FGTR'.
perform bdc_dynpro      using 'SAPLMGMM' '0070'.
perform bdc_field       using 'BDC_CURSOR'
                              'MSICHTAUSW-DYTXT(01)'.
perform bdc_field       using 'BDC_OKCODE'
                              '=ENTR'.
perform bdc_field       using 'MSICHTAUSW-KZSEL(01)'
                              'X'.
perform bdc_dynpro      using 'SAPLMGMM' '4004'.
perform bdc_field       using 'BDC_OKCODE'
                              '=BU'.
perform bdc_field       using 'MAKT-MAKTX'
                              'ZIN: Demo Material'.
perform bdc_field       using 'BDC_CURSOR'
                              'MARA-MEINS'.
perform bdc_field       using 'MARA-MEINS'
                              'PC'.
perform bdc_field       using 'MARA-MTPOS_MARA'
                              'NORM'.
perform bdc_transaction using 'MM01'.

perform close_group.
