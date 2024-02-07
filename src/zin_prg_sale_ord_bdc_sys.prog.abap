report ZIN_PRG_SALE_ORD_BDC_SYS
       no standard page heading line-size 255.

* Include bdcrecx1_s:
* The call transaction using is called WITH AUTHORITY-CHECK!
* If you have own auth.-checks you can use include bdcrecx1 instead.
include bdcrecx1_s.

start-of-selection.

perform open_group.

perform bdc_dynpro      using 'SAPMV45A' '0101'.
perform bdc_field       using 'BDC_CURSOR'
                              'VBAK-SPART'.
perform bdc_field       using 'BDC_OKCODE'
                              '=ENT2'.
perform bdc_field       using 'VBAK-AUART'
                              'OR'.
perform bdc_field       using 'VBAK-VKORG'
                              '1710'.
perform bdc_field       using 'VBAK-VTWEG'
                              '10'.
perform bdc_field       using 'VBAK-SPART'
                              '00'.
perform bdc_dynpro      using 'SAPMV45A' '4001'.
perform bdc_field       using 'BDC_OKCODE'
                              'PICK'.
perform bdc_field       using 'BDC_CURSOR'
                              'KUWEV-KUNNR'.
perform bdc_field       using 'KUAGV-KUNNR'
                              '17100006'.
perform bdc_field       using 'KUWEV-KUNNR'
                              '17100007'.
perform bdc_field       using 'VBKD-BSTKD'
                              'Aditya'.
perform bdc_field       using 'VBKD-BSTDK'
                              '06.04.2023'.
perform bdc_field       using 'RV45A-KETDAT'
                              '06.04.2023'.
perform bdc_field       using 'RV45A-KPRGBZ'
                              'D'.
perform bdc_field       using 'VBKD-PRSDT'
                              '06.04.2023'.
perform bdc_dynpro      using 'SAPMV45A' '4001'.
perform bdc_field       using 'BDC_OKCODE'
                              '/00'.
perform bdc_field       using 'VBKD-BSTKD'
                              'Aditya'.
perform bdc_field       using 'VBKD-BSTDK'
                              '06.04.2023'.
perform bdc_field       using 'KUAGV-KUNNR'
                              '17100006'.
perform bdc_field       using 'KUWEV-KUNNR'
                              '17100007'.
perform bdc_field       using 'RV45A-KETDAT'
                              '06.04.2023'.
perform bdc_field       using 'RV45A-KPRGBZ'
                              'D'.
perform bdc_field       using 'VBKD-PRSDT'
                              '06.04.2023'.
perform bdc_field       using 'VBKD-ZTERM'
                              '0004'.
perform bdc_field       using 'VBKD-INCO1'
                              'EXW'.
perform bdc_field       using 'VBKD-INCO2_L'
                              'Palo Alto'.
perform bdc_field       using 'BDC_CURSOR'
                              'VBAP-VRKME(01)'.
perform bdc_field       using 'RV45A-MABNR(01)'
                              'TG11'.
perform bdc_field       using 'RV45A-KWMENG(01)'
                              '                 20'.
perform bdc_field       using 'VBAP-VRKME(01)'
                              'PC'.
perform bdc_dynpro      using 'SAPMSSY0' '0120'.
perform bdc_field       using 'BDC_CURSOR'
                              '04/03'.
perform bdc_field       using 'BDC_OKCODE'
                              '=CANC'.
perform bdc_dynpro      using 'SAPMV45A' '4001'.
perform bdc_field       using 'BDC_OKCODE'
                              'CANC'.
perform bdc_field       using 'VBKD-BSTKD'
                              'Aditya'.
perform bdc_field       using 'VBKD-BSTDK'
                              '06.04.2023'.
perform bdc_field       using 'KUAGV-KUNNR'
                              '17100006'.
perform bdc_field       using 'KUWEV-KUNNR'
                              '17100007'.
perform bdc_field       using 'RV45A-KETDAT'
                              '06.04.2023'.
perform bdc_field       using 'RV45A-KPRGBZ'
                              'D'.
perform bdc_field       using 'VBKD-PRSDT'
                              '06.04.2023'.
perform bdc_field       using 'VBKD-ZTERM'
                              '0004'.
perform bdc_field       using 'VBKD-INCO1'
                              'EXW'.
perform bdc_field       using 'VBKD-INCO2_L'
                              'Palo Alto'.
perform bdc_field       using 'BDC_CURSOR'
                              'VBAP-VRKME(02)'.
perform bdc_field       using 'RV45A-MABNR(02)'
                              'TG11'.
perform bdc_field       using 'RV45A-KWMENG(02)'
                              '                 30'.
perform bdc_field       using 'VBAP-VRKME(02)'
                              'PC'.
perform bdc_dynpro      using 'SAPMV45A' '4001'.
perform bdc_field       using 'BDC_OKCODE'
                              '=SICH'.
perform bdc_field       using 'VBKD-BSTKD'
                              'Aditya'.
perform bdc_field       using 'VBKD-BSTDK'
                              '06.04.2023'.
perform bdc_field       using 'KUAGV-KUNNR'
                              '17100006'.
perform bdc_field       using 'KUWEV-KUNNR'
                              '17100007'.
perform bdc_field       using 'RV45A-KETDAT'
                              '06.04.2023'.
perform bdc_field       using 'RV45A-KPRGBZ'
                              'D'.
perform bdc_field       using 'VBKD-PRSDT'
                              '06.04.2023'.
perform bdc_field       using 'VBKD-ZTERM'
                              '0004'.
perform bdc_field       using 'VBKD-INCO1'
                              'EXW'.
perform bdc_field       using 'VBKD-INCO2_L'
                              'Palo Alto'.
perform bdc_field       using 'BDC_CURSOR'
                              'RV45A-MABNR(02)'.
perform bdc_dynpro      using 'SAPLSPO2' '0101'.
perform bdc_field       using 'BDC_OKCODE'
                              '=OPT1'.
perform bdc_transaction using 'VA01'.

perform close_group.
