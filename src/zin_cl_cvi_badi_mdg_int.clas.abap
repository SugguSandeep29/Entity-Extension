class ZIN_CL_CVI_BADI_MDG_INT definition
  public
  final
  create public .

public section.

  interfaces IF_BADI_INTERFACE .
  interfaces IF_EX_CVI_CUSTOM_MAPPER .
protected section.
private section.
ENDCLASS.



CLASS ZIN_CL_CVI_BADI_MDG_INT IMPLEMENTATION.


  method IF_EX_CVI_CUSTOM_MAPPER~MAP_BP_TO_VENDOR.

    c_vendor-central_data-central-data-zlicense = i_partner-central_data-common-data-zz_license.
    c_vendor-central_data-central-datax-zlicense = i_partner-central_data-common-datax-zz_license.

  endmethod.


  method IF_EX_CVI_CUSTOM_MAPPER~MAP_VENDOR_TO_BP.

    c_partner-central_data-common-data-zz_license = i_vendor-central_data-central-data-zlicense.
    c_partner-central_data-common-datax-zz_license = i_vendor-central_data-central-datax-zlicense.

  endmethod.
ENDCLASS.
