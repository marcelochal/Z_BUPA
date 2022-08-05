"Name: \FU:BUPA_CONV_EI2BAPI\SE:BEGIN\EI
ENHANCEMENT 0 ZEX_BUPA_TAX_NUMBER_CHECK_3.
 CALL METHOD zcl_bupa_tax_number_check=>export_to_memory_bp_group
   EXPORTING
     iv_bp_group = is_bp_interface-central_data-common-data-bp_control-grouping.
ENDENHANCEMENT.
