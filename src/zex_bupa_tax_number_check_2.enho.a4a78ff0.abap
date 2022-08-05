"Name: \TY:CL_MDG_BS_FND_BP_CHECK\ME:TAXNUMBERS_CHECK\SE:BEGIN\EI
ENHANCEMENT 0 ZEX_BUPA_TAX_NUMBER_CHECK_2.
    CALL METHOD zcl_bupa_tax_number_check=>export_to_memory_bp_group(
                 is_bp-central_data-common-data-bp_control-grouping ).
ENDENHANCEMENT.
