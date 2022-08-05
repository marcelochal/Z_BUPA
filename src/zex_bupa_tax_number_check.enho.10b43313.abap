"Name: \PR:SAPLBUPA_DIALOG_JOEL\TY:LCL_BDT\ME:CHECK_CURRENT_SCREEN\SE:BEGIN\EI
ENHANCEMENT 0 ZEX_BUPA_TAX_NUMBER_CHECK.
    CALL METHOD zcl_bupa_tax_number_check=>export_to_memory_bp_group
      EXPORTING
        iv_bp_group = bus_joel_main-creation_group.

ENDENHANCEMENT.
