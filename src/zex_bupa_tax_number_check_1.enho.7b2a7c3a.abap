"Name: \FU:BUPA_TAX_NUMBER_CHECK_DUPL\SE:BEGIN\EI
ENHANCEMENT 0 ZEX_BUPA_TAX_NUMBER_CHECK_1.
*AGIR - S4H-FIN-EFD-M-035

  CHECK zcl_bupa_tax_number_check=>check_bp_duplicity(
                                      iv_partner = iv_partner
                                      iv_taxnum  = iv_taxnum ) EQ zcl_bupa_tax_number_check=>co_check_tax-ON.

ENDENHANCEMENT.
