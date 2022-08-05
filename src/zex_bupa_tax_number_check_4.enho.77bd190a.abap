"Name: \FU:TAXNUMBER_CHECK_DUPL_VENDOR\SE:BEGIN\EI
ENHANCEMENT 0 ZEX_BUPA_TAX_NUMBER_CHECK_4.

* Verifica se o Fornecedor é passivel de criar mesmo com o CNPJ duplicado
  CHECK zcl_bupa_tax_number_check=>check_bp_duplicity(
                                      is_vendor  = vendor ) EQ zcl_bupa_tax_number_check=>co_check_tax-ON.

ENDENHANCEMENT.
