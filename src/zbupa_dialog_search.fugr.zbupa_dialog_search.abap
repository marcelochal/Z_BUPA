FUNCTION ZBUPA_DIALOG_SEARCH.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     REFERENCE(IV_SEARCH) TYPE REF TO  CL_BUS_LOCATOR_SEARCH
*"  EXCEPTIONS
*"      SEARCH_VALUES_MISSING
*"----------------------------------------------------------------------
**********************************************************************
* Local data.
  DATA:
    ls_search_fields TYPE bus_joel_search.

* Convert generic container into specific fields.
  ls_search_fields = iv_search->gv_search_fields.           "#EC ENHOK

* Choose the select.
  CASE iv_search->gv_search_id.

    WHEN lcl_searcher=>gc_search_id-tax_number.
      CALL METHOD lcl_searcher=>select_by_tax_taxnum
        EXPORTING
          iv_search             = iv_search
          im_v_taxnum           = ls_search_fields-taxnum
        EXCEPTIONS
          search_values_missing = 1.

    WHEN OTHERS.
      CLEAR sy-subrc.
  ENDCASE.


  CASE sy-subrc.
    WHEN 1.
      RAISE search_values_missing.
  ENDCASE.

ENDFUNCTION.
