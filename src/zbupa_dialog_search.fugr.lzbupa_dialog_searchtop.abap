*--------------------------------------------------------------------*
*                       P R O J E T O   A G I R                      *
*--------------------------------------------------------------------*
* Consulting  .....: IntechPro                                       *
* ABAP Developer ..: Marcelo Alvares (MA004818)                      *
* Business Consult.: Marcelo Alvares                                 *
* Module ..........: BUPA - Business partner                         *
* Program     .....: ZBUPA_DIALOG_SEARCH                             *
* Transaction .....: BP                                              *
* Type        .....: Function                                        *
* Objective   .....: Additional BP Selection Fields                  *
* Request     .....:                                                 *
*--------------------------------------------------------------------*
* Change Control:                                                    *
* Version  Date       Who                What                        *
*    1.00  02/10/2019 Marcelo Alvares    Initial release             *
**********************************************************************
FUNCTION-POOL zbupa_dialog_search MESSAGE-ID bupa_joels_dialog. "#EC * "#EC MG_MISSING

*COPY From BUPA_DIALOG_SEARCH CHECK BEFORE CHANGE!!!!
**********************************************************************
*>>>>>>>>>>>>>>>>>>>>>>>>> GENERAL Section <<<<<<<<<<<<<<<<<<<<<<<<<<*
**********************************************************************

CONSTANTS:
  gc_adrv_element TYPE ad_dtel      VALUE 'BU_ADDRVERS_MAP'.


DATA:
  gv_addrv_active  TYPE bu_boolean,
  gv_addrv_checked TYPE bu_boolean  VALUE space,
  gv_logon_nation  TYPE ad_nation   VALUE space.

*-----------------------------------------------------------------------
*
* DIALOG Section
*
*-----------------------------------------------------------------------

* Forward declarations for global classes.
CLASS cl_bupa_dialog_searcher DEFINITION LOAD.

*
DATA gs_dynpro_id TYPE bus_screen-area.

*
TABLES bus_joel_search.

* The contents of the dropdown boxes.
DATA:
  gt_screen_nation_dropdown     TYPE bus_screen-dropdown_values.

*---------------------------------------------------------------------*
*       CLASS lcl_searcher IMPLEMENTATION
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*

CLASS lcl_searcher DEFINITION FINAL.

  PUBLIC SECTION.
*    CLASS-DATA:
*      gv_addrv_checked TYPE bu_boolean VALUE space.

    CONSTANTS:

*     The search id.
      BEGIN OF gc_search_id,
*       For all partner types.
        tax_number TYPE bus_locator-search_id VALUE 'ZBUPA_ALL_TAX',
      END OF gc_search_id.

    CLASS-METHODS:

      determine_addrv_active,

      select_by_tax_taxnum
        IMPORTING
                   iv_search   TYPE REF TO cl_bus_locator_search
                   im_v_taxnum TYPE bus_joel_search-taxnum
        EXCEPTIONS search_values_missing.

ENDCLASS.
