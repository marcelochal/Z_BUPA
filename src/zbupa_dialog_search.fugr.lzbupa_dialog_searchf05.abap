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

*COPY From LBUPA_DIALOG_SEARCHF05, PLEASE CHECK BEFORE CHANGE!!!!
*---------------------------------------------------------------------*
*       CLASS lcl_searcher IMPLEMENTATION
*---------------------------------------------------------------------*

CLASS lcl_searcher IMPLEMENTATION.

*---------------------------------------------------------------------
* METHOD SELECT_BY_TAX_TAXNUM - Search by Tax Numbers.
*---------------------------------------------------------------------
  METHOD select_by_tax_taxnum.
*   local data.
    CLASS cl_exithandler DEFINITION LOAD.

    DATA:
      lv_partner_type  TYPE bus_partner-type,
      lr_taxnum        TYPE RANGE OF dfkkbptaxnum-taxnum,
      lt_partner_guids TYPE bus_partner-guid_table.

*   Check that a search value has been specified.
    IF im_v_taxnum IS INITIAL.
      RAISE search_values_missing.
    ENDIF.

    CALL METHOD iv_search->get_range_for
      EXPORTING
        iv_value = im_v_taxnum
      IMPORTING
        et_range = lr_taxnum.

* Determine the partner type to filter by.
    CASE iv_search->gv_search_type.
      WHEN cl_bupa_dialog_searcher=>gc_search_type_all.
        lv_partner_type = space.
      WHEN cl_bupa_dialog_searcher=>gc_search_type_person.
        lv_partner_type = '1'.
      WHEN cl_bupa_dialog_searcher=>gc_search_type_organization.
        lv_partner_type = '2'.
      WHEN cl_bupa_dialog_searcher=>gc_search_type_group.
        lv_partner_type = '3'.
    ENDCASE.

* Perform any filtering on the result?
    IF ( lv_partner_type NE space ).
      SELECT DISTINCT partner_guid
        INTO TABLE lt_partner_guids
        FROM but000             AS but
        INNER JOIN dfkkbptaxnum AS tax ON tax~partner = but~partner
        UP TO iv_search->gv_maximum_rows ROWS
        WHERE tax~taxnum IN lr_taxnum
          AND type       EQ lv_partner_type.

    ELSE.
      SELECT DISTINCT partner_guid
        INTO TABLE lt_partner_guids
        FROM but000 AS but
        INNER JOIN dfkkbptaxnum AS tax ON tax~partner = but~partner
        UP TO iv_search->gv_maximum_rows ROWS
        WHERE tax~taxnum IN lr_taxnum.
    ENDIF.

*   This selection requires no filtering.
    CALL METHOD cl_bupa_dialog_searcher=>add_partner_guids_to_result
      EXPORTING
        it_partner_guids         = lt_partner_guids
        iv_filter_by_search_type = space
        iv_search                = iv_search.

  ENDMETHOD.

*---------------------------------------------------------------------
* METHOD determine_addrv_active
*---------------------------------------------------------------------
  METHOD determine_addrv_active.
*   Determine, if adress versions are active
    DATA: lv_active_vers TYPE i,
          lt_adrv_act    TYPE STANDARD TABLE OF v_saptsadv,
          ls_active_vers LIKE LINE OF lt_adrv_act,
          lv_addrv_exist TYPE boole-boole,
          ls_dropdown    TYPE bus_screen-dropdown_value.

    IF gv_addrv_checked IS INITIAL.

      CLEAR gv_addrv_active.

*     function modules exists, check for address versions
      CALL FUNCTION 'ADDR_TSADV_READ_ALL'
        IMPORTING
          number_of_active_versions = lv_active_vers
        TABLES
          active_versions           = lt_adrv_act.

*     check result
      IF lv_active_vers GT 0.
        gv_addrv_active = 'X'.

*     check logon nation -> needed for display
        CALL FUNCTION 'ADDR_LANGUAGE_MAP_TO_VERSION'
          EXPORTING
            iv_language              = sy-langu
            iv_application_component = gc_adrv_element
          IMPORTING
            ev_nation                = gv_logon_nation
          EXCEPTIONS
            parameter_error          = 1
            internal_error           = 2
            OTHERS                   = 3.

        IF sy-subrc <> 0.
          gv_logon_nation = space.
        ENDIF.

*       copy active addressversions to dropdown list
        CLEAR gt_screen_nation_dropdown.

        LOOP AT lt_adrv_act INTO ls_active_vers.
          MOVE ls_active_vers-nation_tex TO ls_dropdown-text.
          MOVE ls_active_vers-nation     TO ls_dropdown-key.
          APPEND ls_dropdown TO gt_screen_nation_dropdown.
        ENDLOOP.

*       insert empty dropdown entry
        ls_dropdown-key = space.
        ls_dropdown-text = space.
        INSERT ls_dropdown INTO gt_screen_nation_dropdown INDEX 1.

      ENDIF.

      gv_addrv_checked = 'X'.

    ENDIF.

  ENDMETHOD.                    "determine_addrv_active

ENDCLASS.                    "lcl_searcher IMPLEMENTATION
