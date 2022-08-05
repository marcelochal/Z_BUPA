*----------------------------------------------------------------------*
*                      ___    _     __    __    _                      *
*                       |    |_|   |_    (_    |_|                     *
*                       |    | |   |__   __)   | |                     *
*                                                                      *
*----------------------------------------------------------------------*
*            TRANSMISSORA ALIANÇA DE ENERGIA ELÉTRICA S.A.             *
*----------------------------------------------------------------------*
* ABAP Developer ..: Marcelo Alvares (MA004818)                        *
* Business Consult.: Marcelo Alvares (MA004818)                        *
* Module ..........: BUPA - Business partner                           *
* Program     .....: ZBUPA_0002                                        *
* Transaction .....: ZBP004                                            *
* Type        .....: Report Include                                    *
* Objective   .....: Business partners load                            *
* Request     .....: SHDK908669                                        *
*----------------------------------------------------------------------*
* Change Control:                                                      *
* Date       | Who                                      |Task          *
* 08.05.2020 | Marcelo Alvares (MA004818)               |RITM0077873   *
************************************************************************
* Classic ABAP OO                                                      *
* Knowledge   of   object-oriented   programming   is   required.      *
* Only  do  maintenance  if  you  really  know  what  you  are  doing. *
* Note  Subroutines  are  obsolete in S4/HANA.  Do  not create new     *
* subroutines in new  programs.  Methods  should  be  used   instead.  *
* Subroutines created in existing programs for internal modularization *
* can   continue   to   be   called.   Whenever   possible,   however, *
* external  subroutine  calls  from  other  programs should be avoided.*
************************************************************************
*&---------------------------------------------------------------------*
*& Include          ZBUPA_0002_LCL
*&---------------------------------------------------------------------*

"! <p class="shorttext synchronized" lang="en">Local class inheriting from <strong>zcl_file_upload</strong> with all procedures to upload excel file format </p>
CLASS lcl_file_upload DEFINITION INHERITING FROM zcl_file_upload FRIENDS lcl_zbupa_0002.

  PUBLIC SECTION.

    TYPES:
      "! <p class="shorttext synchronized" lang="en">Type of Structure for BP Data, used in the upload excel file</p>
      BEGIN OF ty_s_upload_general_data,
        taxnum     TYPE dfkkbptaxnum-taxnum,  " Business Partner Tax Number
*        bukrs      TYPE lfb1-bukrs, "Check it out --NO NEED
        name       TYPE hrpaybr_dirf_comp_name, " Full Name
        serchterm  TYPE adrc-sort1,             " Termo de Pesquisa
        insc_est   TYPE lfa1-stcd3,             " Inscrição Estadual
        insc_mun   TYPE lfa1-stcd4,             " Inscrição Municipal
        abc        TYPE lfm1-lfabc,             " Curva ABC
        legal_org  TYPE but000-legal_org,       " Parte Relacionada
        street     TYPE addr1_data-street,      " Street
        house_no   TYPE addr1_data-house_num1,  " House Number
        city       TYPE addr1_data-city1,       " City
        district   TYPE addr1_data-city2,       " District
        region     TYPE addr1_data-region,      " Region (State, Province, County)
        postl_cod1 TYPE addr2_data-post_code1,  " City postal code
        tel_number TYPE sza1_d0100-tel_number,  " First telephone no.: dialling code+number
        smtp_addr  TYPE sza1_d0100-smtp_addr,   " E-Mail Address
        banco      TYPE zpct_banco,             " BANK ID COMPE
        bankl      TYPE bankl,                  " Bank number
        bankn      TYPE bankn,                  " Bank account number
        ctrl_key   TYPE bkont,                  " Bank Control Key
        zcateg1    TYPE lfa1-zcateg1,
        zcateg2    TYPE lfa1-zcateg2,
        zcateg3    TYPE lfa1-zcateg3,
        zsubcateg1 TYPE lfa1-zsubcateg1,
        zsubcateg2 TYPE lfa1-zsubcateg2,
        zsubcateg3 TYPE lfa1-zsubcateg3,
        zsubcateg4 TYPE lfa1-zsubcateg4,
        zsubcateg5 TYPE lfa1-zsubcateg5,
        zsubcateg6 TYPE lfa1-zsubcateg6,
      END OF ty_s_upload_general_data,

      "! <p class="shorttext synchronized" lang="en">Type of Table for BP Data, used in the upload excel file</p>
      ty_t_upload_general_data TYPE STANDARD TABLE OF ty_s_upload_general_data WITH KEY taxnum
                               WITH NON-UNIQUE SORTED KEY taxnum_key COMPONENTS taxnum.

  PROTECTED SECTION.

    DATA:
        "! <p class="shorttext synchronized" lang="en">Data of Structure for BP, used in the upload excel file</p>
        gt_upload_file TYPE ty_t_upload_general_data.

  PRIVATE SECTION.


ENDCLASS.

CLASS lcl_file_upload IMPLEMENTATION.


ENDCLASS.


CLASS lcl_zbupa_0002 DEFINITION INHERITING FROM zcl_bp_maintain FRIENDS lcl_file_upload.

  PUBLIC SECTION.

    TYPES:
      "! <p class="shorttext synchronized" lang="en">Structure of BP Data</p>
      BEGIN OF ty_s_bp_create.
        INCLUDE TYPE ty_s_bp_key.
    TYPES:
      status_bp    TYPE tp_icon,    " Status
      message      TYPE bapi_msg,
      general_data TYPE lcl_file_upload=>ty_s_upload_general_data,
      partner_bus  TYPE bus_ei_extern,
      vendor_vmds  TYPE vmds_ei_extern,
      return_bp    TYPE bapiret2_t,
      END OF ty_s_bp_create.

    TYPES:
      "! <p class="shorttext synchronized" lang="en">Table of BP Data</p>
      ty_t_bp_create TYPE STANDARD TABLE OF ty_s_bp_create WITH KEY partner partner_guid taxnum vendor
        WITH NON-UNIQUE SORTED KEY taxnum_key         COMPONENTS taxnum
        WITH NON-UNIQUE SORTED KEY partner_sort_key   COMPONENTS partner  vendor
        WITH NON-UNIQUE SORTED KEY vendor_sort_key    COMPONENTS vendor   partner .

    TYPES:
      "! <p class="shorttext synchronized" lang="en">Type of Structure Company ID</p>
      BEGIN OF ty_s_bukrs,
        bukrs TYPE bukrs,
      END OF ty_s_bukrs,
      "! <p class="shorttext synchronized" lang="en">Type of Table Company ID</p>
      ty_t_bukrs TYPE STANDARD TABLE OF ty_s_bukrs WITH KEY bukrs.

    TYPES:
      "! <p class="shorttext synchronized" lang="en">Type of Structure Purchasing organization ID</p>
      BEGIN OF ty_s_ekorg,
        ekorg TYPE ekorg,
      END OF ty_s_ekorg,
      "! <p class="shorttext synchronized" lang="en">Type of Table Purchasing organization ID</p>
      ty_t_ekorg TYPE STANDARD TABLE OF ty_s_ekorg WITH KEY ekorg.

    TYPES:
      "! <p class="shorttext synchronized" lang="en">Structure for ALV</p>
      BEGIN OF ty_s_alv,
        taxnum    TYPE bptaxnum,
        partner   TYPE bu_partner,
        vendor    TYPE lifnr,
        status_bp TYPE tp_icon,    " Status
        message   TYPE bapi_msg,
      END OF ty_s_alv,

      "! <p class="shorttext synchronized" lang="en">Type of table for ALV</p>
      ty_t_alv TYPE STANDARD TABLE OF ty_s_alv WITH KEY taxnum partner vendor
                 WITH NON-UNIQUE SORTED KEY taxnum_key         COMPONENTS taxnum
                 WITH NON-UNIQUE SORTED KEY partner_sort_key   COMPONENTS partner  vendor
                 WITH NON-UNIQUE SORTED KEY vendor_sort_key    COMPONENTS vendor   partner .

    CONSTANTS:
      "! <p class="shorttext synchronized" lang="en">Constant Reconciliation account for BP Company data</p>
      co_akont           TYPE lfb1-akont  VALUE '2110130108',
      "! <p class="shorttext synchronized" lang="en">Constant Terms of Payment Key for BP Company data</p>
      co_zterm           TYPE dzterm      VALUE 'TB30',
      co_default_variant TYPE raldb_vari  VALUE 'DEFAULT'.

    METHODS:

      "! <p class="shorttext synchronized" lang="en">Object Constructor method</p>
      constructor,

      "! <p class="shorttext synchronized" lang="en">Excel file upload method</p>
      upload_file
        RAISING
          zcx_file_upload
          zcx_excel,

      "! <p class="shorttext synchronized" lang="en">Create BP method, start process to create BP</p>
      create_bp
        RAISING
          zcx_abap_error,

      "! <p class="shorttext synchronized" lang="en"> On event Link Click </p>
      "! @parameter row | ROW
      "! @parameter column | Column
      on_alv_link_click     FOR EVENT link_click OF cl_salv_events_table
        IMPORTING
            row
            column,

      "! <p class="shorttext synchronized" lang="en"> On event User Command </p>
      "! @parameter e_salv_function | <p class="shorttext synchronized" lang="en"> ALV Function </p>
      on_alv_user_command   FOR EVENT if_salv_events_functions~added_function  OF cl_salv_events_table
        IMPORTING
            e_salv_function,

      "! <p class="shorttext synchronized" lang="en">Class Object for ALV cl_salv_table</p>
      "! @parameter row     | <p class="shorttext synchronized" lang="en">Selected row in alv</p>
      "! @parameter column  | <p class="shorttext synchronized" lang="en">Selected column in alv</p>
      on_alv_double_click   FOR EVENT double_click OF cl_salv_events_table
        IMPORTING
            row
            column,
      "! <p class="shorttext synchronized" lang="en">Display ALV method</p>
      alv_display.

    CLASS-METHODS:
      "! <p class="shorttext synchronized" lang="en">Class Constructor method</p>
      class_constructor,
      "! <p class="shorttext synchronized" lang="en">Export/download file model excel for upload </p>
      export_model.

    DATA:
    "! <p class="shorttext synchronized" lang="en">CLass Object for ALV cl_salv_table</p>
      go_alv_table TYPE REF TO cl_salv_table.

  PROTECTED SECTION.

    DATA:
      go_file    TYPE REF TO lcl_file_upload,
      gt_bp_data TYPE ty_t_bp_create,
      gv_test    TYPE abap_bool,
      gt_alv     TYPE ty_t_alv,
      gt_bukrs   TYPE ty_t_bukrs,
      gt_ekorg   TYPE ty_t_ekorg.

  PRIVATE SECTION.

    METHODS:
      msg_initialize,
      call_bp_maintain
        CHANGING
          ch_s_data TYPE lcl_zbupa_0002=>ty_s_bp_create,
      alv_create,
      alv_set_icon_type
        IMPORTING
          im_t_return      TYPE bapiret2_t
        EXPORTING
          ex_v_status_icon TYPE tp_icon
          ex_v_message     TYPE bapi_msg,

      alv_set_columns,
      get_bp_from_taxnumber_lc
        IMPORTING
          im_t_upload_file TYPE lcl_file_upload=>ty_t_upload_general_data
        CHANGING
          ch_t_bp_data     TYPE lcl_zbupa_0002=>ty_t_bp_create,
      convert_cpf_cnpj
        CHANGING
          ch_v_taxnum TYPE any,
      convert_field_date
        CHANGING
          ch_v_struc TYPE any,
      call_commit_and_check
        CHANGING
          ch_s_data TYPE lcl_zbupa_0002=>ty_s_bp_create,
      extend_vendor
        CHANGING
          ch_s_data TYPE lcl_zbupa_0002=>ty_s_bp_create,
      msg_show
        IMPORTING
          im_t_return TYPE bapiret2_t
          im_v_zeile  TYPE string,
      fill_bank_key
        IMPORTING
          im_s_general_data TYPE lcl_file_upload=>ty_s_upload_general_data
        RETURNING
          VALUE(r_result)   TYPE bus_ei_struc_bankdetail-bank_key,
      update_vendor
        CHANGING
          ch_s_data TYPE lcl_zbupa_0002=>ty_s_bp_create
        RAISING
          zcx_abap_error,
      compare_data
        IMPORTING
          im_s_source  TYPE any
          im_s_current TYPE any
        CHANGING
          ch_s_data_x  TYPE any,
      compare_bp_source_fill_datax
        CHANGING
          ch_s_source TYPE ty_s_bp_create
        RAISING
          zcx_abap_error,
      fill_organization
        IMPORTING
          im_s_general_data    TYPE lcl_file_upload=>ty_s_upload_general_data
        CHANGING
          ch_s_bp_organization TYPE bus_ei_struc_central_organ,
      fill_searchterm
        IMPORTING
          im_v_name       TYPE string
        RETURNING
          VALUE(r_result) TYPE bus_ei_struc_central-searchterm1.

ENDCLASS.

CLASS lcl_zbupa_0002 IMPLEMENTATION.

  METHOD constructor.

    super->constructor( ).

    CREATE OBJECT:
        me->go_file
          EXPORTING
            im_v_file_path = p_file,
        o_prog_ind TYPE zcl_progress_indicator.

    gv_test = p_test.

    SELECT bukrs FROM t001      " #EC CI_NO_TRANSFORM
    INTO TABLE me->gt_bukrs
    WHERE bukrs IN r_bukrs.

    SELECT ekorg FROM t024e      " #EC CI_NO_TRANSFORM
    INTO TABLE me->gt_ekorg
    WHERE ekorg IN r_ekorg.

    me->alv_create( ).

  ENDMETHOD.

  METHOD class_constructor.

  ENDMETHOD.


  METHOD export_model.

    DATA: ls_file_structure  TYPE lcl_file_upload=>ty_s_upload_general_data.

    lcl_file_upload=>export_model(
      EXPORTING
        im_model       = ls_file_structure ).

  ENDMETHOD.


  METHOD upload_file.

    CALL METHOD:
        me->go_file->file_validate_path,
        me->go_file->set_parameter_id.

    me->go_file->upload_zcl_excel(
      CHANGING
        ch_s_converted_data   = me->go_file->gt_upload_file
      EXCEPTIONS
        OTHERS                = 3 ).

    IF sy-subrc IS NOT INITIAL.
      MESSAGE ID sy-msgid TYPE 'S' NUMBER sy-msgno
        WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 DISPLAY LIKE 'E'.
      RAISE EXCEPTION TYPE zcx_file_upload
        EXPORTING
          syst_at_raise = syst.
    ENDIF.

    " Remove if TaxNum is initial.
    DELETE me->go_file->gt_upload_file WHERE taxnum IS INITIAL.

    LOOP AT me->go_file->gt_upload_file ASSIGNING FIELD-SYMBOL(<general_data>).
      me->convert_cpf_cnpj(
        CHANGING
          ch_v_taxnum = <general_data>-taxnum ).

      me->convert_field_date(
        CHANGING
          ch_v_struc = <general_data> ).

    ENDLOOP.

  ENDMETHOD.

  METHOD create_bp.

    DATA: ls_company   TYPE vmds_ei_company,
          ls_purch     TYPE vmds_ei_purchasing,   " Purchasing Organization
          ls_functions TYPE vmds_ei_functions.    " Partner Roles

    CALL METHOD me->get_bp_from_taxnumber_lc(
      EXPORTING
        im_t_upload_file = me->go_file->gt_upload_file
      CHANGING
        ch_t_bp_data     = me->gt_bp_data ).

    me->o_prog_ind->set_total( im_total = lines( me->gt_bp_data )  ).
    me->o_prog_ind->reset_processed( ).

    "Process only that do not exist in the database
    LOOP AT me->gt_bp_data ASSIGNING FIELD-SYMBOL(<bp_data>).

      me->o_prog_ind->show( ).

*>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> E X T E N D  B P <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<*

      " If Find Business Partner already exist, make the update and extension
      IF <bp_data>-partner IS NOT INITIAL AND <bp_data>-vendor IS NOT INITIAL.

        me->update_vendor(
          CHANGING
            ch_s_data = <bp_data> ).

        me->extend_vendor(
          CHANGING
            ch_s_data = <bp_data> ).



        CONTINUE. " Next LOOP

      ENDIF.

      " Assigning fields symbols
      APPEND INITIAL LINE TO:
        <bp_data>-partner_bus-central_data-role-roles               ASSIGNING FIELD-SYMBOL(<ls_roles_00>), "Para função FLVN00
        <bp_data>-partner_bus-central_data-role-roles               ASSIGNING FIELD-SYMBOL(<ls_roles_01>), "Para função FLVN01
        <bp_data>-partner_bus-central_data-address-addresses        ASSIGNING FIELD-SYMBOL(<ls_addresses>).

      ASSIGN <bp_data>-general_data TO FIELD-SYMBOL(<general_data>).

      " Add Insert task to all fields
      MOVE me->co_task-insert TO:
        <bp_data>-partner_bus-header-object_task,
        <bp_data>-vendor_vmds-header-object_task,
        ls_company-task,
        <ls_addresses>-task,
        <ls_roles_00>-task,
        <ls_roles_01>-task.


*>>>>>>>>>>>>>>>>>>>>>>>>>>>> C E N T R A L  D A T A <<<<<<<<<<<<<<<<<<<<<<<<<<<<

      <bp_data>-partner_guid                                                      = me->create_bp_partner_guid( ).
      <bp_data>-partner_bus-header-object_instance-bpartnerguid                   = <bp_data>-partner_guid.
      <bp_data>-partner_bus-central_data-common-data-bp_control-category          = me->co_category_bp_as-org.

      fill_organization(
        EXPORTING
          im_s_general_data    = <general_data>
        CHANGING
          ch_s_bp_organization = <bp_data>-partner_bus-central_data-common-data-bp_organization       ).

      " Business Partner Grouping
      IF p_grp IS INITIAL.
        <bp_data>-partner_bus-central_data-common-data-bp_control-grouping        = me->co_grouping-nac_pj.
      ELSE.
        <bp_data>-partner_bus-central_data-common-data-bp_control-grouping        = p_grp.
      ENDIF.


      " Search term 1 for business partner
      IF <bp_data>-general_data-serchterm IS NOT INITIAL.
        <bp_data>-partner_bus-central_data-common-data-bp_centraldata-searchterm1 = <bp_data>-general_data-serchterm.
      ELSE.
        <bp_data>-partner_bus-central_data-common-data-bp_centraldata-searchterm1 = me->fill_searchterm( |{ <general_data>-name }| ).
      ENDIF.

*>>>>>>>>>>>>>>>>>>>>>>>>>>>> A D D R E S S E S <<<<<<<<<<<<<<<<<<<<<<<<<<<<*

      MOVE-CORRESPONDING <general_data> TO <ls_addresses>-data-postal-data.
      <ls_addresses>-data-postal-data-country  = me->co_country_iso_br.
      <ls_addresses>-data-postal-data-languiso = me->co_languiso_pt.
      <ls_addresses>-data-postal-data-langu    = me->co_languiso_pt.

      " Telephone
      IF <general_data>-tel_number IS NOT INITIAL.
        APPEND INITIAL LINE TO
          <bp_data>-partner_bus-central_data-communication-phone-phone ASSIGNING FIELD-SYMBOL(<phone>).
        <phone>-contact-task           = me->co_task-insert. " I
        <phone>-contact-data-country   = me->co_country_iso_br.
        <phone>-contact-data-telephone = <general_data>-tel_number.
        <phone>-contact-data-r_3_user  = '1'. " Default
      ENDIF.

      " Email
      IF <general_data>-smtp_addr IS NOT INITIAL.
        APPEND INITIAL LINE TO
          <bp_data>-partner_bus-central_data-communication-smtp-smtp ASSIGNING FIELD-SYMBOL(<email>).
        <email>-contact-task        = me->co_task-insert. " I.
        <email>-contact-data-e_mail = <general_data>-smtp_addr.
      ENDIF.


*>>>>>>>>>>>>>>>>>>>>>>>>>>>> B A N K S  <<<<<<<<<<<<<<<<<<<<<<<<<<<<*

      IF <general_data>-banco IS NOT INITIAL.

        APPEND INITIAL LINE TO <bp_data>-partner_bus-central_data-bankdetail-bankdetails ASSIGNING FIELD-SYMBOL(<ls_bankdetails>).

        <ls_bankdetails>-task           = me->co_task-insert. " I.
        <ls_bankdetails>-data-bank_ctry = 'BR'.
        <ls_bankdetails>-data-bank_key  = me->fill_bank_key( <general_data> ).
        <ls_bankdetails>-data-bank_acct = <general_data>-bankn.     " Bank account number
        <ls_bankdetails>-data-ctrl_key  = <general_data>-ctrl_key.  " Bank Control Key

        IF me->gv_test IS NOT INITIAL.
          <ls_bankdetails>-data_key     = <general_data>-banco. " Avoid msg erros in test
        ENDIF.

      ENDIF.

*>>>>>>>>>>>>>>>>>>>>>>>>>>>> R O L E S  <<<<<<<<<<<<<<<<<<<<<<<<<<<<*

      "FLVN00
      MOVE co_rolecategory-supplier_fin TO:
        <ls_roles_00>-data-rolecategory,
        <ls_roles_00>-data_key.

      <ls_roles_00>-data-valid_from         = sy-datum.

      "FLVN01
      MOVE co_rolecategory-supplier_purchasing TO:
        <ls_roles_01>-data-rolecategory,
        <ls_roles_01>-data_key.

      <ls_roles_01>-data-valid_from         = sy-datum.

*>>>>>>>>>>>>>>>>>>>>>>>> T A X  N U M B E R S <<<<<<<<<<<<<<<<<<<<<<*

      " CNPJ
      APPEND INITIAL LINE TO:
      <bp_data>-partner_bus-central_data-taxnumber-taxnumbers ASSIGNING FIELD-SYMBOL(<ls_cnpj>).
      <ls_cnpj>-task                            = me->co_task-insert.
      <ls_cnpj>-data_key-taxtype                = me->co_taxtype-cnpj.
      <ls_cnpj>-data_key-taxnumber              = <bp_data>-taxnum.


      " Inscrição Estadual
      IF <bp_data>-general_data-insc_est IS NOT INITIAL.
        APPEND INITIAL LINE TO:
        <bp_data>-partner_bus-central_data-taxnumber-taxnumbers ASSIGNING FIELD-SYMBOL(<ls_insc_estadual>).
        <ls_insc_estadual>-task                 = me->co_task-insert.
        <ls_insc_estadual>-data_key-taxnumber   = <bp_data>-general_data-insc_est.
        <ls_insc_estadual>-data_key-taxtype     = me->co_taxtype-ins_stadual.
      ENDIF.

      " Inscrição Municipal
      IF <bp_data>-general_data-insc_mun IS NOT INITIAL.
        APPEND INITIAL LINE TO:
        <bp_data>-partner_bus-central_data-taxnumber-taxnumbers ASSIGNING FIELD-SYMBOL(<ls_insc_municipal>).
        <ls_insc_municipal>-task                = me->co_task-insert.
        <ls_insc_municipal>-data_key-taxnumber  = <bp_data>-general_data-insc_mun.
        <ls_insc_municipal>-data_key-taxtype    = me->co_taxtype-ins_municipal.
      ENDIF.

*>>>>>>>>>>>>>>>>>>>>>>>>>>>> C O M P A N Y <<<<<<<<<<<<<<<<<<<<<<<<<*

      ls_company-data-zwels = p_zwels.  " List of Respected Payment Methods

      IF <general_data>-banco IS INITIAL.

        DATA(lv_zwels_not_allowed) = zformas_pgto_x_dados_banco=>get_bank_payment_methods_tvarv( ).

        REPLACE ALL OCCURRENCES OF `/` IN lv_zwels_not_allowed WITH ` `.

        TRANSLATE ls_company-data-zwels USING lv_zwels_not_allowed.

        CONDENSE ls_company-data-zwels NO-GAPS.

      ENDIF.

      MOVE-CORRESPONDING <bp_data>-general_data TO <bp_data>-vendor_vmds-central_data-central-data.

      IF p_grpte IS INITIAL.
        ls_company-data-fdgrv = me->co_vendor_planning_group-pj.    " Planning Group
      ELSE.
        ls_company-data-fdgrv = p_grpte.
      ENDIF.

      IF p_conpg IS INITIAL.
        ls_company-data-zterm = me->co_zterm.                       " Terms of Payment Key
      ELSE.
        ls_company-data-zterm = p_conpg.
      ENDIF.

      IF p_conpg IS INITIAL.
        ls_company-data-akont = me->co_akont.                       " Reconciliation Account in General Ledger
      ELSE.
        ls_company-data-akont = p_akont.
      ENDIF.

      ls_company-data-reprf = abap_on.                              " Check Flag for Double Invoices or Credit Memos

      " fill data for every company
      LOOP AT me->gt_bukrs ASSIGNING FIELD-SYMBOL(<bukrs>).
        ls_company-data_key-bukrs = <bukrs>-bukrs.
        APPEND ls_company TO <bp_data>-vendor_vmds-company_data-company.
      ENDLOOP.

      " If don't have any company, the standard is TB01 TAESA.
      IF syst-subrc IS NOT INITIAL.
        ls_company-data_key-bukrs = 'TB01'.
        APPEND ls_company TO <bp_data>-vendor_vmds-company_data-company.
      ENDIF.

*>>>>>>>>>>>>>>>>>>>>>>>>>>>> P U R C H A S I N G <<<<<<<<<<<<<<<<<<<<<<<<<<<<*

      CLEAR: ls_functions, ls_purch.

      ls_purch-task           = me->co_task-insert.
      ls_purch-data-lfabc     = <general_data>-abc.
      ls_purch-data-zterm     = me->co_zterm.
      ls_purch-data-lebre     = abap_on.
      ls_purch-data-webre     = abap_on.

      " Partner Roles for Purchasing data
      ls_functions-task           = me->co_task-insert.
      ls_functions-data_key-parvw = 'LF'.
      APPEND ls_functions TO ls_purch-functions-functions.

      ls_functions-data_key-parvw = 'WL'.
      APPEND ls_functions TO ls_purch-functions-functions.

      ls_functions-data_key-parvw = 'RS'.
      APPEND ls_functions TO ls_purch-functions-functions.

      LOOP AT me->gt_ekorg ASSIGNING FIELD-SYMBOL(<ekorg>).

        ls_purch-data_key-ekorg = <ekorg>-ekorg.

        APPEND ls_purch TO <bp_data>-vendor_vmds-purchasing_data-purchasing.

      ENDLOOP.

*>>>>>>>>>>>>>>>>>>>>>> C A L L  M A I N T A I N <<<<<<<<<<<<<<<<<<<*

      CALL METHOD me->call_bp_maintain
        CHANGING
          ch_s_data = <bp_data>.

    ENDLOOP.


  ENDMETHOD.

  METHOD alv_set_icon_type.

    "Set Icon type

    "I Inform
    READ TABLE im_t_return WITH KEY type = 'I' ASSIGNING FIELD-SYMBOL(<return>).
    IF sy-subrc IS INITIAL.
      ex_v_status_icon = icon_information.
      ex_v_message     = <return>-message.
    ENDIF.

    "S Success
    READ TABLE im_t_return WITH KEY type = 'S' ASSIGNING <return>.
    IF sy-subrc IS INITIAL.
      ex_v_status_icon = icon_led_green.
      ex_v_message     = <return>-message.
    ENDIF.

    "W Warning
    READ TABLE im_t_return WITH KEY type = 'W' ASSIGNING <return>.
    IF sy-subrc IS INITIAL.
      ex_v_status_icon  = icon_led_yellow.
      ex_v_message      = <return>-message.
    ENDIF.

    "E Error
    READ TABLE im_t_return WITH KEY type = 'E' ASSIGNING <return>.
    IF sy-subrc IS INITIAL.
      ex_v_status_icon  = icon_led_red.
      ex_v_message      = <return>-message.
    ENDIF.

    "A Abort cancel.
    READ TABLE im_t_return WITH KEY type = 'A' ASSIGNING <return>.
    IF sy-subrc IS INITIAL.
      ex_v_status_icon  = icon_message_critical.
      ex_v_message      = <return>-message.
    ENDIF.

  ENDMETHOD.

  METHOD call_bp_maintain.

    DATA:
      ls_data       TYPE cvis_ei_extern,
      lt_data       TYPE cvis_ei_extern_t,
      lt_return_map TYPE mdg_bs_bp_msgmap_t,
      lt_return     TYPE bapiretm,
      lt_return_bp  TYPE bapiret2_t.

    ls_data-partner                           = ch_s_data-partner_bus.
    ls_data-vendor                            = ch_s_data-vendor_vmds.
    ls_data-vendor-purchasing_data-purchasing = ch_s_data-vendor_vmds-purchasing_data-purchasing.

    IF ch_s_data-vendor IS INITIAL.
      ls_data-ensure_create-create_vendor       = abap_true.
    ENDIF.

    IF me->gv_test EQ abap_on.

      CALL METHOD cl_md_bp_maintain=>validate_single
        EXPORTING
          i_data        = ls_data
        IMPORTING
          et_return_map = lt_return_map.

      IF lt_return_map IS INITIAL.
        APPEND INITIAL LINE TO ch_s_data-return_bp ASSIGNING FIELD-SYMBOL(<data_return>).
        MESSAGE s036(z_bupa) INTO <data_return>-message. " Verification without errors! the BP will be created.
        <data_return>-id     = 'Z_BUPA'.
        <data_return>-type   = 'S'.
        <data_return>-number = '036'.
      ELSE.
        MOVE-CORRESPONDING lt_return_map TO lt_return_bp.
        APPEND LINES OF lt_return_bp TO ch_s_data-return_bp.

      ENDIF.

    ELSE.

      APPEND ls_data TO lt_data.

      CALL METHOD cl_md_bp_maintain=>maintain
        EXPORTING
          i_data   = lt_data      " Inbound for Customer/Vendor Integration
        IMPORTING
          e_return = lt_return.   " BAPIRETI Table Type for Multiple Objects

      READ TABLE lt_return ASSIGNING FIELD-SYMBOL(<return>) INDEX 1.

      " Error or Warning Found!!
      IF syst-subrc IS INITIAL.
        LOOP AT <return>-object_msg ASSIGNING FIELD-SYMBOL(<object_msg>).
          APPEND INITIAL LINE TO ch_s_data-return_bp ASSIGNING <data_return>.
          MOVE-CORRESPONDING <object_msg> TO <data_return>.

          IF <object_msg>-type CA 'AE'. "Error Found!!!!
            CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
          ELSE.

            CALL METHOD me->call_commit_and_check
              CHANGING
                ch_s_data = ch_s_data.
          ENDIF.
        ENDLOOP.

        CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.

      ELSE.

        CALL METHOD me->call_commit_and_check
          CHANGING
            ch_s_data = ch_s_data.

      ENDIF.

    ENDIF.

    alv_set_icon_type(
      EXPORTING
        im_t_return      = ch_s_data-return_bp
      IMPORTING
        ex_v_status_icon = ch_s_data-status_bp
        ex_v_message     = ch_s_data-message    ).



  ENDMETHOD.

  METHOD alv_create.

    DATA:
      lo_events                  TYPE REF TO cl_salv_events_table,
      lo_alv_display_settings    TYPE REF TO cl_salv_display_settings,
      lo_alv_functional_settings TYPE REF TO cl_salv_functional_settings,
      lo_alv_selections          TYPE REF TO cl_salv_selections,
      lo_functions_list          TYPE REF TO cl_salv_functions_list,
      lo_alv_top_of_list         TYPE REF TO cl_salv_form_layout_grid,
      lo_logo                    TYPE REF TO cl_salv_form_layout_logo.

    TRY.

        CREATE OBJECT:
         lo_alv_top_of_list,
         lo_logo.

        " Create object ALV TABLE
        CALL METHOD cl_salv_table=>factory
          IMPORTING
            r_salv_table = me->go_alv_table " Basis Class Simple ALV Tables
          CHANGING
            t_table      = me->gt_alv.

        me->go_alv_table->get_functions( )->set_all( abap_true )..

        " Set Display Settings
        me->go_alv_table->get_display_settings( )->set_horizontal_lines( abap_on ).
        me->go_alv_table->get_display_settings( )->set_striped_pattern( abap_on ).

        " Set Functional Settings
        me->go_alv_table->get_functional_settings( )->set_sort_on_header_click( abap_on ).
        me->go_alv_table->get_selections( )->set_selection_mode( if_salv_c_selection_mode=>single ).

        " Set Columns Parameters
        me->alv_set_columns( ).

        " Header Top Of Page
        me->go_alv_table->set_top_of_list( value = lo_alv_top_of_list ).
        lo_alv_top_of_list->create_label( row = 1 column  = 1 text = TEXT-t05  ). "MBO

        IF me->gv_test EQ abap_on.
          lo_alv_top_of_list->create_label( row = 2 column  = 1 text = 'Execução de TESTE'(005)  ).
        ELSE.
          lo_alv_top_of_list->create_label( row = 2 column  = 1 text = 'Execução de efetiva'(006)  ).
        ENDIF.

        lo_alv_top_of_list->create_flow( row = 3 column  = 1 )->create_label( text = 'Usuário:'(016) ).
        lo_alv_top_of_list->create_flow( row = 3 column  = 2 )->create_text(  text = |{ syst-uname }| ).
        lo_alv_top_of_list->create_flow( row = 4 column  = 1 )->create_label( text = |{ 'Data de execução:'(007) }| ).
        lo_alv_top_of_list->create_flow( row = 4 column  = 2 )->create_text(  text = |{ sy-datlo DATE = USER } { syst-uzeit TIME = USER }| ).

* set left content
        lo_logo->set_left_content( lo_alv_top_of_list ).

* set Right Image
        lo_logo->set_right_logo( 'ZTAESA_LOGO_ALV' ).

*   set the top of list using the header for Online.
        me->go_alv_table->set_top_of_list( lo_logo ).

        "events
        lo_events = me->go_alv_table->get_event( ).
        SET HANDLER me->on_alv_link_click       FOR lo_events.
        SET HANDLER me->on_alv_double_click     FOR lo_events.
        SET HANDLER me->on_alv_user_command     FOR lo_events.

      CATCH cx_root.
        " Deu ruim!

    ENDTRY.

  ENDMETHOD.

  METHOD alv_set_columns.
    DATA:
      lo_alv_columns TYPE REF TO cl_salv_columns,
      lo_alv_colum   TYPE REF TO cl_salv_column_table,
      lt_components  TYPE cl_abap_structdescr=>component_table,
      lt_ddfields    TYPE ddfields.

    TRY.
        lo_alv_columns = me->go_alv_table->get_columns( ).
        lo_alv_columns->set_optimize( abap_true ).

        zcl_utils=>get_field_list(
          EXPORTING
            im_t_table      = me->gt_alv
          IMPORTING
            ex_t_components = lt_components
          RECEIVING
            r_result        = lt_ddfields        ).

        LOOP AT lt_components ASSIGNING FIELD-SYMBOL(<component>).

          lo_alv_colum ?= lo_alv_columns->get_column( columnname = |{ <component>-name }| ).
          lo_alv_colum->set_alignment( value = if_salv_c_alignment=>centered ).

          CASE <component>-name.

            WHEN 'TAXNUM'.
              lo_alv_colum->set_cell_type( if_salv_c_cell_type=>hotspot ).
              lo_alv_colum->set_key( abap_on ).

            WHEN 'STATUS_BP' OR 'STATUS_DOC'.
              lo_alv_colum->set_cell_type( if_salv_c_cell_type=>hotspot ).
              lo_alv_colum->set_icon( abap_on ).
              lo_alv_colum->set_long_text( 'Log Erro' ).
              lo_alv_colum->set_medium_text( 'Log Erro' ).
              lo_alv_colum->set_short_text( 'Log Erro' ).

            WHEN 'PARTNER'.
              lo_alv_colum->set_cell_type( if_salv_c_cell_type=>hotspot ).

            WHEN 'VENDOR'.
              lo_alv_colum->set_cell_type( if_salv_c_cell_type=>hotspot ).

            WHEN 'BELNR'.
              lo_alv_colum->set_cell_type( if_salv_c_cell_type=>hotspot ).

            WHEN 'BELNR_KEY'.
              lo_alv_colum->set_technical( value = if_salv_c_bool_sap=>true ).

            WHEN OTHERS.

          ENDCASE.

        ENDLOOP.
      CATCH cx_salv_not_found. " ALV: General Error Class (Checked During Syntax Check)
      CATCH cx_root.
    ENDTRY.

  ENDMETHOD.

  METHOD on_alv_double_click.

  ENDMETHOD.

  METHOD on_alv_link_click.

    READ TABLE me->gt_bp_data INDEX row ASSIGNING FIELD-SYMBOL(<row_alv>).

    CASE column.

      WHEN 'VENDOR' OR 'PARTNER'.

        CHECK <row_alv>-partner IS NOT INITIAL.

        SET PARAMETER ID 'BPA' FIELD <row_alv>-partner.
        CALL TRANSACTION 'BP' WITH AUTHORITY-CHECK.

      WHEN 'STATUS_BP'.

        me->msg_show(
            EXPORTING
            im_t_return = <row_alv>-return_bp
            im_v_zeile = |BP{ row }|  ).

    ENDCASE.

  ENDMETHOD.

  METHOD on_alv_user_command.

  ENDMETHOD.


  METHOD get_bp_from_taxnumber_lc.

    DATA:
      lt_bp_key      TYPE zcl_bp_standard=>ty_t_bp_key,
      lt_get_bp_data TYPE zcl_bp_standard=>ty_t_bp,
      lt_select_data TYPE zif_bp_standard=>ty_t_bp_key,
      lr_taxnum      TYPE RANGE OF bptaxnum.

    " Fill Range of Tax Number
    lr_taxnum = VALUE #( FOR ls_general_data IN im_t_upload_file
                      ( sign    = 'I'
                        option  = 'EQ'
                        low     = ls_general_data-taxnum ) ).


    SORT lr_taxnum ASCENDING BY low.
    DELETE ADJACENT DUPLICATES FROM lr_taxnum.

    SELECT DISTINCT
            but~partner,
            but~partner_guid,
            bptaxnum~taxnum,
            vendor,
            customer
        FROM dfkkbptaxnum       AS bptaxnum
        LEFT JOIN but000        AS but      ON bptaxnum~partner  = but~partner
        LEFT JOIN cvi_vend_link AS vend     ON vend~partner_guid = but~partner_guid
        LEFT JOIN cvi_cust_link AS cust     ON cust~partner_guid = but~partner_guid
        INTO TABLE @lt_select_data
        WHERE bptaxnum~taxnum IN @lr_taxnum.

    LOOP AT lr_taxnum ASSIGNING FIELD-SYMBOL(<lr_taxnum>).

      APPEND INITIAL LINE TO:
        ch_t_bp_data ASSIGNING FIELD-SYMBOL(<bp_data>).

      <bp_data>-taxnum = <lr_taxnum>-low.

      APPEND INITIAL LINE TO <bp_data>-return_bp ASSIGNING FIELD-SYMBOL(<return>).

      " Check if exist in DB
      READ TABLE lt_select_data WITH KEY taxnum = <lr_taxnum>-low ASSIGNING FIELD-SYMBOL(<select_data>).

      IF syst-subrc IS INITIAL.

        MOVE-CORRESPONDING <select_data> TO <bp_data>.

        APPEND INITIAL LINE TO lt_bp_key ASSIGNING FIELD-SYMBOL(<ls_bp_key>).
        MOVE-CORRESPONDING <bp_data> TO <ls_bp_key>.

        TRY.
            zcl_bp_extend_vendor=>get_bp_data_from_vendor(
              EXPORTING
                im_t_bp_key  = lt_bp_key
              IMPORTING
                ex_t_bp_data = lt_get_bp_data            ).

            <bp_data>-partner_bus = lt_get_bp_data[ 1 ]-partner_bus.
            <bp_data>-vendor_vmds = lt_get_bp_data[ 1 ]-vendor_vmds.

          CATCH zcx_abap_error. " Exception Class with its own Constructor
        ENDTRY.

        " O PN &1 ( &2 ) já cadastrado.
        MESSAGE s037(z_bupa) WITH <bp_data>-partner <bp_data>-vendor INTO <return>-message.
        <bp_data>-message   = <return>-message.
        <bp_data>-status_bp = icon_led_green.


      ELSE. " Not exist in DB

        MESSAGE i201(r1) INTO <return>-message.

        <bp_data>-message   = <return>-message.
        <bp_data>-status_bp = icon_led_red.

      ENDIF.

      <return>-id         = syst-msgid. " Message Class
      <return>-type       = syst-msgty. " Message type: S Success, E Error, W Warning, I Info, A Abort
      <return>-number     = syst-msgno. " Message Number
      <return>-message_v1 = syst-msgv1. " Message Variable
      <return>-message_v2 = syst-msgv2. " Message Variable

      LOOP AT im_t_upload_file
        ASSIGNING FIELD-SYMBOL(<general_data>) USING KEY taxnum_key WHERE taxnum = <bp_data>-taxnum.
        <bp_data>-general_data = <general_data>.
      ENDLOOP.

    ENDLOOP.

  ENDMETHOD.


  METHOD alv_display.

    MOVE-CORRESPONDING me->gt_bp_data TO me->gt_alv.
    me->go_alv_table->display( ).

  ENDMETHOD.



  METHOD convert_cpf_cnpj.
    DATA:
      lv_taxnum TYPE bptaxnum.

    CALL FUNCTION 'CONVERSION_EXIT_CPFBR_INPUT'
      EXPORTING
        input     = ch_v_taxnum   " CPF in screen format (999.999.999-99)
      IMPORTING
        output    = lv_taxnum  " CPF in internal format (NUMC 11)
      EXCEPTIONS
        not_valid = 1
        OTHERS    = 2.
    IF syst-subrc IS INITIAL.
      ch_v_taxnum = lv_taxnum.
    ELSE. " CNPJ ?
      CALL FUNCTION 'CONVERSION_EXIT_CGCBR_INPUT'
        EXPORTING
          input     = ch_v_taxnum " CGC in screen format (99.999.999/9999-99)
        IMPORTING
          output    = lv_taxnum             " CGC in internal format (NUMC 14)
        EXCEPTIONS
          not_valid = 1
          OTHERS    = 2.
      IF sy-subrc EQ 0.
        ch_v_taxnum = lv_taxnum.
      ENDIF.
    ENDIF.

  ENDMETHOD.

  METHOD convert_field_date.
    DATA:
     lv_cell_date  TYPE zexcel_cell_value.

    DO.
      TRY.
          ASSIGN COMPONENT syst-index OF STRUCTURE ch_v_struc TO FIELD-SYMBOL(<fs_comp>).
          IF sy-subrc NE 0.
            EXIT.
          ELSE.
            DESCRIBE FIELD <fs_comp> TYPE DATA(lv_desc_field).
            IF lv_desc_field EQ 'D'.

              IF <fs_comp> IS NOT INITIAL.

                lv_cell_date = <fs_comp>.

                CALL METHOD zcl_excel_common=>excel_string_to_date
                  EXPORTING
                    ip_value = lv_cell_date
                  RECEIVING
                    ep_value = <fs_comp>.
              ELSE.
                <fs_comp> = syst-datum.
              ENDIF.
            ENDIF.
          ENDIF.

        CATCH zcx_excel. " Exceptions for ABAP2XLSX
        CATCH cx_root.
      ENDTRY.
    ENDDO.

  ENDMETHOD.

  METHOD call_commit_and_check.

    " commit work and wait. " DUMP!
    CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
      EXPORTING
        wait = abap_true.

    " Get BP Number and Vendor
    SELECT SINGLE but~partner vend~vendor
    INTO CORRESPONDING FIELDS OF ch_s_data
      FROM but000 AS but
      LEFT JOIN cvi_vend_link AS vend ON but~partner_guid = vend~partner_guid
      WHERE but~partner_guid = ch_s_data-partner_guid .

    APPEND INITIAL LINE TO ch_s_data-return_bp ASSIGNING FIELD-SYMBOL(<data_return>).

    IF ch_s_data-vendor IS NOT INITIAL.
      " Business Partner &1 &2 successfully created!
      MESSAGE s038(z_bupa) WITH ch_s_data-partner ch_s_data-vendor INTO <data_return>-message.
      ch_s_data-message        = <data_return>-message.
      ch_s_data-status_bp      = icon_led_green.
    ELSE.
      " PN &1 criado com sucesso! Visão fornecedor não criado, Ver MDS_PPO2!
      MESSAGE w039(z_bupa) WITH ch_s_data-partner ch_s_data-vendor INTO <data_return>-message.
      ch_s_data-message       = <data_return>-message.
      ch_s_data-status_bp     = icon_led_yellow.
    ENDIF.
    <data_return>-id         = syst-msgid.
    <data_return>-type       = syst-msgty.
    <data_return>-number     = syst-msgno.
    <data_return>-message_v1 = syst-msgv1.
    <data_return>-message_v2 = syst-msgv2.

  ENDMETHOD.


  METHOD extend_vendor.

    DATA:
      lo_extend_vendor TYPE REF TO zcl_bp_extend_vendor,
      lt_bp_key        TYPE zcl_bp_standard=>ty_t_bp_key,
      ls_bp_key        TYPE zcl_bp_standard=>ty_s_bp_key,
      lt_bp_data       TYPE zcl_bp_extend_vendor=>ty_t_bp_extend,
      lt_get_bp_data   TYPE zcl_bp_standard=>ty_t_bp,
*      ls_bp_data       TYPE zcl_bp_extend_vendor=>ty_s_bp_extend,
      lt_bukrs_for     TYPE zif_bp_standard~ty_t_t001_key,
      lt_extend_return TYPE bapiretm,                       " BAPIRETI Table Type for Multiple Objects
      lv_bukrs_from    TYPE bukrs.


    SELECT SINGLE bukrs
        FROM lfb1
        INTO lv_bukrs_from
        WHERE lifnr = ch_s_data-vendor.

    " Takes all companies that have not been extended at Vendor
    SELECT t001~bukrs                     " #EC CI_NO_TRANSFORM
        FROM t001 AS t001
        INTO TABLE @lt_bukrs_for
        FOR ALL ENTRIES IN  @gt_bukrs
        WHERE
            t001~bukrs = @gt_bukrs-bukrs AND
            t001~bukrs NOT IN ( SELECT bukrs FROM lfb1 WHERE lifnr = @ch_s_data-vendor ).

    CHECK syst-subrc IS INITIAL. " There are no companies to extend

    MOVE-CORRESPONDING ch_s_data TO ls_bp_key.
    APPEND ls_bp_key TO lt_bp_key.

    TRY.

        zcl_bp_extend_vendor=>get_bp_data_from_vendor(
          EXPORTING
            im_t_bp_key  = lt_bp_key
          IMPORTING
            ex_t_bp_data = lt_get_bp_data            ).

        MOVE-CORRESPONDING lt_get_bp_data TO lt_bp_data.

        CREATE OBJECT lo_extend_vendor
          EXPORTING
            im_v_bukrs_from = lv_bukrs_from
            im_t_bukrs_for  = lt_bukrs_for
            im_t_bp_data    = lt_bp_data.

        lo_extend_vendor->extend_vendor(
            EXPORTING
              im_b_test   = me->gv_test       " Test Flag
            IMPORTING
              ex_t_return = lt_extend_return  " Return bapiretm
        ).

        LOOP AT lt_extend_return ASSIGNING FIELD-SYMBOL(<extend_return>).

          LOOP AT <extend_return>-object_msg ASSIGNING FIELD-SYMBOL(<object_msg>).

            IF <object_msg>-type CA 'AE'.
              DATA(lv_error) = abap_true.
            ENDIF.

            APPEND INITIAL LINE TO ch_s_data-return_bp ASSIGNING FIELD-SYMBOL(<return_bp>).

            MOVE-CORRESPONDING <object_msg> TO <return_bp>.

          ENDLOOP.

        ENDLOOP.

        IF me->gv_test IS NOT INITIAL.

          IF lv_error EQ abap_true.
            " Simulação de extensão para o fornecedor &1 com erros!
            MESSAGE e033(z_bupa) WITH ch_s_data-vendor INTO DATA(lv_msgdummy).

          ELSE.
            " Simulação de extensão para o fornecedor &1 realizada com sucesso!
            MESSAGE s032(z_bupa) WITH ch_s_data-vendor INTO lv_msgdummy.
          ENDIF.

        ELSE.

          LOOP AT lt_bukrs_for ASSIGNING FIELD-SYMBOL(<bukrs_extended>).
            " Check If Company exists for vendor
            SELECT SINGLE bukrs FROM lfb1 BYPASSING BUFFER
                INTO @DATA(lv_bukrs)
                WHERE lifnr = @ch_s_data-vendor AND
                      bukrs = @<bukrs_extended>-bukrs.

            IF syst-subrc IS INITIAL.
              " Fornecedor &1 foi estendido para a empresa &2 com sucesso.
              MESSAGE s028(z_bupa) WITH ch_s_data-vendor <bukrs_extended>-bukrs INTO lv_msgdummy.
            ELSE.
              " ERRO ao estender Fornecedor &1 para a empresa &2. Ver MDS_PPO2!
              MESSAGE e029(z_bupa) WITH ch_s_data-vendor <bukrs_extended>-bukrs INTO lv_msgdummy.
            ENDIF.

          ENDLOOP.

        ENDIF.

        APPEND INITIAL LINE TO ch_s_data-return_bp ASSIGNING <return_bp>.
        <return_bp>-id         = sy-msgid.
        <return_bp>-type       = sy-msgty.
        <return_bp>-number     = sy-msgno.
        <return_bp>-message_v1 = sy-msgv1.
        <return_bp>-message_v2 = sy-msgv2.
        <return_bp>-message    = lv_msgdummy.

      CATCH zcx_abap_error. " Exception Class with its own Constructor
    ENDTRY.

  ENDMETHOD.

  METHOD msg_initialize.
    DATA:
    lv_identification TYPE syst-uzeit.

*    Initialize message store.
*    CALL FUNCTION 'MESSAGES_ACTIVE'
*      EXCEPTIONS
*        not_active = 1
*        OTHERS     = 2.
*    IF sy-subrc IS NOT INITIAL.
    CALL FUNCTION 'MESSAGES_INITIALIZE'
      EXPORTING
        reset                = abap_on            " Reset collected messages if x
      IMPORTING
        e_identification     = lv_identification  " Identification
      EXCEPTIONS
        log_not_active       = 1                  " Log not active -> no reset
        wrong_identification = 2                  " Identification not correct (-> long text)
        OTHERS               = 3.
*    ENDIF.

    IF sy-subrc NE 0.
      MESSAGE ID syst-msgid TYPE 'S' NUMBER syst-msgno
            WITH syst-msgv1 syst-msgv2 syst-msgv3 syst-msgv4 DISPLAY LIKE 'E'.
    ENDIF.
  ENDMETHOD.


  METHOD msg_show.

    CALL METHOD me->msg_initialize.

    LOOP AT im_t_return ASSIGNING FIELD-SYMBOL(<return>).

      CALL FUNCTION 'MESSAGE_STORE'
        EXPORTING
          exception_if_not_active = abap_true               " X = exception not_active is initialized if
          arbgb                   = <return>-id          " Message ID
          msgty                   = <return>-type        " Type of message (I, S, W, E, A)
          msgv1                   = <return>-message_v1  " First variable parameter of message
          msgv2                   = <return>-message_v2  " Second variable parameter of message
          msgv3                   = <return>-message_v3  " Third variable parameter of message
          msgv4                   = <return>-message_v4  " Fourth variable parameter of message
          txtnr                   = <return>-number      " Message Number
          zeile                   = im_v_zeile              " Reference line (if it exists)
        EXCEPTIONS
          message_type_not_valid  = 1                " Type of message not I, S, W, E or A
          not_active              = 2                " Collection of messages not activated
          OTHERS                  = 3.
      IF sy-subrc IS NOT INITIAL.
        MESSAGE ID sy-msgid TYPE 'S' NUMBER sy-msgno
          WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 DISPLAY LIKE 'E'.
      ENDIF.

    ENDLOOP.

    CALL FUNCTION 'MESSAGES_SHOW'
      EXPORTING
*       line_from          = lv_line          " Only show messages with longer reference line
*       line_to            = lv_line          " Only show messages with shorter reference line
        batch_list_type    = 'J'              " J = job log / L = in spool list / B = both
*       show_linno_text    = 'Cliente'   " Column header for row
*       show_linno_text_len = '3'              " Column width for row in display
        i_use_grid         = abap_true        " Use ALV Grid for Display; Otherwise Classic ALV
      EXCEPTIONS
        inconsistent_range = 1                " LINE_TO is shorter than LINE_FROM
        no_messages        = 2                " No messages in required interval
        OTHERS             = 3.
    IF sy-subrc NE 0.
      MESSAGE ID sy-msgid TYPE 'S' NUMBER sy-msgno DISPLAY LIKE 'E'
        WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.

  ENDMETHOD.

  METHOD fill_bank_key.

    DATA:
      lv_string TYPE c LENGTH 3,
      lv_length TYPE i,
      lv_mult1  TYPE n,
      lv_mult2  TYPE n VALUE 4,
      lv_prod   TYPE n LENGTH 2,
      lv_addi   TYPE p VALUE 0,
      lv_rest   TYPE p,
      lv_dv     TYPE n.

    lv_string = im_s_general_data-banco.

    lv_length = strlen( lv_string ).

    DO lv_length TIMES.

      WRITE lv_string(1) TO lv_mult1.

      lv_prod = lv_mult1 * lv_mult2.

      ADD lv_prod TO lv_addi.

      SUBTRACT 1 FROM lv_mult2.
      SHIFT lv_string.              " delete first char
    ENDDO.

    lv_rest = lv_addi MOD 11.

    lv_dv = 11 - lv_rest.

    r_result =  |{ im_s_general_data-banco }{ lv_dv }{ im_s_general_data-bankl }|.

  ENDMETHOD.


  METHOD update_vendor.

    DATA:
      ls_company    TYPE vmds_ei_company,
      ls_purchasing TYPE vmds_ei_purchasing.

    " Assigning fields symbols
    ASSIGN:
        ch_s_data-partner_bus-central_data-address-addresses[ data-postal-data-standardaddress = abap_true ] TO FIELD-SYMBOL(<ls_addresses>),
        ch_s_data-general_data TO FIELD-SYMBOL(<general_data>).

    " Add Insert task to all fields
    MOVE me->co_task-update TO:
        ch_s_data-partner_bus-header-object_task,
        ch_s_data-vendor_vmds-header-object_task,
        ls_company-task,
        ls_purchasing-task,
        <ls_addresses>-task.

    " Ajust ROLES Update

    " Set fields , avoid errors AUTHORITY-CHECK
    CLEAR:
        ch_s_data-partner_bus-central_data-role-time_dependent,
        ch_s_data-partner_bus-central_data-role-current_state.

*>>>>>>>>>>>>>>>>>>>>>>>>>>>> C E N T R A L  D A T A <<<<<<<<<<<<<<<<<<<<<<<<<<<<
    fill_organization(
      EXPORTING
        im_s_general_data    = <general_data>
      CHANGING
        ch_s_bp_organization = ch_s_data-partner_bus-central_data-common-data-bp_organization ).

    IF p_test IS NOT INITIAL.
      <ls_addresses>-data-postal-data-c_o_name = me->fill_searchterm( |{ <general_data>-name }| ).
    ENDIF.

    " Avoid Erros, is filled automatically
    CLEAR <ls_addresses>-data-postal-data-taxjurcode.

    " Search term 1 for business partner
    IF ch_s_data-general_data-serchterm IS NOT INITIAL.
      ch_s_data-partner_bus-central_data-common-data-bp_centraldata-searchterm1 = ch_s_data-general_data-serchterm.
    ELSE.
      ch_s_data-partner_bus-central_data-common-data-bp_centraldata-searchterm1 = me->fill_searchterm( |{ <general_data>-name }| ).
    ENDIF.

*>>>>>>>>>>>>>>>>>>>>>>>>>>>> A D D R E S S E S <<<<<<<<<<<<<<<<<<<<<<<<<<<<
    MOVE-CORRESPONDING <general_data> TO <ls_addresses>-data-postal-data.

    " Telephone
    IF <general_data>-tel_number IS NOT INITIAL AND
        NOT line_exists(
            ch_s_data-partner_bus-central_data-communication-phone-phone[ contact-data-telephone = <general_data>-tel_number ] ).

      APPEND INITIAL LINE TO
        ch_s_data-partner_bus-central_data-communication-phone-phone ASSIGNING FIELD-SYMBOL(<phone>).
      <phone>-contact-task           = me->co_task-insert. " I
      <phone>-contact-data-country   = me->co_country_iso_br.
      <phone>-contact-data-telephone = <general_data>-tel_number.
      <phone>-contact-data-r_3_user  = '1'. " Default
    ENDIF.

    " Email
    IF <general_data>-smtp_addr IS NOT INITIAL AND
        NOT line_exists(
            ch_s_data-partner_bus-central_data-communication-smtp-smtp[ contact-data-e_mail = <general_data>-smtp_addr ] ).

      APPEND INITIAL LINE TO
        ch_s_data-partner_bus-central_data-communication-smtp-smtp ASSIGNING FIELD-SYMBOL(<email>).
      <email>-contact-task        = me->co_task-insert. " I.
      <email>-contact-data-e_mail = <general_data>-smtp_addr.
    ENDIF.


*>>>>>>>>>>>>>>>>>>>>>>>>>>>> B A N K S  <<<<<<<<<<<<<<<<<<<<<<<<<<<<*
    " If the bank account already exists, do nothing, if different or none exist then create the account
    IF <general_data>-banco IS NOT INITIAL AND
      NOT line_exists(
        ch_s_data-partner_bus-central_data-bankdetail-bankdetails[ data-bank_key  = me->fill_bank_key( <general_data> )
                                                                   data-bank_acct = <general_data>-bankn ] ).

      APPEND INITIAL LINE TO ch_s_data-partner_bus-central_data-bankdetail-bankdetails ASSIGNING FIELD-SYMBOL(<ls_bankdetails>).

      <ls_bankdetails>-task           = me->co_task-update. " I.
      <ls_bankdetails>-data-bank_ctry = 'BR'.
      <ls_bankdetails>-data-bank_key  = me->fill_bank_key( <general_data> ).
      <ls_bankdetails>-data-bank_acct = <general_data>-bankn.     " Bank account number
      <ls_bankdetails>-data-ctrl_key  = <general_data>-ctrl_key.  " Bank Control Key

      IF me->gv_test IS NOT INITIAL.
        <ls_bankdetails>-data_key     = <general_data>-banco. " Avoid msg erros in test
      ENDIF.

    ENDIF.

*>>>>>>>>>>>>>>>>>>>>>>>> T A X  N U M B E R S <<<<<<<<<<<<<<<<<<<<<<*

    " Inscrição Estadual, check if is filled
    IF ch_s_data-general_data-insc_est IS NOT INITIAL.

      " Checks if it already exists
      ASSIGN ch_s_data-partner_bus-central_data-taxnumber-taxnumbers[
                      data_key-taxtype = me->co_taxtype-ins_stadual ] TO FIELD-SYMBOL(<ls_insc_estadual>).

      IF syst-subrc IS INITIAL.

        " Checks if is not equal, if is equal do nothing
        IF <ls_insc_estadual>-data_key-taxnumber NE ch_s_data-general_data-insc_est.
          <ls_insc_estadual>-task                 = me->co_task-update.
          <ls_insc_estadual>-data_key-taxnumber   = ch_s_data-general_data-insc_est.
          <ls_insc_estadual>-data_key-taxtype     = me->co_taxtype-ins_stadual.
        ENDIF.

      ELSE. " Not exist and insert
        APPEND INITIAL LINE TO ch_s_data-partner_bus-central_data-taxnumber-taxnumbers ASSIGNING <ls_insc_estadual>.

        <ls_insc_estadual>-task                 = me->co_task-insert.
        <ls_insc_estadual>-data_key-taxnumber   = ch_s_data-general_data-insc_est.
        <ls_insc_estadual>-data_key-taxtype     = me->co_taxtype-ins_stadual.
      ENDIF.

    ENDIF.

    " Inscrição Municipal, check if is filled
    IF ch_s_data-general_data-insc_mun IS NOT INITIAL.

      " Checks if it already exists
      ASSIGN ch_s_data-partner_bus-central_data-taxnumber-taxnumbers[
                      data_key-taxtype = me->co_taxtype-ins_municipal ] TO FIELD-SYMBOL(<ls_insc_municipal>).

      IF syst-subrc IS INITIAL.

        " Checks if is not equal, if is equal do nothing
        IF <ls_insc_estadual>-data_key-taxnumber NE ch_s_data-general_data-insc_est.
          <ls_insc_municipal>-task                = me->co_task-update.
          <ls_insc_municipal>-data_key-taxnumber  = ch_s_data-general_data-insc_mun.
          <ls_insc_municipal>-data_key-taxtype    = me->co_taxtype-ins_municipal.
        ENDIF.

      ELSE. " Not exist and insert
        APPEND INITIAL LINE TO ch_s_data-partner_bus-central_data-taxnumber-taxnumbers ASSIGNING <ls_insc_municipal>.

        <ls_insc_municipal>-task                = me->co_task-insert.
        <ls_insc_municipal>-data_key-taxnumber  = ch_s_data-general_data-insc_mun.
        <ls_insc_municipal>-data_key-taxtype    = me->co_taxtype-ins_municipal.
      ENDIF.

    ENDIF.

*>>>>>>>>>>>>>>>>>>>>>>>>>>>> C O M P A N Y <<<<<<<<<<<<<<<<<<<<<<<<<*

    MOVE-CORRESPONDING ch_s_data-general_data TO ch_s_data-vendor_vmds-central_data-central-data.

    MODIFY ch_s_data-vendor_vmds-company_data-company       FROM ls_company     TRANSPORTING task WHERE task IS INITIAL.
    MODIFY ch_s_data-vendor_vmds-purchasing_data-purchasing FROM ls_purchasing  TRANSPORTING task WHERE task IS INITIAL.

*>>>>>>>>>>>>>>>>>>>>>> C O M P A R E  D A T A  <<<<<<<<<<<<<<<<<<<<*
    me->compare_bp_source_fill_datax(
      CHANGING
        ch_s_source = ch_s_data    ).

*>>>>>>>>>>>>>>>>>>>>>> C A L L  M A I N T A I N <<<<<<<<<<<<<<<<<<<*
    CALL METHOD me->call_bp_maintain
      CHANGING
        ch_s_data = ch_s_data.

  ENDMETHOD.

*&----------------------------------------------------------------------*
*& METHOD COMPARE_BP_SOURCE_FILL_DATAX
*&----------------------------------------------------------------------*
  METHOD compare_bp_source_fill_datax.

    DATA:
      lt_bp_key          TYPE zif_bp_standard=>ty_t_bp_key,
      lt_bp_data_current TYPE zif_bp_standard=>ty_t_bp,
      ls_bp_current      TYPE bus_ei_main.

*----------------------------------------------------------------------
* Get current recorded BP data
*----------------------------------------------------------------------

    APPEND INITIAL LINE TO lt_bp_key ASSIGNING FIELD-SYMBOL(<ls_bp_key>).
    MOVE-CORRESPONDING ch_s_source TO <ls_bp_key>.

    zcl_bp_extend_vendor=>get_bp_data_from_vendor(
      EXPORTING
        im_t_bp_key  = lt_bp_key
      IMPORTING
        ex_t_bp_data = lt_bp_data_current   ).

    ASSIGN lt_bp_data_current[ 1 ] TO FIELD-SYMBOL(<ls_bp_current>).

*----------------------------------------------------------------------
* Check change in central_data
*----------------------------------------------------------------------
    CALL METHOD me->compare_data
      EXPORTING
        im_s_source  = ch_s_source-partner_bus-central_data-common-data-bp_centraldata
        im_s_current = <ls_bp_current>-partner_bus-central_data-common-data-bp_centraldata
      CHANGING
        ch_s_data_x  = ch_s_source-partner_bus-central_data-common-datax-bp_centraldata.

*----------------------------------------------------------------------
* Check for change in bp_organization
*----------------------------------------------------------------------
    CALL METHOD me->compare_data
      EXPORTING
        im_s_source  = ch_s_source-partner_bus-central_data-common-data-bp_organization
        im_s_current = <ls_bp_current>-partner_bus-central_data-common-data-bp_organization
      CHANGING
        ch_s_data_x  = ch_s_source-partner_bus-central_data-common-datax-bp_organization.

    " Avoid Error
    IF ch_s_source-partner_bus-central_data-common-datax-bp_organization IS INITIAL.
      ch_s_source-partner_bus-central_data-common-datax-bp_organization-name1 = abap_on.
    ENDIF.

*----------------------------------------------------------------------
* Verify change address
*----------------------------------------------------------------------
    ASSIGN <ls_bp_current>-partner_bus-central_data-address-addresses[ 1 ]  TO FIELD-SYMBOL(<ls_adress_current>).
    ASSIGN ch_s_source-partner_bus-central_data-address-addresses[ 1 ]      TO FIELD-SYMBOL(<ls_adress_source>).

    IF <ls_adress_current> IS ASSIGNED AND <ls_adress_source> IS ASSIGNED.

      " Postal
      CALL METHOD me->compare_data
        EXPORTING
          im_s_source  = <ls_adress_source>-data-postal-data
          im_s_current = <ls_adress_current>-data-postal-data
        CHANGING
          ch_s_data_x  = <ls_adress_source>-data-postal-datax.

      IF <ls_adress_source>-data-postal-datax IS NOT INITIAL.
        <ls_adress_source>-data_key-guid = <ls_adress_current>-data_key-guid.
        <ls_adress_source>-task          = me->co_task-update.
      ENDIF.

    ENDIF.

*----------------------------------------------------------------------
* Check Insc State
*----------------------------------------------------------------------
    ASSIGN ch_s_source-partner_bus-central_data-taxnumber-taxnumbers[ data_key-taxtype = me->co_taxtype-ins_stadual ] TO FIELD-SYMBOL(<ls_source_insc_state>).

    IF line_exists( <ls_bp_current>-partner_bus-central_data-taxnumber-taxnumbers[ data_key-taxtype = me->co_taxtype-ins_stadual ] ) AND
        <ls_source_insc_state> IS ASSIGNED.
      <ls_source_insc_state>-task = me->co_task-update.
    ENDIF.

*----------------------------------------------------------------------
* Check Insc Municipal
*----------------------------------------------------------------------
    ASSIGN ch_s_source-partner_bus-central_data-taxnumber-taxnumbers[ data_key-taxtype = me->co_taxtype-ins_municipal ] TO FIELD-SYMBOL(<ls_source_ins_municipal>).

    IF line_exists( <ls_bp_current>-partner_bus-central_data-taxnumber-taxnumbers[ data_key-taxtype = me->co_taxtype-ins_municipal ] ) AND
       <ls_source_ins_municipal> IS ASSIGNED.
      <ls_source_ins_municipal>-task = me->co_task-update.
    ENDIF.

*----------------------------------------------------------------------
* Check change in Vendor central_data
*----------------------------------------------------------------------
    CALL METHOD me->compare_data
      EXPORTING
        im_s_source  = ch_s_source-vendor_vmds-central_data-central-data
        im_s_current = <ls_bp_current>-vendor_vmds-central_data-central-data
      CHANGING
        ch_s_data_x  = ch_s_source-vendor_vmds-central_data-central-datax.

*----------------------------------------------------------------------
* Check change in Vendor central_data
*----------------------------------------------------------------------
    IF line_exists( ch_s_source-vendor_vmds-company_data-company[ 1 ] ).
      CALL METHOD me->compare_data
        EXPORTING
          im_s_source  = ch_s_source-vendor_vmds-company_data-company[ 1 ]-data
          im_s_current = <ls_bp_current>-vendor_vmds-company_data-company[ 1 ]-data
        CHANGING
          ch_s_data_x  = ch_s_source-vendor_vmds-company_data-company[ 1 ]-datax.
    ENDIF.

  ENDMETHOD.

*&----------------------------------------------------------------------*
*&  METHOD COMPARE_DATA
*&----------------------------------------------------------------------*
  METHOD compare_data.

    DATA:
      lo_table_descr  TYPE REF TO cl_abap_tabledescr,
      lo_struct_descr TYPE REF TO cl_abap_structdescr,
      lo_data         TYPE REF TO data.

    " Clear all fields of change
    CLEAR ch_s_data_x.

    CREATE DATA lo_data LIKE im_s_current.

    lo_struct_descr ?= cl_abap_structdescr=>describe_by_data_ref( lo_data ).

    LOOP AT lo_struct_descr->components ASSIGNING FIELD-SYMBOL(<ls_field>).

      ASSIGN COMPONENT <ls_field>-name OF STRUCTURE im_s_current TO FIELD-SYMBOL(<lv_component_current>).
      ASSIGN COMPONENT <ls_field>-name OF STRUCTURE im_s_source  TO FIELD-SYMBOL(<lv_component_source>).
      ASSIGN COMPONENT <ls_field>-name OF STRUCTURE ch_s_data_x  TO FIELD-SYMBOL(<lv_component_x>).

      CHECK:
          <lv_component_current>   IS ASSIGNED,
          <lv_component_source>    IS ASSIGNED,
          <lv_component_x>         IS ASSIGNED.

      IF <lv_component_current> NE <lv_component_source>.
        <lv_component_x> = abap_true.
      ELSE.
        <lv_component_x> = abap_false.
      ENDIF.

      UNASSIGN:  <lv_component_current>, <lv_component_source>, <lv_component_x>.

    ENDLOOP.

  ENDMETHOD.

*&----------------------------------------------------------------------*
*& METHOD FILL_ORGANIZATION
*&----------------------------------------------------------------------*
  METHOD fill_organization.

    DATA:
      lt_text_tab TYPE TABLE OF char255,
      lv_text     TYPE string.

    ch_s_bp_organization-legalorg = im_s_general_data-legal_org.

    lv_text = im_s_general_data-name.

    CALL FUNCTION 'SOTR_SERV_STRING_TO_TABLE'
      EXPORTING
        text        = lv_text
        line_length = 35
      TABLES
        text_tab    = lt_text_tab.

    LOOP AT lt_text_tab ASSIGNING FIELD-SYMBOL(<ls_text>).

      "Fill fields Name 1, 2, 3, 4
      CHECK sy-tabix < 5.

      ASSIGN COMPONENT sy-tabix OF STRUCTURE ch_s_bp_organization TO FIELD-SYMBOL(<ls_central_organ>).

      <ls_central_organ> = <ls_text>.

    ENDLOOP.

  ENDMETHOD.



  METHOD fill_searchterm.

    DATA:
      lt_text_tab    TYPE TABLE OF char255.

    CALL FUNCTION 'SOTR_SERV_STRING_TO_TABLE'
      EXPORTING
        text        = im_v_name
        line_length = 15
      TABLES
        text_tab    = lt_text_tab.

    r_result = lt_text_tab[ 1 ].

  ENDMETHOD.

ENDCLASS.
