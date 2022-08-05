*----------------------------------------------------------------------*
*                   ___    _     __    __    _                         *
*                    |    |_|   |_    (_    |_|                        *
*                    |    | |   |__   __)   | |                        *
*            TRANSMISSORA ALIANÇA DE ENERGIA ELÉTRICA S.A.             *
*----------------------------------------------------------------------*
* Consulting  .....: IntechPro                                         *
* ABAP Developer ..: Marcelo Alvares (MA004818)                        *
* Business Consult.: Marcia Barbisan                                   *
* Module ..........: BUPA - Business partner                           *
* Program     .....: ZBUPA_0001                                        *
* Transaction .....: ZBP002                                            *
* Type        .....: Report Include                                    *
* Objective   .....: Business partners load                            *
* Request     .....: SHDK904703                                        *
*----------------------------------------------------------------------*
* Change Control:                                                      *
* Version  Date       Who                What                          *
*    1.00  05.11.2019 Marcelo Alvares    Initial release               *
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
*& Include          ZBUPA_0001_LCL
*&---------------------------------------------------------------------*

CLASS lcl_file_upload DEFINITION INHERITING FROM zcl_file_upload FRIENDS lcl_zbupa_0001.

  PUBLIC SECTION.

    TYPES:
      BEGIN OF ty_s_upload_general_data,
        taxnum     TYPE dfkkbptaxnum-taxnum,  " Business Partner Tax Number
*        bukrs      TYPE lfb1-bukrs, "Check it out --NO NEED
        name1      TYPE but000-name1_text,      " Full Name
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
      END OF ty_s_upload_general_data,

*      BEGIN OF ty_s_upload_invoice_data,
*        taxnum     TYPE dfkkbptaxnum-taxnum,    " Business Partner Tax Number
*        bukrs      TYPE lfb1-bukrs,             " Check it out --NO NEED
*        gsber      TYPE bseg-gsber,
*        bldat      TYPE bkpf-bldat,             " Document Date in Document
*        budat      TYPE bkpf-budat,             " Posting Date in the Document
*        faedt      TYPE faedt_fpos,             " Net Due Date
*        xblnr      TYPE bkpf-xblnr,             " Reference Document Number
*        attr       TYPE bseg-zuonr,             " Assignment number
*        amt_doccur TYPE bapidoccur,             " Amount in Document Currency
*        gkont      TYPE bseg-gkont,             " Offsetting Account Number
*        nplnr      TYPE bseg-nplnr,             " Network Number for Account Assignment
*        aufpl      TYPE bseg-aufpl,             " Task List Number for Operations in Order
*      END OF ty_s_upload_invoice_data,

      ty_t_upload_general_data TYPE STANDARD TABLE OF ty_s_upload_general_data WITH KEY taxnum
                                 WITH NON-UNIQUE SORTED KEY taxnum_key COMPONENTS taxnum.
*      ty_t_upload_invoice_data TYPE STANDARD TABLE OF ty_s_upload_invoice_data WITH KEY taxnum bukrs
*        WITH NON-UNIQUE SORTED KEY taxnum_key COMPONENTS taxnum.

*    TYPES: BEGIN OF ty_s_upload_file.
*    TYPES: general_data TYPE ty_t_upload_general_data,
*           invoice_data TYPE ty_t_upload_invoice_data.
*    TYPES: END OF ty_s_upload_file.

  PROTECTED SECTION.
    DATA: gt_upload_file TYPE ty_t_upload_general_data.

  PRIVATE SECTION.


ENDCLASS.

CLASS lcl_file_upload IMPLEMENTATION.


ENDCLASS.


CLASS lcl_zbupa_0001 DEFINITION INHERITING FROM zcl_bp_maintain FRIENDS lcl_file_upload.

  PUBLIC SECTION.

    TYPES:
      "! <p class="shorttext synchronized" lang="en">Structure of BP Data</p>
      BEGIN OF ty_s_bp_create.
        INCLUDE TYPE ty_s_bp_key.
    TYPES:
      status_bp      TYPE tp_icon,    " Status
      belnr          TYPE belnr_d,
      status_doc     TYPE tp_icon,    " Status
      message        TYPE bapi_msg,
      general_data   TYPE lcl_file_upload=>ty_s_upload_general_data,
*      invoice_data   TYPE lcl_file_upload=>ty_t_upload_invoice_data,
      partner_bus    TYPE bus_ei_extern,
      vendor_vmds    TYPE vmds_ei_extern,
      return_bp      TYPE bapiret2_t,
      return_doc     TYPE bapiret2_t,
      belnr_key      TYPE awkey,
      create_invoice TYPE abap_bool.
    TYPES:       END OF ty_s_bp_create.

    TYPES:
      "! <p class="shorttext synchronized" lang="en">Table of BP Data</p>
      ty_t_bp_create TYPE STANDARD TABLE OF ty_s_bp_create WITH KEY partner partner_guid taxnum vendor
        WITH NON-UNIQUE SORTED KEY taxnum_key         COMPONENTS taxnum
        WITH NON-UNIQUE SORTED KEY partner_sort_key   COMPONENTS partner  vendor
        WITH NON-UNIQUE SORTED KEY vendor_sort_key    COMPONENTS vendor   partner .

    TYPES: BEGIN OF ty_s_bukrs,
             bukrs TYPE bukrs,
           END OF ty_s_bukrs,
           ty_t_bukrs TYPE STANDARD TABLE OF ty_s_bukrs WITH KEY bukrs.

    TYPES:
      "! <p class="shorttext synchronized" lang="en">Structure of ALV</p>
      BEGIN OF ty_s_alv,
        taxnum    TYPE bptaxnum,
        partner   TYPE bu_partner,
        vendor    TYPE lifnr,
        status_bp TYPE tp_icon,    " Status
*        belnr      TYPE belnr_d,
*        status_doc TYPE tp_icon,    " Status
        message   TYPE bapi_msg,
*        belnr_key  TYPE awkey,
      END OF ty_s_alv,

      "! <p class="shorttext synchronized" lang="en">Table of ALV</p>
      ty_t_alv TYPE STANDARD TABLE OF ty_s_alv WITH KEY taxnum partner vendor
                 WITH NON-UNIQUE SORTED KEY taxnum_key         COMPONENTS taxnum
                 WITH NON-UNIQUE SORTED KEY partner_sort_key   COMPONENTS partner  vendor
                 WITH NON-UNIQUE SORTED KEY vendor_sort_key    COMPONENTS vendor   partner .

    CONSTANTS:
      "! <p class="shorttext synchronized" lang="en">Reconciliation account for BP Company data</p>
      co_akont TYPE lfb1-akont  VALUE '2110130108',
      "! <p class="shorttext synchronized" lang="en">Terms of Payment Key for BP Company data</p>
      co_zterm TYPE dzterm      VALUE 'TB30'.

    METHODS:
      "! Constructor
      constructor,
      upload_file
        RAISING
          zcx_file_upload
          zcx_excel,
      create_bp,

      "! On event Link Click
      "! @parameter row | ROW
      "! @parameter column | Column
      on_alv_link_click     FOR EVENT link_click OF cl_salv_events_table
        IMPORTING
            row
            column,

      on_alv_user_command   FOR EVENT if_salv_events_functions~added_function  OF cl_salv_events_table
        IMPORTING
            e_salv_function,
      on_alv_double_click   FOR EVENT double_click OF cl_salv_events_table
        IMPORTING
            row
            column,
      alv_display.

    CLASS-METHODS:
      class_constructor,
      export_model.

    DATA:
      go_alv_table TYPE REF TO cl_salv_table.

  PROTECTED SECTION.

    DATA:
      go_file    TYPE REF TO lcl_file_upload,
      gt_bp_data TYPE ty_t_bp_create,
      gv_test    TYPE abap_bool,
      gt_alv     TYPE ty_t_alv,
      gt_bukrs   TYPE ty_t_bukrs.

  PRIVATE SECTION.

    METHODS:
      msg_initialize,
      call_bp_maintain
        CHANGING
          ch_s_data TYPE lcl_zbupa_0001=>ty_s_bp_create,
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
          ch_t_bp_data     TYPE lcl_zbupa_0001=>ty_t_bp_create,
      convert_cpf_cnpj
        CHANGING
          ch_v_taxnum TYPE any,
      convert_field_date
        CHANGING
          ch_v_struc TYPE any,
      call_commit_and_check
        CHANGING
          ch_s_data TYPE lcl_zbupa_0001=>ty_s_bp_create,
      extend_vendor
        CHANGING
          ch_s_data TYPE lcl_zbupa_0001=>ty_s_bp_create,
      msg_show
        IMPORTING
          im_t_return TYPE bapiret2_t
          im_v_zeile  TYPE string,
      fill_bank_key
        IMPORTING
          im_s_general_data TYPE lcl_file_upload=>ty_s_upload_general_data
        RETURNING
          VALUE(r_result)   TYPE bus_ei_struc_bankdetail-bank_key.

ENDCLASS.

CLASS lcl_zbupa_0001 IMPLEMENTATION.

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

    DATA: ls_company TYPE vmds_ei_company.

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
      IF <bp_data>-partner IS NOT INITIAL AND <bp_data>-vendor IS NOT INITIAL.

        me->extend_vendor(
          CHANGING
            ch_s_data = <bp_data> ).

        CONTINUE. " Next LOOP

      ENDIF.

      " Assigning fields symbols
      APPEND INITIAL LINE TO:
        <bp_data>-partner_bus-central_data-role-roles               ASSIGNING FIELD-SYMBOL(<ls_roles>),
        <bp_data>-partner_bus-central_data-taxnumber-taxnumbers     ASSIGNING FIELD-SYMBOL(<ls_taxnumbers>),
        <bp_data>-partner_bus-central_data-address-addresses        ASSIGNING FIELD-SYMBOL(<ls_addresses>).

      ASSIGN <bp_data>-general_data TO FIELD-SYMBOL(<general_data>).

      " Add Insert task to all fields
      MOVE me->co_task-insert TO:
        <bp_data>-partner_bus-header-object_task,
        <bp_data>-vendor_vmds-header-object_task,
        ls_company-task,
        <ls_addresses>-task,
        <ls_roles>-task,
        <ls_taxnumbers>-task.

*>>>>>>>>>>>>>>>>>>>>>>>>>>>> C E N T R A L  D A T A <<<<<<<<<<<<<<<<<<<<<<<<<<<<*
      <bp_data>-partner_guid                                                         = me->create_bp_partner_guid( ).
      <bp_data>-partner_bus-header-object_instance-bpartnerguid                      = <bp_data>-partner_guid.
      <bp_data>-partner_bus-central_data-common-data-bp_control-category             = me->co_category_bp_as-person.
      <bp_data>-partner_bus-central_data-common-data-bp_control-grouping             = me->co_grouping-nac_pf.
      <bp_data>-partner_bus-central_data-common-data-bp_person-firstname             = me->get_first_name( <general_data>-name1 ).
      <bp_data>-partner_bus-central_data-common-data-bp_person-lastname              = me->get_last_name( <general_data>-name1 ).
      <bp_data>-partner_bus-central_data-common-data-bp_centraldata-searchterm1      = me->get_first_name( <general_data>-name1 ).
      <bp_data>-partner_bus-central_data-common-data-bp_person-fullname              = <general_data>-name1.
      <bp_data>-partner_bus-central_data-common-data-bp_person-correspondlanguageiso = me->co_languiso_pt.
      <bp_data>-partner_bus-central_data-common-data-bp_person-nationalityiso        = me->co_country_iso_br.
      <bp_data>-partner_bus-central_data-taxnumber-common-data-nat_person            = abap_on. " Mandatory for person

*>>>>>>>>>>>>>>>>>>>>>>>>>>>> A D D R E S S E S <<<<<<<<<<<<<<<<<<<<<<<<<<<<*
      MOVE-CORRESPONDING <general_data> TO <ls_addresses>-data-postal-data.
      <ls_addresses>-data-postal-data-country    = me->co_country_iso_br.
      <ls_addresses>-data-postal-data-languiso   = me->co_languiso_pt.
      <ls_addresses>-data-postal-data-langu      = me->co_languiso_pt.

      " Telephone
      IF <general_data>-tel_number IS NOT INITIAL.
        APPEND INITIAL LINE TO
          <bp_data>-partner_bus-central_data-communication-phone-phone ASSIGNING FIELD-SYMBOL(<phone>).
        <phone>-contact-task            = me->co_task-insert. "'I'.
        <phone>-contact-data-country    = me->co_country_iso_br.
        <phone>-contact-data-telephone  = <general_data>-tel_number.
        <phone>-contact-data-r_3_user   = '1'. " Default
      ENDIF.

      " Email
      IF <general_data>-smtp_addr IS NOT INITIAL.
        APPEND INITIAL LINE TO
          <bp_data>-partner_bus-central_data-communication-smtp-smtp ASSIGNING FIELD-SYMBOL(<email>).
        <email>-contact-task            = me->co_task-insert. "'I'.
        <email>-contact-data-e_mail     = <general_data>-smtp_addr.
      ENDIF.


*>>>>>>>>>>>>>>>>>>>>>>>>>>>> B A N K S  <<<<<<<<<<<<<<<<<<<<<<<<<<<<*
      IF <general_data>-banco IS NOT INITIAL.
        APPEND INITIAL LINE TO <bp_data>-partner_bus-central_data-bankdetail-bankdetails ASSIGNING FIELD-SYMBOL(<ls_bankdetails>).
        <ls_bankdetails>-task           = me->co_task-insert. "'I'.
        <ls_bankdetails>-data-bank_ctry = 'BR'.
        <ls_bankdetails>-data-bank_key  = me->fill_bank_key( <general_data> ).
        <ls_bankdetails>-data-bank_acct = <general_data>-bankn.     " Bank account number
        <ls_bankdetails>-data-ctrl_key  = <general_data>-ctrl_key.  " Bank Control Key
        IF me->gv_test IS NOT INITIAL.
          <ls_bankdetails>-data_key     = <general_data>-banco. " Avoid msg erros in test
        ENDIF.
      ENDIF.

*>>>>>>>>>>>>>>>>>>>>>>>>>>>> R O L E S  <<<<<<<<<<<<<<<<<<<<<<<<<<<<*
      MOVE co_rolecategory-supplier_fin TO:
        <ls_roles>-data-rolecategory,
        <ls_roles>-data_key.
      <ls_roles>-data-valid_from         = sy-datum.

*>>>>>>>>>>>>>>>>>>>>>>>> T A X  N U M B E R S <<<<<<<<<<<<<<<<<<<<<<*
      <ls_taxnumbers>-data_key-taxnumber = <bp_data>-taxnum.
      <ls_taxnumbers>-data_key-taxtype   = me->co_taxtype-cpf.

*>>>>>>>>>>>>>>>>>>>>>>>>>>>> C O M P A N Y <<<<<<<<<<<<<<<<<<<<<<<<<*
      ls_company-data-zwels = 'C'.
      ls_company-data-fdgrv = me->co_vendor_planning_group-pf.   " Planning Group
      ls_company-data-zterm = me->co_zterm.                      " Terms of Payment Key
      ls_company-data-akont = me->co_akont.                      " Reconciliation Account in General Ledger
      ls_company-data-reprf = abap_on.                           " Check Flag for Double Invoices or Credit Memos

      LOOP AT me->gt_bukrs ASSIGNING FIELD-SYMBOL(<bukrs>).
        ls_company-data_key-bukrs = <bukrs>-bukrs.
        APPEND ls_company TO <bp_data>-vendor_vmds-company_data-company.
      ENDLOOP.

      " If don't have any company, the standard is TB01 TAESA.
      IF syst-subrc IS NOT INITIAL.
        ls_company-data_key-bukrs = 'TB01'.
        APPEND ls_company TO <bp_data>-vendor_vmds-company_data-company.
      ENDIF.

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

    ls_data-partner = ch_s_data-partner_bus.
    ls_data-vendor  = ch_s_data-vendor_vmds.
    ls_data-ensure_create-create_vendor = abap_true.

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
        lo_alv_top_of_list->create_label( row = 1 column  = 1 text = TEXT-t01  ).

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

      WHEN 'STATUS_DOC'.

        me->msg_show(
            EXPORTING
            im_t_return = <row_alv>-return_doc
            im_v_zeile = |FI{ row }|  ).


      WHEN 'BELNR'.
        CHECK:
            <row_alv>-belnr     IS NOT INITIAL,
            <row_alv>-belnr_key IS NOT INITIAL.
        SET PARAMETER ID 'BUK' FIELD <row_alv>-belnr_key+10(4).
        SET PARAMETER ID 'BLP' FIELD <row_alv>-belnr_key(10).
        SET PARAMETER ID 'GJR' FIELD <row_alv>-belnr_key+14(4).
        CALL TRANSACTION 'FBV3' WITH AUTHORITY-CHECK AND SKIP FIRST SCREEN.

    ENDCASE.

  ENDMETHOD.

  METHOD on_alv_user_command.

  ENDMETHOD.


  METHOD get_bp_from_taxnumber_lc.

    DATA:
      lt_select_data TYPE zif_bp_standard=>ty_t_bp_key,
      lr_taxnum      TYPE RANGE OF bptaxnum.

    " Fill Range of Tax Number
    lr_taxnum = VALUE #( FOR ls_general_data IN im_t_upload_file
                      ( sign    = 'I'
                        option  = 'EQ'
                        low     = ls_general_data-taxnum ) ).

*    lr_taxnum = VALUE #( FOR ls_invoice_data IN im_s_upload_file-invoice_data
*                      ( sign    = 'I'
*                        option  = 'EQ'
*                        low     = ls_invoice_data-taxnum ) ).

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

*    LOOP AT im_s_upload_file-general_data ASSIGNING FIELD-SYMBOL(<general_data>).
    LOOP AT lr_taxnum ASSIGNING FIELD-SYMBOL(<lr_taxnum>).

      APPEND INITIAL LINE TO:
        ch_t_bp_data ASSIGNING FIELD-SYMBOL(<bp_data>).

      <bp_data>-taxnum = <lr_taxnum>-low.

      APPEND INITIAL LINE TO <bp_data>-return_bp ASSIGNING FIELD-SYMBOL(<return>).

      " Check if exist in DB
      READ TABLE lt_select_data WITH KEY taxnum = <lr_taxnum>-low ASSIGNING FIELD-SYMBOL(<select_data>).

      IF syst-subrc IS INITIAL.

        MOVE-CORRESPONDING <select_data> TO <bp_data>.

        " O PN &1 ( &2 ) já cadastrado.
        MESSAGE s037(z_bupa) WITH <bp_data>-partner <bp_data>-vendor INTO <return>-message.
        <bp_data>-message   = <return>-message.
        <bp_data>-status_bp = icon_led_green.

        IF <bp_data>-vendor IS NOT INITIAL.
          <bp_data>-create_invoice = abap_true.
        ENDIF.

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
      ch_s_data-create_invoice = abap_true.
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

ENDCLASS.
