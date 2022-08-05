*----------------------------------------------------------------------*
*                      ___    _     __    __    _                      *
*                       |    |_|   |_    (_    |_|                     *
*                       |    | |   |__   __)   | |                     *
*                                                                      *
*----------------------------------------------------------------------*
*            TRANSMISSORA ALIANÇA DE ENERGIA ELÉTRICA S.A.             *
*----------------------------------------------------------------------*
* ABAP Developer ..: Luciana Oliveira (90000237)                       *
* Business Consult.: Marcelo Alvares  (90000130)                       *
* Module ..........: BUPA - Business partner                           *
* Program     .....: ZBUPA_0003                                        *
* Transaction .....: ZBP004                                            *
* Type        .....: Report Include                                    *
* Objective   .....: Business partners load - Customer                 *
* Request     .....: SHDK921433                                        *
*----------------------------------------------------------------------*
* Change Control:                                                      *
* Date       | Who                                      |Task          *
* 14.12.2021 | Luciana Oliveira (90000237)              |RITM0153502   *
* 03.02.2022 | Marcelo Alvares  (90000130)              |RITM0153502   *
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
*& Include          ZBUPA_0003_LCL
*&---------------------------------------------------------------------*

"! <p class="shorttext synchronized" lang="en">Local class inheriting from <strong>zcl_file_upload</strong> with all procedures to upload excel file format </p>
CLASS lcl_file_upload DEFINITION INHERITING FROM zcl_file_upload FRIENDS lcl_zbupa_0003.

  PUBLIC SECTION.

    TYPES:
      "! <p class="shorttext synchronized" lang="en">Type of Structure for BP Data, used in the upload excel file</p>
      BEGIN OF ty_s_upload_general_data,
        taxnum     TYPE dfkkbptaxnum-taxnum,  " Business Partner Tax Number
*        bukrs      TYPE lfb1-bukrs, "Check it out --NO NEED
        name       TYPE hrpaybr_dirf_comp_name, " Full Name
        serchterm  TYPE adrc-sort1,             " Search Term 1
        insc_est   TYPE kna1-stcd3,             " Tax Number 3 Inscrição Estadual
        insc_mun   TYPE kna1-stcd4,             " Tax Number 4 Inscrição Municipal
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
        inco1      TYPE knvv-inco1,             " Incoterms (Part 1)
        inco2_l    TYPE knvv-inco2_l,           " Incoterms Location 1
        inco2      TYPE knvv-inco2,             " Incoterms (Part 2)
        ktgrd      TYPE knvv-ktgrd,             " Account Assignment Group for this customer
*        cmds_ei_sales_data
*        vtweg      TYPE knvv-vtweg,
*        spart      TYPE knvv-spart,
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

*&----------------------------------------------------------------------*
*& CLASS LCL_ZBUPA_0003 DEFINITION
*&----------------------------------------------------------------------*
CLASS lcl_zbupa_0003 DEFINITION INHERITING FROM zcl_bp_maintain FRIENDS lcl_file_upload.

  PUBLIC SECTION.

    TYPES:
      "! <p class="shorttext synchronized" lang="en">Structure of BP Data</p>
      BEGIN OF ty_s_bp_create.
        INCLUDE TYPE ty_s_bp_key.
      TYPES:
        status_bp       TYPE tp_icon,    " Status
        message         TYPE bapi_msg,
        general_data    TYPE lcl_file_upload=>ty_s_upload_general_data,
        partner_bus     TYPE bus_ei_extern,
*        vendor_vmds   TYPE vmds_ei_extern,
        customer_cmds   TYPE cmds_ei_extern,
        return_bp       TYPE bapiret2_t,
        update          TYPE abap_bool,
        bp_data_current TYPE zif_bp_standard=>ty_s_bp,
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
      BEGIN OF ty_s_vkorg,
        vkorg TYPE tvta-vkorg,
      END OF ty_s_vkorg,
      "! <p class="shorttext synchronized" lang="en">Type of Table Purchasing organization ID</p>
      ty_t_vkorg TYPE STANDARD TABLE OF ty_s_vkorg WITH KEY vkorg.

    TYPES:
      "! <p class="shorttext synchronized" lang="en">Structure for ALV</p>
      BEGIN OF ty_s_alv,
        taxnum    TYPE bptaxnum,
        partner   TYPE bu_partner,
        customer  TYPE kna1-kunnr,
        status_bp TYPE tp_icon,    " Status
        message   TYPE bapi_msg,
      END OF ty_s_alv,

      "! <p class="shorttext synchronized" lang="en">Type of table for ALV</p>
      ty_t_alv TYPE STANDARD TABLE OF ty_s_alv WITH KEY taxnum partner customer
                 WITH NON-UNIQUE SORTED KEY taxnum_key         COMPONENTS taxnum
                 WITH NON-UNIQUE SORTED KEY partner_sort_key   COMPONENTS partner  customer
                 WITH NON-UNIQUE SORTED KEY customer_sort_key  COMPONENTS customer   partner ,

      "! <p class="shorttext synchronized" lang="en">Screen fields and additional parameters</p>
      BEGIN OF ty_s_parameters,
        test    TYPE abap_bool,              " Test parameter
        update  TYPE abap_bool,              " Update all Customers if found in system
        maxprc  TYPE i,                      " Max Dialog Operational Process for parallel processing (see SM50)
        r_bukrs TYPE RANGE OF t001-bukrs,    " Company Code
        group   TYPE tb001-bu_group,         " Business Partner Grouping
        icmstx  TYPE j_1bicmstaxpay,         " ICMS Taxpayer
        fdgrv   TYPE knb1-fdgrv,             " Planning Group
        zterm   TYPE knb1-zterm,             " Terms of Payment Key
        akont   TYPE knb1-akont,             " Reconciliation Account in General Ledger '2110130108'.
        zwels   TYPE knb1-zwels,             " List of Respected Payment Methods
        r_vkorg TYPE RANGE OF knvv-vkorg,    " Sales Organization
        vtweg   TYPE knvv-vtweg,             " Distribution Channel
        spart   TYPE knvv-spart,             " Division
        kalks   TYPE knvv-kalks,             " Customer Classification for Pricing Procedure Determination
        ktgrd   TYPE knvv-ktgrd,             " Account Assignment Group for this customer
        vsbed   TYPE knvv-vsbed,             " Shipping Conditions
        tatyp   TYPE knvi-tatyp,             " Tax category (sales tax, federal sales tax,...)
        taxkd   TYPE knvi-taxkd,             " Tax classification for customer
        t_bukrs TYPE ty_t_bukrs,
        t_vkorg TYPE ty_t_vkorg,
      END OF ty_s_parameters,

      "! <p class="shorttext synchronized" lang="en">Structure for runtime trace </p>
      BEGIN OF ty_s_runtime,
        t1_start  TYPE i,
        t2_finish TYPE i,
      END OF ty_s_runtime.

    CONSTANTS:
      "! <p class="shorttext synchronized" lang="en">Constants parameters and STVARV variables</p>
      BEGIN OF gc_parameters,
        "! <p class="shorttext synchronized" lang="en">Constant Reconciliation account for BP Company data</p>
        akont           TYPE lfb1-akont VALUE '1121121001',
        "! <p class="shorttext synchronized" lang="en">Constant Terms of Payment Key for BP Company data</p>
        zterm           TYPE dzterm     VALUE 'TB30',
        default_variant TYPE raldb_vari VALUE 'DEFAULT',
        tvarv_incoterms TYPE rvari_vnam VALUE 'ZSD_INCONT',
        tvarv_kalks     TYPE rvari_vnam VALUE 'ZSD_CLASS_FIS',
      END OF gc_parameters.

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

*      "! <p class="shorttext synchronized" lang="en"> On event User Command </p>
*      "! @parameter e_salv_function | <p class="shorttext synchronized" lang="en"> ALV Function </p>
*      on_alv_user_command   FOR EVENT if_salv_events_functions~added_function  OF cl_salv_events_table
*        IMPORTING
*          e_salv_function,

*      "! <p class="shorttext synchronized" lang="en">Class Object for ALV cl_salv_table</p>
*      "! @parameter row     | <p class="shorttext synchronized" lang="en">Selected row in alv</p>
*      "! @parameter column  | <p class="shorttext synchronized" lang="en">Selected column in alv</p>
*      on_alv_double_click   FOR EVENT double_click OF cl_salv_events_table
*        IMPORTING
*          row
*          column,
      "! <p class="shorttext synchronized" lang="en">Display ALV method</p>
      alv_display.

    CLASS-METHODS:
*      "! <p class="shorttext synchronized" lang="en">Class Constructor method</p>
*      class_constructor,
      "! <p class="shorttext synchronized" lang="en">Export/download file model excel for upload </p>
      export_model,


      at_selection_screen.

    DATA:
    "! <p class="shorttext synchronized" lang="en">CLass Object for ALV cl_salv_table</p>
      go_alv_table TYPE REF TO cl_salv_table.

  PROTECTED SECTION.

    DATA:
      go_file       TYPE REF TO lcl_file_upload,
      gt_bp_data    TYPE ty_t_bp_create,
      gt_alv        TYPE ty_t_alv,
      gs_parameters TYPE ty_s_parameters.

  PRIVATE SECTION.
    DATA: gs_runtime TYPE ty_s_runtime.

    METHODS:
      msg_initialize,
      alv_create,
*      alv_set_icon_type
*        IMPORTING
*          im_t_return      TYPE bapiret2_t
*        EXPORTING
*          ex_v_status_icon TYPE tp_icon
*          ex_v_message     TYPE bapi_msg,

      alv_set_columns,
      get_bp_from_taxnumber_lc
        IMPORTING
          im_t_upload_file TYPE lcl_file_upload=>ty_t_upload_general_data
        CHANGING
          ch_t_bp_data     TYPE lcl_zbupa_0003=>ty_t_bp_create,
      convert_cpf_cnpj
        CHANGING
          ch_v_taxnum TYPE any,
      convert_field_date
        CHANGING
          ch_v_struc TYPE any,
      call_commit_and_check
        CHANGING
          ch_s_data TYPE lcl_zbupa_0003=>ty_s_bp_create,
      msg_show
        IMPORTING
          im_t_return TYPE bapiret2_t
          im_v_zeile  TYPE string,

      "! <p class="shorttext synchronized" lang="en">Fill Bank Key Structure</p>
      fill_bank_key
        IMPORTING
          im_s_general_data TYPE lcl_file_upload=>ty_s_upload_general_data
        RETURNING
          VALUE(r_result)   TYPE bus_ei_struc_bankdetail-bank_key,

      "! <p class="shorttext synchronized" lang="en">Update customer</p>
      update_customer
        CHANGING
          ch_s_data TYPE lcl_zbupa_0003=>ty_s_bp_create
        RAISING
          zcx_abap_error,

      "! <p class="shorttext synchronized" lang="en">Compare Any data structure</p>
      compare_data
        IMPORTING
          im_s_source  TYPE any
          im_s_current TYPE any
        CHANGING
          ch_s_data_x  TYPE any,

      "! <p class="shorttext synchronized" lang="en">Compare BP data and fill datax structure</p>
      compare_bp_source_fill_datax
        CHANGING
          ch_s_source TYPE ty_s_bp_create
        RAISING
          zcx_abap_error,

      "! <p class="shorttext synchronized" lang="en">Fill Organization</p>
      "! @parameter im_s_general_data | Data
      "! @parameter ch_s_bp_organization | Organization structure
      fill_organization
        IMPORTING
          im_s_general_data    TYPE lcl_file_upload=>ty_s_upload_general_data
        CHANGING
          ch_s_bp_organization TYPE bus_ei_struc_central_organ,

      "! <p class="shorttext synchronized" lang="en">Fill Search Term</p>
      "! @parameter im_v_name | Import name
      "! @parameter r_result | Result
      fill_searchterm
        IMPORTING
          im_v_name       TYPE string
        RETURNING
          VALUE(r_result) TYPE bus_ei_struc_central-searchterm1,

      "! <p class="shorttext synchronized" lang="en">Fill Tax Numbers data for create BP</p>
      "! @parameter ch_data | Default structure from interaction LOOP
      fill_tax_numbers
        CHANGING
          ch_data TYPE lcl_zbupa_0003=>ty_s_bp_create,

      "! <p class="shorttext synchronized" lang="en">Fill bank details for create BP</p>
      "! @parameter ch_data | Default structure from interaction LOOP
      fill_bank_details
        CHANGING
          ch_data TYPE lcl_zbupa_0003=>ty_s_bp_create,

      "! <p class="shorttext synchronized" lang="en">Fill Central data for create BP</p>
      "! @parameter ch_data | Default structure from interaction LOOP
      fill_central_data
        CHANGING
          ch_data TYPE lcl_zbupa_0003=>ty_s_bp_create,

      "! <p class="shorttext synchronized" lang="en">Fill company data for create BP</p>
      "! @parameter ch_data | Default structure from interaction LOOP
      fill_company_data
        CHANGING
          ch_data TYPE lcl_zbupa_0003=>ty_s_bp_create,

      "! <p class="shorttext synchronized" lang="en">Fill address data for create BP</p>
      "! @parameter ch_data | Default structure from interaction LOOP
      fill_addresses
        CHANGING
          ch_data TYPE lcl_zbupa_0003=>ty_s_bp_create,

      "! <p class="shorttext synchronized" lang="en">Fill Sales data for create BP</p>
      "! @parameter ch_data | Default structure from interaction LOOP
      fill_sales_data
        CHANGING
          ch_data TYPE lcl_zbupa_0003=>ty_s_bp_create,
      "! <p class="shorttext synchronized" lang="en">Initialization of PBT environment Parallel Processing</p>
      spbt_initialize,
      catch_exception_save_msg
        IMPORTING
          io_exception TYPE REF TO cx_root
        CHANGING
          cs_bp_data   TYPE lcl_zbupa_0003=>ty_s_bp_create.

ENDCLASS.

*&----------------------------------------------------------------------*
*& CLASS LCL_BP_MAINTAIN DEFINITION
*&----------------------------------------------------------------------*
CLASS lcl_bp_maintain DEFINITION FINAL.
  PUBLIC SECTION.
    INTERFACES:
      if_serializable_object,
      zif_thread_runnable,
      zif_thread_runnable_result.
    DATA:
    "!<p class="shorttext synchronized" lang="en">BP structure data</p>
      gt_bp_data TYPE lcl_zbupa_0003=>ty_s_bp_create READ-ONLY.

    METHODS:
      "!<p class="shorttext synchronized" lang="en">Class Object Constructor</p>
      "! @parameter is_bp_data | BP structure data
      "! @parameter is_parameters | Parameters
      constructor
        IMPORTING
          is_bp_data    TYPE lcl_zbupa_0003=>ty_s_bp_create
          is_parameters TYPE lcl_zbupa_0003=>ty_s_parameters OPTIONAL.

  PRIVATE SECTION.
    DATA:
      gs_parameters TYPE lcl_zbupa_0003=>ty_s_parameters.

    METHODS:

      "!<p class="shorttext synchronized" lang="en">Extend Customer</p>
      extend_customer
        CHANGING
          ch_s_data TYPE lcl_zbupa_0003=>ty_s_bp_create,

      "!<p class="shorttext synchronized" lang="en">Call commit and check if BP is OK</p>
      call_commit_and_check
        CHANGING
          cs_data TYPE lcl_zbupa_0003=>ty_s_bp_create,

      "!<p class="shorttext synchronized" lang="en">Class Object Constructor</p>
      "! @parameter it_return | Return table of messages
      "! @parameter ev_status_icon | Status Icon
      "! @parameter ev_message | Message
      alv_set_icon_type
        IMPORTING
          it_return      TYPE bapiret2_t
        EXPORTING
          ev_status_icon TYPE tp_icon
          ev_message     TYPE bapi_msg.

ENDCLASS.

*&----------------------------------------------------------------------*
*& CLASS LCL_BP_MAINTAIN IMPLEMENTATION
*&----------------------------------------------------------------------*
CLASS lcl_bp_maintain IMPLEMENTATION.


*&----------------------------------------------------------------------*
*& METHOD CONSTRUCTOR
*&----------------------------------------------------------------------*
  METHOD constructor.

    me->gt_bp_data          = is_bp_data.
    me->gs_parameters   = is_parameters.

  ENDMETHOD.

*&----------------------------------------------------------------------*
*& METHOD ZIF_THREAD_RUNNABLE~RUN
*&----------------------------------------------------------------------*
  METHOD zif_thread_runnable~run.

    DATA:
      lt_data       TYPE cvis_ei_extern_t,
      lt_return_map TYPE mdg_bs_bp_msgmap_t,
      lt_return     TYPE bapiretm,
      lt_return_bp  TYPE bapiret2_t.

    APPEND INITIAL LINE TO lt_data ASSIGNING FIELD-SYMBOL(<ls_data>).

    <ls_data>-partner                           = gt_bp_data-partner_bus.
    <ls_data>-customer                          = gt_bp_data-customer_cmds.

    " If customer already exist, don't fill to create customer
    IF gt_bp_data-customer IS INITIAL.
      <ls_data>-ensure_create-create_customer   = abap_true.
    ENDIF.

    IF me->gs_parameters-test EQ abap_on.

      cl_md_bp_maintain=>validate_single(
        EXPORTING
          i_data        = <ls_data>
        IMPORTING
          et_return_map = lt_return_map ).

      IF lt_return_map IS INITIAL.
        APPEND INITIAL LINE TO gt_bp_data-return_bp ASSIGNING FIELD-SYMBOL(<data_return>).
        MESSAGE s036(z_bupa) INTO <data_return>-message. " Verification without errors! the BP will be created.
        <data_return>-id     = 'Z_BUPA'.
        <data_return>-type   = 'S'.
        <data_return>-number = '036'.
      ELSE.
        MOVE-CORRESPONDING lt_return_map TO lt_return_bp.
        APPEND LINES OF lt_return_bp TO gt_bp_data-return_bp.

      ENDIF.

    ELSE.

      cl_md_bp_maintain=>maintain(
        EXPORTING
          i_data   = lt_data      " Inbound for Customer/Vendor Integration
        IMPORTING
          e_return = lt_return  )." BAPIRETI Table Type for Multiple Objects

      READ TABLE lt_return ASSIGNING FIELD-SYMBOL(<return>) INDEX 1.

      " Error or Warning Found!!
      IF syst-subrc IS INITIAL.
        LOOP AT <return>-object_msg ASSIGNING FIELD-SYMBOL(<object_msg>).
          APPEND INITIAL LINE TO gt_bp_data-return_bp ASSIGNING <data_return>.
          MOVE-CORRESPONDING <object_msg> TO <data_return>.

          IF <object_msg>-type CA 'AE'. "Error Found!!!!
            CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
          ELSE.

            me->call_commit_and_check(
              CHANGING
                cs_data = gt_bp_data ).
          ENDIF.
        ENDLOOP.

        CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.

      ELSE.

        me->call_commit_and_check(
          CHANGING
            cs_data = gt_bp_data ).

      ENDIF.

    ENDIF.

    me->alv_set_icon_type(
        EXPORTING
            it_return = gt_bp_data-return_bp
        IMPORTING
            ev_status_icon = gt_bp_data-status_bp
            ev_message     = gt_bp_data-message ).

    me->extend_customer(
      CHANGING
        ch_s_data = gt_bp_data ).

    ro_result = me.

  ENDMETHOD.

*&----------------------------------------------------------------------*
*& METHOD EXTEND_CUSTOMER
*&----------------------------------------------------------------------*
  METHOD extend_customer.

    DATA:
      lt_bp_key        TYPE zcl_bp_standard=>ty_t_bp_key,
      ls_bp_key        TYPE zcl_bp_standard=>ty_s_bp_key,
      lt_bp_data       TYPE zcl_bp_extend_customer=>ty_t_bp_extend,
      lt_get_bp_data   TYPE zcl_bp_standard=>ty_t_bp,
*      ls_bp_data       TYPE zcl_bp_extend_vendor=>ty_s_bp_extend,
      lt_bukrs_for     TYPE zcl_bp_standard=>ty_t_t001_key,
      lt_vkorg_for     TYPE zcl_bp_extend_customer=>ty_t_tvko,
      lt_extend_return TYPE bapiretm,                       " BAPIRETI Table Type for Multiple Objects
      lv_bukrs_from    TYPE bukrs,
      lv_vkorg_from    TYPE vkorg.


    SELECT SINGLE bukrs
        FROM knb1
        INTO lv_bukrs_from
        WHERE kunnr = ch_s_data-customer.

    " Takes all companies that have not been extended at Customer
    SELECT t001~bukrs                     " #EC CI_NO_TRANSFORM
        FROM t001 AS t001
        INTO TABLE @lt_bukrs_for
        FOR ALL ENTRIES IN  @me->gs_parameters-t_bukrs
        WHERE
            t001~bukrs = @me->gs_parameters-t_bukrs-bukrs AND
            t001~bukrs NOT IN ( SELECT bukrs FROM knb1 WHERE kunnr = @ch_s_data-customer ).

    SELECT SINGLE vkorg
        FROM tvko
        INTO lv_vkorg_from
        WHERE kunnr = ch_s_data-customer.

    " Takes all Sales Organizations that have not been extended at Customer
    SELECT tvko~vkorg                     " #EC CI_NO_TRANSFORM
        FROM tvko AS tvko
        INTO TABLE @lt_vkorg_for
        FOR ALL ENTRIES IN  @me->gs_parameters-t_vkorg
        WHERE
            tvko~vkorg  = @me->gs_parameters-t_vkorg-vkorg AND
            tvko~vkorg  NOT IN ( SELECT vkorg FROM knvv WHERE kunnr = @ch_s_data-customer ).

    CHECK:
        lt_vkorg_for IS NOT INITIAL OR " There are no companies to extend
        lt_bukrs_for IS NOT INITIAL.

    MOVE-CORRESPONDING ch_s_data TO ls_bp_key.
    APPEND ls_bp_key TO lt_bp_key.

    TRY.

        zcl_bp_extend_vendor=>get_bp_data_from_vendor(
          EXPORTING
            im_t_bp_key  = lt_bp_key
          IMPORTING
            ex_t_bp_data = lt_get_bp_data   ).

        MOVE-CORRESPONDING lt_get_bp_data TO lt_bp_data.

        DATA(lo_extend_customer) = NEW zcl_bp_extend_customer(
                                          im_v_bukrs_from = lv_bukrs_from
                                          im_v_vkorg_from = lv_vkorg_from
                                          im_t_bukrs_for  = lt_bukrs_for
                                          im_t_vkorg_for  = lt_vkorg_for
                                          im_t_bp_data    = lt_bp_data    ).


        lo_extend_customer->extend_customer(
            EXPORTING
              im_b_test   = me->gs_parameters-test       " Test Flag
            IMPORTING
              ex_t_return = lt_extend_return ).  " Return bapiretm


        LOOP AT lt_extend_return ASSIGNING FIELD-SYMBOL(<extend_return>).

          LOOP AT <extend_return>-object_msg ASSIGNING FIELD-SYMBOL(<object_msg>).

            IF <object_msg>-type CA 'AE'.
              DATA(lv_error) = abap_true.
            ENDIF.

            APPEND INITIAL LINE TO ch_s_data-return_bp ASSIGNING FIELD-SYMBOL(<return_bp>).

            MOVE-CORRESPONDING <object_msg> TO <return_bp>.

          ENDLOOP.

        ENDLOOP.

        IF me->gs_parameters-test  IS NOT INITIAL.

          IF lv_error EQ abap_true.
            " Simulação de extensão para o cliente &1 com erros!
            MESSAGE e054(z_bupa) WITH ch_s_data-customer INTO DATA(lv_msgdummy).

          ELSE.
            " Simulação de extensão para o cliente &1 realizada com sucesso!
            MESSAGE s053(z_bupa) WITH ch_s_data-customer INTO lv_msgdummy.
          ENDIF.

        ELSE.

          LOOP AT lt_bukrs_for ASSIGNING FIELD-SYMBOL(<bukrs_extended>).
            " Check If Company exists for customer
            SELECT SINGLE bukrs FROM knb1 BYPASSING BUFFER
                INTO @DATA(lv_bukrs)
                WHERE kunnr = @ch_s_data-customer AND
                      bukrs = @<bukrs_extended>-bukrs.

            IF syst-subrc IS INITIAL.
              " Cliente &1 foi estendido para a empresa &2 com sucesso.
              MESSAGE s055(z_bupa) WITH ch_s_data-customer <bukrs_extended>-bukrs INTO lv_msgdummy.
            ELSE.
              " ERRO ao estender Cliente &1 para a empresa &2. Ver MDS_PPO2!
              MESSAGE e056(z_bupa) WITH ch_s_data-customer <bukrs_extended>-bukrs INTO lv_msgdummy.
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

*&----------------------------------------------------------------------*
*& METHOD CALL_COMMIT_AND_CHECK
*&----------------------------------------------------------------------*
  METHOD call_commit_and_check.

    " commit work and wait. " DUMP!
    CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
      EXPORTING
        wait = abap_true.

    " Get BP Number and Vendor
    SELECT SINGLE but~partner cust~customer
    INTO CORRESPONDING FIELDS OF cs_data
      FROM but000 AS but
      LEFT JOIN cvi_cust_link AS cust ON but~partner_guid = cust~partner_guid
      WHERE but~partner_guid = cs_data-partner_guid .

    APPEND INITIAL LINE TO cs_data-return_bp ASSIGNING FIELD-SYMBOL(<data_return>).

    IF cs_data-customer IS NOT INITIAL.
      " Business Partner &1 &2 successfully created!
      MESSAGE s038(z_bupa) WITH cs_data-partner cs_data-vendor INTO <data_return>-message.
      cs_data-message        = <data_return>-message.
      cs_data-status_bp      = icon_led_green.
    ELSE.
      " PN &1 criado com sucesso! Visão fornecedor não criado, Ver MDS_PPO2! ---->mudar msg para cliente. Verificar
      MESSAGE w039(z_bupa) WITH cs_data-partner cs_data-customer INTO <data_return>-message.
      cs_data-message       = <data_return>-message.
      cs_data-status_bp     = icon_led_yellow.
    ENDIF.
    <data_return>-id         = syst-msgid.
    <data_return>-type       = syst-msgty.
    <data_return>-number     = syst-msgno.
    <data_return>-message_v1 = syst-msgv1.
    <data_return>-message_v2 = syst-msgv2.


  ENDMETHOD.

*&----------------------------------------------------------------------*
*& METHOD ALV_SET_ICON_TYPE
*&----------------------------------------------------------------------*
  METHOD alv_set_icon_type.

    "Set Icon type

    "I Inform
    READ TABLE it_return WITH KEY type = 'I' ASSIGNING FIELD-SYMBOL(<return>).
    IF sy-subrc IS INITIAL.
      ev_status_icon = icon_information.
      ev_message     = <return>-message.
    ENDIF.

    "S Success
    READ TABLE it_return WITH KEY type = 'S' ASSIGNING <return>.
    IF sy-subrc IS INITIAL.
      ev_status_icon = icon_led_green.
      ev_message     = <return>-message.
    ENDIF.

    "W Warning
    READ TABLE it_return WITH KEY type = 'W' ASSIGNING <return>.
    IF sy-subrc IS INITIAL.
      ev_status_icon  = icon_led_yellow.
      ev_message      = <return>-message.
    ENDIF.

    "E Error
    READ TABLE it_return WITH KEY type = 'E' ASSIGNING <return>.
    IF sy-subrc IS INITIAL.
      ev_status_icon  = icon_led_red.
      ev_message      = <return>-message.
    ENDIF.

    "A Abort cancel.
    READ TABLE it_return WITH KEY type = 'A' ASSIGNING <return>.
    IF sy-subrc IS INITIAL.
      ev_status_icon  = icon_message_critical.
      ev_message      = <return>-message.
    ENDIF.

  ENDMETHOD.

ENDCLASS.

*&----------------------------------------------------------------------*
*& CLASS LCL_ZBUPA_0003
*&----------------------------------------------------------------------*
CLASS lcl_zbupa_0003 IMPLEMENTATION.

  METHOD constructor.

    super->constructor( ).

    GET RUN TIME FIELD me->gs_runtime-t1_start.

    CREATE OBJECT:
        me->go_file
          EXPORTING
            im_v_file_path = p_file,
        o_prog_ind TYPE zcl_progress_indicator.

    me->gs_parameters-test    = p_test.     " Test parameter
    me->gs_parameters-update  = p_updt.     " Update all Customers if found in system
    me->gs_parameters-maxprc  = p_maxprc.   " Max Dialog Operational Process for parallel processing (see SM50)
    me->gs_parameters-r_bukrs = r_bukrs[].  " Company Code
    me->gs_parameters-group   = p_group.    " Business Partner Grouping
    me->gs_parameters-icmstx  = p_icmstx.   " ICMS Taxpayer
    me->gs_parameters-fdgrv   = p_fdgrv.    " Planning Group
    me->gs_parameters-zterm   = p_zterm.    " Terms of Payment Key
    me->gs_parameters-akont   = p_akont.    " Reconciliation Account in General Ledger '2110130108'.
    me->gs_parameters-zwels   = p_zwels.    " List of Respected Payment Methods
    me->gs_parameters-r_vkorg = r_vkorg[].  " Sales Organization
    me->gs_parameters-vtweg   = p_vtweg.    " Distribution Channel
    me->gs_parameters-spart   = p_spart.    " Division
    me->gs_parameters-kalks   = p_kalks.    " Customer Classification for Pricing Procedure Determination
    me->gs_parameters-ktgrd   = p_ktgrd.    " Account Assignment Group for this customer
    me->gs_parameters-vsbed   = p_vsbed.    " Shipping Conditions
    me->gs_parameters-tatyp   = p_tatyp.    "
    me->gs_parameters-taxkd   = p_taxkd.    "

    SELECT bukrs FROM t001      " #EC CI_NO_TRANSFORM
    INTO TABLE me->gs_parameters-t_bukrs
    WHERE bukrs IN r_bukrs.

    SELECT vkorg FROM tvko      " #EC CI_NO_TRANSFORM
    INTO TABLE me->gs_parameters-t_vkorg
    WHERE vkorg IN r_vkorg.

    me->alv_create( ).

    me->spbt_initialize( ).

  ENDMETHOD.

  METHOD export_model.

    DATA: ls_file_structure  TYPE lcl_file_upload=>ty_s_upload_general_data.

    lcl_file_upload=>export_model(
      EXPORTING
        im_model       = ls_file_structure ).

  ENDMETHOD.


  METHOD upload_file.


    me->go_file->file_validate_path( ).
    me->go_file->set_parameter_id( ).

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

*&----------------------------------------------------------------------*
*& METHOD CREATE_BP
*&----------------------------------------------------------------------*
  METHOD create_bp.

    DATA: lt_runnables   TYPE zif_thread_executor_service=>tty_runnables.

    "this the fixed pool executor will ensure that now matter how many threads are submitted/invoked
    "only iv_threads will be running in parallel at the same time
    DATA(lo_fixed_pool) = zcl_thread_executors=>new_fixed_thread_pool(
                            iv_threads        = me->gs_parameters-maxprc  ).

    me->get_bp_from_taxnumber_lc(
      EXPORTING
        im_t_upload_file = me->go_file->gt_upload_file
      CHANGING
        ch_t_bp_data     = me->gt_bp_data ).

    me->o_prog_ind->set_total( im_total = lines( me->gt_bp_data )  ).
    me->o_prog_ind->reset_processed( ).

    "Process only that do not exist in the database
    LOOP AT me->gt_bp_data ASSIGNING FIELD-SYMBOL(<ls_bp_data>).

      TRY.
          me->o_prog_ind->show( ).

*>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> E X T E N D  B P <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<*

          " If Find Business Partner already exist, make the update and extension
          IF <ls_bp_data>-update      EQ abap_true.

            " Check if client update and extend is enable
            IF me->gs_parameters-update EQ abap_false AND
               me->gs_parameters-update EQ abap_false.
              " Do nothing
              CONTINUE. " Next LOOP
            ENDIF.

            " Handle with data to update
            me->update_customer(
              CHANGING
                ch_s_data = <ls_bp_data> ).

            DATA(lo_run_bp_maintain) = NEW lcl_bp_maintain(
              is_bp_data    = <ls_bp_data>
              is_parameters = me->gs_parameters ).

            APPEND lo_run_bp_maintain TO lt_runnables.

            CONTINUE. " Next LOOP
          ENDIF.

          " Assigning fields symbols
          APPEND INITIAL LINE TO:
            <ls_bp_data>-partner_bus-central_data-role-roles   ASSIGNING FIELD-SYMBOL(<ls_roles_00>), "Para função FLVN00
            <ls_bp_data>-partner_bus-central_data-role-roles   ASSIGNING FIELD-SYMBOL(<ls_roles_01>). "Para função FLVN01

          " Add Insert task to all fields
          MOVE me->co_task-insert TO:
            <ls_bp_data>-partner_bus-header-object_task,
            <ls_bp_data>-customer_cmds-header-object_task,
            <ls_roles_00>-task,
            <ls_roles_01>-task.

*>>>>>>>>>>>>>>>>>>>>>>>>>>>> R O L E S  <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<*
          "
          MOVE co_rolecategory-customer_fin TO:
            <ls_roles_00>-data-rolecategory,
            <ls_roles_00>-data_key.

          <ls_roles_00>-data-valid_from         = sy-datum.

          "
          MOVE co_rolecategory-customer_sales TO:
            <ls_roles_01>-data-rolecategory,
            <ls_roles_01>-data_key.

          <ls_roles_01>-data-valid_from         = sy-datum.

*>>>>>>>>>>>>>>>>>>>>>>>>>>>> C E N T R A L  D A T A <<<<<<<<<<<<<<<<<<<<<<<<<<<<*
          me->fill_central_data( CHANGING ch_data = <ls_bp_data> ).

*>>>>>>>>>>>>>>>>>>>>>>>>>>>> A D D R E S S E S <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<*
          me->fill_addresses(    CHANGING ch_data = <ls_bp_data> ).

*>>>>>>>>>>>>>>>>>>>>>>>>>>>> B A N K S  <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<*
          me->fill_bank_details( CHANGING ch_data = <ls_bp_data> ).

*>>>>>>>>>>>>>>>>>>>>>>>> T A X  N U M B E R S <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<*
          me->fill_tax_numbers(  CHANGING ch_data = <ls_bp_data> ).

*>>>>>>>>>>>>>>>>>>>>>>>>>>>> C O M P A N Y <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<*
          me->fill_company_data( CHANGING ch_data = <ls_bp_data> ).

*>>>>>>>>>>>>>>>>>>>>>> S A L E S <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<*
          me->fill_sales_data(   CHANGING ch_data = <ls_bp_data> ).

*>>>>>>>>>>>>>>>>>>>>>> C A L L  M A I N T A I N <<<<<<<<<<<<<<<<<<<*
*      me->call_bp_maintain(  CHANGING ch_s_data = <ls_bp_data> ).

          lo_run_bp_maintain = NEW lcl_bp_maintain(
            is_bp_data    = <ls_bp_data>
            is_parameters = me->gs_parameters ).

          APPEND lo_run_bp_maintain TO lt_runnables.

        CATCH cx_root INTO DATA(lo_error) .
          me->catch_exception_save_msg(
            EXPORTING
              io_exception = lo_error
            CHANGING
              cs_bp_data   = <ls_bp_data>  ).

      ENDTRY.

    ENDLOOP.

    me->o_prog_ind->show_msg_standalone(
        im_v_text = 'Execução em andamento...' ).

    " Run all in Parallel Processing
    DATA(lt_futures) = lo_fixed_pool->invoke_all( lt_runnables ).

    me->o_prog_ind->show_msg_standalone(
        im_v_text = 'Finalizando a execução...' ).
    " Wait to finish
    lo_fixed_pool->await_termination(  ).

    "Get results
    LOOP AT lt_futures ASSIGNING FIELD-SYMBOL(<ls_futures>).
      lo_run_bp_maintain ?= <ls_futures>->get( ).
      me->gt_bp_data[ partner_guid =  lo_run_bp_maintain->gt_bp_data-partner_guid ] = lo_run_bp_maintain->gt_bp_data.
    ENDLOOP.

  ENDMETHOD.

*&----------------------------------------------------------------------*
*& METHOD ALV_CREATE
*&----------------------------------------------------------------------*
  METHOD alv_create.

    DATA:
      lo_events TYPE REF TO cl_salv_events_table.

    TRY.

        " Create object ALV TABLE
        cl_salv_table=>factory(
          IMPORTING
            r_salv_table = me->go_alv_table " Basis Class Simple ALV Tables
          CHANGING
            t_table      = me->gt_alv ).

        me->go_alv_table->get_functions( )->set_all( abap_true )..

        " Set Display Settings
        me->go_alv_table->get_display_settings( )->set_horizontal_lines( abap_on ).
        me->go_alv_table->get_display_settings( )->set_striped_pattern( abap_on ).

        " Set Functional Settings
        me->go_alv_table->get_functional_settings( )->set_sort_on_header_click( abap_on ).
        me->go_alv_table->get_selections( )->set_selection_mode( if_salv_c_selection_mode=>single ).

        " Set Columns Parameters
        me->alv_set_columns( ).


        "events
        lo_events = me->go_alv_table->get_event( ).
        SET HANDLER me->on_alv_link_click       FOR lo_events.
*        SET HANDLER me->on_alv_double_click     FOR lo_events.
*        SET HANDLER me->on_alv_user_command     FOR lo_events.

      CATCH cx_root.
        " Deu ruim!

    ENDTRY.

  ENDMETHOD.

*&----------------------------------------------------------------------*
*& METHOD ALV_SET COLUMNS
*&----------------------------------------------------------------------*
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

*&----------------------------------------------------------------------*
*& METHOD ALV_DISPLAY
*&----------------------------------------------------------------------*
  METHOD alv_display.

    GET RUN TIME FIELD me->gs_runtime-t2_finish. "Get time for processing
    me->gs_runtime-t2_finish = me->gs_runtime-t2_finish / 1000000.

    DATA(lo_alv_top_of_list) = NEW cl_salv_form_layout_grid( columns = 2 ).
    DATA(lo_logo) = NEW cl_salv_form_layout_logo( ).

    " Header Top Of Page
    me->go_alv_table->set_top_of_list( value = lo_alv_top_of_list ).
    lo_alv_top_of_list->create_label( row = 1 column  = 1 text = TEXT-t05  ). "MBO

    IF me->gs_parameters-test EQ abap_on.
      lo_alv_top_of_list->create_label( row = 2 column  = 1 text = 'Execução de TESTE'(005)  ).
    ELSE.
      lo_alv_top_of_list->create_label( row = 2 column  = 1 text = 'Execução de efetiva'(006)  ).
    ENDIF.

    lo_alv_top_of_list->create_flow( row = 3 column  = 1 )->create_label( text = 'Usuário:'(016) ).
    lo_alv_top_of_list->create_flow( row = 3 column  = 2 )->create_text(  text = |{ syst-uname }| ).

    lo_alv_top_of_list->create_flow( row = 4 column  = 1 )->create_label( text = |{ 'Data de execução:'(007) }| ).
    lo_alv_top_of_list->create_flow( row = 4 column  = 2 )->create_text(  text = |{ sy-datlo DATE = USER } { syst-uzeit TIME = USER }| ).

    lo_alv_top_of_list->create_flow( row = 5 column  = 1 )->create_label( text = 'Total number of selected records: '(a04) ).
    lo_alv_top_of_list->create_flow( row = 5 column  = 2 )->create_text(  text = lines( me->gt_bp_data ) ).

    lo_alv_top_of_list->create_flow( row = 6 column  = 1 )->create_label( text = 'Tempo de execução: : '(a05) ).
    lo_alv_top_of_list->create_flow( row = 6 column  = 2 )->create_text(  text = |{ me->gs_runtime-t2_finish } segundos| ).


    " set left content
    lo_logo->set_left_content( lo_alv_top_of_list ).

    " set Right Image
    lo_logo->set_right_logo( 'ZTAESA_LOGO_ALV' ).

    " set the top of list using the header for Online.
    me->go_alv_table->set_top_of_list( lo_logo ).

    MOVE-CORRESPONDING me->gt_bp_data TO me->gt_alv.
    me->go_alv_table->display( ).

  ENDMETHOD.

*&----------------------------------------------------------------------*
*& METHOD ON_ALV_LINK_CLICK
*&----------------------------------------------------------------------*
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

*&----------------------------------------------------------------------*
*& METHOD GET_BP_FROM_TAXNUMBER_LC
*&----------------------------------------------------------------------*
  METHOD get_bp_from_taxnumber_lc.

    DATA:
      lt_bp_key              TYPE zcl_bp_standard=>ty_t_bp_key,
      lt_get_bp_data         TYPE zcl_bp_standard=>ty_t_bp,
      lt_selected_local_data TYPE zif_bp_standard=>ty_t_bp_key,
      lr_taxnum              TYPE RANGE OF bptaxnum.

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
        INTO TABLE @lt_selected_local_data
        WHERE bptaxnum~taxnum IN @lr_taxnum.

    LOOP AT lr_taxnum ASSIGNING FIELD-SYMBOL(<lr_taxnum>).

      APPEND INITIAL LINE TO:
        ch_t_bp_data ASSIGNING FIELD-SYMBOL(<ls_bp_data>).

      <ls_bp_data>-taxnum = <lr_taxnum>-low.

      APPEND INITIAL LINE TO <ls_bp_data>-return_bp ASSIGNING FIELD-SYMBOL(<return>).

      " Check if exist in DB
      READ TABLE lt_selected_local_data WITH KEY taxnum = <lr_taxnum>-low ASSIGNING FIELD-SYMBOL(<ls_local_data>).

      IF syst-subrc IS INITIAL. "Found!

        <ls_bp_data>-update = abap_true. " Update data!

        MOVE-CORRESPONDING <ls_local_data> TO <ls_bp_data>.

        APPEND INITIAL LINE TO lt_bp_key ASSIGNING FIELD-SYMBOL(<ls_bp_key>).
        MOVE-CORRESPONDING <ls_bp_data> TO <ls_bp_key>.


        " O PN &1 ( &2 ) já cadastrado.
        MESSAGE s037(z_bupa) WITH <ls_bp_data>-partner <ls_bp_data>-customer INTO <return>-message.
        <ls_bp_data>-message   = <return>-message.
        <ls_bp_data>-status_bp = icon_led_green.

      ELSE. " Not exist in DB

        MESSAGE i201(r1) INTO <return>-message.
        <ls_bp_data>-message   = <return>-message.
        <ls_bp_data>-status_bp = icon_led_red.

      ENDIF.

      <return>-id         = syst-msgid. " Message Class
      <return>-type       = syst-msgty. " Message type: S Success, E Error, W Warning, I Info, A Abort
      <return>-number     = syst-msgno. " Message Number
      <return>-message_v1 = syst-msgv1. " Message Variable
      <return>-message_v2 = syst-msgv2. " Message Variable

    ENDLOOP.


    TRY.
        "Select customer for all lines
        zcl_bp_extend_customer=>get_bp_data_from_customer(
          EXPORTING
            im_t_bp_key  = lt_bp_key
          IMPORTING
            ex_t_bp_data = lt_get_bp_data            ).

      CATCH zcx_abap_error. " Exception Class with its own Constructor
      CATCH cx_root.
    ENDTRY.

    LOOP AT ch_t_bp_data ASSIGNING <ls_bp_data>.
      TRY.
          <ls_bp_data>-partner_bus   = lt_get_bp_data[ partner = <ls_bp_data>-partner ]-partner_bus.
          <ls_bp_data>-customer_cmds = lt_get_bp_data[ partner = <ls_bp_data>-partner ]-customer_cmds.

          <ls_bp_data>-bp_data_current = lt_get_bp_data[ partner = <ls_bp_data>-partner ].

        CATCH cx_root. " Ignores if don't exist in DB
      ENDTRY.

      LOOP AT im_t_upload_file ASSIGNING FIELD-SYMBOL(<ls_general_data>)
          USING KEY taxnum_key WHERE taxnum EQ <ls_bp_data>-taxnum.

        <ls_bp_data>-general_data = <ls_general_data>.
      ENDLOOP.
    ENDLOOP.

  ENDMETHOD.




*&----------------------------------------------------------------------*
*& METHOD CONVERT_CPF_CNPJ
*&----------------------------------------------------------------------*
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

*&----------------------------------------------------------------------*
*& METHOD CONVERT_FIELD_DATE
*&----------------------------------------------------------------------*
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

                zcl_excel_common=>excel_string_to_date(
                  EXPORTING
                    ip_value = lv_cell_date
                  RECEIVING
                    ep_value = <fs_comp>              ).
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

*&----------------------------------------------------------------------*
*& METHOD CALL_COMMIT_AND_CHECK
*&----------------------------------------------------------------------*
  METHOD call_commit_and_check.

    " commit work and wait. " DUMP!
    CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
      EXPORTING
        wait = abap_true.

    " Get BP Number and Vendor
    SELECT SINGLE but~partner cust~customer
    INTO CORRESPONDING FIELDS OF ch_s_data
      FROM but000 AS but
      LEFT JOIN cvi_cust_link AS cust ON but~partner_guid = cust~partner_guid
      WHERE but~partner_guid = ch_s_data-partner_guid .

    APPEND INITIAL LINE TO ch_s_data-return_bp ASSIGNING FIELD-SYMBOL(<data_return>).

    IF ch_s_data-customer IS NOT INITIAL.
      " Business Partner &1 &2 successfully created!
      MESSAGE s038(z_bupa) WITH ch_s_data-partner ch_s_data-vendor INTO <data_return>-message.
      ch_s_data-message        = <data_return>-message.
      ch_s_data-status_bp      = icon_led_green.
    ELSE.
      " PN &1 criado com sucesso! Visão fornecedor não criado, Ver MDS_PPO2! ---->mudar msg para cliente. Verificar
      MESSAGE w039(z_bupa) WITH ch_s_data-partner ch_s_data-customer INTO <data_return>-message.
      ch_s_data-message       = <data_return>-message.
      ch_s_data-status_bp     = icon_led_yellow.
    ENDIF.
    <data_return>-id         = syst-msgid.
    <data_return>-type       = syst-msgty.
    <data_return>-number     = syst-msgno.
    <data_return>-message_v1 = syst-msgv1.
    <data_return>-message_v2 = syst-msgv2.

  ENDMETHOD.

*&----------------------------------------------------------------------*
*& METHOD MSG_INITIALIZE
*&----------------------------------------------------------------------*
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

*&----------------------------------------------------------------------*
*& METHOD MSG_SHOW
*&----------------------------------------------------------------------*
  METHOD msg_show.

    me->msg_initialize( ).

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

*&----------------------------------------------------------------------*
*& METHOD FILL_BANK_KEY
*&----------------------------------------------------------------------*
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

*&----------------------------------------------------------------------*
*& METHOD UPDATE_CUSTOMER
*&----------------------------------------------------------------------*
  METHOD update_customer.

    DATA:
      ls_company TYPE cmds_ei_company,
      ls_sales   TYPE cmds_ei_sales,
      ls_tax_ind TYPE cmds_ei_tax_ind.

    " Assigning fields symbols
    ASSIGN ch_s_data-general_data TO FIELD-SYMBOL(<general_data>).
    ASSIGN ch_s_data-partner_bus-central_data-address-addresses[ data-postal-data-standardaddress = abap_true ] TO FIELD-SYMBOL(<ls_addresses>).

    IF syst-subrc IS INITIAL. "If exist
      IF me->gs_parameters-update EQ abap_true.
        <ls_addresses>-task = me->co_task-update.
      ENDIF.

    ELSEIF me->gs_parameters-update EQ abap_true. "If not exist, create address from upload data
      APPEND INITIAL LINE TO ch_s_data-partner_bus-central_data-address-addresses ASSIGNING <ls_addresses>.
      <ls_addresses>-task = me->co_task-insert.
    ENDIF.

    " Add Update or Insert task to all fields
    IF me->gs_parameters-update EQ abap_true.
      ch_s_data-partner_bus-header-object_task   = me->co_task-update.
      ch_s_data-customer_cmds-header-object_task = me->co_task-update.
    ENDIF.

    " Set fields , avoid errors AUTHORITY-CHECK
    CLEAR:
        ch_s_data-partner_bus-central_data-role-time_dependent,
        ch_s_data-partner_bus-central_data-role-current_state.

*>>>>>>>>>>>>>>>>>>>>>>>>>>>> C E N T R A L  D A T A <<<<<<<<<<<<<<<<<<<<<<<<<<<<
    " Verificar se é PF ou PJ
    me->fill_organization(
      EXPORTING
        im_s_general_data    = <general_data>
      CHANGING
        ch_s_bp_organization = ch_s_data-partner_bus-central_data-common-data-bp_organization ).

    IF me->gs_parameters-test IS NOT INITIAL AND
          <general_data>-name IS NOT INITIAL.
      <ls_addresses>-data-postal-data-c_o_name = me->fill_searchterm( |{ <general_data>-name }| ).
    ENDIF.

    " Avoid Erros, is filled automatically
    CLEAR <ls_addresses>-data-postal-data-taxjurcode.

    " Search term 1 for business partner
    IF <general_data>-serchterm IS NOT INITIAL.
      ch_s_data-partner_bus-central_data-common-data-bp_centraldata-searchterm1 = <general_data>-serchterm.
    ELSEIF ch_s_data-bp_data_current-partner_bus-central_data-common-data-bp_centraldata-searchterm1 IS NOT INITIAL.
      ch_s_data-partner_bus-central_data-common-data-bp_centraldata-searchterm1 =
        ch_s_data-bp_data_current-partner_bus-central_data-common-data-bp_centraldata-searchterm1.
    ELSE.
      ch_s_data-partner_bus-central_data-common-data-bp_centraldata-searchterm1 = me->fill_searchterm( |{ ch_s_data-partner_bus-central_data-common-data-bp_person-fullname }| ).
    ENDIF.

*>>>>>>>>>>>>>>>>>>>>>>>>>>>> A D D R E S S E S <<<<<<<<<<<<<<<<<<<<<<<<<<<<
*    MOVE-CORRESPONDING <general_data> TO <ls_addresses>-data-postal-data.

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

      IF me->gs_parameters-test  IS NOT INITIAL.
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

*>>>>>>>>>>>>>>>>>>>>>>>>>>>> R O L E S  <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<*
    IF NOT line_exists( ch_s_data-partner_bus-central_data-role-roles[ data-rolecategory = co_rolecategory-customer_fin ] ).

      APPEND INITIAL LINE TO ch_s_data-partner_bus-central_data-role-roles ASSIGNING FIELD-SYMBOL(<ls_roles>).

      MOVE co_rolecategory-customer_fin TO:
        <ls_roles>-data-rolecategory,
        <ls_roles>-data_key.
      <ls_roles>-task                   = me->co_task-insert.
      <ls_roles>-data-valid_from        = sy-datum.

    ENDIF.

    IF NOT line_exists( ch_s_data-partner_bus-central_data-role-roles[ data-rolecategory = co_rolecategory-customer_sales ] ).

      APPEND INITIAL LINE TO ch_s_data-partner_bus-central_data-role-roles ASSIGNING <ls_roles>.

      MOVE co_rolecategory-customer_sales TO:
        <ls_roles>-data-rolecategory,
        <ls_roles>-data_key.
      <ls_roles>-task                 = me->co_task-insert.
      <ls_roles>-data-valid_from      = sy-datum.

    ENDIF.

*>>>>>>>>>>>>>>>>>>>>>>>>>>>> C O M P A N Y <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<*
    IF ch_s_data-customer_cmds-company_data-company IS INITIAL.
      me->fill_company_data( CHANGING ch_data = ch_s_data ).
    ELSEIF me->gs_parameters-update EQ abap_true.

      ls_company-task       = me->co_task-update.
      ls_company-data-zwels = me->gs_parameters-zwels.
      ls_company-data-fdgrv = me->gs_parameters-fdgrv.
      ls_company-data-zterm = me->gs_parameters-zterm.
      ls_company-data-akont = me->gs_parameters-akont.

      MOVE-CORRESPONDING ch_s_data-general_data TO ch_s_data-customer_cmds-central_data-central-data.
      MODIFY ch_s_data-customer_cmds-company_data-company FROM ls_company TRANSPORTING task data WHERE task IS INITIAL.

    ENDIF.

*>>>>>>>>>>>>>>>>>>>>>> S A L E S <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<*
    IF ch_s_data-customer_cmds-sales_data-sales IS INITIAL AND
       me->gs_parameters-update EQ abap_true.
      me->fill_sales_data(   CHANGING ch_data = ch_s_data ).

    ELSE.
      ls_sales-task             = me->co_task-update.
      ls_sales-data-kalks       = me->gs_parameters-kalks.
      ls_sales-data-ktgrd       = me->gs_parameters-ktgrd.
      ls_sales-data-vsbed       = me->gs_parameters-vsbed.
      ls_tax_ind-task           = me->co_task-update.
      ls_tax_ind-data_key-tatyp = me->gs_parameters-tatyp.    "'IBRX'.
      ls_tax_ind-data-taxkd     = me->gs_parameters-taxkd.

      MODIFY ch_s_data-customer_cmds-sales_data-sales             FROM ls_sales   TRANSPORTING task data WHERE task IS INITIAL.
      MODIFY ch_s_data-customer_cmds-central_data-tax_ind-tax_ind FROM ls_tax_ind TRANSPORTING task data WHERE task IS INITIAL.

    ENDIF.

*>>>>>>>>>>>>>>>>>>>>>> C O M P A R E  D A T A  <<<<<<<<<<<<<<<<<<<<*
    me->compare_bp_source_fill_datax(
      CHANGING
        ch_s_source = ch_s_data    ).


  ENDMETHOD.

*&----------------------------------------------------------------------*
*& METHOD COMPARE_BP_SOURCE_FILL_DATAX
*&----------------------------------------------------------------------*
  METHOD compare_bp_source_fill_datax.

    DATA:
      lt_bp_key          TYPE zif_bp_standard=>ty_t_bp_key,
      lt_bp_data_current TYPE zif_bp_standard=>ty_t_bp,
      ls_bp_current      TYPE bus_ei_main.

    IF me->gs_parameters-update EQ abap_false.

      ch_s_source-customer_cmds-header-object_task = me->co_task-current_state.
      ch_s_source-partner_bus-header-object_task   = me->co_task-current_state.

      CLEAR:
      ch_s_source-partner_bus-central_data-common-datax-bp_centraldata,
      ch_s_source-partner_bus-central_data-common-datax-bp_organization,
      ch_s_source-customer_cmds-central_data-central-datax.

      LOOP AT ch_s_source-partner_bus-central_data-address-addresses ASSIGNING FIELD-SYMBOL(<ls_adress_source>).
        CLEAR <ls_adress_source>-data-postal-datax.
      ENDLOOP.

      LOOP AT ch_s_source-customer_cmds-company_data-company ASSIGNING FIELD-SYMBOL(<ls_company>).
        CLEAR <ls_company>-datax.
      ENDLOOP.

      RETURN.

    ENDIF.

    " Get current recorded BP data
    APPEND INITIAL LINE TO lt_bp_key ASSIGNING FIELD-SYMBOL(<ls_bp_key>).
    MOVE-CORRESPONDING ch_s_source TO <ls_bp_key>.

*    zcl_bp_extend_customer=>get_bp_data_from_customer(
*      EXPORTING
*        im_t_bp_key  = lt_bp_key
*      IMPORTING
*        ex_t_bp_data = lt_bp_data_current   ).

*    ASSIGN lt_bp_data_current[ 1 ] TO FIELD-SYMBOL(<ls_bp_current>).

    " Check change in central_data
    me->compare_data(
      EXPORTING
        im_s_source  = ch_s_source-partner_bus-central_data-common-data-bp_centraldata
        im_s_current = ch_s_source-bp_data_current-partner_bus-central_data-common-data-bp_centraldata
      CHANGING
        ch_s_data_x  = ch_s_source-partner_bus-central_data-common-datax-bp_centraldata ).


    "Check for change in bp_organization
    me->compare_data(
      EXPORTING
        im_s_source  = ch_s_source-partner_bus-central_data-common-data-bp_organization
        im_s_current = ch_s_source-bp_data_current-partner_bus-central_data-common-data-bp_organization
      CHANGING
        ch_s_data_x  = ch_s_source-partner_bus-central_data-common-datax-bp_organization ).

    " Avoid Error
    IF ch_s_source-partner_bus-central_data-common-datax-bp_organization IS INITIAL.
      ch_s_source-partner_bus-central_data-common-datax-bp_organization-name1 = abap_on.
    ENDIF.


    " Verify change address

    ASSIGN ch_s_source-bp_data_current-partner_bus-central_data-address-addresses[ 1 ]  TO FIELD-SYMBOL(<ls_adress_current>).
    ASSIGN ch_s_source-partner_bus-central_data-address-addresses[ 1 ]                  TO <ls_adress_source>.

    IF <ls_adress_current> IS ASSIGNED AND <ls_adress_source> IS ASSIGNED.

      " Postal
      me->compare_data(
        EXPORTING
          im_s_source  = <ls_adress_source>-data-postal-data
          im_s_current = <ls_adress_current>-data-postal-data
        CHANGING
          ch_s_data_x  = <ls_adress_source>-data-postal-datax ).

      IF <ls_adress_source>-data-postal-datax IS NOT INITIAL.
        <ls_adress_source>-data_key-guid = <ls_adress_current>-data_key-guid.
        <ls_adress_source>-task          = me->co_task-update.
      ENDIF.

    ENDIF.


    " Check Insc State
    ASSIGN ch_s_source-partner_bus-central_data-taxnumber-taxnumbers[ data_key-taxtype = me->co_taxtype-ins_stadual ] TO FIELD-SYMBOL(<ls_source_insc_state>).

    IF line_exists( ch_s_source-bp_data_current-partner_bus-central_data-taxnumber-taxnumbers[ data_key-taxtype = me->co_taxtype-ins_stadual ] ) AND
        <ls_source_insc_state> IS ASSIGNED.
      <ls_source_insc_state>-task = me->co_task-update.
    ENDIF.


    " Check Insc Municipal
    ASSIGN ch_s_source-partner_bus-central_data-taxnumber-taxnumbers[ data_key-taxtype = me->co_taxtype-ins_municipal ] TO FIELD-SYMBOL(<ls_source_ins_municipal>).

    IF line_exists( ch_s_source-bp_data_current-partner_bus-central_data-taxnumber-taxnumbers[ data_key-taxtype = me->co_taxtype-ins_municipal ] ) AND
       <ls_source_ins_municipal> IS ASSIGNED.
      <ls_source_ins_municipal>-task = me->co_task-update.
    ENDIF.


    " Check change in Customer central_data
    me->compare_data(
      EXPORTING
        im_s_source  = ch_s_source-customer_cmds-central_data-central-data
        im_s_current = ch_s_source-bp_data_current-customer_cmds-central_data-central-data
      CHANGING
        ch_s_data_x  = ch_s_source-customer_cmds-central_data-central-datax ).


    " Check change in Customer central_data
    LOOP AT ch_s_source-customer_cmds-company_data-company ASSIGNING <ls_company>.
      TRY.
          me->compare_data(
            EXPORTING
              im_s_source  = ch_s_source-customer_cmds-company_data-company[ data_key-bukrs = <ls_company>-data_key-bukrs ]-data
              im_s_current = <ls_company>-data
            CHANGING
              ch_s_data_x  = <ls_company>-datax ).
        CATCH cx_sy_itab_line_not_found.
          CONTINUE.
      ENDTRY.
    ENDLOOP.

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

      ASSIGN COMPONENT:
        <ls_field>-name OF STRUCTURE im_s_current TO FIELD-SYMBOL(<lv_component_current>),
        <ls_field>-name OF STRUCTURE im_s_source  TO FIELD-SYMBOL(<lv_component_source>),
        <ls_field>-name OF STRUCTURE ch_s_data_x  TO FIELD-SYMBOL(<lv_component_x>).

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
*& METHOD FILL_CENTRAL_DATA
*&----------------------------------------------------------------------*
  METHOD fill_central_data.

    ch_data-partner_guid                                                = me->create_bp_partner_guid( ).
    ch_data-partner_bus-header-object_instance-bpartnerguid             = ch_data-partner_guid.
    ch_data-partner_bus-central_data-common-data-bp_control-category    = me->co_category_bp_as-org.

    fill_organization(
      EXPORTING
        im_s_general_data    = ch_data-general_data
      CHANGING
        ch_s_bp_organization = ch_data-partner_bus-central_data-common-data-bp_organization       ).

    " Business Partner Grouping
    IF p_group IS INITIAL.
      ch_data-partner_bus-central_data-common-data-bp_control-grouping        = me->co_grouping-nac_pj.
    ELSE.
      ch_data-partner_bus-central_data-common-data-bp_control-grouping        = me->gs_parameters-group.
    ENDIF.

    " Search term 1 for business partner
    IF ch_data-general_data-serchterm IS NOT INITIAL.
      ch_data-partner_bus-central_data-common-data-bp_centraldata-searchterm1 = ch_data-general_data-serchterm.
    ELSE.
      ch_data-partner_bus-central_data-common-data-bp_centraldata-searchterm1 = me->fill_searchterm( |{ ch_data-general_data-name }| ).
    ENDIF.

  ENDMETHOD.

*&----------------------------------------------------------------------*
*& METHOD FILL_ORGANIZATION
*&----------------------------------------------------------------------*
  METHOD fill_organization.

    DATA:
      lt_text_tab TYPE TABLE OF char255,
      lv_text     TYPE string.

    CHECK im_s_general_data-name IS NOT INITIAL.

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


*&----------------------------------------------------------------------*
*& METHOD FILL_SEARCHTERM
*&----------------------------------------------------------------------*
  METHOD fill_searchterm.

    DATA:
      lt_text_tab    TYPE TABLE OF char255.

    TRY.
        CALL FUNCTION 'SOTR_SERV_STRING_TO_TABLE'
          EXPORTING
            text        = im_v_name
            line_length = 15
          TABLES
            text_tab    = lt_text_tab.

        r_result = lt_text_tab[ 1 ].

      CATCH cx_root. "On error ignore
    ENDTRY.

  ENDMETHOD.

*&----------------------------------------------------------------------*
*& METHOD FILL_TAX_NUMBERS
*&----------------------------------------------------------------------*
  METHOD fill_tax_numbers.

    " CNPJ
    APPEND INITIAL LINE TO:
    ch_data-partner_bus-central_data-taxnumber-taxnumbers ASSIGNING FIELD-SYMBOL(<ls_cnpj>).
    <ls_cnpj>-task                            = me->co_task-insert.
    <ls_cnpj>-data_key-taxtype                = me->co_taxtype-cnpj.
    <ls_cnpj>-data_key-taxnumber              = ch_data-taxnum.


    " Inscrição Estadual
    IF ch_data-general_data-insc_est IS NOT INITIAL.
      APPEND INITIAL LINE TO:
      ch_data-partner_bus-central_data-taxnumber-taxnumbers ASSIGNING FIELD-SYMBOL(<ls_insc_estadual>).
      <ls_insc_estadual>-task                 = me->co_task-insert.
      <ls_insc_estadual>-data_key-taxnumber   = ch_data-general_data-insc_est.
      <ls_insc_estadual>-data_key-taxtype     = me->co_taxtype-ins_stadual.
    ENDIF.

    " Inscrição Municipal
    IF ch_data-general_data-insc_mun IS NOT INITIAL.
      APPEND INITIAL LINE TO:
      ch_data-partner_bus-central_data-taxnumber-taxnumbers ASSIGNING FIELD-SYMBOL(<ls_insc_municipal>).
      <ls_insc_municipal>-task                = me->co_task-insert.
      <ls_insc_municipal>-data_key-taxnumber  = ch_data-general_data-insc_mun.
      <ls_insc_municipal>-data_key-taxtype    = me->co_taxtype-ins_municipal.
    ENDIF.

  ENDMETHOD.

*&----------------------------------------------------------------------*
*& METHOD FILL_BANK_DETAILS
*&----------------------------------------------------------------------*
  METHOD fill_bank_details.

    IF ch_data-general_data-banco IS NOT INITIAL.

      APPEND INITIAL LINE TO ch_data-partner_bus-central_data-bankdetail-bankdetails ASSIGNING FIELD-SYMBOL(<ls_bankdetails>).

      <ls_bankdetails>-task           = me->co_task-insert. " I.
      <ls_bankdetails>-data-bank_ctry = 'BR'.
      <ls_bankdetails>-data-bank_key  = me->fill_bank_key( ch_data-general_data ).
      <ls_bankdetails>-data-bank_acct = ch_data-general_data-bankn.     " Bank account number
      <ls_bankdetails>-data-ctrl_key  = ch_data-general_data-ctrl_key.  " Bank Control Key

      IF me->gs_parameters-test  IS NOT INITIAL.
        <ls_bankdetails>-data_key     = ch_data-general_data-banco. " Avoid msg erros in test
      ENDIF.

    ENDIF.

  ENDMETHOD.

*&----------------------------------------------------------------------*
*& METHOD FILL_COMPANY_DATA
*&----------------------------------------------------------------------*
  METHOD fill_company_data.

    DATA: ls_company   TYPE cmds_ei_company.

    " Add Insert task
    ls_company-task = me->co_task-insert.

    ls_company-data-zwels = me->gs_parameters-zwels.  " List of Respected Payment Methods

    MOVE-CORRESPONDING ch_data-general_data TO ch_data-customer_cmds-central_data-central-data.

    IF me->gs_parameters-fdgrv IS INITIAL.
      ls_company-data-fdgrv = me->co_vendor_planning_group-pj.  " Planning Group
    ELSE.
      ls_company-data-fdgrv = me->gs_parameters-fdgrv.
    ENDIF.

    IF me->gs_parameters-zterm IS INITIAL.
      ls_company-data-zterm = me->gc_parameters-zterm.          " Terms of Payment Key
    ELSE.
      ls_company-data-zterm = me->gs_parameters-zterm.
    ENDIF.

    IF me->gs_parameters-akont IS INITIAL.
      ls_company-data-akont = me->gc_parameters-akont.          " Reconciliation Account in General Ledger
    ELSE.
      ls_company-data-akont = me->gs_parameters-akont.
    ENDIF.

    " fill data for every company
    LOOP AT me->gs_parameters-t_bukrs ASSIGNING FIELD-SYMBOL(<ls_bukrs>).
      ls_company-data_key-bukrs = <ls_bukrs>-bukrs.
      APPEND ls_company TO ch_data-customer_cmds-company_data-company.
    ENDLOOP.

    " If don't have any company, the standard is TB01 TAESA.
    IF syst-subrc IS NOT INITIAL.
      ls_company-data_key-bukrs = 'TB01'.
      APPEND ls_company TO ch_data-customer_cmds-company_data-company.
    ENDIF.

  ENDMETHOD.

*&----------------------------------------------------------------------*
*& METHOD FILL_ADDRESSES
*&----------------------------------------------------------------------*
  METHOD fill_addresses.

    " Assigning fields symbols
    APPEND INITIAL LINE TO:
      ch_data-partner_bus-central_data-address-addresses        ASSIGNING FIELD-SYMBOL(<ls_addresses>).

    " Add Insert task
    <ls_addresses>-task = me->co_task-insert.

    MOVE-CORRESPONDING ch_data-general_data TO <ls_addresses>-data-postal-data.
    <ls_addresses>-data-postal-data-country  = me->co_country_iso_br.
    <ls_addresses>-data-postal-data-languiso = me->co_languiso_pt.
    <ls_addresses>-data-postal-data-langu    = me->co_languiso_pt.

    " Telephone
    IF ch_data-general_data-tel_number IS NOT INITIAL.
      APPEND INITIAL LINE TO
        ch_data-partner_bus-central_data-communication-phone-phone ASSIGNING FIELD-SYMBOL(<phone>).
      <phone>-contact-task           = me->co_task-insert. " I
      <phone>-contact-data-country   = me->co_country_iso_br.
      <phone>-contact-data-telephone = ch_data-general_data-tel_number.
      <phone>-contact-data-r_3_user  = '1'. " Default
    ENDIF.

    " Email
    IF ch_data-general_data-smtp_addr IS NOT INITIAL.
      APPEND INITIAL LINE TO
        ch_data-partner_bus-central_data-communication-smtp-smtp ASSIGNING FIELD-SYMBOL(<email>).
      <email>-contact-task        = me->co_task-insert. " I.
      <email>-contact-data-e_mail = ch_data-general_data-smtp_addr.
    ENDIF.

  ENDMETHOD.

*&----------------------------------------------------------------------*
*& METHOD FILL_SALES_DATA
*&----------------------------------------------------------------------*
  METHOD fill_sales_data.

    DATA:
      ls_sales     TYPE cmds_ei_sales,
      ls_functions TYPE cmds_ei_functions,    " Partner Roles
      ls_tax_ind   TYPE cmds_ei_tax_ind.

    MOVE-CORRESPONDING ch_data-general_data TO ls_sales-data_key.

    IF me->gs_parameters-icmstx IS INITIAL.
      ch_data-customer_cmds-central_data-central-data-icmstaxpay = 'NC'.
    ELSE.
      ch_data-customer_cmds-central_data-central-data-icmstaxpay = me->gs_parameters-icmstx.
    ENDIF.

    ls_sales-task             = me->co_task-insert.
    ls_tax_ind-task           = me->co_task-insert.
    ls_tax_ind-data_key-aland = 'BR'.
    ls_tax_ind-data_key-tatyp = me->gs_parameters-tatyp.    "'IBRX'.
    ls_tax_ind-data-taxkd     = me->gs_parameters-taxkd.

    APPEND ls_tax_ind TO ch_data-customer_cmds-central_data-tax_ind-tax_ind.

    zcl_utils=>get_parameter( EXPORTING i_par_id = me->gc_parameters-tvarv_incoterms IMPORTING e_value = ls_sales-data-inco1 ).
    ls_sales-data-inco2   = ch_data-general_data-city.
    ls_sales-data-inco2_l = ch_data-general_data-city.

    " Account Assignment Group for this customer
    IF ch_data-general_data-ktgrd IS INITIAL.
      ls_sales-data-ktgrd = me->gs_parameters-ktgrd.
    ELSE.
      ls_sales-data-ktgrd = ch_data-general_data-ktgrd.
    ENDIF.

    ls_sales-data-kalks = me->gs_parameters-kalks.  " Customer Classification for Pricing Procedure Determination
    ls_sales-data-vsbed = me->gs_parameters-vsbed.  " Shipping Conditions

    "  Sales Organization
    LOOP AT me->gs_parameters-t_vkorg ASSIGNING FIELD-SYMBOL(<vkorg>).

      ls_sales-data_key-vkorg     = <vkorg>-vkorg.              " Sales Organization
      ls_sales-data_key-vtweg     = me->gs_parameters-vtweg.    " Distribution Channel
      ls_sales-data_key-spart     = me->gs_parameters-spart.    " Division

      ls_functions-task           = me->co_task-insert.
      ls_functions-data_key-parvw = 'AG'.
      APPEND ls_functions TO ls_sales-functions-functions.

      ls_functions-task           = me->co_task-insert.
      ls_functions-data_key-parvw = 'RE'.
      APPEND ls_functions TO ls_sales-functions-functions.

      ls_functions-task           = me->co_task-insert.
      ls_functions-data_key-parvw = 'RG'.
      APPEND ls_functions TO ls_sales-functions-functions.

      ls_functions-task           = me->co_task-insert.
      ls_functions-data_key-parvw = 'WE'.
      APPEND ls_functions TO ls_sales-functions-functions.

      APPEND ls_sales TO ch_data-customer_cmds-sales_data-sales.

    ENDLOOP.

  ENDMETHOD.

*&----------------------------------------------------------------------*
*& METHOD SPBT_INITIALIZE
*&----------------------------------------------------------------------*
  METHOD spbt_initialize.

    DATA:
      lv_max_pbt_wps  TYPE i,
      lv_free_pbt_wps TYPE i,
      lv_rfcdest      TYPE rfcdest.

    "Initialization of PBT environment Parallel Processing
    CALL FUNCTION 'SPBT_INITIALIZE'
      EXPORTING
        group_name                     = space            " PBT server group name
      IMPORTING
        max_pbt_wps                    = lv_max_pbt_wps   " Max. no. of PBT resources in the system
        free_pbt_wps                   = lv_free_pbt_wps  " PBT resources that are currently free
      EXCEPTIONS
        invalid_group_name             = 1                " PBT server group name is invalid
        internal_error                 = 2                " Internal error has occurred (see sysLog)
        pbt_env_already_initialized    = 3                " PBT environment is already initialized for group
        currently_no_resources_avail   = 4                " All PBT resources are currently busy
        no_pbt_resources_found         = 5                " No PBT resources configured in the system
        cant_init_different_pbt_groups = 6                " Diff. PBT groups cannot be initialized
        OTHERS                         = 7.

    IF lv_max_pbt_wps LT me->gs_parameters-maxprc.
      me->gs_parameters-maxprc = lv_max_pbt_wps.
    ENDIF.

    IF me->gs_parameters-maxprc IS INITIAL.
      me->gs_parameters-maxprc = 1.
    ENDIF.

    CALL FUNCTION 'SPBT_GET_PP_DESTINATION'
      IMPORTING
        rfcdest = lv_rfcdest.                 " Selected Destination for Parallel Processing

  ENDMETHOD.


  METHOD catch_exception_save_msg.

    DATA: lv_program_name TYPE syrepid,
          lv_include_name TYPE syrepid,
          lv_source_line  TYPE i.

    cs_bp_data-message = io_exception->get_text( ).

    io_exception->get_source_position(
      IMPORTING
        program_name = lv_program_name                 " ABAP Program: Current Master Program
        include_name = lv_include_name
        source_line  = lv_source_line   ).

    " Ocorreu um exceção do tipo &1
    MESSAGE a059(z_bupa) INTO DATA(lv_msg_dummy). "For Where used list!

    APPEND INITIAL LINE TO cs_bp_data-return_bp ASSIGNING FIELD-SYMBOL(<ls_return_bp>).
    <ls_return_bp>-id     = 'Z_BUPA'.
    <ls_return_bp>-number = '059'.
    <ls_return_bp>-type   = 'A'.
    <ls_return_bp>-message_v1 = io_exception->kernel_errid.
    <ls_return_bp>-message_v2 = lv_program_name.
    <ls_return_bp>-message_v3 = lv_include_name.
    <ls_return_bp>-message_v4 = lv_source_line.

  ENDMETHOD.

  METHOD at_selection_screen.

    DATA:
      lv_free_pbt_workprocess  TYPE i,
      lv_total_pbt_workprocess TYPE i,
      lt_instances             TYPE STANDARD TABLE OF rzlliapsrv WITH DEFAULT KEY,
      lv_server_to_check       TYPE msxxlist-name.

    CASE sscrfields-ucomm.
      WHEN'FC01'. " Download File Model
        lcl_zbupa_0003=>export_model( ).
    ENDCASE.

    CALL FUNCTION 'SMLG_GET_DEFINED_SERVERS'
      TABLES
        instances          = lt_instances     " Instances for the Specified Group
      EXCEPTIONS
        invalid_group_type = 1                " Invalid Group Type
        no_instances_found = 2                " No Instances Exist For the Specified Group
        OTHERS             = 3.

    CHECK syst-subrc IS INITIAL.

    lv_server_to_check = lt_instances[ 1 ]-applserver.

    CALL FUNCTION 'TH_ARFC_REQUESTS'
      EXPORTING
        server   = lv_server_to_check   " Application Server Name
        priority = 1                    " Normal
      IMPORTING
        noreq    = lv_free_pbt_workprocess
        maxreq   = lv_total_pbt_workprocess
      EXCEPTIONS
        OTHERS   = 1.

    stxt1 = 'Este programa utiliza execução paralelizada por processos operacionais.'(008).
    stxt2 = |{ 'Diálogos disponíveis atualmente:'(009) } { lv_free_pbt_workprocess } { 'de um total:'(010) } { lv_total_pbt_workprocess }|.

  ENDMETHOD.

ENDCLASS.
