
*--------------------------------------------------------------------*
*               P R O J E T O    A G I R - T A E S A                 *
*--------------------------------------------------------------------*
* Consultoria .....: I N T E C H P R O                               *
* Res. ABAP........: Marcelo Alvares                                 *
* Res. Funcional...: Bernardo Torres                                 *
* Módulo...........: BUPA Business Partner                           *
* Programa.........: ZBUPA_CARGA_ONS                                 *
* Transação........: ZCDC                                            *
* Tipo de Programa.: REPORT                                          *
* Request     .....: S4DK901018                                      *
* Objetivo.........: Cadastro/Atualização de clientes por carga - ONS*
*--------------------------------------------------------------------*
* Change Control:                                                    *
* Version | Date      | Who                 |   What                 *
*    1.00 | 30.06.18  | Marcelo Alvares     |   Versão Inicial       *
*    1.01 | 12.08.18  | Marcelo Alvares     |   Ajustes diversos     *
*    1.02 | 13.09.18  | Marcelo Alvares     |   Ajustes Performance  *
*    1.03 | 02.10.18  | Marcelo Alvares     |   Ajustes Perf. e msg  *
*    1.04 | 09.11.18  | Marcelo Alvares     |   Aj. Msg retorno      *
*    1.05 | 30.11.18  | Marcelo Alvares     |   Ajs Diversos Perfor  *
*    1.06 | 30.11.18  | Marcelo Alvares     |   Contato              *
*    1.07 | 30.01.19  | Marcelo Alvares     |   Ch 1000000572        *
*    1.08 | 31.01.19  | Marcelo Alvares     |   Ch 1000000572        *
*    1.09 | 01.02.19  | Marcelo Alvares     |   Ch 1000000572        *
**********************************************************************
*&---------------------------------------------------------------------*
*& Include          ZBUPA_CARGA_ONS_CLASS
*&---------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*& CLASS DEFINITION CL_FILE
*&---------------------------------------------------------------------*
CLASS lcl_file DEFINITION FINAL FRIENDS lcl_messages lcl_bupa lcl_alv.
  PUBLIC SECTION.
    CONSTANTS:
        co_parameter_id     TYPE memoryid   VALUE 'ZBUPA_FILE_01'.

    CLASS-METHODS:
      export_model,
      get_init_filename
        EXPORTING
          ex_v_dirup        TYPE string
          ex_v_dirdown      TYPE string
        RETURNING
          VALUE(r_filename) TYPE string,
      select_file
        RETURNING
          VALUE(r_filename) TYPE file_table-filename,
      set_parameter_id
        IMPORTING
          im_v_file TYPE rlgrap-filename,
      set_sscrtexts.

    METHODS:
      upload
        EXCEPTIONS
          conversion_failed
          upload_date_not_found.

    DATA:
      table_excel TYPE STANDARD TABLE OF ty_e_upload_layout.

    DATA:
      upload_data LIKE s_upload_data.

ENDCLASS.           "lcl_file
*&---------------------------------------------------------------------*
*&CLASS DEFINITION MESSAGES
*&---------------------------------------------------------------------*
CLASS lcl_messages DEFINITION FINAL FRIENDS lcl_bupa lcl_alv lcl_file.
  PUBLIC SECTION.
    CLASS-METHODS:
      initialize,
      show  IMPORTING im_v_line  TYPE any OPTIONAL,
      store IMPORTING im_v_zeile TYPE any,
      store_validate
        IMPORTING
          im_t_return_map TYPE mdg_bs_bp_msgmap_t
          im_v_index_line TYPE any,
      progress_indicator
        IMPORTING
          VALUE(im_v_text) TYPE any
          im_v_processed   TYPE syst-tabix OPTIONAL
          im_v_total       TYPE syst-tabix OPTIONAL.

ENDCLASS.

*&---------------------------------------------------------------------*
*& Class definition cl_bupa
*&---------------------------------------------------------------------*
CLASS lcl_bupa DEFINITION FINAL FRIENDS lcl_file lcl_messages lcl_alv.
  PUBLIC SECTION.

    CONSTANTS:
      co_grouping_naco            TYPE bu_group           VALUE 'NACO',       "Clientes ONS
      co_languiso_pt              TYPE laiso              VALUE 'PT',         "Código de idiomas ISO de 2 dígitos
      co_object_task_insert       TYPE bus_ei_object_task VALUE 'I',          "Insert
      co_object_task_update       TYPE bus_ei_object_task VALUE 'U',          "Update
      co_rolecategory_client      TYPE bu_partnerrolecat  VALUE 'FLCU00',     "Cliente (contab.financ.)
      co_rolecategory_vendas      TYPE bu_partnerrolecat  VALUE 'FLCU01',     "
      co_rolecategory_contact     TYPE bu_partnerrolecat  VALUE 'BUP001',     "Pessoa de contato
      co_taxtype_cnpj             TYPE bptaxtype          VALUE 'BR1',        "BR1 Brasil: nº CNPJ
      co_taxtype_insc_estadual    TYPE bptaxtype          VALUE 'BR3',        "BR3 Brasil: inscrição estadual
      co_title_key_3              TYPE ad_title           VALUE '0003',       "Forma de Tratamento "Empresa"
      co_pais_iso_br              TYPE intca              VALUE 'BR',         "Codigo ISO BR telefone
      co_bp_relationship_category TYPE bu_reltyp          VALUE 'BUR001',
      co_nc                       TYPE c LENGTH 15        VALUE 'ZSD_INSC_STA_NA',
      co_no                       TYPE c LENGTH 12        VALUE 'ZSD_INSC_STA',
      co_inco1                    TYPE c LENGTH 10        VALUE 'ZSD_INCONT',
      co_inco2                    TYPE c LENGTH 15        VALUE 'ZSD_INCONT_LOC',
      co_ktgrd                    TYPE c LENGTH 14        VALUE 'ZSD_GRPO_CLAS',
      co_class_fis_cli            TYPE c LENGTH 17        VALUE 'ZSD_CLASS_FIS_CLI',
      co_clas_fis                 TYPE c LENGTH 13        VALUE 'ZSD_CLASS_FIS',
      co_cond_exp                 TYPE c LENGTH 12        VALUE 'ZSD_COND_EXP'.

    DATA:
      o_file  TYPE REF TO lcl_file,
      t_alv   TYPE TABLE OF ty_e_alv  WITH NON-UNIQUE SORTED KEY key_codigo COMPONENTS codigo,
      t_bukrs TYPE TABLE OF ty_e_t001 WITH UNIQUE     SORTED KEY key_bukrs  COMPONENTS bukrs.

    CLASS-METHODS:
      get_bp_from_kunnr
        IMPORTING
          im_v_kunnr         TYPE any OPTIONAL
        EXPORTING
          ex_v_kunnr         TYPE kunnr
          ex_v_bp_guid       TYPE bu_partner_guid
          ex_v_bp_number     TYPE bu_partner
        RETURNING
          VALUE(r_bp_number) TYPE bu_partner,
      get_guid
        RETURNING
          VALUE(r_result) TYPE string,
      set_commit.

    METHODS:
      constructor
        EXCEPTIONS upload_error ,
      get_first_name
        IMPORTING
          im_v_name_rep       TYPE c
        RETURNING
          VALUE(r_first_name) TYPE string,
      get_last_name
        IMPORTING
          im_v_name_rep      TYPE c
        RETURNING
          VALUE(r_last_name) TYPE string,
      get_alv_tabix
        IMPORTING
          im_v_number    TYPE kunnr
        RETURNING
          VALUE(r_tabix) TYPE i,
      effective_load,
      get_test RETURNING VALUE(r_result) TYPE xtest,
      set_test IMPORTING i_test          TYPE xtest,
      get_time_finish RETURNING VALUE(r_result)  LIKE syst-timlo,
      set_time_finish IMPORTING im_v_time_finish LIKE syst-timlo,
      get_time_start RETURNING VALUE(r_result)   LIKE syst-timlo,
      get_elapsed_time
        RETURNING
          VALUE(r_return) TYPE i.

  PRIVATE SECTION.

    DATA:
      it_bp_numbers LIKE STANDARD TABLE OF s_bp_numbers,
      it_zj1btxavc  TYPE STANDARD TABLE OF zj1btxavc,
      wa_zj1btxavc  LIKE LINE OF it_zj1btxavc,
      it_zj1btxcli  TYPE STANDARD TABLE OF zj1btxcli,
      wa_zj1btxcli  LIKE LINE OF it_zj1btxcli,
      time_ms       TYPE i,
      time_start    LIKE syst-timlo,
      time_finish   LIKE syst-timlo,
      test          TYPE xtest VALUE abap_true,
      bp_numbers    LIKE s_bp_numbers.

    CLASS-METHODS:
      get_bp_numbers
        IMPORTING
          VALUE(im_v_bu_bpext) TYPE any OPTIONAL
        CHANGING
          ch_v_bp_guid         TYPE bu_partner_guid OPTIONAL
          ch_v_bp_number       TYPE bu_partner      OPTIONAL.

    METHODS:
      call_bp_maintain
        IMPORTING
          im_t_cvis_ei_extern TYPE cvis_ei_extern_t
        CHANGING
          ch_s_bp_numbers     LIKE s_bp_numbers,
      change_icon_alv_status
        IMPORTING
          im_v_icon  TYPE tp_icon
          im_v_kunnr TYPE kunnr,
      check_customer_exists
        CHANGING
          ch_s_bp_numbers LIKE s_bp_numbers,
      check_customer_iu_successfully
        IMPORTING
          im_s_bp_numbers LIKE s_bp_numbers,
      compare_bp_source_fill_datax
        CHANGING ch_s_source TYPE cvis_ei_extern,
      compare_data
        IMPORTING
          im_s_source  TYPE any
          im_s_current TYPE any
        CHANGING
          ch_s_data_x  TYPE any,
      create_bp,
      create_contact
        CHANGING
          ch_s_bp_numbers LIKE LINE OF it_bp_numbers,
      fill_address
        IMPORTING
          im_s_addresses  TYPE ty_e_upload_layout
        RETURNING
          VALUE(r_return) TYPE bus_ei_bupa_address_t,
      fill_company_data
        IMPORTING
          im_v_kunnr      TYPE kunnr
          im_codimp       TYPE zj1btaxtyp
        RETURNING
          VALUE(r_result) TYPE cmds_ei_company_t,
      fill_wtax_type
        IMPORTING
                  im_v_kunnr      TYPE kunnr
        RETURNING VALUE(r_result) TYPE cmds_ei_wtax_type_t,
      fill_sales_data
        IMPORTING
          im_v_kunnr      TYPE kunnr
          im_v_bp_number  TYPE bu_partner
        RETURNING
          VALUE(r_result) TYPE cmds_ei_sales_t,
      fill_tax_ind
        IMPORTING
          im_v_kunnr      TYPE kunnr
          im_v_bp_number  TYPE bu_partner
        RETURNING
          VALUE(r_result) TYPE cmds_ei_tax_ind_t,
      fill_contact_addresses
        IMPORTING
          im_s_contactpersons TYPE ty_e_upload_layout
        RETURNING
          VALUE(r_result)     TYPE bus_ei_bupa_address_t,
      fill_organization_name
        IMPORTING
          im_v_razao_social TYPE any
        RETURNING
          VALUE(r_result)   TYPE bus_ei_struc_central_organ,
      get_bp_from_data_base,
      get_bukrs_list,
      get_mestre_de_vendas, "RA 114
      get_estimated_time
        RETURNING
          VALUE(r_result) TYPE string,
      handle_return_messages
        IMPORTING
          VALUE(im_t_bapiretm) TYPE bapiretm
        CHANGING
          ch_s_bp_numbers      LIKE s_bp_numbers,
      remove_agents,
      set_icon_alv_status
        IMPORTING
          im_v_kunnr    TYPE kunnr
          im_t_msg      TYPE ANY TABLE
        RETURNING
          VALUE(r_icon) TYPE tp_icon,
      set_rollback,
      bp_relationship_create
        IMPORTING im_s_bp_numbers LIKE s_bp_numbers,
      convert_time
        IMPORTING
          im_v_micro      TYPE int8  OPTIONAL
          im_v_sec        TYPE i     OPTIONAL
            PREFERRED PARAMETER im_v_micro
        EXPORTING
          ex_v_sec        TYPE i
          ex_v_min        TYPE i
        RETURNING
          VALUE(r_result) TYPE string,
      get_taxjurcode
        IMPORTING
          im_s_addresses  TYPE bus_ei_struc_address
        RETURNING
          VALUE(r_result) TYPE bus_ei_struc_address-taxjurcode.

ENDCLASS.                       "lcl_bupa

*&---------------------------------------------------------------------*
*& Class ALV
*&---------------------------------------------------------------------*
CLASS lcl_alv DEFINITION FINAL FRIENDS lcl_bupa lcl_file lcl_messages .
  PUBLIC SECTION.

    CONSTANTS:
      co_exec1        TYPE c LENGTH 6 VALUE '&EXEC1',
      co_double_click TYPE syst_ucomm VALUE '&IC1',
      co_all_message  TYPE syst_ucomm VALUE '&ALLMSG'.

    CLASS-METHODS:
      show CHANGING it_outtab TYPE STANDARD TABLE,

      create_fieldcatalog IMPORTING im_t_outtab       TYPE ty_e_alv
                          EXPORTING ex_t_fieldcat_alv TYPE slis_t_fieldcat_alv,
      top_of_page.

  PRIVATE SECTION.

ENDCLASS.

*&---------------------------------------------------------------------*
*& Class ALV IMPLEMENTATION
*&---------------------------------------------------------------------*
CLASS lcl_alv IMPLEMENTATION.

  METHOD create_fieldcatalog.

    DATA:
      lr_tabdescr TYPE REF TO cl_abap_structdescr,
      lr_data     TYPE REF TO data,
      lt_dfies    TYPE ddfields,
      ls_dfies    TYPE dfies,
      ls_fieldcat TYPE slis_fieldcat_alv.

    CREATE DATA lr_data LIKE im_t_outtab.

    lr_tabdescr ?= cl_abap_structdescr=>describe_by_data_ref( lr_data ).
    lt_dfies = cl_salv_data_descr=>read_structdescr( lr_tabdescr ).

    LOOP AT lt_dfies INTO ls_dfies.

      CLEAR ls_fieldcat.

      MOVE-CORRESPONDING ls_dfies TO ls_fieldcat.

      " Status field set as icon
      IF sy-tabix EQ 1.
        ls_fieldcat-icon = abap_true.
      ENDIF.
      " BP or client field set to one click
      IF sy-tabix EQ 2 OR sy-tabix EQ 3.
        ls_fieldcat-hotspot = abap_true.
        ls_fieldcat-no_zero = abap_true.
      ENDIF.

      IF ls_fieldcat-rollname IS INITIAL.
        ls_fieldcat-seltext_l = ls_fieldcat-fieldname.
        ls_fieldcat-seltext_m = ls_fieldcat-fieldname.
        ls_fieldcat-seltext_s = ls_fieldcat-fieldname.

      ENDIF.

      " Defines the fields as centralized
      ls_fieldcat-just = 'C'.

      APPEND ls_fieldcat TO ex_t_fieldcat_alv.

    ENDLOOP.

  ENDMETHOD.

*&----------------------------------------------------------------------*
*& METHOD SHOW.
*&----------------------------------------------------------------------*
  METHOD show.

    DATA:
      it_fieldcat TYPE slis_t_fieldcat_alv,
      lv_title    TYPE lvc_title,
      ls_alv      TYPE ty_e_alv,
      t_table     TYPE STANDARD TABLE OF ty_e_alv,
      ls_layout   TYPE slis_layout_alv.

    " Required because the imported table does not allow change
    t_table = it_outtab.

    CALL METHOD lcl_alv=>create_fieldcatalog
      EXPORTING
        im_t_outtab       = ls_alv
      IMPORTING
        ex_t_fieldcat_alv = it_fieldcat.

    lv_title = 'Dados importados'(007).

    ls_layout-colwidth_optimize = abap_true.

    o_bupa->set_time_finish( sy-timlo ).

    CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
      EXPORTING
        i_callback_program       = syst-cprog       " Name of the calling program
        i_callback_pf_status_set = 'ALV_SET_STATUS'   " Set EXIT routine to status
        i_callback_user_command  = 'USER_COMMAND_ALV' " EXIT routine for command handling
        i_callback_top_of_page   = 'TOP_OF_PAGE'      " EXIT routine for handling TOP-OF-PAGE
        i_grid_title             = lv_title         " Control title
        is_layout                = ls_layout        " List layout specifications
        it_fieldcat              = it_fieldcat      " Field catalog with field descriptions
        i_default                = abap_true        " Initial variant active/inactive logic
        i_save                   = abap_true        " Variants can be saved
*       is_variant               =                  " Variant information
*       it_events                =                  " Table of events to perform
*       it_event_exit            =                  " Standard fcode exit requests table
      TABLES
        t_outtab                 = it_outtab         " Table with data to be displayed
      EXCEPTIONS
        program_error            = 1                " Program errors
        OTHERS                   = 2.
    IF sy-subrc NE 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
        WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.

  ENDMETHOD.

  METHOD top_of_page.

*alv Header declarations
    DATA:
      lt_header TYPE slis_t_listheader,
      ls_header TYPE slis_listheader.

* Title
    IF o_bupa->get_test( ) IS INITIAL.
      ls_header-info = 'Resultado do processamento efetivo'.
    ELSE.
      ls_header-info = 'Resultado do processamento de teste'.
    ENDIF.

    ls_header-typ = 'H'.
    APPEND ls_header TO lt_header.
    CLEAR ls_header.

* Date
    ls_header-typ = 'S'.
    ls_header-key = 'Data: '.
    DATA(teste) = |{ sy-datlo DATE = USER }|.  "todays date

    CONCATENATE  teste '-'
                cl_abap_char_utilities=>horizontal_tab
                'Usuario: ' sy-uname
                INTO ls_header-info
                SEPARATED BY space
                RESPECTING BLANKS.

    APPEND ls_header TO lt_header.
    CLEAR: ls_header.

    ls_header-typ = 'S'.
    ls_header-key = 'Total: '.
    ls_header-info = |{ lines( o_bupa->it_bp_numbers ) }  parceiros de negócios processados.|.

    APPEND ls_header TO lt_header.
    CLEAR: ls_header.

    ls_header-typ = 'S'.
    ls_header-key = 'Tempo: '.
    ls_header-info = |INÍCIO: {
        o_bupa->get_time_start( ) TIME = USER } - FIM: {
        o_bupa->get_time_finish( ) TIME = USER } - Tempo total {
        o_bupa->convert_time(
          EXPORTING
            im_v_sec   = o_bupa->get_elapsed_time( ) ) } |.

    APPEND ls_header TO lt_header.
    CLEAR: ls_header.

    CALL FUNCTION 'REUSE_ALV_COMMENTARY_WRITE'
      EXPORTING
        it_list_commentary = lt_header.


  ENDMETHOD.

ENDCLASS.

*&---------------------------------------------------------------------*
*& Class (Implementation) cl_bupa
*&---------------------------------------------------------------------*
CLASS lcl_bupa IMPLEMENTATION .

  METHOD constructor.
    CREATE OBJECT o_file.

    o_file->upload(
      EXCEPTIONS
        conversion_failed     = 1
        upload_date_not_found = 2
        OTHERS                = 3 ).
    IF sy-subrc NE 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
        WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4
        RAISING upload_error.
      RETURN.
    ENDIF.

    me->time_start = sy-timlo.

    MOVE-CORRESPONDING o_file->table_excel TO me->t_alv.

    CALL METHOD me->get_bukrs_list.

    CALL METHOD me->remove_agents( ).

    CALL METHOD me->get_bp_from_data_base( ).

    CALL METHOD me->create_bp( ).

  ENDMETHOD.

*&----------------------------------------------------------------------*
*& METHOD GET_GUID
*&----------------------------------------------------------------------*
  METHOD get_guid.
    TRY.
        CALL METHOD cl_system_uuid=>create_uuid_x16_static
          RECEIVING
            uuid = r_result.
      CATCH cx_uuid_error ##NO_HANDLER.
    ENDTRY.
  ENDMETHOD.

*&----------------------------------------------------------------------*
*& METHOD GET_LAST_NAME
*&----------------------------------------------------------------------*
  METHOD get_last_name.
    CHECK im_v_name_rep IS NOT INITIAL.

    SPLIT im_v_name_rep AT space INTO TABLE DATA(lt_names).
    READ TABLE lt_names INTO r_last_name INDEX lines( lt_names ).
  ENDMETHOD.

*&----------------------------------------------------------------------*
*& METHOD GET_FIRST_NAME
*&----------------------------------------------------------------------*
  METHOD get_first_name.
    CHECK im_v_name_rep IS NOT INITIAL.

    SPLIT im_v_name_rep AT space INTO TABLE DATA(lt_names).
    DELETE lt_names INDEX lines( lt_names ).
    CONCATENATE LINES OF lt_names INTO r_first_name SEPARATED BY space.
  ENDMETHOD.                  "get_first_name

*&----------------------------------------------------------------------*
*& METHOD SET_COMMIT
*&----------------------------------------------------------------------*
  METHOD set_commit.
    CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
      EXPORTING
        wait = abap_true.
  ENDMETHOD.                  "set_commit

*&----------------------------------------------------------------------*
*& METHOD SET_ROLLBACK
*&----------------------------------------------------------------------*
  METHOD set_rollback.
    CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
  ENDMETHOD.                  "SET_ROLLBACK

*&----------------------------------------------------------------------*
*& METHOD REMOVE_AGENTS
*&----------------------------------------------------------------------*
  METHOD remove_agents.

    SORT me->t_alv BY funcao_representante.

    DELETE o_file->table_excel WHERE
      funcao_representante NE TEXT-001 AND  "Representante para assuntos financeiros
      funcao_representante NE TEXT-002 AND  "Representante para envio de documentos
      funcao_representante NE TEXT-003.     "Responsável pelo recebimento das faturas.

*    SORT me->t_alv BY codigo nome_representante_agente.
*
*    DELETE ADJACENT DUPLICATES FROM me->t_alv COMPARING codigo nome_representante_agente.

    SORT me->t_alv BY codigo nome_representante_agente funcao_representante.

    DELETE ADJACENT DUPLICATES FROM me->t_alv COMPARING codigo nome_representante_agente funcao_representante.

  ENDMETHOD.

*&----------------------------------------------------------------------*
*& METHOD GET_BP_FROM_DATA_BASE
*&----------------------------------------------------------------------*
  METHOD get_bp_from_data_base.

    DATA:
      lt_relationship    TYPE STANDARD TABLE OF bapibus1006_relations,
      ls_contact_numbers TYPE ty_e_contact_numbers,
      lv_valid_time      TYPE timestamp,
      ls_bp_numbers      LIKE LINE OF me->it_bp_numbers,
      ls_alv             LIKE LINE OF me->t_alv,
      lv_tabix           LIKE syst-tabix.

    DATA: lv_cnpj TYPE n LENGTH 14.

    FIELD-SYMBOLS :
      <fs_alv>          LIKE LINE OF me->t_alv,
      <fs_relationship> LIKE LINE OF lt_relationship.

    LOOP AT me->t_alv ASSIGNING <fs_alv>.

* RA 114 - Alexandre Bach - Início.
      lv_cnpj = <fs_alv>-cnpj.

      <fs_alv>-cnpj = lv_cnpj.

* RA 114 - Alexandre Bach - Fim.

      CALL METHOD lcl_messages=>progress_indicator
        EXPORTING
          im_v_text      = |{ TEXT-m02 } { <fs_alv>-codigo ALPHA = OUT }|
          im_v_processed = sy-tabix
          im_v_total     = lines( me->t_alv ).

      CLEAR ls_bp_numbers.

      <fs_alv>-bp = lcl_bupa=>get_bp_from_kunnr( <fs_alv>-codigo ).

*   CHECK if the client has already been checked inside the loop
      READ TABLE it_bp_numbers WITH KEY kunnr = <fs_alv>-codigo TRANSPORTING NO FIELDS BINARY SEARCH.
      CHECK sy-subrc IS NOT INITIAL.

      CALL METHOD lcl_bupa=>get_bp_from_kunnr
        EXPORTING
          im_v_kunnr     = <fs_alv>-codigo
        IMPORTING
          ex_v_kunnr     = ls_bp_numbers-kunnr
          ex_v_bp_guid   = ls_bp_numbers-bp_guid
          ex_v_bp_number = ls_bp_numbers-bp_number.

      IF ls_bp_numbers-bp_number IS NOT INITIAL.

        ls_bp_numbers-task_bp = me->co_object_task_update.

        GET TIME STAMP FIELD lv_valid_time.

*    Get all Contacts for every BP Customer
        CALL FUNCTION 'BUPA_RELATIONSHIPS_READ'
          EXPORTING
            iv_partner_guid  = ls_bp_numbers-bp_guid          " Business Partner GUID
            iv_reltyp        = me->co_bp_relationship_category " Business partner-Relationship category
            iv_valid_time    = lv_valid_time                  " UTC Time Stamp in Short Form (YYYYMMDDhhmmss)
*           iv_reset_buffer  =                                " Data Element for Domain BOOLE: TRUE (="X") and FALSE (=" ")
            iv_req_blk_msg   = space
            iv_req_mask      = 'X'
          TABLES
            et_relationships = lt_relationship             " Business Partner Relationships
*           et_relation_revers    =
          EXCEPTIONS
            not_found        = 1
            OTHERS           = 2.

        IF sy-subrc EQ 0.

*   Saves the contact numbers in the structure
          LOOP AT lt_relationship  ASSIGNING <fs_relationship>.

            CLEAR ls_contact_numbers.

            MOVE:
             <fs_relationship>-partner2 TO ls_contact_numbers-number_bp_contact.

            lcl_bupa=>get_bp_numbers(
              CHANGING
                ch_v_bp_number = ls_contact_numbers-number_bp_contact
                ch_v_bp_guid   = ls_contact_numbers-guid_bp_contact   ).

            ls_contact_numbers-task_bp_contact = me->co_object_task_update.

            APPEND ls_contact_numbers TO ls_bp_numbers-contact.

          ENDLOOP.
        ENDIF.

      ELSE.
*   The business partner does not exist and needs to be created
        ls_bp_numbers-task_bp   = me->co_object_task_insert.
        ls_bp_numbers-bp_guid   = me->get_guid( ).

      ENDIF. "bp_number IS NOT INITIAL.

*      Check the customer already exists
      CALL METHOD me->check_customer_exists(
        CHANGING
          ch_s_bp_numbers = ls_bp_numbers ).

      IF ls_bp_numbers-create_customer IS INITIAL.
* Chamado 1000000572 INICIO
*        MESSAGE ID 'CVI_MAPPING' TYPE 'S' NUMBER '042'
        "Cliente &1 existente, já registrado no sistema e será atualizado
        MESSAGE ID 'Z_BUPA' TYPE 'S' NUMBER '009'
* Chamado 1000000572 FIM
          WITH ls_bp_numbers-kunnr INTO DATA(lv_dummy).

        CALL METHOD lcl_messages=>store( ls_bp_numbers-kunnr ).

        CALL METHOD me->change_icon_alv_status
          EXPORTING
            im_v_icon  = icon_led_green
            im_v_kunnr = ls_bp_numbers-kunnr.
      ELSE.

* Chamado 1000000572 INICIO
        "MESSAGE ID 'F2' TYPE 'W' NUMBER '153'
        "Cliente &1 novo, não existe no sistema
        MESSAGE ID 'Z_BUPA' TYPE 'S' NUMBER '008'
* Chamado 1000000572 FIM
            WITH ls_bp_numbers-kunnr INTO lv_dummy.
        CALL METHOD lcl_messages=>store( ls_bp_numbers-kunnr ).

      ENDIF.

      APPEND ls_bp_numbers TO me->it_bp_numbers.

    ENDLOOP.    "AT me->t_alv ASSIGNING <fs_alv>.

  ENDMETHOD.    "METHOD GET_BP_FROM_CODIGO
*&----------------------------------------------------------------------*
*& METHOD GET_BP_NUMBERS
*&----------------------------------------------------------------------*
  METHOD get_bp_numbers.

    DATA lv_bu_bpext TYPE bu_bpext.

    lv_bu_bpext = |{ im_v_bu_bpext ALPHA = OUT }|.

    CALL FUNCTION 'BUPA_NUMBERS_GET'
      EXPORTING
        iv_partner          = ch_v_bp_number  " Business Partner Number
        iv_partner_guid     = ch_v_bp_guid    " Business Partner GUID
        iv_partner_external = lv_bu_bpext     " Business Partner Number in External System
      IMPORTING
        ev_partner          = ch_v_bp_number  " Business Partner Number
        ev_partner_guid     = ch_v_bp_guid.   " Business Partner GUID

  ENDMETHOD.

*&----------------------------------------------------------------------*
*& METHOD CREATE_BP
*&----------------------------------------------------------------------*
  METHOD create_bp.

    DATA:
      lt_saida          TYPE STANDARD TABLE OF ty_e_upload_layout,
      ls_cvis_ei_extern TYPE cvis_ei_extern,
      lt_cvis_ei_extern TYPE cvis_ei_extern_t,
      lt_return_map     TYPE mdg_bs_bp_msgmap_t,
      ls_saida          TYPE ty_e_upload_layout,
      ls_taxnumber      TYPE LINE OF bus_ei_bupa_taxnumber_t,
      ls_roles          TYPE bus_ei_bupa_roles,
      lv_object_task    TYPE bus_ei_object_task,
      lv_tabix          TYPE syst_tabix,
      lv_text           TYPE string,
      lv_time           TYPE string.

    DATA: lv_inscricao_estadual TYPE bus_ei_struc_bapitax-taxnumber.

    DATA: lt_dfkkbptaxnum TYPE TABLE OF dfkkbptaxnum.

    FIELD-SYMBOLS:
      <fs_bp_numbers> LIKE LINE OF it_bp_numbers,
      <fs_return_map> LIKE LINE OF lt_return_map,
      <fs_alv>        LIKE LINE OF me->t_alv.

    DATA: lv_kunnr TYPE kunnr.
    MOVE-CORRESPONDING me->t_alv TO lt_saida.

    SORT lt_saida BY codigo.
    DELETE ADJACENT DUPLICATES FROM lt_saida COMPARING codigo.

* RA 114 - Alexandre Bach - Início.
    IF NOT p_flag IS INITIAL.
      CALL METHOD me->get_mestre_de_vendas( ).
    ENDIF.
* RA 114 - Alexandre Bach - Fim.

    IF NOT p_avc IS INITIAL.

* Inclui os impostos AVC.
      SELECT * FROM zj1btxavc
        INTO TABLE it_zj1btxavc
        FOR ALL ENTRIES IN me->t_bukrs
        WHERE bukrs = me->t_bukrs-bukrs
          AND ativo = 'X'.

    ENDIF.

    SELECT * FROM tvko
      INTO TABLE it_tvko.

    CLEAR w_msg.

    LOOP AT it_bp_numbers ASSIGNING <fs_bp_numbers>.

      lv_tabix = syst-tabix.

      CLEAR: ls_cvis_ei_extern, ls_roles, ls_taxnumber, ls_saida, lv_time, me->bp_numbers.

      me->bp_numbers = <fs_bp_numbers>.

      lv_time = me->get_estimated_time( ).

      READ TABLE lt_saida INTO ls_saida WITH KEY codigo = <fs_bp_numbers>-kunnr.

* Checks if an update or upgrade, the condition is is the BP already exists in the system
      IF <fs_bp_numbers>-bp_number IS INITIAL.
*        <fs_bp_numbers>-bp_guid                         = lcl_bupa=>get_guid( ).
*        ls_cvis_ei_extern-ensure_create-create_customer = abap_true.
        IF me->test IS INITIAL.
          lv_text = |{ 'Criando PN para o cliente:_ '(m03) }{ <fs_bp_numbers>-kunnr ALPHA = OUT }|.
        ELSE.
          lv_text = |{ 'Val. novo PN para o cliente:_ '(m04) }{ <fs_bp_numbers>-kunnr ALPHA = OUT }{ lv_time }|.
        ENDIF.

      ELSE.
        IF me->test IS INITIAL.
          lv_text = |{ 'Atualizando cadastro do PN:_ '(m05) }{ <fs_bp_numbers>-bp_number ALPHA = OUT }|.
        ELSE.
          lv_text = |{ 'Verificando atualização do PN:_ '(m06) }{ <fs_bp_numbers>-bp_number ALPHA = OUT }|.
        ENDIF.
      ENDIF.

*      <fs_bp_numbers>-task = lv_object_task.

      TRANSLATE lv_text USING '_ '. "ABAP BUGS!

      lv_text = lv_text && lv_time.

      CALL METHOD lcl_messages=>progress_indicator
        EXPORTING
          im_v_text      = lv_text
          im_v_processed = lv_tabix
          im_v_total     = lines( it_bp_numbers ).

*----------------------------------------------------------------------
*    Central Business Partner Data
*----------------------------------------------------------------------
      ls_cvis_ei_extern-partner-header-object_task                                      = <fs_bp_numbers>-task_bp.
      ls_cvis_ei_extern-partner-header-object_instance-bpartnerguid                     = <fs_bp_numbers>-bp_guid.
      ls_cvis_ei_extern-partner-header-object_instance-bpartner                         = <fs_bp_numbers>-bp_number.
      ls_cvis_ei_extern-partner-header-object_instance-identificationnumber             = <fs_bp_numbers>-kunnr.
      ls_cvis_ei_extern-partner-central_data-common-data-bp_control-category            = '2'.                   "Organização
      ls_cvis_ei_extern-partner-central_data-common-data-bp_control-grouping            = me->co_grouping_naco.  "NACO Clientes ONS
      ls_cvis_ei_extern-partner-central_data-common-data-bp_centraldata-title_key       = me->co_title_key_3.    "Empresa"
      ls_cvis_ei_extern-partner-central_data-common-data-bp_centraldata-searchterm1     = ls_saida-sigla_do_agente.
      ls_cvis_ei_extern-partner-central_data-common-data-bp_centraldata-partnerexternal = |{ <fs_bp_numbers>-kunnr ALPHA = OUT }|.

      ls_roles-data-rolecategory = me->co_rolecategory_client. "FLCU00 Cliente (contab.financ.)

      APPEND ls_roles TO ls_cvis_ei_extern-partner-central_data-role-roles.

*--------------------------------+--------------------------------------
*    Name data
*----------------------------------------------------------------------
      ls_cvis_ei_extern-partner-central_data-common-data-bp_organization = me->fill_organization_name( ls_saida-razao_social ).

*----------------------------------------------------------------------
*    Business Partner Tax ID Data
*----------------------------------------------------------------------
      CLEAR lt_dfkkbptaxnum.
      CALL FUNCTION 'RTP_US_DB_DFKKBPTAXNUM_READ'
        EXPORTING
          i_partner    = <fs_bp_numbers>-bp_number
          i_taxtype    = me->co_taxtype_cnpj
        TABLES
          t_taxnumbers = lt_dfkkbptaxnum
        exceptions
          not_found    = 1
          OTHERS       = 2.

      IF sy-subrc <> 0.

        ls_taxnumber-task                 = me->co_object_task_insert.

      ELSE.

        ls_taxnumber-task                 = me->co_object_task_update.
        "ls_taxnumber-task                 = me->co_object_task_insert.

      ENDIF.

      "ls_taxnumber-task                 = me->co_object_task_insert. "If update, change after in compare_bp_source_fill_datax
      ls_taxnumber-data_key-taxtype     = me->co_taxtype_cnpj. "BR1  Brasil: nº CNPJ
      ls_taxnumber-data_key-taxnumber   = ls_saida-cnpj.
      APPEND ls_taxnumber TO ls_cvis_ei_extern-partner-central_data-taxnumber-taxnumbers.

      CLEAR lt_dfkkbptaxnum.
      CALL FUNCTION 'RTP_US_DB_DFKKBPTAXNUM_READ'
        EXPORTING
          i_partner    = <fs_bp_numbers>-bp_number
          i_taxtype    = me->co_taxtype_insc_estadual
        TABLES
          t_taxnumbers = lt_dfkkbptaxnum
        exceptions
          not_found    = 1
          OTHERS       = 2.

      IF sy-subrc <> 0.

        ls_taxnumber-task                 = me->co_object_task_insert.

      ELSE.

        ls_taxnumber-task                 = me->co_object_task_update.
        "ls_taxnumber-task                 = me->co_object_task_insert.

      ENDIF.

      "ls_taxnumber-task                 = me->co_object_task_insert.
      ls_taxnumber-data_key-taxtype     = me->co_taxtype_insc_estadual. "BR3  Brasil: inscrição estadual
      ls_taxnumber-data_key-taxnumber   = ls_saida-inscricao_estadual.
      APPEND ls_taxnumber TO ls_cvis_ei_extern-partner-central_data-taxnumber-taxnumbers.

*----------------------------------------------------------------------
*    Business Partner Address Data
*----------------------------------------------------------------------
      ls_cvis_ei_extern-partner-central_data-address-addresses = me->fill_address( ls_saida ).

*----------------------------------------------------------------------
*    Business Partner Customer role data
*----------------------------------------------------------------------
* RA 114 - Alexandre Bach - Início.
      ls_cvis_ei_extern-customer-central_data-central-data-icmstaxpay = 'NC'. "Não Contribuinte.

      READ TABLE me->t_alv ASSIGNING <fs_alv> WITH KEY codigo = <fs_bp_numbers>-kunnr.

      IF sy-subrc EQ 0.

        CLEAR lv_inscricao_estadual.

        lv_inscricao_estadual = <fs_alv>-inscricao_estadual.

        TRANSLATE lv_inscricao_estadual TO UPPER CASE.

* O campo inscrição estadual não pode estar vazio.
        "IF <fs_alv>-inscricao_estadual IS INITIAL.
        IF lv_inscricao_estadual EQ 'ISENTO' OR lv_inscricao_estadual EQ 'ISENTA'.

          SELECT SINGLE low FROM tvarvc
            INTO ls_cvis_ei_extern-customer-central_data-central-data-icmstaxpay
            WHERE name = co_nc
              AND type = sy-langu
              AND numb = 0.

        ELSE.

          SELECT SINGLE low FROM tvarvc
            INTO ls_cvis_ei_extern-customer-central_data-central-data-icmstaxpay
            WHERE name = co_no
              AND type = sy-langu
              AND numb = 0.

        ENDIF.

      ENDIF.
* RA 114 - Alexandre Bach - Fim.

      ls_cvis_ei_extern-ensure_create-create_customer           = <fs_bp_numbers>-create_customer.
      ls_cvis_ei_extern-customer-header-object_task             = <fs_bp_numbers>-task_customer.
      ls_cvis_ei_extern-customer-header-object_instance-kunnr   = <fs_bp_numbers>-kunnr.
      LOOP AT o_file->table_excel ASSIGNING FIELD-SYMBOL(<fs_table_excel>) WHERE codigo = <fs_bp_numbers>-kunnr.

        ls_cvis_ei_extern-customer-company_data-company           = me->fill_company_data( EXPORTING: im_v_kunnr = <fs_bp_numbers>-kunnr im_codimp = <fs_table_excel>-impostos_avc ).

      ENDLOOP.
* RA 114 - Alexandre Bach - Início.

      IF NOT p_flag IS INITIAL.

        ls_roles-data-rolecategory = me->co_rolecategory_vendas.
        ls_roles-data_key          = me->co_rolecategory_vendas.


        ls_cvis_ei_extern-customer-sales_data-sales               = me->fill_sales_data( EXPORTING im_v_kunnr = <fs_bp_numbers>-kunnr im_v_bp_number = <fs_bp_numbers>-bp_number ).
        ls_cvis_ei_extern-customer-central_data-tax_ind-tax_ind   = me->fill_tax_ind( EXPORTING im_v_kunnr = <fs_bp_numbers>-kunnr im_v_bp_number = <fs_bp_numbers>-bp_number ).

        READ TABLE ls_cvis_ei_extern-customer-sales_data-sales TRANSPORTING NO FIELDS WITH KEY task = cc_object_task_update.

        IF sy-subrc EQ 0.

          ls_roles-task = cc_object_task_update.

          CALL FUNCTION 'RTP_US_DB_BUT100_EXISTS'
            EXPORTING
              i_partner           = <fs_bp_numbers>-bp_number
              i_role              = me->co_rolecategory_vendas
            EXCEPTIONS
              partner_not_in_role = 1
              OTHERS              = 2.

          IF sy-subrc <> 0.

            ls_roles-task = cc_object_task_insert.

          ENDIF.

        ELSE.

          SELECT kunnr FROM knvv
            INTO lv_kunnr
            WHERE kunnr = <fs_bp_numbers>-kunnr.
          ENDSELECT.

          IF sy-subrc NE 0.

            ls_roles-task = cc_object_task_insert.

          ELSE.

            ls_roles-task = cc_object_task_update.

          ENDIF.

        ENDIF.

        APPEND ls_roles TO ls_cvis_ei_extern-partner-central_data-role-roles.

      ENDIF.
* RA 114 - Alexandre Bach - Fim.
*----------------------------------------------------------------------
* Updating the BP registry
*----------------------------------------------------------------------
* If it is an update you must fill in the fields that will be updated
*----------------------------------------------------------------------
      IF ls_cvis_ei_extern-partner-header-object_task EQ 'U'.

        CALL METHOD me->compare_bp_source_fill_datax
          CHANGING
            ch_s_source = ls_cvis_ei_extern.

      ENDIF.

*----------------------------------------------------------------------
* Method to validate execution, presents possible errors.
*----------------------------------------------------------------------
      CLEAR lt_return_map.

      CALL METHOD cl_md_bp_maintain=>validate_single
        EXPORTING
          i_data        = ls_cvis_ei_extern
        IMPORTING
          et_return_map = lt_return_map.

* Remove standard error message, it is not impeditive.
* Função PN &1 não existe para parceiro &2
      DELETE lt_return_map WHERE
        id      = 'R11' AND
        number  = '657' AND
        type    = 'E'.

      "Indicar um valor para campo NAME1
      DELETE lt_return_map WHERE
        id          = 'R11' AND
        number      = '401' AND
        type        = 'E' AND
        message_v1  = 'NAME1'.
      "Indicar um valor para campo SEARCHTERM1
      DELETE lt_return_map WHERE
        id          = 'R11' AND
        number      = '401' AND
        type        = 'E'   AND
        message_v1  = 'SEARCHTERM1'.

*    Check for validate error
      IF lt_return_map IS INITIAL.

        CALL METHOD me->change_icon_alv_status(
          EXPORTING
            im_v_icon  = icon_led_green
            im_v_kunnr = <fs_bp_numbers>-kunnr ).

        "Msg Dados do parceiro de negócios sem erros
        MESSAGE ID 'BUPA_DIALOG_JOEL' TYPE 'S' NUMBER '108' INTO DATA(lv_msgdummy).

        "store messages
        CALL METHOD lcl_messages=>store( <fs_bp_numbers>-kunnr ).

        CLEAR lt_cvis_ei_extern.
        APPEND ls_cvis_ei_extern TO lt_cvis_ei_extern.

        " Maintain BP
        CALL METHOD me->call_bp_maintain(
          EXPORTING
            im_t_cvis_ei_extern = lt_cvis_ei_extern
          CHANGING
            ch_s_bp_numbers     = <fs_bp_numbers> ).

      ELSE.
        "Passa para o a msg a tabix
        LOOP AT lt_return_map ASSIGNING <fs_return_map>.
          <fs_return_map>-row = lv_tabix.
        ENDLOOP.

        "store messages
        CALL METHOD lcl_messages=>store_validate
          EXPORTING
            im_t_return_map = lt_return_map
            im_v_index_line = <fs_bp_numbers>-kunnr.

        "Change status in ALV
        CALL METHOD me->set_icon_alv_status
          EXPORTING
            im_v_kunnr = <fs_bp_numbers>-kunnr
            im_t_msg   = lt_return_map
          RECEIVING
            r_icon     = DATA(lv_icon).

        "Alert message is not impeditive
*        IF lv_icon  = icon_led_yellow.
*Start    - Marcelo Alvares - MA004818 S4D ZBUPA_CARGA_ONS_CLASS Z_BUPA - 09.11.2018 15:58
* Due to failures of method validate single, it returns non-impeding errors for BP creation
* Performs even then maintain to return a real error
        CLEAR lt_cvis_ei_extern.
        APPEND ls_cvis_ei_extern TO lt_cvis_ei_extern.

        CALL METHOD me->call_bp_maintain(
          EXPORTING
            im_t_cvis_ei_extern = lt_cvis_ei_extern
          CHANGING
            ch_s_bp_numbers     = <fs_bp_numbers> ).

*        ENDIF.

      ENDIF.

    ENDLOOP. " LOOP AT it_bp_numbers ASSIGNING <fs_bp_numbers>

    IF me->test EQ abap_true.
      CALL METHOD me->set_rollback.
    ENDIF.

    " Problemas sérios com performance no ambiente de teste
    " A execução deve acontecer 1:1, 1:N não é possivel.
*    CHECK p_test IS INITIAL.
*    me->call_bp_maintain( lt_cvis_ei_extern ).

  ENDMETHOD.


**********************************************************************
*#
**********************************************************************
  METHOD create_contact.

    DATA:
      ls_cvis_ei_extern    TYPE cvis_ei_extern,
      lt_cvis_ei_extern    TYPE cvis_ei_extern_t,
      lt_return_map        TYPE mdg_bs_bp_msgmap_t,
      lt_return            TYPE bapiretm,
      ls_roles             TYPE bus_ei_bupa_roles,
      ls_contact           TYPE ty_e_contact_numbers,
      ls_business_partners TYPE bus_ei_main,
      ls_bp_current        TYPE bus_ei_main,
      ls_bus_ei_extern     TYPE bus_ei_extern,
      ls_error             TYPE mds_ctrls_error,
      lt_contactpersons    TYPE TABLE OF ty_e_upload_layout WITH NON-UNIQUE SORTED KEY key1 COMPONENTS codigo.

    FIELD-SYMBOLS:
      <fs_contactpersons> LIKE LINE OF lt_contactpersons.

    MOVE-CORRESPONDING me->t_alv TO lt_contactpersons.
    DELETE lt_contactpersons WHERE codigo NE ch_s_bp_numbers-kunnr.

*    Pega os dados selecionados  a partir do relacionamento existentes do bp
    LOOP AT ch_s_bp_numbers-contact ASSIGNING FIELD-SYMBOL(<fs_contact>) WHERE number_bp_contact IS NOT INITIAL.
      CLEAR: ls_bus_ei_extern.
*   Copia para a tabela do campo partners da estrutura ls_business_partners
      ls_bus_ei_extern-header-object_instance-bpartnerguid = <fs_contact>-guid_bp_contact.
      ls_bus_ei_extern-header-object_instance-bpartner     = <fs_contact>-number_bp_contact.
*        ls_bus_ei_extern-central_data-common-data-bp_centraldata-partnerexternal = ch_s_bp_numbers-bp_number.

      APPEND ls_bus_ei_extern TO ls_business_partners-partners.

    ENDLOOP.

*   contacts already exist
    IF ls_business_partners IS NOT INITIAL.
      CLEAR: ls_bp_current, ls_error.

      cl_bupa_current_data=>get_all(
        EXPORTING
          is_business_partners = ls_business_partners " Complex External Interface of the Business Partner (Tab.)
        IMPORTING
          es_business_partners = ls_bp_current        " Complex External Interface of the Business Partner (Tab.)
          es_error             = ls_error ).          " Message Structure of the Controller

      SORT ls_bp_current-partners
        BY central_data-common-data-bp_person-fullname ASCENDING .

    ENDIF.

*   Loop in spreadsheet loaded data
    LOOP AT lt_contactpersons ASSIGNING <fs_contactpersons>.

      CLEAR:
        ls_cvis_ei_extern, ls_roles.

* Checks whether the contact already exists in the base
      READ TABLE ls_bp_current-partners ASSIGNING FIELD-SYMBOL(<fs_bp_current>)
          WITH KEY central_data-common-data-bp_person-fullname = <fs_contactpersons>-nome_representante_agente
          BINARY SEARCH.

      IF sy-subrc EQ 0. "Found
        ls_cvis_ei_extern-partner-header-object_task                    = me->co_object_task_update. " U=Update
        ls_cvis_ei_extern-partner-header-object_instance-bpartnerguid   = <fs_bp_current>-header-object_instance-bpartnerguid.

* RA 114 - Alexandre Bach - Início.
        "ls_cvis_ei_extern-customer-header-object_task                   = me->co_object_task_update. " U=Update
* RA 114 - Alexandre Bach - Fim.

      ELSE.

        ls_cvis_ei_extern-partner-header-object_task                    = me->co_object_task_insert. "I=Incluir,
        ls_cvis_ei_extern-partner-header-object_instance-bpartnerguid   = lcl_bupa=>get_guid( ).

        ls_contact-task_bp_contact = ls_cvis_ei_extern-partner-header-object_task.
        ls_contact-guid_bp_contact = ls_cvis_ei_extern-partner-header-object_instance-bpartnerguid.
        APPEND ls_contact TO ch_s_bp_numbers-contact.

      ENDIF.

*----------------------------------------------------------------------
*    Dados centrais do Parceiro de Negocios
*----------------------------------------------------------------------
      ls_cvis_ei_extern-partner-central_data-common-data-bp_control-category = '1'. "Pessoa
      ls_cvis_ei_extern-partner-central_data-common-data-bp_control-grouping = 'NACF'.  "NACF Nacional Pessoa Fisica

      ls_roles-data-rolecategory = me->co_rolecategory_contact. "'BUP001'.  "Pessoa de contato
      APPEND ls_roles TO ls_cvis_ei_extern-partner-central_data-role-roles.

*----------------------------------------------------------------------
*    Dados do Nome
*----------------------------------------------------------------------
      ls_cvis_ei_extern-partner-central_data-common-data-bp_person-firstname = me->get_first_name( <fs_contactpersons>-nome_representante_agente ).
      ls_cvis_ei_extern-partner-central_data-common-data-bp_person-lastname  = me->get_last_name( <fs_contactpersons>-nome_representante_agente ).

      ls_cvis_ei_extern-partner-central_data-common-data-bp_person-fullname              = <fs_contactpersons>-nome_representante_agente.
      ls_cvis_ei_extern-partner-central_data-common-data-bp_person-correspondlanguageiso = me->co_languiso_pt.
      ls_cvis_ei_extern-partner-central_data-common-data-bp_person-nationalityiso        = me->co_pais_iso_br.

* RA 114 - Alexandre Bach - Início.
*----------------------------------------------------------------------
*    Dados do cliente com empresa
*----------------------------------------------------------------------

*      ls_cvis_ei_extern-customer-header-object_instance-kunnr   = <fs_contactpersons>-codigo.
*      ls_cvis_ei_extern-customer-company_data-company           = me->fill_company_data( <fs_contactpersons>-codigo ).
*      ls_cvis_ei_extern-customer-sales_data-sales               = me->fill_sales_data( <fs_contactpersons>-codigo ).
*      ls_cvis_ei_extern-customer-central_data-tax_ind-tax_ind   = me->fill_tax_ind( <fs_contactpersons>-codigo ).
* RA 114 - Alexandre Bach - Fim.
*----------------------------------------------------------------------
*    Business Partner Address Data
*----------------------------------------------------------------------
      APPEND LINES OF me->fill_contact_addresses( <fs_contactpersons> ) TO ls_cvis_ei_extern-partner-central_data-address-addresses.

**----------------------------------------------------------------------
**    Performs the validation of the data.
**----------------------------------------------------------------------
      CALL METHOD cl_md_bp_maintain=>validate_single
        EXPORTING
          i_data        = ls_cvis_ei_extern
        IMPORTING
          et_return_map = lt_return_map.

      APPEND ls_cvis_ei_extern TO lt_cvis_ei_extern.

    ENDLOOP.

*----------------------------------------------------------------------
*    Record all changes
*----------------------------------------------------------------------
    IF me->test IS INITIAL.

      CALL METHOD cl_md_bp_maintain=>maintain
        EXPORTING
          i_data   = lt_cvis_ei_extern
        IMPORTING
          e_return = lt_return.

      MODIFY zj1btxcli FROM TABLE it_zj1btxcli.

      CALL METHOD lcl_bupa=>set_commit( ).

      LOOP AT ch_s_bp_numbers-contact ASSIGNING FIELD-SYMBOL(<fs_contact_numbers>) WHERE number_bp_contact IS INITIAL.
        me->get_bp_numbers(
          CHANGING
            ch_v_bp_guid   = <fs_contact_numbers>-guid_bp_contact
            ch_v_bp_number = <fs_contact_numbers>-number_bp_contact ).
      ENDLOOP.

    ENDIF.

  ENDMETHOD.

*&----------------------------------------------------------------------*
*& METHOD FILL_ADDRESS
*&----------------------------------------------------------------------*
  METHOD fill_address.
    DATA ls_data TYPE bus_ei_bupa_address.
    DATA wa_kna1 TYPE kna1.

    SELECT SINGLE * FROM kna1
      INTO wa_kna1
      WHERE kunnr = im_s_addresses-codigo.

    ls_data-task = me->co_object_task_update.

    IF im_s_addresses-cep IS INITIAL.
      ls_data-data-postal-data-postl_cod1 = wa_kna1-pstlz.
    ELSE.
      ls_data-data-postal-data-postl_cod1 = im_s_addresses-cep.
    ENDIF.

    IF im_s_addresses-logradouro IS INITIAL.
      ls_data-data-postal-data-street     = wa_kna1-stras.
    ELSE.
      ls_data-data-postal-data-street     = im_s_addresses-logradouro.
    ENDIF.

    ls_data-data-postal-data-house_no = im_s_addresses-numero. "correção AB005518 - número do endereço.

    IF im_s_addresses-cidade IS INITIAL.
      ls_data-data-postal-data-city       = wa_kna1-ort01.
    ELSE.
      ls_data-data-postal-data-city       = im_s_addresses-cidade.
    ENDIF.

    IF im_s_addresses-bairro IS INITIAL.
      ls_data-data-postal-data-district   = wa_kna1-ort02.
    ELSE.
      ls_data-data-postal-data-district   = im_s_addresses-bairro.
    ENDIF.

    IF im_s_addresses-uf IS INITIAL.
      ls_data-data-postal-data-region     = wa_kna1-regio.
    ELSE.
      ls_data-data-postal-data-region     = im_s_addresses-uf.
    ENDIF.

    ls_data-data-postal-data-countryiso = me->co_pais_iso_br.
    ls_data-data-postal-data-country    = me->co_pais_iso_br.
    ls_data-data-postal-data-languiso   = me->co_languiso_pt.
    ls_data-data-postal-data-langu      = me->co_languiso_pt(1).

    APPEND ls_data TO r_return .

    CALL METHOD me->get_taxjurcode
      EXPORTING
        im_s_addresses = ls_data-data-postal-data
      RECEIVING
        r_result       = ls_data-data-postal-data-taxjurcode.

  ENDMETHOD.

*&----------------------------------------------------------------------*
*& METHOD FILL_ORGANIZATION_NAME
*&----------------------------------------------------------------------*
  METHOD fill_organization_name.

    DATA:
      lt_text_tab TYPE TABLE OF char255,
      lv_text     TYPE string.

    FIELD-SYMBOLS:
      <fs_central_organ> TYPE any,
      <fs_text>          TYPE char255.

    lv_text = im_v_razao_social.

    CALL FUNCTION 'SOTR_SERV_STRING_TO_TABLE'
      EXPORTING
        text        = lv_text
*       flag_no_line_breaks = 'X'
        line_length = 35
*       LANGU       = SY-LANGU
      TABLES
        text_tab    = lt_text_tab.

    LOOP AT lt_text_tab ASSIGNING <fs_text>.

      CHECK sy-tabix < 5.

      ASSIGN COMPONENT sy-tabix OF STRUCTURE r_result TO  <fs_central_organ>.

      <fs_central_organ> = <fs_text>.

    ENDLOOP.

    UNASSIGN <fs_central_organ>.

  ENDMETHOD.

*&----------------------------------------------------------------------*
*& METHOD COMPARE_BP_SOURCE_FILL_DATAX
*&----------------------------------------------------------------------*
  METHOD compare_bp_source_fill_datax.

    DATA:
      ls_business_partners TYPE bus_ei_main,
      ls_bp_current        TYPE bus_ei_main,
      ls_bus_ei_extern     TYPE bus_ei_extern,
      ls_error             TYPE mds_ctrls_error.

    FIELD-SYMBOLS:
      <fs_bp_current>         TYPE bus_ei_extern,
      <fs_adress_current>     TYPE bus_ei_bupa_address,
      <fs_adress_source>      TYPE bus_ei_bupa_address,
      <fs_taxnumbers_source>  TYPE bus_ei_bupa_taxnumber,
      <fs_taxnumbers_current> TYPE bus_ei_bupa_taxnumber.

*----------------------------------------------------------------------
* Get current recorded BP data
*----------------------------------------------------------------------
    MOVE-CORRESPONDING ch_s_source-partner-header-object_instance TO ls_bus_ei_extern-header-object_instance.

    APPEND ls_bus_ei_extern TO ls_business_partners-partners.

    cl_bupa_current_data=>get_all(
      EXPORTING
        is_business_partners = ls_business_partners " Complex External Interface of the Business Partner (Tab.)
      IMPORTING
        es_business_partners = ls_bp_current        " Complex External Interface of the Business Partner (Tab.)
        es_error             = ls_error ).          " Message Structure of the Controller

    CLEAR ls_bus_ei_extern.

    READ TABLE ls_bp_current-partners ASSIGNING <fs_bp_current> INDEX 1.

*----------------------------------------------------------------------
* Check change in central_data
*----------------------------------------------------------------------
    CALL METHOD me->compare_data
      EXPORTING
        im_s_source  = ch_s_source-partner-central_data-common-data-bp_centraldata
        im_s_current = <fs_bp_current>-central_data-common-data-bp_centraldata
      CHANGING
        ch_s_data_x  = ch_s_source-partner-central_data-common-datax-bp_centraldata.

*----------------------------------------------------------------------
* Check for change in bp_organization
*----------------------------------------------------------------------
    CALL METHOD me->compare_data
      EXPORTING
        im_s_source  = ch_s_source-partner-central_data-common-data-bp_organization
        im_s_current = <fs_bp_current>-central_data-common-data-bp_organization
      CHANGING
        ch_s_data_x  = ch_s_source-partner-central_data-common-datax-bp_organization.

*----------------------------------------------------------------------
* Verify CNPJ and State Registration has change
*----------------------------------------------------------------------
*    LOOP AT ch_s_source-partner-central_data-taxnumber-taxnumbers ASSIGNING <fs_taxnumbers_source>.
*
*      READ TABLE <fs_bp_current>-central_data-taxnumber-taxnumbers
*        ASSIGNING <fs_taxnumbers_current> WITH KEY data_key-taxtype = <fs_taxnumbers_source>-data_key-taxtype.
*
*      " Check if there was a change in the number, if not remove from the update.
*      " if in case it gets error in validation and recording a number without change.
*      CHECK <fs_taxnumbers_current> IS ASSIGNED.
*
*      IF <fs_taxnumbers_current>-data_key-taxnumber EQ <fs_taxnumbers_source>-data_key-taxnumber.
*        DELETE ch_s_source-partner-central_data-taxnumber-taxnumbers WHERE data_key = <fs_taxnumbers_current>-data_key.
*      ELSE.
*        <fs_taxnumbers_source>-task = me->co_object_task_update. "Update
*      ENDIF.
*
*    ENDLOOP.

*----------------------------------------------------------------------
* Verify change in mailing address
* The data sent by the ONS in the excel spreadsheet contains only 1 address,
* in the comparison it is always considered the first record, the others are not verified.
*----------------------------------------------------------------------
    READ TABLE <fs_bp_current>-central_data-address-addresses     ASSIGNING <fs_adress_current> INDEX 1.
    READ TABLE ch_s_source-partner-central_data-address-addresses ASSIGNING <fs_adress_source>  INDEX 1.

    IF <fs_adress_current> IS ASSIGNED AND <fs_adress_source> IS ASSIGNED.
      CALL METHOD me->compare_data
        EXPORTING
          im_s_source  = <fs_adress_source>-data-postal-data
          im_s_current = <fs_adress_current>-data-postal-data
        CHANGING
          ch_s_data_x  = <fs_adress_source>-data-postal-datax.

* Chamado 1000000572 - Testes integrados Ciclo 2
* Start - Marcelo Alvares - MA004818 S4D ZBUPA_CARGA_ONS_CLASS Z_BUPA - 01.02.2019 10:35
      IF <fs_adress_source>-data-postal-datax IS NOT INITIAL.
        <fs_adress_source>-data_key-guid = <fs_adress_current>-data_key-guid.
        <fs_adress_source>-task = me->co_object_task_update.
      ENDIF.

    ELSE.
      <fs_adress_source>-task = me->co_object_task_insert.
    ENDIF.
* END   - Marcelo Alvares - MA004818 S4D ZBUPA_CARGA_ONS_CLASS Z_BUPA - 01.02.2019 10:35

    " Always update, required field
    ch_s_source-customer-central_data-central-datax-icmstaxpay = abap_true.

*----------------------------------------------------------------------
* Verify company data
*----------------------------------------------------------------------
    DATA it_knb1 TYPE STANDARD TABLE OF knb1 WITH NON-UNIQUE SORTED KEY key_primary COMPONENTS kunnr bukrs.

    CALL FUNCTION 'FTBP_READ_KNB1'
      EXPORTING
        i_partner          = ch_s_source-partner-header-object_instance-bpartner
      TABLES
        t_knb1             = it_knb1
      EXCEPTIONS
        customer_not_found = 1
        no_connection      = 2
        OTHERS             = 3.

    IF sy-subrc EQ 0.

      LOOP AT it_knb1 ASSIGNING FIELD-SYMBOL(<fs_company_current>).

        READ TABLE ch_s_source-customer-company_data-company
            ASSIGNING FIELD-SYMBOL(<fs_company_source>)
                WITH KEY data_key-bukrs = <fs_company_current>-bukrs.

        CHECK sy-subrc EQ 0.

        CALL METHOD me->compare_data
          EXPORTING
            im_s_source  = <fs_company_source>-data
            im_s_current = <fs_company_current>
          CHANGING
            ch_s_data_x  = <fs_company_source>-datax.

      ENDLOOP.
    ENDIF.

    DATA it_knvv TYPE STANDARD TABLE OF knvv WITH NON-UNIQUE SORTED KEY key_primary COMPONENTS kunnr vkorg.
    IF NOT it_knb1 IS INITIAL.

      SELECT * FROM knvv
        INTO TABLE it_knvv
        FOR ALL ENTRIES IN it_knb1
        WHERE kunnr = it_knb1-kunnr.

    ENDIF.

    LOOP AT it_knvv ASSIGNING FIELD-SYMBOL(<fs_ov_current>).

      READ TABLE ch_s_source-customer-sales_data-sales
          ASSIGNING FIELD-SYMBOL(<fs_ov_source>)
              WITH KEY data_key-vkorg = <fs_ov_current>-vkorg
                       data_key-vtweg = <fs_ov_current>-vtweg
                       data_key-spart = <fs_ov_current>-spart.

      CHECK sy-subrc EQ 0.

      CALL METHOD me->compare_data
        EXPORTING
          im_s_source  = <fs_ov_source>-data
          im_s_current = <fs_ov_current>
        CHANGING
          ch_s_data_x  = <fs_ov_source>-datax.

    ENDLOOP.

    "ENDIF.

  ENDMETHOD.

*&----------------------------------------------------------------------*
*&  METHOD COMPARE_DATA
*&----------------------------------------------------------------------*
  METHOD compare_data.

    DATA:
      lo_table_descr  TYPE REF TO cl_abap_tabledescr,
      lo_struct_descr TYPE REF TO cl_abap_structdescr,
      lo_data         TYPE REF TO data,
      it_columns      TYPE abap_compdescr_tab.

    CREATE DATA lo_data LIKE im_s_current.

    lo_struct_descr ?= cl_abap_structdescr=>describe_by_data_ref( lo_data ).
*    lo_table_descr ?= cl_abap_typedescr=>describe_by_data( it_knb1 ).
*    lo_struct_descr ?= lo_table_descr->get_table_line_type( ).
    it_columns = lo_struct_descr->components.

    LOOP AT it_columns ASSIGNING FIELD-SYMBOL(<fs_field>).

      ASSIGN COMPONENT <fs_field>-name OF STRUCTURE im_s_current TO FIELD-SYMBOL(<fs_comp_current>).
      ASSIGN COMPONENT <fs_field>-name OF STRUCTURE im_s_source  TO FIELD-SYMBOL(<fs_comp_source>).
      ASSIGN COMPONENT <fs_field>-name OF STRUCTURE ch_s_data_x  TO FIELD-SYMBOL(<fs_comp_x>).

      CHECK:
          <fs_comp_current>   IS ASSIGNED,
          <fs_comp_source>    IS ASSIGNED,
          <fs_comp_x>         IS ASSIGNED.

      IF <fs_comp_current> NE <fs_comp_source>.
        <fs_comp_x> = abap_true.
      ENDIF.

      UNASSIGN:  <fs_comp_current>, <fs_comp_source>, <fs_comp_x>.

    ENDLOOP.

  ENDMETHOD.

*&----------------------------------------------------------------------*
*& METHOD GET_BP_FROM_KUNNR.
*&----------------------------------------------------------------------*
  METHOD get_bp_from_kunnr.

    DATA:
      lv_kunnr          TYPE kunnr,
      lo_bp_customer_xd TYPE REF TO cvi_ka_bp_customer.

    lo_bp_customer_xd = cvi_ka_bp_customer=>get_instance( ).

    ex_v_kunnr = lv_kunnr = |{ im_v_kunnr ALPHA = IN }|.

    " Look for the BP from the Customer number, can not find.
    lo_bp_customer_xd->get_assigned_bp_for_customer(
      EXPORTING
        i_customer       = lv_kunnr         " Customer Number 1
*          i_persisted_only =               " Return Only Assignment That Has Already Been Made Persistent
      RECEIVING
        r_partner        = ex_v_bp_guid ).  " Business Partner GUID

    lcl_bupa=>get_bp_numbers(
      EXPORTING
        im_v_bu_bpext  = lv_kunnr
      CHANGING
        ch_v_bp_guid   = ex_v_bp_guid
        ch_v_bp_number = ex_v_bp_number ).

    r_bp_number = ex_v_bp_number.

  ENDMETHOD.

*&----------------------------------------------------------------------*
*& METHOD CALL_BP_MAINTAIN.
*&    CALL cl_md_bp_maintain and write insert or updates
*&----------------------------------------------------------------------*
  METHOD call_bp_maintain.

    DATA:
      lt_return TYPE bapiretm.

    FIELD-SYMBOLS:
      <fs_bp_numbers> LIKE s_bp_numbers,
      <fs_alv>        TYPE ty_e_alv.

    CHECK:
      me->it_bp_numbers     IS NOT INITIAL,
      im_t_cvis_ei_extern   IS NOT INITIAL,
      me->test              IS INITIAL. "  Execução de teste


    CALL METHOD cl_md_bp_maintain=>maintain
      EXPORTING
        i_data   = im_t_cvis_ei_extern
      IMPORTING
        e_return = lt_return.

    CALL METHOD lcl_bupa=>set_commit( ).

* Cria os contatos do BP
    CALL METHOD me->create_contact
      CHANGING
        ch_s_bp_numbers = ch_s_bp_numbers.

* Cria os relacionamentos do BP e Contatos
    CALL METHOD me->bp_relationship_create( ch_s_bp_numbers ).

* Get number of new BP´s
*    LOOP AT me->it_bp_numbers ASSIGNING <fs_bp_numbers> WHERE bp_number IS INITIAL.
    IF ch_s_bp_numbers-bp_number IS INITIAL.
      me->get_bp_numbers(
        CHANGING
          ch_v_bp_guid   = ch_s_bp_numbers-bp_guid
          ch_v_bp_number = ch_s_bp_numbers-bp_number  ).
    ENDIF.
*    ENDLOOP.

*   Associates business partner number to alv table
    LOOP AT me->t_alv ASSIGNING <fs_alv> USING KEY key_codigo
        WHERE codigo = ch_s_bp_numbers-kunnr AND bp IS INITIAL.
      <fs_alv>-bp = ch_s_bp_numbers-bp_number.
    ENDLOOP.

    CALL METHOD me->handle_return_messages
      EXPORTING
        im_t_bapiretm   = lt_return
      CHANGING
        ch_s_bp_numbers = ch_s_bp_numbers.


  ENDMETHOD.

*&----------------------------------------------------------------------*
*&  METHOD GET_ALV_TABIX
*&    Get tabix from alv table to show message
*&----------------------------------------------------------------------*
  METHOD get_alv_tabix.

    READ TABLE me->it_bp_numbers WITH KEY kunnr = im_v_number TRANSPORTING NO FIELDS.

    r_tabix = sy-tabix.

  ENDMETHOD.

*&----------------------------------------------------------------------*
*&  METHOD handle_return_messages
*&    Handle messages from method cl_md_bp_maintain=>maintain
*&----------------------------------------------------------------------*
  METHOD handle_return_messages.

    DATA:
      lv_msgdummy TYPE string,
      lv_tabix    TYPE syst_tabix.

    FIELD-SYMBOLS:
      <fs_bapiretm>   LIKE LINE OF im_t_bapiretm,
      <fs_object_msg> TYPE bapiretc.
*      <fs_bp_numbers> LIKE LINE OF me->it_bp_numbers.

    lv_tabix = me->get_alv_tabix( ch_s_bp_numbers-kunnr ).

    LOOP AT im_t_bapiretm ASSIGNING <fs_bapiretm>.

* checks if there is any error message on bp or client creation
* It may happen that BP is created but the client role does not.
      CALL METHOD me->set_icon_alv_status
        EXPORTING
          im_v_kunnr = ch_s_bp_numbers-kunnr
          im_t_msg   = <fs_bapiretm>-object_msg.

      LOOP AT <fs_bapiretm>-object_msg ASSIGNING <fs_object_msg>.

        MESSAGE ID  <fs_object_msg>-id
        TYPE        <fs_object_msg>-type
        NUMBER      <fs_object_msg>-number
        WITH        <fs_object_msg>-message_v1
                    <fs_object_msg>-message_v2
                    <fs_object_msg>-message_v3
                    <fs_object_msg>-message_v4
        INTO lv_msgdummy.

        CALL METHOD lcl_messages=>store( ch_s_bp_numbers-kunnr ).

        IF <fs_object_msg>-type CA 'AE'.
          ch_s_bp_numbers-errors_found = abap_true.
        ENDIF.

      ENDLOOP.  "<fs_bapiretm>-object_msg ASSIGNING <fs_object_msg>.

    ENDLOOP. "LOOP AT im_t_bapiretm ASSIGNING <fs_bapiretm>.

    CALL METHOD me->check_customer_iu_successfully( ch_s_bp_numbers ).

  ENDMETHOD.

*&---------------------------------------------------------------------*
*&  METHOD GET_BUKRS_LIST
*&---------------------------------------------------------------------*
  METHOD get_bukrs_list.

    SELECT bukrs
      FROM t001
      INTO  TABLE me->t_bukrs
      WHERE bukrs IN s_bukrs.

  ENDMETHOD.

*&---------------------------------------------------------------------*
*&  METHOD GET_MESTRE_DE_VENDAS
*&---------------------------------------------------------------------*
  METHOD get_mestre_de_vendas.

    IF NOT it_bp_numbers IS INITIAL.

*      SELECT kunnr vkorg vtweg spart kdgrp bzirk vkbur vkgrp eikto awahr klabc waers konda pltyp kalks versg
*             lprio kzazu vsbed vwerk autlf kztlf perfk incov inco1 inco2 inco3_l zterm ktgrd aufsd lifsd faksd
*        FROM knvv
*        INTO  TABLE me->o_file->upload_data-sales_data
*        FOR ALL ENTRIES IN it_bp_numbers
*        WHERE kunnr EQ it_bp_numbers-kunnr
*          AND vkorg IN s_vkorg
*          AND vtweg IN s_vtweg
*          AND spart IN s_spart.

    ENDIF.

  ENDMETHOD.

*&---------------------------------------------------------------------*
*&  METHOD fill_company_data
*&  Verify which company is already associated with the customer and add the rest
*&---------------------------------------------------------------------*
  METHOD fill_company_data.

    DATA:
      ls_cmds_ei_company TYPE cmds_ei_company,
      ls_knb1            TYPE knb1,
      lv_texto_msg       TYPE c LENGTH 60.

    LOOP AT me->t_bukrs ASSIGNING FIELD-SYMBOL(<fs_bukrs>).

      CLEAR: ls_cmds_ei_company.

      CALL FUNCTION 'KNB1_SINGLE_READ'
        EXPORTING
          i_kunnr         = im_v_kunnr       " Customer Number
          i_bukrs         = <fs_bukrs>-bukrs " Company Code
        EXCEPTIONS
          not_found       = 1                " No Entry Found
          parameter_error = 2                " Error in parameters
          kunnr_blocked   = 3                " KUNNR blocked
          OTHERS          = 4.

      IF sy-subrc EQ 1. " No Entry Found

        READ TABLE it_tvko INTO wa_tvko WITH KEY bukrs = <fs_bukrs>-bukrs.

        IF sy-subrc EQ 0.

          IF NOT wa_tvko-vkorg IN s_vkorg AND w_msg IS INITIAL.

            w_msg = 1.

            " Mensagem de aviso.
            MESSAGE ID  'Z_BUPA' TYPE 'I' NUMBER '000' WITH TEXT-070 TEXT-071 TEXT-072 TEXT-073 INTO lv_texto_msg.

            MESSAGE lv_texto_msg TYPE 'I' DISPLAY LIKE 'I'.

          ENDIF.

        ENDIF.

        ls_cmds_ei_company-task = me->co_object_task_insert. " I Insert
        ls_cmds_ei_company-data_key-bukrs = <fs_bukrs>-bukrs.
        ls_cmds_ei_company-data-akont = '1121121001'.
        ls_cmds_ei_company-data-fdgrv = 'CLI ONS'.
        ls_cmds_ei_company-wtax_type-wtax_type   = me->fill_wtax_type( im_v_kunnr ).

        APPEND ls_cmds_ei_company TO r_result.

      ENDIF.
      IF NOT p_avc IS INITIAL.

        READ TABLE it_zj1btxavc INTO wa_zj1btxavc WITH KEY bukrs  = <fs_bukrs>-bukrs
                                                           codimp = im_codimp.

        IF sy-subrc EQ 0.

          MOVE-CORRESPONDING wa_zj1btxavc TO wa_zj1btxcli.
          wa_zj1btxcli-kunnr   = im_v_kunnr.
          wa_zj1btxcli-responsavel = sy-uname.
          APPEND wa_zj1btxcli TO it_zj1btxcli.
          CLEAR wa_zj1btxcli.

        ENDIF.

      ENDIF.

    ENDLOOP.

  ENDMETHOD.

*&----------------------------------------------------------------------*
*&  METHOD FILL_WTAX_TYPE
*&
*&----------------------------------------------------------------------*
  METHOD fill_wtax_type.

    DATA: ls_cmds_ei_wtax_type TYPE cmds_ei_wtax_type.

    DATA: lv_cnpj TYPE n LENGTH 14.

    LOOP AT me->t_alv ASSIGNING FIELD-SYMBOL(<fs_alv>) WHERE codigo = im_v_kunnr.

      IF <fs_alv>-cod_categoria_irf IS INITIAL AND <fs_alv>-cod_irf IS INITIAL AND <fs_alv>-cod_autorizado_irf IS INITIAL AND
         <fs_alv>-autorizado_deduzir_irf_de IS INITIAL AND <fs_alv>-autorizado_deduzir_irf_ate IS INITIAL.

        CONTINUE.

      ENDIF.

      ls_cmds_ei_wtax_type-task           = me->co_object_task_insert. " I Insert
      ls_cmds_ei_wtax_type-data_key-witht = <fs_alv>-cod_categoria_irf.
      ls_cmds_ei_wtax_type-data-wt_withcd = <fs_alv>-cod_irf.
      ls_cmds_ei_wtax_type-data-wt_agent  = <fs_alv>-cod_autorizado_irf.
      CONCATENATE <fs_alv>-autorizado_deduzir_irf_de+6(4) <fs_alv>-autorizado_deduzir_irf_de+3(2) <fs_alv>-autorizado_deduzir_irf_de+0(2)
                  INTO ls_cmds_ei_wtax_type-data-wt_agtdf.
      "ls_cmds_ei_wtax_type-data-wt_agtdf  = <fs_alv>-autorizado_deduzir_irf_de.
      CONCATENATE <fs_alv>-autorizado_deduzir_irf_ate+6(4) <fs_alv>-autorizado_deduzir_irf_ate+3(2) <fs_alv>-autorizado_deduzir_irf_ate+0(2)
                  INTO ls_cmds_ei_wtax_type-data-wt_agtdt.
      "ls_cmds_ei_wtax_type-data-wt_agtdt  = <fs_alv>-autorizado_deduzir_irf_ate.

      APPEND ls_cmds_ei_wtax_type TO r_result.

    ENDLOOP.

    "ENDIF.

  ENDMETHOD.

*&----------------------------------------------------------------------*
*&  METHOD FILL_SALES_DATA
*&
*&----------------------------------------------------------------------*
  METHOD fill_sales_data.

    DATA:
      ls_cmds_ei_sales     TYPE cmds_ei_sales,
      ls_cmds_ei_functions TYPE cmds_ei_functions,
      lv_texto_msg         TYPE c LENGTH 60.

    SELECT * FROM knvp
      INTO TABLE it_knvp
      WHERE kunnr = im_v_kunnr
        AND vkorg IN s_vkorg.

    LOOP AT me->o_file->upload_data-sales_data ASSIGNING FIELD-SYMBOL(<fs_sales_data>)
     USING KEY key_kunnr WHERE kunnr =  im_v_kunnr.

      READ TABLE it_tvko INTO wa_tvko WITH KEY vkorg = <fs_sales_data>-vkorg.

      IF sy-subrc EQ 0.

        IF NOT wa_tvko-bukrs IN s_bukrs AND w_msg IS INITIAL.

          w_msg = 1.

          " Mensagem de aviso.
          MESSAGE ID  'Z_BUPA' TYPE 'I' NUMBER '000' WITH TEXT-070 TEXT-071 TEXT-072 TEXT-073 INTO lv_texto_msg.

          MESSAGE lv_texto_msg TYPE 'I' DISPLAY LIKE 'I'.

        ENDIF.

      ENDIF.

      CLEAR: ls_cmds_ei_sales.

      CALL FUNCTION 'KNVV_SINGLE_READ'
        EXPORTING
          i_kunnr         = <fs_sales_data>-kunnr    " Customer Number
          i_vkorg         = <fs_sales_data>-vkorg    " Sales Organization
          i_vtweg         = <fs_sales_data>-vtweg    " Distribution Channel
          i_spart         = <fs_sales_data>-spart    " Division
        EXCEPTIONS
          not_found       = 1                " No Entry Found
          parameter_error = 2                " Error in parameters
          kunnr_blocked   = 3                " KUNNR Blocked
          OTHERS          = 4.

      IF sy-subrc EQ 1.
        ls_cmds_ei_sales-task       = cc_object_task_insert. " I Insert
        ls_cmds_ei_functions-task   = cc_object_task_insert. " I Insert
      ELSE.
        ls_cmds_ei_sales-task       = cc_object_task_update.  " U Update
        ls_cmds_ei_functions-task   = cc_object_task_update.  " U Update
        READ TABLE it_knvp INTO wa_knvp WITH KEY kunnr = <fs_sales_data>-kunnr
                                                 vkorg = <fs_sales_data>-vkorg.

        IF sy-subrc NE 0.

          ls_cmds_ei_functions-task   = cc_object_task_insert. " I Insert

        ENDIF.
      ENDIF.

      MOVE-CORRESPONDING:
      <fs_sales_data>       TO ls_cmds_ei_sales-data_key,
      <fs_sales_data>       TO ls_cmds_ei_sales-data.

      SELECT SINGLE low FROM tvarvc
        INTO ls_cmds_ei_sales-data-inco1
        WHERE name = co_inco1
          AND type = sy-langu
          AND numb = 0.

      SELECT SINGLE low FROM tvarvc
        INTO ls_cmds_ei_sales-data-inco2_l
        WHERE name = co_inco2
          AND type = sy-langu
          AND numb = 0.

      SELECT SINGLE low FROM tvarvc
        INTO ls_cmds_ei_sales-data-inco2
        WHERE name = co_inco2
          AND type = sy-langu
          AND numb = 0.

      SELECT SINGLE low FROM tvarvc
        INTO ls_cmds_ei_sales-data-ktgrd
        WHERE name = co_ktgrd
          AND type = sy-langu
          AND numb = 0.

      SELECT SINGLE low FROM tvarvc
        INTO ls_cmds_ei_sales-data-kalks
        WHERE name = co_clas_fis
          AND type = sy-langu
          AND numb = 0.

      SELECT SINGLE low FROM tvarvc
        INTO ls_cmds_ei_sales-data-vsbed
        WHERE name = co_cond_exp
          AND type = sy-langu
          AND numb = 0.

      ls_cmds_ei_functions-data-partner   = <fs_sales_data>-kunnr.

      ls_cmds_ei_functions-data_key-parvw = 'AG'.   "SP Emissor da ordem
*      ls_cmds_ei_functions-data_key-parza = '001'.
      APPEND ls_cmds_ei_functions TO ls_cmds_ei_sales-functions-functions.

      ls_cmds_ei_functions-data_key-parvw = 'RE'.   "BP Recebedor da fatura
*      ls_cmds_ei_functions-data_key-parza = '002'.
      APPEND ls_cmds_ei_functions TO ls_cmds_ei_sales-functions-functions.

      ls_cmds_ei_functions-data_key-parvw = 'RG'.   "PY Pagador
*      ls_cmds_ei_functions-data_key-parza = '003'.
      APPEND ls_cmds_ei_functions TO ls_cmds_ei_sales-functions-functions.

      ls_cmds_ei_functions-data_key-parvw = 'WE'.   "SH Receb.mercad.
*      ls_cmds_ei_functions-data_key-parza = '004'.
      APPEND ls_cmds_ei_functions TO ls_cmds_ei_sales-functions-functions.

      APPEND ls_cmds_ei_sales TO r_result.

    ENDLOOP."AT me->o_file->upload_data-sales_data ASSIGNING FIELD-SYMBOL(<fs_sales_data>)

    IF NOT sy-subrc IS INITIAL.

      SELECT vkorg vtweg spart
        FROM tvta
        INTO TABLE it_area_vendas
        WHERE vkorg IN s_vkorg.

      LOOP AT it_area_vendas INTO wa_area_vendas.

        READ TABLE it_tvko INTO wa_tvko WITH KEY vkorg = wa_area_vendas-vkorg.

        IF sy-subrc EQ 0.

          IF NOT wa_tvko-bukrs IN s_bukrs AND w_msg IS INITIAL.

            w_msg = 1.

            " Mensagem de aviso.
            MESSAGE ID  'Z_BUPA' TYPE 'I' NUMBER '000' WITH TEXT-070 TEXT-071 TEXT-072 TEXT-073 INTO lv_texto_msg.

            MESSAGE lv_texto_msg TYPE 'I' DISPLAY LIKE 'I'.

          ENDIF.

        ENDIF.

        CLEAR: ls_cmds_ei_sales.

        CALL FUNCTION 'KNVV_SINGLE_READ'
          EXPORTING
            i_kunnr         = im_v_kunnr               " Customer Number
            i_vkorg         = wa_area_vendas-vkorg     " Sales Organization
            i_vtweg         = wa_area_vendas-vtweg     " Distribution Channel
            i_spart         = wa_area_vendas-spart     " Division
          EXCEPTIONS
            not_found       = 1                " No Entry Found
            parameter_error = 2                " Error in parameters
            kunnr_blocked   = 3                " KUNNR Blocked
            OTHERS          = 4.

        IF sy-subrc EQ 1.
          ls_cmds_ei_sales-task       = cc_object_task_insert. " I Insert
          ls_cmds_ei_functions-task   = cc_object_task_insert. " I Insert
        ELSE.
          ls_cmds_ei_sales-task       = cc_object_task_update.  " U Update
          ls_cmds_ei_functions-task   = cc_object_task_update.  " U Update

          READ TABLE it_knvp INTO wa_knvp WITH KEY kunnr = im_v_kunnr
                                                   vkorg = wa_area_vendas-vkorg.

          IF sy-subrc NE 0.

            ls_cmds_ei_functions-task   = cc_object_task_insert. " I Insert

          ENDIF.
        ENDIF.
        ls_cmds_ei_sales-data_key-vkorg   = wa_area_vendas-vkorg.
        ls_cmds_ei_sales-data_key-vtweg   = wa_area_vendas-vtweg.
        ls_cmds_ei_sales-data_key-spart   = wa_area_vendas-spart.
        "ls_cmds_ei_functions-task   = cc_object_task_insert. " I Insert

        SELECT SINGLE low FROM tvarvc
          INTO ls_cmds_ei_sales-data-inco1
          WHERE name = co_inco1
            AND type = sy-langu
            AND numb = 0.

        SELECT SINGLE low FROM tvarvc
          INTO ls_cmds_ei_sales-data-inco2_l
          WHERE name = co_inco2
            AND type = sy-langu
            AND numb = 0.

        SELECT SINGLE low FROM tvarvc
          INTO ls_cmds_ei_sales-data-inco2
          WHERE name = co_inco2
            AND type = sy-langu
            AND numb = 0.

        SELECT SINGLE low FROM tvarvc
          INTO ls_cmds_ei_sales-data-ktgrd
          WHERE name = co_ktgrd
            AND type = sy-langu
            AND numb = 0.

        SELECT SINGLE low FROM tvarvc
          INTO ls_cmds_ei_sales-data-kalks
          WHERE name = co_clas_fis
            AND type = sy-langu
            AND numb = 0.

        SELECT SINGLE low FROM tvarvc
          INTO ls_cmds_ei_sales-data-vsbed
          WHERE name = co_cond_exp
            AND type = sy-langu
            AND numb = 0.

        ls_cmds_ei_functions-data-partner   = im_v_kunnr.

        ls_cmds_ei_functions-data_key-parvw = 'AG'.   "SP Emissor da ordem
        "ls_cmds_ei_functions-data_key-parza = '001'.
        APPEND ls_cmds_ei_functions TO ls_cmds_ei_sales-functions-functions.

        ls_cmds_ei_functions-data_key-parvw = 'RE'.   "BP Recebedor da fatura
        "ls_cmds_ei_functions-data_key-parza = '002'.
        APPEND ls_cmds_ei_functions TO ls_cmds_ei_sales-functions-functions.

        ls_cmds_ei_functions-data_key-parvw = 'RG'.   "PY Pagador
        "ls_cmds_ei_functions-data_key-parza = '003'.
        APPEND ls_cmds_ei_functions TO ls_cmds_ei_sales-functions-functions.

        ls_cmds_ei_functions-data_key-parvw = 'WE'.   "SH Receb.mercad.
        "ls_cmds_ei_functions-data_key-parza = '004'.
        APPEND ls_cmds_ei_functions TO ls_cmds_ei_sales-functions-functions.

        APPEND ls_cmds_ei_sales TO r_result.

      ENDLOOP.

    ENDIF.

  ENDMETHOD.

*=====================================================================
  METHOD fill_tax_ind.
    CONSTANTS:
      c_tatyp_ibrx TYPE knvi-tatyp VALUE 'IBRX' ##NO_TEXT,
      c_aland_br   TYPE knvi-aland VALUE cc_pais_iso_br.

    DATA:
      ls_cmds_ei_tax_ind TYPE cmds_ei_tax_ind.

    LOOP AT me->o_file->upload_data-tax_classf ASSIGNING FIELD-SYMBOL(<fs_tax_classf>)
        WHERE kunnr =  im_v_kunnr.

      CLEAR: ls_cmds_ei_tax_ind.

      CALL FUNCTION 'KNVI_SINGLE_READ'
        EXPORTING
          i_kunnr   = <fs_tax_classf>-kunnr   " Customer Number
          i_aland   = <fs_tax_classf>-aland   " Deliv. Country
          i_tatyp   = <fs_tax_classf>-tatyp   " Tax Category
        EXCEPTIONS
          not_found = 1                " No Entry Found
          OTHERS    = 4.

      IF sy-subrc EQ 1.
        ls_cmds_ei_tax_ind-task = cc_object_task_insert. " I Insert
      ELSE.
        ls_cmds_ei_tax_ind-task        = cc_object_task_update.  " U Update
        ls_cmds_ei_tax_ind-datax-taxkd = abap_true.
      ENDIF.

      MOVE-CORRESPONDING:
      <fs_tax_classf>       TO ls_cmds_ei_tax_ind-data_key,
      <fs_tax_classf>       TO ls_cmds_ei_tax_ind-data.

      APPEND ls_cmds_ei_tax_ind TO r_result.

    ENDLOOP."me->o_file->upload_data-tax_classf ASSIGNING FIELD-SYMBOL(<fs_tax_classf>)

    IF syst-subrc IS NOT INITIAL. "AND p_taxc EQ abap_true.

      CALL FUNCTION 'KNVI_SINGLE_READ'
        EXPORTING
          i_kunnr   = im_v_kunnr        " Customer Number
          i_aland   = c_aland_br        " Deliv. Country
          i_tatyp   = c_tatyp_ibrx      " Tax Category
        EXCEPTIONS
          not_found = 1                 " No Entry Found
          OTHERS    = 4.

      IF sy-subrc EQ 1.
        ls_cmds_ei_tax_ind-task        = cc_object_task_insert. " I Insert
      ELSE.
        ls_cmds_ei_tax_ind-task        = cc_object_task_update.  " U Update
        ls_cmds_ei_tax_ind-datax-taxkd = abap_true.
        CALL FUNCTION 'RTP_US_DB_BUT100_EXISTS'
          EXPORTING
            i_partner           = im_v_bp_number
            i_role              = me->co_rolecategory_vendas
          EXCEPTIONS
            partner_not_in_role = 1
            OTHERS              = 2.

        IF sy-subrc <> 0.

          RETURN.

        ENDIF.

      ENDIF.

      ls_cmds_ei_tax_ind-data_key-aland = c_aland_br.
      ls_cmds_ei_tax_ind-data_key-tatyp = c_tatyp_ibrx.


      SELECT SINGLE low
        FROM tvarvc
        INTO ls_cmds_ei_tax_ind-data-taxkd
        WHERE name = co_class_fis_cli
          AND type = sy-langu
          AND numb = 0.

      APPEND ls_cmds_ei_tax_ind TO r_result.

    ENDIF.

  ENDMETHOD.

**********************************************************************
  METHOD fill_contact_addresses.
    DATA:
      ls_adress        TYPE bus_ei_bupa_address,
      ls_bp_telephone  TYPE bus_ei_bupa_telephone,
      ls_bp_fax        TYPE bus_ei_bupa_fax,
      ls_bp_email      TYPE bus_ei_bupa_smtp,
      ls_bp_com_remark TYPE bus_ei_bupa_comrem.

    ls_adress-data-postal-data-postl_cod1 = im_s_contactpersons-cep_representante.
    ls_adress-data-postal-data-street     = im_s_contactpersons-logadouro_representante.
    ls_adress-data-postal-data-house_no   = im_s_contactpersons-numero_representante.
    ls_adress-data-postal-data-house_no2  = im_s_contactpersons-complemento_representante.
    ls_adress-data-postal-data-city       = im_s_contactpersons-cidade_representante.
    ls_adress-data-postal-data-district   = im_s_contactpersons-bairro_representante.
    ls_adress-data-postal-data-countryiso = me->co_pais_iso_br.
    ls_adress-data-postal-data-region     = im_s_contactpersons-uf_representante.
    ls_adress-data-postal-data-languiso   = me->co_languiso_pt.

    IF im_s_contactpersons-celular IS NOT INITIAL.
      ls_bp_telephone-contact-data-telephone  = im_s_contactpersons-celular.
      ls_bp_telephone-contact-data-country    = me->co_pais_iso_br.
      ls_bp_com_remark-data-comm_notes        = 'Celular'(009).
      ls_bp_com_remark-data-langu_iso         = me->co_languiso_pt.
      APPEND ls_bp_com_remark   TO ls_bp_telephone-remark-remarks.
      APPEND ls_bp_telephone    TO ls_adress-data-communication-phone-phone.
    ENDIF.

    IF im_s_contactpersons-telefone_1 IS NOT INITIAL.
      ls_bp_telephone-contact-data-telephone  = im_s_contactpersons-telefone_1.
      ls_bp_telephone-contact-data-country    = me->co_pais_iso_br.
      ls_bp_com_remark-data-comm_notes        = 'Telefone 1'(010).
      ls_bp_com_remark-data-langu_iso         = me->co_languiso_pt.
      APPEND ls_bp_com_remark   TO ls_bp_telephone-remark-remarks.
      APPEND ls_bp_telephone    TO ls_adress-data-communication-phone-phone.
    ENDIF.

    IF im_s_contactpersons-telefone_2 IS NOT INITIAL.
      ls_bp_telephone-contact-data-telephone  = im_s_contactpersons-telefone_2.
      ls_bp_telephone-contact-data-country    = me->co_pais_iso_br.
      ls_bp_com_remark-data-comm_notes        = 'Telefone 2'(011).
      ls_bp_com_remark-data-langu_iso         = me->co_languiso_pt.
      APPEND ls_bp_com_remark   TO ls_bp_telephone-remark-remarks.
      APPEND ls_bp_telephone    TO ls_adress-data-communication-phone-phone.
    ENDIF.

    IF im_s_contactpersons-fax IS NOT INITIAL.
      ls_bp_fax-contact-data-fax        = im_s_contactpersons-fax.
      ls_bp_fax-contact-data-country    = me->co_pais_iso_br.
      ls_bp_com_remark-data-comm_notes  = 'FAX'.
      ls_bp_com_remark-data-langu_iso   = me->co_languiso_pt.
      APPEND ls_bp_com_remark   TO ls_bp_fax-remark-remarks.
      APPEND ls_bp_fax          TO ls_adress-data-communication-fax-fax.
    ENDIF.

    IF im_s_contactpersons-email IS NOT INITIAL.
      ls_bp_email-contact-task = me->co_object_task_insert.
      ls_bp_email-contact-data-e_mail   = im_s_contactpersons-email.
      ls_bp_com_remark-data-comm_notes  = 'E-Mail'(012).
      ls_bp_com_remark-data-langu_iso   = me->co_languiso_pt.
      APPEND ls_bp_com_remark   TO ls_bp_email-remark-remarks.
      APPEND ls_bp_email        TO ls_adress-data-communication-smtp-smtp.
    ENDIF.

    IF im_s_contactpersons-email_alterna IS NOT INITIAL.
      ls_bp_email-contact-task = me->co_object_task_insert.
      ls_bp_email-contact-data-e_mail = im_s_contactpersons-email_alterna.
      ls_bp_com_remark-data-comm_notes  = 'E-Mail Alternativo'(013).
      ls_bp_com_remark-data-langu_iso   = me->co_languiso_pt.
      APPEND ls_bp_com_remark   TO ls_bp_email-remark-remarks.
      APPEND ls_bp_email        TO ls_adress-data-communication-smtp-smtp.
    ENDIF.

    IF im_s_contactpersons-email_secretaria IS NOT INITIAL.
      ls_bp_email-contact-task = me->co_object_task_insert.
      ls_bp_email-contact-data-e_mail = im_s_contactpersons-email_secretaria.
      ls_bp_com_remark-data-comm_notes  = 'E-Mail Secretaria'(014).
      ls_bp_com_remark-data-langu_iso   = me->co_languiso_pt.
      APPEND ls_bp_com_remark   TO ls_bp_email-remark-remarks.
      APPEND ls_bp_email        TO ls_adress-data-communication-smtp-smtp.
    ENDIF.

    APPEND ls_adress TO r_result.

  ENDMETHOD.


  METHOD set_icon_alv_status.

    DATA:
*      lv_icon    TYPE tp_icon,
      lt_bapiret TYPE bapiret2_t.

    MOVE-CORRESPONDING im_t_msg TO lt_bapiret.

    "S Success
    READ TABLE lt_bapiret WITH KEY type = 'S' TRANSPORTING NO FIELDS.
    IF sy-subrc IS INITIAL.
      r_icon  = icon_led_green.
    ENDIF.

    "W Warning
    READ TABLE lt_bapiret WITH KEY type = 'W' TRANSPORTING NO FIELDS.
    IF sy-subrc IS INITIAL.
      r_icon  = icon_led_yellow.
    ENDIF.

    "E Error
    READ TABLE lt_bapiret WITH KEY type = 'E' TRANSPORTING NO FIELDS.
    IF sy-subrc IS INITIAL.
      r_icon  = icon_led_red.
*      <fs_bp_numbers>-errors_found = abap_true.
    ENDIF.

    "A Abort cancel.
    READ TABLE lt_bapiret WITH KEY type = 'A' TRANSPORTING NO FIELDS.
    IF sy-subrc IS INITIAL.
      r_icon  = icon_message_critical.
*      <fs_bp_numbers>-errors_found = abap_true.
    ENDIF.

    "E erro
*    IF <fs_alv>-status IS INITIAL.
*      lv_icon  = icon_led_red.
*    ENDIF.

    CALL METHOD me->change_icon_alv_status
      EXPORTING
        im_v_icon  = r_icon
        im_v_kunnr = im_v_kunnr.

  ENDMETHOD.

  METHOD change_icon_alv_status.

    CHECK:
        im_v_icon  IS NOT INITIAL,
        im_v_kunnr IS NOT INITIAL.

    LOOP AT me->t_alv ASSIGNING FIELD-SYMBOL(<fs_alv>) USING KEY key_codigo WHERE codigo = im_v_kunnr.
      <fs_alv>-status = im_v_icon.
    ENDLOOP.

  ENDMETHOD.


  METHOD get_estimated_time.

    DATA:
      lv_time  TYPE i,
      lv_micro TYPE int8.

    GET RUN TIME FIELD lv_time.

    IF lv_time > me->time_ms.

      TRY.
          "Difference between start and end time
          lv_micro = lv_time - me->time_ms.

          "Calculates the estimated time for all interactions
          lv_micro = lv_micro * lines( me->it_bp_numbers ).

          r_result = me->convert_time( lv_micro ).

*          " CATCH cx_sy_conversion_no_number.
        CATCH cx_root.

      ENDTRY.

    ENDIF.

    me->time_ms = lv_time.

  ENDMETHOD.

  METHOD bp_relationship_create.

    DATA:
      ls_return             TYPE TABLE OF bapiret2.

    LOOP AT im_s_bp_numbers-contact ASSIGNING FIELD-SYMBOL(<fs_bp_contact_numbers>).

      CALL FUNCTION 'BUPR_CONTP_CREATE'
        EXPORTING
*         IV_PARTNER            =
          iv_partner_guid       = im_s_bp_numbers-bp_guid
*         IV_CONTACTPERSON      =
          iv_contactperson_guid = <fs_bp_contact_numbers>-guid_bp_contact
          iv_date_from          = sy-datlo
          iv_x_save             = abap_true
        TABLES
          et_return             = ls_return.

      CALL METHOD lcl_bupa=>set_commit( ).

    ENDLOOP.

  ENDMETHOD.

  METHOD check_customer_iu_successfully.

    CONSTANTS:
      co_msg_num_not_create       TYPE syst-msgno VALUE '153', "Cliente & não foi criado
      co_msg_num_created          TYPE syst-msgno VALUE '247', "O cliente &1 foi criado.
      co_msg_num_exists_bukrs     TYPE syst-msgno VALUE '152', "O cliente &1 já existe na empresa &2.
      co_msg_num_notcreated_bukrs TYPE syst-msgno VALUE '154', "Cliente &1 não foi criado para empresa &2
      co_msg_num_notcreated_org   TYPE syst-msgno VALUE '156', "Cliente &1 não está criado para área de vendas &2 &3 &4
      co_msg_num_exists_org       TYPE syst-msgno VALUE '155', "Cliente &1 já foi criado para empresa &2, área de vendas &3
      co_msg_num_created_bukrs    TYPE syst-msgno VALUE '171', "O cliente &1 foi criado na empresa &2..
      co_msg_num_created_vkorg    TYPE syst-msgno VALUE '172', "O cliente &1 foi criado na área de vendas &2 &3 &4.
      co_msg_num_created_org      TYPE syst-msgno VALUE '173', "O cliente &1 foi criado na organização de compras &2.
      co_msg_num_exists_vkorg     TYPE syst-msgno VALUE '157'. "O cliente &1 já existe na área de vendas &2 &3 &4.

    DATA:
      vl_msg_num TYPE syst-msgno VALUE IS INITIAL,
      vl_msg_ty  TYPE syst-msgty VALUE 'S'.

    CALL FUNCTION 'KNA1_SINGLE_READ'
      EXPORTING
        kzrfb         = abap_true               " Indicator: Refresh Buffer Entry
        kna1_kunnr    = im_s_bp_numbers-kunnr   " Customer Number
*       cvp_behavior  =                  " Behavior for API
      EXCEPTIONS
        not_found     = 1
        kunnr_blocked = 2
        OTHERS        = 3.

    " Cliente não foi criado!
    IF sy-subrc NE 0.

      "Msg Cliente & não foi criado
      MESSAGE ID  'F2' TYPE 'E' NUMBER co_msg_num_not_create WITH |{ im_s_bp_numbers-kunnr ALPHA = OUT }|
        INTO DATA(lv_msgdummy).

      CALL METHOD lcl_messages=>store( im_s_bp_numbers-kunnr ).

      CALL METHOD me->change_icon_alv_status
        EXPORTING
          im_v_icon  = icon_led_red
          im_v_kunnr = im_s_bp_numbers-kunnr.

      "CLiente foi criado!!!
    ELSE.

      "Msg O cliente &1 foi criado
      MESSAGE ID  'F2' TYPE 'S' NUMBER co_msg_num_created WITH |{ im_s_bp_numbers-kunnr ALPHA = OUT }|
        INTO lv_msgdummy.
      CALL METHOD lcl_messages=>store( im_s_bp_numbers-kunnr ).

      " Verifica se as extensoes para empresas foram criadas.
      LOOP AT me->t_bukrs ASSIGNING FIELD-SYMBOL(<fs_bukrs>).

        CALL FUNCTION 'KNB1_SINGLE_READ'
          EXPORTING
            i_kunnr        = im_s_bp_numbers-kunnr  " Customer Number
            i_bukrs        = <fs_bukrs>-bukrs       " Company Code
            i_reset_buffer = abap_true              " Clear Buffer? (Yes = X)
          EXCEPTIONS
            not_found      = 1                      " No Entry Found
            OTHERS         = 2.

        IF sy-subrc NE 0. "Erro

          "Msg Cliente &1 não foi criado para empresa &2
          MESSAGE ID  'F2' TYPE 'E' NUMBER co_msg_num_notcreated_bukrs
          WITH |{ im_s_bp_numbers-kunnr ALPHA = OUT }| <fs_bukrs>-bukrs
            INTO lv_msgdummy.

          CALL METHOD lcl_messages=>store( im_s_bp_numbers-kunnr ).

          CALL METHOD me->change_icon_alv_status
            EXPORTING
              im_v_icon  = icon_led_red
              im_v_kunnr = im_s_bp_numbers-kunnr.

        ELSE. "OK

          IF im_s_bp_numbers-task_customer EQ me->co_object_task_insert.

            "O cliente &1 foi criado na empresa &2
            vl_msg_num = co_msg_num_created_bukrs.

          ELSE.

            "O cliente &1 já existe na empresa &2.
            vl_msg_num = co_msg_num_exists_bukrs.

          ENDIF.

          MESSAGE ID  'F2' TYPE 'S' NUMBER vl_msg_num
          WITH |{ im_s_bp_numbers-kunnr ALPHA = OUT }| <fs_bukrs>-bukrs
            INTO lv_msgdummy.

          CALL METHOD lcl_messages=>store( im_s_bp_numbers-kunnr ).

        ENDIF.
      ENDLOOP.

      LOOP AT it_area_vendas INTO wa_area_vendas.

        CALL FUNCTION 'KNVV_SINGLE_READ'
          EXPORTING
            i_kunnr         = im_s_bp_numbers-kunnr    " Customer Number
            i_vkorg         = wa_area_vendas-vkorg     " Sales Organization
            i_vtweg         = wa_area_vendas-vtweg     " Distribution Channel
            i_spart         = wa_area_vendas-spart     " Division
          EXCEPTIONS
            not_found       = 1                " No Entry Found
            parameter_error = 2                " Error in parameters
            kunnr_blocked   = 3                " KUNNR Blocked
            OTHERS          = 4.

        IF sy-subrc EQ 0.

          "O cliente &1 já existe na área de vendas &2 &3 &4.
          vl_msg_num = co_msg_num_exists_vkorg.

        ELSE. "Erro

          "O cliente &1 foi criado na área de vendas &2 &3 &4.
          vl_msg_num = co_msg_num_created_vkorg.

        ENDIF.

        MESSAGE ID  'F2' TYPE 'S' NUMBER vl_msg_num
        WITH im_s_bp_numbers-kunnr wa_area_vendas-vkorg wa_area_vendas-vtweg wa_area_vendas-spart
          INTO lv_msgdummy.

        CALL METHOD lcl_messages=>store( im_s_bp_numbers-kunnr ).

      ENDLOOP.

*      LOOP AT me->o_file->upload_data-sales_data ASSIGNING FIELD-SYMBOL(<fs_sales_data>)
*              USING KEY key_kunnr WHERE kunnr = im_s_bp_numbers-kunnr.
*
*        CALL FUNCTION 'KNVV_SINGLE_READ'
*          EXPORTING
*            i_kunnr         = im_s_bp_numbers-kunnr  " Customer Number
*            i_vkorg         = <fs_sales_data>-vkorg  " Sales Organization
*            i_vtweg         = <fs_sales_data>-vtweg  " Distribution Channel
*            i_spart         = <fs_sales_data>-spart  " Division
*            i_reset_buffer  = abap_true              " Clear Buffer? (Yes = X)
**           i_bypassing_buffer =                     " Process Buffer? (No = X)
**           i_cvp_behavior  =
*          EXCEPTIONS
*            not_found       = 1                " No Entry Found
*            parameter_error = 2                " Error in parameters
*            kunnr_blocked   = 3                " KUNNR Blocked
*            OTHERS          = 4.
*
*        IF sy-subrc NE 0. "Erro
*
*          "Fornecedor &1 não foi criado para organização de compras &2
*          vl_msg_num = co_msg_num_notcreated_org.
*          vl_msg_ty  = 'E'.
*
*          CALL METHOD me->change_icon_alv_status
*            EXPORTING
*              im_v_icon  = icon_led_red
*              im_v_kunnr = im_s_bp_numbers-kunnr.
*
*        ELSE. "OK
*
*          "Já existe fornecedor &1 para organização de compras &2
*          vl_msg_num = co_msg_num_exists_org.
*          vl_msg_ty  = 'S'.
*        ENDIF.
*
*        MESSAGE ID  'F2' TYPE vl_msg_ty NUMBER vl_msg_num
*         WITH |{ <fs_sales_data>-kunnr ALPHA = OUT }| <fs_sales_data>-vkorg
*            INTO lv_msgdummy.
*
*        CALL METHOD lcl_messages=>store( im_s_bp_numbers-kunnr ).
*
*
*      ENDLOOP.

    ENDIF.

  ENDMETHOD.

  METHOD check_customer_exists.

    CALL FUNCTION 'KNA1_SINGLE_READ'
      EXPORTING
        kzrfb         = abap_true               " Indicator: Refresh Buffer Entry
        kna1_kunnr    = ch_s_bp_numbers-kunnr   " Customer Number
*       cvp_behavior  =                  " Behavior for API
      EXCEPTIONS
        not_found     = 1
        kunnr_blocked = 2
        OTHERS        = 3.

    IF sy-subrc EQ 1.
*    Customer does not exist
      ch_s_bp_numbers-create_customer = abap_true.
      ch_s_bp_numbers-task_customer   = me->co_object_task_insert.

    ELSE.
*    Customer already exists
      ch_s_bp_numbers-create_customer = abap_false.
      ch_s_bp_numbers-task_customer   = me->co_object_task_update.
    ENDIF.

  ENDMETHOD.

*&----------------------------------------------------------------------*
*&  METHOD EFFECTIVE_LOAD
*&----------------------------------------------------------------------*
  METHOD effective_load.

    READ TABLE me->t_alv WITH KEY status = icon_led_red TRANSPORTING NO FIELDS.

    IF sy-subrc EQ 0.

      DATA lv_answer TYPE c.

      CALL FUNCTION 'POPUP_TO_CONFIRM'
        EXPORTING
          titlebar              = 'Confirmation'(019)
          text_question         = 'Ocorreram erros Deseja realizar a carga efetiva?'(018)
          text_button_1         = 'Sim'(020)
          text_button_2         = 'Não'(021)
          default_button        = '2'
          display_cancel_button = abap_false
        IMPORTING
          answer                = lv_answer " to hold the FM's return value
        EXCEPTIONS
          text_not_found        = 1
          OTHERS                = 2.
      IF sy-subrc NE 0.
        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
        WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
      ENDIF.

      IF lv_answer EQ '1'.

*       Sim executa
        me->test = abap_false.

      ELSE.
*       Não
        RETURN.

      ENDIF.

    ELSE.

      me->test = abap_false.

    ENDIF.

    me->time_start = syst-timlo.

    CALL METHOD me->create_bp.

    me->time_finish = syst-timlo.

  ENDMETHOD.

  METHOD get_test.
    r_result = me->test.
  ENDMETHOD.

  METHOD set_test.
    me->test = i_test.
  ENDMETHOD.

  METHOD get_time_finish.
    r_result = me->time_finish.
  ENDMETHOD.

  METHOD set_time_finish.
    me->time_finish = im_v_time_finish.
  ENDMETHOD.

  METHOD get_time_start.
    r_result = me->time_start.
  ENDMETHOD.

  METHOD get_elapsed_time.
    r_return = me->time_finish - me->time_start.
  ENDMETHOD.

  METHOD convert_time.

    DATA:
      lv_time  TYPE i,
      lv_micro TYPE int8,
      lv_sec_n TYPE i,
      w_min    TYPE c LENGTH 3,
      w_sec    TYPE c LENGTH 2.

    TRY.
        "Convert to seconds
        IF im_v_micro IS INITIAL.
          lv_sec_n = im_v_sec.
        ELSE.
          lv_sec_n = abs( im_v_micro DIV 1000000 ).
        ENDIF.

        "Sample data in w_second variable
        ex_v_min =  lv_sec_n DIV 60 .
        ex_v_sec =  lv_sec_n MOD 60 .

        IF im_v_micro IS INITIAL.
          r_result = |Est. { ex_v_min }m { ex_v_sec }s |.
        ELSE.
          UNPACK ex_v_min TO w_min.
          UNPACK ex_v_sec TO w_sec.

          r_result = |Est. { w_min }m { w_sec }s |.
        ENDIF.

        " CATCH cx_sy_conversion_no_number.
      CATCH cx_root.

    ENDTRY.

  ENDMETHOD.


  METHOD get_taxjurcode.

    DATA:
      ls_adrs_post TYPE adrs_post,
      lv_country   TYPE t005-land1 VALUE me->co_pais_iso_br,
      lt_treg_city TYPE TABLE OF j_1btreg_city.

    MOVE-CORRESPONDING im_s_addresses TO ls_adrs_post.

    ls_adrs_post-post_code1 = im_s_addresses-postl_cod1.
    ls_adrs_post-city1 = im_s_addresses-city.

    CALL FUNCTION 'ADDR_POSTAL_CODE_CHECK'
      EXPORTING
        country                        = lv_country     " Country Indicator
        postal_address                 = ls_adrs_post
      EXCEPTIONS
        country_not_valid              = 1
        region_not_valid               = 2
        postal_code_city_not_valid     = 3
        postal_code_po_box_not_valid   = 4
        postal_code_company_not_valid  = 5
        po_box_missing                 = 6
        postal_code_po_box_missing     = 7
        postal_code_missing            = 8
        postal_code_pobox_comp_missing = 9
        po_box_region_not_valid        = 10
        po_box_country_not_valid       = 11
        pobox_and_poboxnum_filled      = 12
        OTHERS                         = 13.
    IF sy-subrc NE 0.
      CALL METHOD lcl_messages=>store( me->bp_numbers-kunnr ).
    ENDIF.

    SELECT * FROM j_1btreg_city INTO TABLE lt_treg_city
      WHERE
            country    =  lv_country AND
*           region     =  location_data-state AND
            pstcd_from <= ls_adrs_post-post_code1 AND
            pstcd_to   >= ls_adrs_post-post_code1.

    IF syst-subrc IS INITIAL.

      LOOP AT lt_treg_city ASSIGNING FIELD-SYMBOL(<fs_treg_city>).
        IF ls_adrs_post-region EQ <fs_treg_city>-region.
          r_result = <fs_treg_city>-taxjurcode.
        ELSE.

          MESSAGE ID 'Z_BUPA' TYPE 'E' NUMBER '011'
            WITH ls_adrs_post-post_code1 ls_adrs_post-region <fs_treg_city>-region
            INTO DATA(lv_msg_dummy).
          CALL METHOD lcl_messages=>store( me->bp_numbers-kunnr ).

        ENDIF.
      ENDLOOP.

    ELSE.
      MESSAGE ID 'Z_BUPA' TYPE 'E' NUMBER '010'
          WITH ls_adrs_post-post_code1 INTO lv_msg_dummy.
      CALL METHOD lcl_messages=>store( me->bp_numbers-kunnr ).
    ENDIF.

  ENDMETHOD.

ENDCLASS.                       " lcl_bupa

"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
*&---------------------------------------------------------------------*
*& Class (Implementation) cl_file
*&---------------------------------------------------------------------*
CLASS lcl_file IMPLEMENTATION.

  METHOD get_init_filename.

    DATA lv_value TYPE filepath.

    " Get the path parameter set for the user
    GET PARAMETER ID lcl_file=>co_parameter_id FIELD lv_value.

    CALL METHOD cl_gui_frontend_services=>get_upload_download_path
      CHANGING
        upload_path   = ex_v_dirup
        download_path = ex_v_dirdown.

    " If the parameter is not set / empty
    " takes the definition of the working folder of the sapgui user
    IF lv_value IS INITIAL.
      CONCATENATE ex_v_dirup 'ONS.xlsx' INTO r_filename.
    ELSE.
      CONCATENATE ex_v_dirup lv_value   INTO r_filename.
    ENDIF.

  ENDMETHOD.                    "get_init_filename

*&---------------------------------------------------------------------*
*&  METHOD SELECT_FILE
*&---------------------------------------------------------------------*
  METHOD select_file.
    DATA:
      lt_filetable TYPE filetable,
      lv_subrc     TYPE sysubrc.

    CONSTANTS co_file_ext(26) TYPE c VALUE '*.xlsx;*.xlsm;*.xlsb;*.xls'.

    CALL METHOD cl_gui_frontend_services=>file_open_dialog
      EXPORTING
        window_title            = |{ TEXT-006 }|
        file_filter             = |{ TEXT-004 } ({ co_file_ext })\| { co_file_ext }\|  { TEXT-005 } (*.*)\|*.*|
        multiselection          = abap_false
      CHANGING
        file_table              = lt_filetable
        rc                      = lv_subrc
      EXCEPTIONS
        file_open_dialog_failed = 1
        cntl_error              = 2
        error_no_gui            = 3
        not_supported_by_gui    = 4
        OTHERS                  = 5.

    IF sy-subrc NE 0.
      RETURN.
    ENDIF.

    READ TABLE lt_filetable INTO r_filename INDEX 1.

  ENDMETHOD.                    "select_file

*&---------------------------------------------------------------------*
*&  METHOD UPLOAD
*&---------------------------------------------------------------------*
  METHOD upload.

    DATA :
      it_raw               TYPE truxs_t_text_data.

    FIELD-SYMBOLS: <fs_table_excel> LIKE LINE OF me->table_excel.

    CALL METHOD lcl_messages=>progress_indicator
      EXPORTING
        im_v_text = 'Carregando dados da planilha'(016).

    CALL FUNCTION 'TEXT_CONVERT_XLS_TO_SAP'
      EXPORTING
        i_line_header        = abap_true        " Character Field Length 1
        i_tab_raw_data       = it_raw           " WORK TABLE
        i_filename           = p_file           " Local file for upload/download
      TABLES
        i_tab_converted_data = me->table_excel  " Predefined Type
      EXCEPTIONS
        conversion_failed    = 1
        OTHERS               = 2.

    IF sy-subrc NE 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
        WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4
        RAISING conversion_failed.

    ENDIF.

* Checks whether any data has been imported
    IF me->table_excel IS INITIAL.
      MESSAGE ID 'Z_BUPA' TYPE cc_msg_error NUMBER '007'
        RAISING upload_date_not_found.
    ENDIF.

*  Add leading zeros to the customer number.,
    LOOP AT me->table_excel ASSIGNING <fs_table_excel>.
      <fs_table_excel>-codigo = |{ <fs_table_excel>-codigo ALPHA = IN }|.
    ENDLOOP.

  ENDMETHOD.

*&---------------------------------------------------------------------*
*&  METHOD set_sscrtexts
*&---------------------------------------------------------------------*
  METHOD set_sscrtexts.

    DATA l_text TYPE smp_dyntxt.

    MOVE:
      icon_export          TO l_text-icon_id,
      'Baixar modelo'(t03) TO l_text-text,                  "#EC *
      'Baixar modelo'(t03) TO l_text-icon_text.

    sscrfields-functxt_01 = l_text.

  ENDMETHOD.

*&---------------------------------------------------------------------*
*&  METHOD EXPORT_MODEL
*&---------------------------------------------------------------------*
  METHOD export_model.

    DATA:
      lo_table  TYPE REF TO cl_salv_table,
      lt_model  TYPE STANDARD TABLE OF ty_e_upload_layout,
      lx_xml    TYPE xstring,
      vl_return TYPE c.

    CONSTANTS:
      c_default_extension TYPE string VALUE 'xlsx',
      c_default_file_name TYPE string VALUE ' modelo.xlsx',
      c_default_mask      TYPE string VALUE 'Excel (*.xlsx)|*.xlsx' ##NO_TEXT.

    TRY .
        cl_salv_table=>factory( IMPORTING r_salv_table = lo_table CHANGING t_table = lt_model ).
      CATCH cx_root.

    ENDTRY.

    lx_xml = lo_table->to_xml( xml_type = '10' ). "XLSX

    CALL FUNCTION 'XML_EXPORT_DIALOG'
      EXPORTING
        i_xml                      = lx_xml
        i_default_extension        = c_default_extension
        i_initial_directory        = lcl_file=>get_init_filename( )
        i_default_file_name        = syst-tcode && c_default_file_name
        i_mask                     = c_default_mask
      EXCEPTIONS
        application_not_executable = 1
        OTHERS                     = 2.

  ENDMETHOD.

  METHOD set_parameter_id.

    DATA :
      lv_dir  TYPE localfile,
      lv_file TYPE localfile.

    CALL FUNCTION 'SO_SPLIT_FILE_AND_PATH'
      EXPORTING
        full_name     = p_file
      IMPORTING
        stripped_name = lv_file   "file name
        file_path     = lv_dir    "directory path
      EXCEPTIONS
        x_error       = 1
        OTHERS        = 2.

    SET PARAMETER ID lcl_file=>co_parameter_id FIELD lv_file.

  ENDMETHOD.

ENDCLASS.                       "lcl_file

"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
*&---------------------------------------------------------------------*
*& Class (Implementation) cl_messages
*&---------------------------------------------------------------------*
CLASS lcl_messages IMPLEMENTATION.

  METHOD initialize.

*    Initialize message store.
    CALL FUNCTION 'MESSAGES_ACTIVE'
      EXCEPTIONS
        not_active = 1
        OTHERS     = 2.
    IF sy-subrc EQ 1.
      CALL FUNCTION 'MESSAGES_INITIALIZE'
        EXCEPTIONS
          log_not_active       = 1
          wrong_identification = 2
          OTHERS               = 3.
    ENDIF.

    IF sy-subrc NE 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
              WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.
  ENDMETHOD.

*&---------------------------------------------------------------------*
*& METHOD SHOW
*&   Show all messages stored
*&---------------------------------------------------------------------*
  METHOD show.
    DATA:
        lv_line TYPE i.

    MOVE im_v_line TO lv_line .

    CALL FUNCTION 'MESSAGES_SHOW'
      EXPORTING
        line_from          = lv_line          " Only show messages with longer reference line
        line_to            = lv_line          " Only show messages with shorter reference line
        batch_list_type    = 'J'              " J = job log / L = in spool list / B = both
        show_linno_text    = 'Cliente'(017)   " Column header for row
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

*&---------------------------------------------------------------------*
*& METHOD STORE
*&   Store messages with lines
*&---------------------------------------------------------------------*
  METHOD store .
    DATA:
        lv_zeile TYPE i.

    lv_zeile = |{ im_v_zeile ALPHA = OUT }|.

    CALL FUNCTION 'MESSAGE_STORE'
      EXPORTING
        exception_if_not_active = abap_true     " X = exception not_active is initialized if
        arbgb                   = syst-msgid    " Message ID
        msgty                   = syst-msgty    " Type of message (I, S, W, E, A)
        msgv1                   = syst-msgv1    " First variable parameter of message
        msgv2                   = syst-msgv2    " Second variable parameter of message
        msgv3                   = syst-msgv3    " Third variable parameter of message
        msgv4                   = syst-msgv4    " Fourth variable parameter of message
        txtnr                   = sy-msgno      " Message Number
        zeile                   = lv_zeile      " Reference line (if it exists)
      EXCEPTIONS
        message_type_not_valid  = 1                " Type of message not I, S, W, E or A
        not_active              = 2                " Collection of messages not activated
        OTHERS                  = 3.
    IF sy-subrc IS NOT INITIAL.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
        WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.

  ENDMETHOD.

*&---------------------------------------------------------------------*
*& METHOD store_validate
*&   Store messages with lines from method validate
*&---------------------------------------------------------------------*
  METHOD store_validate.

    LOOP AT im_t_return_map ASSIGNING FIELD-SYMBOL(<fs_return_map>).
      MESSAGE ID  <fs_return_map>-id
      TYPE        <fs_return_map>-type
      NUMBER      <fs_return_map>-number
      WITH        <fs_return_map>-message_v1
                  <fs_return_map>-message_v2
                  <fs_return_map>-message_v3
                  <fs_return_map>-message_v4
      INTO DATA(lv_msgdummy).

      CALL METHOD lcl_messages=>store( im_v_index_line ).

    ENDLOOP.

  ENDMETHOD.

*&---------------------------------------------------------------------*
*& Form PROGRESS_INDICATOR
*&---------------------------------------------------------------------*
*& --> iv_text      Texto a ser utilizado na mensagem
*& --> iv_processed Valor sendo processado
*& --> iv_total     Total do valores a serem processados
*&---------------------------------------------------------------------*
  METHOD progress_indicator.

    DATA:
      lv_text     TYPE string,
      lv_output_i TYPE boole_d VALUE IS INITIAL.

    IF im_v_total IS NOT INITIAL.

      lv_text = | { im_v_text } [{ im_v_processed } de { im_v_total }] |.

    ELSE.

      lv_text = im_v_text .

    ENDIF.

    IF im_v_processed LT 4.
      lv_output_i = abap_true.
    ENDIF.


    CALL METHOD cl_progress_indicator=>progress_indicate
      EXPORTING
        i_text               = lv_text          " Progress Text (If no message transferred in I_MSG*)
        i_processed          = im_v_processed   " Number of Objects Already Processed
        i_total              = im_v_total       " Total Number of Objects to Be Processed
        i_output_immediately = lv_output_i.     " X = Display Progress Immediately

  ENDMETHOD.

ENDCLASS.
