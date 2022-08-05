*--------------------------------------------------------------------*
*               P R O J E T O    A G I R - T A E S A                 *
*--------------------------------------------------------------------*
* Consultoria .....: I N T E C H P R O                               *
* Res. ABAP........: Marcelo Alvares ( MA004818 )                    *
* Res. Funcional...: Marcelo Alvares                                 *
* Módulo...........: FI                                              *
* Programa.........: ZBUPA_CARGA_VENDOR_EMAIL                        *
* Transação........: ZBP003                                          *
* Tipo de Programa.: REPORT                                          *
* Request..........:                                                 *
* Objetivo.........: Carga de emails de representantes no BP         *
*--------------------------------------------------------------------*
* Change Control:                                                    *
* Version | Date      | Who                 |   What                 *
*    1.00 | 09/03/20  | Marcelo Alvares     |   Versão Inicial       *
**********************************************************************
REPORT zbupa_carga_vendor_email MESSAGE-ID zcmcarga.

TABLES:
  sscrfields.

TYPE-POOLS icon.

CONSTANTS cc_parameter_id TYPE memoryid VALUE 'ZBP003'.

*--------------------------------------------------------------------
*   PARÂMETROS DE SELEÇÃO
*--------------------------------------------------------------------
SELECTION-SCREEN:
  FUNCTION KEY 1.

SELECTION-SCREEN BEGIN OF BLOCK b011 WITH FRAME TITLE TEXT-t01.

PARAMETERS:
  p_file    TYPE rlgrap-filename MEMORY ID cc_parameter_id ##EXISTS.

SELECTION-SCREEN SKIP.

PARAMETERS:
  p_test    TYPE xtest   AS CHECKBOX DEFAULT abap_true.

SELECTION-SCREEN END OF BLOCK b011.

CLASS lcx_select_error DEFINITION INHERITING FROM zcx_abap_error FINAL.
ENDCLASS.

CLASS lcl_vendor_email DEFINITION FINAL.

  PUBLIC SECTION.

    TYPES:
      BEGIN OF ty_s_upload_layout,
        vendor TYPE lifnr,       " Account Number of Vendor or Creditor
        taxnum TYPE bptaxnum,
        e_mail TYPE bapiadsmtp-e_mail,
      END OF ty_s_upload_layout,

      BEGIN OF ty_s_alv,
        partner  TYPE but000-partner,
        vendor   TYPE cvi_vend_link-vendor,
        taxnum   TYPE bptaxnum,
        mc_name1 TYPE but000-mc_name1,
        e_mail   TYPE bapiadsmtp-e_mail,
        status   TYPE status1,
        message  TYPE bapiret2-message,
      END OF ty_s_alv,

      BEGIN OF ty_s_data,
        partner        TYPE but000-partner,
        partner_guid   TYPE but000-partner_guid,
        vendor         TYPE cvi_vend_link-vendor,
        taxnum         TYPE bptaxnum,
        mc_name1       TYPE but000-mc_name1,
        e_mail         TYPE bapiadsmtp-e_mail,
        status         TYPE status1,
        message        TYPE bapiret2-message,
        std_addrnumber TYPE ad_addrnum,
        std_addrguid   TYPE bu_address_guid,
        smtp_t         TYPE bapiadsmtp_t,
        return         TYPE bapiret2_t,
        error          TYPE abap_bool,
      END OF ty_s_data,

      ty_t_data          TYPE STANDARD TABLE OF ty_s_data WITH KEY partner partner_guid vendor taxnum
                            WITH NON-UNIQUE SORTED KEY sorted_key COMPONENTS vendor partner,

      ty_t_upload_layout TYPE STANDARD TABLE OF ty_s_upload_layout WITH KEY vendor taxnum e_mail
                            WITH NON-UNIQUE SORTED KEY sorted_key COMPONENTS vendor taxnum e_mail
                            WITH NON-UNIQUE SORTED KEY email_key  COMPONENTS e_mail,


      ty_t_alv           TYPE STANDARD TABLE OF ty_s_alv WITH NON-UNIQUE KEY vendor taxnum e_mail
                            WITH NON-UNIQUE SORTED KEY sorted_key COMPONENTS vendor taxnum e_mail
                            WITH NON-UNIQUE SORTED KEY email_key  COMPONENTS e_mail.

    CLASS-METHODS:
      export_model.

    METHODS:
      constructor,
      upload_file
        RAISING
          zcx_file_upload
          lcx_select_error,
      select_vendor
        IMPORTING
          im_t_upload_layout TYPE ty_t_upload_layout
        RAISING
          lcx_select_error,
      process_data,
      alv_show.

  PRIVATE SECTION.
    DATA:
      go_file_upload        TYPE REF TO zcl_file_upload,
      gt_alv                TYPE ty_t_alv,
      gt_data               TYPE ty_t_data,
      gv_save               TYPE ad_save,
      go_progress_indicator TYPE REF TO zcl_progress_indicator,
      go_alv_table          TYPE REF TO cl_salv_table.

    METHODS:
      alv_create,

      alv_set_columns
        RAISING
          cx_salv_not_found,

      on_alv_link_click     FOR EVENT link_click OF cl_salv_events_table
        IMPORTING
            row
            column,
*      on_alv_user_command   FOR EVENT if_salv_events_functions~added_function  OF cl_salv_events_table
*        IMPORTING
*            e_salv_function,
      on_alv_double_click   FOR EVENT double_click OF cl_salv_events_table
        IMPORTING
            row
            column ##NEEDED,
      alv_show_error
        IMPORTING
          im_s_alv TYPE lcl_vendor_email=>ty_s_alv.

ENDCLASS.


INITIALIZATION.
  zcl_file_upload=>set_sscrtexts_export_model(
    RECEIVING
      r_sscrfields = sscrfields              ).

  zcl_file_upload=>get_parameter_file_path_value(
    EXPORTING
      im_v_id_spa_gpa = cc_parameter_id
    RECEIVING
      r_file_path = p_file                      ).

*----------------------------------------------------------------------*
* AT SELECTION-SCREEN
*----------------------------------------------------------------------*
AT SELECTION-SCREEN.
  CASE sscrfields-ucomm.
    WHEN'FC01'.
      lcl_vendor_email=>export_model( ).

  ENDCASE.

*----------------------------------------------------------------------*
* AT SELECTION-SCREEN ON VALUE-REQUEST FOR
*----------------------------------------------------------------------*
AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_file.
  p_file = zcl_file_upload=>select_file_open_dialog( ).

*----------------------------------------------------------------------*
* START-OF-SELECTION
*----------------------------------------------------------------------*
START-OF-SELECTION.

  TRY.
      DATA(go_vendor_email) = NEW lcl_vendor_email( ).

      go_vendor_email->upload_file( ).
      go_vendor_email->process_data( ).

    CATCH cx_root ##CATCH_ALL.
      STOP.
  ENDTRY.


CLASS lcl_vendor_email IMPLEMENTATION.

  METHOD constructor.

    IF p_test IS INITIAL.
      me->gv_save = abap_on.
    ELSE.
      CLEAR me->gv_save.
    ENDIF.

    alv_create( ).

    CREATE OBJECT me->go_file_upload
      EXPORTING
        im_v_file_path = p_file.

  ENDMETHOD.

  METHOD upload_file.

    DATA:
      lt_upload_layout  TYPE ty_t_upload_layout.

    SET PARAMETER ID cc_parameter_id FIELD p_file.

    me->go_file_upload->upload_file(
      EXPORTING
        im_v_file_path        = p_file
        im_v_use_abapxlsx     = abap_true
      CHANGING
        ch_tab_converted_data = lt_upload_layout
      EXCEPTIONS
        conversion_failed     = 1
        upload_date_not_found = 2
        OTHERS                = 3 ).

    IF sy-subrc NE 0.
      MESSAGE ID sy-msgid TYPE 'S' NUMBER sy-msgno DISPLAY LIKE 'E'
        WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4." INTO DATA(lv_msgdummy).
      RAISE EXCEPTION TYPE zcx_file_upload.
    ENDIF.

    DELETE ADJACENT DUPLICATES FROM lt_upload_layout USING KEY sorted_key.

    LOOP  AT lt_upload_layout ASSIGNING FIELD-SYMBOL(<fs_upload_layout>).
      UNPACK <fs_upload_layout>-vendor  TO <fs_upload_layout>-vendor.
    ENDLOOP.

    me->select_vendor( lt_upload_layout ).

  ENDMETHOD.


  METHOD select_vendor.

    SELECT DISTINCT
      but00~partner, cvi~vendor, but00~partner_guid, bptaxnum~taxnum, but00~mc_name1
       FROM cvi_vend_link           AS cvi           " Assignment Between vendor and Business Partner
       INNER JOIN but000            AS but00    ON cvi~partner_guid = but00~partner_guid    " BP: General data I
       INNER JOIN dfkkbptaxnum      AS bptaxnum ON bptaxnum~partner = but00~partner AND     " BP: General data I
                                                 ( bptaxnum~taxtype = 'BR1'     " CNPJ
                                                OR bptaxnum~taxtype = 'BR2' )   " CPF
       INTO CORRESPONDING FIELDS OF TABLE @me->gt_data
       FOR ALL ENTRIES IN @im_t_upload_layout
      WHERE cvi~vendor      EQ @im_t_upload_layout-vendor OR
            bptaxnum~taxnum EQ @im_t_upload_layout-taxnum ##TOO_MANY_ITAB_FIELDS.

    IF sy-subrc NE 0.
      MESSAGE 'Não foi encontrado nenhum fornecedor com os dados informados'(001) TYPE 'S' DISPLAY LIKE 'E'.
      RAISE EXCEPTION TYPE lcx_select_error.
    ENDIF.

    " Identifies all data not found and assign the loaded data to the corresponding fields
    LOOP AT im_t_upload_layout ASSIGNING FIELD-SYMBOL(<fs_upload_layout>).  " WHERE vendor NOT IN lr_vendor
      "   AND taxnum NOT IN lr_taxnum.

      LOOP AT me->gt_data ASSIGNING FIELD-SYMBOL(<fs_data>) WHERE vendor EQ <fs_upload_layout>-vendor OR
                                                                  taxnum EQ <fs_upload_layout>-taxnum.

        APPEND INITIAL LINE TO <fs_data>-smtp_t ASSIGNING FIELD-SYMBOL(<smtp>).
        <smtp>-e_mail   = <fs_upload_layout>-e_mail.

      ENDLOOP.

      IF syst-subrc IS NOT INITIAL. "ERROR!
        APPEND INITIAL LINE TO gt_alv ASSIGNING FIELD-SYMBOL(<fs_alv>).
        MOVE-CORRESPONDING <fs_upload_layout> TO <fs_alv>.
        <fs_alv>-status = icon_cancel.
        <fs_alv>-message = 'Não foi encontrado nenhum fornecedor com os dados informados'(001).
      ENDIF.

    ENDLOOP.

    CREATE OBJECT me->go_progress_indicator
      EXPORTING
        im_v_total = lines( me->gt_data ).

  ENDMETHOD.


  METHOD process_data.

    DATA:
      lt_smtp_current TYPE bapiadsmtp_t,
      lt_bapiadsmtx   TYPE bapiadsmtx_t.

    LOOP AT gt_data ASSIGNING FIELD-SYMBOL(<fs_data>) USING KEY sorted_key.

      CLEAR: lt_bapiadsmtx, lt_smtp_current.

      go_progress_indicator->show(
        EXPORTING
          im_v_text      = |{ 'Processando BP'(003) } { <fs_data>-partner ALPHA = OUT }/{ <fs_data>-vendor ALPHA = OUT }:|
          im_v_processed = sy-tabix ).

      CALL FUNCTION 'BUPA_ADDRESSES_GET'
        EXPORTING
          iv_partner             = <fs_data>-partner              " Business Partner Number
          iv_partner_guid        = <fs_data>-partner_guid         " Business Partner GUID
          iv_valid_date          = sy-datlo                       " Validity Date
        IMPORTING
          ev_standard_addrnumber = <fs_data>-std_addrnumber       " Address Number
          ev_standard_addrguid   = <fs_data>-std_addrguid         " UUID in Character Format
*         ev_standard_used_instead = lv_standard_used_instead     " Indicator: Address is Standard Address
        TABLES
          et_return              = <fs_data>-return.              " Messages

      READ TABLE <fs_data>-return ASSIGNING FIELD-SYMBOL(<return>) WITH KEY type = 'E' .

      IF syst-subrc IS INITIAL.
        <fs_data>-message = <return>-message.
        <fs_data>-status  = icon_cancel.
        CONTINUE.
      ENDIF.

      CALL FUNCTION 'BUPA_ADDRESS_GET_DETAIL'
        EXPORTING
          iv_partner      = <fs_data>-partner          " Business Partner Number
          iv_partner_guid = <fs_data>-partner_guid     " Business Partner GUID
          iv_addrnumber   = <fs_data>-std_addrnumber   " Address Number
          iv_addrguid     = <fs_data>-std_addrguid     " GUID of a Business Partner Address
          iv_valdt        = sy-datlo                   " Validity Date
        TABLES
          et_adsmtp       = lt_smtp_current            " E-Mail Addresses
          et_return       = <fs_data>-return.          " Messages

      " Compare base email with uploaded emails
      LOOP AT <fs_data>-smtp_t ASSIGNING FIELD-SYMBOL(<fs_smtp>) USING KEY primary_key.
        READ TABLE lt_smtp_current WITH KEY e_mail = <fs_smtp>-e_mail ASSIGNING FIELD-SYMBOL(<fs_smtp_current>).

        " If the email already exists in the database, it does nothing, it keeps the email registered.
        IF syst-subrc IS INITIAL.

          APPEND INITIAL LINE TO gt_alv ASSIGNING FIELD-SYMBOL(<fs_alv>).
          MOVE-CORRESPONDING <fs_data> TO <fs_alv>.

          <fs_alv>-message = 'E-Mail já existe no cadastro!'(002).
          <fs_alv>-status  = icon_okay.
          <fs_alv>-e_mail  = <fs_smtp_current>-e_mail.

          DELETE <fs_data>-smtp_t USING KEY loop_key.
          DELETE TABLE lt_smtp_current FROM <fs_smtp_current>.
        ELSE.
          APPEND INITIAL LINE TO lt_bapiadsmtx ASSIGNING FIELD-SYMBOL(<bapiadsmtx>).
          <bapiadsmtx>-updateflag = 'I'.
        ENDIF.
      ENDLOOP.

      IF <fs_data>-smtp_t IS NOT INITIAL.

        CALL FUNCTION 'BUPA_ADDRESS_CHANGE'
          EXPORTING
            iv_partner      = <fs_data>-partner          " Business Partner Number
            iv_partner_guid = <fs_data>-partner_guid     " Business Partner GUID
            iv_addrnumber   = <fs_data>-std_addrnumber   " Address Number
            iv_addrguid     = <fs_data>-std_addrguid     " GUID of a Business Partner Address
            iv_x_save       = me->gv_save                " Indicator
          TABLES
            it_adsmtp       = <fs_data>-smtp_t           " E-Mail Addresses
            it_adsmt_x      = lt_bapiadsmtx              " Change Information on BAPIADSMTP
            et_return       = <fs_data>-return.          " Messages

        READ TABLE <fs_data>-return WITH KEY type = 'E' ASSIGNING <return>.
        IF syst-subrc IS INITIAL.
          <fs_data>-error = abap_true.
        ENDIF.
      ENDIF.

      LOOP AT <fs_data>-smtp_t ASSIGNING <fs_smtp>.

        APPEND INITIAL LINE TO gt_alv ASSIGNING <fs_alv>.
        MOVE-CORRESPONDING <fs_data> TO <fs_alv>.
        <fs_alv>-e_mail = <fs_smtp>-e_mail.

        IF <fs_data>-error IS INITIAL.

          COMMIT WORK AND WAIT.

          <fs_alv>-message = |{ 'Cadastro de e-mail realizado com sucesso! Chave:'(008) } { <fs_smtp>-consnumber } |.
          <fs_alv>-status  = icon_okay.

        ELSE.
          <fs_alv>-message = <return>-message.
          <fs_alv>-status  = icon_cancel.
          ROLLBACK WORK.                               "#EC CI_ROLLBACK
        ENDIF.
      ENDLOOP.
    ENDLOOP.

    me->alv_show( ).

  ENDMETHOD.


  METHOD export_model.
    DATA: lt_model TYPE lcl_vendor_email=>ty_t_upload_layout.

    zcl_file_upload=>export_model( lt_model ).

  ENDMETHOD.

  METHOD alv_create.

    DATA:
      lo_events     TYPE REF TO cl_salv_events_table,
      ls_layout_key TYPE salv_s_layout_key.

    TRY.

        " Create object ALV TABLE
        CALL METHOD cl_salv_table=>factory
          IMPORTING
            r_salv_table = me->go_alv_table " Basis Class Simple ALV Tables
          CHANGING
            t_table      = me->gt_alv.

        me->go_alv_table->get_functions( )->set_all( abap_true ).

        " Set Display Settings
        me->go_alv_table->get_display_settings( )->set_horizontal_lines( abap_true ).
        me->go_alv_table->get_display_settings( )->set_striped_pattern( abap_true ).

        " Set Functional Settings
        me->go_alv_table->get_functional_settings( )->set_sort_on_header_click( abap_true ).

        " Set Selections Type
        me->go_alv_table->get_selections( )->set_selection_mode( if_salv_c_selection_mode=>row_column ).

        " Set Columns Parameters
        me->alv_set_columns( ).

        " Set Layout
        ls_layout_key-report = sy-repid.
        me->go_alv_table->get_layout( )->set_key( ls_layout_key ).
        me->go_alv_table->get_layout( )->set_save_restriction( if_salv_c_layout=>restrict_none ).

        "events
        lo_events = me->go_alv_table->get_event( ).
        SET HANDLER me->on_alv_link_click       FOR lo_events.
        SET HANDLER me->on_alv_double_click     FOR lo_events.

      CATCH cx_root INTO DATA(lcx_root) ##CATCH_ALL.
        " Deu ruim!
        MESSAGE lcx_root->get_longtext( ) TYPE 'E'.

    ENDTRY.
                                                        "#EC CI_VALPAR.
  ENDMETHOD.

  METHOD alv_set_columns.
    DATA:
      lo_alv_columns TYPE REF TO cl_salv_columns,
      lo_alv_colum   TYPE REF TO cl_salv_column_table,
      lt_components  TYPE cl_abap_structdescr=>component_table,
      lt_ddfields    TYPE ddfields ##NEEDED,
      ls_color       TYPE lvc_s_colo.

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

      IF <component>-type->kind NE cl_abap_datadescr=>kind_elem. " Elementary Type
        CONTINUE.
      ENDIF.

      lo_alv_colum ?= lo_alv_columns->get_column( columnname = |{ <component>-name }| ).
      lo_alv_colum->set_alignment( value = if_salv_c_alignment=>centered ).

      CASE <component>-name.

        WHEN 'PARTNER' OR
             'VENDOR'.

          lo_alv_colum->set_cell_type( if_salv_c_cell_type=>hotspot ).
          lo_alv_colum->set_key( abap_on ).

        WHEN 'E_MAIL'.

          ls_color-col = 4.
          ls_color-int = 0.
          ls_color-inv = 1.
          lo_alv_colum->set_cell_type( if_salv_c_cell_type=>hotspot ).
          lo_alv_colum->set_color( ls_color ).

        WHEN 'MC_NAME1'.
          lo_alv_colum->set_alignment( value = if_salv_c_alignment=>left ).

        WHEN 'STATUS'.
          lo_alv_colum->set_cell_type( if_salv_c_cell_type=>hotspot ).

        WHEN OTHERS.
          lo_alv_colum->set_optimized( abap_true ).

      ENDCASE.


    ENDLOOP.

  ENDMETHOD.


  METHOD alv_show.

    DATA:
      lo_alv_top_of_list TYPE REF TO cl_salv_form_layout_grid,
      lo_logo            TYPE REF TO cl_salv_form_layout_logo.


    CREATE OBJECT lo_logo.
    CREATE OBJECT lo_alv_top_of_list
      EXPORTING
        columns = 1.

    IF me->gv_save IS INITIAL.
      DATA(lv_text_test) = |{ 'Processamento de TESTE'(004) }|.
    ELSE.
      lv_text_test       = 'Processamento de Efetivo'(005).
    ENDIF.

    lo_alv_top_of_list->create_label( row = 1 column  = 1 )->set_text( lv_text_test ).
    lo_alv_top_of_list->create_flow(  row = 2 column  = 1 )->create_text( text = |{ 'Data de execução: '(006) } { sy-datlo DATE = USER } { syst-uzeit TIME = USER }| ).
    lo_alv_top_of_list->create_flow(  row = 3 column  = 1 )->create_text( text = |{ 'Número total de registros selecionados:'(007) } { lines( me->gt_alv ) }| ).

* Set left content
    lo_logo->set_left_content( lo_alv_top_of_list ).

* set Right Image
    lo_logo->set_right_logo( 'ZTAESA_LOGO_ALV' ).

*   set the top of list using the header for Online.
    me->go_alv_table->set_top_of_list( lo_logo ).

    SORT me->gt_alv BY partner vendor taxnum e_mail.

    CALL METHOD me->go_alv_table->display.

  ENDMETHOD.

  METHOD on_alv_link_click.

    READ TABLE me->gt_alv INDEX row ASSIGNING FIELD-SYMBOL(<row_alv>).

    CASE column.

      WHEN 'VENDOR' OR 'PARTNER'.

        CHECK <row_alv>-partner IS NOT INITIAL.

        SET PARAMETER ID 'BPA' FIELD <row_alv>-partner.
        CALL TRANSACTION 'BP' WITH AUTHORITY-CHECK.

      WHEN 'STATUS'.

        CALL METHOD me->alv_show_error( <row_alv> ).

    ENDCASE.
  ENDMETHOD.

  METHOD on_alv_double_click.

    READ TABLE me->gt_alv INDEX row ASSIGNING FIELD-SYMBOL(<row_alv>).

*    CASE column.
*
*      WHEN 'VENDOR' OR 'PARTNER'.
*
*        SET PARAMETER ID 'BPA' FIELD <row_alv>-partner.
*        CALL TRANSACTION 'BP' WITH AUTHORITY-CHECK.
*
*      WHEN OTHERS.
*
*    ENDCASE.

  ENDMETHOD.


  METHOD alv_show_error.

    READ TABLE me->gt_data
        WITH KEY partner = im_s_alv-partner
        ASSIGNING FIELD-SYMBOL(<fs_data>).

    CHECK syst-subrc IS INITIAL. " Avoid DUMP, BP not found

*    initialize message store.
    CALL FUNCTION 'MESSAGES_ACTIVE'
      EXCEPTIONS
        not_active = 1.                " Collection of messages is not acti
    IF sy-subrc EQ 1.
      CALL FUNCTION 'MESSAGES_INITIALIZE'
        EXCEPTIONS
          log_not_active       = 1
          wrong_identification = 2
          OTHERS               = 3.
      IF sy-subrc NE 0.
        MESSAGE ID sy-msgid TYPE 'E' NUMBER sy-msgno
          WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
      ENDIF.
    ENDIF.

    LOOP AT <fs_data>-return ASSIGNING FIELD-SYMBOL(<return>).

      CALL FUNCTION 'MESSAGE_STORE'
        EXPORTING
          exception_if_not_active = abap_true           " X = exception not_active is initialized if
          arbgb                   = <return>-id         " Message ID
          msgty                   = <return>-type       " Type of message (I, S, W, E, A)
          msgv1                   = <return>-message_v1 " First variable parameter of message
          msgv2                   = <return>-message_v2 " Second variable parameter of message
          msgv3                   = <return>-message_v3 " Third variable parameter of message
          msgv4                   = <return>-message_v4 " Fourth variable parameter of message
          txtnr                   = <return>-number     " Message Number
*         zeile                   =       " Reference line (if it exists)
        EXCEPTIONS
          message_type_not_valid  = 1                " Type of message not I, S, W, E or A
          not_active              = 2                " Collection of messages not activated
          OTHERS                  = 3.
      IF sy-subrc IS NOT INITIAL.
        MESSAGE ID sy-msgid TYPE 'E' NUMBER sy-msgno
          WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
      ENDIF.
    ENDLOOP.

    CALL FUNCTION 'MESSAGES_SHOW'
      EXPORTING
        batch_list_type = 'J'              " J = job log / L = in spool list / B = both
        i_use_grid      = abap_true        " Use ALV Grid for Display; Otherwise Classic ALV
      EXCEPTIONS
        OTHERS          = 3.
    IF sy-subrc NE 0.
      MESSAGE ID sy-msgid TYPE 'S' NUMBER sy-msgno DISPLAY LIKE 'E'
        WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.

    CALL FUNCTION 'MESSAGES_STOP'
      EXCEPTIONS
        OTHERS = 1.
    IF sy-subrc NE 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
        WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.

  ENDMETHOD.

ENDCLASS.
