*--------------------------------------------------------------------*
*               P R O J E T O    A G I R - T A E S A                 *
*--------------------------------------------------------------------*
* Consultoria .....: I N T E C H P R O                               *
* Res. ABAP........: Marcelo Alvares                                 *
* Res. Funcional...: Marcelo Alvares                                 *
* Módulo...........: FI                                              *
* Programa.........: ZBUPA_CARGA_BP_EMAIL                            *
* Transação........: ZCEC                                             *
* Tipo de Programa.: REPORT                                          *
* Request..........:                                                 *
* Objetivo.........: Carga de emails de representantes no BP         *
*--------------------------------------------------------------------*
* Change Control:                                                    *
* Version | Date      | Who                 |   What                 *
*    1.00 | 29/05/19  | Marcelo Alvares     |   Versão Inicial       *
**********************************************************************
REPORT zbupa_carga_bp_email MESSAGE-ID zcmcarga.

TABLES:
  sscrfields.

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


TYPES:
  BEGIN OF ty_s_upload_layout,
    kunnr TYPE kunnr,      " Nº cliente
    email TYPE bapiadsmtp-e_mail,
*    funcao_representante TYPE c LENGTH 60,
  END OF ty_s_upload_layout,

  BEGIN OF ty_s_data,
    partner        TYPE but000-partner,
    partner_guid   TYPE but000-partner_guid,
    customer       TYPE cvi_cust_link-customer,
    std_addrnumber TYPE ad_addrnum,
    std_addrguid   TYPE bu_address_guid,
    smtp_t         TYPE bapiadsmtp_t,
    comrem_t       TYPE TABLE OF bapicomrem WITH DEFAULT KEY,
  END OF ty_s_data,

  ty_t_data          TYPE TABLE OF ty_s_data WITH DEFAULT KEY
    WITH NON-UNIQUE SORTED KEY sorted_key COMPONENTS customer partner,

  ty_t_upload_layout TYPE TABLE OF ty_s_upload_layout WITH KEY kunnr email
              WITH NON-UNIQUE SORTED KEY sorted_key COMPONENTS kunnr email
              WITH NON-UNIQUE SORTED KEY email_key COMPONENTS email.

DATA:
  gt_upload_layout TYPE ty_t_upload_layout,
  gt_bp_data       TYPE ty_t_data,
  gt_return        TYPE bapiret2_t,
  gt_smtp_current  TYPE bapiadsmtp_t,
  gs_smtp          TYPE bapiadsmtp,
  gt_bapiadsmtx    TYPE bapiadsmtx_t,
  gs_bapiadsmtx    TYPE bapiadsmtx,
  go_file          TYPE REF TO zcl_file_upload,
  gv_save          LIKE p_test,
  go_pro_ind       TYPE REF TO zcl_progress_indicator.

INITIALIZATION.
  CALL METHOD zcl_file_upload=>set_sscrtexts_export_model
    RECEIVING
      r_sscrfields = sscrfields.
  CALL METHOD zcl_file_upload=>get_parameter_file_path_value
    RECEIVING
      r_file_path = p_file.

  CREATE OBJECT go_file
    EXPORTING
      im_v_file_path = p_file.

*----------------------------------------------------------------------*
* AT SELECTION-SCREEN
*----------------------------------------------------------------------*
AT SELECTION-SCREEN.
  CASE sscrfields-ucomm.
    WHEN'FC01'.
      CALL METHOD zcl_file_upload=>export_model( gt_upload_layout ).

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

  IF p_test IS INITIAL.
    gv_save = abap_on.
  ELSE.
    CLEAR gv_save.
  ENDIF.

  CALL METHOD go_file->upload_file
    EXPORTING
      im_v_file_path        = p_file
      im_v_use_abapxlsx     = abap_true
    CHANGING
      ch_tab_converted_data = gt_upload_layout
    EXCEPTIONS
      conversion_failed     = 1
      upload_date_not_found = 2
      OTHERS                = 3.

  IF sy-subrc NE 0.
    MESSAGE ID sy-msgid TYPE 'S' NUMBER sy-msgno DISPLAY LIKE 'E'
      WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    STOP.
  ENDIF.

  DELETE ADJACENT DUPLICATES FROM gt_upload_layout USING KEY sorted_key.

  LOOP AT gt_upload_layout ASSIGNING FIELD-SYMBOL(<fs_upload_layout>).
    UNPACK <fs_upload_layout>-kunnr TO <fs_upload_layout>-kunnr.
  ENDLOOP.

  SELECT
      but00~partner cvi~customer but00~partner_guid
       FROM cvi_cust_link  AS cvi           " Assignment Between Customer and Business Partner
       INNER JOIN but000   AS but00    ON cvi~partner_guid = but00~partner_guid  " BP: General data I
       INTO CORRESPONDING FIELDS OF TABLE gt_bp_data
       FOR ALL ENTRIES IN gt_upload_layout
      WHERE cvi~customer EQ gt_upload_layout-kunnr.

  CREATE OBJECT go_pro_ind
    EXPORTING
      im_v_total = lines( gt_bp_data ).

  LOOP AT gt_bp_data ASSIGNING FIELD-SYMBOL(<fs_bp_data>) USING KEY sorted_key.

    CLEAR: gs_smtp, gt_bapiadsmtx, gt_return, gt_smtp_current.

    go_pro_ind->show(
      EXPORTING
        im_v_text      = | Processando BP { <fs_bp_data>-partner ALPHA = OUT }/{ <fs_bp_data>-customer ALPHA = OUT }:|
        im_v_processed = sy-tabix ).

    FORMAT RESET.
    WRITE: | BP:{ <fs_bp_data>-partner ALPHA = OUT } Cliente:{ <fs_bp_data>-customer ALPHA = OUT }|.

    LOOP AT gt_upload_layout ASSIGNING <fs_upload_layout>
                             USING KEY sorted_key WHERE kunnr = <fs_bp_data>-customer
                                                   AND  email IS NOT INITIAL.
      MOVE:
      <fs_upload_layout>-email TO gs_smtp-e_mail,
      'I'                      TO gs_bapiadsmtx-updateflag.
      APPEND:
        gs_smtp       TO <fs_bp_data>-smtp_t,
        gs_bapiadsmtx TO gt_bapiadsmtx.
    ENDLOOP.

    CALL FUNCTION 'BUPA_ADDRESSES_GET'
      EXPORTING
        iv_partner             = <fs_bp_data>-partner          " Business Partner Number
        iv_partner_guid        = <fs_bp_data>-partner_guid     " Business Partner GUID
        iv_valid_date          = sy-datlo         " Validity Date
      IMPORTING
        ev_standard_addrnumber = <fs_bp_data>-std_addrnumber   " Address Number
        ev_standard_addrguid   = <fs_bp_data>-std_addrguid     " UUID in Character Format
*       ev_standard_used_instead = lv_standard_used_instead " Indicator: Address is Standard Address
      TABLES
        et_return              = gt_return.                 " Messages

    READ TABLE gt_return WITH KEY type = 'E' TRANSPORTING NO FIELDS.

    IF syst-subrc IS NOT INITIAL. "Erro!!!

      CALL FUNCTION 'BUPA_ADDRESS_GET_DETAIL'
        EXPORTING
          iv_partner      = <fs_bp_data>-partner          " Business Partner Number
          iv_partner_guid = <fs_bp_data>-partner_guid     " Business Partner GUID
          iv_addrnumber   = <fs_bp_data>-std_addrnumber   " Address Number
          iv_addrguid     = <fs_bp_data>-std_addrguid     " GUID of a Business Partner Address**  IMPORTING
          iv_valdt        = sy-datlo         " Validity Date
        TABLES
          et_adsmtp       = gt_smtp_current.         " E-Mail Addresses

      " Compare base email with uploaded emails
      LOOP AT <fs_bp_data>-smtp_t ASSIGNING FIELD-SYMBOL(<fs_smtp>) USING KEY primary_key.
        READ TABLE gt_smtp_current WITH KEY e_mail = <fs_smtp>-e_mail ASSIGNING FIELD-SYMBOL(<fs_smtp_current>). "INTO gs_smtp.

        " If the email already exists in the database, it does nothing, it keeps the email registered.
        IF syst-subrc IS INITIAL.
          WRITE: /2 <fs_smtp>-e_mail, 50 'JÁ CADASTRADO!'.
          DELETE <fs_bp_data>-smtp_t USING KEY loop_key.
          DELETE TABLE gt_smtp_current FROM <fs_smtp_current>."FROM gs_smtp.
          DELETE gt_bapiadsmtx INDEX 1.
        ENDIF.
      ENDLOOP.

      " mark all remaining email's for deletion
      LOOP AT gt_smtp_current ASSIGNING <fs_smtp_current>.
        gs_bapiadsmtx-updateflag = 'D'. " Delete
*        APPEND:
*            <fs_smtp_current> TO <fs_bp_data>-smtp_t ,
*            gs_bapiadsmtx     TO gt_bapiadsmtx.
        "  AM E 256 Entradas na tabela BAPIADSMT_X com índice atual.'I' têm de estar separadas no fim
        INSERT:
            <fs_smtp_current> INTO <fs_bp_data>-smtp_t INDEX 1,
            gs_bapiadsmtx     INTO gt_bapiadsmtx       INDEX 1.
      ENDLOOP.

      CALL FUNCTION 'BUPA_ADDRESS_CHANGE'
        EXPORTING
          iv_partner      = <fs_bp_data>-partner          " Business Partner Number
          iv_partner_guid = <fs_bp_data>-partner_guid     " Business Partner GUID
          iv_addrnumber   = <fs_bp_data>-std_addrnumber   " Address Number
          iv_addrguid     = <fs_bp_data>-std_addrguid     " GUID of a Business Partner Address
          iv_x_save       = gv_save                       " Indicator
        TABLES
          it_adsmtp       = <fs_bp_data>-smtp_t           " E-Mail Addresses
          it_adsmt_x      = gt_bapiadsmtx                 " Change Information on BAPIADSMTP
          et_return       = gt_return.                    " Messages

    ENDIF.

    SKIP.

    READ TABLE gt_return WITH KEY type = 'E' TRANSPORTING NO FIELDS.
    IF syst-subrc IS NOT INITIAL.
      WRITE icon_okay AS ICON.
      FORMAT COLOR COL_NORMAL.
      WRITE: | Execução realizada com sucesso!!|.
      COMMIT WORK AND WAIT.

      LOOP AT <fs_bp_data>-smtp_t ASSIGNING <fs_smtp>.
        READ TABLE gt_bapiadsmtx ASSIGNING FIELD-SYMBOL(<bapiadsmtx>) INDEX syst-tabix.
        IF <bapiadsmtx>-updateflag EQ 'D'.
          FORMAT COLOR COL_GROUP.
          WRITE: /2 <fs_smtp>-e_mail, 50 |Eliminado!  Chave:   { <fs_smtp>-consnumber }|.
        ELSE.
          FORMAT COLOR COL_POSITIVE.
          WRITE: /2 <fs_smtp>-e_mail, 50 |Cadastrado! Chave:   { <fs_smtp>-consnumber }|.
*        WRITE / | { <fs_smtp>-e_mail } Cadastrado! Chave:   { <fs_smtp>-consnumber }|.
        ENDIF.
      ENDLOOP.
    ELSE.
      WRITE icon_cancel AS ICON.
      FORMAT COLOR COL_NORMAL.
      WRITE: | Execução com Erro!!|.
      ROLLBACK WORK.                                   "#EC CI_ROLLBACK
    ENDIF.

    LOOP AT gt_return ASSIGNING FIELD-SYMBOL(<fs_return>).

      FORMAT RESET.
      SKIP.

      CASE <fs_return>-type.
        WHEN 'S'.
          WRITE icon_okay AS ICON.
          FORMAT COLOR COL_NORMAL.
        WHEN 'W'.
          WRITE icon_warning AS ICON.
          FORMAT COLOR COL_TOTAL .
        WHEN 'I'.
          WRITE icon_information AS ICON.
          FORMAT COLOR COL_NORMAL.
        WHEN 'E'.
          WRITE icon_cancel AS ICON.
          FORMAT COLOR COL_NORMAL.
      ENDCASE.

      WRITE:
        <fs_return>-id(5),
        <fs_return>-type,
        <fs_return>-number,
        <fs_return>-message(80).

    ENDLOOP.
    NEW-LINE NO-SCROLLING.
    ULINE.

  ENDLOOP.
