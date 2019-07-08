*&---------------------------------------------------------------------*
*& Include ZBUPA_CARGA_DADOS_BANC_FORN_F1
*&---------------------------------------------------------------------*
  FORM select_file CHANGING r_filename.
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

  ENDFORM.


*  FORM ler_dados_bancarios_forn.
*    DATA:
*        lv_fields  TYPE rsz_t_string.
*
*    lv_fields = zcl_utils=>get_itab_fields( gt_lfbk ).
*
*    SELECT (lv_fields) FROM lfbk
*      INTO TABLE gt_lfbk
*      WHERE lifnr IN s_forn.
*
*  ENDFORM.

*  FORM ler_dados_bp.
*
*    DATA:
*        lv_fields  TYPE rsz_t_string.
*
*    IF NOT gt_lfbk IS INITIAL.
*
*      lv_fields = zcl_utils=>get_itab_fields( gt_cvi_vend_link ).
*
*      SELECT (lv_fields) FROM cvi_vend_link
*        INTO TABLE gt_cvi_vend_link
*        FOR ALL ENTRIES IN gt_lfbk
*        WHERE vendor = gt_lfbk-lifnr.
*
*      IF NOT gt_cvi_vend_link IS INITIAL.
*
*        SELECT * FROM but000
*          INTO TABLE gt_but000
*          FOR ALL ENTRIES IN gt_cvi_vend_link
*          WHERE partner_guid = gt_cvi_vend_link-partner_guid.
*
*      ENDIF.
*
*    ENDIF.
*
*  ENDFORM.

  FORM upload.

    DATA :
      it_raw               TYPE truxs_t_text_data.

    CALL FUNCTION 'TEXT_CONVERT_XLS_TO_SAP'
      EXPORTING
        i_line_header        = abap_true        " Character Field Length 1
        i_tab_raw_data       = it_raw           " WORK TABLE
        i_filename           = p_file           " Local file for upload/download
      TABLES
        i_tab_converted_data = gt_table_excel  " Predefined Type
      EXCEPTIONS
        conversion_failed    = 1
        OTHERS               = 2.

  ENDFORM.


  FORM ler_excel.

    DATA:
      lv_partner        TYPE bapibus1006_head-bpartner,
      lv_bankdetail     TYPE but0bk-bkvid,
      lv_bankdetail_ins LIKE bapibus1006_bankdetail,
      lv_bankdetailid   LIKE bapibus1006_head-bankdetailid,
      lt_return         TYPE TABLE OF bapiret2,
      wa_return         LIKE LINE OF lt_return,
      lv_msg            TYPE c LENGTH 400,
      ls_forn           TYPE lif_range,
      ls_bankdetail     LIKE bapibus1006_bankdetail,
      lt_bankdetails    TYPE STANDARD TABLE OF bapibus1006_bankdetails.
*
*    SORT: gt_lfbk           BY lifnr,
*          gt_cvi_vend_link  BY vendor,
*          gt_but000         BY partner_guid.

* Leitura da planilha EXCEL
    LOOP AT gt_table_excel ASSIGNING FIELD-SYMBOL(<fs_table_excel>) .

      UNPACK <fs_table_excel>-lifnr TO <fs_table_excel>-lifnr.

      ls_forn-sign      = 'I'.
      ls_forn-option    = 'EQ'.
      ls_forn-lifnr_low = <fs_table_excel>-lifnr.

      APPEND ls_forn TO s_forn.

    ENDLOOP.


    CLEAR: lt_return.

    PERFORM select_bp.

    "????? why not?
    DELETE ADJACENT DUPLICATES FROM gt_but000 USING KEY but000_sec_key.

    LOOP AT gt_but000 ASSIGNING FIELD-SYMBOL(<fs_but000>) USING KEY but000_sec_key.

      PERFORM write_header USING <fs_but000>.

      CALL FUNCTION 'BAPI_BUPA_BANKDETAILS_GET'
        EXPORTING
          businesspartner = <fs_but000>-partner " Business Partner Number
        TABLES
          bankdetails     = lt_bankdetails      " Bank Details
          return          = lt_return.          " Messages

      IF lt_return IS NOT INITIAL.
        PERFORM return_msg USING lt_return <fs_but000>-vendor.
      ENDIF.



      LOOP AT gt_table_excel ASSIGNING <fs_table_excel>
        USING KEY excel_sec_key WHERE lifnr = <fs_but000>-vendor.







      ENDLOOP.



    ENDLOOP.

*    READ TABLE gt_lfbk INTO wa_lfbk WITH KEY lifnr = wa_table_excel-lifnr BINARY SEARCH.
*
*    IF sy-subrc EQ 0.
*
** Se for igual não há necessidade de excluir e incluir.
*      IF <fs_table_excel>-bvtyp EQ wa_lfbk-bvtyp.
*
*        CONTINUE.
*
*      ENDIF.
*
*      READ TABLE gt_cvi_vend_link INTO wa_cvi_vend_link WITH KEY vendor = wa_lfbk-lifnr BINARY SEARCH.
*
*      IF sy-subrc EQ 0.
*
*        READ TABLE gt_but000 INTO wa_but000 WITH KEY partner_guid = wa_cvi_vend_link-partner_guid BINARY SEARCH.
*
*        IF sy-subrc EQ 0.
*
*          lv_partner     = wa_but000-partner.
*          lv_bankdetail  = wa_lfbk-bvtyp.
*
** Excluir BAPI REMOVE e depois BAPI de inserção.
** Excluir da LFBK.
*          CALL FUNCTION 'BAPI_BUPA_BANKDETAIL_REMOVE'
*            EXPORTING
*              businesspartner = lv_partner
*              bankdetailid    = lv_bankdetail
*            TABLES
*              return          = lt_return.
*
*          READ TABLE lt_return INTO wa_return WITH KEY type = 'E'.
*
*          IF sy-subrc EQ 0.
*
*            MESSAGE ID wa_return-id TYPE wa_return-type NUMBER wa_return-number
*               WITH wa_return-message_v1 wa_return-message_v2 wa_return-message_v3 wa_return-message_v4
*               INTO lv_msg.
*
*            WRITE lv_msg.
*
*          ELSE.
*
*            CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
*              EXPORTING
*                wait = 'X'.
*
*            LOOP AT lt_return INTO wa_return.
*              MESSAGE ID wa_return-id TYPE wa_return-type NUMBER wa_return-number
*                 WITH wa_return-message_v1 wa_return-message_v2 wa_return-message_v3 wa_return-message_v4
*                 INTO lv_msg.
*
*              WRITE lv_msg.
*
*            ENDLOOP.
*
*            IF lv_msg IS INITIAL.
*
*              WRITE: '´Fornecedor', wa_lfbk-lifnr, '  ', wa_lfbk-bvtyp, 'Excluído'.
*
*            ENDIF.
*
*            lv_partner         = wa_but000-partner.
*            lv_bankdetail_ins  = wa_table_excel-bvtyp.
*
** BAPI de inserção.
*            CLEAR lt_return.
*            lv_bankdetail_ins-bank_ctry     = wa_table_excel-banks.
*            lv_bankdetail_ins-bank_ctryiso  = wa_table_excel-banks.
*            lv_bankdetail_ins-bank_key      = wa_table_excel-bankl.
*            lv_bankdetail_ins-bank_acct     = wa_table_excel-bankn.
*            lv_bankdetail_ins-ctrl_key      = wa_table_excel-bkont.
*
*            lv_bankdetailid                 = wa_table_excel-bvtyp.
*
*            CALL FUNCTION 'BAPI_BUPA_BANKDETAIL_ADD'
*              EXPORTING
*                businesspartner = lv_partner
*                bankdetailid    = lv_bankdetailid
*                bankdetaildata  = lv_bankdetail_ins
*              TABLES
*                return          = lt_return.
*
*            READ TABLE lt_return INTO wa_return WITH KEY type = 'E'.
*
*            IF sy-subrc EQ 0.
*
*              MESSAGE ID wa_return-id TYPE wa_return-type NUMBER wa_return-number
*                 WITH wa_return-message_v1 wa_return-message_v2 wa_return-message_v3 wa_return-message_v4
*                 INTO lv_msg.
*
*              WRITE lv_msg.
*
*            ELSE.
*
*              CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
*                EXPORTING
*                  wait = 'X'.
*
*              LOOP AT lt_return INTO wa_return.
*
*                MESSAGE ID wa_return-id TYPE wa_return-type NUMBER wa_return-number
*                   WITH wa_return-message_v1 wa_return-message_v2 wa_return-message_v3 wa_return-message_v4
*                   INTO lv_msg.
*
*                WRITE lv_msg.
*
*              ENDLOOP.
*
*              IF lv_msg IS INITIAL.
*
*                WRITE: '´Fornecedor', wa_lfbk-lifnr, '  ', wa_lfbk-bvtyp, 'Excluído'.
*
*              ENDIF.
*
*            ENDIF.
*
*          ENDIF.
*
*        ENDIF.
*
*      ENDIF.        .
*
** Apenas BAPI de inserção.
*    ELSE.
*
*      lv_partner                      = wa_but000-partner.
*      lv_bankdetail_ins-bank_ctry     = wa_table_excel-banks.
*      lv_bankdetail_ins-bank_ctryiso  = wa_table_excel-banks.
*      lv_bankdetail_ins-bank_key      = wa_table_excel-bankl.
*      lv_bankdetail_ins-bank_acct     = wa_table_excel-bankn.
*      lv_bankdetail_ins-ctrl_key      = wa_table_excel-bkont.
*
*      lv_bankdetailid                 = wa_table_excel-bvtyp.
*
*      CALL FUNCTION 'BAPI_BUPA_BANKDETAIL_ADD'
*        EXPORTING
*          businesspartner = lv_partner
*          bankdetailid    = lv_bankdetailid
*          bankdetaildata  = lv_bankdetail_ins
*        TABLES
*          return          = lt_return.
*
*      READ TABLE lt_return INTO wa_return WITH KEY type = 'E'.
*
*      IF sy-subrc EQ 0.
*
*        MESSAGE ID wa_return-id TYPE wa_return-type NUMBER wa_return-number
*           WITH wa_return-message_v1 wa_return-message_v2 wa_return-message_v3 wa_return-message_v4
*           INTO lv_msg.
*
*        WRITE lv_msg.
*
*      ELSE.
*
*        CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
*          EXPORTING
*            wait = 'X'.
*
*        LOOP AT lt_return INTO wa_return.
*
*          MESSAGE ID wa_return-id TYPE wa_return-type NUMBER wa_return-number
*             WITH wa_return-message_v1 wa_return-message_v2 wa_return-message_v3 wa_return-message_v4
*             INTO lv_msg.
*
*          WRITE lv_msg.
*
*        ENDLOOP.
*
*        IF lv_msg IS INITIAL.
*
*          WRITE: '´Fornecedor', wa_lfbk-lifnr, '  ', wa_lfbk-bvtyp, 'Excluído'.
*
*        ENDIF.
*
*      ENDIF.
*
*    ENDIF.
*
*  ENDLOOP.

  ENDFORM.

  FORM change_bkvid.

    DATA:
      ls_bankdetail  LIKE bapibus1006_bankdetail,
      lt_return      TYPE TABLE OF bapiret2,
      lt_bankdetails TYPE STANDARD TABLE OF bapibus1006_bankdetails.


*    SELECT
*      but~partner,
*      but~partner_guid,
*      link~vendor
*       FROM cvi_vend_link AS link
*      INNER JOIN but000 AS but ON link~partner_guid = but~partner_guid
*      INTO CORRESPONDING FIELDS OF TABLE @lt_but000
*      WHERE link~vendor IN @s_forn.

    PERFORM  select_bp.

    IF syst-subrc IS NOT INITIAL.
      MESSAGE 'Dados não encontrados' TYPE 'S' DISPLAY LIKE 'E'.
      RETURN.

    ENDIF.

    "????? why not?
    DELETE ADJACENT DUPLICATES FROM gt_but000 USING KEY but000_sec_key.

    LOOP AT gt_but000 ASSIGNING FIELD-SYMBOL(<fs_but000>) USING KEY but000_sec_key.

      FORMAT RESET.
      ULINE.
      WRITE:
        'BP :',
        <fs_but000>-partner,
        'Fornecedor:',
        <fs_but000>-vendor.
      NEW-LINE NO-SCROLLING.

      CALL FUNCTION 'BAPI_BUPA_BANKDETAILS_GET'
        EXPORTING
          businesspartner = <fs_but000>-partner " Business Partner Number
        TABLES
          bankdetails     = lt_bankdetails      " Bank Details
          return          = lt_return.          " Messages

      IF lt_return IS NOT INITIAL.
        PERFORM return_msg USING lt_return <fs_but000>-vendor.
      ENDIF.

      LOOP AT lt_bankdetails ASSIGNING FIELD-SYMBOL(<fs_bankdetails>).

        CLEAR:
          ls_bankdetail.

        IF <fs_bankdetails>-bankdetailid(3) EQ <fs_bankdetails>-bank_key(3).
          FORMAT COLOR COL_NORMAL.
          WRITE:
            icon_okay AS ICON,
            'O Fornecedor possui o ID Correto',
            <fs_bankdetails>-bankdetailid,
            'Correto:',
            <fs_bankdetails>-bank_key(3),
            'Fornecedor:',
            <fs_but000>-vendor.
          NEW-LINE NO-SCROLLING.

        ELSE.
          FORMAT COLOR COL_NEGATIVE.
          WRITE:
            icon_cancel AS ICON,
            'O Fornecedor possui o ID invalido',
            <fs_bankdetails>-bankdetailid,
            'Correto:',
            <fs_bankdetails>-bank_key(3),
            'Fornecedor:',
            <fs_but000>-vendor.
          NEW-LINE NO-SCROLLING.

* Excluir BAPI REMOVE e depois BAPI de inserção.
* Excluir da LFBK.
          CALL FUNCTION 'BAPI_BUPA_BANKDETAIL_REMOVE'
            EXPORTING
              businesspartner = <fs_but000>-partner
              bankdetailid    = <fs_bankdetails>-bankdetailid
            TABLES
              return          = lt_return.

          PERFORM return_msg USING lt_return <fs_but000>-vendor.

          READ TABLE lt_return WITH KEY type = 'E' TRANSPORTING NO FIELDS BINARY SEARCH.

          IF sy-subrc EQ 0.

            CONTINUE.

          ELSEIF p_test IS INITIAL.

            PERFORM call_bapi_commit.

            <fs_bankdetails>-bankdetailid = <fs_bankdetails>-bank_key(3).

            MOVE-CORRESPONDING <fs_bankdetails> TO ls_bankdetail.

            CALL FUNCTION 'BAPI_BUPA_BANKDETAIL_ADD'
              EXPORTING
                businesspartner = <fs_but000>-partner
                bankdetailid    = <fs_bankdetails>-bankdetailid
                bankdetaildata  = ls_bankdetail
              TABLES
                return          = lt_return.

            PERFORM return_msg USING lt_return <fs_but000>-vendor.

            READ TABLE lt_return WITH KEY type = 'E' TRANSPORTING NO FIELDS BINARY SEARCH.

            IF sy-subrc NE 0.
              PERFORM call_bapi_commit.

              FORMAT COLOR COL_POSITIVE.
              WRITE:
                icon_okay AS ICON,
                'Dados alterados com sucesso!',
                <fs_bankdetails>-bankdetailid,
                'Fornecedor:',
                <fs_but000>-vendor.
              NEW-LINE NO-SCROLLING.

            ENDIF.

          ENDIF.
        ENDIF.

      ENDLOOP.

    ENDLOOP.

  ENDFORM.
*&---------------------------------------------------------------------*
*& Form CALL_BAPI_COMMIT
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
  FORM call_bapi_commit .

    IF p_test IS INITIAL.
      CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
        EXPORTING
          wait = 'X'.
    ENDIF.

  ENDFORM.


  FORM return_msg USING i_t_return TYPE bapiret2_t
                        i_v_vendor TYPE lifnr.

    LOOP AT i_t_return ASSIGNING FIELD-SYMBOL(<fs_return>).

      FORMAT RESET.

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
        <fs_return>-message(80),
        i_v_vendor.

      NEW-LINE NO-SCROLLING.

    ENDLOOP.

  ENDFORM.
*&---------------------------------------------------------------------*
*& Form SELECT_BP
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
  FORM select_bp .

    SELECT
      but~partner,
      but~partner_guid,
      link~vendor
       FROM cvi_vend_link AS link
      INNER JOIN but000 AS but ON link~partner_guid = but~partner_guid
      INTO CORRESPONDING FIELDS OF TABLE @gt_but000
      WHERE link~vendor IN @s_forn.

  ENDFORM.
*&---------------------------------------------------------------------*
*& Form WRITE_HEADER
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
  FORM write_header USING i_s_but000 TYPE ty_but000.

    FORMAT RESET.
    ULINE.
    WRITE:
      'BP :',
      i_s_but000-partner,
      'Fornecedor:',
      i_s_but000-vendor.
    NEW-LINE NO-SCROLLING.

  ENDFORM.
