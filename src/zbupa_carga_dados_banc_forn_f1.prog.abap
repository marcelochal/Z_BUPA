*&---------------------------------------------------------------------*
*& Include ZBUPA_CARGA_DADOS_BANC_FORN_F1
*&---------------------------------------------------------------------*

  FORM ler_excel.

    DATA:
      ls_bankdetail_ins LIKE bapibus1006_bankdetail,
      lt_return         TYPE bapiret2_t,
      lv_msg            TYPE c LENGTH 300,
      lo_msg_progress   TYPE REF TO zcl_progress_indicator.

    CHECK gt_table_excel IS NOT INITIAL.

    CREATE OBJECT lo_msg_progress
      EXPORTING
        im_v_total = lines( gt_table_excel ).

    IF p_test IS NOT INITIAL.
      WRITE 'EXECUÇÃO DE TESTE!'.
    ELSE.
      WRITE 'EXECUÇÂO EFETIVA!'.
    ENDIF.


* Leitura da planilha EXCEL
    LOOP AT gt_table_excel ASSIGNING FIELD-SYMBOL(<fs_table_excel>) .
      UNPACK <fs_table_excel>-lifnr TO <fs_table_excel>-lifnr.
    ENDLOOP.

    SELECT DISTINCT
      but~partner,
      but~partner_guid,
      link~vendor,
      but~name1_text
       FROM cvi_vend_link AS link
          INNER JOIN but000 AS but ON link~partner_guid = but~partner_guid
          INTO CORRESPONDING FIELDS OF TABLE @gt_but000
          FOR ALL ENTRIES IN   @gt_table_excel
          WHERE link~vendor EQ @gt_table_excel-lifnr.

    IF syst-subrc IS NOT INITIAL.
      MESSAGE 'Nenhum fornecedor encontrado' TYPE 'S' DISPLAY LIKE 'E'.
      STOP.
    ENDIF.

    LOOP AT gt_but000 ASSIGNING FIELD-SYMBOL(<fs_but000>) USING KEY but000_sec_key.

      CALL METHOD lo_msg_progress->show
        EXPORTING
          im_v_text = |Processando Forn. { <fs_but000>-vendor ALPHA = OUT }|.

      PERFORM write_header USING <fs_but000>.

      LOOP AT gt_table_excel ASSIGNING <fs_table_excel> USING KEY key_lifnr WHERE lifnr = <fs_but000>-vendor .

        MOVE-CORRESPONDING <fs_table_excel> TO ls_bankdetail_ins.
        ls_bankdetail_ins-accountholder = <fs_but000>-name1_text.

        CALL FUNCTION 'BAPI_BUPA_BANKDETAIL_ADD'
          EXPORTING
            businesspartner = <fs_but000>-partner
            bankdetailid    = <fs_table_excel>-bankdetailid
            bankdetaildata  = ls_bankdetail_ins
          TABLES
            return          = lt_return.

        READ TABLE lt_return ASSIGNING FIELD-SYMBOL(<return>) WITH KEY type = 'E'.

        IF sy-subrc EQ 0.

          PERFORM return_msg USING lt_return <fs_but000>-vendor.
        ELSE.
          PERFORM call_bapi_commit.

          FORMAT COLOR COL_POSITIVE.
          WRITE:
            icon_okay AS ICON,
            'Dados alterados com sucesso!',
            <fs_table_excel>-bankdetailid,  ls_bankdetail_ins-bank_key, ls_bankdetail_ins-bank_acct,
            'Fornecedor:',
            <fs_but000>-vendor.
          NEW-LINE NO-SCROLLING.

          PERFORM return_msg USING lt_return <fs_but000>-vendor.

        ENDIF.
      ENDLOOP.
    ENDLOOP.


  ENDFORM.

  FORM change_bkvid.

    DATA:
      ls_bankdetail  LIKE bapibus1006_bankdetail,
      lt_return      TYPE TABLE OF bapiret2,
      lt_bankdetails TYPE STANDARD TABLE OF bapibus1006_bankdetails.


*    PERFORM  select_bp.

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

*&---------------------------------------------------------------------*
*& Form return_msg
*&---------------------------------------------------------------------*

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
*  FORM select_bp .
*
*    SELECT DISTINCT
*      but~partner,
*      but~partner_guid,
*      link~vendor
*       FROM cvi_vend_link AS link
*      INNER JOIN but000 AS but ON link~partner_guid = but~partner_guid
*      INTO CORRESPONDING FIELDS OF TABLE @gt_but000
*      WHERE link~vendor IN @s_forn
*
*      ORDER BY but~partner,
*               but~partner_guid,
*               link~vendor .
*
*  ENDFORM.
*&---------------------------------------------------------------------*
*& Form WRITE_HEADER
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
  FORM write_header USING i_s_but000 TYPE ty_s_but000.

    FORMAT RESET.
    ULINE.
    WRITE:
      'BP :',
      i_s_but000-partner,
      'Fornecedor:',
      i_s_but000-vendor.
    NEW-LINE NO-SCROLLING.

  ENDFORM.
