*--------------------------------------------------------------------*
*               P R O J E T O    A G I R - T A E S A                 *
*--------------------------------------------------------------------*
* Consultoria .....: I N T E C H P R O                               *
* Res. ABAP........: Ricardo Monte                                   *
* Res. Funcional...:                                                 *
* Módulo...........: Suprimentos                                     *
* Programa.........: ZIF_VENDOR_DISPLAY                              *
* Transação........: N/A                                             *
* Tipo de Programa.: FUNÇÃO                                          *
* Request     .....:                                                 *
* Objetivo.........: Atualização de Fornecedores pelo SERTRAS        *
*--------------------------------------------------------------------*
* Change Control:                                                    *
* Version | Date      | Who                 |   What                 *
*    2.00 | 28/10/18  | Ricardo Monte       |   Versão AGIR          *
*    2.01 | 06/05/19  | Marcelo Alvares     |   Ajustes 1000001164   *
*    2.02 | 06/06/19  | Marcelo Alvares     |   Dump INC094486       *
*    2.03 | 06/09/19  | Marcelo Alvares     |   INC0115504           *
**********************************************************************
* Favor após qualquer alteração verificar a execução dos testes do   *
* ABAP UNIT!                                                         *
*                                                                    *
**********************************************************************

FUNCTION ZIF_VENDOR_DISPLAY.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(I_CPF_CNPJ) TYPE  LFA1-STCD1
*"  EXPORTING
*"     VALUE(E_FORNECEDOR) TYPE  ZPF_FORNECEDOR
*"  TABLES
*"      E_BANCO_TAB STRUCTURE  ZPF_BANCO OPTIONAL
*"      E_IR_TAB STRUCTURE  ZPF_IR OPTIONAL
*"      E_MESS_TAB STRUCTURE  ZPF_MENSAGEM OPTIONAL
*"      E_TEL_TAB STRUCTURE  ADR2 OPTIONAL
*"      E_FAX_TAB STRUCTURE  ADR3 OPTIONAL
*"      E_EMAIL_TAB STRUCTURE  ADR6 OPTIONAL
*"      E_CONTATOS_TAB STRUCTURE  KNVK OPTIONAL
*"----------------------------------------------------------------------
**********************************************************************
  DATA:
    ls_bus_ei_extern     TYPE bus_ei_extern,    " Interface externa complexa do parceiro de negócios
    ls_business_partners TYPE bus_ei_main,      " Dados globais parceiro negócios
    ls_bp_current        TYPE bus_ei_main,      " Dados globais parceiro negócios
    ls_error             TYPE mds_ctrls_error,  " Estrutura de mensagem do controlador
    ls_lfa1              TYPE lfa1,             " Mestre de fornecedores (parte geral)
    ls_msg               TYPE zpf_mensagem,     " Estrutura de mensagem
    lv_lifnr             TYPE lifnr,            " Nº conta do fornecedor
    lv_flag_erro         TYPE abap_bool.

  CLEAR:
    e_fornecedor, e_banco_tab, e_ir_tab, e_mess_tab.

*Start  - Marcelo Alvares - MA004818 INC0114565 - 19.08.2019 17:30
  go_bal_log = NEW lcl_log_display( ).
  go_bal_log->add_msg_import_table( im_v_data = i_cpf_cnpj ).
*END    - Marcelo Alvares - MA004818 INC0114565 - 19.08.2019 17:30

*  Verifica se a identificação está vazia
  CHECK i_cpf_cnpj IS NOT INITIAL.

  TRY.

      PERFORM get_lifnr USING i_cpf_cnpj CHANGING lv_lifnr.

      IF lv_lifnr IS INITIAL.
        RAISE EXCEPTION TYPE zcx_abap_error.
      ENDIF.

*    Busca Fornecedor a partir do CPF ou CNPJ
      SELECT SINGLE * FROM lfa1
        WHERE lifnr = @lv_lifnr
        INTO @ls_lfa1.

      MOVE-CORRESPONDING ls_lfa1 TO e_fornecedor.

      PERFORM get_bp_gui USING lv_lifnr
                      CHANGING ls_bus_ei_extern-header-object_instance
                               e_mess_tab[]
                               lv_flag_erro.

      IF lv_flag_erro IS INITIAL.

        APPEND ls_bus_ei_extern TO ls_business_partners-partners.

        CALL METHOD cl_bupa_current_data=>get_all(
          EXPORTING
            is_business_partners = ls_business_partners " Complex External Interface of the Business Partner (Tab.)
          IMPORTING
            es_business_partners = ls_bp_current        " Complex External Interface of the Business Partner (Tab.)
            es_error             = ls_error ).          " Message Structure of the Controller

        READ TABLE ls_bp_current-partners ASSIGNING FIELD-SYMBOL(<fs_current_partners>) INDEX 1.

        READ TABLE <fs_current_partners>-central_data-address-addresses
          ASSIGNING FIELD-SYMBOL(<fs_addresses>)
          WITH KEY data-postal-data-standardaddress = abap_true. "Seleção: endereço é endereço standard

        IF <fs_current_partners> IS ASSIGNED.

          MOVE:
            <fs_addresses>-data-postal-data-street      TO e_fornecedor-street,         " Rua
            <fs_addresses>-data-postal-data-str_suppl3  TO e_fornecedor-str_suppl3,     " Rua 4
            <fs_addresses>-data-postal-data-house_no    TO e_fornecedor-house_num1,     " Nº
            <fs_addresses>-data-postal-data-house_no2   TO e_fornecedor-house_num2,     " Complemento do nº
            <fs_addresses>-data-postal-data-city        TO e_fornecedor-city1,          " Local
            <fs_addresses>-data-postal-data-district    TO e_fornecedor-city2,          " Bairro
            <fs_addresses>-data-postal-data-postl_cod1  TO e_fornecedor-post_code1,     " Código postal da localidade
            <fs_addresses>-data-postal-data-countryiso  TO e_fornecedor-country,        " Pais
            <fs_addresses>-data-postal-data-region      TO e_fornecedor-region,         " Região (estado federal)
            ls_lfa1-telf1                               TO e_fornecedor-tel_number,     " Primeiro nº de telefone: código + nº
            ls_lfa1-telf2                               TO e_fornecedor-mob_number,     " Primeiro nº de telefone celular: prefixo + conexão
            ls_lfa1-telfx                               TO e_fornecedor-fax_number.     " Primeiro nº fax: código telefónico + nº

          "Email
          READ TABLE <fs_current_partners>-central_data-communication-smtp-smtp
                     INDEX 1
                     ASSIGNING FIELD-SYMBOL(<smtp>).
          IF sy-subrc IS INITIAL.
            e_fornecedor-smtp_addr = <smtp>-contact-data-e_mail.
          ENDIF.
        ENDIF.

*END    - Marcelo Alvares - MA004818 SHD 1000001164-INC094486-INC094491 - 08.05.2019 15:22

*    ENDIF.
*  ELSE.
*    ls_msg-type  = 'E'.
*    ls_msg-texto = 'Fornecedor não encontrado'(001).
*    APPEND ls_msg TO e_mess_tab.
*    RETURN.
*  ENDIF.

*  Seleciona informações bancarias do Fornecedor
*  O CVI não transporta todas as informações do BP, somente as informações dentro da validade
        SELECT
          FROM        lfbk "Mestre de fornecedores (coordenadas do banco)
          INNER JOIN  bnka "Mestre de bancos
            ON  bnka~banks = lfbk~banks
            AND bnka~bankl = lfbk~bankl
          FIELDS  lfbk~banks, lfbk~bankl, lfbk~bankn,
                  lfbk~koinh, lfbk~bvtyp, lfbk~bkont,
                  bnka~banka, bnka~provz, bnka~stras,
                  bnka~ort01, bnka~brnch
          WHERE lfbk~lifnr = @ls_lfa1-lifnr
          INTO CORRESPONDING FIELDS OF TABLE @e_banco_tab.

        SELECT * FROM lfbw
          WHERE lifnr = @ls_lfa1-lifnr
          INTO CORRESPONDING FIELDS OF TABLE @e_ir_tab.

**********************************************************************
* Nºs telefônicos (administração de endereços central)
        SELECT * FROM adr2
          WHERE addrnumber = @ls_lfa1-adrnr
          INTO TABLE @e_tel_tab.

**********************************************************************
* Nºs fax (administração de endereços central)
        SELECT * FROM adr3
          WHERE addrnumber = @ls_lfa1-adrnr
          INTO TABLE @e_fax_tab.

**********************************************************************
* Endereços de e-mail (administração de endereços central)
        SELECT * FROM adr6
          WHERE addrnumber = @ls_lfa1-adrnr
          INTO TABLE @e_email_tab.

*Start  - Marcelo Alvares - MA004818 SHD 1000001164-INC094486-INC094491 - 08.05.2019 18:09
* Retorna o primeiro email relacionado ao endereço principal do Fornecedor.
* Não existe definição para qual email é prioritario do endereço ou comunição idependente de endereço
        IF e_fornecedor-smtp_addr IS INITIAL AND e_email_tab IS NOT INITIAL.
          READ TABLE e_email_tab ASSIGNING FIELD-SYMBOL(<fs_email_tab>) INDEX 1.
          MOVE <fs_email_tab>-smtp_addr TO e_fornecedor-smtp_addr.
        ENDIF.
*END    - Marcelo Alvares - MA004818 SHD 1000001164-INC094486-INC094491 - 08.05.2019 18:09

**********************************************************************
* Mestre de clientes - pessoas de contato
        SELECT * FROM knvk
          WHERE lifnr = @ls_lfa1-lifnr
          INTO TABLE @e_contatos_tab.
**********************************************************************
*    Busca Chave de condições de pagamento
        SELECT SINGLE zterm FROM lfb1
          WHERE lifnr = @lv_lifnr
          INTO @e_fornecedor-zterm.

      ENDIF.

*Start  - Marcelo Alvares - MA004818 INC0114565 - 04.09.2019 17:30
    CATCH zcx_abap_error.
      ls_msg-type  = 'E'.
      ls_msg-texto = 'Fornecedor não encontrado'(001).
      APPEND ls_msg TO e_mess_tab.
    CATCH cx_root.
  ENDTRY.

  go_bal_log->add_msg_return( im_t_mess_tab    = e_mess_tab[]   ).
  go_bal_log->add_msg_import_table( im_t_table = e_ir_tab[]     ).
  go_bal_log->add_msg_import_table( im_t_table = e_tel_tab[]    ).
  go_bal_log->add_msg_import_table( im_t_table = e_tel_tab[]    ).
  go_bal_log->add_msg_import_table( im_t_table = e_fax_tab[]    ).
  go_bal_log->add_msg_import_table( im_t_table = e_email_tab[]  ).
  go_bal_log->add_msg_import_table( im_t_table = e_contatos_tab[] ).
  go_bal_log->save( ).
*END    - Marcelo Alvares - MA004818 INC0114565 - 19.08.2019 17:30

ENDFUNCTION.
