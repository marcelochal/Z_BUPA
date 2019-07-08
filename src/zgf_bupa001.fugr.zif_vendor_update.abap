*--------------------------------------------------------------------*
*               P R O J E T O    A G I R - T A E S A                 *
*--------------------------------------------------------------------*
* Consultoria .....: I N T E C H P R O                               *
* Res. ABAP........: Ricardo Monte                                   *
* Res. Funcional...:                                                 *
* Módulo...........: Suprimentos                                     *
* Programa.........: ZIF_VENDOR_UPDATE                               *
* Transação........:                                                 *
* Tipo de Programa.: FUNÇÃO                                          *
* Request     .....:                                                 *
* Objetivo.........: Atualização de Fornecedores pelo SERTRAS        *
*--------------------------------------------------------------------*
* Change Control:                                                    *
* Version | Date      | Who                 |   What                 *
*    2.00 | 28/10/18  | Ricardo Monte       |   Versão AGIR          *
*    2.01 | 06/05/19  | Marcelo Alvares     |   Ajustes 1000001164   *
**********************************************************************
FUNCTION ZIF_VENDOR_UPDATE.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     VALUE(I_FORNECEDOR) TYPE  ZPF_FORNECEDOR
*"     VALUE(I_FORNECEDORX) TYPE  ZPF_FORNECEDORX
*"     VALUE(I_LIFNR) TYPE  LIFNR OPTIONAL
*"     VALUE(I_CPF_CNPJ) TYPE  LFA1-STCD1 OPTIONAL
*"  EXPORTING
*"     VALUE(E_LIFNR) TYPE  LIFNR
*"     VALUE(E_RETURN_MAP) TYPE  MDG_BS_BP_MSGMAP_T
*"     VALUE(E_RETURN) TYPE  BAPIRETM
*"  TABLES
*"      I_BANCO_TAB STRUCTURE  ZPF_BANCO OPTIONAL
*"      I_IR_TAB STRUCTURE  ZPF_IR OPTIONAL
*"      E_MESS_TAB STRUCTURE  ZPF_MENSAGEM OPTIONAL
*"      E_TEL_TAB STRUCTURE  ZPF_TELEFONE OPTIONAL
*"      E_CEL_TAB STRUCTURE  ZPF_CELULAR OPTIONAL
*"      E_FAX_TAB STRUCTURE  ZPF_FAX OPTIONAL
*"      E_EMAIL_TAB STRUCTURE  ZPF_EMAIL OPTIONAL
*"      E_CONTATOS_TAB STRUCTURE  ZPF_CONTATO OPTIONAL
*"----------------------------------------------------------------------
**********************************************************************
  CLEAR: e_lifnr, e_return_map, e_return, e_mess_tab.

  DATA:
    ls_bus_ei_extern     TYPE bus_ei_extern,
    ls_business_partners TYPE bus_ei_main,
    ls_bp_current        TYPE bus_ei_main,
    ls_error             TYPE mds_ctrls_error,
    ls_msg               TYPE zpf_mensagem,
    ls_forn              TYPE ty_s_forn,
    l_bukrs_model        TYPE bukrs,
    l_bukrs_model_oth    TYPE bukrs,
    l_ekorg_model_oth    TYPE ekorg.


  "Encontra o nr do fornecedor (e_lifnr)
  IF i_lifnr <> '' AND i_lifnr <> '0000000000'.
    e_lifnr = i_lifnr.
  ELSEIF i_cpf_cnpj IS NOT INITIAL.
    PERFORM get_lifnr USING i_cpf_cnpj CHANGING e_lifnr.

  ELSEIF i_fornecedor-lifnr <> '' AND i_fornecedor-lifnr <> '0000000000'.
    e_lifnr = i_fornecedor-lifnr.
  ELSEIF i_fornecedor-stcd1 IS NOT INITIAL.
    PERFORM get_lifnr USING i_fornecedor-stcd1 CHANGING e_lifnr.

  ENDIF.

  "********* Preenche informaçãoes gerais do BP e do Vendor *****************
  PERFORM get_bp_gui USING e_lifnr
                  CHANGING ls_bus_ei_extern-header-object_instance
                           e_mess_tab[]
                           gv_flag_erro.

  CHECK gv_flag_erro IS INITIAL. "ELSE.   "BP existe. Atualização é possível

*    ls_bus_ei_extern-header-object_instance-bpartner = gv_partner.
  APPEND ls_bus_ei_extern TO ls_business_partners-partners.

  CALL METHOD cl_bupa_current_data=>get_all(
    EXPORTING
      is_business_partners = ls_business_partners " Complex External Interface of the Business Partner (Tab.)
    IMPORTING
      es_business_partners = ls_bp_current        " Complex External Interface of the Business Partner (Tab.)
      es_error             = ls_error ).          " Message Structure of the Controller

  READ TABLE ls_bp_current-partners INDEX 1 ASSIGNING FIELD-SYMBOL(<fs_current_partners>).


  "Busca empresas, org de compras e ctas de reconciliação do fornecedor e_lifnr
  PERFORM busca_info_fornecedor USING e_lifnr
                             CHANGING gt_forn
                                      e_mess_tab[].

  CHECK e_mess_tab IS INITIAL.

  "Seta a 1a primeira empresa e org de compra como modelo inicial
  READ TABLE gt_forn INTO ls_forn INDEX 1.
  IF sy-subrc = 0.
    l_bukrs_model_oth = ls_forn-bukrs.
    l_ekorg_model_oth = ls_forn-ekorg.
  ENDIF.

  "Encontra as empresas cadastradas como estensíveis
  PERFORM get_bukrs
      CHANGING gt_bukrs
               e_mess_tab[].

  "Filtra apenas as empresas que o fornecedor está estendido
  PERFORM filtra_bukrs
      USING    e_lifnr
      CHANGING gt_bukrs.

  "Elimina certo tipo de imposto
  DELETE i_ir_tab WHERE witht = 'OP'
                     OR witht = space.

*   Move HEADER Partner number and GUID.
  MOVE-CORRESPONDING:
      <fs_current_partners>-header                 TO gs_cvis_ei_extern-partner-header.

  gs_cvis_ei_extern-partner-header-object_task     =  gc_object_task_update.


  PERFORM fill_inscricoes
      USING    i_fornecedorx
               i_fornecedor
               e_lifnr
      CHANGING gs_cvis_ei_extern-partner-central_data-taxnumber-taxnumbers.

  PERFORM fill_vendor     USING       i_fornecedorx
                                      i_fornecedor
                                      e_lifnr
                          CHANGING    gs_cvis_ei_extern-partner-central_data-common
                                      gs_cvis_ei_extern-vendor-header
                                      gs_cvis_ei_extern-partner-central_data-role.

  PERFORM f_update_fill_address USING i_fornecedorx
                                      i_fornecedor
                                      gs_cvis_ei_extern-partner-header-object_instance-bpartner
                                      <fs_current_partners>-central_data-address-addresses
                          CHANGING    gs_cvis_ei_extern-partner-central_data-address.

  PERFORM f_update_fill_bank USING    i_banco_tab[]
                                      gs_cvis_ei_extern-partner-header-object_instance-bpartner
                                      <fs_current_partners>-central_data-bankdetail-bankdetails
                          CHANGING    gs_cvis_ei_extern-partner-central_data-bankdetail-bankdetails.

  PERFORM fill_comm_update USING      i_fornecedorx
                                      i_fornecedor
                                      <fs_current_partners>-central_data-communication
                        CHANGING      gs_cvis_ei_extern-partner-central_data-communication.

*  PERFORM fill_contact USING e_contatos_tab[]
*                             wa_bp_current-vendor-central_data-contact
*                    CHANGING gs_cvis_ei_extern-vendor-central_data-contact.


  "Para cada empresa cadastrada para extensão de fornecedor, faça
  LOOP AT gt_bukrs INTO gs_bukrs.

    "Encontra a empresa que servirá de modelo para a atualização (l_bukrs_model)
    READ TABLE gt_forn TRANSPORTING NO FIELDS WITH KEY bukrs = gs_bukrs-bukrs.
    IF sy-subrc = 0.
      l_bukrs_model = gs_bukrs-bukrs.
    ELSEIF l_bukrs_model_oth IS NOT INITIAL.
      l_bukrs_model = l_bukrs_model_oth.
    ELSE.
      CONTINUE.
    ENDIF.

    PERFORM fill_company USING i_fornecedor
                               i_fornecedorx
                               i_ir_tab[]
                               gs_bukrs-bukrs
                               gs_cvis_ei_extern-vendor-company_data.

  ENDLOOP.

  "Valida os dados
  CLEAR: e_return_map, e_return.

  PERFORM f_call_bp_maintain
      USING
       gs_cvis_ei_extern
       abap_off          " Validate
      CHANGING
       e_mess_tab[]
       e_return_map
       e_return
       gv_flag_erro.

  IF gv_flag_erro IS INITIAL.

    "Efetiva a atualização
    APPEND gs_cvis_ei_extern TO gt_cvis_ei_extern_t.

    PERFORM f_call_bp_maintain
        USING
          gs_cvis_ei_extern
          abap_on         " Update
        CHANGING
          e_mess_tab[]
          e_return_map
          e_return
          gv_flag_erro.

    IF gv_flag_erro IS INITIAL.

      CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
        EXPORTING
          wait = abap_true.        " Use of Command `COMMIT AND WAIT`

      ls_msg-type  = 'S'.
      ls_msg-texto = 'Fornecedor atualizado'.
      APPEND ls_msg TO e_mess_tab.

    ENDIF.

  ENDIF.

ENDFUNCTION.
