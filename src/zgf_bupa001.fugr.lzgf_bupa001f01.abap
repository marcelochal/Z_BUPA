*&---------------------------------------------------------------------*
*& Form GET_BP_GUI
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> E_LIFNR
*&      <-- LV_PARTNER_GUID
*&---------------------------------------------------------------------*
FORM get_bp_gui  USING p_v_lifnr     TYPE lifnr
              CHANGING ch_s_instance TYPE bus_ei_instance
                       ch_t_mess_tab TYPE ty_t_mensagem
                       ch_v_error    TYPE abap_bool.

  DATA:
    lv_lifnr        TYPE lifnr,
    lo_bp_vendor_xd TYPE REF TO cvi_ka_bp_vendor, " Assignment of BP to Vendor
    lv_partner_guid TYPE bu_partner_guid,
    lt_return       TYPE bapiret2_t,
    ls_mess_tab     TYPE zpf_mensagem.

  lo_bp_vendor_xd = cvi_ka_bp_vendor=>get_instance( ).

  lv_lifnr = |{ p_v_lifnr ALPHA = IN }|.

  CALL FUNCTION 'LFA1_SINGLE_READ'
    EXPORTING
      lfa1_lifnr    = lv_lifnr         " Vendor Number
    EXCEPTIONS
      not_found     = 1                " Vendor does not exist
      lifnr_blocked = 2
      OTHERS        = 3.
  IF sy-subrc IS NOT INITIAL.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
        WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 INTO DATA(lv_msg_dummy).
    ls_mess_tab-type  = syst-msgty.
    ls_mess_tab-texto = lv_msg_dummy.
    APPEND ls_mess_tab TO ch_t_mess_tab.
    ch_v_error = abap_true.
    RETURN.
  ENDIF.

  " Look for the BP from the Vendor number
  lo_bp_vendor_xd->get_assigned_bp_for_vendor(
    EXPORTING
      i_vendor         = lv_lifnr           " Account Number of Vendor
*        i_persisted_only =                 " Return Only Assignment That Has Already Been Made Persistent
    RECEIVING
      r_partner        = lv_partner_guid  )." Business Partner GUID

  IF lv_partner_guid IS INITIAL.
    ch_v_error = abap_true.
    CONCATENATE 'Parceiro de negócios não encontrado para o fornecedor:' lv_lifnr INTO
        ls_mess_tab-texto  SEPARATED BY space.
    ls_mess_tab-type = 'E'.
    APPEND ls_mess_tab TO ch_t_mess_tab.
    RETURN.
  ENDIF.

  CALL FUNCTION 'BUPA_NUMBERS_GET'
    EXPORTING
*     iv_partner      = ch_v_bp_number         " Business Partner Number
      iv_partner_guid = lv_partner_guid        " Business Partner GUID
    IMPORTING
      ev_partner      = ch_s_instance-bpartner " Business Partner Number
      ev_partner_guid = lv_partner_guid        " Business Partner GUID
    TABLES
      et_return       = lt_return.             " Messages

  LOOP AT lt_return ASSIGNING FIELD-SYMBOL(<fs_s_return_map>) WHERE type CA 'AE'. "Contains Any A or E
    ch_v_error = abap_true.
    ls_mess_tab-type  = <fs_s_return_map>-type.
    ls_mess_tab-texto = <fs_s_return_map>-message.
    APPEND ls_mess_tab TO ch_t_mess_tab.
  ENDLOOP.

  IF ch_v_error IS INITIAL.
    ch_s_instance-bpartnerguid = lv_partner_guid.
  ELSE.
    CLEAR: ch_s_instance.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form FILL_VENDOR
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> I_FORNECEDORX
*&      --> I_FORNECEDOR
*&      --> E_LIFNR
*&      <-- E_MESS_TAB[]
*&      <-- <FS_CVIS_EI_EXTERN>_PARTNER_CE
*&---------------------------------------------------------------------*
FORM fill_vendor USING p_fornecedorx TYPE zpf_fornecedorx
                       p_fornecedor  TYPE zpf_fornecedor
                       p_lifnr       TYPE lifnr
              CHANGING p_common      TYPE bus_ei_bupa_central
                       p_header      TYPE vmds_ei_header
                       p_role        TYPE bus_ei_roles.

  DATA ls_roles TYPE bus_ei_bupa_roles.

  "Fornecedor
  ls_roles-task               = gc_object_task_update. "'U'.
  ls_roles-data_key           = 'FLVN01'.
  ls_roles-data-rolecategory  = 'FLVN01'.
  APPEND ls_roles TO p_role-roles.

  "Fornecedor contábil
  ls_roles-task               = gc_object_task_update. "'U'.
  ls_roles-data_key           = 'FLVN00'.
  ls_roles-data-rolecategory  = 'FLVN00'.
  APPEND ls_roles TO p_role-roles.

  "Update do Fornecedor
  p_header-object_instance-lifnr = p_lifnr.
  p_header-object_task           = gc_object_task_update. "'U'.

  "Bloqueio de fornecedor
  gs_cvis_ei_extern-vendor-central_data-central-data-sperr  = abap_on.
  gs_cvis_ei_extern-vendor-central_data-central-data-sperm  = abap_on.
  gs_cvis_ei_extern-vendor-central_data-central-datax-sperr = abap_on.
  gs_cvis_ei_extern-vendor-central_data-central-datax-sperm = abap_on.


  IF p_fornecedorx-sortl IS NOT INITIAL.
    p_common-datax-bp_centraldata-searchterm1 = p_fornecedorx-sortl.
    p_common-data-bp_centraldata-searchterm1  = p_fornecedor-sortl.
  ENDIF.

  IF p_fornecedorx-name1 IS NOT INITIAL.
    p_common-datax-bp_organization-name1 = abap_on.
    p_common-data-bp_organization-name1  = p_fornecedor-name1.
  ENDIF.

  p_common-data-bp_control-grouping = p_fornecedor-ktokk.
  p_common-data-bp_control-category = '2'.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form FILL_ADDRESS
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> I_FORNECEDORX
*&      --> I_FORNECEDOR
*&      --> E_LIFNR
*&      <-- E_MESS_TAB[]
*&      <-- <FS_CVIS_EI_EXTERN>_PARTNER_CE
*&---------------------------------------------------------------------*
FORM f_update_fill_address USING p_s_fornecedorx  TYPE zpf_fornecedorx
                                 p_s_fornecedor   TYPE zpf_fornecedor
                                 p_v_partner      TYPE bu_partner
                                 p_t_address_curr TYPE bus_ei_bupa_address_t
                        CHANGING ch_s_addresses   TYPE bus_ei_address.

  DATA:
    lv_update               TYPE abap_bool,
    lv_partner_guid_address TYPE cvi_vend_link-partner_guid,
    ls_addresses            TYPE bus_ei_bupa_address,
    ls_telephone            TYPE bus_ei_bupa_telephone,
    ls_fax                  TYPE bus_ei_bupa_fax,
    ls_smtp                 TYPE bus_ei_bupa_smtp.

  READ TABLE p_t_address_curr ASSIGNING FIELD-SYMBOL(<fs_current_address>) INDEX 1.

  CALL FUNCTION 'BUPA_ADDRESSES_GET'
    EXPORTING
      iv_partner           = p_v_partner
    IMPORTING
      ev_standard_addrguid = lv_partner_guid_address.

  ls_addresses-task          = gc_object_task_update. "'U'.
  ls_addresses-data_key-guid = lv_partner_guid_address.

  "Nome do endereço
  IF p_s_fornecedorx-name1 IS NOT INITIAL.
    ls_addresses-data-postal-datax-c_o_name = abap_on.
    ls_addresses-data-postal-data-c_o_name  = p_s_fornecedor-name1.
    lv_update = abap_on.
  ENDIF.

  "Rua
  IF p_s_fornecedorx-street IS NOT INITIAL.
    ls_addresses-data-postal-datax-street   = abap_on.
    ls_addresses-data-postal-data-street    = p_s_fornecedor-street.
    lv_update = abap_on.
  ENDIF.

  "Rua - Complemento
  IF p_s_fornecedorx-str_suppl3 IS NOT INITIAL.
    ls_addresses-data-postal-datax-str_suppl3 = abap_on.
    ls_addresses-data-postal-data-str_suppl3  = p_s_fornecedor-str_suppl3.
    lv_update = abap_on.
  ENDIF.

  "Número no endereço
  IF p_s_fornecedorx-house_num1 IS NOT INITIAL.
    ls_addresses-data-postal-datax-house_no = abap_on.
    ls_addresses-data-postal-data-house_no  = p_s_fornecedor-house_num1.
    lv_update = abap_on.
  ENDIF.

  "Complemento do número no endereço
  IF p_s_fornecedorx-house_num2 IS NOT INITIAL.
    ls_addresses-data-postal-datax-house_no2 = abap_on.
    ls_addresses-data-postal-data-house_no2  = p_s_fornecedor-house_num2.
    lv_update = abap_on.
  ENDIF.

  "Bairro
  IF p_s_fornecedorx-city2 IS NOT INITIAL.
    ls_addresses-data-postal-datax-district = abap_on.
    ls_addresses-data-postal-data-district  = p_s_fornecedor-city2.
    lv_update = abap_on.
  ENDIF.

  "Código postal
  IF p_s_fornecedorx-post_code1 IS NOT INITIAL.
    ls_addresses-data-postal-datax-postl_cod1 = abap_on.
    ls_addresses-data-postal-data-postl_cod1  = p_s_fornecedor-post_code1.
    lv_update = abap_on.
  ENDIF.

  "Cidade
  IF p_s_fornecedorx-city1 IS NOT INITIAL.
    ls_addresses-data-postal-datax-city = abap_on.
    ls_addresses-data-postal-data-city  = p_s_fornecedor-city1.
    lv_update = abap_on.
  ENDIF.

  "País
  IF p_s_fornecedorx-country IS NOT INITIAL.
    ls_addresses-data-postal-datax-country = abap_on.
    ls_addresses-data-postal-data-country  = p_s_fornecedor-country.
    lv_update = abap_on.
  ENDIF.

  "Estado
  IF p_s_fornecedorx-region IS NOT INITIAL.
    ls_addresses-data-postal-datax-region = abap_on.
    ls_addresses-data-postal-data-region  = p_s_fornecedor-region.
    lv_update = abap_on.
  ENDIF.

*Start    - Marcelo Alvares - MA004818 SHD 1000001164-INC094486-INC094491 - 07.05.2019 15:51
  "Rua 4
  IF p_s_fornecedorx-str_suppl3 IS NOT INITIAL.
    ls_addresses-data-postal-datax-str_suppl3 = abap_true.
    ls_addresses-data-postal-data-str_suppl3  = p_s_fornecedor-str_suppl3.
    lv_update = abap_on.
  ENDIF.

  CHECK <fs_current_address> IS ASSIGNED. " avoid dump

  " Fixo
  IF p_s_fornecedorx-tel_number IS NOT INITIAL.
    LOOP AT <fs_current_address>-data-communication-phone-phone
        ASSIGNING FIELD-SYMBOL(<phone>)
        WHERE contact-data-r_3_user = gc_default_fixed_tel.

      <phone>-contact-task             = gc_object_task_update. "'U'.
      <phone>-contact-datax-country    = abap_on.
      <phone>-contact-datax-telephone  = abap_on.
      <phone>-contact-datax-extension  = abap_on.
      <phone>-contact-datax-updateflag = abap_on.
      <phone>-contact-data-country     = p_s_fornecedor-country.
      <phone>-contact-data-telephone   = p_s_fornecedor-tel_number.
*      <phone>-contact-data-tel_no      = p_s_fornecedor-tel_number.
      APPEND <phone> TO ls_addresses-data-communication-phone-phone.
    ENDLOOP.

    " Em caso de erro, não existe telefone fixo, então cria um novo
    IF syst-subrc IS NOT INITIAL.
      ls_telephone-contact-task            = gc_object_task_insert. "'I'.
      ls_telephone-contact-data-country    = p_s_fornecedor-country.
      ls_telephone-contact-data-telephone  = p_s_fornecedor-tel_number.
      ls_telephone-contact-data-r_3_user   = gc_default_fixed_tel.
      APPEND ls_telephone TO ls_addresses-data-communication-phone-phone.
    ENDIF.
  ENDIF.

  CLEAR ls_telephone.

  "Celular
  IF p_s_fornecedorx-mob_number IS NOT INITIAL.
    LOOP AT <fs_current_address>-data-communication-phone-phone
        ASSIGNING FIELD-SYMBOL(<mob_phone>)
        WHERE contact-data-r_3_user        = gc_default_mobile_tel.
      <mob_phone>-contact-task             = gc_object_task_update. "'U'.
      <mob_phone>-contact-datax-country    = abap_on.
      <mob_phone>-contact-datax-telephone  = abap_on.
      <mob_phone>-contact-datax-extension  = abap_on.
      <mob_phone>-contact-data-country     = p_s_fornecedor-country.
      <mob_phone>-contact-data-telephone   = p_s_fornecedor-mob_number.
      APPEND <mob_phone> TO ls_addresses-data-communication-phone-phone.
    ENDLOOP.

    " Em caso de erro, não existe telefone fixo, então cria um novo
    IF syst-subrc IS NOT INITIAL.
      ls_telephone-contact-task            = gc_object_task_insert.
      ls_telephone-contact-data-country    = p_s_fornecedor-country.
      ls_telephone-contact-data-telephone  = p_s_fornecedor-mob_number.
      ls_telephone-contact-data-r_3_user   = gc_default_mobile_tel.
      APPEND ls_telephone TO ls_addresses-data-communication-phone-phone.
    ENDIF.
  ENDIF.

  "Fax
  IF p_s_fornecedorx-fax_number IS NOT INITIAL.
    LOOP AT <fs_current_address>-data-communication-fax-fax ASSIGNING FIELD-SYMBOL(<fax>).
      <fax>-contact-task             = gc_object_task_update. "'U'.
      <fax>-contact-datax-country    = abap_on.
      <fax>-contact-datax-fax        = abap_on.
      <fax>-contact-data-country     = p_s_fornecedor-country.
      <fax>-contact-data-fax         = p_s_fornecedor-fax_number.
      APPEND <fax> TO ls_addresses-data-communication-fax-fax.
    ENDLOOP.

    " Em caso de erro, não existe telefone fixo, então cria um novo
    IF syst-subrc IS NOT INITIAL.
      ls_fax-contact-task         = gc_object_task_insert. "'I'.
      ls_fax-contact-data-country = p_s_fornecedor-country.
      ls_fax-contact-data-fax     = p_s_fornecedor-fax_number.
      APPEND ls_fax TO ls_addresses-data-communication-fax-fax.
    ENDIF.
  ENDIF.

  "Email
  IF p_s_fornecedorx-smtp_addr IS NOT INITIAL.
    LOOP AT <fs_current_address>-data-communication-smtp-smtp ASSIGNING FIELD-SYMBOL(<fs_s_smtp>).
      <fs_s_smtp>-contact-task         = gc_object_task_update. "'U'.
      <fs_s_smtp>-contact-datax-e_mail = abap_on.
      <fs_s_smtp>-contact-data-e_mail  = p_s_fornecedor-smtp_addr.
      lv_update = abap_on.
      APPEND <fs_s_smtp> TO ls_addresses-data-communication-smtp-smtp.
    ENDLOOP.

    IF syst-subrc IS NOT INITIAL.
      ls_smtp-contact-task         = gc_object_task_insert. "'I'.
      ls_smtp-contact-data-e_mail  = p_s_fornecedor-smtp_addr.
      APPEND ls_smtp TO ls_addresses-data-communication-smtp-smtp.
    ENDIF.
  ENDIF.

*END    - Marcelo Alvares - MA004818 SHD 1000001164-INC094486-INC094491 - 07.05.2019 15:51

  " Modifica o domicilio fiscal caso existir mudança no CEP.
  IF ls_addresses-data-postal-datax-postl_cod1 EQ abap_true.
* Domicilio fiscal
    SELECT SINGLE taxjurcode
      INTO ls_addresses-data-postal-data-taxjurcode
      FROM j_1btreg_city
     WHERE country    =  p_s_fornecedorx-country
       AND region     =  p_s_fornecedor-region
       AND pstcd_from <= p_s_fornecedor-post_code1
       AND pstcd_to   >= p_s_fornecedor-post_code1.

    ls_addresses-data-postal-datax-taxjurcode     = abap_on.
    ls_addresses-data-postal-data-langu           = gc_languiso_pt.
    ls_addresses-data-postal-data-standardaddress = abap_on.

  ENDIF.

  IF p_s_fornecedorx IS NOT INITIAL. " Existe modificações previstas

    "Validade
    ls_addresses-data-postal-datax-validfromdate = abap_on.
    ls_addresses-data-postal-data-validfromdate  = sy-datum.

    APPEND ls_addresses TO ch_s_addresses-addresses.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form F_UPDATE_FILL_BANK
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> I_BANCO_TAB
*&      <-- E_MESS_TAB[]
*&      <-- <FS_CVIS_EI_EXTERN>_PARTNER_CE
*&---------------------------------------------------------------------*
FORM f_update_fill_bank USING p_t_banco_tab           TYPE ty_t_zpf_banco
                              p_v_partner             TYPE bu_partner
                              p_t_current_bankdetails TYPE bus_ei_bupa_bankdetail_t
                     CHANGING ch_t_bankdetails        TYPE bus_ei_bupa_bankdetail_t.

  DATA:
    ls_bankdetails TYPE bus_ei_bupa_bankdetail.

  CHECK p_t_banco_tab IS NOT INITIAL.

  " Adiciona os dados atuais para a tabela a ser atualizada.
  MOVE-CORRESPONDING p_t_current_bankdetails TO ch_t_bankdetails.
  ls_bankdetails-task = gc_object_task_update.
  MODIFY ch_t_bankdetails FROM ls_bankdetails
    TRANSPORTING task
    WHERE task = gc_object_task_insert.

  LOOP AT p_t_banco_tab ASSIGNING FIELD-SYMBOL(<fs_s_banco>).
    CLEAR ls_bankdetails.

    READ TABLE p_t_current_bankdetails
        WITH KEY data-bank_ctry    = <fs_s_banco>-banks " Código do país do banco
                 data-bank_key     = <fs_s_banco>-bankl " Chave do banco
                 data-bank_acct    = <fs_s_banco>-bankn " Nº conta bancária
                 TRANSPORTING NO FIELDS.
*            ASSIGNING FIELD-SYMBOL(<fs_s_bank_details>).

    IF sy-subrc IS INITIAL. "Corrdenadas bancarias ja existem
*      MOVE-CORRESPONDING
*      <fs_s_bank_details> TO ls_bankdetails.
*      ls_bankdetails-task  = gc_object_task_update. "'U'.

    ELSE. "Corrdenadas bancarias não existem INSERT
      ls_bankdetails-task                   = gc_object_task_insert. "'I'.
      ls_bankdetails-data_key               = <fs_s_banco>-bvtyp. " Tipo de banco do parceiro
      ls_bankdetails-data-bank_ctry         = <fs_s_banco>-banks. " Código do país do banco
      ls_bankdetails-data-bank_key          = <fs_s_banco>-bankl. " Chave do banco
      ls_bankdetails-data-bank_acct         = <fs_s_banco>-bankn. " Nº conta bancária
      ls_bankdetails-data-ctrl_key          = <fs_s_banco>-bkont. " Chave de controle de bancos
      ls_bankdetails-data-bankaccountname   = <fs_s_banco>-banka. " Nome da instituição financeira
      ls_bankdetails-data-accountholder     = <fs_s_banco>-koinh. " Nome do titular da conta
      ls_bankdetails-data-bank_ref          = <fs_s_banco>-brnch. " ?

*    Verifica se ID de dados bancários é igual ao codigo do banco.
      IF ls_bankdetails-data_key(3) NE ls_bankdetails-data-bank_key(3).
        ls_bankdetails-data_key = ls_bankdetails-data-bank_key(3).
      ENDIF.

      APPEND ls_bankdetails TO ch_t_bankdetails.

    ENDIF.

  ENDLOOP.

  PERFORM f_check_and_change_bankid
      CHANGING ch_t_bankdetails.

  " Não faz nada ao atualizar.
  DELETE ch_t_bankdetails WHERE task = gc_object_task_update.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form FILL_BANK_DATA
*&---------------------------------------------------------------------*
*& Cadastro de coordenadas bancarias do BP
*&---------------------------------------------------------------------*
*&      --> I_BANCO_TAB
*&      <-- LS_CVIS_EI_EXTERN_PARTNER_CENT
*&---------------------------------------------------------------------*
FORM f_create_fill_bank_data USING p_i_banco_tab    TYPE ty_t_zpf_banco
                          CHANGING ch_s_bankdetails TYPE bus_ei_bankdetail.
  DATA:
    ls_bus_ei_bupa_bankdetail TYPE bus_ei_bupa_bankdetail.

  LOOP AT p_i_banco_tab ASSIGNING FIELD-SYMBOL(<fs_bank_data>).

    CLEAR: ls_bus_ei_bupa_bankdetail.

    ls_bus_ei_bupa_bankdetail-task                   = gc_object_task_insert. " I Insert
*Start    - Marcelo Alvares - MA004818 SHD 1000001164-INC094486-INC094491 - 06.05.2019 15:16
    ls_bus_ei_bupa_bankdetail-data_key               = <fs_bank_data>-bvtyp.    " Tipo de banco do parceiro
    ls_bus_ei_bupa_bankdetail-data-bank_ctry         = <fs_bank_data>-banks.    " Código do país do banco
    ls_bus_ei_bupa_bankdetail-data-bank_key          = <fs_bank_data>-bankl.    " Chave do banco
    ls_bus_ei_bupa_bankdetail-data-bank_acct         = <fs_bank_data>-bankn.    " Nº conta bancária
    ls_bus_ei_bupa_bankdetail-data-ctrl_key          = <fs_bank_data>-bkont.    " Chave de controle de bancos
    ls_bus_ei_bupa_bankdetail-data-accountholder     = <fs_bank_data>-koinh.    " Nome do titular da conta
    ls_bus_ei_bupa_bankdetail-data-bank_ref          = <fs_bank_data>-banka.    " Nome da instituição financeira
    ls_bus_ei_bupa_bankdetail-data-bankaccountname   = <fs_bank_data>-banka.    " Nome da instituição financeira

*    Verifica se ID de dados bancários é igual ao codigo do banco.
    IF ls_bus_ei_bupa_bankdetail-data_key(3) NE ls_bus_ei_bupa_bankdetail-data-bank_key(3).
      ls_bus_ei_bupa_bankdetail-data_key = ls_bus_ei_bupa_bankdetail-data-bank_key(3).
    ENDIF.
*END    - Marcelo Alvares - MA004818 SHD 1000001164-INC094486-INC094491 - 06.05.2019 15:16

    APPEND ls_bus_ei_bupa_bankdetail TO ch_s_bankdetails-bankdetails.

  ENDLOOP.

  PERFORM f_check_and_change_bankid
    CHANGING ch_s_bankdetails-bankdetails.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form FILL_COMM
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> E_TEL_TAB
*&      --> E_CEL_TAB
*&      --> E_FAX_TAB
*&      --> E_EMAIL_TAB
*&      <-- E_MESS_TAB[]
*&      <-- <FS_CVIS_EI_EXTERN>_PARTNER_CE
*&---------------------------------------------------------------------*
FORM fill_comm_create USING p_pos           TYPE int1
                            p_tel_tab       TYPE ty_t_zpf_telefone
                            p_cel_tab       TYPE ty_t_zpf_celular
                            p_fax_tab       TYPE ty_t_zpf_fax
                            p_email_tab     TYPE ty_t_zpf_email
                   CHANGING p_communication TYPE bus_ei_communication.

  DATA:
    wa_phone TYPE bus_ei_bupa_telephone,
    wa_fax   TYPE bus_ei_bupa_fax,
    wa_email TYPE bus_ei_bupa_smtp.

  FIELD-SYMBOLS:
    <tel>   TYPE zpf_telefone,
    <cel>   TYPE zpf_celular,
    <fax>   TYPE zpf_fax,
    <email> TYPE zpf_email.

  wa_phone-contact-task             = gc_object_task_insert. "'I'.

  "Fixo
  READ TABLE p_tel_tab WITH KEY pos = p_pos
                      ASSIGNING <tel>.
  IF sy-subrc IS INITIAL.
    wa_phone-contact-data-telephone  = <tel>-telefone.
    wa_phone-contact-data-extension  = <tel>-extensao.
    wa_phone-contact-data-r_3_user   = ''.

    PERFORM fill_remark USING <tel>-remark
                     CHANGING wa_phone-remark.
    APPEND wa_phone TO p_communication-phone-phone.

  ENDIF.

  "Celular
  READ TABLE p_cel_tab WITH KEY pos = p_pos
                      ASSIGNING <cel>.
  IF sy-subrc IS INITIAL.
    wa_phone-contact-data-telephone  = <cel>-celular.
    wa_phone-contact-data-r_3_user   = '3'.

    PERFORM fill_remark USING <tel>-remark
                     CHANGING wa_phone-remark.
    APPEND wa_phone TO p_communication-phone-phone.

  ENDIF.

  "Fax
  READ TABLE p_fax_tab WITH KEY pos = p_pos
                      ASSIGNING <fax>.
  IF sy-subrc IS INITIAL.
    wa_fax-contact-data-fax        = <fax>-fax.
    wa_fax-contact-data-extension  = <fax>-extensao.

    PERFORM fill_remark USING <fax>-remark
                     CHANGING wa_fax-remark.

    APPEND wa_fax TO p_communication-fax-fax.

  ENDIF.


  "Email
  READ TABLE p_email_tab WITH KEY pos = p_pos
                        ASSIGNING <email>.
  IF sy-subrc IS INITIAL.
    wa_email-contact-data-e_mail = <email>-email.

    PERFORM fill_remark USING <email>-remark
                     CHANGING wa_email-remark.

    APPEND wa_email TO p_communication-smtp-smtp.

  ENDIF.


ENDFORM.
*&---------------------------------------------------------------------*
*& Form FILL_COMM
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> E_TEL_TAB
*&      --> E_CEL_TAB
*&      --> E_FAX_TAB
*&      --> E_EMAIL_TAB
*&      <-- E_MESS_TAB[]
*&      <-- <FS_CVIS_EI_EXTERN>_PARTNER_CE
*&---------------------------------------------------------------------*
*FORM fill_comm USING p_tel_tab       TYPE tty_zpf_telefone
*                     p_cel_tab       TYPE tty_zpf_celular
*                     p_fax_tab       TYPE tty_zpf_fax
*                     p_email_tab     TYPE tty_zpf_email
*            CHANGING p_communication TYPE bus_ei_communication.
*
**  "Telefones
*  PERFORM fill_phone USING p_tel_tab
*                           p_cel_tab
*                  CHANGING p_communication-phone-phone.
*
*  "Fax
*  PERFORM fill_fax USING p_fax_tab
*                CHANGING p_communication-fax-fax.
*
**  "Email
*  PERFORM fill_email USING p_email_tab
*                  CHANGING p_communication-smtp-smtp.
*
*ENDFORM.
*&---------------------------------------------------------------------*
*& Form FILL_CONTACT
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> E_CONTATOS_TAB
*&      <-- E_MESS_TAB[]
*&      <-- <FS_CVIS_EI_EXTERN>_VENDOR_CEN
*&---------------------------------------------------------------------*
*FORM fill_contact USING p_contatos_tab TYPE ty_t_zpf_contato
*                        "p_curr_contact TYPE vmds_ei_vmd_contacts
*               CHANGING p_contacts     TYPE vmds_ei_vmd_contacts.

*  DATA:
*    wa_contact TYPE VMDS_EI_CONTACTS.
*
*  IF p_contatos_tab IS NOT INITIAL.
*    LOOP AT p_contatos_tab ASSIGNING FIELD-SYMBOL(<contatos>).
*      READ TABLE p_curr_contact with key pos = <contatos>-pos
*                  ASSIGNING FIELD-SYMBOL(<curr_contact>).
*      IF sy-subrc is initial.
*        <curr_contact>-task     = c_object_task_update. "'U'.
*        <curr_contact>-data_key = <contatos>-pos.
*
*        <curr_contact>-datax-parh1 = abap_on.
*        <curr_contact>-datax-parh2 = abap_on.
*
*        <curr_contact>-data-parh1  = <contatos>-nome.
*        <curr_contact>-data-parh2  = <contatos>-sobrenome.
*      else.
*        CLEAR wa_contact.
*        wa_contact-task        = c_object_task_insert. "'I'.
*        wa_contact-data_key    = <contatos>-pos.
*
*        wa_contact-datax-parh1 = abap_on.
*        wa_contact-datax-parh2 = abap_on.
*
*        wa_contact-data-parh1  = <contatos>-nome.
*        wa_contact-data-parh2  = <contatos>-sobrenome.
*
*        APPEND wa_contact TO p_curr_contact-contacts.
*
*      ENDIF.
*    ENDLOOP.
*
*    p_contacts = p_curr_contact.
*  ENDIF.

*ENDFORM.
*&---------------------------------------------------------------------*
*& Form FILL_PHONE_create
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> P_TEL_TAB
*&      <-- P_COMMUNICATION_PHONE_PHONE
*&---------------------------------------------------------------------*
FORM fill_phone_create USING p_fornecedor TYPE zpf_fornecedor
                    CHANGING p_t_phone      TYPE bus_ei_bupa_telephone_t.
  DATA:
    ls_phone TYPE bus_ei_bupa_telephone.

  ls_phone-contact-task             =  gc_object_task_insert. "'I'.

  "Fixo
  ls_phone-contact-data-country    = p_fornecedor-country.
  ls_phone-contact-data-telephone  = p_fornecedor-tel_number.
  ls_phone-contact-data-r_3_user   = gc_default_fixed_tel. "1.
  APPEND ls_phone TO p_t_phone.

  "Celular
  ls_phone-contact-data-country    = p_fornecedor-country.
  ls_phone-contact-data-telephone  = p_fornecedor-mob_number.
  ls_phone-contact-data-r_3_user   = gc_default_mobile_tel."'3'.
  APPEND ls_phone TO p_t_phone.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form FILL_PHONE
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> P_TEL_TAB
*&      <-- P_COMMUNICATION_PHONE_PHONE
*&---------------------------------------------------------------------*
*FORM fill_phone USING p_tel_tab TYPE tty_zpf_telefone
*                      p_cel_tab TYPE tty_zpf_celular
*             CHANGING p_phone   TYPE bus_ei_bupa_telephone_t.
*  DATA:
*    wa_phone TYPE bus_ei_bupa_telephone.
*
*  LOOP AT p_tel_tab ASSIGNING FIELD-SYMBOL(<tel>).
*    CLEAR wa_phone.
*    wa_phone-contact-task             = c_object_task_update. "'U'.
*    wa_phone-contact-datax-country    = abap_on.
*    wa_phone-contact-datax-telephone  = abap_on.
*    wa_phone-contact-datax-extension  = abap_on.
*    wa_phone-contact-datax-r_3_user   = abap_on.
*
*    wa_phone-contact-data-country    = 'BR'.
*    wa_phone-contact-data-telephone  = <tel>-telefone.
*    wa_phone-contact-data-extension  = <tel>-extensao.
*    wa_phone-contact-data-r_3_user   = ''.
*
*    PERFORM fill_remark USING <tel>-remark
*                     CHANGING wa_phone-remark.
*
*    APPEND wa_phone TO p_phone.
*  ENDLOOP.
*
*  LOOP AT p_cel_tab ASSIGNING FIELD-SYMBOL(<cel>).
*    CLEAR wa_phone.
*    wa_phone-contact-task             = c_object_task_update. "'U'.
*
*    wa_phone-contact-datax-country    = abap_on.
*    wa_phone-contact-datax-telephone  = abap_on.
*    wa_phone-contact-datax-r_3_user   = abap_on.
*
*    wa_phone-contact-data-country    = 'BR'.
*    wa_phone-contact-data-telephone  = <cel>-celular.
*    wa_phone-contact-data-r_3_user   = c_tel_mobile.
*
*    PERFORM fill_remark USING <tel>-remark
*                     CHANGING wa_phone-remark.
*
*    APPEND wa_phone TO p_phone.
*  ENDLOOP.
*
*ENDFORM.
*&---------------------------------------------------------------------*
*& Form FILL_REMARK
*&------v---------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> <TEL>_REMARK
*&      <-- WA_PHONE_REMARK
*&---------------------------------------------------------------------*
FORM fill_remark USING p_txt_remark TYPE ad_remark2
              CHANGING p_remark     TYPE bus_ei_comrem.

  DATA:
      wa_remark TYPE bus_ei_bupa_comrem.

  IF p_txt_remark IS NOT INITIAL.
    wa_remark-task             = gc_object_task_update. "'U'.
    wa_remark-datax-comm_notes = abap_on.
    wa_remark-data-comm_notes  = p_txt_remark.
    APPEND wa_remark TO p_remark-remarks.
  ENDIF.


ENDFORM.
*&---------------------------------------------------------------------*
*& Form FILL_FAX
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> P_FAX_TAB
*&      <-- P_COMMUNICATION_PHONE_PHONE
*&---------------------------------------------------------------------*
FORM fill_fax_create USING p_fornecedor TYPE zpf_fornecedor
                  CHANGING p_fax        TYPE bus_ei_bupa_fax_t.

  DATA:
    wa_fax TYPE bus_ei_bupa_fax.

  wa_fax-contact-task            = gc_object_task_insert. "'I'.

****      wa_fax-contact-datax-fax       = abap_on.
****      wa_fax-contact-datax-extension = abap_on.

  wa_fax-contact-data-fax       = p_fornecedor-fax_number.
  wa_fax-contact-data-country = p_fornecedor-country.
  " wa_fax-contact-data-extension  = <fax>-extensao.

  "
  APPEND wa_fax TO p_fax.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form FILL_FAX
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> P_FAX_TAB
*&      <-- P_COMMUNICATION_PHONE_PHONE
*&---------------------------------------------------------------------*
*FORM fill_fax USING p_fax_tab TYPE tty_zpf_fax
*             CHANGING p_fax   TYPE bus_ei_bupa_fax_t.
*
*  DATA:
*    wa_fax TYPE bus_ei_bupa_fax.
*
*  IF p_fax_tab IS NOT INITIAL.
*    LOOP AT p_fax_tab ASSIGNING FIELD-SYMBOL(<fax>).
*      CLEAR wa_fax.
*      wa_fax-contact-task            = c_object_task_update. "'U'.
*
*      wa_fax-contact-datax-fax       = abap_on.
*      wa_fax-contact-datax-extension = abap_on.
*
*      wa_fax-contact-data-fax        = <fax>-fax.
*      wa_fax-contact-data-extension  = <fax>-extensao.
*
*      PERFORM fill_remark USING <fax>-remark
*                       CHANGING wa_fax-remark.
*
*      APPEND wa_fax TO p_fax.
*    ENDLOOP.
*  ENDIF.
*
*ENDFORM.
*&---------------------------------------------------------------------*
*& Form FILL_EMAIL_CREATE
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> P_EMAIL_TAB
*&      <-- P_COMMUNICATION_PHONE_PHONE
*&---------------------------------------------------------------------*
FORM fill_email_create USING p_fornecedor TYPE zpf_fornecedor
                    CHANGING p_smtp       TYPE bus_ei_bupa_smtp_t.
  DATA:
    wa_smtp TYPE bus_ei_bupa_smtp.

  wa_smtp-contact-task         = gc_object_task_insert. "'I'.
  "wa_smtp-contact-datax-e_mail = abap_on.

  wa_smtp-contact-data-e_mail  = p_fornecedor-smtp_addr.

*      PERFORM fill_remark USING <email>-remark
*                       CHANGING wa_smtp-remark.

  APPEND wa_smtp TO p_smtp.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form FILL_EMAIL
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> P_EMAIL_TAB
*&      <-- P_COMMUNICATION_PHONE_PHONE
*&---------------------------------------------------------------------*
*FORM fill_email USING p_email_tab TYPE tty_zpf_email
*             CHANGING p_smtp      TYPE bus_ei_bupa_smtp_t.
*  DATA:
*    wa_smtp TYPE bus_ei_bupa_smtp.
*
*  IF p_email_tab IS NOT INITIAL.
*    LOOP AT p_email_tab ASSIGNING FIELD-SYMBOL(<email>).
*      CLEAR wa_smtp.
*      wa_smtp-contact-task         = c_object_task_update. "'U'.
*      wa_smtp-contact-datax-e_mail = abap_on.
*
*      wa_smtp-contact-data-e_mail  = <email>-email.
*
*      PERFORM fill_remark USING <email>-remark
*                       CHANGING wa_smtp-remark.
*
*      APPEND wa_smtp TO p_smtp.
*    ENDLOOP.
*  ENDIF.
*
*ENDFORM.
*&---------------------------------------------------------------------*
*& Form FILL_TAX
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> I_IR_TAB
*&      <-- E_MESS_TAB[]
*&      <-- <FS_CVIS_EI_EXTERN>_COMPANY_DA
*&---------------------------------------------------------------------*
*FORM fill_tax USING p_ir_tab   TYPE      tty_zpf_ir
*                    p_bukrs    TYPE      bukrs
*           CHANGING p_company  TYPE      vmds_ei_vmd_company.
*
*  DATA:
*    wa_tax_type TYPE vmds_ei_wtax_type,
*    wa_company  TYPE vmds_ei_company.
*
*  IF p_ir_tab IS NOT INITIAL.
*
*    CLEAR wa_company.
*    wa_company-task     = c_object_task_update. "'U'.
*    wa_company-data_key = p_bukrs.
*
*    LOOP AT p_ir_tab ASSIGNING FIELD-SYMBOL(<tax>).
*      CLEAR wa_tax_type.
*
*      wa_tax_type-task            = c_object_task_update. "'U'.
*      wa_tax_type-data_key        = <tax>-witht.
*
*      wa_tax_type-datax-wt_withcd = abap_on.
*      wa_tax_type-data-wt_withcd  = <tax>-wt_withcd.
*
*      APPEND wa_tax_type TO wa_company-wtax_type-wtax_type.
*    ENDLOOP.
*
*    APPEND wa_company TO p_company-company.
*  ENDIF.
*
*ENDFORM.
*&---------------------------------------------------------------------*
*& Form FILL_COMPANY
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> P_FORNECEDOR
*&      --> P_FORNECEDOR_X
*&      --> LV_CVIS_EI_EXTERN_VENDOR_COMPA
*&---------------------------------------------------------------------*
FORM fill_company USING p_fornecedor   TYPE zpf_fornecedor
                        p_fornecedorx  TYPE zpf_fornecedorx
                        p_ir_tab       TYPE ty_t_zpf_ir
                        p_bukrs        TYPE bukrs
               CHANGING p_company      TYPE vmds_ei_vmd_company.

  DATA:
    lv_changed  TYPE c,
    wa_tax_type TYPE vmds_ei_wtax_type,
    wa_company  TYPE vmds_ei_company.

  IF p_ir_tab IS NOT INITIAL.

    CLEAR wa_company.
    LOOP AT p_ir_tab ASSIGNING FIELD-SYMBOL(<tax>).
      CLEAR wa_tax_type.

      wa_tax_type-task            = gc_object_task_update. "'U'.
      wa_tax_type-data_key        = <tax>-witht.

      wa_tax_type-datax-wt_withcd = abap_on.
      wa_tax_type-data-wt_withcd  = <tax>-wt_withcd.

      APPEND wa_tax_type TO wa_company-wtax_type-wtax_type.
    ENDLOOP.

    lv_changed = abap_on.
  ENDIF.

  IF p_fornecedorx-zterm IS NOT INITIAL.
    wa_company-datax-zterm = abap_on.
*Start    - Marcelo Alvares - MA004818 INC0115504 - 06.09.2019 17:06
*        wa_company-data-zterm  = p_fornecedor-zterm.
    " Definição Regra de negocio que todo fornecedor deve ter a forma de pagamento TB30.
    wa_company-data-zterm  =  gc_zterm_tb30.
*END    - Marcelo Alvares - MA004818 INC0115504 - 06.09.2019 17:06

    lv_changed = abap_on.
  ENDIF.

  IF lv_changed IS NOT INITIAL.
    wa_company-task        = gc_object_task_update. "'U'.
    wa_company-data_key    = p_bukrs.


    CASE p_fornecedor-ktokk.
      WHEN gc_pessoa_juridica_nacj.
        wa_company-data-fdgrv = 'FORN NAC'.

      WHEN gc_pessoa_fisica_nacf.
        wa_company-data-fdgrv = 'FOR PESS F'.

    ENDCASE.

    APPEND wa_company TO p_company-company.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form FILTRA_BUKRS
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> E_LIFNR
*&      <-- GT_BUKRS
*&---------------------------------------------------------------------*
FORM filtra_bukrs USING p_lifnr   TYPE lifnr
               CHANGING p_t_bukrs TYPE ty_t_bukrs.

  DATA ti_bukrs TYPE ty_t_bukrs.

  IF p_t_bukrs IS NOT INITIAL.

    SELECT bukrs
      FROM lfb1
      INTO TABLE ti_bukrs
       FOR ALL ENTRIES IN p_t_bukrs
     WHERE bukrs = p_t_bukrs-bukrs
       AND lifnr = p_lifnr.

    p_t_bukrs = ti_bukrs.

  ENDIF.


ENDFORM.
*&---------------------------------------------------------------------*
*& Form FILL_INSCRICOES
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> I_FORNECEDORX
*&      --> I_FORNECEDOR
*&      --> E_LIFNR
*&      <-- LV_CVIS_EI_EXTERN_PARTNER_CENT
*&---------------------------------------------------------------------*
FORM fill_inscricoes USING p_fornecedorx TYPE zpf_fornecedorx
                           p_fornecedor  TYPE zpf_fornecedor
                           p_lifnr       TYPE lifnr
                  CHANGING p_taxnumbers  TYPE bus_ei_bupa_taxnumber_t.

  DATA:
    lv_stcd3     TYPE stcd3,
    lv_stcd4     TYPE stcd4,
    lv_taxnumber TYPE ty_s_taxnumber.


  SELECT SINGLE stcd3 stcd4
    FROM lfa1
    INTO (lv_stcd3, lv_stcd4)
   WHERE lifnr = p_lifnr.

*   Inscrição Estadual
  IF p_fornecedorx-stcd3 IS NOT INITIAL.
    IF lv_stcd3 IS INITIAL.
      MOVE gc_object_task_insert           TO lv_taxnumber-task.
    ELSE.
      MOVE gc_object_task_update           TO lv_taxnumber-task.
    ENDIF.
    MOVE p_fornecedor-stcd3 TO lv_taxnumber-data_key-taxnumber.
    MOVE 'BR3'              TO lv_taxnumber-data_key-taxtype.
    APPEND lv_taxnumber     TO p_taxnumbers.
  ENDIF.

*   Inscrição Municipal
  IF p_fornecedorx-stcd4 IS NOT INITIAL.
    IF lv_stcd4 IS INITIAL.
      MOVE gc_object_task_insert           TO lv_taxnumber-task.
    ELSE.
      MOVE gc_object_task_update           TO lv_taxnumber-task.
    ENDIF.
    MOVE p_fornecedor-stcd4 TO lv_taxnumber-data_key-taxnumber.
    MOVE 'BR4'              TO lv_taxnumber-data_key-taxtype.
    APPEND lv_taxnumber     TO gs_cvis_ei_extern-partner-central_data-taxnumber-taxnumbers.
  ENDIF.




ENDFORM.
*&---------------------------------------------------------------------*
*& Form FILL_COMM_UPDATE
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> E_TEL_TAB[]
*&      --> E_CEL_TAB[]
*&      --> E_FAX_TAB[]
*&      --> E_EMAIL_TAB[]
*&      --> E_CONTATOS_TAB[]
*&      --> LS_BP_CURRENT_CENTRAL_DATA_COM
*&      <-- LV_CVIS_EI_EXTERN_PARTNER_CENT
*&---------------------------------------------------------------------*
FORM fill_comm_update USING p_fornecedorx   TYPE zpf_fornecedorx
                            p_fornecedor    TYPE zpf_fornecedor
                   CHANGING p_comm_curr     TYPE bus_ei_communication  "Current
                            p_communication TYPE bus_ei_communication. "Update

  DATA:
    lv_update TYPE c,
    wa_phone  TYPE bus_ei_bupa_telephone,
    wa_fax    TYPE bus_ei_bupa_fax,
    wa_smtp   TYPE bus_ei_bupa_smtp.

  " Fixo
  IF p_fornecedorx-tel_number IS NOT INITIAL.
    LOOP AT p_comm_curr-phone-phone ASSIGNING FIELD-SYMBOL(<phone>)
         WHERE contact-data-r_3_user   = gc_default_fixed_tel. "1.
*      IF <phone>-contact-data-r_3_user   = ''.
      <phone>-contact-task             = gc_object_task_update. "'U'.
      <phone>-contact-datax-country    = abap_on.
      <phone>-contact-datax-telephone  = abap_on.
      <phone>-contact-datax-extension  = abap_on.
      <phone>-contact-datax-r_3_user   = abap_on.
      <phone>-contact-data-country     = p_fornecedor-country.
      <phone>-contact-data-telephone   = p_fornecedor-tel_number.
      lv_update = abap_on.
      EXIT.
*      ENDIF.
    ENDLOOP.

    IF lv_update IS INITIAL.
      wa_phone-contact-task            = gc_object_task_insert. "'I'.
      wa_phone-contact-data-country    = p_fornecedor-country.
      wa_phone-contact-data-telephone  = p_fornecedor-tel_number.
      wa_phone-contact-data-r_3_user   = ''.
      APPEND wa_phone TO p_comm_curr-phone-phone.
    ENDIF.
  ENDIF.
  CLEAR lv_update.

  "Celular
  IF p_fornecedorx-mob_number IS NOT INITIAL.
    LOOP AT p_comm_curr-phone-phone ASSIGNING FIELD-SYMBOL(<mob_phone>)
             WHERE contact-data-r_3_user   = '3'.
*      IF <mob_phone>-contact-data-r_3_user   = '3'.
      <mob_phone>-contact-task             = gc_object_task_update. "'U'.
      <mob_phone>-contact-datax-country    = abap_on.
      <mob_phone>-contact-datax-telephone  = abap_on.
      <mob_phone>-contact-datax-extension  = abap_on.
      <mob_phone>-contact-datax-r_3_user   = abap_on.
      <mob_phone>-contact-data-country     = p_fornecedor-country.
      <mob_phone>-contact-data-telephone   = p_fornecedor-mob_number.
      lv_update = abap_on.
      EXIT.
*      ENDIF.
    ENDLOOP.

    IF lv_update IS INITIAL.
      wa_phone-contact-task            = gc_object_task_insert. "'I'.
      wa_phone-contact-data-country    = p_fornecedor-country.
      wa_phone-contact-data-telephone  = p_fornecedor-mob_number.
      wa_phone-contact-data-r_3_user   = '3'.
      APPEND wa_phone TO p_comm_curr-phone-phone.
    ENDIF.
  ENDIF.
  CLEAR lv_update.

  "Fax
  IF p_fornecedorx-fax_number IS NOT INITIAL.
    LOOP AT p_comm_curr-fax-fax ASSIGNING FIELD-SYMBOL(<fax>).
      <fax>-contact-task             = gc_object_task_update. "'U'.
      <fax>-contact-datax-country    = abap_on.
      <fax>-contact-datax-fax        = abap_on.
      <fax>-contact-data-country     = p_fornecedor-country.
      <fax>-contact-data-fax         = p_fornecedor-fax_number.
      lv_update = abap_on.
      EXIT.
    ENDLOOP.

    IF lv_update IS INITIAL.
      wa_fax-contact-task         = gc_object_task_insert. "'I'.
      wa_fax-contact-data-country = p_fornecedor-country.
      wa_fax-contact-data-fax     = p_fornecedor-fax_number.
      APPEND wa_fax TO p_comm_curr-fax-fax.
    ENDIF.
  ENDIF.
  CLEAR lv_update.

  "Email
  IF p_fornecedorx-smtp_addr IS NOT INITIAL.
    LOOP AT p_comm_curr-smtp-smtp ASSIGNING FIELD-SYMBOL(<smtp>).
      <smtp>-contact-task         = gc_object_task_update. "'U'.
      <smtp>-contact-datax-e_mail = abap_on.
      <smtp>-contact-data-e_mail  = p_fornecedor-smtp_addr.
      lv_update = abap_on.
      EXIT.
    ENDLOOP.

    IF lv_update IS INITIAL.
      wa_smtp-contact-task         = gc_object_task_insert. "'I'.
      wa_smtp-contact-data-e_mail     = p_fornecedor-smtp_addr.
      APPEND wa_smtp TO p_comm_curr-smtp-smtp.
    ENDIF.
  ENDIF.
  CLEAR lv_update.

  p_communication = p_comm_curr.
ENDFORM.


*&---------------------------------------------------------------------*
*&      Form  GET_RANGE_BUKRS
*&---------------------------------------------------------------------*
FORM get_bukrs CHANGING pt_bukrs TYPE table
                        pt_msg TYPE ty_t_mensagem.
  DATA:
    rg_bukrs TYPE RANGE OF t001-bukrs.

  CALL METHOD zcl_variant=>get_range_from_set
    EXPORTING
      i_range_id      = 'ZEXTENSAO_FORNECEDOR'
    IMPORTING
      ep_range        = rg_bukrs[]
    EXCEPTIONS
      range_not_found = 1
      OTHERS          = 2.

  IF sy-subrc = 0.
    SELECT bukrs
    FROM t001
    INTO TABLE pt_bukrs
    WHERE bukrs IN rg_bukrs.
  ELSE.
    APPEND 'Set "ZEXTENSAO_FORNECEDOR" não encontrado.' TO pt_msg.
  ENDIF.

ENDFORM.                    " GET_RANGE_BUKRS
*&---------------------------------------------------------------------*
*&      Form  GET_LFB1
*&---------------------------------------------------------------------*
*FORM get_lfb1  USING    p_lifnr TYPE lfa1-lifnr
*                        pt_bukrs TYPE ty_bukrs
*               CHANGING pt_lfb1 TYPE ty_lfb1.
*
*  SELECT *
*  INTO TABLE pt_lfb1
*  FROM lfb1
*  FOR ALL ENTRIES IN pt_bukrs
*  WHERE lifnr = p_lifnr
*    AND bukrs = pt_bukrs-bukrs.
*
*ENDFORM.                                                    " GET_LFB1
*&---------------------------------------------------------------------*
*&      Form  EXCLUI_FORNECEDOR
*&---------------------------------------------------------------------*
*FORM exclui_fornecedor  USING    pt_lfb1 TYPE ty_lfb1
*                        CHANGING pt_msg TYPE ty_mensagem.
*  DATA:
*    ls_lfb1 TYPE lfb1,
*    lt_msg  TYPE bapiret2_t.
*
*  LOOP AT pt_lfb1 INTO ls_lfb1.
*    zcl_bdc=>reset( ).
*
*    zcl_bdc=>add( prog = 'SAPMF02K' dynpro = '0500' dynbg = 'X' ).
*    zcl_bdc=>add( fnam = 'BDC_OKCODE' fval = '/00' ).
*    zcl_bdc=>add( fnam = 'RF02K-LIFNR' fval = ls_lfb1-lifnr ).
*    zcl_bdc=>add( fnam = 'RF02K-BUKRS' fval = ls_lfb1-bukrs ).
*
*    zcl_bdc=>add( prog = 'SAPMF02K' dynpro = '0520' dynbg = 'X' ).
*    zcl_bdc=>add( fnam = 'BDC_OKCODE' fval = '=UPDA' ).
*    zcl_bdc=>add( fnam = 'LFA1-LOEVM' fval = 'X' ).
*    zcl_bdc=>add( fnam = 'LFA1-NODEL' fval = 'X' ).
*    zcl_bdc=>add( fnam = 'LFB1-LOEVM' fval = 'X' ).
*    zcl_bdc=>add( fnam = 'LFB1-NODEL' fval = 'X' ).
*
*    zcl_bdc=>call_transaction( EXPORTING iv_tcode    = 'XK06'
*                               IMPORTING et_messages = lt_msg ).
*
*    PERFORM converte_mensagem  USING lt_msg CHANGING pt_msg.
*
*  ENDLOOP.
*
*ENDFORM.                    " EXCLUI_FORNECEDOR
*&---------------------------------------------------------------------*
*&      Form  CONVERTE_MENSAGEM
*&---------------------------------------------------------------------*
*FORM converte_mensagem  USING  pt_bapi TYPE bapiret2_t
*                        CHANGING pt_msg TYPE ty_mensagem.
*
*  DATA:
*    ls_bapi TYPE bapiret2,
*    ls_msg  TYPE zpf_mensagem.
*
*  LOOP AT pt_bapi INTO ls_bapi.
*    ls_msg-type = ls_bapi-type.
*    MESSAGE ID ls_bapi-id TYPE ls_bapi-type NUMBER ls_bapi-number
*                  INTO ls_msg-texto
*                  WITH ls_bapi-message_v1 ls_bapi-message_v2 ls_bapi-message_v3 ls_bapi-message_v4.
*    APPEND ls_msg TO pt_msg.
*  ENDLOOP.
*
*ENDFORM.                    " CONVERTE_MENSAGEM
*&---------------------------------------------------------------------*
*&      Form  GET_LIFNR
*&---------------------------------------------------------------------*
FORM get_lifnr  USING p_cpf_cnpj TYPE lfa1-stcd1
             CHANGING p_lifnr TYPE lfa1-lifnr.
  DATA:
    l_count TYPE i.

  l_count = strlen( p_cpf_cnpj ).

  IF l_count = 11.
    SELECT SINGLE lifnr
      INTO p_lifnr
      FROM lfa1
     WHERE stcd2 = p_cpf_cnpj.

  ELSE.
    SELECT SINGLE lifnr
      INTO p_lifnr
      FROM lfa1
     WHERE stcd1 = p_cpf_cnpj.

  ENDIF.

ENDFORM.                    " GET_LIFNR


*&---------------------------------------------------------------------*
*& Form FILL_COMPANY_DATA
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> GT_BUKRS
*&      <-- LS_VMDS_EI_COMPANY
*&---------------------------------------------------------------------*
FORM fill_company_data USING p_gt_bukrs          TYPE ty_t_bukrs
                             p_s_fornecedor      TYPE zpf_fornecedor
                    CHANGING p_t_vmds_ei_company TYPE vmds_ei_company_t.

  DATA:
    ls_vmds_ei_company TYPE vmds_ei_company.

  CLEAR: ls_vmds_ei_company.

  LOOP AT p_gt_bukrs ASSIGNING FIELD-SYMBOL(<bukrs>).


    CALL FUNCTION 'LFB1_SINGLE_READ'
      EXPORTING
        i_lifnr         = p_s_fornecedor-lifnr
        i_bukrs         = <bukrs>-bukrs
      EXCEPTIONS
        not_found       = 1
        parameter_error = 2
        lifnr_blocked   = 3
        OTHERS          = 4.

    IF sy-subrc EQ 1. " No Entry Found
      ls_vmds_ei_company-task = gc_object_task_insert. "'I'.  "cc_object_task_insert. " I Insert
    ELSE.
      ls_vmds_ei_company-task = gc_object_task_update. "'U'.  "cc_object_task_update. " U
    ENDIF.

    ls_vmds_ei_company-data_key-bukrs = <bukrs>-bukrs.

*Start    - Marcelo Alvares - MA004818 INC0115504 - 06.09.2019 17:06
*        ls_vmds_ei_company-data-zterm     =  p_s_fornecedor-zterm.
    " Definição Regra de negocio que todo fornecedor deve ter a forma de pagamento TB30.
    ls_vmds_ei_company-data-zterm     =  gc_zterm_tb30.
*END    - Marcelo Alvares - MA004818 INC0115504 - 06.09.2019 17:06

    CASE p_s_fornecedor-ktokk.
      WHEN gc_pessoa_juridica_nacj.
        ls_vmds_ei_company-data-fdgrv = 'FORN NAC'.

      WHEN gc_pessoa_fisica_nacf.
        ls_vmds_ei_company-data-fdgrv = 'FOR PESS F'.

    ENDCASE.

    ls_vmds_ei_company-data-akont = gc_akont.

*      ls_vmds_ei_company-wtax_type  = me->fill_tax_data( EXPORTING
*                                                          im_s_bp_numbers = im_s_bp_numbers
*                                                          im_v_bukrs      = <fs_company_data>-bukrs ).

    APPEND ls_vmds_ei_company TO p_t_vmds_ei_company.

  ENDLOOP.
ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  busca_info_fornecedor
*&---------------------------------------------------------------------*
FORM busca_info_fornecedor USING p_lifnr TYPE lfa1-lifnr
                        CHANGING pt_forn TYPE ty_t_forn
                                 pt_msg  TYPE ty_t_mensagem.
  DATA:
      ls_msg        TYPE zpf_mensagem.

  REFRESH pt_forn.

  SELECT b~bukrs m~ekorg b~akont
    INTO TABLE pt_forn
    FROM lfa1 AS a
    LEFT OUTER JOIN lfb1 AS b ON b~lifnr = a~lifnr
    LEFT OUTER JOIN lfm1 AS m ON m~lifnr = a~lifnr
   WHERE a~lifnr = p_lifnr.

  IF sy-subrc = 4.
    CONCATENATE 'Fornecedor não cadastrado. Código: ' p_lifnr
           INTO ls_msg-texto
           SEPARATED BY space.
    ls_msg-type = 'E'.
    APPEND ls_msg TO pt_msg.
  ENDIF.

ENDFORM.                    " busca_info_fornecedor

*&---------------------------------------------------------------------*
*& Form F_CREATE_FILL_ADDRESS
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      <-- GS_ADDRESS
*&---------------------------------------------------------------------*
FORM f_create_fill_address  USING p_s_fornecedor TYPE zpf_fornecedor
                         CHANGING ch_t_mess_tab  TYPE ty_t_mensagem
                                  ch_t_address   TYPE bus_ei_bupa_address_t.

  DATA:
    ls_address    TYPE bus_ei_bupa_address,
    lv_taxjurcode TYPE j_1btxjcd,
    ls_mess_tab   TYPE zpf_mensagem.

  ls_address-task                             = gc_object_task_insert.
  ls_address-data-postal-data-standardaddress = abap_on.
  ls_address-data-postal-data-langu           = gc_languiso_pt.
  ls_address-data-postal-data-street          = p_s_fornecedor-street.        " Rua
  ls_address-data-postal-data-postl_cod1      = p_s_fornecedor-post_code1.    " Código postal da localidade
  ls_address-data-postal-data-city            = p_s_fornecedor-city1.         " Local
  ls_address-data-postal-data-district        = p_s_fornecedor-city2.         " Bairro
  ls_address-data-postal-data-region          = p_s_fornecedor-region.        " Região (estado federal)
  ls_address-data-postal-data-country         = p_s_fornecedor-country.       " Chave do país
  ls_address-data-postal-data-house_no        = p_s_fornecedor-house_num1.    " Nº
  ls_address-data-postal-data-house_no2       = p_s_fornecedor-house_num2.    " Complemento do nº
  ls_address-data-postal-data-str_suppl3      = p_s_fornecedor-str_suppl3.    " Rua 4

  PERFORM fill_phone_create    USING p_s_fornecedor
                            CHANGING ls_address-data-communication-phone-phone.

  PERFORM fill_fax_create      USING p_s_fornecedor
                            CHANGING ls_address-data-communication-fax-fax.

  PERFORM fill_email_create    USING p_s_fornecedor
                            CHANGING ls_address-data-communication-smtp-smtp.


* domicilio fiscal
  SELECT SINGLE taxjurcode
    INTO lv_taxjurcode
    FROM j_1btreg_city
   WHERE country = 'BR'
     AND region      = p_s_fornecedor-region
     AND pstcd_from <= p_s_fornecedor-post_code1
     AND pstcd_to   >= p_s_fornecedor-post_code1.

  IF sy-subrc IS NOT INITIAL.
    gv_flag_erro        = abap_true.
    ls_mess_tab-type    = 'E'.
    ls_mess_tab-texto   = 'Código Postal inválido'.
    APPEND ls_mess_tab TO ch_t_mess_tab.
    RETURN.
  ENDIF.
  ls_address-data-postal-data-taxjurcode       = lv_taxjurcode.

  APPEND ls_address TO ch_t_address.


ENDFORM.
*&---------------------------------------------------------------------*
*& Form F_CREATE_BPARTNERGUID
*&---------------------------------------------------------------------*
*& Generates 16 Byte System UUID in Binary Format
*&---------------------------------------------------------------------*
*&      <--> P_CH_BPARTNERGUID
*&---------------------------------------------------------------------*
FORM f_create_bpartnerguid  CHANGING p_ch_bpartnerguid TYPE bus_ei_instance-bpartnerguid.

  TRY.
      CALL METHOD cl_system_uuid=>create_uuid_x16_static
        RECEIVING
          uuid = p_ch_bpartnerguid.
    CATCH cx_uuid_error ##NO_HANDLER.
  ENDTRY.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form F_CREATE_FILL_PURCHASING_DATA
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      <-- GS_CVIS_EI_BP_VENDOR_PURCHASIN
*&---------------------------------------------------------------------*
FORM f_create_fill_purchasing_data  CHANGING p_t_vendor_purchasin TYPE vmds_ei_purchasing_t.

  CONSTANTS:
    lc_purchasing_org_pg01 TYPE ekorg  VALUE 'PG01', " Compras Suprimentos
    lc_purchasing_org_pg02 TYPE ekorg  VALUE 'PG02'. " Compras Diretas

  DATA:
    ls_purchasing TYPE vmds_ei_purchasing,   " organização de compras
    ls_functions  TYPE vmds_ei_functions.    " funções de parceiro


  MOVE:
    gc_object_task_insert   TO ls_purchasing-task,
    abap_off                TO ls_purchasing-data-sperm, " Compras bloqueadas a nível da organização de compras
    lc_purchasing_org_pg01  TO ls_purchasing-data_key-ekorg, " Organização de compras
    gc_zterm_tb30           TO ls_purchasing-data-zterm, " Chave de condições de pagamento
    gc_moeda_brl            TO ls_purchasing-data-waers, " Moeda do pedido
    abap_on                 TO ls_purchasing-data-lebre, " Código para revisão de faturas baseada em serviços
    abap_on                 TO ls_purchasing-data-webre. " Código: revisão de faturas baseada em entrada de mercadorias

  APPEND ls_purchasing          TO p_t_vendor_purchasin.

  MOVE lc_purchasing_org_pg02   TO ls_purchasing-data_key-ekorg.

  APPEND ls_purchasing          TO p_t_vendor_purchasin.

* funcao parceiro
  LOOP AT p_t_vendor_purchasin ASSIGNING FIELD-SYMBOL(<fs_purchasing>) .
    MOVE:
     gc_object_task_insert  TO ls_functions-task,                       " código de modificação funções de parceiro
         'LF'               TO ls_functions-data_key-parvw.             " Função do parceiro
    APPEND ls_functions     TO <fs_purchasing>-functions-functions.

    MOVE 'WL'               TO ls_functions-data_key-parvw.             " Função do parceiro
    APPEND ls_functions     TO <fs_purchasing>-functions-functions.

    MOVE 'RS'               TO ls_functions-data_key-parvw.             " Função do parceiro
    APPEND ls_functions     TO <fs_purchasing>-functions-functions.
  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form F_CREATE_CONTACT_RELATIONSHIP
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> P_V_PARTNER_GUID
*&      --> P_V_CONTACT_GUID
*&---------------------------------------------------------------------*
FORM f_create_contact_relationship  USING p_v_partner_guid TYPE bu_partner_guid_bapi
                                          p_v_contact_guid TYPE bu_partner_guid_bapi
                                 CHANGING ch_t_return_map  TYPE mdg_bs_bp_msgmap_t.

  DATA:
    lv_partner_guid       TYPE bu_partner_guid,
    lv_contactperson_guid TYPE bu_partner_guid,
    lt_bapiret2           TYPE bapiret2_t.

  MOVE: p_v_partner_guid TO lv_partner_guid,
        p_v_contact_guid TO lv_contactperson_guid.

  CALL FUNCTION 'BUPR_CONTP_CREATE'
    EXPORTING
      iv_partner_guid       = lv_partner_guid
      iv_contactperson_guid = lv_contactperson_guid
      iv_date_from          = sy-datlo
      iv_x_save             = abap_on
    TABLES
      et_return             = lt_bapiret2.

  LOOP AT lt_bapiret2 TRANSPORTING NO FIELDS WHERE type CA 'AE'. " Contains Any
  ENDLOOP.

  IF syst-subrc IS NOT INITIAL. " NO ERRORS
    CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
      EXPORTING
        wait = abap_true.     " Use of Command `COMMIT AND WAIT`

  ELSE.


  ENDIF.


ENDFORM.
*&---------------------------------------------------------------------*
*& Form F_CREATE_BP_MAINTAIN
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> P_S_DATA        TYPE CVIS_EI_EXTERN
*&      --> P_V_MAINTAIN    TYPE ABAP_BOOL
*&     <--> CH_T_RETURN_MAP TYPE MDG_BS_BP_MSGMAP_T
*&     <--> CH_T_BAPIRETM   TYPE BAPIRETM
*&     <--> CH_V_ERROR      TYPE ABAP_BOOL.
*&---------------------------------------------------------------------*
FORM f_call_bp_maintain    USING    p_s_data        TYPE cvis_ei_extern
                                    p_v_maintain    TYPE abap_bool
                           CHANGING ch_t_mess_tab   TYPE ty_t_mensagem
                                    ch_t_return_map TYPE mdg_bs_bp_msgmap_t
                                    ch_t_bapiretm   TYPE bapiretm
                                    ch_v_error      TYPE abap_bool.

  DATA:
    lo_cl_md_bp_maintain TYPE REF TO cl_md_bp_maintain,
    lt_data              TYPE cvis_ei_extern_t,
    ls_mess_tab          TYPE zpf_mensagem.

  CREATE OBJECT lo_cl_md_bp_maintain.

  CLEAR:
    ch_v_error.

  IF p_v_maintain IS INITIAL.

    CALL METHOD lo_cl_md_bp_maintain->validate_single(
      EXPORTING
        i_data        = p_s_data
      IMPORTING
        et_return_map = ch_t_return_map ).

    LOOP AT ch_t_return_map ASSIGNING FIELD-SYMBOL(<fs_s_return_map>) WHERE type CA 'AE'. "Contains Any A or E
      ch_v_error = abap_true.
      ls_mess_tab-type  = <fs_s_return_map>-type.
      ls_mess_tab-texto = <fs_s_return_map>-message.
      APPEND ls_mess_tab TO ch_t_mess_tab.
    ENDLOOP.

  ELSE.

    APPEND p_s_data TO lt_data.

    CALL METHOD lo_cl_md_bp_maintain->maintain
      EXPORTING
        i_data   = lt_data
      IMPORTING
        e_return = ch_t_bapiretm.

    LOOP AT ch_t_bapiretm ASSIGNING FIELD-SYMBOL(<fs_e_return>).
      LOOP AT <fs_e_return>-object_msg ASSIGNING FIELD-SYMBOL(<fs_s_object_msg>) WHERE type CA 'AE'. "Contains Any A or E
        ch_v_error        = abap_true.
        ls_mess_tab-type  = <fs_s_object_msg>-type.
        ls_mess_tab-texto = <fs_s_object_msg>-message.
        APPEND ls_mess_tab TO ch_t_mess_tab.
      ENDLOOP.
    ENDLOOP.

  ENDIF.


ENDFORM.
*&---------------------------------------------------------------------*
*& Form F_CHECK_AND_CHANGE_BANKID
*&---------------------------------------------------------------------*
*& Form to set the ID of the business partner bank, it is not possible
*& to have more than one ID equal.
*& If the data has more than one equal, the system adds an ASCII character
*& at position four of the ID sequentially until there are no matches.
*& EXAMPLE A-> B-> C ....
*&---------------------------------------------------------------------*
*&      <-- CH_T_BANKDETAILS
*&---------------------------------------------------------------------*
FORM f_check_and_change_bankid  CHANGING ch_t_bankdetails TYPE bus_ei_bupa_bankdetail_t.

  CONSTANTS:
    c_ascii_value_initial TYPE i VALUE 64.
  DATA:
    lv_char    TYPE c LENGTH 1,
    lv_ascii   TYPE i VALUE c_ascii_value_initial,
    lv_counter TYPE i,
    lv_control TYPE abap_bool VALUE abap_false,
    lv_bkvid   TYPE bu_bkvid.

*=====================================================================
*   Verifica se já existe algum ID de dados bancários igual

  SORT ch_t_bankdetails ASCENDING BY data_key data task ASCENDING .

  LOOP AT ch_t_bankdetails ASSIGNING FIELD-SYMBOL(<fs_group_key>).

*====================AT NEW data_key==================================
    AT NEW data_key(3).
      "Inicializa variavel:
      lv_counter = 1.
    ENDAT.
*====================AT NEW data_key==================================

    ADD 1 TO lv_counter.

*--------------------AT END OF data_key------------------------------
    AT END OF data_key(3).

      CHECK lv_counter GT 1.

      LOOP AT ch_t_bankdetails
          ASSIGNING FIELD-SYMBOL(<fs_s_bankdetails>)
          WHERE data_key(3) EQ <fs_group_key>-data_key(3).

        lv_control  = abap_false.
        lv_ascii    = c_ascii_value_initial.

        WHILE lv_control EQ abap_false OR
              lv_ascii GT 90.

          ADD 1 TO lv_ascii.
          lv_char = cl_abap_conv_in_ce=>uccpi( lv_ascii ).
          lv_bkvid = <fs_s_bankdetails>-data_key(3).
          lv_bkvid+3(1) = lv_char.

          READ TABLE ch_t_bankdetails
            TRANSPORTING NO FIELDS
            WITH KEY data_key = lv_bkvid .

          IF syst-subrc IS NOT INITIAL.
            <fs_s_bankdetails>-data_key = lv_bkvid.
            lv_control = abap_true.
          ELSEIF lv_bkvid EQ <fs_s_bankdetails>-data_key. "Está correto
            lv_control = abap_true.                       "Encerra o while
          ENDIF.

        ENDWHILE.

      ENDLOOP.

    ENDAT. "AT END OF data_key.
*--------------------AT END OF data_key------------------------------

  ENDLOOP.  "AT ch_t_bankdetails

  SORT ch_t_bankdetails ASCENDING BY data_key data.

ENDFORM.
