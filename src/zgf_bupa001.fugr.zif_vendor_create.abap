*--------------------------------------------------------------------*
*               P R O J E T O    A G I R - T A E S A                 *
*--------------------------------------------------------------------*
* Consultoria .....: I N T E C H P R O                               *
* Res. ABAP........: Ricardo Monte                                   *
* Res. Funcional...:                                                 *
* Módulo...........: Suprimentos                                     *
* Programa.........: ZIF_VENDOR_CREATE                               *
* Transação........: N/A                                             *
* Tipo de Programa.: FUNÇÃO                                          *
* Request     .....: S4DK926253                                      *
* Objetivo.........: Atualização de Fornecedores pelo SERTRAS        *
*--------------------------------------------------------------------*
* Change Control:                                                    *
* Version | Date      | Who                 |   What                 *
*    2.00 | 28/10/18  | Ricardo Monte       |   Versão AGIR          *
*    2.01 | 06/05/19  | Marcelo Alvares     |   Ajustes 1000001164   *
*    2.02 | 26/03/20  | Hemerson Barbosa    |   SHDK907533           *
**********************************************************************
FUNCTION zif_vendor_create.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(I_FORNECEDOR) TYPE  ZPF_FORNECEDOR
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

  CLEAR: e_lifnr, e_return_map, e_return, e_mess_tab .

  DATA:
    ls_roles        TYPE bus_ei_bupa_roles,
    lv_role         TYPE bu_role,
    lv_partner_guid TYPE bu_partner_guid.

*Start  - Marcelo Alvares - MA004818 INC0114565 - 19.08.2019 17:30
  go_bal_log = NEW lcl_log_create( ).
  go_bal_log->add_msg_import_table( im_s_struc = i_fornecedor ).
  go_bal_log->add_msg_import_table( im_t_table = i_banco_tab[] ).
  go_bal_log->add_msg_import_table( im_t_table = i_ir_tab[] ).
  go_bal_log->add_msg_import_table( im_t_table = e_tel_tab[] ).
  go_bal_log->add_msg_import_table( im_t_table = e_cel_tab[] ).
  go_bal_log->add_msg_import_table( im_t_table = e_fax_tab[] ).
  go_bal_log->add_msg_import_table( im_t_table = e_email_tab[] ).
  go_bal_log->add_msg_import_table( im_t_table = e_contatos_tab[] ).
*END    - Marcelo Alvares - MA004818 INC0114565 - 19.08.2019 17:30

  " Move task I Insert to create BP and Vendor
  MOVE gc_object_task_insert TO:
       gs_cvis_ei_bp-partner-header-object_task,
       gs_cvis_ei_bp-vendor-header-object_task.

* categoria PN
  MOVE gc_bp_category_organization
    TO gs_cvis_ei_bp-partner-central_data-common-data-bp_control-category.

  PERFORM f_create_bpartnerguid CHANGING gs_cvis_ei_bp-partner-header-object_instance-bpartnerguid.

  PERFORM f_create_fill_address USING i_fornecedor
                             CHANGING e_mess_tab[]
                                      gs_cvis_ei_bp-partner-central_data-address-addresses.

  PERFORM fill_phone_create     USING i_fornecedor
                             CHANGING gs_cvis_ei_bp-partner-central_data-communication-phone-phone.

  PERFORM fill_fax_create       USING i_fornecedor
                             CHANGING gs_cvis_ei_bp-partner-central_data-communication-fax-fax.

  PERFORM fill_email_create     USING i_fornecedor
                             CHANGING gs_cvis_ei_bp-partner-central_data-communication-smtp-smtp.

  MOVE i_fornecedor-sortl TO:
       gs_cvis_ei_bp-partner-central_data-common-data-bp_centraldata-searchterm1.
*       gs_bapibus1006-data-bp_centraldata-searchterm1.

  PERFORM get_bukrs      CHANGING gt_bukrs
                                  e_mess_tab[].

  PERFORM fill_company_data USING gt_bukrs
                                  i_fornecedor
                         CHANGING gs_cvis_ei_bp-vendor-company_data-company.

  PERFORM f_create_fill_bank_data
                            USING i_banco_tab[]
                         CHANGING gs_cvis_ei_bp-partner-central_data-bankdetail.

  " Block vendor
  MOVE abap_on TO:
    gs_cvis_ei_bp-vendor-central_data-central-data-sperr, " Central posting block
    gs_cvis_ei_bp-vendor-central_data-central-data-sperm, " Centrally imposed purchasing block
    gs_cvis_ei_bp-vendor-central_data-central-data-zz_is_dbloq_por_wf.  "SHDK907533

  gs_cvis_ei_bp-partner-central_data-common-data-bp_organization-name1 = i_fornecedor-name1.
  gs_cvis_ei_bp-partner-central_data-common-data-bp_control-grouping   = i_fornecedor-ktokk.

  PERFORM f_create_fill_purchasing_data
                       CHANGING gs_cvis_ei_bp-vendor-purchasing_data-purchasing.

* roles
  MOVE 'FLVN00'  TO: ls_roles-data_key,
                     ls_roles-data-rolecategory.
  MOVE: gc_object_task_insert   TO ls_roles-task,
        sy-datum   TO  ls_roles-data-valid_from,
        '99991231' TO  ls_roles-data-valid_to.

  APPEND ls_roles TO  gs_cvis_ei_bp-partner-central_data-role-roles.

  MOVE 'FLVN01'   TO: ls_roles-data_key,
                      ls_roles-data-rolecategory.
  APPEND ls_roles TO gs_cvis_ei_bp-partner-central_data-role-roles.

  "Exporta o Role do PN para bloqueio pela Badi ZCL_IM_BADI_BP_BLOQ
  lv_role = ls_roles-data_key.
  EXPORT lv_role = lv_role TO MEMORY ID 'FNRL'.

* CNPJ
  MOVE: gc_object_task_insert TO gs_taxnumber-task,
        i_fornecedor-stcd1    TO gs_taxnumber-data_key-taxnumber,
        'BR1'                 TO gs_taxnumber-data_key-taxtype.
  APPEND gs_taxnumber         TO gs_cvis_ei_bp-partner-central_data-taxnumber-taxnumbers.

* Inscrição Estadual
  IF i_fornecedor-stcd3 IS NOT INITIAL.
    MOVE: gc_object_task_insert  TO gs_taxnumber-task,
          i_fornecedor-stcd3     TO gs_taxnumber-data_key-taxnumber,
         'BR3'                   TO gs_taxnumber-data_key-taxtype.
    APPEND gs_taxnumber          TO gs_cvis_ei_bp-partner-central_data-taxnumber-taxnumbers.
  ENDIF.

* Inscrição Municipal
  IF i_fornecedor-stcd3 IS NOT INITIAL.
    MOVE: gc_object_task_insert TO gs_taxnumber-task,
          i_fornecedor-stcd4    TO gs_taxnumber-data_key-taxnumber,
          'BR4'                 TO gs_taxnumber-data_key-taxtype.
    APPEND gs_taxnumber         TO gs_cvis_ei_bp-partner-central_data-taxnumber-taxnumbers.
  ENDIF.

  PERFORM f_call_bp_maintain
   USING    gs_cvis_ei_bp
            abap_false      " Validate
   CHANGING e_mess_tab[]
            e_return_map
            e_return
            gv_flag_erro.

  IF gv_flag_erro IS INITIAL.

*   criacao do BP
    PERFORM f_call_bp_maintain
     USING    gs_cvis_ei_bp
              abap_true     " Create
     CHANGING e_mess_tab[]
              e_return_map
              e_return
              gv_flag_erro.

  ENDIF.

  IF gv_flag_erro IS INITIAL.
    CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
      EXPORTING
        wait = abap_true.      " Use of Command `COMMIT AND WAIT`

    CLEAR:
        gs_cvis_ei_extern,
        gt_cvis_ei_extern_t,
        e_return[],
        e_return.

* criando BP contatos
    LOOP AT e_contatos_tab.

      CLEAR: gs_cvis_ei_contact,
             gt_cvis_ei_extern_t.

      gs_cvis_ei_contact-partner-header-object_task = gc_object_task_insert.
      PERFORM f_create_bpartnerguid
       CHANGING gs_cvis_ei_contact-partner-header-object_instance-bpartnerguid.

      PERFORM fill_comm_create
          USING e_contatos_tab-pos
                e_tel_tab[]
                e_cel_tab[]
                e_fax_tab[]
                e_email_tab[]
       CHANGING gs_cvis_ei_contact-partner-central_data-communication.

      MOVE-CORRESPONDING gs_cvis_ei_bp-partner-central_data-address-addresses TO
                         gs_cvis_ei_contact-partner-central_data-address-addresses.

      gs_cvis_ei_contact-partner-central_data-common-data-bp_person-firstname           = e_contatos_tab-nome.
      gs_cvis_ei_contact-partner-central_data-common-data-bp_person-lastname            = e_contatos_tab-sobrenome.
      gs_cvis_ei_contact-partner-central_data-common-data-bp_person-correspondlanguage  = gc_languiso_pt.

      gs_cvis_ei_contact-partner-central_data-common-data-bp_control-category = gc_bp_category_person.     " Pessoa
      gs_cvis_ei_contact-partner-central_data-common-data-bp_control-grouping = gc_pessoa_fisica_nacf.

      CLEAR ls_roles.
      MOVE 'BUP001'   TO ls_roles-data-rolecategory.
      MOVE sy-datum   TO ls_roles-data-valid_from.
      MOVE '99991231' TO ls_roles-data-valid_to.
      MOVE 'I'        TO ls_roles-task.

      APPEND ls_roles           TO gs_cvis_ei_contact-partner-central_data-role-roles.


      APPEND gs_cvis_ei_contact TO gt_cvis_ei_extern_t.

*     criacao da pessoa
      PERFORM f_call_bp_maintain
        USING
          gs_cvis_ei_contact
          abap_true
        CHANGING
          e_mess_tab[]
          e_return_map
          e_return
          gv_flag_erro.

      IF gv_flag_erro IS INITIAL.
        CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
          EXPORTING
            wait = abap_true.          " Use of Command `COMMIT AND WAIT`

        PERFORM f_create_contact_relationship
            USING gs_cvis_ei_bp-partner-header-object_instance-bpartnerguid
                  gs_cvis_ei_contact-partner-header-object_instance-bpartnerguid
         CHANGING e_return_map.

      ENDIF.

    ENDLOOP.

    lv_partner_guid = gs_cvis_ei_bp-partner-header-object_instance-bpartnerguid.

    CALL FUNCTION 'BUPA_NUMBERS_GET'
      EXPORTING
        iv_partner_guid = lv_partner_guid
      IMPORTING
        ev_partner      = gv_partner_ret.


    IF gv_partner_ret IS NOT INITIAL.

      DATA(lo_bp_vendor_ret) = cvi_bp_vendor=>get_instance(
                               i_partner = gv_partner_ret ).

      e_lifnr = lo_bp_vendor_ret->get_vendor( ).
    ENDIF.

    gv_key = gv_partner_ret.  "lv_vendor.

    CALL FUNCTION 'SAP_WAPI_CREATE_EVENT'
      EXPORTING
        object_type = 'BUS1006'
        object_key  = gv_key
        event       = 'ZCREATED'.

  ENDIF.

*Start  - Marcelo Alvares - MA004818 INC0114565 - 19.08.2019 17:30
  go_bal_log->add_msg_return(
    EXPORTING
      im_t_mess_tab     = e_mess_tab[]
      im_t_return       = e_return[]
      im_t_return_map   = e_return_map[]  ).
  go_bal_log->save( ).
*END    - Marcelo Alvares - MA004818 INC0114565 - 19.08.2019 17:30


ENDFUNCTION.
