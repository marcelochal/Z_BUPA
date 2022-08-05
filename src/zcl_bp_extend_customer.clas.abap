"! <p class="shorttext synchronized" lang="en">Custumer / BP extension to company and sales organization</p>
"! <p class="shorttext synchronized" lang="pt">Cliente / Estensão de BP para Empresas e Organização de vendas</p>
CLASS zcl_bp_extend_customer DEFINITION PUBLIC
  INHERITING FROM zcl_bp_standard
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES zif_bp_extend_customer.

    ALIASES: ty_s_bp_extend FOR zif_bp_extend_customer~ty_s_bp_extend,
             ty_t_bp_extend FOR zif_bp_extend_customer~ty_t_bp_extend.

    TYPES:
      tab TYPE STANDARD TABLE OF REF TO zcl_bp_extend_customer WITH DEFAULT KEY .

    "! <p class="shorttext synchronized" lang="en">Class Constructor</p>
    "!
    "! @parameter im_v_bukrs_from | <p class="shorttext synchronized" lang="en">From Company</p>
    "! @parameter im_v_vkorg_from | <p class="shorttext synchronized" lang="en">From Sale Organization</p>
    "! @parameter im_t_bukrs_for | <p class="shorttext synchronized" lang="en">For Company Codes</p>
    "! @parameter im_t_vkorg_for | <p class="shorttext synchronized" lang="en">For Sales Organizations</p>
    "! @parameter im_t_bp_data | <p class="shorttext synchronized" lang="en">All BP Data</p>
    "! @raising zcx_abap_error | <p class="shorttext synchronized" lang="en">Class-based exception</p>
    METHODS constructor
      IMPORTING
        im_v_bukrs_from TYPE bukrs OPTIONAL
        im_v_vkorg_from TYPE vkorg OPTIONAL
        im_t_bukrs_for  TYPE zif_bp_standard~ty_t_t001_key OPTIONAL
        im_t_vkorg_for  TYPE zif_bp_standard~ty_t_tvko OPTIONAL
        im_t_bp_data    TYPE ty_t_bp_extend OPTIONAL
      RAISING
        zcx_abap_error .


    METHODS:
      "! <p class="shorttext synchronized" lang="en">Extend customer company and sales data</p>
      "! <p class="shorttext synchronized" lang="pt">Extensão de clientes para empresas e vendas</p>
      "!
      "! @parameter im_b_test    | <p class="shorttext synchronized" lang="en">Test Flag</p>
      "! @parameter ex_t_return  | <p class="shorttext synchronized" lang="en">Return bapiretm</p>
      "! @raising zcx_abap_error | <p class="shorttext synchronized" lang="en">Error Class</p>
      extend_customer
        IMPORTING
          !im_b_test   TYPE abap_bool OPTIONAL
        EXPORTING
          !ex_t_return TYPE bapiretm
        RAISING
          zcx_abap_error .

  PROTECTED SECTION.
    ALIASES:
        bukrs_from  FOR zif_bp_extend_customer~bukrs_from,
        vkorg_from  FOR zif_bp_extend_customer~vkorg_from,
        t_bukrs_for FOR zif_bp_extend_customer~t_bukrs_for,
        t_vkorg_for FOR zif_bp_extend_customer~t_vkorg_for,
        t_bp_data   FOR zif_bp_extend_customer~t_bp_data.


  PRIVATE SECTION.

ENDCLASS.


CLASS zcl_bp_extend_customer IMPLEMENTATION.

  METHOD extend_customer.

    DATA:
      lt_return_maintain        TYPE bapiretm,
      lt_return_validate_single TYPE mdg_bs_bp_msgmap_t,
      lt_cvis_ei_extern_t       TYPE cvis_ei_extern_t,
      ls_cvis_ei_extern         TYPE cvis_ei_extern,
      ls_company                TYPE cmds_ei_company,
      ls_sales                  TYPE cmds_ei_sales,
      lt_zj1btxcli              TYPE zif_bp_standard~ty_t_zj1btxcli,
      lt_zj1btxcli_dele         TYPE zif_bp_standard~ty_t_zj1btxcli.

    IF me->o_prog_ind IS BOUND.
      me->o_prog_ind->set_total( im_total = lines( me->t_bp_data ) ).
    ELSE.
      CREATE OBJECT me->o_prog_ind
        EXPORTING
          im_v_total        = lines( me->t_bp_data )
          im_v_text_default = |{ 'Estendendo Clientes'(002) }|.
    ENDIF.

    LOOP AT me->t_bp_data ASSIGNING FIELD-SYMBOL(<fs_bp_data>) USING KEY customer_sort_key.

      me->o_prog_ind->show( ).

      CLEAR: ls_cvis_ei_extern, ls_sales,
             ls_company, lt_return_validate_single,
             lt_cvis_ei_extern_t, lt_return_maintain,

      " Set fields , avoid errors AUTHORITY-CHECK
            <fs_bp_data>-partner_bus-central_data-role-time_dependent,
            <fs_bp_data>-partner_bus-central_data-role-current_state.

      " Initialized return messages
      APPEND INITIAL LINE TO ex_t_return ASSIGNING FIELD-SYMBOL(<ex_s_return>).
      <ex_s_return>-object_idx = syst-tabix.
      <ex_s_return>-object_key = <fs_bp_data>-partner_guid.

      MOVE-CORRESPONDING:
        <fs_bp_data>-partner_bus          TO ls_cvis_ei_extern-partner,
        <fs_bp_data>-customer_cmds-header TO ls_cvis_ei_extern-customer-header.

      MOVE co_task_update TO:
        ls_cvis_ei_extern-partner-header-object_task,
        ls_cvis_ei_extern-customer-header-object_task.



**********************************************************************
*-------- S T A R T  C O M P A N Y   E X T E N S I O N --------------*
**********************************************************************
      IF me->bukrs_from IS NOT INITIAL.

        READ TABLE <fs_bp_data>-customer_cmds-company_data-company
          WITH KEY data_key-bukrs = me->bukrs_from
          INTO ls_company.

        IF syst-subrc IS NOT INITIAL.
          " Error Company model data not found
          MESSAGE e044 " Company &1 is not valid as a template for Customer &2
              WITH me->bukrs_from <fs_bp_data>-customer
              INTO DATA(lv_msgdummy).
          APPEND INITIAL LINE TO <ex_s_return>-object_msg ASSIGNING FIELD-SYMBOL(<return_msg>).
          <return_msg>-id         = sy-msgid.
          <return_msg>-type       = sy-msgty.
          <return_msg>-number     = sy-msgno.
          <return_msg>-message_v1 = sy-msgv1.
          <return_msg>-message    = lv_msgdummy.
        ENDIF.

        ls_company-task = co_task_insert.

        LOOP AT me->t_bukrs_for ASSIGNING FIELD-SYMBOL(<fs_bukrs_for>).
          CLEAR: lt_zj1btxcli, lt_zj1btxcli_dele.

          " Taxes for AVC
          lt_zj1btxcli = VALUE #( FOR ls_zj1btxcli IN <fs_bp_data>-customer_kn-zj1btxcli
                                    WHERE ( bukrs       = me->bukrs_from )
                                          ( mandt       = ls_zj1btxcli-mandt
                                            kunnr       = ls_zj1btxcli-kunnr
                                            bukrs       = <fs_bukrs_for>-bukrs
                                            codimp      = ls_zj1btxcli-codimp
                                            responsavel = syst-uname ) ).

          " Remove data using reference data model
          LOOP AT      <fs_bp_data>-customer_kn-zj1btxcli ASSIGNING FIELD-SYMBOL(<ls_zj1btxcli_for_bukrs>)  WHERE bukrs EQ <fs_bukrs_for>-bukrs.
            READ TABLE <fs_bp_data>-customer_kn-zj1btxcli ASSIGNING FIELD-SYMBOL(<ls_zj1btxcli_from_bukrs>)
                WITH TABLE KEY sort_key COMPONENTS bukrs  = me->bukrs_from
                                                   kunnr  = <ls_zj1btxcli_for_bukrs>-kunnr
                                                   codimp = <ls_zj1btxcli_for_bukrs>-codimp.
            IF syst-subrc IS NOT INITIAL.
              APPEND <ls_zj1btxcli_for_bukrs> TO lt_zj1btxcli_dele.
            ENDIF.
          ENDLOOP.

          " Taxes for AVC Delete, Modify or Insert
          MODIFY zj1btxcli FROM TABLE lt_zj1btxcli.
          DELETE zj1btxcli FROM TABLE lt_zj1btxcli_dele.

          " Check if already exists
          READ TABLE <fs_bp_data>-customer_cmds-company_data-company
            WITH KEY data_key-bukrs = <fs_bukrs_for>-bukrs TRANSPORTING NO FIELDS.
          IF syst-subrc IS INITIAL.
            APPEND INITIAL LINE TO <ex_s_return>-object_msg ASSIGNING <return_msg>.
            " 'Company code &1 is already available in the customer'
            MESSAGE ID 'CVI_EI' TYPE 'W' NUMBER '026'
                WITH <fs_bukrs_for>-bukrs INTO lv_msgdummy.
            <return_msg>-id         = sy-msgid.
            <return_msg>-type       = sy-msgty.
            <return_msg>-number     = sy-msgno.
            <return_msg>-message_v1 = sy-msgv1.
            <return_msg>-message    = lv_msgdummy.

            CONTINUE. "Continue with the next loop pass.
          ENDIF.

          APPEND INITIAL LINE TO <fs_bp_data>-bukrs_extended
            ASSIGNING FIELD-SYMBOL(<bukrs_extended>).

          MOVE <fs_bukrs_for>-bukrs TO:
                <bukrs_extended>-bukrs,
                ls_company-data_key-bukrs.

          APPEND ls_company TO ls_cvis_ei_extern-customer-company_data-company.

        ENDLOOP.

      ENDIF.
**********************************************************************
*---------- E N D  C O M P A N Y   E X T E N S I O N ----------------*
**********************************************************************

**********************************************************************
*---------- S T A R T   S A L E S   E X T E N S I O N ---------------*
**********************************************************************
      IF me->vkorg_from IS NOT INITIAL.
        READ TABLE <fs_bp_data>-customer_cmds-sales_data-sales
          WITH KEY data_key-vkorg = me->vkorg_from
          INTO ls_sales.

        IF syst-subrc IS NOT INITIAL.
          " Error Purchasing model data not found

          MESSAGE e040
              WITH me->vkorg_from <fs_bp_data>-customer
              INTO lv_msgdummy.
          APPEND INITIAL LINE TO <ex_s_return>-object_msg ASSIGNING <return_msg>.
          <return_msg>-id         = sy-msgid.
          <return_msg>-type       = sy-msgty.
          <return_msg>-number     = sy-msgno.
          <return_msg>-message_v1 = sy-msgv1.
          <return_msg>-message    = lv_msgdummy.

        ELSE.

          ls_sales-task = co_task_insert.

          LOOP AT me->t_vkorg_for ASSIGNING FIELD-SYMBOL(<fs_vkorg_for>).

            " Check if already exists
            READ TABLE <fs_bp_data>-customer_cmds-sales_data-sales
                WITH KEY data_key-vkorg = <fs_vkorg_for>-vkorg TRANSPORTING NO FIELDS.
            IF syst-subrc IS INITIAL.
              APPEND INITIAL LINE TO <ex_s_return>-object_msg ASSIGNING <return_msg>.
              " A organização de compras &1 já existe no fornecedor &2
              MESSAGE w052 WITH <fs_vkorg_for>-vkorg  <fs_bp_data>-customer INTO lv_msgdummy.
              <return_msg>-id         = sy-msgid.
              <return_msg>-type       = sy-msgty.
              <return_msg>-number     = sy-msgno.
              <return_msg>-message_v1 = sy-msgv1.
              <return_msg>-message_v2 = sy-msgv2.
              <return_msg>-message    = lv_msgdummy.
              CONTINUE. " Continue with the next loop pass.
            ENDIF.

            APPEND INITIAL LINE TO <fs_bp_data>-vkorg_extended
              ASSIGNING FIELD-SYMBOL(<vkorg_extended>).

            MOVE <fs_vkorg_for>-vkorg TO:
                  <vkorg_extended>-vkorg,
                  ls_sales-data_key-vkorg.

            APPEND ls_sales TO ls_cvis_ei_extern-customer-sales_data-sales.
          ENDLOOP.

        ENDIF.
      ENDIF.
**********************************************************************
*------------- E N D  S A L E S  E X T E N S I O N ------------------*
**********************************************************************


      CALL METHOD cl_md_bp_maintain=>validate_single
        EXPORTING
          i_data        = ls_cvis_ei_extern
        IMPORTING
          et_return_map = lt_return_validate_single.

      " remove standard error message, it is not impediment.
      " Função PN &1 não existe para parceiro &2
      DELETE lt_return_validate_single WHERE
                                        id      = 'R11' AND
                                        number  = '657' AND
                                        type    = 'E'.
      "Indicar um valor para campo &1
      " Error is not possible BUGS!
      DELETE lt_return_validate_single WHERE
                                        id      = 'R11' AND
                                        number  = '401' AND
                                        type    = 'E'.

      LOOP AT lt_return_validate_single ASSIGNING FIELD-SYMBOL(<return_validade_single>).

        APPEND INITIAL LINE TO <ex_s_return>-object_msg ASSIGNING <return_msg>.
        MOVE-CORRESPONDING <return_validade_single> TO <return_msg>.

      ENDLOOP.


      APPEND ls_cvis_ei_extern TO lt_cvis_ei_extern_t.

**********************************************************************
*------------------ C A L L  M A I N T A I N ------------------------*
**********************************************************************
      CALL METHOD cl_md_bp_maintain=>maintain
        EXPORTING
          i_data   = lt_cvis_ei_extern_t        " Inbound for Customer/Vendor Integration
        IMPORTING
          e_return = lt_return_maintain.                 " BAPIRETI Table Type for Multiple Objects

      IF lt_return_maintain IS INITIAL AND
         im_b_test          IS INITIAL.

        CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
          EXPORTING
            wait = abap_on.                 " Use of Command `COMMIT AND WAIT`

      ELSE.
        CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.

        READ TABLE lt_return_maintain
            ASSIGNING FIELD-SYMBOL(<return_maintain>)
            WITH KEY object_key = <fs_bp_data>-partner_guid.

        IF <return_maintain> IS ASSIGNED AND sy-subrc IS INITIAL.
          APPEND LINES OF <return_maintain>-object_msg TO:
              <ex_s_return>-object_msg.
        ENDIF.

      ENDIF.

      APPEND LINES OF <ex_s_return>-object_msg TO <fs_bp_data>-object_msg.

    ENDLOOP. " AT me->t_bp_data

  ENDMETHOD.

  METHOD constructor.

    super->constructor( ).

    me->bukrs_from  = im_v_bukrs_from.
    me->vkorg_from  = im_v_vkorg_from.
    me->t_bukrs_for = im_t_bukrs_for.
    me->t_vkorg_for = im_t_vkorg_for.
    me->t_bp_data   = im_t_bp_data.

  ENDMETHOD.

ENDCLASS.
