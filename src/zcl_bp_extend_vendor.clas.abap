"! <p class="shorttext synchronized" lang="en">Vendor / BP extension to company and purchasing organization</p>
"! <p class="shorttext synchronized" lang="pt">Vendor / BP extension to company and purchasing organization</p>
CLASS zcl_bp_extend_vendor DEFINITION INHERITING FROM zcl_bp_standard
  PUBLIC
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES zif_bp_extend_vendor.

    ALIASES: ty_s_bp_extend FOR zif_bp_extend_vendor~ty_s_bp_extend,
             ty_t_bp_extend FOR zif_bp_extend_vendor~ty_t_bp_extend.

    TYPES:
      tab TYPE STANDARD TABLE OF REF TO zcl_bp_extend_vendor WITH DEFAULT KEY .

    METHODS constructor
      IMPORTING
        im_v_bukrs_from TYPE bukrs OPTIONAL
        im_v_ekorg_from TYPE ekorg OPTIONAL
        im_t_bukrs_for  TYPE zif_bp_standard~ty_t_t001_key OPTIONAL
        im_t_ekorg_for  TYPE zif_bp_standard~ty_t_t024e_key OPTIONAL
        im_t_bp_data    TYPE ty_t_bp_extend OPTIONAL
      RAISING
        zcx_abap_error .

    "! <p class="shorttext synchronized" lang="en">Extend vendor company and sales data</p>
    "! <p class="shorttext synchronized" lang="pt">Extensão de fornecedores para empresas e vendas</p>
    "!
    "! @parameter im_b_test    | <p class="shorttext synchronized" lang="en">Test Flag</p>
    "! @parameter ex_t_return  | <p class="shorttext synchronized" lang="en">Return bapiretm</p>
    "! @raising zcx_abap_error | <p class="shorttext synchronized" lang="en">Error Class</p>
    METHODS extend_vendor
      IMPORTING
        !im_b_test   TYPE abap_bool OPTIONAL
      EXPORTING
        !ex_t_return TYPE bapiretm
      RAISING
        zcx_abap_error .


  PROTECTED SECTION.
    ALIASES:
        bukrs_from  FOR zif_bp_extend_vendor~bukrs_from,
        ekorg_from  FOR zif_bp_extend_vendor~ekorg_from,
        t_bukrs_for FOR zif_bp_extend_vendor~t_bukrs_for,
        t_ekorg_for FOR zif_bp_extend_vendor~t_ekorg_for,
        t_bp_data   FOR zif_bp_extend_vendor~t_bp_data.


  PRIVATE SECTION.


ENDCLASS.


CLASS zcl_bp_extend_vendor IMPLEMENTATION.

  METHOD extend_vendor.

    DATA:
      lt_return_maintain        TYPE bapiretm,
      lt_return_validate_single TYPE mdg_bs_bp_msgmap_t,
      lt_cvis_ei_extern_t       TYPE cvis_ei_extern_t,
      ls_cvis_ei_extern         TYPE cvis_ei_extern,
      ls_company                TYPE vmds_ei_company,
      ls_purchasing             TYPE vmds_ei_purchasing.

    IF me->o_prog_ind IS BOUND.
      me->o_prog_ind->set_total( im_total = lines( me->t_bp_data ) ).
    ELSE.
      CREATE OBJECT me->o_prog_ind
        EXPORTING
          im_v_total        = lines( me->t_bp_data )
          im_v_text_default = |{ 'Estendendo Fornecedores'(002) }|.
    ENDIF.

    LOOP AT me->t_bp_data ASSIGNING FIELD-SYMBOL(<fs_bp_data>) USING KEY vendor_sort_key.

      me->o_prog_ind->show( ).

      CLEAR: ls_cvis_ei_extern, ls_purchasing,
             ls_company, lt_return_validate_single,
             lt_cvis_ei_extern_t, lt_return_maintain.

      " Initialized return messages
      APPEND INITIAL LINE TO ex_t_return ASSIGNING FIELD-SYMBOL(<ex_s_return>).
      <ex_s_return>-object_idx = syst-tabix.
      <ex_s_return>-object_key = <fs_bp_data>-partner_guid.

      MOVE-CORRESPONDING:
        <fs_bp_data>-partner_bus        TO ls_cvis_ei_extern-partner,
        <fs_bp_data>-vendor_vmds-header TO ls_cvis_ei_extern-vendor-header.

      MOVE co_task_update TO:
        ls_cvis_ei_extern-partner-header-object_task,
        ls_cvis_ei_extern-vendor-header-object_task.

**********************************************************************
*-------- S T A R T  C O M P A N Y   E X T E N S I O N --------------*
**********************************************************************
      IF me->bukrs_from IS NOT INITIAL.

        READ TABLE <fs_bp_data>-vendor_vmds-company_data-company
          WITH KEY data_key-bukrs = me->bukrs_from
          INTO ls_company.

        IF syst-subrc IS NOT INITIAL.
          " Error Company model data not found
          MESSAGE e017 " Empresa &1 não é valida como modelo para o fornecedor &2
              WITH me->bukrs_from <fs_bp_data>-vendor
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

          " Check if already exists
          READ TABLE <fs_bp_data>-vendor_vmds-company_data-company
            WITH KEY data_key-bukrs = <fs_bukrs_for>-bukrs TRANSPORTING NO FIELDS.
          IF syst-subrc IS INITIAL.
            APPEND INITIAL LINE TO <ex_s_return>-object_msg ASSIGNING <return_msg>.
            MESSAGE ID 'CVI_EI' TYPE 'W' NUMBER '050'
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

          APPEND ls_company TO ls_cvis_ei_extern-vendor-company_data-company.
        ENDLOOP.
      ENDIF.
**********************************************************************
*---------- E N D  C O M P A N Y   E X T E N S I O N ----------------*
**********************************************************************

**********************************************************************
*----- S T A R T   P U R C H A S I N G   E X T E N S I O N ----------*
**********************************************************************
      IF me->ekorg_from IS NOT INITIAL.
        READ TABLE <fs_bp_data>-vendor_vmds-purchasing_data-purchasing
          WITH KEY data_key-ekorg = me->ekorg_from
          INTO ls_purchasing.

        IF syst-subrc IS NOT INITIAL.
          " Error Purchasing model data not found

          MESSAGE e012
              WITH me->ekorg_from <fs_bp_data>-vendor
              INTO lv_msgdummy.
          APPEND INITIAL LINE TO <ex_s_return>-object_msg ASSIGNING <return_msg>.
          <return_msg>-id         = sy-msgid.
          <return_msg>-type       = sy-msgty.
          <return_msg>-number     = sy-msgno.
          <return_msg>-message_v1 = sy-msgv1.
          <return_msg>-message    = lv_msgdummy.

        ELSE.

          ls_purchasing-task = co_task_insert.

          LOOP AT me->t_ekorg_for ASSIGNING FIELD-SYMBOL(<fs_ekorg_for>).

            " Check if already exists
            READ TABLE <fs_bp_data>-vendor_vmds-purchasing_data-purchasing
                WITH KEY data_key-ekorg = <fs_ekorg_for>-ekorg TRANSPORTING NO FIELDS.
            IF syst-subrc IS INITIAL.
              APPEND INITIAL LINE TO <ex_s_return>-object_msg ASSIGNING <return_msg>.
              " A organização de compras &1 já existe no fornecedor &2
              MESSAGE w027 WITH <fs_ekorg_for>-ekorg  <fs_bp_data>-vendor INTO lv_msgdummy.
              <return_msg>-id         = sy-msgid.
              <return_msg>-type       = sy-msgty.
              <return_msg>-number     = sy-msgno.
              <return_msg>-message_v1 = sy-msgv1.
              <return_msg>-message_v2 = sy-msgv2.
              <return_msg>-message    = lv_msgdummy.
              CONTINUE. " Continue with the next loop pass.
            ENDIF.

            APPEND INITIAL LINE TO <fs_bp_data>-ekorg_extended
              ASSIGNING FIELD-SYMBOL(<ekorg_extended>).

            MOVE <fs_ekorg_for>-ekorg TO:
                  <ekorg_extended>-ekorg,
                  ls_purchasing-data_key-ekorg.

            APPEND ls_purchasing TO ls_cvis_ei_extern-vendor-purchasing_data-purchasing.
          ENDLOOP.

        ENDIF.
      ENDIF.
**********************************************************************
*------- E N D  P U R C H A S I N G   E X T E N S I O N -------------*
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
    me->ekorg_from  = im_v_ekorg_from.
    me->t_bukrs_for = im_t_bukrs_for.
    me->t_ekorg_for = im_t_ekorg_for.
    me->t_bp_data   = im_t_bp_data.

  ENDMETHOD.

ENDCLASS.
