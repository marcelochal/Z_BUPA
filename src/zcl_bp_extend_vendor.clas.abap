CLASS zcl_bp_extend_vendor DEFINITION
  PUBLIC
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES if_amdp_marker_hdb .
    INTERFACES if_cvi_common_constants .
    INTERFACES if_fsbp_generic_constants .
    INTERFACES zif_bp_standard .

    ALIASES co_task_current_state
      FOR if_cvi_common_constants~task_current_state .
    ALIASES co_task_insert
      FOR if_cvi_common_constants~task_insert .
    ALIASES co_task_modify
      FOR if_cvi_common_constants~task_modify .
    ALIASES co_task_update
      FOR if_cvi_common_constants~task_update .
    ALIASES ty_r_bukrs
      FOR zif_bp_standard~ty_r_bukrs .                                 " Company Code
    ALIASES ty_r_ekorg
      FOR zif_bp_standard~ty_r_ekorg .                                 " Purchasing organization
    ALIASES ty_r_lifnr
      FOR zif_bp_standard~ty_r_lifnr .                                 " Account Number of Vendor or Creditor
    ALIASES ty_r_partner
      FOR zif_bp_standard~ty_r_partner .
    ALIASES ty_s_vendor_lf_3
      FOR zif_bp_standard~ty_s_vendor_lf_3 .
    ALIASES ty_t_bp
      FOR zif_bp_standard~ty_t_bp .
    ALIASES ty_t_bp_key
      FOR zif_bp_standard~ty_t_bp_key .
    ALIASES ty_t_lfa1_text
      FOR zif_bp_standard~ty_t_lfa1_text .
    ALIASES ty_t_t001_key
      FOR zif_bp_standard~ty_t_t001_key .
    ALIASES ty_t_t024e_key
      FOR zif_bp_standard~ty_t_t024e_key .

    TYPES:
      tab TYPE STANDARD TABLE OF REF TO zcl_bp_extend_vendor WITH DEFAULT KEY .
    TYPES:
      BEGIN OF ty_s_bp_extend.
        INCLUDE TYPE zif_bp_standard~ty_s_bp.
    TYPES: bukrs_extended TYPE ty_t_t001_key,
           ekorg_extended TYPE ty_t_t024e_key,
           object_msg     TYPE bapiretct.
    TYPES: END OF ty_s_bp_extend .
    TYPES:
      ty_t_bp_extend TYPE STANDARD TABLE OF ty_s_bp_extend WITH DEFAULT KEY
                         WITH UNIQUE SORTED KEY     partner_sort_key  COMPONENTS partner  vendor
                         WITH NON-UNIQUE SORTED KEY vendor_sort_key   COMPONENTS vendor   partner .

*          im_t_lifnr   TYPE ty_r_lifnr   OPTIONAL
*          im_r_partner TYPE ty_r_partner OPTIONAL
    CLASS-METHODS get_bp_data_from_vendor
      IMPORTING
        !im_t_bp_key  TYPE ty_t_bp_key
      EXPORTING
        !ex_t_bp_data TYPE ty_t_bp
      RAISING
        zcx_abap_error .
    CLASS-METHODS get_bp_from_vendor
          AMDP OPTIONS READ-ONLY CDS SESSION CLIENT im_v_mandt
      IMPORTING
        VALUE(im_v_mandt)        TYPE syst_mandt
        VALUE(im_v_vendor)       TYPE lifnr
      EXPORTING
        VALUE(ex_v_partner)      TYPE bu_partner_t
        VALUE(ex_v_partner_guid) TYPE bu_partner_guid .
    CLASS-METHODS get_field_list
      IMPORTING
        !im_r_data       TYPE REF TO data
      EXPORTING
        !ex_t_components TYPE cl_abap_structdescr=>component_table
      RETURNING
        VALUE(r_result)  TYPE ddfields .
    CLASS-METHODS change_task
      IMPORTING
        !im_v_task  TYPE bus_ei_object_task
      CHANGING
        !ch_t_table TYPE ANY TABLE OPTIONAL
        !ch_s_struc TYPE any OPTIONAL .
    METHODS constructor
      RAISING
        zcx_abap_error .
    METHODS extend_vendor
      IMPORTING
        !im_b_test   TYPE abap_bool OPTIONAL
      EXPORTING
        !ex_t_return TYPE bapiretm
      RAISING
        zcx_abap_error .
  PROTECTED SECTION.
    DATA:
      t_bp_data   TYPE ty_t_bp_extend,
      bukrs_from  TYPE bukrs,
      ekorg_from  TYPE ekorg,
      t_bukrs_for TYPE ty_t_t001_key,
      t_ekorg_for TYPE ty_t_t024e_key,
      o_prog_ind  TYPE REF TO zcl_progress_indicator.

    CLASS-METHODS:
      get_vendor_text
        IMPORTING
          im_v_tdname    TYPE tdobname OPTIONAL
          im_v_lifnr     TYPE lifnr    OPTIONAL
        EXPORTING
          ex_t_text_cvis TYPE cvis_ei_text_t
          ex_t_lfa1_text TYPE ty_t_lfa1_text
          ex_t_lfb1_text TYPE zif_bp_standard~ty_t_lfb1_text
          ex_t_lfm1_text TYPE zif_bp_standard~ty_t_lfm1_text
        RAISING
          zcx_abap_error.

  PRIVATE SECTION.


ENDCLASS.



CLASS zcl_bp_extend_vendor IMPLEMENTATION.


  METHOD change_task.
    CONSTANTS:
      co_flat_structure    TYPE abap_typekind VALUE cl_abap_typedescr=>typekind_struct1, "u
      co_deep_structure    TYPE abap_typekind VALUE cl_abap_typedescr=>typekind_struct2, "v
      co_internal_table    TYPE abap_typekind VALUE cl_abap_typedescr=>typekind_table,   "h
      co_field_task        TYPE fieldname VALUE 'TASK',
      co_field_object_task TYPE fieldname VALUE 'OBJECT_TASK'.

    DATA:
      lr_data  TYPE REF TO data,
      lt_dfies TYPE ddfields.

    IF     ch_t_table IS NOT INITIAL.
      LOOP AT ch_t_table ASSIGNING FIELD-SYMBOL(<fs_s_table>).
        CALL METHOD change_task
          EXPORTING
            im_v_task  = im_v_task
          CHANGING
            ch_s_struc = <fs_s_table>.
      ENDLOOP.

    ELSEIF ch_s_struc IS NOT INITIAL.
      CREATE DATA lr_data LIKE ch_s_struc.
      lt_dfies = get_field_list( lr_data ).

      LOOP AT lt_dfies ASSIGNING FIELD-SYMBOL(<fs_dfies>)
          WHERE ( inttype   EQ co_internal_table    OR
                  inttype   EQ co_deep_structure    OR
                  inttype   EQ co_flat_structure )  OR
                ( fieldname EQ co_field_task        OR
                  fieldname EQ co_field_object_task ).

        ASSIGN COMPONENT <fs_dfies>-fieldname
          OF STRUCTURE ch_s_struc
          TO FIELD-SYMBOL(<fs_field_data>).

        CHECK: syst-subrc IS INITIAL,
               <fs_field_data> IS ASSIGNED.

        CASE <fs_dfies>-inttype.
          WHEN co_internal_table.
            CALL METHOD change_task
              EXPORTING
                im_v_task  = im_v_task
              CHANGING
                ch_t_table = <fs_field_data>.

          WHEN co_deep_structure OR
               co_flat_structure.
            CALL METHOD change_task
              EXPORTING
                im_v_task  = im_v_task
              CHANGING
                ch_s_struc = <fs_field_data>.

          WHEN OTHERS.
            IF  <fs_dfies>-fieldname EQ 'TASK' OR
                <fs_dfies>-fieldname EQ 'OBJECT_TASK'.
              <fs_field_data> = co_task_current_state.
              EXIT.
            ENDIF.

        ENDCASE.

      ENDLOOP.
    ELSE.
      RETURN.
    ENDIF.

  ENDMETHOD.


  METHOD constructor.



  ENDMETHOD.


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
              WITH me->bukrs_from <fs_bp_data>-vendor
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


  METHOD get_bp_data_from_vendor.

    DATA:
      ls_get_all_bp   TYPE bus_ei_main,
      ls_bp_current   TYPE bus_ei_main,
      ls_error        TYPE mds_ctrls_error,
      ls_vendor_lf    TYPE zcl_bp_extend_vendor=>ty_s_vendor_lf_3,
      ls_t100key      TYPE scx_t100key,
      lo_progress_msg TYPE REF TO zcl_progress_indicator.

    CHECK im_t_bp_key IS NOT INITIAL.

    "Seleciona Fornecedores
    SELECT  but~partner
            cvi~vendor
            but~partner_guid
       FROM cvi_vend_link  AS cvi         " Assignment Between Vendor and Business Partner
       INNER JOIN but000   AS but ON cvi~partner_guid = but~partner_guid  " BP: General data I
       INTO CORRESPONDING FIELDS OF TABLE ex_t_bp_data
       FOR ALL ENTRIES IN im_t_bp_key
       WHERE cvi~vendor       = im_t_bp_key-vendor  OR
             but~partner      = im_t_bp_key-partner OR
             but~partner_guid = im_t_bp_key-partner_guid.
*      WHERE cvi~vendor  IN im_r_lifnr OR
*            but~partner IN im_r_partner.

    IF sy-subrc NE 0.
      MESSAGE e025 INTO DATA(lv_msg_dummy). " Não foi encontrado nenhum fornecedor válido
      ls_t100key-msgno = syst-msgno.
      ls_t100key-msgid = syst-msgid.
      RAISE EXCEPTION TYPE zcx_abap_error USING MESSAGE
        EXPORTING
          im_syst_at_raise = syst
          im_error         = lv_msg_dummy
          im_t100key       = ls_t100key.
    ENDIF.

    CREATE OBJECT lo_progress_msg
      EXPORTING
        im_v_total        = lines( ex_t_bp_data )
        im_v_text_default = |{ 'Selecionando informação de Fornecedores'(001) }|.

    CALL METHOD lo_progress_msg->show( ).

    LOOP AT ex_t_bp_data ASSIGNING FIELD-SYMBOL(<fs_bp>)
                         USING KEY partner_sort_key.

      APPEND INITIAL LINE TO ls_get_all_bp-partners
        ASSIGNING FIELD-SYMBOL(<fs_partners>).

      <fs_partners>-header-object_instance-bpartner     = <fs_bp>-partner.
      <fs_partners>-header-object_instance-bpartnerguid = <fs_bp>-partner_guid.

    ENDLOOP.

    CALL METHOD cl_bupa_current_data=>get_all(
      EXPORTING
        is_business_partners = ls_get_all_bp   " Complex External Interface of the Business Partner (Tab.)
      IMPORTING
        es_business_partners = ls_bp_current   " Complex External Interface of the Business Partner (Tab.)
        es_error             = ls_error ).     " Message Structure of the Controller

    CALL METHOD change_task
      EXPORTING
        im_v_task  = co_task_current_state
      CHANGING
        ch_t_table = ls_bp_current-partners.

    "Seleciona os fornecedores a extender
    SELECT * FROM lfa1
        INTO TABLE ls_vendor_lf-lfa1
        FOR ALL ENTRIES IN ex_t_bp_data
        WHERE lifnr = ex_t_bp_data-vendor.
*        WHERE lifnr IN im_r_lifnr.

*    "Seleciona dados de impostos dos fornecedores a extender
    SELECT * FROM lfbw              " Vendor master record (withholding tax types) X
        INTO TABLE @ls_vendor_lf-lfbw
        FOR ALL ENTRIES IN @ex_t_bp_data
        WHERE lifnr = @ex_t_bp_data-vendor.
*        WHERE lifnr IN @im_r_lifnr.

*    "Seleciona dados de impostos dos fornecedores a extender
    SELECT * FROM lfat              " Vendor master record (tax groupings)
        INTO TABLE ls_vendor_lf-lfat
        FOR ALL ENTRIES IN ex_t_bp_data
        WHERE lifnr = ex_t_bp_data-vendor.
*        WHERE lifnr IN im_r_lifnr.

    "Seleciona dados de todas as empresas do fornecedores a extender
    SELECT * FROM lfb1              " Vendor Master (Company Code)
        INTO TABLE ls_vendor_lf-lfb1
        FOR ALL ENTRIES IN ex_t_bp_data
        WHERE lifnr = ex_t_bp_data-vendor.
*        WHERE lifnr IN im_r_lifnr.

    "Seleciona dados de organização de compras dos fornecedores a extender
    SELECT * FROM lfm1              " Vendor master record purchasing organization data
        INTO TABLE ls_vendor_lf-lfm1
        FOR ALL ENTRIES IN ex_t_bp_data
        WHERE lifnr = ex_t_bp_data-vendor.
*        WHERE lifnr IN im_r_lifnr.

    "Seleciona dados de organização de compras dos fornecedores a extender
    SELECT * FROM lfm2              " Vendor Master Record: Purchasing Data
        INTO TABLE ls_vendor_lf-lfm2
        FOR ALL ENTRIES IN ex_t_bp_data
        WHERE lifnr = ex_t_bp_data-vendor.
*        WHERE lifnr IN im_r_lifnr.

    " Select Partner Functions
    SELECT * FROM wyt3
        INTO TABLE ls_vendor_lf-wyt3
        FOR ALL ENTRIES IN ex_t_bp_data
        WHERE lifnr = ex_t_bp_data-vendor.
*        WHERE lifnr IN im_r_lifnr.

    lo_progress_msg->reset_processed( ).
**********************************************************************
    LOOP AT ex_t_bp_data ASSIGNING <fs_bp> USING KEY vendor_sort_key.

      lo_progress_msg->show( ).

      READ TABLE ls_bp_current-partners ASSIGNING FIELD-SYMBOL(<fs_bp_current>)
         WITH KEY header-object_instance-bpartnerguid = <fs_bp>-partner_guid.

      MOVE-CORRESPONDING <fs_bp_current> TO <fs_bp>-partner_bus.

      " LFA1 Supplier Master (General Section)
      LOOP AT ls_vendor_lf-lfa1 ASSIGNING FIELD-SYMBOL(<fs_lfa1>)
                                USING KEY sort_key
                                WHERE lifnr EQ <fs_bp>-vendor.

        MOVE-CORRESPONDING:
            <fs_lfa1> TO <fs_bp>-vendor_lf-lfa1,
            <fs_lfa1> TO <fs_bp>-vendor_vmds-header-object_instance,
            <fs_lfa1> TO <fs_bp>-vendor_vmds-central_data-central-data.

        CALL METHOD get_vendor_text
          EXPORTING
            im_v_tdname    = |{ <fs_lfa1>-lifnr }|
          IMPORTING
            ex_t_text_cvis = <fs_bp>-vendor_vmds-central_data-text-texts
            ex_t_lfa1_text = <fs_bp>-vendor_lf-lfa1_text.

      ENDLOOP.

*=====================================================================
      " LFB1 Vendor Master (Company Code)
      LOOP AT ls_vendor_lf-lfb1 ASSIGNING FIELD-SYMBOL(<fs_lfb1>)
                                USING KEY sort_key
                                WHERE lifnr EQ <fs_bp>-vendor.

        APPEND INITIAL LINE TO <fs_bp>-vendor_vmds-company_data-company
                 ASSIGNING FIELD-SYMBOL(<fs_company>).

        MOVE-CORRESPONDING <fs_lfb1> TO:
            <fs_company>-data_key,
            <fs_company>-data.

        APPEND <fs_lfb1> TO <fs_bp>-vendor_lf-lfb1.

        CALL METHOD get_vendor_text
          EXPORTING
            im_v_tdname    = |{ <fs_lfb1>-lifnr }{ <fs_lfb1>-bukrs }|
          IMPORTING
            ex_t_text_cvis = <fs_company>-texts-texts.

        " LFBW Vendor master record (withholding tax types) X
        LOOP AT ls_vendor_lf-lfbw ASSIGNING FIELD-SYMBOL(<fs_lfbw>) USING KEY sort_key
                                                                    WHERE lifnr EQ <fs_bp>-vendor
                                                                      AND bukrs EQ <fs_lfb1>-bukrs.
          APPEND <fs_lfbw> TO <fs_bp>-vendor_lf-lfbw.
          APPEND INITIAL LINE TO <fs_company>-wtax_type-wtax_type
                 ASSIGNING FIELD-SYMBOL(<fs_wtax>).
          MOVE-CORRESPONDING <fs_lfbw> TO:
              <fs_wtax>-data_key,
              <fs_wtax>-data.

        ENDLOOP.

      ENDLOOP. " LFB1 Vendor Master (Company Code)
*=====================================================================

      " LFAT Vendor master record (tax groupings)
      LOOP AT ls_vendor_lf-lfat
        ASSIGNING FIELD-SYMBOL(<fs_lfat>) USING KEY sort_key
                                          WHERE lifnr EQ <fs_bp>-vendor.
        APPEND <fs_lfat> TO <fs_bp>-vendor_lf-lfat.
      ENDLOOP.


      " Vendor master record purchasing organization data
      LOOP AT ls_vendor_lf-lfm1
        ASSIGNING FIELD-SYMBOL(<fs_lfm1>) USING KEY sort_key
                                          WHERE lifnr EQ <fs_bp>-vendor.

        APPEND INITIAL LINE TO <fs_bp>-vendor_vmds-purchasing_data-purchasing
                 ASSIGNING FIELD-SYMBOL(<fs_purchasing>).
        APPEND <fs_lfm1> TO <fs_bp>-vendor_lf-lfm1.
        MOVE-CORRESPONDING <fs_lfm1> TO:
            <fs_purchasing>-data,
            <fs_purchasing>-data_key.

        CALL METHOD get_vendor_text
          EXPORTING
            im_v_tdname    = |{ <fs_lfm1>-lifnr }{ <fs_lfm1>-ekorg }|
          IMPORTING
            ex_t_text_cvis = <fs_purchasing>-texts-texts.

        MOVE-CORRESPONDING:
            <fs_purchasing>-texts-texts TO <fs_bp>-vendor_lf-lfm1_text.

        " Vendor Master Record: Purchasing Data 2
        LOOP AT ls_vendor_lf-lfm2
            ASSIGNING FIELD-SYMBOL(<fs_lfm2>) USING KEY sort_key
                                              WHERE lifnr EQ <fs_bp>-vendor AND
                                                    ekorg EQ <fs_lfm1>-ekorg.
          APPEND:
            <fs_lfm2> TO <fs_bp>-vendor_lf-lfm2,
            INITIAL LINE TO <fs_purchasing>-purchasing2-purchasing2 ASSIGNING FIELD-SYMBOL(<fs_purchasing2>).
          MOVE-CORRESPONDING <fs_lfm2> TO:
              <fs_purchasing2>-data,
              <fs_purchasing2>-data_key.
        ENDLOOP.

        " Partner Functions
        LOOP AT ls_vendor_lf-wyt3
            ASSIGNING FIELD-SYMBOL(<fs_wyt3>) USING KEY sort_key
                                              WHERE lifnr EQ <fs_bp>-vendor AND
                                                    ekorg EQ <fs_lfm1>-ekorg.
          APPEND:
            <fs_wyt3> TO <fs_bp>-vendor_lf-wyt3,
            INITIAL LINE TO <fs_purchasing>-functions-functions ASSIGNING FIELD-SYMBOL(<fs_functions>).
          MOVE-CORRESPONDING <fs_wyt3> TO:
              <fs_functions>-data,
              <fs_functions>-data_key.
        ENDLOOP. "  LOOP AT ls_vendor_lf-wyt3

      ENDLOOP. " LOOP AT ls_vendor_lf-lfm1

    ENDLOOP. " AT me->t_bp

  ENDMETHOD.


  METHOD get_bp_from_vendor  BY DATABASE PROCEDURE FOR HDB LANGUAGE SQLSCRIPT USING
   but000 cvi_vend_link.

    ex_v_partner = SELECT
                      but.partner
                       FROM cvi_vend_link  AS cvi
                       INNER JOIN but000   AS but     ON cvi.partner_guid = but.partner_guid
                      WHERE cvi.client = :im_v_mandt
                      AND   cvi.vendor = :im_v_vendor;

  ENDMETHOD.


  METHOD get_field_list.

    DATA:
      lr_strucdescr       TYPE REF TO cl_abap_structdescr.

    lr_strucdescr ?= cl_abap_structdescr=>describe_by_data_ref( p_data_ref = im_r_data ).

    r_result = cl_salv_data_descr=>read_structdescr( r_structdescr = lr_strucdescr ).

    ex_t_components = lr_strucdescr->get_components( ).

  ENDMETHOD.


  METHOD get_vendor_text.

    DATA:
      lt_stxh   TYPE TABLE OF stxh_key  WITH DEFAULT KEY,
      lt_lines  TYPE tline_tab,
      ls_header TYPE thead,
      ls_text   LIKE LINE OF ex_t_text_cvis.

    FIELD-SYMBOLS:
      <tdkey>   TYPE vmds_tdkey,
      <lines>   TYPE tline_tab,
      <lf_text> TYPE any.

    CLEAR: ex_t_lfa1_text, ex_t_lfb1_text,
           ex_t_lfm1_text, ex_t_text_cvis.

    CHECK im_v_tdname IS NOT INITIAL.

    SELECT  tdobject tdname tdid tdspras
         FROM stxh
         INTO TABLE lt_stxh
         WHERE tdobject LIKE 'LF%'   AND
               tdname   LIKE im_v_tdname.

    LOOP AT lt_stxh ASSIGNING FIELD-SYMBOL(<fs_stxh>).

      CALL FUNCTION 'READ_TEXT'
        EXPORTING
          client   = sy-mandt           " Client
          id       = <fs_stxh>-tdid     " Text ID of text to be read
          language = <fs_stxh>-tdspras  " Language of text to be read
          name     = <fs_stxh>-tdname   " Name of text to be read
          object   = <fs_stxh>-tdobject " Object of text to be read
        IMPORTING
          header   = ls_header          " Text header of text read
        TABLES
          lines    = lt_lines           " Lines of text read
        EXCEPTIONS
          OTHERS   = 1.

      IF sy-subrc IS INITIAL.
        MOVE:   lt_lines          TO ls_text-data,
                ls_header-tdspras TO ls_text-data_key-langu,
                ls_header-tdid    TO ls_text-data_key-text_id.
        APPEND  ls_text           TO ex_t_text_cvis.

        CASE <fs_stxh>-tdobject.
          WHEN 'LFA1'.
            APPEND INITIAL LINE TO ex_t_lfa1_text
                ASSIGNING FIELD-SYMBOL(<lfa1_text>).

            MOVE:
             ls_header-tdname(10)   TO <lfa1_text>-lifnr.
            ASSIGN <lfa1_text> TO <lf_text>.


          WHEN 'LFB1'.
            APPEND INITIAL LINE TO ex_t_lfb1_text
                ASSIGNING FIELD-SYMBOL(<lfb1_text>).

            MOVE:
             ls_header-tdname(10)   TO <lfb1_text>-lifnr,
             ls_header-tdname+10(4) TO <lfb1_text>-bukrs.

            ASSIGN <lfb1_text> TO <lf_text>.

          WHEN 'LFM1'.
            APPEND INITIAL LINE TO ex_t_lfm1_text
                ASSIGNING FIELD-SYMBOL(<lfm1_text>).

            MOVE:
             ls_header-tdname(10)   TO <lfm1_text>-lifnr,
             ls_header-tdname+10(4) TO <lfm1_text>-ekorg.
            ASSIGN <lfm1_text> TO <lf_text>.

        ENDCASE.

        IF <lf_text> IS ASSIGNED.
          ASSIGN COMPONENT:
            'LINES' OF STRUCTURE <lf_text> TO <lines>,
            'TDKEY' OF STRUCTURE <lf_text> TO <tdkey>.
        ENDIF.

        IF <tdkey> IS ASSIGNED AND
           <lines> IS ASSIGNED.
          MOVE:
              ls_header-tdobject  TO <tdkey>-object,
              ls_header-tdname    TO <tdkey>-name,
              ls_header-tdid      TO <tdkey>-id,
              ls_header-tdspras   TO <tdkey>-spras,
              lt_lines            TO <lines>.
        ENDIF.
      ENDIF.

    ENDLOOP.

  ENDMETHOD.
ENDCLASS.
