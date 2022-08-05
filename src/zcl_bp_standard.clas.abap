"! <p class="shorttext synchronized" lang="pt">Rotinas padrão para o BP</p>
"! <p class="shorttext synchronized" lang="EN">Standard Routines for BP</p>
CLASS zcl_bp_standard DEFINITION
  PUBLIC
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES zif_bp_standard .

    ALIASES:
        " Types
        get_field_list          FOR zif_bp_standard~get_field_list,
        ty_r_bukrs              FOR zif_bp_standard~ty_r_bukrs,      " Company Code
        ty_r_ekorg              FOR zif_bp_standard~ty_r_ekorg,      " Purchasing organization
        ty_r_vkorg              FOR zif_bp_standard~ty_r_vkorg,      " Purchasing organization
        ty_r_kunnr              FOR zif_bp_standard~ty_r_kunnr,      " Account Number of Costumer
        ty_r_lifnr              FOR zif_bp_standard~ty_r_lifnr,      " Account Number of Vendor or Creditor
        ty_r_partner            FOR zif_bp_standard~ty_r_partner,
        ty_s_vendor_lf_3        FOR zif_bp_standard~ty_s_vendor_lf_3,
        ty_t_bp                 FOR zif_bp_standard~ty_t_bp,
        ty_s_bp_key             FOR zif_bp_standard~ty_s_bp_key,
        ty_t_bp_key             FOR zif_bp_standard~ty_t_bp_key,
        ty_t_lfa1_text          FOR zif_bp_standard~ty_t_lfa1_text,
        ty_t_t001_key           FOR zif_bp_standard~ty_t_t001_key,
        ty_t_t024e_key          FOR zif_bp_standard~ty_t_t024e_key,
        ty_t_tvko               FOR zif_bp_standard~ty_t_tvko,      " Org. Unit Sales Organizations

        " Constants
        co_task_current_state   FOR if_cvi_common_constants~task_current_state,
        co_task_insert          FOR if_cvi_common_constants~task_insert ,
        co_task_modify          FOR if_cvi_common_constants~task_modify ,
        co_task_update          FOR if_cvi_common_constants~task_update ,
        co_task                 FOR zif_bp_standard~co_task,
        co_category_bp_as       FOR zif_bp_standard~co_category_bp_as,
        co_rolecategory         FOR zif_bp_standard~co_rolecategory,
        co_taxtype              FOR zif_bp_standard~co_taxtype,
        co_grouping             FOR zif_bp_standard~co_grouping,
        co_vendor_planning_group FOR zif_bp_standard~co_vendor_planning_group,
        co_country_iso_br       FOR zif_bp_standard~co_country_iso_br,
        co_languiso_pt          FOR zif_bp_standard~co_languiso_pt,

        " Methods
        change_task             FOR zif_bp_standard~change_task,
        get_bp_from_taxnumber   FOR zif_bp_standard~get_bp_from_taxnumber,
        get_bp_from_vendor      FOR zif_bp_standard~get_bp_from_vendor,
        create_bp_partner_guid  FOR zif_bp_standard~create_bp_partner_guid,
        get_first_name          FOR zif_bp_standard~get_first_name,
        get_last_name           FOR zif_bp_standard~get_last_name.


    CLASS-METHODS get_bp_data_from_vendor
      IMPORTING
        im_t_bp_key  TYPE ty_t_bp_key
      EXPORTING
        ex_t_bp_data TYPE ty_t_bp
      RAISING
        zcx_abap_error .

    CLASS-METHODS get_bp_data_from_customer
      IMPORTING
        im_t_bp_key  TYPE ty_t_bp_key
      EXPORTING
        ex_t_bp_data TYPE ty_t_bp
      RAISING
        zcx_abap_error .

    METHODS constructor.


  PROTECTED SECTION.
    ALIASES: o_prog_ind FOR zif_bp_standard~o_prog_ind.

    "! Get Vendor Texts
    "! @parameter im_v_tdname | TDname
    "! @parameter im_v_lifnr |Vendor number
    "! @parameter ex_t_text_cvis |
    "! LFA1 Text @parameter ex_t_lfa1_text |
    "! LFB1 Text @parameter ex_t_lfb1_text |
    "! LFM1 Text @parameter ex_t_lfm1_text |
    "! Class Error @raising zcx_abap_error |
    CLASS-METHODS get_vendor_text
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


    "! Get Customer Text
    "! Name @parameter im_v_tdname |
    "! Customer Number  @parameter im_v_kunnr |
    "! CVI Texts @parameter ex_t_text_cvis |
    "! KNA1 Texts @parameter ex_t_kna1_text |
    "! KNB1 Texts @parameter ex_t_knb1_text |
    "! Knvv Texts @parameter ex_t_knvv_text |
    "! Classe Error @raising zcx_abap_error |
    CLASS-METHODS get_customer_text
      IMPORTING
        im_v_tdname    TYPE tdobname OPTIONAL
        im_v_kunnr     TYPE kunnr    OPTIONAL
      EXPORTING
        ex_t_text_cvis TYPE cvis_ei_text_t
        ex_t_kna1_text TYPE zif_bp_standard~ty_t_kna1_text
        ex_t_knb1_text TYPE zif_bp_standard~ty_t_knb1_text
        ex_t_knvv_text TYPE zif_bp_standard~ty_t_knvv_text
      RAISING
        zcx_abap_error.

  PRIVATE SECTION.


ENDCLASS.



CLASS zcl_bp_standard IMPLEMENTATION.

  METHOD constructor.


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

  METHOD get_customer_text.

    DATA:
      lt_stxh   TYPE TABLE OF stxh_key  WITH DEFAULT KEY,
      lt_lines  TYPE tline_tab,
      ls_header TYPE thead,
      ls_text   LIKE LINE OF ex_t_text_cvis.

    FIELD-SYMBOLS:
      <tdkey>   TYPE vmds_tdkey,
      <lines>   TYPE tline_tab,
      <lf_text> TYPE any.

    CLEAR: ex_t_kna1_text, ex_t_knb1_text,
           ex_t_knvv_text, ex_t_text_cvis.

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
          WHEN 'KNA1'.
            APPEND INITIAL LINE TO ex_t_kna1_text
                ASSIGNING FIELD-SYMBOL(<kna1_text>).

            MOVE:
             ls_header-tdname(10)   TO <kna1_text>-kunnr.
            ASSIGN <kna1_text> TO <lf_text>.


          WHEN 'KNB1'.
            APPEND INITIAL LINE TO ex_t_knb1_text
                ASSIGNING FIELD-SYMBOL(<knb1_text>).

            MOVE:
             ls_header-tdname(10)   TO <knb1_text>-kunnr,
             ls_header-tdname+10(4) TO <knb1_text>-bukrs.

            ASSIGN <knb1_text> TO <lf_text>.

          WHEN 'KNVV'.
            APPEND INITIAL LINE TO ex_t_knvv_text
                ASSIGNING FIELD-SYMBOL(<knvv_text>).

            MOVE:
             ls_header-tdname(10)   TO <knvv_text>-kunnr,
             ls_header-tdname+10(4) TO <knvv_text>-vkorg.
            ASSIGN <knvv_text> TO <lf_text>.

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
      MESSAGE  e025(z_bupa) INTO DATA(lv_msg_dummy). " Não foi encontrado nenhum fornecedor válido
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

*=====================================================================
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

*=====================================================================
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

*=====================================================================
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

*=====================================================================
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

*=====================================================================
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

  METHOD get_bp_data_from_customer.

    DATA:
      ls_get_all_bp   TYPE bus_ei_main,
      ls_bp_current   TYPE bus_ei_main,
      ls_error        TYPE mds_ctrls_error,
      ls_customer_kn  TYPE zif_bp_standard~ty_s_customer_kn_3,
      ls_t100key      TYPE scx_t100key,
      lt_fields       TYPE rsz_t_string,
      lo_progress_msg TYPE REF TO zcl_progress_indicator.

    CHECK im_t_bp_key IS NOT INITIAL.

    "Seleciona Fornecedores
    SELECT  but~partner
            cvi~customer
            but~partner_guid
       FROM cvi_cust_link  AS cvi         " Assignment Between Vendor and Business Partner
       INNER JOIN but000   AS but ON cvi~partner_guid = but~partner_guid  " BP: General data I
       INTO CORRESPONDING FIELDS OF TABLE ex_t_bp_data
       FOR ALL ENTRIES IN im_t_bp_key
       WHERE cvi~customer     = im_t_bp_key-customer  OR
             but~partner      = im_t_bp_key-partner   OR
             but~partner_guid = im_t_bp_key-partner_guid.


    IF sy-subrc NE 0.
      MESSAGE  e051(z_bupa) INTO DATA(lv_msg_dummy). " Não foi encontrado nenhum cliente válido
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
        im_v_text_default = |{ 'Selecionando informação de Clientes'(002) }|.

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


    " General Data in Customer Master
    lt_fields = zcl_utils=>get_tab_fields( ls_customer_kn-kna1 ).
    SELECT (lt_fields) FROM kna1
        INTO TABLE @ls_customer_kn-kna1
        FOR ALL ENTRIES IN @ex_t_bp_data
        WHERE kunnr = @ex_t_bp_data-customer.


    " Customer master record (withholding tax types) X
    lt_fields = zcl_utils=>get_tab_fields( ls_customer_kn-knbw ).
    SELECT * FROM knbw              " Customer master record (withholding tax types) X
        INTO TABLE @ls_customer_kn-knbw
        FOR ALL ENTRIES IN @ex_t_bp_data
        WHERE kunnr = @ex_t_bp_data-customer.


    " Customer Master Record (Tax Groupings)
    lt_fields = zcl_utils=>get_tab_fields( ls_customer_kn-knat ).
    SELECT (lt_fields) FROM knat
        INTO TABLE @ls_customer_kn-knat
        FOR ALL ENTRIES IN @ex_t_bp_data
        WHERE kunnr = @ex_t_bp_data-customer.

    " Customer Master (Company Code)
    lt_fields = zcl_utils=>get_tab_fields( ls_customer_kn-knb1 ).
    SELECT (lt_fields) FROM knb1
        INTO TABLE @ls_customer_kn-knb1
        FOR ALL ENTRIES IN @ex_t_bp_data
        WHERE kunnr = @ex_t_bp_data-customer.

    " Customer Master Sales Data
    lt_fields = zcl_utils=>get_tab_fields( ls_customer_kn-knvv ).
    SELECT (lt_fields) FROM knvv
        INTO TABLE @ls_customer_kn-knvv
        FOR ALL ENTRIES IN @ex_t_bp_data
        WHERE kunnr = @ex_t_bp_data-customer.

    " Select Partner Functions
    lt_fields = zcl_utils=>get_tab_fields( ls_customer_kn-knvp ).
    SELECT (lt_fields) FROM knvp
        INTO TABLE @ls_customer_kn-knvp
        FOR ALL ENTRIES IN @ex_t_bp_data
        WHERE kunnr = @ex_t_bp_data-customer.

    " Select Partner Functions
    lt_fields = zcl_utils=>get_tab_fields( ls_customer_kn-zj1btxcli ).
    SELECT (lt_fields) FROM zj1btxcli
        INTO TABLE @ls_customer_kn-zj1btxcli
        FOR ALL ENTRIES IN @ex_t_bp_data
        WHERE kunnr = @ex_t_bp_data-customer.


    lo_progress_msg->reset_processed( ).
**********************************************************************
    LOOP AT ex_t_bp_data ASSIGNING <fs_bp> USING KEY customer_sort_key.

      lo_progress_msg->show( ).

      READ TABLE ls_bp_current-partners ASSIGNING FIELD-SYMBOL(<fs_bp_current>)
         WITH KEY header-object_instance-bpartnerguid = <fs_bp>-partner_guid.

      MOVE-CORRESPONDING <fs_bp_current> TO <fs_bp>-partner_bus.

      " KNA1 General Data in Customer Master
      LOOP AT ls_customer_kn-kna1 ASSIGNING FIELD-SYMBOL(<fs_kna1>)
                                USING KEY sort_key
                                WHERE kunnr EQ <fs_bp>-vendor.

        MOVE-CORRESPONDING:
            <fs_kna1> TO <fs_bp>-customer_kn-kna1,
            <fs_kna1> TO <fs_bp>-customer_cmds-header-object_instance,
            <fs_kna1> TO <fs_bp>-customer_cmds-central_data-central-data.

        CALL METHOD get_customer_text
          EXPORTING
            im_v_tdname    = |{ <fs_kna1>-kunnr }|
          IMPORTING
            ex_t_text_cvis = <fs_bp>-customer_cmds-central_data-text-texts
            ex_t_kna1_text = <fs_bp>-customer_kn-kna1_text.

      ENDLOOP.

*=====================================================================
      " KNB1 Customer Master (Company Code)
      LOOP AT ls_customer_kn-knb1 ASSIGNING FIELD-SYMBOL(<fs_knb1>)
                                USING KEY sort_key
                                WHERE kunnr EQ <fs_bp>-customer.

        APPEND INITIAL LINE TO <fs_bp>-customer_cmds-company_data-company
                 ASSIGNING FIELD-SYMBOL(<fs_company>).

        MOVE-CORRESPONDING <fs_knb1> TO:
            <fs_company>-data_key,
            <fs_company>-data.

        APPEND <fs_knb1> TO <fs_bp>-customer_kn-knb1.

        CALL METHOD get_customer_text
          EXPORTING
            im_v_tdname    = |{ <fs_knb1>-kunnr }{ <fs_knb1>-bukrs }|
          IMPORTING
            ex_t_text_cvis = <fs_company>-texts-texts.

*=====================================================================
        " KNBW Customer master record (withholding tax types) X
        LOOP AT ls_customer_kn-knbw ASSIGNING FIELD-SYMBOL(<fs_knbw>) USING KEY sort_key
                                                                    WHERE kunnr EQ <fs_bp>-customer
                                                                      AND bukrs EQ <fs_knb1>-bukrs.
          APPEND <fs_knbw> TO <fs_bp>-customer_kn-knbw.
          APPEND INITIAL LINE TO <fs_company>-wtax_type-wtax_type
                 ASSIGNING FIELD-SYMBOL(<fs_wtax>).
          MOVE-CORRESPONDING <fs_knbw> TO:
              <fs_wtax>-data_key,
              <fs_wtax>-data.

        ENDLOOP.
*=====================================================================
        " ZJ1BTXCLI Customer withholding tax types for AVC
        LOOP AT ls_customer_kn-zj1btxcli ASSIGNING FIELD-SYMBOL(<fs_zj1btxcli>) USING KEY sort_key
                                                                    WHERE kunnr EQ <fs_bp>-customer
                                                                      AND bukrs EQ <fs_knb1>-bukrs.
          APPEND <fs_zj1btxcli> TO <fs_bp>-customer_kn-zj1btxcli.

        ENDLOOP.


      ENDLOOP. " LFB1 Vendor Master (Company Code)
*=====================================================================

      " KNAT Customer master record (tax groupings)
      LOOP AT ls_customer_kn-knat
        ASSIGNING FIELD-SYMBOL(<fs_lfat>) USING KEY sort_key
                                          WHERE kunnr EQ <fs_bp>-customer.
        APPEND <fs_lfat> TO <fs_bp>-customer_kn-knat.
      ENDLOOP.


*=====================================================================

      " KNVV Customer master record purchasing organization data
      LOOP AT ls_customer_kn-knvv
        ASSIGNING FIELD-SYMBOL(<fs_knvv>) USING KEY sort_key
                                          WHERE kunnr EQ <fs_bp>-customer.

        APPEND INITIAL LINE TO <fs_bp>-customer_cmds-sales_data-sales
                 ASSIGNING FIELD-SYMBOL(<fs_sales>).

        APPEND <fs_knvv> TO <fs_bp>-customer_kn-knvv.
        MOVE-CORRESPONDING <fs_knvv> TO:
            <fs_sales>-data,
            <fs_sales>-data_key.

        CALL METHOD get_customer_text
          EXPORTING
            im_v_tdname    = |{ <fs_knvv>-kunnr }{ <fs_knvv>-vkorg }|
          IMPORTING
            ex_t_text_cvis = <fs_sales>-texts-texts.

        MOVE-CORRESPONDING:
            <fs_sales>-texts-texts TO <fs_bp>-customer_kn-knvv_text.

*=====================================================================
        " Partner Functions
        LOOP AT ls_customer_kn-knvp
            ASSIGNING FIELD-SYMBOL(<fs_knvp>) USING KEY sort_key
                                              WHERE kunnr EQ <fs_bp>-customer AND
                                                    vkorg EQ <fs_knvv>-vkorg.
          APPEND:
            <fs_knvp> TO <fs_bp>-customer_kn-knvp,
            INITIAL LINE TO <fs_sales>-functions-functions ASSIGNING FIELD-SYMBOL(<fs_functions>).
          MOVE-CORRESPONDING <fs_knvp> TO:
              <fs_functions>-data,
              <fs_functions>-data_key.
        ENDLOOP. " ls_customer_kn-knvp

      ENDLOOP. " LOOP AT ls_customer_kn-knvv

    ENDLOOP. " LOOP AT ex_t_bp_data

  ENDMETHOD.

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

  METHOD get_field_list.

    DATA:
      lr_strucdescr       TYPE REF TO cl_abap_structdescr.

    lr_strucdescr ?= cl_abap_structdescr=>describe_by_data_ref( p_data_ref = im_r_data ).

    r_result = cl_salv_data_descr=>read_structdescr( r_structdescr = lr_strucdescr ).

    ex_t_components = lr_strucdescr->get_components( ).

  ENDMETHOD.

  METHOD get_bp_from_vendor  BY DATABASE PROCEDURE FOR HDB LANGUAGE SQLSCRIPT USING
   but000 cvi_vend_link.

    ex_v_partner = select but.partner
                    from cvi_vend_link  as cvi
                    inner join but000   as but     on cvi.partner_guid = but.partner_guid
    where cvi.vendor = :im_v_vendor;

  ENDMETHOD.

  METHOD get_bp_from_taxnumber BY DATABASE PROCEDURE FOR HDB LANGUAGE SQLSCRIPT USING dfkkbptaxnum but000 cvi_vend_link cvi_cust_link.

    ch_t_bp = SELECT DISTINCT partner,
                              partner_guid,
                              taxnum,
                              vendor,
                              customer FROM (
                SELECT
                  CASE but.partner      WHEN ''    THEN ch_bp.partner       ELSE but.partner       END     AS partner,
                  CASE but.partner_guid WHEN ''    THEN ch_bp.partner_guid  ELSE but.partner_guid  END     AS partner_guid,
                  ch_bp.taxnum            AS taxnum,
                  CASE vend.vendor      WHEN ''    THEN ch_bp.vendor        ELSE vend.vendor       END     AS vendor,
                  CASE cust.customer    WHEN ''    THEN ch_bp.customer      ELSE cust.customer     END     AS customer
                  from :ch_t_bp           AS ch_bp
                  LEFT JOIN dfkkbptaxnum  AS taxnum   ON taxnum.taxnum     = ch_bp.taxnum
                  LEFT JOIN but000        AS but      ON taxnum.partner    = but.partner
                  LEFT JOIN cvi_vend_link AS vend     ON vend.partner_guid = but.partner_guid
                  LEFT JOIN cvi_cust_link AS cust     ON cust.partner_guid = but.partner_guid
);


  ENDMETHOD.

  METHOD create_bp_partner_guid .
    TRY.
        CALL METHOD cl_system_uuid=>create_uuid_x16_static
          RECEIVING
            uuid = r_result.
      CATCH cx_uuid_error ##NO_HANDLER. " Error Class for UUID Processing Errors
    ENDTRY.
  ENDMETHOD.

  METHOD get_last_name.
    CHECK im_v_full_name IS NOT INITIAL.

    SPLIT im_v_full_name AT space INTO TABLE DATA(lt_names).
    READ TABLE lt_names INTO r_last_name INDEX lines( lt_names ).
  ENDMETHOD.

  METHOD get_first_name.
    CHECK im_v_full_name IS NOT INITIAL.

    SPLIT im_v_full_name AT space INTO TABLE DATA(lt_names).
    DELETE lt_names INDEX lines( lt_names ).
    CONCATENATE LINES OF lt_names INTO r_first_name SEPARATED BY space.
  ENDMETHOD.


ENDCLASS.
