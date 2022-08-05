*--------------------------------------------------------------------*
*           P R O J E C T    A G I R - B R A Z I L                   *
*--------------------------------------------------------------------*
* Consultoria .....: I  N  T  E  C  H    P  R  O                     *
* Res. ABAP   .....: Sérgio Aires                                    *
* Res. FUNCIONAL...: José Carlos                                     *
* Módulo...........: FI                                              *
* Program     .....: ZFIC0013                                        *
* Transaction .....: ZFIC0013                                        *
* Tipo de prg .....: REPORT                                          *
* Objetivo    .....: Estender Fornecedor p/ Empresas & Org de Compras*
* Request     .....: TBDK919598                                      *
*--------------------------------------------------------------------*
* Change Control:                                                    *
* Version  Date       Who                        What                *
*    1.00  28/06/2008  Sérgio Aires              Versão Inicial      *
*    2.00  24/07/2018  Ricardo Monte             Projeto Agir        *
*          27/08/2018  Alexandre Bach            Projeto Agir        *
*    2.10  21/05/2019  Marcelo Alvares           Chamado INC0094502  *
*--------------------------------------------------------------------*
REPORT zfic0013
      MESSAGE-ID z_bupa
      NO STANDARD PAGE HEADING LINE-SIZE 255.
TABLES:
  lfb1, lfm1, but000.

**********************************************************************
CLASS:
    lcl_fic0013 DEFINITION DEFERRED.

**********************************************************************
DATA :
  go_extend_bp TYPE REF TO lcl_fic0013,
  gox_error    TYPE REF TO zcx_abap_error.

**********************************************************************
*>>>>>>>>>>>>> S E L E C T I O N - S C R E E N <<<<<<<<<<<<<<<<<<<<<<<
**********************************************************************
SELECTION-SCREEN BEGIN OF BLOCK bl1 WITH FRAME TITLE TEXT-t01.

SELECTION-SCREEN BEGIN OF BLOCK bl2 WITH FRAME TITLE TEXT-t02.

SELECT-OPTIONS: so_lifnr FOR lfb1-lifnr,
                so_bupa  FOR but000-partner.
SELECTION-SCREEN END OF BLOCK bl2.

" From 'Seleção de dados de origem'
SELECTION-SCREEN BEGIN OF BLOCK bl3 WITH FRAME TITLE TEXT-t03.
PARAMETERS:     p_bukrs   LIKE lfb1-bukrs.
PARAMETERS:     p_ekorg   LIKE lfm1-ekorg.
SELECTION-SCREEN END OF BLOCK bl3.

" For 'Determinação das estruturas organizacionais de destino'
SELECTION-SCREEN BEGIN OF BLOCK bl4 WITH FRAME TITLE TEXT-t04.
SELECT-OPTIONS:  so_bukrs  FOR  lfb1-bukrs,
                 so_ekorg  FOR  lfm1-ekorg.
SELECTION-SCREEN END OF BLOCK bl4.
PARAMETERS:     p_test    TYPE xtest AS CHECKBOX DEFAULT abap_on.

SELECTION-SCREEN END OF BLOCK bl1.


*&---------------------------------------------------------------------*
*& Class definition lcl_bupa
*&---------------------------------------------------------------------*
CLASS lcl_fic0013 DEFINITION INHERITING FROM zcl_bp_extend_vendor FINAL .
  PUBLIC SECTION.
    TYPES:
      ty_v_bukrs_from TYPE bukrs,   " Company Code
      ty_v_ekorg_from TYPE ekorg,   " Purchasing organization

      BEGIN OF ty_s_alv,
        partner TYPE bu_partner,
        vendor  TYPE lifnr,
        status  TYPE tp_icon,
        message TYPE bapi_msg,
        type    TYPE bapi_mtype,
        number  TYPE symsgno,
      END OF ty_s_alv,
      ty_t_alv TYPE TABLE OF ty_s_alv.

    CLASS-METHODS:
      check_field_bukrs
        IMPORTING
          im_v_bukrs_from TYPE ty_v_bukrs_from OPTIONAL
          im_r_bukrs_for  TYPE ty_r_bukrs      OPTIONAL
            PREFERRED PARAMETER im_v_bukrs_from,
      check_field_ekorg
        IMPORTING
          im_v_ekorg_from TYPE ty_v_ekorg_from OPTIONAL
          im_r_ekorg_for  TYPE ty_r_ekorg      OPTIONAL
            PREFERRED PARAMETER im_v_ekorg_from,
      check_field_lifnr,
      check_field_bupa,
      check_field_block_bl2
        IMPORTING
          im_b_onli     TYPE abap_bool OPTIONAL
        RETURNING
          VALUE(r_erro) TYPE abap_bool,
      check_field_block_bl3
        IMPORTING
          im_b_onli     TYPE abap_bool OPTIONAL
        RETURNING
          VALUE(r_erro) TYPE abap_bool,
      check_field_block_bl4
        IMPORTING
          im_b_onli     TYPE abap_bool OPTIONAL
        RETURNING
          VALUE(r_erro) TYPE abap_bool.

    METHODS:
      constructor
        IMPORTING
          im_v_ekorg_from TYPE ty_v_ekorg_from
          im_v_bukrs_from TYPE ty_v_bukrs_from
          im_r_ekorg_for  TYPE ty_r_ekorg       OPTIONAL
          im_r_bukrs_for  TYPE ty_r_bukrs       OPTIONAL
          im_r_lifnr      TYPE ty_r_lifnr       OPTIONAL
          im_r_partner    TYPE ty_r_partner     OPTIONAL
        RAISING
          zcx_abap_error,
      get_bukrs_from RETURNING VALUE(r_result)  TYPE ty_v_bukrs_from,
      set_bukrs_from IMPORTING im_bukrs_from    TYPE ty_v_bukrs_from,
      get_ekorg_from RETURNING VALUE(r_result)  TYPE ty_v_ekorg_from,
      set_ekorg_from IMPORTING im_ekorg_from    TYPE ty_v_ekorg_from,
      extend_vendor REDEFINITION,
      check_extensions,
      show_messages.

  PROTECTED SECTION.
  PRIVATE SECTION.
    DATA:
      r_lifnr     TYPE ty_r_lifnr,
      r_ekorg_for TYPE ty_r_ekorg,
      r_bukrs_for TYPE ty_r_bukrs,
      o_error     TYPE REF TO zcx_abap_error,
      t_alv       TYPE ty_t_alv.

    METHODS:
      on_link_click FOR EVENT link_click OF cl_salv_events_table
        IMPORTING
          row
          column.

ENDCLASS.

**********************************************************************
*>>>>>>>>>>>>>>>>>>>>>>>>>AT SELECTION-SCREEN<<<<<<<<<<<<<<<<<<<<<<<<<
**********************************************************************
AT SELECTION-SCREEN ON so_lifnr.
  CALL METHOD lcl_fic0013=>check_field_lifnr.
**********************************************************************
AT SELECTION-SCREEN ON so_bupa.
  CALL METHOD lcl_fic0013=>check_field_bupa.
**********************************************************************
AT SELECTION-SCREEN ON p_bukrs.
  IF so_bukrs IS NOT INITIAL AND p_bukrs IS INITIAL.
    " Empresa de destino preenchida. Favor indicar empresa de origem.
    MESSAGE s018 DISPLAY LIKE 'E'. STOP.
  ENDIF.

  CALL METHOD lcl_fic0013=>check_field_bukrs( p_bukrs ).
**********************************************************************
AT SELECTION-SCREEN ON p_ekorg.
  IF so_ekorg IS NOT INITIAL AND p_ekorg IS INITIAL.
    " Org. Compras de destino preenchida. Favor indicar a de origem.
    MESSAGE s014 DISPLAY LIKE 'E'. STOP.
  ENDIF.
  CALL METHOD lcl_fic0013=>check_field_ekorg( p_ekorg ).
**********************************************************************
AT SELECTION-SCREEN ON so_bukrs.
  IF so_bukrs IS INITIAL AND p_bukrs IS NOT INITIAL.
    " Empresa de origem preenchida. Favor indicar empresas de destino.
    MESSAGE s019 DISPLAY LIKE 'E'. STOP.
  ENDIF.
  CALL METHOD lcl_fic0013=>check_field_bukrs(
      im_v_bukrs_from = p_bukrs
      im_r_bukrs_for  = so_bukrs[] ).
**********************************************************************
AT SELECTION-SCREEN ON so_ekorg .
  IF so_ekorg IS INITIAL AND p_ekorg IS NOT INITIAL.
    " Org. Compras de origem preenchida. Favor indicar a de destino.
    MESSAGE s015 DISPLAY LIKE 'E'. STOP.
  ENDIF.
  CALL METHOD lcl_fic0013=>check_field_ekorg(
    EXPORTING
      im_v_ekorg_from = p_ekorg
      im_r_ekorg_for  = so_ekorg[] ).
**********************************************************************
AT SELECTION-SCREEN ON BLOCK bl2.
  CALL METHOD lcl_fic0013=>check_field_block_bl2.
**********************************************************************
AT SELECTION-SCREEN ON BLOCK bl3.
  CALL METHOD lcl_fic0013=>check_field_block_bl3.
**********************************************************************
AT SELECTION-SCREEN ON BLOCK bl4.
  CALL METHOD lcl_fic0013=>check_field_block_bl4.
**********************************************************************

  "------- START-OF-SELECION -----------------------------------------

START-OF-SELECTION.

  IF lcl_fic0013=>check_field_block_bl2( abap_true ). STOP. ENDIF.
  IF lcl_fic0013=>check_field_block_bl3( abap_true ). STOP. ENDIF.
  IF lcl_fic0013=>check_field_block_bl4( abap_true ). STOP. ENDIF.

  TRY.
      CREATE OBJECT go_extend_bp
        EXPORTING
          im_v_ekorg_from = p_ekorg
          im_v_bukrs_from = p_bukrs
          im_r_ekorg_for  = so_ekorg[]
          im_r_bukrs_for  = so_bukrs[]
          im_r_lifnr      = so_lifnr[]
          im_r_partner    = so_bupa[].

      CALL METHOD go_extend_bp->extend_vendor( im_b_test = p_test ).

      CALL METHOD go_extend_bp->check_extensions( ).

      CALL METHOD go_extend_bp->show_messages( ).

    CATCH zcx_abap_error INTO gox_error. " Exception Class with its own Constructor

      CALL METHOD gox_error->message_show( im_v_display_like = 'E' ).
      STOP.

  ENDTRY.

  "------- END-OF-SELECION -----------------------------------------

*END-OF-SELECTION.   " Obsolete Syntax


CLASS lcl_fic0013 IMPLEMENTATION .

  METHOD constructor.

    DATA:
      lt_bp_key    TYPE ty_t_bp_key,
      lt_bp_data   TYPE ty_t_bp,
      lt_r_lifnr   TYPE ty_r_lifnr,
      lt_r_partner TYPE ty_r_partner,

*Indra – INC0258963 – 27/05/2022 – Início
      lr_fupn      TYPE RANGE OF bu_role,
      ls_fupn      LIKE LINE OF lr_fupn.
*Indra – INC0258963 – 27/05/2022 – Fim

    super->constructor( ).

    TRY.

        CREATE OBJECT me->o_prog_ind
          EXPORTING
            im_v_text_default = |{ 'Realizando extensões.'(010) }|.

        " key lifnr : lifnr not null;
        " Avoid select problems
        lt_r_lifnr = im_r_lifnr.
        IF lt_r_lifnr IS INITIAL.
          APPEND INITIAL LINE TO lt_r_lifnr
          ASSIGNING FIELD-SYMBOL(<fs_r_lifnr>).
          <fs_r_lifnr>-sign   = 'I'.
          <fs_r_lifnr>-option = 'EQ'.
        ENDIF.

        " BUT000-partner key partner : bu_partner not null;
        " Avoid select problems
        lt_r_partner = im_r_partner.
        IF lt_r_partner IS INITIAL.
          APPEND INITIAL LINE TO lt_r_partner ASSIGNING FIELD-SYMBOL(<fs_r_partner>).
          <fs_r_partner>-sign   = 'I'.
          <fs_r_partner>-option = 'EQ'.
        ENDIF.

        MOVE:
            im_v_bukrs_from TO me->bukrs_from,
            im_v_ekorg_from TO me->ekorg_from.

        MOVE-CORRESPONDING:
            im_r_bukrs_for  TO me->r_bukrs_for,
            im_r_ekorg_for  TO me->r_ekorg_for,
            lt_r_lifnr      TO me->r_lifnr.

        IF me->r_bukrs_for IS NOT INITIAL.
          SELECT bukrs FROM t001
          INTO TABLE me->t_bukrs_for
          WHERE bukrs IN me->r_bukrs_for.

          IF sy-subrc IS NOT INITIAL.
            " Não foi selecionado nenhuma empresa válida para extender.
            MESSAGE e026 INTO DATA(lv_msgdummy).
            RAISE EXCEPTION TYPE zcx_abap_error USING MESSAGE
              EXPORTING
                im_error = lv_msgdummy.
          ENDIF.
        ENDIF.

        IF me->r_ekorg_for IS NOT INITIAL.
          SELECT ekorg FROM t024e
          INTO TABLE me->t_ekorg_for
          WHERE ekorg IN me->r_ekorg_for.
          IF sy-subrc IS NOT INITIAL.
            " Nenhuma Organização de compras válida para extender.
            MESSAGE e023 INTO lv_msgdummy.
            RAISE EXCEPTION TYPE zcx_abap_error USING MESSAGE
              EXPORTING
                im_error = lv_msgdummy.


*MESSAGE ID     sy-msgid
*            TYPE   sy-msgty
*            NUMBER sy-msgno
*            WITH   sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4
          ENDIF.
        ENDIF.

        me->o_prog_ind->show( im_v_text = |{ 'Selecionando Fornecedores.'(011) }| ).

        "Seleciona Fornecedores
        SELECT  but~partner
                cvi~vendor
                but~partner_guid
           FROM cvi_vend_link  AS cvi         " Assignment Between Vendor and Business Partner
           INNER JOIN but000   AS but ON cvi~partner_guid = but~partner_guid  " BP: General data I
           INTO CORRESPONDING FIELDS OF TABLE lt_bp_key
           WHERE cvi~vendor  IN me->r_lifnr OR
                 but~partner IN lt_r_partner.

        CALL METHOD me->get_bp_data_from_vendor
          EXPORTING
            im_t_bp_key  = lt_bp_key
          IMPORTING
            ex_t_bp_data = lt_bp_data.

*Indra – INC0258963 – 27/05/2022 – Início
        ls_fupn-sign = 'I'.
        ls_fupn-option = 'EQ'.
        ls_fupn-low = 'FLVN00'.
        APPEND ls_fupn TO lr_fupn.

        ls_fupn-low = 'FLVN01'.
        APPEND ls_fupn TO lr_fupn.
        CLEAR ls_fupn.

        LOOP AT lt_bp_data ASSIGNING FIELD-SYMBOL(<lfs_bp>).
          CLEAR <lfs_bp>-partner_bus-central_data-role-current_state.
          LOOP AT <lfs_bp>-partner_bus-central_data-role-roles INTO DATA(ls_role).
            IF ls_role-data_key NOT IN lr_fupn.
              DELETE <lfs_bp>-partner_bus-central_data-role-roles INDEX sy-tabix.
            ENDIF.
          ENDLOOP.

          CLEAR ls_role.
        ENDLOOP.
        UNASSIGN <lfs_bp>.
*Indra – INC0258963 – 27/05/2022 – Início

        MOVE-CORRESPONDING lt_bp_data TO me->t_bp_data.

      CATCH zcx_abap_error INTO me->o_error.
        " Exception Class with its own Constructor

        RAISE EXCEPTION TYPE zcx_abap_error USING MESSAGE
          EXPORTING
            im_textid        = me->o_error->textid
            im_syst_at_raise = me->o_error->syst_at_raise
            im_error         = me->o_error->error.

    ENDTRY.

  ENDMETHOD.

  METHOD get_bukrs_from.
    r_result = me->bukrs_from.
  ENDMETHOD.

  METHOD set_bukrs_from.
    me->bukrs_from = im_bukrs_from.
  ENDMETHOD.

  METHOD get_ekorg_from.
    r_result = me->ekorg_from.
  ENDMETHOD.

  METHOD set_ekorg_from.
    me->ekorg_from = im_ekorg_from.
  ENDMETHOD.

  METHOD check_field_bukrs.
    DATA:
      lv_bukrs    TYPE bukrs,
      lr_bukrs    TYPE RANGE OF bukrs,
      lr_im_bukrs TYPE ty_r_bukrs.

    IF im_r_bukrs_for IS NOT INITIAL.
      lr_im_bukrs = im_r_bukrs_for.
    ELSEIF im_v_bukrs_from IS NOT INITIAL.
      APPEND INITIAL LINE TO lr_im_bukrs ASSIGNING FIELD-SYMBOL(<r_bukrs>).
      <r_bukrs>-sign   = 'I'.
      <r_bukrs>-option = 'EQ'.
      <r_bukrs>-low    = im_v_bukrs_from.
    ELSE.
      RETURN.
    ENDIF.

    LOOP AT lr_im_bukrs ASSIGNING <r_bukrs>.
      CLEAR: lr_bukrs.
      APPEND <r_bukrs> TO lr_bukrs.
      SELECT SINGLE bukrs FROM t001 " Company Codes
          INTO lv_bukrs
      WHERE bukrs IN lr_bukrs.
      IF syst-subrc IS NOT INITIAL.
        MESSAGE ID 'F2' TYPE 'E' NUMBER '219' " 'Company code & is not defined'
            WITH <r_bukrs>-low.

        " Check if BUKRS FOR is equal BUKRS FROM.
      ELSEIF im_r_bukrs_for  IS NOT INITIAL AND
             lv_bukrs EQ im_v_bukrs_from.
        MESSAGE e016 WITH lv_bukrs im_v_bukrs_from.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.

  METHOD check_field_ekorg.
    DATA:
      lv_ekorg    TYPE ekorg,
      lr_ekorg    TYPE ty_r_ekorg,
      lr_im_ekorg TYPE ty_r_ekorg.

    IF im_r_ekorg_for IS NOT INITIAL.
      lr_im_ekorg = im_r_ekorg_for.
    ELSEIF im_v_ekorg_from IS NOT INITIAL.
      APPEND INITIAL LINE TO lr_im_ekorg ASSIGNING FIELD-SYMBOL(<r_ekorg>).
      <r_ekorg>-sign   = 'I'.
      <r_ekorg>-option = 'EQ'.
      <r_ekorg>-low    = im_v_ekorg_from.
    ELSE.
      RETURN.
    ENDIF.

    LOOP AT lr_im_ekorg ASSIGNING <r_ekorg>.
      CLEAR: lr_ekorg.

      APPEND <r_ekorg> TO lr_ekorg.

      SELECT SINGLE ekorg FROM t024e " Purchasing Organizations
          INTO lv_ekorg
      WHERE ekorg IN lr_ekorg.

      IF syst-subrc IS NOT INITIAL.
        MESSAGE ID 'F2' TYPE 'E' NUMBER '045' "'Purchasing organization & is not defined'
            WITH |{ <r_ekorg>-low ALPHA = OUT }|.

        " Check if EKORG FOR is equal EKORG FROM.
      ELSEIF im_r_ekorg_for  IS NOT INITIAL AND
             lv_ekorg EQ im_v_ekorg_from.
        " Org. Compras &1 de destino é igual &2 de origem. Favor verificar
        MESSAGE e013 WITH lv_ekorg im_v_ekorg_from.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.

  METHOD check_field_bupa.

    DATA:
      lv_partner   TYPE bu_partner,
      lr_partner   TYPE RANGE OF bu_partner,
      lo_bp_vendor TYPE REF TO cvi_bp_vendor.

    LOOP AT so_bupa ASSIGNING FIELD-SYMBOL(<r_partner>).
      CLEAR: lr_partner.

      APPEND <r_partner> TO lr_partner.

      SELECT
      SINGLE partner FROM but000 " Supplier Master (General Section)
          INTO lv_partner
      WHERE partner IN lr_partner.

      IF syst-subrc IS NOT INITIAL.
        MESSAGE ID 'R1' TYPE 'E' NUMBER '201'   " 'Vendor & has not been created'
            WITH |{ <r_partner>-low ALPHA = OUT }|.
      ELSE.

        lo_bp_vendor = cvi_bp_vendor=>get_instance( i_partner = lv_partner ).

        IF lo_bp_vendor->get_vendor( ) IS INITIAL.
          " Não existe fornecedor válido para o Parceiro de negócios &1
          MESSAGE e024 WITH lv_partner.
        ENDIF.

      ENDIF.

    ENDLOOP.

  ENDMETHOD.

  METHOD check_field_lifnr.

    DATA:
      lv_lifnr TYPE lifnr,
      lr_lifnr TYPE RANGE OF lifnr.

    LOOP AT so_lifnr ASSIGNING FIELD-SYMBOL(<r_lifnr>).
      CLEAR: lr_lifnr.

      APPEND <r_lifnr> TO lr_lifnr.

      SELECT
      SINGLE lifnr FROM lfa1 " Supplier Master (General Section)
          INTO lv_lifnr
      WHERE lifnr IN lr_lifnr.

      IF syst-subrc IS NOT INITIAL.

        MESSAGE ID 'F2' TYPE 'E' NUMBER '163'   " 'Vendor & has not been created'
            WITH |{ <r_lifnr>-low ALPHA = OUT }|.
      ENDIF.

    ENDLOOP.

  ENDMETHOD.

  METHOD check_field_block_bl2.

    CHECK  so_lifnr IS INITIAL AND
           so_bupa  IS INITIAL.
    IF im_b_onli EQ abap_true.
      MESSAGE s022 DISPLAY LIKE 'E'. " Favor indicar um fornecedor válido para estender
      r_erro = abap_on.
    ELSEIF syst-ucomm EQ 'ONLI'.
      MESSAGE e022.
    ENDIF.

  ENDMETHOD.

  METHOD check_field_block_bl3.

    CHECK p_bukrs IS INITIAL AND
          p_ekorg IS INITIAL.

    IF im_b_onli EQ abap_true.
      MESSAGE s020 DISPLAY LIKE 'E'. " Favor indicar Empresa ou Org. Compras válida como dados de origem.
      r_erro = abap_on.
    ELSEIF syst-ucomm EQ 'ONLI'.
      MESSAGE e020.
    ENDIF.

  ENDMETHOD.


  METHOD check_field_block_bl4.

    IF so_bukrs IS INITIAL AND
       so_ekorg IS INITIAL.

      IF im_b_onli EQ abap_true.
        " Favor indicar empresa ou Org. Compras válida para destino.
        MESSAGE s021 DISPLAY LIKE 'E'.
        r_erro = abap_on.
        RETURN.
      ELSEIF syst-ucomm EQ 'ONLI'.
        MESSAGE e021.
      ENDIF.
    ENDIF.

  ENDMETHOD.

  METHOD extend_vendor.

    CALL METHOD super->extend_vendor
      EXPORTING
        im_b_test = im_b_test.

  ENDMETHOD.


  METHOD check_extensions.

    DATA:
      lv_bukrs TYPE bukrs,
      lv_ekorg TYPE ekorg.


    LOOP AT me->t_bp_data ASSIGNING FIELD-SYMBOL(<bp_data>) USING KEY vendor_sort_key.

      IF p_test IS NOT INITIAL.
        LOOP AT <bp_data>-object_msg TRANSPORTING NO FIELDS WHERE type CA 'AE'. ENDLOOP.
        IF syst-subrc IS NOT INITIAL. " ERROR NOT Found!!
          " Simulação de extensão para o fornecedor &1 realizada com sucesso!
          MESSAGE s032 WITH <bp_data>-vendor INTO DATA(lv_msgdummy).
        ELSE.
          " Simulação de extensão para o fornecedor &1 com erros!
          MESSAGE e033 WITH <bp_data>-vendor INTO lv_msgdummy.
        ENDIF.

        APPEND INITIAL LINE TO <bp_data>-object_msg ASSIGNING FIELD-SYMBOL(<object_msg>).
        <object_msg>-id         = sy-msgid.
        <object_msg>-type       = sy-msgty.
        <object_msg>-number     = sy-msgno.
        <object_msg>-message_v1 = sy-msgv1.
        <object_msg>-message_v2 = sy-msgv2.
        <object_msg>-message    = lv_msgdummy.
        CONTINUE.

      ELSE. " Effective execution


**********************************************************************
*-------- C H E C K  C O M P A N Y   E X T E N S I O N --------------*
**********************************************************************
        LOOP AT <bp_data>-bukrs_extended ASSIGNING FIELD-SYMBOL(<bukrs_extended>).

          " Check If Company exists for vendor
          SELECT SINGLE bukrs FROM lfb1 BYPASSING BUFFER
              INTO lv_bukrs
              WHERE lifnr = <bp_data>-vendor AND
                    bukrs = <bukrs_extended>-bukrs.

          IF syst-subrc IS INITIAL.
            " Fornecedor &1 foi estendido para a empresa &2 com sucesso.
            MESSAGE s028 WITH <bp_data>-vendor <bukrs_extended>-bukrs INTO lv_msgdummy.
          ELSE.
            " ERRO ao estender Fornecedor &1 para a empresa &2. Ver MDS_PPO2!
            MESSAGE e029 WITH <bp_data>-vendor <bukrs_extended>-bukrs INTO lv_msgdummy.
          ENDIF.

          APPEND INITIAL LINE TO <bp_data>-object_msg ASSIGNING <object_msg>.
          <object_msg>-id         = sy-msgid.
          <object_msg>-type       = sy-msgty.
          <object_msg>-number     = sy-msgno.
          <object_msg>-message_v1 = sy-msgv1.
          <object_msg>-message_v2 = sy-msgv2.
          <object_msg>-message    = lv_msgdummy.

        ENDLOOP.

**********************************************************************
*----- C H E C K   P U R C H A S I N G   E X T E N S I O N ----------*
**********************************************************************
        LOOP AT <bp_data>-ekorg_extended ASSIGNING FIELD-SYMBOL(<ekorg_extended>).

          " Check If Company exists for vendor
          SELECT SINGLE ekorg FROM lfm1 BYPASSING BUFFER
              INTO lv_ekorg
              WHERE lifnr = <bp_data>-vendor AND
                    ekorg = <ekorg_extended>-ekorg.

          IF syst-subrc IS INITIAL.
            " Fornecedor &1 foi estendido para a Org. Compras &2 com sucesso.
            MESSAGE s030 WITH <bp_data>-vendor <ekorg_extended>-ekorg INTO lv_msgdummy.
          ELSE.
            " ERRO ao estender Fornecedor &1 para a Org. Compras &2. Ver MDS_PPO2!
            MESSAGE e031 WITH <bp_data>-vendor <ekorg_extended>-ekorg INTO lv_msgdummy.
          ENDIF.

          APPEND INITIAL LINE TO <bp_data>-object_msg ASSIGNING <object_msg>.
          <object_msg>-id         = sy-msgid.
          <object_msg>-type       = sy-msgty.
          <object_msg>-number     = sy-msgno.
          <object_msg>-message_v1 = sy-msgv1.
          <object_msg>-message_v2 = sy-msgv2.
          <object_msg>-message    = lv_msgdummy.

        ENDLOOP.

        LOOP AT <bp_data>-object_msg TRANSPORTING NO FIELDS WHERE type CA 'AE'. ENDLOOP.
        IF syst-subrc IS NOT INITIAL. " ERROR NOT Found!!
          " Extensão para o fornecedor &1 realizada com sucesso!
          MESSAGE s034 WITH <bp_data>-vendor INTO lv_msgdummy.
        ELSE.
          " Extensão para o fornecedor &1 com erros!
          MESSAGE e035 WITH <bp_data>-vendor INTO lv_msgdummy.
        ENDIF.

        APPEND INITIAL LINE TO <bp_data>-object_msg ASSIGNING <object_msg>.
        <object_msg>-id         = sy-msgid.
        <object_msg>-type       = sy-msgty.
        <object_msg>-number     = sy-msgno.
        <object_msg>-message_v1 = sy-msgv1.
        <object_msg>-message_v2 = sy-msgv2.
        <object_msg>-message    = lv_msgdummy.

      ENDIF.

    ENDLOOP.

  ENDMETHOD.


  METHOD show_messages.
    DATA :
      lo_alv_table     TYPE REF TO cl_salv_table,
      lo_alv_columns   TYPE REF TO cl_salv_columns,
      lo_alv_colum     TYPE REF TO cl_salv_column_table,
      lo_alv_functions TYPE REF TO cl_salv_functions_list,
      lo_top_of_list   TYPE REF TO cl_salv_form_layout_grid,
      lo_label         TYPE REF TO cl_salv_form_label,
      lo_flow          TYPE REF TO cl_salv_form_layout_flow,
      lo_events        TYPE REF TO cl_salv_events_table,
      lo_sorts         TYPE REF TO cl_salv_sorts,
      lv_text_top      TYPE string.


    LOOP AT me->t_bp_data ASSIGNING FIELD-SYMBOL(<bp_data>) USING KEY vendor_sort_key.

      LOOP AT <bp_data>-object_msg ASSIGNING FIELD-SYMBOL(<object_msg>).

        APPEND INITIAL LINE TO me->t_alv ASSIGNING FIELD-SYMBOL(<fs_alv>).

        <fs_alv>-partner    = <bp_data>-partner.
        <fs_alv>-vendor     = <bp_data>-vendor.
        <fs_alv>-type       = <object_msg>-type.
        <fs_alv>-number     = <object_msg>-number.
        <fs_alv>-message    = <object_msg>-message.

        CASE <object_msg>-type.
          WHEN 'S'.
            <fs_alv>-status = icon_led_green.
          WHEN 'W'. "W Warning
            <fs_alv>-status = icon_led_yellow.
          WHEN 'E'.
            <fs_alv>-status = icon_led_red.
          WHEN 'A'.
            <fs_alv>-status = icon_message_critical.
        ENDCASE.

      ENDLOOP.

    ENDLOOP.

    IF p_test IS NOT INITIAL.
      lv_text_top = 'EXECUÇÃO DE TESTE'.
    ELSE.
      lv_text_top = 'EXECUÇÃO'.
    ENDIF.

    TRY.
        CALL METHOD cl_salv_table=>factory
          IMPORTING
            r_salv_table = lo_alv_table " Basis Class Simple ALV Tables
          CHANGING
            t_table      = me->t_alv.

        lo_alv_functions = lo_alv_table->get_functions( ).
        lo_alv_functions->set_default( abap_true ).

        lo_alv_columns = lo_alv_table->get_columns( ).
        lo_alv_columns->set_optimize( abap_true ).

        lo_alv_colum ?= lo_alv_columns->get_column( columnname = 'VENDOR' ).
        lo_alv_colum->set_alignment( value = if_salv_c_alignment=>centered ).
        lo_alv_colum->set_cell_type( if_salv_c_cell_type=>hotspot ).
        lo_alv_colum->set_key( value = if_salv_c_bool_sap=>true ).

        lo_alv_colum ?= lo_alv_columns->get_column( columnname = 'PARTNER' ).
        lo_alv_colum->set_alignment( value = if_salv_c_alignment=>centered ).
        lo_alv_colum->set_cell_type( if_salv_c_cell_type=>hotspot ).
        lo_alv_colum->set_key( value = if_salv_c_bool_sap=>true ).

        lo_alv_colum ?= lo_alv_columns->get_column( columnname = 'TYPE' ).
        lo_alv_colum->set_alignment( value = if_salv_c_alignment=>centered ).
        lo_alv_colum->set_visible( value = if_salv_c_bool_sap=>false ).

        lo_alv_colum ?= lo_alv_columns->get_column( columnname = 'NUMBER' ).
        lo_alv_colum->set_alignment( value = if_salv_c_alignment=>centered ).
        lo_alv_colum->set_visible( value = if_salv_c_bool_sap=>false ).

        lo_sorts = lo_alv_table->get_sorts( ).
        lo_sorts->add_sort( columnname = 'VENDOR' ).
        lo_sorts->add_sort( columnname = 'PARTNER' ).

        "events
        lo_events = lo_alv_table->get_event( ).

        SET HANDLER me->on_link_click FOR lo_events.


        CREATE OBJECT lo_top_of_list.

        lo_label = lo_top_of_list->create_label( row = 1 column  = 1 ).
        lo_label->set_text( value = lv_text_top ).
        lo_flow = lo_top_of_list->create_flow( row = 2 column  = 1 ).
        lo_flow->create_text(
            text = |Data de execução: { sy-datlo DATE = USER } Hora: { sy-timlo TIME = USER }| ).
        lo_alv_table->set_top_of_list( value =  lo_top_of_list ).


        CALL METHOD lo_alv_table->display.

      CATCH cx_salv_msg. " ALV: General Error Class with Message
      CATCH cx_salv_not_found. " ALV: General Error Class (Checked During Syntax Check)
      CATCH cx_salv_existing.   " ALV: General Error Class (Checked During Syntax Check)
      CATCH cx_salv_data_error. " ALV: General Error Class (Checked During Syntax Check)
    ENDTRY.


  ENDMETHOD.


  METHOD on_link_click.
    DATA:
        lv_partner_number TYPE bu_partner.

    READ TABLE me->t_alv ASSIGNING FIELD-SYMBOL(<alv_line>) INDEX row.
    CHECK sy-subrc = 0.

    CASE column.
      WHEN 'PARTNER' OR 'VENDOR'.
        lv_partner_number = |{ <alv_line>-partner ALPHA = IN }|.

        CHECK lv_partner_number IS NOT INITIAL.

        SET PARAMETER ID 'BPA' FIELD lv_partner_number.
        CALL TRANSACTION 'BP' WITH AUTHORITY-CHECK.

    ENDCASE.

  ENDMETHOD.

ENDCLASS. " lcl_bupa IMPLEMENTATION .
