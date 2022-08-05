
CLASS lcl_bupa001_test DEFINITION FOR TESTING
  DURATION SHORT RISK LEVEL HARMLESS.

*?ï»¿<asx:abap xmlns:asx="http://www.sap.com/abapxml" version="1.0">
*?<asx:values>
*?<TESTCLASS_OPTIONS>
*?<TEST_CLASS>lcl_Bupa001_Test
*?</TEST_CLASS>
*?<TEST_MEMBER>-no-
*?</TEST_MEMBER>
*?<OBJECT_UNDER_TEST/>
*?<OBJECT_IS_LOCAL/>
*?<GENERATE_FIXTURE>X
*?</GENERATE_FIXTURE>
*?<GENERATE_CLASS_FIXTURE>X
*?</GENERATE_CLASS_FIXTURE>
*?<GENERATE_INVOCATION>X
*?</GENERATE_INVOCATION>
*?<GENERATE_ASSERT_EQUAL>X
*?</GENERATE_ASSERT_EQUAL>
*?</TESTCLASS_OPTIONS>
*?</asx:values>
*?</asx:abap>
  PRIVATE SECTION.

    TYPES:
      ty_t_zpf_banco    TYPE STANDARD TABLE OF zpf_banco    WITH DEFAULT KEY,
      ty_t_zpf_ir       TYPE STANDARD TABLE OF zpf_ir       WITH DEFAULT KEY,
      ty_t_adr2         TYPE STANDARD TABLE OF adr2         WITH DEFAULT KEY,
      ty_t_zpf_telefone TYPE STANDARD TABLE OF zpf_telefone WITH DEFAULT KEY,
      ty_t_zpf_celular  TYPE STANDARD TABLE OF zpf_celular  WITH DEFAULT KEY,
      ty_t_adr3         TYPE STANDARD TABLE OF adr3         WITH DEFAULT KEY,
      ty_t_adr6         TYPE STANDARD TABLE OF adr6         WITH DEFAULT KEY,
      ty_t_knvk         TYPE STANDARD TABLE OF knvk         WITH DEFAULT KEY,
      ty_t_contatos     TYPE STANDARD TABLE OF zpf_contato  WITH DEFAULT KEY,
      ty_t_zpf_mensagem TYPE STANDARD TABLE OF zpf_mensagem WITH DEFAULT KEY,
      ty_t_zpf_email    TYPE STANDARD TABLE OF zpf_email    WITH DEFAULT KEY,
      ty_t_zpf_fax      TYPE STANDARD TABLE OF zpf_fax      WITH DEFAULT KEY.

    CLASS-METHODS     return_random_cnpj
      RETURNING
        VALUE(r_result) TYPE stcd1.

*    CLASS-METHODS:
*      class_setup,
*      class_teardown.
    METHODS:
      setup,
      teardown,
      zif_vendor_create             FOR TESTING,
      zif_vendor_delete             FOR TESTING,
      zif_vendor_display_positive   FOR TESTING,
      zif_vendor_display_negative   FOR TESTING,
      zif_vendor_update             FOR TESTING,
      "! Get random vendor for testing
      "! @parameter ex_s_lfa1 | Mestre de fornecedores (parte geral)
      "! @parameter ex_s_fornecedor | Dados fornecedor para suporte ao Portal do Fornecedor
      get_random_vendor_for_use
        EXPORTING
          ex_s_lfa1         TYPE lfa1
          ex_s_fornecedor   TYPE zpf_fornecedor
          ex_t_banco_tab    TYPE ty_t_zpf_banco
          ex_t_ir_tab       TYPE ty_t_zpf_ir
          ex_t_tel_tab      TYPE ty_t_adr2
          ex_t_fax_tab      TYPE ty_t_adr3
          ex_t_email_tab    TYPE ty_t_adr6
          ex_t_contatos_tab TYPE ty_t_knvk .


ENDCLASS.       "lcl_Bupa001_Test


CLASS lcl_bupa001_test IMPLEMENTATION.

*  METHOD class_setup.
*
*  ENDMETHOD.
*
*  METHOD class_teardown.
*
*  ENDMETHOD.

  METHOD setup.


  ENDMETHOD.


  METHOD teardown.


  ENDMETHOD.


  METHOD zif_vendor_create.

    DATA:
      ls_lfa1                  TYPE lfa1,
      lv_lifnr                 TYPE lifnr,
      ls_fornecedor            TYPE zpf_fornecedor,
      lt_banco_tab_expected    TYPE ty_t_zpf_banco,
      lt_banco_tab_return      TYPE ty_t_zpf_banco,
      lt_contatos_tab_expected TYPE STANDARD TABLE OF zpf_contato,
      lt_contatos_tab_return   TYPE STANDARD TABLE OF zpf_contato,
      lt_email_tab_expected    TYPE ty_t_zpf_email,
      lt_email_tab_return      TYPE ty_t_zpf_email,
      lt_fax_tab_expected      TYPE ty_t_zpf_fax,
      lt_fax_tab_return        TYPE ty_t_zpf_fax,
      lt_ir_tab_expected       TYPE ty_t_zpf_ir,
      lt_ir_tab_return         TYPE ty_t_zpf_ir,
      lt_mess_tab_expected     TYPE ty_t_mensagem,
      lt_mess_tab_return       TYPE ty_t_mensagem,
      lt_tel_tab_expected      TYPE ty_t_zpf_telefone,
      lt_tel_tab_return        TYPE ty_t_zpf_telefone,
      lt_cel_tab_expected      TYPE ty_t_zpf_celular,
      lt_cel_tab_return        TYPE ty_t_zpf_celular,
      lt_return_map_return     TYPE mdg_bs_bp_msgmap_t,
      lt_return_map_expected   TYPE mdg_bs_bp_msgmap_t,
      lt_return_return         TYPE bapiretm,
      lt_return_expected       TYPE bapiretm.


**********************************************************************

    ls_fornecedor-stcd1       = return_random_cnpj( ).
    ls_fornecedor-ktokk       = gc_pessoa_juridica_nacj.
    ls_fornecedor-name1       = |TESTE ABAP UNIT { syst-timlo }{ syst-datlo } Interface SERTRAS|.
    ls_fornecedor-sortl       = |TESTE ABAP UNIT|.
    ls_fornecedor-street      = |Rua do Mercado|.
    ls_fornecedor-str_suppl3  = |Rua 4_str_suppl3|.
    ls_fornecedor-house_num1  = syst-timlo+3(3).
    ls_fornecedor-house_num2  = |NUM2|.
    ls_fornecedor-city2       = |Centro|.
    ls_fornecedor-post_code1  = |20010-120|.
    ls_fornecedor-city1       = |Rio de Janeiro|.
    ls_fornecedor-country     = |BR|.
    ls_fornecedor-region      = |RJ|.
    ls_fornecedor-tel_number  = |(21)34 { syst-timlo }|.
    ls_fornecedor-fax_number  = |(21)75 { syst-timlo }|.
    ls_fornecedor-mob_number  = |(21)99 { syst-timlo }|.
    ls_fornecedor-smtp_addr   = |teste{ syst-timlo }@teste.com.br|.
    ls_fornecedor-stcd4       = |{ syst-timlo }{ syst-datlo }|.
    ls_fornecedor-stcd3       = |{ syst-datlo }{ syst-timlo }|.
    ls_fornecedor-zterm       = |TB00|. "Erro previsto uma vez que o Sertras manda essa condição errada!

    INSERT INITIAL LINE INTO lt_banco_tab_return
        ASSIGNING FIELD-SYMBOL(<fs_banco>) INDEX 1.
    <fs_banco>-banks = 'BR'.
    <fs_banco>-bankl = '23720105'.
    <fs_banco>-bankn = syst-timlo.
    <fs_banco>-koinh = ls_fornecedor-name1.
    <fs_banco>-bvtyp = '237'.
    <fs_banco>-bkont = '00'.

    INSERT INITIAL LINE INTO lt_banco_tab_return
        ASSIGNING <fs_banco> INDEX 1.
    <fs_banco>-banks = 'BR'.
    <fs_banco>-bankl = '00190030'.
    <fs_banco>-bankn = syst-timlo.
    <fs_banco>-koinh = ls_fornecedor-name1.
    <fs_banco>-bvtyp = '001'.
    <fs_banco>-bkont = '00'.


    INSERT INITIAL LINE INTO lt_tel_tab_return
        ASSIGNING FIELD-SYMBOL(<fs_tel>) INDEX 1.
    <fs_tel>-telefone = |(21)45 { syst-timlo }|.

    INSERT INITIAL LINE INTO lt_fax_tab_return
        ASSIGNING FIELD-SYMBOL(<fs_fax>) INDEX 1.
    <fs_fax>-fax = |(21)11 { syst-timlo }|.

    INSERT INITIAL LINE INTO lt_email_tab_return
        ASSIGNING FIELD-SYMBOL(<fs_email>) INDEX 1.
    <fs_email>-email = |email_tab{ syst-timlo }@teste.com.br|.


    MOVE-CORRESPONDING:
        lt_email_tab_return TO lt_email_tab_expected,
        lt_fax_tab_return   TO lt_fax_tab_expected,
        lt_tel_tab_return   TO lt_tel_tab_expected,
        lt_banco_tab_return TO lt_banco_tab_expected.

    CALL FUNCTION 'ZIF_VENDOR_CREATE'
      EXPORTING
        i_fornecedor   = ls_fornecedor
      IMPORTING
        e_lifnr        = lv_lifnr
        e_return_map   = lt_return_map_return
        e_return       = lt_return_return
      TABLES
        i_banco_tab    = lt_banco_tab_return
        i_ir_tab       = lt_ir_tab_return
        e_mess_tab     = lt_mess_tab_return
        e_tel_tab      = lt_tel_tab_return
        e_cel_tab      = lt_cel_tab_return
        e_fax_tab      = lt_fax_tab_return
        e_email_tab    = lt_email_tab_return
        e_contatos_tab = lt_contatos_tab_return.


    CALL METHOD:
        cl_abap_unit_assert=>assert_equals(
          EXPORTING
            act = lt_cel_tab_return                 " Data object with current value
            exp = lt_cel_tab_expected               " Data object with expected type
            msg = |{ TEXT-003 } e_Cel_Tab| ),       " Description

        cl_abap_unit_assert=>assert_equals(
          EXPORTING
            act   = lt_contatos_tab_return          " Data object with current value
            exp   = lt_contatos_tab_expected        " Data object with expected type
            msg   = |{ TEXT-003 } e_Contatos_Tab| )," Description

        cl_abap_unit_assert=>assert_equals(
          EXPORTING
            act   = lt_email_tab_return             " Data object with current value
            exp   = lt_email_tab_expected           " Data object with expected type
            msg   = |{ TEXT-003 } e_Email_Tab | ),  " Description

        cl_abap_unit_assert=>assert_equals(
          EXPORTING
            act   = lt_fax_tab_return               " Data object with current value
            exp   = lt_fax_tab_expected             " Data object with expected type
            msg   = |{ TEXT-003 } e_Fax_Tab | ),    " Description

        cl_abap_unit_assert=>assert_not_initial(
          EXPORTING
            act  = lv_lifnr                         " Actual Data Object
            msg  = |{ TEXT-003 } e_Lifnr | ),       " Message in Case of Error

        cl_abap_unit_assert=>assert_equals(
          EXPORTING
            act   = lt_tel_tab_return               " Data object with current value
            exp   = lt_tel_tab_expected             " Data object with expected type
            msg   = |{ TEXT-003 } e_Tel_Tab | ),    " Description

        cl_abap_unit_assert=>assert_equals(
          EXPORTING
            act   = lt_banco_tab_return             " Data object with current value
            exp   = lt_banco_tab_expected           " Data object with expected type
            msg   = |{ TEXT-003 } i_Banco_Tab | ),  " Description

        cl_abap_unit_assert=>assert_equals(
          EXPORTING
            act   = lt_ir_tab_return                " Data object with current value
            exp   = lt_ir_tab_expected              " Data object with expected type
            msg   = |{ TEXT-003 } i_Ir_Tab | ),     " Description

        cl_abap_unit_assert=>assert_equals(
          EXPORTING
            act   = lt_mess_tab_return              " Data object with current value
            exp   = lt_mess_tab_expected            " Data object with expected type
            msg   = |{ TEXT-003 } e_mess_tab | ),   " Description

        cl_abap_unit_assert=>assert_equals(
          EXPORTING
            act   = lt_return_map_return            " Data object with current value
            exp   = lt_return_map_return            " Data object with expected type
            msg   = |{ TEXT-003 } e_return_map | ), " Description

        cl_abap_unit_assert=>assert_equals(
          EXPORTING
            act   = lt_return_return                " Data object with current value
            exp   = lt_return_return                " Data object with expected type
            msg   = |{ TEXT-003 } e_return | ).     " Description


    SELECT SINGLE * FROM lfa1 INTO @ls_lfa1
    WHERE stcd1 = @ls_fornecedor-stcd1.

    CALL METHOD cl_abap_unit_assert=>assert_subrc
      EXPORTING
        exp   = 0                            " Expected return code, optional, if not zero
        act   = sy-subrc                     " Return code of ABAP statements
        msg   = |{ TEXT-003 } Fornecedor não criado! | " Description
        level = if_aunit_constants=>critical. " Severity (TOLERABLE, >CRITICAL<, FATAL)

  ENDMETHOD.


  METHOD zif_vendor_delete.

    DATA:
      ls_lfa1              TYPE lfa1,
      lt_mess_tab_expected TYPE STANDARD TABLE OF zpf_mensagem WITH DEFAULT KEY,
      lt_mess_tab_return   TYPE STANDARD TABLE OF zpf_mensagem WITH DEFAULT KEY.

    INSERT INITIAL LINE INTO lt_mess_tab_expected
        ASSIGNING FIELD-SYMBOL(<fs_mess_tab_expected>) INDEX 1.
    <fs_mess_tab_expected>-type   = 'E'.
    <fs_mess_tab_expected>-texto  = TEXT-002. " Sem autorização para excluir fornecedor

    CALL METHOD get_random_vendor_for_use
      IMPORTING
        ex_s_lfa1 = ls_lfa1.

    CALL FUNCTION 'ZIF_VENDOR_DELETE'
      EXPORTING
        i_lifnr    = ls_lfa1-lifnr
        i_cpf_cnpj = ls_lfa1-stcd1
      TABLES
        e_mess_tab = lt_mess_tab_return.


    CALL METHOD cl_abap_unit_assert=>assert_equals(
      EXPORTING
        act   = lt_mess_tab_return              " Data object with current value
        exp   = lt_mess_tab_expected            " Data object with expected type
        msg   = |{ TEXT-003 } e_mess_tab |      " Description
        level = if_aunit_constants=>critical ). " Severity (TOLERABLE, CRITICAL, FATAL)

  ENDMETHOD.


  METHOD zif_vendor_display_positive.

    DATA:
      ls_fornecedor            TYPE zpf_fornecedor,
      ls_fornecedor_return     TYPE zpf_fornecedor,
      ls_lfa1                  TYPE lfa1,
      lt_banco_tab_expected    TYPE ty_t_zpf_banco,
      lt_banco_tab_return      TYPE ty_t_zpf_banco,
      lt_contatos_tab_expected TYPE ty_t_knvk,
      lt_contatos_tab_return   TYPE ty_t_knvk,
      lt_email_tab_expected    TYPE ty_t_adr6,
      lt_email_tab_return      TYPE ty_t_adr6,
      lt_fax_tab_expected      TYPE ty_t_adr3,
      lt_fax_tab_return        TYPE ty_t_adr3,
      lt_ir_tab_expected       TYPE ty_t_zpf_ir,
      lt_ir_tab_return         TYPE ty_t_zpf_ir,
      lt_mess_tab_expected     TYPE ty_t_zpf_mensagem,
      lt_mess_tab_return       TYPE ty_t_zpf_mensagem,
      lt_tel_tab_expected      TYPE ty_t_adr2,
      lt_tel_tab_return        TYPE ty_t_adr2.


    CALL METHOD get_random_vendor_for_use
      IMPORTING
        ex_s_lfa1         = ls_lfa1
        ex_s_fornecedor   = ls_fornecedor
        ex_t_banco_tab    = lt_banco_tab_expected
        ex_t_ir_tab       = lt_ir_tab_expected
        ex_t_tel_tab      = lt_tel_tab_expected
        ex_t_fax_tab      = lt_fax_tab_expected
        ex_t_email_tab    = lt_email_tab_expected
        ex_t_contatos_tab = lt_contatos_tab_expected.

    CALL FUNCTION 'ZIF_VENDOR_DISPLAY'
      EXPORTING
        i_cpf_cnpj     = ls_fornecedor-stcd1
      IMPORTING
        e_fornecedor   = ls_fornecedor_return
      TABLES
        e_banco_tab    = lt_banco_tab_return
        e_ir_tab       = lt_ir_tab_return
        e_mess_tab     = lt_mess_tab_return
        e_tel_tab      = lt_tel_tab_return
        e_fax_tab      = lt_fax_tab_return
        e_email_tab    = lt_email_tab_return
        e_contatos_tab = lt_contatos_tab_return.


    cl_abap_unit_assert=>assert_equals(
      EXPORTING
        act = lt_banco_tab_return              " Data object with current value
        exp = lt_banco_tab_expected            " Data object with expected type
        msg = |{ TEXT-003 } e_Banco_Tab|      " Description
        level = if_aunit_constants=>critical   " Severity (TOLERABLE, CRITICAL, FATAL)
    ).

    cl_abap_unit_assert=>assert_equals(
      act   = lt_contatos_tab_return
      exp   = lt_contatos_tab_expected          "<--- please adapt expected value
      msg   = |{ TEXT-003 } e_Contatos_Tab|
      level = if_aunit_constants=>tolerable
    ).
    cl_abap_unit_assert=>assert_equals(
      act   = lt_email_tab_return
      exp   = lt_email_tab_return          "<--- please adapt expected value
      msg   = |{ TEXT-003 } e_Email_Tab|
      level = if_aunit_constants=>tolerable
    ).
    cl_abap_unit_assert=>assert_equals(
        act   = lt_fax_tab_return
        exp   = lt_fax_tab_expected          "<--- please adapt expected value
        msg   = |{ TEXT-003 } e_Fax_Tab|
        level = if_aunit_constants=>tolerable
    ).
    cl_abap_unit_assert=>assert_equals(
        act   = ls_fornecedor_return
        exp   = ls_fornecedor          "<--- please adapt expected value
        msg   = |{ TEXT-003 } e_Fornecedor|
        level = if_aunit_constants=>tolerable
    ).
    cl_abap_unit_assert=>assert_equals(
        act   = lt_ir_tab_return
        exp   = lt_ir_tab_expected          "<--- please adapt expected value
        msg   = |{ TEXT-003 } e_Ir_Tab|
        level = if_aunit_constants=>tolerable
    ).
    cl_abap_unit_assert=>assert_equals(
        act   = lt_mess_tab_return
        exp   = lt_mess_tab_expected          "<--- please adapt expected value
        msg   = |{ TEXT-003 } e_Mess_Tab|
        level = if_aunit_constants=>tolerable
    ).
    cl_abap_unit_assert=>assert_equals(
        act   = lt_tel_tab_return
        exp   = lt_tel_tab_expected          "<--- please adapt expected value
        msg   = |{ TEXT-003 } e_Tel_Tab|
        level = if_aunit_constants=>tolerable
    ).


  ENDMETHOD.


  METHOD zif_vendor_update.

    "Não está pronto pois não obtive tempo necessario apra criar o Cenario de teste! :-(

    DATA:
      i_fornecedor   TYPE zpf_fornecedor,
      i_fornecedorx  TYPE zpf_fornecedorx,
      e_lifnr        TYPE lifnr,
      e_return_map   TYPE mdg_bs_bp_msgmap_t,
      e_return       TYPE bapiretm,
      i_banco_tab    TYPE STANDARD TABLE OF zpf_banco    WITH DEFAULT KEY,
      i_ir_tab       TYPE STANDARD TABLE OF zpf_ir       WITH DEFAULT KEY,
      e_mess_tab     TYPE STANDARD TABLE OF zpf_mensagem WITH DEFAULT KEY,
      e_tel_tab      TYPE STANDARD TABLE OF zpf_telefone WITH DEFAULT KEY,
      e_cel_tab      TYPE STANDARD TABLE OF zpf_celular  WITH DEFAULT KEY,
      e_fax_tab      TYPE STANDARD TABLE OF zpf_fax      WITH DEFAULT KEY,
      e_email_tab    TYPE STANDARD TABLE OF zpf_email    WITH DEFAULT KEY,
      e_contatos_tab TYPE STANDARD TABLE OF zpf_contato  WITH DEFAULT KEY.


    CALL FUNCTION 'ZIF_VENDOR_UPDATE'
      EXPORTING
        i_fornecedor  = i_fornecedor      " Mestre de fornecedores (parte geral)
        i_fornecedorx = i_fornecedorx     " Dados fornecedor para suporte ao Portal do Fornecedor
*       i_lifnr       =                  " Nº conta do fornecedor
*       i_cpf_cnpj    =                  " Nº ID fiscal 1
*      IMPORTING
*       e_lifnr       =                  " Nº conta do fornecedor
*       e_return_map  =                  " Table type for structure MDG_BS_BP_MSGMAP
*       e_return      =                  " Categoria de tabela de BAPIRETI p/objetos múltiplos
*      TABLES
*       i_banco_tab   =                  " Dados bancários para suporte ao Portal do Fornecedor
*       i_ir_tab      =                  " Dados de imposto para suporte ao Portal do Fornecedor
*       e_mess_tab    =                  " Mensagens de retorno
*       e_tel_tab     =                  " Dados de Telefones para suporte ao Portal do Fornecedor
*       e_cel_tab     =                  " Dados de celulares para suporte ao Portal do Fornecedor
*       e_fax_tab     =                  " Dados de fax para suporte ao Portal do Fornecedor
*       e_email_tab   =                  " Dados de email para suporte ao Portal do Fornecedor
*       e_contatos_tab =                  " Dados de contatos para suporte ao Portal do Fornecedor
      .



    cl_abap_unit_assert=>assert_equals(
      act   = e_cel_tab
      exp   = e_cel_tab          "<--- please adapt expected value
    " msg   = 'Testing value e_Cel_Tab'
*     level =
    ).
    cl_abap_unit_assert=>assert_equals(
      act   = e_contatos_tab
      exp   = e_contatos_tab          "<--- please adapt expected value
    " msg   = 'Testing value e_Contatos_Tab'
*     level =
    ).
    cl_abap_unit_assert=>assert_equals(
      act   = e_email_tab
      exp   = e_email_tab          "<--- please adapt expected value
    " msg   = 'Testing value e_Email_Tab'
*     level =
    ).
    cl_abap_unit_assert=>assert_equals(
      act   = e_fax_tab
      exp   = e_fax_tab          "<--- please adapt expected value
    " msg   = 'Testing value e_Fax_Tab'
*     level =
    ).
    cl_abap_unit_assert=>assert_equals(
      act   = e_lifnr
      exp   = e_lifnr          "<--- please adapt expected value
    " msg   = 'Testing value e_Lifnr'
*     level =
    ).
    cl_abap_unit_assert=>assert_equals(
      act   = e_mess_tab
      exp   = e_mess_tab          "<--- please adapt expected value
    " msg   = 'Testing value e_Mess_Tab'
*     level =
    ).
    cl_abap_unit_assert=>assert_equals(
      act   = e_return
      exp   = e_return          "<--- please adapt expected value
    " msg   = 'Testing value e_Return'
*     level =
    ).
    cl_abap_unit_assert=>assert_equals(
      act   = e_return_map
      exp   = e_return_map          "<--- please adapt expected value
    " msg   = 'Testing value e_Return_Map'
*     level =
    ).
    cl_abap_unit_assert=>assert_equals(
      act   = e_tel_tab
      exp   = e_tel_tab          "<--- please adapt expected value
    " msg   = 'Testing value e_Tel_Tab'
*     level =
    ).
    cl_abap_unit_assert=>assert_equals(
      act   = i_banco_tab
      exp   = i_banco_tab          "<--- please adapt expected value
    " msg   = 'Testing value i_Banco_Tab'
*     level =
    ).
    cl_abap_unit_assert=>assert_equals(
      act   = i_ir_tab
      exp   = i_ir_tab          "<--- please adapt expected value
    " msg   = 'Testing value i_Ir_Tab'
*     level =
    ).
  ENDMETHOD.


  METHOD get_random_vendor_for_use.
*  All data searches are done using vendor information,
*  I used this feature to check for any difference between BP and vendor

    DATA:
      lt_lfa1 TYPE TABLE OF lfa1 WITH DEFAULT KEY,
      lo_ran  TYPE REF TO cl_abap_random_int,
      lv_i    TYPE i.

    " Mestre de fornecedores (parte geral)
    SELECT a1~* FROM lfa1 AS a1
    INNER JOIN lfbk AS bk ON a1~lifnr = bk~lifnr
    INTO CORRESPONDING FIELDS OF TABLE @lt_lfa1
        BYPASSING BUFFER
        WHERE ktokk IN ('NACJ','NACF') AND
              ( stcd1 NE @space OR stcd2 NE @space ) AND " CNPJ ou CPF preenchidos
*              ( stcd3 NE @space OR stcd4 NE @space ) AND " Insc filled
              ( telf1 NE @space ).

    " Get random vendor from selected
    TRY.
        lv_i = cl_abap_random=>seed( ).
        lo_ran = cl_abap_random_int=>create(
               seed = lv_i
               min  = 1
               max  = syst-dbcnt ).

        lv_i = lo_ran->get_next( ).

      CATCH cx_abap_random. " Exception for CL_ABAP_RANDOM* = 1 max = syst-dbcnt ).
        " Deu Ruim
    ENDTRY.

    READ TABLE lt_lfa1 INTO ex_s_lfa1 INDEX lv_i.

    MOVE-CORRESPONDING ex_s_lfa1 TO ex_s_fornecedor.

    MOVE:
      ex_s_lfa1-telf1   TO ex_s_fornecedor-tel_number,     " Primeiro nº de telefone: código + nº
      ex_s_lfa1-telf2   TO ex_s_fornecedor-mob_number,     " Primeiro nº de telefone celular: prefixo + conexão
      ex_s_lfa1-telfx   TO ex_s_fornecedor-fax_number.     " Primeiro nº fax: código telefónico + nº

**********************************************************************
    SELECT
      FROM        lfbk "Mestre de fornecedores (coordenadas do banco)
      INNER JOIN  bnka "Mestre de bancos
        ON  bnka~banks = lfbk~banks
        AND bnka~bankl = lfbk~bankl
      FIELDS  lfbk~banks, lfbk~bankl, lfbk~bankn,
              lfbk~koinh, lfbk~bvtyp, lfbk~bkont,
              bnka~banka, bnka~provz, bnka~stras,
              bnka~ort01, bnka~brnch
      WHERE lfbk~lifnr = @ex_s_lfa1-lifnr
      INTO CORRESPONDING FIELDS OF TABLE @ex_t_banco_tab.

**********************************************************************
    " Mestre de fornecedores (ctgs.de impostos retidos na fonte) X
    SELECT * FROM lfbw
      WHERE lifnr = @ex_s_lfa1-lifnr
      INTO CORRESPONDING FIELDS OF TABLE @ex_t_ir_tab.

**********************************************************************
* Endereços (administração de endereços central)
    SELECT SINGLE * FROM adrc
      WHERE addrnumber = @ex_s_lfa1-adrnr
      INTO CORRESPONDING FIELDS OF @ex_s_fornecedor.
    MOVE-CORRESPONDING ex_s_fornecedor TO ex_s_lfa1.

**********************************************************************
* Nºs telefônicos (administração de endereços central)
    SELECT * FROM adr2
      WHERE addrnumber = @ex_s_lfa1-adrnr
      INTO TABLE @ex_t_tel_tab.

**********************************************************************
* Nºs fax (administração de endereços central)
    SELECT * FROM adr3
      WHERE addrnumber = @ex_s_lfa1-adrnr
      INTO TABLE @ex_t_fax_tab.

**********************************************************************
* Endereços de e-mail (administração de endereços central)
    SELECT * FROM adr6
      WHERE addrnumber = @ex_s_lfa1-adrnr
      INTO TABLE @ex_t_email_tab.

* Retorna o primeiro email relacionado ao endereço principal do Fornecedor.
* Não existe definição para qual email é prioritario do endereço ou comunição idependente de endereço
    IF ex_s_fornecedor-smtp_addr IS INITIAL AND ex_t_email_tab IS NOT INITIAL.
      READ TABLE ex_t_email_tab ASSIGNING FIELD-SYMBOL(<fs_email_tab>) INDEX 1.
      MOVE <fs_email_tab>-smtp_addr TO ex_s_fornecedor-smtp_addr.
    ENDIF.

**********************************************************************
* Mestre de clientes - pessoas de contato
    SELECT * FROM knvk
      WHERE lifnr = @ex_s_lfa1-lifnr
      INTO TABLE @ex_t_contatos_tab.
**********************************************************************
*    Busca Chave de condições de pagamento
    SELECT SINGLE zterm FROM lfb1
      WHERE lifnr = @ex_s_lfa1-lifnr
      INTO @ex_s_fornecedor-zterm.

  ENDMETHOD.

  METHOD zif_vendor_display_negative.

    DATA:
      ls_fornecedor          TYPE zpf_fornecedor,
      ls_fornecedor_return   TYPE zpf_fornecedor,
      lt_banco_tab_return    TYPE ty_t_zpf_banco,
      lt_contatos_tab_return TYPE ty_t_knvk,
      lt_email_tab_return    TYPE ty_t_adr6,
      lt_fax_tab_return      TYPE ty_t_adr3,
      lt_ir_tab_return       TYPE ty_t_zpf_ir,
      lt_mess_tab_expected   TYPE ty_t_zpf_mensagem,
      lt_mess_tab_return     TYPE ty_t_zpf_mensagem,
      lt_tel_tab_return      TYPE ty_t_adr2.

    ls_fornecedor-stcd1 = |12345|.

    CALL FUNCTION 'ZIF_VENDOR_DISPLAY'
      EXPORTING
        i_cpf_cnpj     = ls_fornecedor-stcd1
      IMPORTING
        e_fornecedor   = ls_fornecedor_return
      TABLES
        e_banco_tab    = lt_banco_tab_return
        e_ir_tab       = lt_ir_tab_return
        e_mess_tab     = lt_mess_tab_return
        e_tel_tab      = lt_tel_tab_return
        e_fax_tab      = lt_fax_tab_return
        e_email_tab    = lt_email_tab_return
        e_contatos_tab = lt_contatos_tab_return.

    INSERT INITIAL LINE INTO lt_mess_tab_expected
        ASSIGNING FIELD-SYMBOL(<fs_mess_tab_expected>) INDEX 1.

    <fs_mess_tab_expected>-type  = 'E'.
    <fs_mess_tab_expected>-texto = TEXT-001.

    CALL METHOD:

     cl_abap_unit_assert=>assert_initial(
      EXPORTING
        act = lt_banco_tab_return              " Actual data object
        msg = 'Testing value e_banco_tab'   ), " Description

     cl_abap_unit_assert=>assert_initial(
      EXPORTING
        act = lt_contatos_tab_return           " Actual data object
        msg = 'Testing value e_Contatos_Tab'), " Description

     cl_abap_unit_assert=>assert_initial(
      EXPORTING
        act = lt_banco_tab_return              " Actual data object
        msg = 'Testing value e_banco_tab'   ), " Description

    cl_abap_unit_assert=>assert_initial(
      EXPORTING
        act   = lt_fax_tab_return               " Actual data object
        msg   = 'Testing value e_Email_Tab' ),  " Description

    cl_abap_unit_assert=>assert_initial(
      EXPORTING
        act   = lt_email_tab_return             " Actual data object
        msg   = 'Testing value e_Fax_Tab'   ),  " Description

    cl_abap_unit_assert=>assert_initial(
      EXPORTING
        act   = ls_fornecedor_return            " Actual data object
        msg   = 'Testing value e_Fornecedor'),  " Description

    cl_abap_unit_assert=>assert_initial(
      EXPORTING
        act   = lt_ir_tab_return                " Actual data object
        msg   = 'Testing value e_Ir_Tab'    ),  " Description

    cl_abap_unit_assert=>assert_initial(
      EXPORTING
        act   = lt_tel_tab_return               " Actual data object
        msg   = 'Testing value e_Tel_Tab'   ),  " Description

    cl_abap_unit_assert=>assert_equals(
      EXPORTING
        act   = lt_mess_tab_return
        exp   = lt_mess_tab_expected
        msg   = 'Testing value e_Mess_Tab'  ).

  ENDMETHOD.

  METHOD return_random_cnpj.
    CONSTANTS:
      lc_filial   TYPE c LENGTH 4 VALUE '0001'.

    DATA:
      lo_ran         TYPE REF TO cl_abap_random_int,
      lv_seed        TYPE i,
      lv_cnpj_core   TYPE c LENGTH 8,
      lv_cnpj_sem_dv TYPE c LENGTH 12,
      lv_i           TYPE p,
      lv_j           TYPE p,
      lv_cnpj        TYPE c LENGTH 14,
      lv_digsum      TYPE p,
      lv_digrest     TYPE p,
      lv_digit       TYPE c LENGTH 2 VALUE '00',
      lv_first_dv    TYPE i,
      lv_second_dv   TYPE i.

    " Get random vendor from selected
    TRY.
        lv_seed = cl_abap_random=>seed( ).

        lo_ran = cl_abap_random_int=>create(
               seed = lv_seed
               min  = 100000
               max  = 99999999 ).

        lv_cnpj_core = lo_ran->get_next( ).

        lv_cnpj_sem_dv = |{ lv_cnpj_core ALPHA = IN }{ lc_filial }|.

      CATCH cx_abap_random. " Exception for CL_ABAP_RANDOM* = 1 max = syst-dbcnt ).
        " Deu Ruim
    ENDTRY.

**********************************************************************

    lv_cnpj  = lv_cnpj_sem_dv.

    CLEAR: lv_digsum, lv_i.

    DO 12 TIMES.
      ADD 1 TO lv_i.
      IF lv_i < 5.
        lv_j = 6 - lv_i.
      ELSE.
        lv_j = 14 - lv_i.
      ENDIF.
      WRITE: lv_cnpj+0(1) TO lv_digit.
      lv_digsum = lv_digsum + ( lv_digit * lv_j ).
      SHIFT lv_cnpj LEFT.
    ENDDO.

    lv_digrest = lv_digsum MOD 11.
    IF lv_digrest = 0 OR lv_digrest = 1.
      lv_first_dv = 0.
    ELSE.
      lv_first_dv = 11 - lv_digrest.
    ENDIF.

    lv_cnpj  = lv_cnpj_sem_dv.

    CLEAR: lv_digsum, lv_i.

    DO 12 TIMES.
      ADD 1 TO lv_i.
      IF lv_i < 6.
        lv_j = 7 - lv_i.
      ELSE.
        lv_j = 15 - lv_i.
      ENDIF.
      WRITE: lv_cnpj+0(1) TO lv_digit.
      lv_digsum = lv_digsum + ( lv_digit * lv_j ).
      SHIFT lv_cnpj LEFT.
    ENDDO.

    lv_digsum = lv_digsum + ( lv_first_dv * 2 ).

    lv_digrest = lv_digsum MOD 11.
    IF lv_digrest = 0 OR lv_digrest = 1.
      lv_second_dv = 0.
    ELSE.
      lv_second_dv = 11 - lv_digrest.
    ENDIF.

    lv_i     = ( lv_first_dv * 10 ) + lv_second_dv.

    IF lv_i EQ 0.
      lv_digit = '00'.
    ELSE.
      MOVE lv_i TO lv_digit.
    ENDIF.

    r_result = |{ lv_cnpj_sem_dv }{ lv_digit }|.

    DO. " Check if CNPJ is Valid, else get another and try again
      CALL FUNCTION 'CONVERSION_EXIT_CGCBR_INPUT'
        EXPORTING
          input     = r_result      " CGC in screen format (99.999.999/9999-99)
        EXCEPTIONS
          not_valid = 1
          OTHERS    = 2.

      IF sy-subrc NE 0.
        CALL METHOD return_random_cnpj
          RECEIVING
            r_result = r_result.
      ELSE.
        EXIT. "EXIT DO
      ENDIF.
    ENDDO.

  ENDMETHOD.

ENDCLASS.
