*----------------------------------------------------------------------*
*                      ___    _     __    __    _                      *
*                       |    |_|   |_    (_    |_|                     *
*                       |    | |   |__   __)   | |                     *
*                                                                      *
*----------------------------------------------------------------------*
*            TRANSMISSORA ALIANÇA DE ENERGIA ELÉTRICA S.A.             *
*----------------------------------------------------------------------*
* ABAP Developer ..: Marcelo Alvares (MA004818)                        *
* Business Consult.: Marcelo Alvares (MA004818)                        *
* Module ..........: BUPA - Business partner                           *
* Program     .....: ZBUPA_0002                                        *
* Transaction .....: ZBP004                                            *
* Type        .....: Report Include                                    *
* Objective   .....: Business partners load                            *
* Request     .....: SHDK908669                                        *
*----------------------------------------------------------------------*
* Change Control:                                                      *
* Date       | Who                                      |Task          *
* 08.05.2020 | Marcelo Alvares (MA004818)               |RITM0077873   *
************************************************************************
* Include for UNIT CLASS METHODS                                       *
*                                                                      *
*&---------------------------------------------------------------------*
*& Include          ZBUPA_0002_T99
*&---------------------------------------------------------------------*


CLASS lcl_unit_test DEFINITION
    INHERITING FROM lcl_zbupa_0002
    FOR TESTING DURATION SHORT RISK LEVEL HARMLESS FRIENDS lcl_file_upload.

  PUBLIC SECTION.

    DATA:
      go_main_class TYPE REF TO lcl_unit_test,
      gt_test_file  TYPE lcl_file_upload=>ty_t_upload_general_data.

    CLASS-METHODS:
      return_random_cnpj
        RETURNING
          VALUE(r_result) TYPE stcd1,
      return_random_cpf
        RETURNING
          VALUE(r_result) TYPE stcd1.

    METHODS:
      testing_report FOR TESTING,
      fill_upload_file,

      "! Get random vendor for testing
      "! @parameter ch_s_upload_file | Dados de upload
      get_random_vendor_for_use
        CHANGING
          ch_s_upload_file TYPE lcl_file_upload=>ty_s_upload_general_data.

ENDCLASS.


CLASS lcl_unit_test IMPLEMENTATION.

  METHOD testing_report.

    DATA: lt_valutab        TYPE STANDARD TABLE OF rsparams WITH DEFAULT KEY,
          lr_dyn_line_table TYPE REF TO data.

    FIELD-SYMBOLS <lt_screen_tab> TYPE STANDARD TABLE.

    CALL FUNCTION 'RS_VARIANT_CONTENTS'
      EXPORTING
        report  = syst-repid                            " Report name
        variant = lcl_zbupa_0002=>co_default_variant    " Variant name
      TABLES
        valutab = lt_valutab                            " Table that contains the values (P + S)
      EXCEPTIONS
        OTHERS  = 3.

    LOOP AT lt_valutab ASSIGNING FIELD-SYMBOL(<ls_valutab>).

      ASSIGN (<ls_valutab>-selname) TO FIELD-SYMBOL(<lv_screen_field>).

      IF syst-subrc IS INITIAL.

        DESCRIBE FIELD <lv_screen_field> TYPE DATA(lv_tdescr1).

        CASE lv_tdescr1.
          WHEN 'C'.

            <lv_screen_field> = <ls_valutab>-low.

          WHEN 'u'.

            DATA(lv_range_tab) = <ls_valutab>-selname && '[]'.

            ASSIGN (lv_range_tab) TO <lt_screen_tab>.

            IF syst-subrc IS INITIAL.
              CREATE DATA lr_dyn_line_table LIKE LINE OF <lt_screen_tab>.

              ASSIGN lr_dyn_line_table->* TO FIELD-SYMBOL(<ls_screen_tabline>).

              MOVE-CORRESPONDING <ls_valutab> TO <ls_screen_tabline>.
              APPEND <ls_screen_tabline> TO <lt_screen_tab>.
            ENDIF.

        ENDCASE.

      ENDIF.

    ENDLOOP.

    CREATE OBJECT go_main_class.
    go_main_class->fill_upload_file( ).
    go_main_class->create_bp( ).
    go_main_class->alv_display( ).


    ASSIGN go_main_class->gt_test_file[ 1 ] TO FIELD-SYMBOL(<ls_test_file>).

    READ TABLE me->go_main_class->gt_bp_data
        ASSIGNING FIELD-SYMBOL(<ls_bp_data>)
        WITH TABLE KEY taxnum_key COMPONENTS taxnum = <ls_test_file>-taxnum.

    cl_abap_unit_assert=>assert_subrc(
      EXPORTING
        exp              = 0                                " Expected return code, optional, if not zero
        act              = sy-subrc                         " Return code of ABAP statements
        msg              = 'Erro na estrutura principal'    " Description
        level            = if_aunit_constants=>fatal        " Severity (TOLERABLE, >CRITICAL<, FATAL)
        quit             = if_aunit_constants=>class  ).    " Alter control flow/ quit test (NO, >METHOD<, CLASS)

    " Msg expected: Business Partner &1 &2 successfully created!
    READ TABLE <ls_bp_data>-return_bp ASSIGNING FIELD-SYMBOL(<ls_return_bp>) WITH KEY id    = 'Z_BUPA'
                                                                                    type    = 'S'
                                                                                    number  = '038'.

    cl_abap_unit_assert=>assert_subrc(
      EXPORTING
        exp              = 0                                " Expected return code, optional, if not zero
        act              = sy-subrc                         " Return code of ABAP statements
        msg              = 'Verificação de BP criado com sucesso'                             " Description
        level            = if_aunit_constants=>critical     " Severity (TOLERABLE, >CRITICAL<, FATAL)
        quit             = if_aunit_constants=>no           " Alter control flow/ quit test (NO, >METHOD<, CLASS)
    ).

**********************************************************************

    ASSIGN go_main_class->gt_test_file[ 2 ] TO <ls_test_file>.

    READ TABLE me->go_main_class->gt_bp_data
        ASSIGNING <ls_bp_data>
        WITH TABLE KEY taxnum_key COMPONENTS taxnum = <ls_test_file>-taxnum.

    cl_abap_unit_assert=>assert_subrc(
      EXPORTING
        exp              = 0                                " Expected return code, optional, if not zero
        act              = sy-subrc                         " Return code of ABAP statements
        msg              = 'Erro na estrutura principal'    " Description
        level            = if_aunit_constants=>fatal        " Severity (TOLERABLE, >CRITICAL<, FATAL)
        quit             = if_aunit_constants=>class  ).    " Alter control flow/ quit test (NO, >METHOD<, CLASS)

    " Msg espected: Business Partner &1 &2 successfully created!
    READ TABLE <ls_bp_data>-return_bp ASSIGNING <ls_return_bp> WITH KEY id    = 'Z_BUPA'
                                                                      type    = 'S'
                                                                      number  = '037'.

    cl_abap_unit_assert=>assert_subrc(
      EXPORTING
        exp              = 0                                " Expected return code, optional, if not zero
        act              = sy-subrc                         " Return code of ABAP statements
        msg              = 'Verificação de BP já existente' " Description
        level            = if_aunit_constants=>critical     " Severity (TOLERABLE, >CRITICAL<, FATAL)
        quit             = if_aunit_constants=>no           " Alter control flow/ quit test (NO, >METHOD<, CLASS)
    ).

**********************************************************************

    ASSIGN go_main_class->gt_test_file[ 3 ] TO <ls_test_file>.

    READ TABLE me->go_main_class->gt_bp_data
        ASSIGNING <ls_bp_data>
        WITH TABLE KEY taxnum_key COMPONENTS taxnum = <ls_test_file>-taxnum.

    cl_abap_unit_assert=>assert_subrc(
      EXPORTING
        exp              = 0                                " Expected return code, optional, if not zero
        act              = sy-subrc                         " Return code of ABAP statements
        msg              = 'Erro na estrutura principal'    " Description
        level            = if_aunit_constants=>fatal        " Severity (TOLERABLE, >CRITICAL<, FATAL)
        quit             = if_aunit_constants=>class  ).    " Alter control flow/ quit test (NO, >METHOD<, CLASS)

    " Msg expected: Business Partner &1 &2 successfully created!
    READ TABLE <ls_bp_data>-return_bp ASSIGNING <ls_return_bp> WITH KEY id    = 'Z_BUPA'
                                                                      type    = 'S'
                                                                      number  = '038'.

    cl_abap_unit_assert=>assert_subrc(
      EXPORTING
        exp              = 0                            " Expected return code, optional, if not zero
        act              = sy-subrc                     " Return code of ABAP statements
        msg              = 'Verificação de BP criado com sucesso'                             " Description
        level            = if_aunit_constants=>critical " Severity (TOLERABLE, >CRITICAL<, FATAL)
        quit             = if_aunit_constants=>no       " Alter control flow/ quit test (NO, >METHOD<, CLASS)
    ).

  ENDMETHOD.

  METHOD get_random_vendor_for_use.
*  All data searches are done using vendor information,
*  I used this feature to check for any difference between BP and vendor

    DATA:
      ls_lfa1      TYPE lfa1,
      lo_ran       TYPE REF TO cl_abap_random_int,
      lv_i         TYPE i,
      lv_lifnr_min TYPE lifnr,
      lv_lifnr_max TYPE lifnr.

    SELECT  MAX( lifnr ) FROM lfa1 INTO @lv_lifnr_max
           WHERE ktokk IN ('NACJ','NACF') AND
               ( stcd1 NE @space OR stcd2 NE @space ). " CNPJ ou CPF preenchidos

    lv_i = lv_lifnr_max.

    " Get random vendor from selected
    TRY.
        lo_ran = cl_abap_random_int=>create(
               seed = cl_abap_random=>seed( )
               min  = lv_i - 500
               max  = lv_i ).

        lv_lifnr_min = lo_ran->get_next( ).

      CATCH cx_abap_random. " Exception for CL_ABAP_RANDOM* = 1 max = syst-dbcnt ).
        " Deu Ruim
    ENDTRY.

    UNPACK lv_lifnr_min TO lv_lifnr_min.

    " Mestre de fornecedores (parte geral)
    SELECT SINGLE a1~* FROM lfa1 AS a1
    INNER JOIN lfbk AS bk ON a1~lifnr = bk~lifnr
    INTO CORRESPONDING FIELDS OF @ls_lfa1
        BYPASSING BUFFER
        WHERE a1~lifnr BETWEEN @lv_lifnr_min AND @lv_lifnr_max  .

    IF ls_lfa1-stcd1 IS NOT INITIAL.
      ch_s_upload_file-taxnum = ls_lfa1-stcd1.
    ELSE.
      ch_s_upload_file-taxnum = ls_lfa1-stcd2.
    ENDIF.

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

  METHOD fill_upload_file.

    APPEND INITIAL LINE TO me->gt_test_file ASSIGNING FIELD-SYMBOL(<ls_upload_file>).

    " Fill with new CNPJ
    <ls_upload_file>-taxnum      = return_random_cnpj( ).
    <ls_upload_file>-name        = |TESTE ABAP UNIT { syst-timlo }1{ syst-datlo } ZBP002|.
    <ls_upload_file>-street      = |Rua do Mercado|.
    <ls_upload_file>-house_no    = syst-timlo+3(3).
    <ls_upload_file>-postl_cod1  = |20010-120|.
    <ls_upload_file>-city        = |Rio de Janeiro|.
    <ls_upload_file>-district    = |Centro|.
    <ls_upload_file>-region      = |RJ|.
    <ls_upload_file>-tel_number  = |(21)34 { syst-timlo }|.
    <ls_upload_file>-smtp_addr   = |teste{ syst-timlo }@teste.com.br|.
    <ls_upload_file>-banco       = '001'.
    <ls_upload_file>-bankl       = '1403'.                  " 00191403
    <ls_upload_file>-bankn       = syst-timlo.


    APPEND INITIAL LINE TO me->gt_test_file ASSIGNING <ls_upload_file>.
    MOVE-CORRESPONDING me->gt_test_file[ 1 ] TO <ls_upload_file>.

    get_random_vendor_for_use(
      CHANGING
        ch_s_upload_file = <ls_upload_file>   ).

    APPEND INITIAL LINE TO me->gt_test_file ASSIGNING <ls_upload_file>.
    MOVE-CORRESPONDING me->gt_test_file[ 1 ] TO <ls_upload_file>.
    <ls_upload_file>-taxnum      = return_random_cnpj( ).
    <ls_upload_file>-name        = |TESTE ABAP UNIT { syst-timlo }2{ syst-datlo } ZBP002|.
    CLEAR: <ls_upload_file>-banco, <ls_upload_file>-bankl, <ls_upload_file>-bankn.

    APPEND INITIAL LINE TO me->gt_test_file ASSIGNING <ls_upload_file>.
    MOVE-CORRESPONDING me->gt_test_file[ 1 ] TO <ls_upload_file>.
    <ls_upload_file>-taxnum      = return_random_cnpj( ).
    <ls_upload_file>-name        = |TESTE ABAP UNIT { syst-timlo }3{ syst-datlo } ZBP002|.
    <ls_upload_file>-banco       = '000'.
    <ls_upload_file>-bankl       = '0000'.
    <ls_upload_file>-bankn       = syst-timlo.


    MOVE-CORRESPONDING me->gt_test_file TO me->go_file->gt_upload_file.

  ENDMETHOD.

  METHOD return_random_cpf.

    DATA:
      lo_ran       TYPE REF TO cl_abap_random_int,
      lv_seed      TYPE i,
      lv_cpf_core  TYPE c LENGTH 9,
      lv_cpf       TYPE c LENGTH 11,
      lv_i         TYPE p,
      lv_j         TYPE p,
      lv_digsum    TYPE p,
      lv_digrest   TYPE p,
      lv_digit     TYPE c LENGTH 2 VALUE '00',
      lv_first_dv  TYPE i,
      lv_second_dv TYPE i.

    " Get random vendor from selected
    TRY.
        lv_seed = cl_abap_random=>seed( ).

        lo_ran = cl_abap_random_int=>create(
               seed = lv_seed
               min  = 1000000
               max  = 999999999 ).

        lv_cpf_core = lo_ran->get_next( ).

        lv_cpf_core = |{ lv_cpf_core ALPHA = IN }|.

      CATCH cx_abap_random. " Exception for CL_ABAP_RANDOM* = 1 max = syst-dbcnt ).
        " Deu Ruim
    ENDTRY.

**********************************************************************

*
* Checking Control-digits
*
* CPF                    X  X X.XXX.XXX-XX
*                        |  | | ||| ||| ||
* First  Control-digit  10  9 8 765 432
* Second Control-digit  11 10 9 876 543 2
*
    lv_cpf      = lv_cpf_core && lv_digit.
    lv_digsum   = 0.
    lv_i        = 0.

    DO  9 TIMES.
      lv_i = lv_i + 1.
      lv_j = 11 - lv_i.
      WRITE: lv_cpf+0(1) TO lv_digit.
      lv_digsum = lv_digsum + ( lv_digit * lv_j ).
      SHIFT lv_cpf LEFT.
    ENDDO.
*
    lv_digrest = lv_digsum MOD 11.
    IF lv_digrest = 0 OR lv_digrest = 1.
      lv_first_dv = 0.
    ELSE.
      lv_first_dv = 11 - lv_digrest.
    ENDIF.
*
    lv_cpf      = lv_cpf_core && '00'.
    lv_digsum   = 0.
    lv_i        = 0.
    DO  9 TIMES.
      lv_i = lv_i + 1.
      lv_j = 12 - lv_i.
      WRITE: lv_cpf+0(1) TO lv_digit.
      lv_digsum = lv_digsum + ( lv_digit * lv_j ).
      SHIFT lv_cpf LEFT.
    ENDDO.
    lv_digsum = lv_digsum + ( lv_first_dv * 2 ).
*
    lv_digrest = lv_digsum MOD 11.
    IF lv_digrest = 0 OR lv_digrest = 1.
      lv_second_dv = 0.
    ELSE.
      lv_second_dv = 11 - lv_digrest.
    ENDIF.
*
    lv_digit    = lv_first_dv * 10 + lv_second_dv.

    r_result = |{ lv_cpf_core }{ lv_digit }|.

    DO. " Check if CPF is Valid, else get another and try again
      CALL FUNCTION 'CONVERSION_EXIT_CPFBR_INPUT'
        EXPORTING
          input     = r_result         " CPF in screen format (999.999.999-99)
        EXCEPTIONS
          not_valid = 1
          OTHERS    = 2.

      IF sy-subrc NE 0.
        CALL METHOD return_random_cpf
          RECEIVING
            r_result = r_result.
      ELSE.
        EXIT. "EXIT DO
      ENDIF.

    ENDDO.

  ENDMETHOD.

ENDCLASS.
