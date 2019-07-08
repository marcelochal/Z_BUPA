CLASS zcl_im_bupa_bank_check DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES if_ex_bupa_bank_check .

    METHODS get_acct_dv
      RETURNING
        VALUE(r_result) TYPE j_1bxh111 .
    METHODS get_branch_dv
      RETURNING
        VALUE(r_result) TYPE j_1bxh109 .
    METHODS get_branch
      RETURNING
        VALUE(r_result) TYPE zdidh08 .
    METHODS get_bank
      RETURNING
        VALUE(r_result) TYPE zdidh06 .
    METHODS get_bankdetail
      RETURNING
        VALUE(r_result) TYPE bapibus1006_bankdetail .
    METHODS get_bkvid
      RETURNING
        VALUE(r_result) TYPE bu_bkvid .
  PROTECTED SECTION.

  PRIVATE SECTION.

    DATA acct_dv TYPE j_1bxh111 .
    DATA branch_dv TYPE j_1bxh109 .
    DATA branch TYPE zdidh08 .
    DATA return TYPE bapiret2 .
    DATA bank TYPE zdidh06 .
    DATA bankdetail TYPE bapibus1006_bankdetail .
    DATA bkvid TYPE bu_bkvid .
    CONSTANTS c_error_ctrl_key_less_than_2  TYPE syst-msgno VALUE '001' ##NO_TEXT.
    CONSTANTS c_error_acct_is_not_numeric   TYPE syst-msgno VALUE '002' ##NO_TEXT.
    CONSTANTS c_error_acct_filled_with_dv   TYPE syst-msgno VALUE '003' ##NO_TEXT.
    CONSTANTS c_error_ctrl_key_is_initial   TYPE syst-msgno VALUE '004' ##NO_TEXT.
    CONSTANTS c_error_branch_dv_no_equal    TYPE syst-msgno VALUE '005' ##NO_TEXT.
    CONSTANTS c_error_acct_dv_no_equal      TYPE syst-msgno VALUE '006' ##NO_TEXT.

    METHODS check_bankdetail
      RETURNING
        VALUE(r_result) TYPE abap_bool .
    METHODS return_msg
      IMPORTING
        !iv_msgno TYPE syst-msgno
        !iv_msgv1 TYPE syst-msgv1 OPTIONAL ##NEEDED
        !iv_msgv2 TYPE syst-msgv2 OPTIONAL ##NEEDED
        !iv_msgv3 TYPE syst-msgv3 OPTIONAL ##NEEDED
        !iv_msgv4 TYPE syst-msgv4 OPTIONAL ##NEEDED.
    METHODS set_bkvid
      IMPORTING
        !iv_bkvid TYPE bu_bkvid .
    METHODS set_branch
      IMPORTING
        !iv_branch TYPE zdidh08 .
    METHODS set_bank
      IMPORTING
        !iv_bank TYPE zdidh06 .
    METHODS set_bankdetail
      IMPORTING
        !is_bankdetail TYPE bapibus1006_bankdetail .
    METHODS is_bank_in_validation
      RETURNING
        VALUE(r_result) TYPE abap_bool .
    METHODS set_acct_dv
      IMPORTING
        !iv_acct_dv TYPE j_1bxh111 .
    METHODS set_branch_dv
      IMPORTING
        !iv_branch_dv TYPE j_1bxh109 .
    METHODS determine_check_digit .
    METHODS get_mod10
      IMPORTING
        !iv_number      TYPE bapibus1006_bankdetail-bank_acct
      RETURNING
        VALUE(r_result) TYPE char1.
    METHODS get_mod11
      IMPORTING
        VALUE(iv_number) TYPE c
      RETURNING
        VALUE(r_dv)      TYPE char1 .
ENDCLASS.



CLASS ZCL_IM_BUPA_BANK_CHECK IMPLEMENTATION.


  METHOD check_bankdetail.

*   Conta preenchida com o digito verificador
    IF me->bankdetail-bank_acct CS '-'.
      me->return_msg( me->c_error_acct_filled_with_dv ).
      r_result = abap_true.
      RETURN.
    ENDIF.

*   Conta contém caracteres não numéricos.
    IF me->bankdetail-bank_acct CN '1234567890 '.
      me->return_msg( me->c_error_acct_is_not_numeric ).
      r_result = abap_true.
      RETURN.
    ENDIF.

*    Digito verificador não preenchido
    IF me->bankdetail-ctrl_key IS INITIAL.
      me->return_msg( me->c_error_ctrl_key_is_initial ).
      r_result = abap_true.
      RETURN.
    ENDIF.

*   Digito menor que 2
    IF strlen( me->bankdetail-ctrl_key ) NE 2.
      me->return_msg( me->c_error_ctrl_key_less_than_2 ).
      r_result = abap_true.
      RETURN.
    ENDIF.

    r_result = abap_false. "NO ERROR

  ENDMETHOD.


  METHOD determine_check_digit.

    CASE me->get_bank( ).
      WHEN zcl_boleto=>c_santander. "033

*    Para o banco santander o calculo do DV ocorre depois dos dois primeros numeros 01 C/C 13 Poupança etc...
        me->set_acct_dv(    iv_acct_dv   = me->get_mod11( iv_number = me->bankdetail-bank_acct+2(8) ) ).
        me->set_branch_dv(  iv_branch_dv = me->get_mod11( iv_number = me->get_branch( )             ) ).

      WHEN zcl_boleto=>c_itau. "271

        me->set_acct_dv(    iv_acct_dv      = me->get_mod10( iv_number = me->bankdetail-bank_acct ) ).
        me->set_branch_dv(  iv_branch_dv    = me->get_mod10( iv_number = me->bankdetail-bank_acct ) ).

      WHEN OTHERS.

        me->set_acct_dv(    iv_acct_dv   = me->get_mod11( iv_number = me->bankdetail-bank_acct )  ).
        me->set_branch_dv(  iv_branch_dv = me->get_mod11( iv_number = me->get_branch( )        )  ).

    ENDCASE.

  ENDMETHOD.


  METHOD get_acct_dv.
    r_result = me->acct_dv.
  ENDMETHOD.


  METHOD get_bank.
    r_result = me->bank.
  ENDMETHOD.


  METHOD get_bankdetail.
    r_result = me->bankdetail.
  ENDMETHOD.


  METHOD get_bkvid.
    r_result = me->bkvid.
  ENDMETHOD.


  METHOD get_branch.
    r_result = me->branch.
  ENDMETHOD.


  METHOD get_branch_dv.
    r_result = me->branch_dv.
  ENDMETHOD.


  METHOD get_mod10.

    DATA vl_cd TYPE c.

    CALL FUNCTION 'CALCULATE_CHECK_DIGIT_MOD10'
      EXPORTING
        number_part = iv_number
      IMPORTING
        check_digit = vl_cd.

    r_result = vl_cd.

  ENDMETHOD.


  METHOD get_mod11.


    CALL METHOD zcl_boleto=>calculate_check_digit_mod11
      EXPORTING
        number_part = iv_number " Dado de entrada
      IMPORTING
        check_digit = r_dv.     " Digito Verificador


  ENDMETHOD.


  METHOD if_ex_bupa_bank_check~check.

*Evitar que pagamentos sejam devolvidos pelos bancos pagadores por informações incorretas nos cadastros dos BP
*cálculo e checagem dos dígitos verificadores de agências e contas, que pode ser o módulo 10 ou 11.
*Abaixo, lista dos bancos que deverão ser validados e por qual regra:
*
*BB         | 001     | 11
*Santander  | 033     | 11
*CEF        | 104     | 11
*Bradesco   | 237     | 11
*Itaú       | 341     | 10
*Sicoob     | 756     | 11
*Sicredi    | 748     | 11

    me->set_bankdetail( is_bankdetail = is_bankdetail ).
    me->set_bkvid( iv_bkvid = iv_bkvid ).

    CHECK:
* Verifica se é uma criação ou alteração, se não sai da BADI
        iv_activity EQ '01' OR "01= Create
        iv_activity EQ '02',   "02= Change
        is_bankdetail-bank_acct IS NOT INITIAL,         "Conta corrente em branco, validação standard
        is_bankdetail-bank_ctry EQ 'BR',                "Despreza bancos extrangeiros
        me->is_bank_in_validation( ) IS NOT INITIAL,    "Verifica se o banco está no escopo
*        is_bankdetail_x IS NOT INITIAL,                 " 'X'  = Has changed Somente verifica se houve modificação, salvo errado fica errado
        iv_dont_check_bank_conn IS INITIAL.             " 'X'  = Do Not Check Bank Details ID Yet

    IF me->check_bankdetail( ).

      APPEND me->return TO et_return.
      RETURN.

    ENDIF.

    me->determine_check_digit( ).

    IF me->get_branch_dv( ) NE is_bankdetail-ctrl_key(1).
      me->return_msg( me->c_error_branch_dv_no_equal ).
      APPEND me->return TO et_return.
      RETURN.
    ENDIF.

    IF me->get_acct_dv( ) NE is_bankdetail-ctrl_key+1(1).
      me->return_msg( me->c_error_acct_dv_no_equal ).
      APPEND me->return TO et_return.
      RETURN.
    ENDIF.

  ENDMETHOD.


  METHOD is_bank_in_validation.

*Verifica se o banco está no escopo de verificação do DV
    CHECK:
       me->get_bank( ) EQ zcl_boleto=>c_banco_do_brasil OR
       me->get_bank( ) EQ zcl_boleto=>c_santander       OR
       me->get_bank( ) EQ zcl_boleto=>c_cef             OR
       me->get_bank( ) EQ zcl_boleto=>c_bradesco        OR
       me->get_bank( ) EQ zcl_boleto=>c_itau            OR
       me->get_bank( ) EQ zcl_boleto=>c_citibank        OR
       me->get_bank( ) EQ '748' OR "Sicredi  748 MOD 11
       me->get_bank( ) EQ '756'. "Sicoob   756 MOD 11

*Se caso estiver na lista retorna verdadeiro
    r_result = abap_true.

*Caso não esteja na lista então não faz parte do escopo
*retorna falso (vazio) para sair da BADI

  ENDMETHOD.


  METHOD return_msg.
    DATA:
      lv_msgv1 TYPE syst-msgv1 VALUE IS INITIAL,
      lv_msgv2 TYPE syst-msgv2 VALUE IS INITIAL,
      lv_msgv3 TYPE syst-msgv3 VALUE IS INITIAL,
      lv_msgv4 TYPE syst-msgv4 VALUE IS INITIAL.

    CASE iv_msgno.
      WHEN c_error_ctrl_key_less_than_2.

        lv_msgv1 = me->get_bkvid( ).

      WHEN c_error_acct_is_not_numeric OR
           c_error_acct_filled_with_dv.

        "A conta bancaria &1 para o ID &2 possui dígitos não numéricos
        lv_msgv1 = me->bankdetail-bank_acct.
        lv_msgv2 = me->get_bkvid( ).

      WHEN  c_error_branch_dv_no_equal  OR
            c_error_acct_dv_no_equal    OR
            c_error_ctrl_key_is_initial.

        lv_msgv1 = me->get_bkvid( ).

        TRY.
*  Converte o DV agencia e banco para base62 e informa na mensagem afim de facilitar a testes.
            lv_msgv4 = /ui2/cl_number=>base_converter(
                         number   = me->get_branch_dv( ) && me->get_acct_dv( )
                         from     = 10
                         to       = 62               ).
          CATCH cx_sy_move_cast_error ##NO_HANDLER. " Superclass for All System Exceptions
*          Não é necessario fazer o tratamento de exceções
        ENDTRY.

    ENDCASE.

    MESSAGE ID 'Z_BUPA' TYPE 'E' NUMBER iv_msgno
      WITH lv_msgv1 lv_msgv2 lv_msgv3 lv_msgv4
      INTO DATA(dummy) ##NEEDED.

    me->return-id          = syst-msgid.
    me->return-type        = syst-msgty.
    me->return-number      = syst-msgno.
    me->return-type        = syst-msgty.
    me->return-message_v1  = syst-msgv1.
    me->return-message_v2  = syst-msgv2.
    me->return-message_v3  = syst-msgv3.
    me->return-message_v4  = syst-msgv4.

  ENDMETHOD.


  METHOD set_acct_dv.
    me->acct_dv = iv_acct_dv.
  ENDMETHOD.


  METHOD set_bank.
    me->bank = iv_bank.
  ENDMETHOD.


  METHOD set_bankdetail.
    me->bankdetail = is_bankdetail.
    me->set_bank( iv_bank = is_bankdetail-bank_key(3) ).
    me->set_branch( iv_branch = is_bankdetail-bank_key+4(4) ).
  ENDMETHOD.


  METHOD set_bkvid.
    me->bkvid = iv_bkvid.
  ENDMETHOD.


  METHOD set_branch.
    me->branch = iv_branch.
  ENDMETHOD.


  METHOD set_branch_dv.
    me->branch_dv = iv_branch_dv.
  ENDMETHOD.
ENDCLASS.
