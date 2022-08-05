CLASS zcl_bupa_tax_number_check DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    CONSTANTS:
      co_memoryid              TYPE rsbmstorageid VALUE 'ZBP_TAX_CHECK' ##NO_TEXT,
      co_grupos_bp_duplicaveis TYPE rvari_vnam VALUE 'ZCHECK_BP_GROUP_CRIA_DUPLICADO' ##NO_TEXT,
*      co_grupo_naco            TYPE bu_group        VALUE 'NACO' ##NO_TEXT,
      co_tax_cpf               TYPE bptaxtype VALUE 'BR2' ##NO_TEXT,
      co_tax_cnpj              TYPE bptaxtype VALUE 'BR1' ##NO_TEXT,
      BEGIN OF co_check_tax,
        off TYPE abap_bool      VALUE abap_off,
        on  TYPE abap_bool      VALUE abap_on,
      END OF co_check_tax .

    CLASS-METHODS class_constructor .
    CLASS-METHODS export_to_memory_bp_group
      IMPORTING
        !iv_bp_group TYPE bu_group .
    CLASS-METHODS check_bp_duplicity
      IMPORTING
        !iv_partner     TYPE bu_partner OPTIONAL
        !iv_taxnum      TYPE bptaxnum OPTIONAL
        !is_vendor      TYPE lfa1 OPTIONAL
      RETURNING
        VALUE(r_return) TYPE abap_bool .
  PROTECTED SECTION.


  PRIVATE SECTION.

    TYPES:
      ty_r_grupos TYPE RANGE OF bu_group .

    CLASS-DATA gr_groups_allowed TYPE ty_r_grupos .

    CLASS-METHODS is_group_allowed_duplicity
      IMPORTING
        !im_v_bp_group  TYPE bu_group OPTIONAL
      RETURNING
        VALUE(r_return) TYPE abap_bool .
    CLASS-METHODS is_valid_taxnum
      IMPORTING
        VALUE(im_v_taxnum) TYPE bptaxnum
      RETURNING
        VALUE(r_return)    TYPE abap_bool .
    CLASS-METHODS is_other_bp_allowed
      IMPORTING
        !im_v_taxnum  TYPE bptaxnum
        !im_v_partner TYPE bu_partner
      CHANGING
        !ch_b_return  TYPE abap_bool .
    CLASS-METHODS get_groups_allowed .
ENDCLASS.



CLASS ZCL_BUPA_TAX_NUMBER_CHECK IMPLEMENTATION.


  METHOD check_bp_duplicity.
* O parametro de retorno está condicionado a verificar ou não o CNPJ pelo standard
* Se o retorno for verdadeiro co_not_check 'X' o sistema sai por CHECK da função de
* verificação permitindo a criação
* Se for falso co_check_tax_id o sistema consulta o CNPJ e verifica que existe duplicidade.

    DATA:
      lv_bp_group TYPE bu_group,
      lv_taxnum   TYPE bptaxnum,
      lv_partner  TYPE bu_partner.

    BREAK-POINT ID zbupa_tax_number_check. " See SAAB Transaction!

    r_return = co_check_tax-on. " Default checks tax num

    " Extract local variable
    lv_taxnum  = iv_taxnum.
    lv_partner = iv_partner.

    " Check is valid CNPJ or CPF.
    CHECK is_valid_taxnum( lv_taxnum ).

    IF iv_partner IS NOT INITIAL.
* Busca o grupo do BP gravado em caso de modificação.
      CALL FUNCTION 'BUPA_CENTRAL_READ_DETAIL'
        EXPORTING
          iv_partner            = iv_partner
        IMPORTING
          ev_group              = lv_bp_group
        EXCEPTIONS
          no_partner_specified  = 1
          no_valid_record_found = 2
          not_found             = 3
          blocked_partner       = 4
          OTHERS                = 5.
    ENDIF.

    IF is_vendor IS NOT INITIAL.
      " Grupo de contas do fornecedor é igual ao Agrupamento de parceiros de negócios
      " O BP já está criado e é estendido para fornecedor, encontra um novo ponto de verificação.
      IF is_vendor-lifnr IS NOT INITIAL.
        lv_partner    = cvi_bp_vendor=>get_instance( i_vendor = is_vendor-lifnr )->get_partner( ).
      ENDIF.
      lv_bp_group   = is_vendor-ktokk. " Grupo de contas do fornecedor
      lv_taxnum     = is_vendor-stcd1. " Nº ID fiscal 1
    ENDIF.

* Verifica se pegou o grupo do BP, se caso vazio o BP não existe
* BP não foi salvo na criação

    IF lv_bp_group IS INITIAL.
* Pega os campos em tela da criação ou modificação do BP
* Parametro exportado antes da chamada da função
* ENHANCEMENT ZEX_BUPA_TAX_NUMBER_CHECK

      IMPORT p_bp_group TO lv_bp_group
           FROM MEMORY ID zcl_bupa_tax_number_check=>co_memoryid.

      "If you clear the memory, errors occur
      "The function call is in loop for each CNPJ / IE of the same BP
*      IF sy-subrc EQ 0.
*        FREE MEMORY ID zcl_bupa_tax_number_check=>cc_memoryid.
*      ENDIF.
    ENDIF.

    " Verifica se o agrupamento do BP permite Duplicidade
    IF is_group_allowed_duplicity( lv_bp_group ) EQ co_check_tax-off.
      r_return = co_check_tax-off.
      RETURN.
    ENDIF.

    "Verifica se todos os BPs com o mesmo CNPJ são de grupos que permitem duplicidade
*    r_return = is_group_must_be_verified( lv_bp_group ).

    is_other_bp_allowed(
      EXPORTING
        im_v_taxnum  = lv_taxnum
        im_v_partner = lv_partner
      CHANGING
        ch_b_return  = r_return ).

  ENDMETHOD.


  METHOD class_constructor.

    get_groups_allowed( ).

  ENDMETHOD.


  METHOD export_to_memory_bp_group.
    EXPORT p_bp_group FROM iv_bp_group
       TO MEMORY ID zcl_bupa_tax_number_check=>co_memoryid.

  ENDMETHOD.


  METHOD get_groups_allowed.

    "Encontra os grupos de conta que permitem duplicidade
    zcl_utils=>get_range(
      EXPORTING
        i_range_id      = co_grupos_bp_duplicaveis  " ABAP: nome da variável de variante
      IMPORTING
        ep_range        = gr_groups_allowed         " Tabela Interna com o Range
      EXCEPTIONS
        range_not_found = 1                         " Range não encontrado
        OTHERS          = 2 ).

  ENDMETHOD.


  METHOD is_group_allowed_duplicity.

    "Verifica se grupo do BP sendo criado permite CNPJ duplicados
    READ TABLE gr_groups_allowed WITH KEY low = im_v_bp_group
                                          TRANSPORTING NO FIELDS.
    IF sy-subrc IS INITIAL.
      r_return = co_check_tax-off.   " Is allowed not Check
    ELSE.
      r_return = co_check_tax-on.       " Check Tax ID
    ENDIF.

  ENDMETHOD.


  METHOD is_other_bp_allowed.
    DATA:
      lt_grupos         TYPE bu_group_t.

    CHECK gr_groups_allowed IS NOT INITIAL.

    "If existing BPs are NACO do not check for duplicity
    SELECT but~bu_group
           INTO TABLE lt_grupos
           FROM dfkkbptaxnum AS taxnum
           INNER JOIN but000 AS but ON taxnum~partner = but~partner

           WHERE ( taxnum~taxtype = co_tax_cnpj
                OR taxnum~taxtype = co_tax_cpf ) " BR1 or BR2
               AND taxnum~taxnum  = im_v_taxnum
               AND taxnum~partner NE im_v_partner " Desprezando o que está sendo processado.
               AND but~bu_group   NOT IN gr_groups_allowed.

    IF syst-subrc IS INITIAL. " RITM0056110 - Erro que permite criação duplicidade NACF
      " found BP with groups other than groups allowed
      ch_b_return = co_check_tax-on.        " Check Tax ID
    ELSE.
      ch_b_return = co_check_tax-off.    " NOT Check Tax ID
    ENDIF.

  ENDMETHOD.


  METHOD is_valid_taxnum.

    r_return = abap_false.

    CALL FUNCTION 'CONVERSION_EXIT_CGCBR_INPUT'
      EXPORTING
        input         = im_v_taxnum
      EXCEPTIONS
        not_valid     = 1
        error_message = 2
        OTHERS        = 3.

    IF sy-subrc EQ 0.
      r_return = abap_true.

    ELSE.

      CALL FUNCTION 'CONVERSION_EXIT_CPFBR_INPUT'
        EXPORTING
          input         = im_v_taxnum
        EXCEPTIONS
          not_valid     = 1
          error_message = 2
          OTHERS        = 3.

      IF sy-subrc EQ 0.
        r_return = abap_true.
      ENDIF.
    ENDIF.

  ENDMETHOD.
ENDCLASS.
