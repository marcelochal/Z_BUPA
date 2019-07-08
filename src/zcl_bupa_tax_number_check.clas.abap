CLASS zcl_bupa_tax_number_check DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    CONSTANTS:
      co_memoryid              TYPE rsbmstorageid  VALUE 'ZBP_TAX_CHECK' ##NO_TEXT,
      co_grupos_pj             TYPE rvari_vnam     VALUE 'ZCHECK_BP_GROUP_PJ' ##NO_TEXT,
      co_tax_cnpj              TYPE bptaxtype      VALUE 'BR1' ##NO_TEXT,
      co_grupos_bp_duplicaveis TYPE rvari_vnam     VALUE 'ZCHECK_BP_GROUP_CRIA_DUPLICADO' ##NO_TEXT,
      co_grupo_naco            TYPE bu_group       VALUE 'NACO' ##NO_TEXT,
      co_not_check             TYPE abap_bool      VALUE abap_false,
      co_check_tax_id          TYPE abap_bool      VALUE abap_true.

    CLASS-METHODS export_to_memory_bp_group
      IMPORTING
        !iv_bp_group TYPE bu_group .

    CLASS-METHODS check_bp_duplicity_check
      IMPORTING
        !iv_partner     TYPE bu_partner OPTIONAL
        !iv_taxnum      TYPE bptaxnum   OPTIONAL
        !is_vendor      TYPE lfa1       OPTIONAL
      RETURNING
        VALUE(r_return) TYPE abap_bool.
  PROTECTED SECTION.

  PRIVATE SECTION.

    TYPES:
      BEGIN OF ty_s_grupos,
        bu_group TYPE bu_group,
      END OF ty_s_grupos,

      ty_t_grupos TYPE STANDARD TABLE OF ty_s_grupos.

    CLASS-METHODS:
      get_duplicate_bp_group_allowed
        RETURNING
          VALUE(r_return) TYPE abap_bool,
      is_group_allowed_duplicity
        IMPORTING
          im_v_bp_group   TYPE bu_group
        RETURNING
          VALUE(r_return) TYPE abap_bool,
      is_group_must_be_verified
        IMPORTING
          im_v_bp_group   TYPE bu_group
        RETURNING
          VALUE(r_return) TYPE abap_bool.

ENDCLASS.



CLASS zcl_bupa_tax_number_check IMPLEMENTATION.


  METHOD check_bp_duplicity_check.
* O parametro de retorno está condicionado a verificar ou não o CNPJ pelo standard
* Se o retorno for verdadeiro co_not_check 'X' o sistema sai por CHECK da função de
* verificação permitindo a criação
* Se for falso co_check_tax_id o sistema consulta o CNPJ e verifica que existe duplicidade.

    DATA:
      lt_grupos   TYPE ty_t_grupos,
      lv_bp_group TYPE bu_group,
      lv_taxnum   TYPE bptaxnum.

    r_return = co_check_tax_id. " Default checks tax num

    " Extract local variable
    lv_taxnum = iv_taxnum.

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
      lv_bp_group   = is_vendor-ktokk. " Grupo de contas do fornecedor
      lv_taxnum     = is_vendor-stcd1. " Nº ID fiscal 1
    ENDIF.

    "CHECK sy-subrc IS INITIAL.

* Verifica se pegou o grupo do BP, se caso vazio o BP não existe
* BP não foi salvo na criação

    IF lv_bp_group IS INITIAL.
* Pega os campos em tela da criação ou modificação do BP
* Parametro exportado antes da chamada da função
* ENHANCEMENT ZEX_BUPA_TAX_NUMBER_CHECK

      IMPORT p_bp_group TO lv_bp_group
           FROM MEMORY ID zcl_bupa_tax_number_check=>co_memoryid.

      "Se limpar a memoria, ocorre erros
      "A chamada da função está em loop para cada CNPJ/IE do mesmo BP
*      IF sy-subrc EQ 0.
*        FREE MEMORY ID zcl_bupa_tax_number_check=>cc_memoryid.
*      ENDIF.
    ENDIF.

    IF is_group_allowed_duplicity( lv_bp_group ) EQ co_not_check.
      r_return = co_not_check.
      RETURN.

    ELSE.  "Verifica se todos os BPs com o mesmo CNPJ são de grupos que permitem duplicidade

      IF is_group_must_be_verified( lv_bp_group ) EQ co_check_tax_id.

        r_return = co_check_tax_id. " Check Tax ID "Não é NACJ, deve checar duplicidade
        RETURN.
      ELSE.

        "É NACJ, se os BPs existentes forem NACO não cheque a duplicidade
        SELECT b~bu_group INTO TABLE lt_grupos
          FROM dfkkbptaxnum AS d
         INNER JOIN but000  AS b
                            ON d~partner = b~partner
         WHERE d~taxtype = co_tax_cnpj
           AND d~taxnum  = lv_taxnum
* Desprezando o que está sendo processado.
           AND d~partner  NE iv_partner "22/02/2019 - RA 122
           AND b~bu_group NE co_grupo_naco.

        IF syst-subrc IS INITIAL. " Encontrou
          r_return = co_check_tax_id. " Check Tax ID
        ELSE.
          r_return = co_not_check. " NOT Check Tax ID
        ENDIF.
      ENDIF.
    ENDIF.

  ENDMETHOD.


  METHOD export_to_memory_bp_group.
    EXPORT p_bp_group FROM iv_bp_group
       TO MEMORY ID zcl_bupa_tax_number_check=>co_memoryid.

  ENDMETHOD.

  METHOD get_duplicate_bp_group_allowed.



  ENDMETHOD.

  METHOD is_group_allowed_duplicity.

    DATA:
        lt_grupos_bp_duplicaveis TYPE RANGE OF bu_group.

    "Encontra os grupos de conta que permitem duplicidade
    CALL METHOD zcl_utils=>get_range
      EXPORTING
        i_range_id      = co_grupos_bp_duplicaveis
      IMPORTING
        ep_range        = lt_grupos_bp_duplicaveis
      EXCEPTIONS
        range_not_found = 1
        OTHERS          = 2.

    "Verifica se grupo do BP sendo criado permite CNPJ duplicados
    READ TABLE lt_grupos_bp_duplicaveis WITH KEY low = im_v_bp_group
                                        TRANSPORTING NO FIELDS.
    IF sy-subrc IS INITIAL.
      r_return = co_not_check.    " Is allowed not Check
    ELSE.
      r_return = co_check_tax_id. " Check Tax ID
    ENDIF.

  ENDMETHOD.

  METHOD is_group_must_be_verified.

    DATA:
        lt_bp_group_check TYPE RANGE OF bu_group.

    "Encontra os grupos de conta que devem checar por duplicidade
    CALL METHOD zcl_utils=>get_range
      EXPORTING
        i_range_id      = co_grupos_pj
      IMPORTING
        ep_range        = lt_bp_group_check
      EXCEPTIONS
        range_not_found = 1
        OTHERS          = 2.

    "Verifica se grupo do BP deve checar duplicidade (NACJ)
    READ TABLE lt_bp_group_check WITH KEY low = im_v_bp_group
                                  TRANSPORTING NO FIELDS.

    IF sy-subrc IS INITIAL.
      r_return = co_check_tax_id. " Check Tax ID "Não é NACJ, deve checar duplicidade
    ELSE.
      r_return = co_not_check.    " Not Check
    ENDIF.

  ENDMETHOD.

ENDCLASS.
