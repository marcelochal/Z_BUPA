class ZCL_IM_BUPA_FURTHER_CHECKS definition
  public
  final
  create public .

public section.

  interfaces IF_EX_BUPA_FURTHER_CHECKS .
protected section.
private section.
ENDCLASS.



CLASS ZCL_IM_BUPA_FURTHER_CHECKS IMPLEMENTATION.


  METHOD if_ex_bupa_further_checks~check_central.
* Solicit. : RITM0048055
* Motivo   : Validacao do BP por agrupamento

    " Agrupamento de BP para area Fiscal
    AUTHORITY-CHECK OBJECT 'ZGROUP_01'
     ID 'ZGROUPING' FIELD iv_group.
    IF syst-subrc NE 0.

*==========================================================================================
      " Agrupamento de BP para area de Suprimentos
      AUTHORITY-CHECK OBJECT 'ZGROUP_02'
        ID 'ZGROUPING' FIELD iv_group.
      IF syst-subrc NE 0.

*==========================================================================================
        " Agrupamento de BP para area Financeira
        AUTHORITY-CHECK OBJECT 'ZGROUP_03'
          ID 'ZGROUPING' FIELD iv_group.
        IF syst-subrc NE 0.

*==========================================================================================
          " Agrupamento de BP para area de Recursos Humanos
          AUTHORITY-CHECK OBJECT 'ZGROUP_04'
            ID 'ZGROUPING' FIELD iv_group.

          " If no authorization has been granted, return the error message
          IF syst-subrc NE 0.
            APPEND INITIAL LINE TO et_return ASSIGNING FIELD-SYMBOL(<return>).

            "Usuário & sem autorização para agrupamento &
            MESSAGE ID 'ZWF' TYPE 'E' NUMBER '000'
              WITH syst-uname iv_group INTO <return>-message.

            <return>-id         = syst-msgid.
            <return>-type       = syst-msgty.
            <return>-number     = syst-msgno.
            <return>-message_v1 = syst-msgv1.
            <return>-message_v2 = syst-msgv2.

          ENDIF.  " ZGROUP_04
        ENDIF.    " ZGROUP_03
      ENDIF.      " ZGROUP_02
    ENDIF.        " ZGROUP_01

  ENDMETHOD.
ENDCLASS.
