"Name: \TY:CL_BUPA_USER\ME:CAN_CHANGE_PARTNER\SE:END\EI
ENHANCEMENT 0 ZEX_BUPA_CHECK_GROUP.

*{   INSERT         SHDK904854                                        1
* Consultor: Luque
* Empresa  : SAP
* Solicit. : RITM0048055
* Motivo   : Validacao do BP por agrupamento

  DATA: lv_bu_group TYPE bu_group,
        lt_bapiret2 TYPE bapiret2_t.

  SELECT SINGLE bu_group
    FROM but000
    INTO lv_bu_group
   WHERE partner_guid = iv_partner_guid.

  DATA(lo_bupa_checks) = NEW zcl_im_bupa_further_checks( ).

  lo_bupa_checks->if_ex_bupa_further_checks~check_central(
    EXPORTING
      iv_activity                 = '02'                " 01 = Add; 02 = Change
      iv_group                    = lv_bu_group         " Business Partner Grouping
    CHANGING
      et_return                   = lt_bapiret2 ).      " Return table

  " Error found
  IF lt_bapiret2 IS NOT INITIAL.

    READ TABLE lt_bapiret2 ASSIGNING FIELD-SYMBOL(<bapire>) WITH KEY type = 'E'.

    IF <bapire> IS ASSIGNED.
      MESSAGE ID <bapire>-id TYPE <bapire>-type NUMBER <bapire>-number
      WITH <bapire>-message_v1 <bapire>-message_v2  RAISING not_allowed.
    ENDIF.
  ENDIF.

ENDENHANCEMENT.
