*&---------------------------------------------------------------------*
*& Report ZADDROLETOUSER
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT ZADDROLETOUSER.



PARAMETERS: p_uname type sy-uname OBLIGATORY,
            p_role  type BAPIAGR-AGR_NAME default 'FI_ALL'.

DATA: ld_uname type sy-uname,
      it_return type STANDARD TABLE OF BAPIRET2,
      wa_return like line of it_return,
      it_ag type STANDARD TABLE OF BAPIAGR,
      wa_ag like line of it_ag,
      wa_logondata type BAPILOGOND ,
      wa_LOGONDATAX type BAPILOGONX.


****************************************************************
*START-OF-SELECTION.
START-OF-SELECTION.

*Get current list of roles assigned to user
  CALL FUNCTION 'BAPI_USER_GET_DETAIL'
    EXPORTING
      USERNAME       = p_uname
    IMPORTING
      logondata      = wa_logondata
    TABLES
      ACTIVITYGROUPS = it_ag
      RETURN         = it_return.

* Add role to it_ag table
  wa_ag-AGR_NAME = p_role.
  wa_ag-FROM_DAT = sy-datum.
  wa_ag-TO_DAT   = sy-datum.
  append wa_ag to it_ag.

* Write list of roles back to user, including new one
  CALL FUNCTION 'BAPI_USER_ACTGROUPS_ASSIGN'
    EXPORTING
      USERNAME       = p_uname
    TABLES
      ACTIVITYGROUPS = it_ag
      RETURN         = it_return.

  CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
    EXPORTING
      WAIT = 'X'.
