*--------------------------------------------------------------------*
*               P R O J E T O    A G I R - T A E S A                 *
*--------------------------------------------------------------------*
* Consultoria .....: I N T E C H P R O                               *
* Res. ABAP........: Marcelo Alvares                                 *
* Res. Funcional...: Bernardo Torres                                 *
* Módulo...........: BUPA Business Partner                           *
* Programa.........: ZBUPA_CARGA_ONS                                 *
* Transação........: ZCDC                                            *
* Tipo de Programa.: REPORT                                          *
* Request     .....: S4DK901018                                      *
* Objetivo.........: Cadastro/Atualização de clientes por carga - ONS*
*--------------------------------------------------------------------*
* Change Control:                                                    *
* Version | Date      | Who                 |   What                 *
*    1.00 | 30/06/18  | Marcelo Alvares     |   Versão Inicial       *
**********************************************************************
INCLUDE: zbupa_carga_ons_top,
         zbupa_carga_ons_class.

*----------------------------------------------------------------------
*	INITIALIZATION
*----------------------------------------------------------------------
INITIALIZATION.

  CALL FUNCTION 'RS_SUPPORT_SELECTIONS'
    EXPORTING
      report               = sy-repid
      variant              = '/DEFAULT'
    EXCEPTIONS
      variant_not_existent = 01
      variant_obsolete     = 02.

  CALL METHOD:
    lcl_messages=>initialize( ),
    lcl_file=>set_sscrtexts( ).

    s_vkorg-option = 'BT'.
    s_vkorg-sign   = 'I'.
    s_vkorg-low    = 'VG01'.
    s_vkorg-high   = 'VG18'.
    APPEND s_vkorg.

    s_vtweg-option = 'EQ'.
    s_vtweg-sign   = 'I'.
    s_vtweg-low    = '01'.
    APPEND s_vtweg.

    s_spart-option = 'EQ'.
    s_spart-sign   = 'I'.
    s_spart-low    = '01'.
    APPEND s_spart.

* Gets data in memory or parameters for the file path field
  p_file = lcl_file=>get_init_filename( ).

*----------------------------------------------------------------------
*	Events
*----------------------------------------------------------------------

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_file.
  p_file = lcl_file=>select_file( ).


AT SELECTION-SCREEN.
  CASE sscrfields-ucomm.
    WHEN'FC01'.
      CALL METHOD lcl_file=>export_model.
  ENDCASE.

*----------------------------------------------------------------------
*	Beginning of Processing
*----------------------------------------------------------------------
START-OF-SELECTION.

*  Sets the memory parameter for the field
  lcl_file=>set_parameter_id( p_file ).

  CREATE OBJECT o_bupa
    EXCEPTIONS
      upload_error = 1
      OTHERS       = 2.

  IF sy-subrc NE 0.
    MESSAGE ID sy-msgid TYPE 'S' NUMBER sy-msgno DISPLAY LIKE cc_msg_error
      WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    LEAVE LIST-PROCESSING.
  ENDIF.

  CALL METHOD lcl_alv=>show
    CHANGING
      it_outtab = o_bupa->t_alv.

END-OF-SELECTION.

*&---------------------------------------------------------------------*
*& Form USER_COMMAND
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM user_command_alv USING p_ucomm     TYPE syst_ucomm
                            p_selfield  TYPE slis_selfield ##CALLED.

  DATA:
    lv_partner_number TYPE bu_partner.


  CASE p_ucomm.

    WHEN lcl_alv=>co_double_click.

      CHECK:
          p_selfield-value IS NOT INITIAL.

      CASE p_selfield-fieldname.
        WHEN 'CODIGO'.

          lv_partner_number = lcl_bupa=>get_bp_from_kunnr( p_selfield-value ).

          PERFORM call_transaction_bp USING lv_partner_number.

        WHEN 'BP'.

          lv_partner_number = |{ p_selfield-value ALPHA = IN }|.

          PERFORM call_transaction_bp USING lv_partner_number.

        WHEN OTHERS.

          READ TABLE o_bupa->t_alv ASSIGNING FIELD-SYMBOL(<fs_alv>) INDEX p_selfield-tabindex.

*          CALL METHOD lcl_messages=>show( o_bupa->get_alv_tabix( ls_alv-codigo ) ).
          CALL METHOD lcl_messages=>show( <fs_alv>-codigo ).


      ENDCASE. "CASE p_selfield-fieldname.

    WHEN lcl_alv=>co_all_message.
      CALL METHOD lcl_messages=>show( ).

    WHEN lcl_alv=>co_exec1.

      CALL METHOD o_bupa->effective_load( ).
      p_selfield-refresh = abap_true.  " refresh ALV list !!!

    WHEN OTHERS.

  ENDCASE. "CASE p_ucomm.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form CALL_TRANSACTION_BP
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> LV_PARTNER_NUMBER
*&---------------------------------------------------------------------*
FORM call_transaction_bp  USING p_partner_number TYPE bu_partner.

  CHECK p_partner_number IS NOT INITIAL.

  SET PARAMETER ID 'BPA' FIELD p_partner_number.
  CALL TRANSACTION 'BP' WITH AUTHORITY-CHECK.

ENDFORM.
FORM top_of_page.
  CALL METHOD lcl_alv=>top_of_page( ).
ENDFORM.
*&---------------------------------------------------------------------*
*& Form ALV_SET_STATUS
*&---------------------------------------------------------------------*
FORM alv_set_status USING p_extab TYPE kkblo_t_extab.

  IF o_bupa->get_test( ) IS INITIAL.
    APPEND lcl_alv=>co_exec1 TO p_extab.
  ENDIF.

  SET PF-STATUS 'ALV_STATUS' EXCLUDING p_extab.
ENDFORM.
