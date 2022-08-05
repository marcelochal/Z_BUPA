*--------------------------------------------------------------------*
*                       T    A    E    S    A                        *
*--------------------------------------------------------------------*
* Consultoria .....: I N T E C H P R O                               *
* Res. ABAP........: Marcelo Alvares                                 *
* Res. Funcional...: Marcelo Alvares                                 *
* Módulo...........: BUPA - Business Partner                         *
* Programa.........: ZGF_BUPA001                                     *
* Transação........: N/A                                             *
* Tipo de Programa.: Include                                         *
* Request     .....: S4DK926253                                      *
* Objetivo.........: Atualização de Fornecedores pelo SERTRAS        *
*--------------------------------------------------------------------*
* Change Control:                                                    *
* Version | Date      | Who                 |   What                 *
*    1.00 | 19/08/19  | Marcelo Alvares     |   INC0114565           *
**********************************************************************
*----------------------------------------------------------------------*
***INCLUDE LZGF_BUPA001D01.
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*& CLASS DEFINITION LCL_BAL_LOG                                        *
*&---------------------------------------------------------------------*
"! Class for SLG1
CLASS lcl_bal_log DEFINITION ABSTRACT.
  PUBLIC SECTION.

    INTERFACES:
      if_xo_const_message.

    TYPES:
        ty_t_zpf_mensagem TYPE STANDARD TABLE OF zpf_mensagem WITH DEFAULT KEY.

    CONSTANTS:
      BEGIN OF co_log_object,
        object_sertras TYPE bal_s_log-object    VALUE 'ZIF_SERTRAS',
        create         TYPE bal_s_log-subobject VALUE 'CREATE',
        display        TYPE bal_s_log-subobject VALUE 'DISPLAY',
        update         TYPE bal_s_log-subobject VALUE 'UPDATE',
      END OF co_log_object,
      BEGIN OF co_msg_type,
        abort   TYPE bapi_mtype VALUE if_xo_const_message~abort,
        error   TYPE bapi_mtype VALUE if_xo_const_message~error,
        exit    TYPE bapi_mtype VALUE if_xo_const_message~exit,
        info    TYPE bapi_mtype VALUE if_xo_const_message~info,
        success TYPE bapi_mtype VALUE if_xo_const_message~success,
        warning TYPE bapi_mtype VALUE if_xo_const_message~warning,
      END OF co_msg_type.

    METHODS:
      constructor
        IMPORTING
          im_v_subobject TYPE bal_s_log-subobject OPTIONAL
        EXCEPTIONS
          log_header_inconsistent ,
      get_log_handle
        RETURNING VALUE(r_result) TYPE balloghndl,
      get_log_header
        RETURNING VALUE(r_result) TYPE bal_s_log,
      get_log_extnumber
        RETURNING VALUE(r_result) TYPE bal_s_log-extnumber,
      add_msg_return
        IMPORTING
          im_t_mess_tab   TYPE ty_t_zpf_mensagem    OPTIONAL
          im_t_return     TYPE bapiretm             OPTIONAL
          im_t_return_map TYPE mdg_bs_bp_msgmap_t   OPTIONAL,
      msg_add
        IMPORTING
          im_s_msg      TYPE bal_s_msg OPTIONAL
          im_s_bapiret2 TYPE bapiret2  OPTIONAL
            PREFERRED PARAMETER im_s_bapiret2
        EXCEPTIONS
          log_not_found
          msg_inconsistent
          log_is_full,
      save,
      add_free_text
        IMPORTING
          im_v_text TYPE c,
      add_msg_import_table
        IMPORTING
          im_t_table TYPE ANY TABLE OPTIONAL
          im_s_struc TYPE any OPTIONAL
          im_v_data  TYPE any OPTIONAL.

  PROTECTED SECTION.


  PRIVATE SECTION.
    DATA:
      log_handle TYPE balloghndl,
      log_header TYPE bal_s_log.

    METHODS:
      create
        EXCEPTIONS
          log_header_inconsistent .

ENDCLASS.

CLASS lcl_log_create DEFINITION INHERITING FROM lcl_bal_log.
  PUBLIC SECTION.
    METHODS constructor.
ENDCLASS.

CLASS lcl_log_update DEFINITION INHERITING FROM lcl_bal_log.
  PUBLIC SECTION.
    METHODS constructor.
ENDCLASS.

CLASS lcl_log_display DEFINITION INHERITING FROM lcl_bal_log.
  PUBLIC SECTION.
    METHODS constructor.
ENDCLASS.

*&=====================================================================*
*& CLASS IMPLEMENTATION LCL_LOG_CREATE                                 *
*&=====================================================================*
CLASS lcl_log_create IMPLEMENTATION.
  METHOD constructor.
    super->constructor( im_v_subobject = co_log_object-create ).
  ENDMETHOD.
ENDCLASS.

*&=====================================================================*
*& CLASS IMPLEMENTATION LCL_LOG_UPDATE                                 *
*&=====================================================================*
CLASS lcl_log_update IMPLEMENTATION.
  METHOD constructor.
    super->constructor( im_v_subobject = co_log_object-update ).
  ENDMETHOD.
ENDCLASS.

*&=====================================================================*
*& CLASS IMPLEMENTATION LCL_LOG_DISPLAY                                *
*&=====================================================================*
CLASS lcl_log_display IMPLEMENTATION.
  METHOD constructor.
    super->constructor( im_v_subobject = co_log_object-display ).
  ENDMETHOD.
ENDCLASS.

*&=====================================================================*
*& CLASS IMPLEMENTATION LCL_BAL_LOG                                    *
*&=====================================================================*
CLASS lcl_bal_log IMPLEMENTATION.

  METHOD constructor.
    DATA:
      ls_msg               TYPE bal_s_msg,
      ls_msg_handle        TYPE balmsghndl,
      lb_msg_was_logged    TYPE boolean,
      lb_msg_was_displayed TYPE boolean.

    GET TIME STAMP FIELD DATA(ts).

    me->log_header-object    = me->co_log_object-object_sertras.
    me->log_header-subobject = im_v_subobject.
    me->log_header-alprog    = sy-repid.
    me->log_header-extnumber = me->log_header-subobject && ts.

    CALL METHOD me->create
      EXCEPTIONS
        log_header_inconsistent = 1
        OTHERS                  = 2.
    IF sy-subrc NE 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
        WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 RAISING log_header_inconsistent.
    ENDIF.

  ENDMETHOD.

  METHOD get_log_handle.
    r_result = me->log_handle.
  ENDMETHOD.

  METHOD msg_add.
    DATA:
      ls_msg               TYPE bal_s_msg,
      ls_msg_handle        TYPE balmsghndl,
      lb_msg_was_logged    TYPE boolean,
      lb_msg_was_displayed TYPE boolean.

    MOVE-CORRESPONDING im_s_msg TO ls_msg.

    IF im_s_bapiret2 IS NOT INITIAL.
      ls_msg-msgid = im_s_bapiret2-id.
      ls_msg-msgno = im_s_bapiret2-number.
      ls_msg-msgty = im_s_bapiret2-type.
      ls_msg-msgv1 = im_s_bapiret2-message_v1.
      ls_msg-msgv2 = im_s_bapiret2-message_v2.
      ls_msg-msgv3 = im_s_bapiret2-message_v3.
      ls_msg-msgv4 = im_s_bapiret2-message_v4.
*      ls_msg-
*      ls_msg- = im_s_bapiret2-id.

    ELSEIF ls_msg IS INITIAL.
      MOVE-CORRESPONDING syst TO ls_msg.
    ENDIF.

    CALL FUNCTION 'BAL_LOG_MSG_ADD'
      EXPORTING
        i_log_handle        = me->log_handle   " Log handle
        i_s_msg             = ls_msg           " Notification data
      IMPORTING
        e_s_msg_handle      = ls_msg_handle         " Message handle
        e_msg_was_logged    = lb_msg_was_logged     " Message collected
        e_msg_was_displayed = lb_msg_was_displayed  " Message output
      EXCEPTIONS
        log_not_found       = 1                " Log not found
        msg_inconsistent    = 2                " Message inconsistent
        log_is_full         = 3                " Message number 999999 reached. Log is full
        OTHERS              = 4.

    CASE sy-subrc.
      WHEN 1.

        CALL METHOD me->create
          EXCEPTIONS
            log_header_inconsistent = 1
            OTHERS                  = 2.

        IF syst-subrc IS INITIAL.
          CALL FUNCTION 'BAL_LOG_MSG_ADD'
            EXPORTING
              i_log_handle        = me->log_handle   " Log handle
              i_s_msg             = ls_msg           " Notification data
            IMPORTING
              e_s_msg_handle      = ls_msg_handle         " Message handle
              e_msg_was_logged    = lb_msg_was_logged     " Message collected
              e_msg_was_displayed = lb_msg_was_displayed  " Message output
            EXCEPTIONS
              log_not_found       = 1                " Log not found
              msg_inconsistent    = 2                " Message inconsistent
              log_is_full         = 3                " Message number 999999 reached. Log is full
              OTHERS              = 4.
        ENDIF.

        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 RAISING log_not_found.
      WHEN 2 OR 4.
        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 RAISING msg_inconsistent.
      WHEN 3.
        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 RAISING log_is_full.

    ENDCASE.


  ENDMETHOD.

  METHOD save.
    DATA:
        lt_log_handle TYPE bal_t_logh.

    APPEND me->get_log_handle( ) TO lt_log_handle.

    CALL FUNCTION 'BAL_DB_SAVE'
      EXPORTING
        i_t_log_handle   = lt_log_handle    " Table of log handles
      EXCEPTIONS
        log_not_found    = 1                " Log not found
        save_not_allowed = 2                " Cannot save
        numbering_error  = 3                " Number assignment error
        OTHERS           = 4.
    IF sy-subrc NE 0.
*     MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
*       WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.


  ENDMETHOD.

  METHOD get_log_header.
    r_result = me->log_header.
  ENDMETHOD.

  METHOD get_log_extnumber.
    r_result = me->log_header-extnumber.
  ENDMETHOD.

  METHOD create.

    IF me->log_handle IS NOT INITIAL.
      CALL FUNCTION 'BAL_LOG_EXIST'
        EXPORTING
          i_log_handle  = me->log_handle    " Log handle
        EXCEPTIONS
          log_not_found = 1                " Log not found
          OTHERS        = 2.
      IF sy-subrc EQ 0.
        RETURN.
      ENDIF.
    ENDIF.

    CALL FUNCTION 'BAL_LOG_CREATE'
      EXPORTING
        i_s_log                 = me->log_header
      IMPORTING
        e_log_handle            = me->log_handle
      EXCEPTIONS
        log_header_inconsistent = 1
        OTHERS                  = 2.

    IF sy-subrc NE 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
        WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 RAISING log_header_inconsistent.
    ENDIF.

  ENDMETHOD.

  METHOD add_msg_return.

    DATA:
      ls_msg               TYPE bal_s_msg,
      ls_msg_handle        TYPE balmsghndl,
      lb_msg_was_logged    TYPE boolean,
      lb_msg_was_displayed TYPE boolean.



*>>>>>>>>>>>>>>>>>>>>>> E_MESS_TAB <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<*

    IF im_t_mess_tab IS INITIAL.
      CALL METHOD me->add_free_text( 'Tabela Retorno E_MESS_TAB sem registros!' ).
    ELSE.
      CALL METHOD me->add_free_text( |Mensagens Tabela de retorno E_MESS_TAB com { lines( im_t_mess_tab ) } registros.| ).
    ENDIF.

    LOOP AT im_t_mess_tab ASSIGNING FIELD-SYMBOL(<mess_tab>).

      ls_msg-msg_count  = syst-tabix.
      ls_msg-msgid      = 'Z_BUPA'.
      ls_msg-msgno      = '000'.
      ls_msg-msgty      = <mess_tab>-type.
      ls_msg-msgv1      = <mess_tab>-texto.
      ls_msg-context-value = |{ <mess_tab>-type }{ ' | ' }{ <mess_tab>-texto }|.

      CALL FUNCTION 'BAL_LOG_MSG_ADD'
        EXPORTING
          i_log_handle        = me->log_handle   " Log handle
          i_s_msg             = ls_msg           " Notification data
        IMPORTING
          e_s_msg_handle      = ls_msg_handle         " Message handle
          e_msg_was_logged    = lb_msg_was_logged     " Message collected
          e_msg_was_displayed = lb_msg_was_displayed  " Message output
        EXCEPTIONS
          log_not_found       = 1                " Log not found
          msg_inconsistent    = 2                " Message inconsistent
          log_is_full         = 3                " Message number 999999 reached. Log is full
          OTHERS              = 4.
    ENDLOOP.


*>>>>>>>>>>>>>>>>>>>>>> E_RETURN <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<*
    IF im_t_return IS INITIAL.
      CALL METHOD me->add_free_text( 'Tabela Retorno E_RETURN sem registros!' ).
    ELSE.
      CALL METHOD me->add_free_text( |Mensagens Tabela de retorno E_RETURN com { lines( im_t_mess_tab ) } registros.| ).
    ENDIF.

    LOOP AT im_t_return ASSIGNING FIELD-SYMBOL(<return>).
      LOOP AT <return>-object_msg ASSIGNING FIELD-SYMBOL(<object_msg>).

        ls_msg-msg_count = syst-tabix.
        ls_msg-msgty     = <object_msg>-type.
        ls_msg-msgid     = <object_msg>-id.
        ls_msg-msgno     = <object_msg>-number.
        ls_msg-msgv1     = <object_msg>-message_v1.
        ls_msg-msgv2     = <object_msg>-message_v2.
        ls_msg-msgv3     = <object_msg>-message_v3.
        ls_msg-msgv4     = <object_msg>-message_v4.

        CALL FUNCTION 'BAL_LOG_MSG_ADD'
          EXPORTING
            i_log_handle        = me->log_handle   " Log handle
            i_s_msg             = ls_msg           " Notification data
          IMPORTING
            e_s_msg_handle      = ls_msg_handle         " Message handle
            e_msg_was_logged    = lb_msg_was_logged     " Message collected
            e_msg_was_displayed = lb_msg_was_displayed  " Message output
          EXCEPTIONS
            log_not_found       = 1                " Log not found
            msg_inconsistent    = 2                " Message inconsistent
            log_is_full         = 3                " Message number 999999 reached. Log is full
            OTHERS              = 4.

      ENDLOOP.
    ENDLOOP.

*>>>>>>>>>>>>>>>>>>>>> E_RETURN_MAP <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<*
    IF im_t_return_map IS INITIAL.
      CALL METHOD me->add_free_text( 'Tabela Retorno E_RETURN_MAP sem registros!' ).
    ELSE.
      CALL METHOD me->add_free_text( |Mensagens Tabela de retorno E_RETURN_MAP com { lines( im_t_mess_tab ) } registros.| ).
    ENDIF.

    LOOP AT im_t_return_map ASSIGNING FIELD-SYMBOL(<return_map>).

      ls_msg-msg_count  = syst-tabix.
      ls_msg-msgty      = <return_map>-type.
      ls_msg-msgid      = <return_map>-id.
      ls_msg-msgno      = <return_map>-number.
      ls_msg-msgv1      = <return_map>-message_v1.
      ls_msg-msgv2      = <return_map>-message_v2.
      ls_msg-msgv3      = <return_map>-message_v3.
      ls_msg-msgv4      = <return_map>-message_v4.

      CALL FUNCTION 'BAL_LOG_MSG_ADD'
        EXPORTING
          i_log_handle        = me->log_handle   " Log handle
          i_s_msg             = ls_msg           " Notification data
        IMPORTING
          e_s_msg_handle      = ls_msg_handle         " Message handle
          e_msg_was_logged    = lb_msg_was_logged     " Message collected
          e_msg_was_displayed = lb_msg_was_displayed  " Message output
        EXCEPTIONS
          log_not_found       = 1                " Log not found
          msg_inconsistent    = 2                " Message inconsistent
          log_is_full         = 3                " Message number 999999 reached. Log is full
          OTHERS              = 4.

    ENDLOOP.

  ENDMETHOD.


  METHOD add_free_text.


    CALL FUNCTION 'BAL_LOG_MSG_ADD_FREE_TEXT'
      EXPORTING
        i_log_handle = me->log_handle       " Log handle
        i_msgty      = me->co_msg_type-info " Message type (A, E, W, I, S)
        i_probclass  = '4'                  " Problem class (1, 2, 3, 4)
        i_text       = im_v_text            " Message data
*       i_s_context  =                      " Context information for free text message
*       i_s_params   =                      " Parameter set for free text message
        i_detlevel   = '1'                  " Application Log: Level of Detail
      EXCEPTIONS
        OTHERS       = 4.

  ENDMETHOD.


  METHOD add_msg_import_table.
    DATA:
      lr_table_descr TYPE REF TO cl_abap_tabledescr,
      lr_strucdescr  TYPE REF TO cl_abap_structdescr,
      lr_elemdescr   TYPE REF TO cl_abap_elemdescr,
      ls_dfies       TYPE dfies,
      lt_dfiess      TYPE ddfields,
      lv_text        TYPE c LENGTH 400.

    TRY.
        IF im_t_table IS SUPPLIED.
          lr_table_descr ?= cl_abap_tabledescr=>describe_by_data( im_t_table ).
          lt_dfiess       = zcl_utils=>get_field_list( im_t_table = im_t_table ).

          LOOP AT lt_dfiess ASSIGNING FIELD-SYMBOL(<dfies>).
            me->add_free_text( |Tabela { <dfies>-tabname } com: { lines( im_t_table ) } registros.| ).
            EXIT.
          ENDLOOP.

          LOOP AT im_t_table ASSIGNING FIELD-SYMBOL(<table>).
            CLEAR lv_text.
            LOOP AT lt_dfiess ASSIGNING <dfies>.
              ASSIGN COMPONENT <dfies>-position OF STRUCTURE <table> TO FIELD-SYMBOL(<component>).
              IF <component> IS ASSIGNED.
                lv_text = |{ lv_text } { '|' } { <component> }|.
                IF strlen( lv_text ) > 255.
                  CALL METHOD me->add_free_text( lv_text ).
                  CLEAR lv_text.
                ENDIF.
              ENDIF.
            ENDLOOP.

            CALL METHOD me->add_free_text( lv_text ).
          ENDLOOP.

          IF syst-subrc IS NOT INITIAL.
            READ TABLE lt_dfiess ASSIGNING <dfies> INDEX 1.
            CALL METHOD me->add_free_text( |Tabela { <dfies>-tabname } sem registros.| ).

          ENDIF.

        ELSEIF im_s_struc IS SUPPLIED.
          lr_strucdescr ?= cl_abap_structdescr=>describe_by_data( p_data = im_s_struc ) .
          lt_dfiess = zcl_utils=>get_field_list( im_s_struc = im_s_struc ).

          IF im_s_struc IS INITIAL.
            READ TABLE lt_dfiess ASSIGNING <dfies> INDEX 1.
            lv_text = |Tabela { <dfies>-tabname } sem registros.|.
          ELSE.
            LOOP AT lt_dfiess ASSIGNING <dfies>.
              ASSIGN COMPONENT <dfies>-position OF STRUCTURE im_s_struc TO <component>.
              IF <component> IS ASSIGNED.
                lv_text = |{ lv_text } { '|' } { <component> }|.
                IF strlen( lv_text ) > 255.
                  CALL METHOD me->add_free_text( lv_text ).
                  CLEAR lv_text.
                ENDIF.
              ENDIF.
            ENDLOOP.
            me->add_free_text( |Tabela { <dfies>-tabname } com: 01 registros.| ).
          ENDIF.
          CALL METHOD me->add_free_text( lv_text ).

        ELSEIF im_v_data IS SUPPLIED.

          lr_elemdescr ?= cl_abap_typedescr=>describe_by_data( im_v_data ) .
          ls_dfies = lr_elemdescr->get_ddic_field( ).

          me->add_free_text( |Campo: { ls_dfies-tabname } com valor { im_v_data }.| ).

        ENDIF.

      CATCH cx_root.
    ENDTRY.

  ENDMETHOD.

ENDCLASS.
