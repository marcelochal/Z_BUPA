*----------------------------------------------------------------------*
*                     ___    _     __    __    _                       *
*                      |    |_|   |_    (_    |_|                      *
*                      |    | |   |__   __)   | |                      *
*                                                                      *
*----------------------------------------------------------------------*
*            TRANSMISSORA ALIANÇA DE ENERGIA ELÉTRICA S.A.             *
*----------------------------------------------------------------------*
* ABAP Developer ..: Marcelo Alvares (90000130)                        *
* Business Consult.: Rafael Oliveira (90000287)                        *
* Module ..........: BP - Bussines Partner / CVI Vendor                *
* Program     .....: ZBUPA_CHECK_ALT_PAYEE                             *
* Transaction .....: N/A                                               *
* Type        .....: Report Include                                    *
* Objective   .....: Check Permitted Alternative Payee is OK           *
* Request     .....: SHDK916490                                        *
*----------------------------------------------------------------------*
* Change Control:                                                      *
* Date       | Who                                      |Task          *
* 14.04.2021 | Marcelo Alvares 90000130                 |PRB0040128    *
************************************************************************
*&---------------------------------------------------------------------*
*& Report zbupa_check_alt_payee
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zbupa_check_alt_payee.



CLASS lcl_check_alt_payee DEFINITION CREATE PRIVATE.

  PUBLIC SECTION.

    TYPES:
      BEGIN OF ty_s_vendor_with_error,
        lifnr            TYPE lfza-lifnr,
        bukrs            TYPE lfza-bukrs,
        name1            TYPE lfa1-name1,
        empfk            TYPE lfza-empfk,
        lfa1_xlfza_error TYPE abap_bool,
        lfb1_xlfzb_error TYPE abap_bool,
      END OF ty_s_vendor_with_error,

      ty_t_vendor_with_error TYPE STANDARD TABLE OF ty_s_vendor_with_error WITH KEY lifnr bukrs.

    CLASS-METHODS:
      main.

  PROTECTED SECTION.
  PRIVATE SECTION.

    CLASS-DATA:
        gt_vendor_with_error TYPE ty_t_vendor_with_error.

    CLASS-METHODS:
      alv_set_columns
        IMPORTING
          im_r_salv_table TYPE REF TO cl_salv_table,
      set_bal_log_slg1,
      alv_display.

ENDCLASS.

CLASS lcl_check_alt_payee IMPLEMENTATION.

  METHOD main.

    TRY.

        " Select all fields in the LFZA table, joining with the LFA1 table where: LFZA-LIFNR = LFA1-LIFNR, LFZA-BUKRS = empty, LFA1-XLFZA = empty
        " All records found in the selection above are considered to be problem records,
        " because when there are alternative suppliers registered in the general data view of the supplier (LFZA-BUKRS empty),
        " the LFA1-XLFZA field must be marked with X.

        SELECT lfza~lifnr,
               lfa1~name1,
               lfza~bukrs,
               lfza~empfk,
               @abap_true  AS lfa1_xlfza_error,
               @abap_false AS lfb1_xlfzb_error
            FROM lfza AS lfza
            INNER JOIN lfa1 AS lfa1 ON lfza~lifnr EQ lfa1~lifnr
            INTO CORRESPONDING FIELDS OF TABLE @gt_vendor_with_error
            WHERE lfza~bukrs EQ @space AND
                  lfa1~xlfza EQ @abap_false.


        " Select all fields in the LFZA table, joining with the LFB1 table where: LFZA-LIFNR = LFB1-LIFNR, LFZA-BUKRS = LFB1-BUKRS, LFB1-XLFZB = empty
        " All records located in the selection above are considered to be problem records,
        " because when there are alternative suppliers registered in the company view, the LFB1-XLFZA field must be marked with X.

        SELECT lfza~lifnr,
               lfa1~name1,
               lfza~bukrs,
               lfza~empfk,
               @abap_false AS lfa1_xlfza_error,
               @abap_true  AS lfb1_xlfzb_error

            FROM lfza AS lfza
            INNER JOIN lfa1 AS lfa1 ON lfza~lifnr EQ lfa1~lifnr
            INNER JOIN lfb1 AS lfb1 ON lfza~lifnr EQ lfb1~lifnr AND
                                       lfza~bukrs EQ lfb1~bukrs AND
                                       lfb1~xlfzb EQ @abap_false
            APPENDING CORRESPONDING FIELDS OF TABLE @gt_vendor_with_error.

        IF gt_vendor_with_error IS INITIAL.
          MESSAGE 'Nenhum registro com erro localizado' TYPE 'S'.
          RETURN.
        ENDIF.

        SORT gt_vendor_with_error ASCENDING BY lifnr bukrs empfk.

        lcl_check_alt_payee=>set_bal_log_slg1( ).

        lcl_check_alt_payee=>alv_display( ).


      CATCH cx_root INTO DATA(lcx_root) ##CATCH_ALL.
        " Deu ruim!
        MESSAGE lcx_root->get_longtext( ) TYPE 'E'.

    ENDTRY.

  ENDMETHOD.

  METHOD alv_display.

    DATA:
      lo_salv_table TYPE REF TO cl_salv_table,
      lo_alv_colum  TYPE REF TO cl_salv_column_table,
      ls_layout_key TYPE salv_s_layout_key.

    TRY.

        cl_salv_table=>factory(
             IMPORTING
               r_salv_table   = lo_salv_table                          " Basis Class Simple ALV Tables
             CHANGING
               t_table      = gt_vendor_with_error         ).


        lo_salv_table->get_functions( )->set_all( abap_true ).

        " Set Display Settings
        lo_salv_table->get_display_settings( )->set_horizontal_lines( abap_true ).
        lo_salv_table->get_display_settings( )->set_striped_pattern( abap_true ).

        " Set Functional Settings
        lo_salv_table->get_functional_settings( )->set_sort_on_header_click( abap_true ).

        " Set Selections Type
        lo_salv_table->get_selections( )->set_selection_mode( if_salv_c_selection_mode=>row_column ).

        " Set Columns Parameters
        lo_salv_table->get_columns( )->set_optimize( abap_true ).

        alv_set_columns( im_r_salv_table = lo_salv_table ).

        " Set Layout
        ls_layout_key-report = sy-repid.
        lo_salv_table->get_layout( )->set_key( ls_layout_key ).
        lo_salv_table->get_layout( )->set_save_restriction( if_salv_c_layout=>restrict_none ).


        lo_salv_table->display( ).

      CATCH cx_root INTO DATA(lcx_root) ##CATCH_ALL.
        " Deu ruim!
        MESSAGE lcx_root->get_longtext( ) TYPE 'E'.

    ENDTRY.

  ENDMETHOD.


  METHOD alv_set_columns.
    DATA:
      lo_alv_columns TYPE REF TO cl_salv_columns,
      lt_ddfields    TYPE ddfields ##NEEDED.

    lo_alv_columns = im_r_salv_table->get_columns( ).
    lo_alv_columns->set_optimize( abap_true ).

    DATA(lt_salv_column_ref) = lo_alv_columns->get( ).


    LOOP AT lt_salv_column_ref ASSIGNING FIELD-SYMBOL(<ls_column>).

      CASE <ls_column>-columnname.

        WHEN 'LFZA-MANDT'.

          <ls_column>-r_column->set_technical( abap_true ).

        WHEN 'LFA1_XLFZA_ERROR'.

          <ls_column>-r_column->set_medium_text( 'Erro LFA1'  ).
          <ls_column>-r_column->set_long_text(   'Erro LFA1'  ).
          <ls_column>-r_column->set_short_text(  'Erro LFA1'  ).
          <ls_column>-r_column->set_optimized( abap_true ).
          <ls_column>-r_column->set_alignment( if_salv_c_alignment=>centered ).

        WHEN 'LFB1_XLFZB_ERROR'.
          <ls_column>-r_column->set_medium_text( 'Erro LFB1'  ).
          <ls_column>-r_column->set_long_text(   'Erro LFB1'  ).
          <ls_column>-r_column->set_short_text(  'Erro LFB1'  ).
          <ls_column>-r_column->set_optimized( abap_true ).
          <ls_column>-r_column->set_alignment( if_salv_c_alignment=>centered ).

        WHEN OTHERS.
          <ls_column>-r_column->set_optimized( abap_true ).

      ENDCASE.


    ENDLOOP.

  ENDMETHOD.


  METHOD set_bal_log_slg1.

    DATA:
      ls_log        TYPE bal_s_log,
      lv_log_handle TYPE balloghndl,
      lt_log_handle TYPE bal_t_logh,
      ls_message    TYPE bal_s_msg.


    ls_log-object    = 'Z_BP'.
    ls_log-subobject = 'Z_XLFZ_CHE'.
    ls_log-aluser    = syst-uname.
    ls_log-alprog    = syst-repid.

    CALL FUNCTION 'BAL_LOG_CREATE'
      EXPORTING
        i_s_log      = ls_log                 " Log header data
      IMPORTING
        e_log_handle = lv_log_handle          " Log handle
      EXCEPTIONS
        OTHERS       = 2.

    IF sy-subrc IS NOT INITIAL.
*      RAISE EXCEPTION TYPE lcx_rsusr_lock_users.
    ENDIF.

    ls_message-msgty      = 'I'.
    ls_message-msgid      = 'Z_BUPA'.
    ls_message-msgno      = '000'.
    ls_message-msgv1      = 'Fornecedores encontrados com erros'.
    ls_message-probclass  = '3'.

    CALL FUNCTION 'BAL_LOG_MSG_ADD'
      EXPORTING
        i_log_handle = lv_log_handle
        i_s_msg      = ls_message
      EXCEPTIONS
        OTHERS       = 1.



    LOOP AT lcl_check_alt_payee=>gt_vendor_with_error ASSIGNING FIELD-SYMBOL(<ls_vendor_with_error>).

      CLEAR: ls_message.

      IF <ls_vendor_with_error>-bukrs IS INITIAL.
        <ls_vendor_with_error>-bukrs = '0000'.
      ENDIF.

      ls_message-msgty      = 'I'.
      ls_message-msgid      = 'Z_BUPA'.
      ls_message-msgno      = '000'.
      ls_message-msgv1      = <ls_vendor_with_error>-lifnr.
      ls_message-msgv2      = | # { <ls_vendor_with_error>-bukrs ALPHA = IN WIDTH = 4 } { <ls_vendor_with_error>-empfk ALPHA = IN }|.
      ls_message-msgv3      = | # LFA1 Error: { <ls_vendor_with_error>-lfa1_xlfza_error WIDTH = 1 }|.
      ls_message-msgv4      = | # LFB1 Error: { <ls_vendor_with_error>-lfb1_xlfzb_error WIDTH = 1 }|.
      ls_message-probclass  = '3'.

      CALL FUNCTION 'BAL_LOG_MSG_ADD'
        EXPORTING
          i_log_handle = lv_log_handle
          i_s_msg      = ls_message
        EXCEPTIONS
          OTHERS       = 1.

    ENDLOOP.

* prepare the database for application log
    CALL FUNCTION 'BAL_DB_SAVE_PREPARE'
      EXPORTING
        i_replace_in_all_logs = abap_on
      EXCEPTIONS
        log_not_found         = 0
        OTHERS                = 1.

    IF sy-subrc IS INITIAL.
      INSERT lv_log_handle INTO TABLE lt_log_handle.
*  Save logs into Database
      CALL FUNCTION 'BAL_DB_SAVE'
        EXPORTING
          i_in_update_task = abap_on
          i_save_all       = abap_on
          i_t_log_handle   = lt_log_handle
        EXCEPTIONS
          OTHERS           = 1.

      IF sy-subrc IS INITIAL.
        COMMIT WORK.
      ENDIF.
    ENDIF.


  ENDMETHOD.

ENDCLASS.


START-OF-SELECTION.

  lcl_check_alt_payee=>main( ).
