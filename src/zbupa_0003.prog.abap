*----------------------------------------------------------------------*
*                      ___    _     __    __    _                      *
*                       |    |_|   |_    (_    |_|                     *
*                       |    | |   |__   __)   | |                     *
*                                                                      *
*----------------------------------------------------------------------*
*            TRANSMISSORA ALIANÇA DE ENERGIA ELÉTRICA S.A.             *
*----------------------------------------------------------------------*
* ABAP Developer ..: Luciana Oliveira (90000237)                       *
* Business Consult.: Marcelo Alvares  (90000130)                       *
* Module ..........: BUPA - Business partner                           *
* Program     .....: ZBUPA_0003                                        *
* Transaction .....: ZBP004                                            *
* Type        .....: Report Include                                    *
* Objective   .....: Business partners load - Custumer                 *
* Request     .....: SHDK921433                                        *
*----------------------------------------------------------------------*
* Change Control:                                                      *
* Date       | Who                                      |Task          *
* 14.12.2021 | Luciana Oliveira (90000237)              |RITM0153502   *
* 03.02.2022 | Marcelo Alvares  (90000130)              |RITM0153502   *
************************************************************************
* Classic ABAP OO                                                      *
* Knowledge   of   object-oriented   programming   is   required.      *
* Only  do  maintenance  if  you  really  know  what  you  are  doing. *
* Note  Subroutines  are  obsolete in S4/HANA.  Do  not create new     *
* subroutines in new  programs.  Methods  should  be  used   instead.  *
* Subroutines created in existing programs for internal modularization *
* can   continue   to   be   called.   Whenever   possible,   however, *
* external  subroutine  calls  from  other  programs should be avoided.*
************************************************************************

INCLUDE zbupa_0003_top.
INCLUDE zbupa_0003_lcl.
INCLUDE zbupa_0003_t99.

*----------------------------------------------------------------------
*   INITIALIZATION
*----------------------------------------------------------------------
INITIALIZATION.

  CALL FUNCTION 'RS_SUPPORT_SELECTIONS'
    EXPORTING
      report  = syst-cprog  " Report Name
      variant = 'DEFAULT'   " Variant Name
    EXCEPTIONS
      OTHERS  = 3.

  sscrfields = lcl_file_upload=>set_sscrtexts_export_model( ).

  "Gets data in memory or parameters for the file path field
  p_file = lcl_file_upload=>get_parameter_file_path_value( ).

*----------------------------------------------------------------------
*   Events
*----------------------------------------------------------------------
AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_file.
  p_file = zcl_file_upload=>select_file_open_dialog( ).

AT SELECTION-SCREEN OUTPUT.
  lcl_zbupa_0003=>at_selection_screen( ).

AT SELECTION-SCREEN.
  lcl_zbupa_0003=>at_selection_screen( ).

*----------------------------------------------------------------------
*   Beginning of Processing
*----------------------------------------------------------------------
START-OF-SELECTION.

  SET RUN TIME ANALYZER ON. "for SAT analysis

  TRY.
      DATA(go_bp) = NEW lcl_zbupa_0003( ).

      go_bp->upload_file( ).
      go_bp->create_bp( ).
      go_bp->alv_display( ).

    CATCH cx_root INTO DATA(go_error).
      MESSAGE go_error TYPE 'S' DISPLAY LIKE 'E'.
      STOP.
  ENDTRY.
