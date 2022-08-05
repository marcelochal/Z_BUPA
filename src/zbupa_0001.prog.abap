*----------------------------------------------------------------------*
*                   ___    _     __    __    _                         *
*                    |    |_|   |_    (_    |_|                        *
*                    |    | |   |__   __)   | |                        *
*            TRANSMISSORA ALIANÇA DE ENERGIA ELÉTRICA S.A.             *
*----------------------------------------------------------------------*
* Consulting  .....: IntechPro                                         *
* ABAP Developer ..: Marcelo Alvares (MA004818)                        *
* Business Consult.: Marcia Barbisan                                   *
* Module ..........: BUPA - Business partner                           *
* Program     .....: ZBUPA_0001                                        *
* Transaction .....: ZBP002                                            *
* Type        .....: Report Include                                    *
* Objective   .....: Business partners load                            *
* Request     .....: SHDK904703                                        *
*----------------------------------------------------------------------*
* Change Control:                                                      *
* Version  Date       Who                What                          *
*    1.00  05.11.2019 Marcelo Alvares    Initial release               *
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
INCLUDE:
  zbupa_0001_top,
  zbupa_0001_lcl,
  zbupa_0001_t99.


*----------------------------------------------------------------------
*   INITIALIZATION
*----------------------------------------------------------------------
INITIALIZATION.

  CALL FUNCTION 'RS_SUPPORT_SELECTIONS'
    EXPORTING
      report  = syst-repid " Report Name
      variant = 'ZBP002'   " Variant Name
    EXCEPTIONS
      OTHERS  = 3.

  sscrfields = lcl_file_upload=>set_sscrtexts_export_model( ).

* Gets data in memory or parameters for the file path field
  p_file = lcl_file_upload=>get_parameter_file_path_value( ).

*----------------------------------------------------------------------
*   Events
*----------------------------------------------------------------------
AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_file.
  p_file = zcl_file_upload=>select_file_open_dialog( ).

AT SELECTION-SCREEN.
  CASE sscrfields-ucomm.
    WHEN'FC01'. " Download File Model
      lcl_zbupa_0001=>export_model( ).
  ENDCASE.

*----------------------------------------------------------------------
*   Beginning of Processing
*----------------------------------------------------------------------
START-OF-SELECTION.

  TRY.

      CREATE OBJECT go_bp.
      CALL METHOD:
        go_bp->upload_file( ),
        go_bp->create_bp( ).

      go_bp->alv_display( ).


    CATCH cx_root INTO DATA(go_error) .
      MESSAGE go_error->get_longtext( ) TYPE 'S' DISPLAY LIKE 'E'.
      STOP.
  ENDTRY.
