*--------------------------------------------------------------------*
*                       P R O J E T O   A G I R                      *
*--------------------------------------------------------------------*
* Consulting  .....: IntechPro                                       *
* ABAP Developer ..: Marcelo Alvares (MA004818)                      *
* Business Consult.: Marcelo Alvares                                 *
* Module ..........: BUPA - Business partner                         *
* Program     .....: ZBUPA_DIALOG_SEARCH                             *
* Transaction .....: BP                                              *
* Type        .....: Function                                        *
* Objective   .....: Additional BP Selection Fields                  *
* Request     .....:                                                 *
*--------------------------------------------------------------------*
* Change Control:                                                    *
* Version  Date       Who                What                        *
*    1.00  02/10/2019 Marcelo Alvares    Initial release             *
**********************************************************************

*&---------------------------------------------------------------------*
*&      Module  DYNPRO_PAI  INPUT
*&---------------------------------------------------------------------*
MODULE dynpro_pai INPUT.
  gs_dynpro_id-program_name  = sy-repid.
  gs_dynpro_id-dynpro_number = sy-dynnr.

  CALL METHOD cl_bus_abstract_screen=>dynpro_pai
    EXPORTING
      iv_program_name  = gs_dynpro_id-program_name   " Program Name
      iv_dynpro_number = gs_dynpro_id-dynpro_number. " Number of Current Screen
ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  DYNPRO_PAI_BEGIN  INPUT
*&---------------------------------------------------------------------*
MODULE dynpro_pai_begin INPUT.
  gs_dynpro_id-program_name  = sy-repid.
  gs_dynpro_id-dynpro_number = sy-dynnr.

  CALL METHOD cl_bus_abstract_screen=>dynpro_pai_begin
    EXPORTING
      iv_program_name  = gs_dynpro_id-program_name   " Program Name
      iv_dynpro_number = gs_dynpro_id-dynpro_number. " Number of Current Screen
ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  DYNPRO_PAI_END  INPUT
*&---------------------------------------------------------------------*
MODULE dynpro_pai_end INPUT.
  gs_dynpro_id-program_name  = sy-repid.
  gs_dynpro_id-dynpro_number = sy-dynnr.

  CALL METHOD cl_bus_abstract_screen=>dynpro_pai_end
    EXPORTING
      iv_program_name  = gs_dynpro_id-program_name   " Program Name
      iv_dynpro_number = gs_dynpro_id-dynpro_number. " Number of Current Screen
ENDMODULE.
