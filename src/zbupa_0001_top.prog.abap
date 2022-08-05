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
*&---------------------------------------------------------------------*
*& Include          ZBUPA_0001_TOP
*&---------------------------------------------------------------------*
REPORT zbupa_0001.

TABLES: sscrfields, t001.

TYPE-POOLS: icon.

*----------------------------------------------------------------------
* TYPES declaration
*----------------------------------------------------------------------
"   Good practices do not use global TYPES!!!
"   Make the appropriate statements in class
*----------------------------------------------------------------------
* Constants declaration
*----------------------------------------------------------------------
"   Good practices do not use global Constants!!!
"   Make the appropriate statements in class
*----------------------------------------------------------------------


*----------------------------------------------------------------------
* Class declaration
*----------------------------------------------------------------------
CLASS:
    lcl_zbupa_0001  DEFINITION DEFERRED,
    lcl_file_upload DEFINITION DEFERRED.

*----------------------------------------------------------------------
* Variables declaration
*----------------------------------------------------------------------
"   Good practices do not use global variables!!!
"   Make the appropriate statements in class
*----------------------------------------------------------------------
DATA:
 go_bp TYPE REF TO lcl_zbupa_0001.

*----------------------------------------------------------------------
*   SELECTION PARAMETERS
*----------------------------------------------------------------------
SELECTION-SCREEN BEGIN OF BLOCK b01 WITH FRAME TITLE TEXT-t01.
SELECTION-SCREEN: FUNCTION KEY 1.

SELECTION-SCREEN SKIP.

SELECTION-SCREEN BEGIN OF BLOCK b02 WITH FRAME TITLE TEXT-t02.
PARAMETERS p_file TYPE rlgrap-filename OBLIGATORY MEMORY ID cc_parameter_id ##EXISTS.
SELECTION-SCREEN END OF BLOCK b02.

SELECTION-SCREEN SKIP.

SELECTION-SCREEN BEGIN OF BLOCK b04 WITH FRAME TITLE TEXT-t04.
PARAMETERS p_test TYPE xtest AS CHECKBOX DEFAULT abap_true.
SELECTION-SCREEN END OF BLOCK b04.

SELECTION-SCREEN SKIP.

SELECTION-SCREEN BEGIN OF BLOCK b03 WITH FRAME TITLE TEXT-t03.
SELECT-OPTIONS r_bukrs FOR t001-bukrs.
SELECTION-SCREEN END OF BLOCK b03.

SELECTION-SCREEN END OF BLOCK b01.
