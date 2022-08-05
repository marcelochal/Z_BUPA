*----------------------------------------------------------------------*
*                      ___    _     __    __    _                      *
*                       |    |_|   |_    (_    |_|                     *
*                       |    | |   |__   __)   | |                     *
*                                                                      *
*----------------------------------------------------------------------*
*            TRANSMISSORA ALIANÇA DE ENERGIA ELÉTRICA S.A.             *
*----------------------------------------------------------------------*
* ABAP Developer ..: Marcelo Alvares (MA004818)                        *
* Business Consult.: Marcelo Alvares (MA004818)                        *
* Module ..........: BUPA - Business partner                           *
* Program     .....: ZBUPA_0002                                        *
* Transaction .....: ZBP004                                            *
* Type        .....: Report Include                                    *
* Objective   .....: Business partners load                            *
* Request     .....: SHDK908669                                        *
*----------------------------------------------------------------------*
* Change Control:                                                      *
* Date       | Who                                      |Task          *
* 08.05.2020 | Marcelo Alvares (MA004818)               |RITM0077873   *
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
*& Include          ZBUPA_0002_TOP
*&---------------------------------------------------------------------*

REPORT zbupa_0002.

TABLES: sscrfields, t001, t024e.

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
    lcl_zbupa_0002  DEFINITION DEFERRED,
    lcl_file_upload DEFINITION DEFERRED.

*----------------------------------------------------------------------
* Variables declaration
*----------------------------------------------------------------------
"   Good practices do not use global variables!!!
"   Make the appropriate statements in class
*----------------------------------------------------------------------

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

SELECTION-SCREEN BEGIN OF BLOCK b03 WITH FRAME TITLE TEXT-t03.
SELECT-OPTIONS r_bukrs FOR t001-bukrs       DEFAULT 'TB01'.
SELECT-OPTIONS r_ekorg FOR t024e-ekorg      DEFAULT 'PG02'.
PARAMETERS p_grp       TYPE tb001-bu_group  DEFAULT 'NACJ'.
PARAMETERS p_grpte     TYPE lfb1-fdgrv      DEFAULT 'FORN NAC'.
PARAMETERS p_conpg     TYPE lfb1-zterm      DEFAULT 'TB30'.
PARAMETERS p_akont     TYPE lfb1-akont      DEFAULT '2110130108'.
PARAMETERS p_zwels     TYPE lfb1-zwels      DEFAULT 'CPRTUXY'.
SELECTION-SCREEN END OF BLOCK b03.

SELECTION-SCREEN SKIP.

SELECTION-SCREEN BEGIN OF BLOCK b04 WITH FRAME TITLE TEXT-t04.
PARAMETERS p_test TYPE xtest AS CHECKBOX DEFAULT abap_true.
SELECTION-SCREEN END OF BLOCK b04.


SELECTION-SCREEN END OF BLOCK b01.
