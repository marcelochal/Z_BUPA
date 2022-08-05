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
*&---------------------------------------------------------------------*
*& Include          ZBUPA_0003_TOP
*&---------------------------------------------------------------------*

REPORT zbupa_0003.

TABLES: sscrfields, t001, t024e, knvv.

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
    lcl_zbupa_0003  DEFINITION DEFERRED,
    lcl_file_upload DEFINITION DEFERRED,
    lcl_bp_maintain DEFINITION DEFERRED.

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
SELECT-OPTIONS:
    r_bukrs FOR t001-bukrs       DEFAULT 'TB01'.        " Company Code
PARAMETERS:
  p_group  TYPE tb001-bu_group   DEFAULT 'NACJ',        " Business Partner Grouping
  p_icmstx TYPE j_1bicmstaxpay   DEFAULT 'NC',          " ICMS Taxpayer
  p_fdgrv  TYPE knb1-fdgrv       DEFAULT 'CLI NAC',     " Planning Group
  p_zterm  TYPE knb1-zterm       DEFAULT 'TB30',        " Terms of Payment Key
  p_akont  TYPE knb1-akont       DEFAULT '1121121001',  " Reconciliation Account in General Ledger '2110130108'.
  p_zwels  TYPE knb1-zwels       DEFAULT 'D'.           " List of Respected Payment Methods
SELECTION-SCREEN END OF BLOCK b03.

SELECTION-SCREEN BEGIN OF BLOCK b04 WITH FRAME TITLE TEXT-t04.
SELECT-OPTIONS:
    r_vkorg FOR knvv-vkorg       DEFAULT 'VG01'.        " Sales Organization
PARAMETERS:
  p_vtweg TYPE knvv-vtweg       DEFAULT '01',           " Distribution Channel
  p_spart TYPE knvv-spart       DEFAULT '01',           " Division
  p_kalks TYPE knvv-kalks       DEFAULT '1',            " Customer Classification for Pricing Procedure Determination
  p_ktgrd TYPE knvv-ktgrd       DEFAULT '01',           " Account Assignment Group for this customer
  p_vsbed TYPE knvv-vsbed       DEFAULT '01',           " Shipping Conditions
  p_tatyp TYPE knvi-tatyp       DEFAULT 'IBRX',
  p_taxkd TYPE knvi-taxkd       DEFAULT '1'.
SELECTION-SCREEN END OF BLOCK b04.

SELECTION-SCREEN SKIP.

SELECTION-SCREEN BEGIN OF BLOCK b05 WITH FRAME TITLE TEXT-t05.
PARAMETERS:
  p_updt   TYPE abap_bool AS CHECKBOX DEFAULT abap_true,
  p_test   TYPE xtest     AS CHECKBOX DEFAULT abap_true,
  p_maxprc TYPE pbtwpnum DEFAULT 10.
SELECTION-SCREEN COMMENT /1(75) stxt1.
SELECTION-SCREEN COMMENT /1(75) stxt2.
SELECTION-SCREEN END OF BLOCK b05.

SELECTION-SCREEN END OF BLOCK b01.
