*&---------------------------------------------------------------------*
*& Report ZBUPA_CARGA_DADOS_BANC_FORN
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zbupa_carga_dados_banc_forn.

TABLES: lfbk, sscrfields.

SELECTION-SCREEN: FUNCTION KEY 1.
SELECTION-SCREEN BEGIN OF BLOCK b011 WITH FRAME TITLE TEXT-t02.

PARAMETERS:
  rb_file RADIOBUTTON GROUP rb2 MODIF ID rb2 DEFAULT 'X'.

PARAMETERS:
  p_file  TYPE rlgrap-filename MEMORY ID cc_parameter_id ##EXISTS.

SELECTION-SCREEN SKIP.
PARAMETERS:
 rb_forn RADIOBUTTON GROUP rb2 MODIF ID rb2.

SELECT-OPTIONS : s_forn  FOR  lfbk-lifnr.

SELECTION-SCREEN SKIP.
PARAMETERS:
  p_test           TYPE xtest   AS CHECKBOX DEFAULT abap_true.
SELECTION-SCREEN END OF BLOCK b011.

TYPES: BEGIN OF ty_e_upload_layout,
         lifnr TYPE lifnr,      " Nº conta do fornecedor
         banks TYPE banks,      " Código do país do banco
         bankl TYPE bankk,      " Chave do banco
         bankn TYPE bankn,      " Nº conta bancária
         bkont TYPE bkont,      " Nº conta bancária
         bvtyp TYPE bvtyp,      " Tipo de banco do parceiro
         xezer TYPE xezer,      " Código: existe ordem de autorização de débito direto?
         bkref TYPE bkref,      " Indicação de referência para os dados bancários
         koinh TYPE koinh_fi,   " Nome do titular da conta
       END OF ty_e_upload_layout,

       BEGIN OF ty_but000,
         partner      TYPE but000-partner,
         partner_guid TYPE but000-partner_guid,
         vendor	      TYPE cvi_vend_link-vendor,
       END OF ty_but000.


DATA:
  gt_lfbk          TYPE TABLE OF lfbk,
  gt_cvi_vend_link TYPE TABLE OF cvi_vend_link,
  gt_but000        TYPE STANDARD TABLE OF ty_but000       WITH NON-UNIQUE SORTED KEY but000_sec_key COMPONENTS partner vendor,
  gt_table_excel   TYPE STANDARD TABLE OF ty_e_upload_layout WITH NON-UNIQUE SORTED KEY excel_sec_key COMPONENTS lifnr.


INCLUDE zbupa_carga_dados_banc_forn_f1.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_file.
  PERFORM select_file USING p_file.

AT SELECTION-SCREEN.
  CASE sscrfields-ucomm.
    WHEN'FC01'.
*      CALL METHOD lcl_file=>export_model.
  ENDCASE.

START-OF-SELECTION.

  CASE abap_on.
    WHEN rb_file.
      PERFORM:
      upload,
      ler_excel.

    WHEN rb_forn.
      PERFORM:
      change_bkvid.

  ENDCASE.
