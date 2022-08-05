*&---------------------------------------------------------------------*
*& Report ZBUPA_CARGA_DADOS_BANC_FORN
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zbupa_carga_dados_banc_forn.

TABLES: lfbk, sscrfields.

CONSTANTS
    co_parameter_id1 TYPE memoryid VALUE zcl_file_upload=>co_parameter_id1.


SELECTION-SCREEN BEGIN OF BLOCK b011 WITH FRAME TITLE TEXT-t02.
SELECTION-SCREEN: FUNCTION KEY 1.
PARAMETERS:
  p_file  TYPE rlgrap-filename MEMORY ID co_parameter_id1 ##EXISTS.

*SELECTION-SCREEN SKIP.

*SELECT-OPTIONS : s_forn  FOR  lfbk-lifnr.

SELECTION-SCREEN SKIP.
PARAMETERS:
  p_test           TYPE xtest   AS CHECKBOX DEFAULT abap_true.
SELECTION-SCREEN END OF BLOCK b011.

TYPES:
  BEGIN OF ty_s_upload_layout,
    lifnr           TYPE lifnr,          " Nº conta do fornecedor
    bankdetailid    TYPE bu_bkvid,       " Bank details ID
    bank_ctry       TYPE banks,          " Código do país do banco
    bank_key        TYPE bankk,          " Chave do banco
    bank_acct       TYPE bankn,          " Nº conta bancária
    ctrl_key        TYPE bkont,          " Bank Control Key
    bankaccountname TYPE bu_bankaccname, " Name of Bank Account
  END OF ty_s_upload_layout,

  ty_t_upload_layout TYPE STANDARD TABLE OF ty_s_upload_layout
      WITH NON-UNIQUE SORTED KEY key_lifnr  COMPONENTS lifnr bankdetailid,

  BEGIN OF ty_s_but000,
    partner      TYPE but000-partner,
    partner_guid TYPE but000-partner_guid,
    vendor       TYPE cvi_vend_link-vendor,
    name1_text   TYPE but000-name1_text,
  END OF ty_s_but000,
  ty_t_but000 TYPE STANDARD TABLE OF ty_s_but000
    WITH DEFAULT KEY WITH NON-UNIQUE SORTED KEY but000_sec_key COMPONENTS partner vendor.

DATA:
  gt_but000      TYPE ty_t_but000,
  gt_table_excel TYPE ty_t_upload_layout,
  go_file        TYPE REF TO zcl_file_upload.


INCLUDE zbupa_carga_dados_banc_forn_f1.

INITIALIZATION.
  sscrfields = zcl_file_upload=>set_sscrtexts_export_model( ).

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_file.
  CALL METHOD zcl_file_upload=>select_file_open_dialog
    RECEIVING
      r_filename = p_file.


AT SELECTION-SCREEN ON BLOCK b011.
  CASE sscrfields-ucomm.
    WHEN'FC01'.
      CALL METHOD zcl_file_upload=>export_model
        EXPORTING
          im_model = gt_table_excel.

  ENDCASE.

START-OF-SELECTION.

  CREATE OBJECT go_file
    EXPORTING
      im_v_file_path = p_file.

  CALL METHOD go_file->upload_file
    EXPORTING
      im_v_use_abapxlsx     = abap_off
    CHANGING
      ch_tab_converted_data = gt_table_excel
    EXCEPTIONS
      conversion_failed     = 1
      upload_date_not_found = 2
      OTHERS                = 3.

  IF sy-subrc NE 0.
    MESSAGE ID sy-msgid TYPE 'S' NUMBER sy-msgno
      WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 DISPLAY LIKE 'E'.
    STOP.
  ENDIF.

  PERFORM ler_excel.
