"! <p class="shorttext synchronized" lang="en">Vendor / BP extension to company and purchasing organization</p>
INTERFACE zif_bp_extend_vendor
  PUBLIC .

  INTERFACES zif_bp_standard .

  ALIASES co_task_current_state
  FOR if_cvi_common_constants~task_current_state .
  ALIASES co_task_insert
    FOR if_cvi_common_constants~task_insert .
  ALIASES co_task_modify
    FOR if_cvi_common_constants~task_modify .
  ALIASES co_task_update
    FOR if_cvi_common_constants~task_update .
  ALIASES ty_r_bukrs
    FOR zif_bp_standard~ty_r_bukrs .                                 " Company Code
  ALIASES ty_r_ekorg
    FOR zif_bp_standard~ty_r_ekorg .                                 " Purchasing organization
  ALIASES ty_r_lifnr
    FOR zif_bp_standard~ty_r_lifnr .                                 " Account Number of Vendor or Creditor
  ALIASES ty_r_partner
    FOR zif_bp_standard~ty_r_partner .
  ALIASES ty_s_vendor_lf_3
    FOR zif_bp_standard~ty_s_vendor_lf_3 .
  ALIASES ty_t_bp
    FOR zif_bp_standard~ty_t_bp .
  ALIASES ty_t_bp_key
    FOR zif_bp_standard~ty_t_bp_key .
  ALIASES ty_t_lfa1_text
    FOR zif_bp_standard~ty_t_lfa1_text .

  TYPES: BEGIN OF ty_s_bp_extend.
      INCLUDE TYPE zif_bp_standard~ty_s_bp.
  TYPES: bukrs_extended TYPE zif_bp_standard~ty_t_t001_key,
         ekorg_extended TYPE zif_bp_standard~ty_t_t024e_key,
         object_msg     TYPE bapiretct.
  TYPES: END OF ty_s_bp_extend ,

  ty_t_bp_extend TYPE STANDARD TABLE OF ty_s_bp_extend WITH DEFAULT KEY
                  WITH UNIQUE SORTED KEY     partner_sort_key  COMPONENTS partner  vendor
                  WITH NON-UNIQUE SORTED KEY vendor_sort_key   COMPONENTS vendor   partner .
  DATA:
    bukrs_from  TYPE bukrs,
    ekorg_from  TYPE ekorg,
    t_bukrs_for TYPE zif_bp_standard~ty_t_t001_key,
    t_ekorg_for TYPE zif_bp_standard~ty_t_t024e_key,
    t_bp_data   TYPE ty_t_bp_extend.

ENDINTERFACE.
