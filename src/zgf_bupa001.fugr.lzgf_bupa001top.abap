FUNCTION-POOL zgf_bupa001.                  "MESSAGE-ID ..

TYPES:
  BEGIN OF ty_s_bukrs,
    bukrs TYPE t001-bukrs,
  END OF ty_s_bukrs,

  BEGIN OF ty_s_forn,
    bukrs TYPE lfb1-bukrs,
    ekorg TYPE lfm1-ekorg,
    akont TYPE lfb1-akont,
  END OF ty_s_forn.

*  BEGIN OF ty_s_lfbw.
*    INCLUDE STRUCTURE lfbw.
*TYPES:END OF ty_s_lfbw.

TYPES:
  ty_t_forn         TYPE TABLE OF ty_s_forn,
  ty_t_bukrs        TYPE TABLE OF ty_s_bukrs,
  ty_t_mensagem     TYPE TABLE OF zpf_mensagem,
  ty_t_zpf_banco    TYPE TABLE OF zpf_banco,
  ty_t_zpf_ir       TYPE TABLE OF zpf_ir,
  ty_t_zpf_telefone TYPE TABLE OF zpf_telefone,
  ty_t_zpf_celular  TYPE TABLE OF zpf_celular,
  ty_t_zpf_fax      TYPE TABLE OF zpf_fax,
  ty_t_zpf_email    TYPE TABLE OF zpf_email,
  ty_s_taxnumber    TYPE bus_ei_bupa_taxnumber.

CONSTANTS:
  gc_akont                    TYPE lfb1-akont         VALUE '2110130108',
  gc_default_fixed_tel        TYPE ad_flgmob          VALUE '1', " Telephone is Default Under Landline Telephones
  gc_default_mobile_tel       TYPE ad_flgmob          VALUE '3', " Default Mobile Telephone
  gc_languiso_pt              TYPE laiso              VALUE 'PT',
  gc_moeda_brl                TYPE lfm1-waers         VALUE 'BRL',
  gc_object_task_insert       TYPE bus_ei_object_task VALUE 'I',
  gc_object_task_update       TYPE bus_ei_object_task VALUE 'U',          "
  gc_pessoa_fisica_nacf       TYPE lfb1-fdgrv         VALUE 'NACF',
  gc_pessoa_juridica_nacj     TYPE lfb1-fdgrv         VALUE 'NACJ',
  gc_bp_category_person       TYPE bu_type            VALUE '1',
  gc_bp_category_organization TYPE bu_type            VALUE '2',
  gc_zterm_tb30               TYPE dzterm             VALUE 'TB30'. " Default Terms of Payment Key

CLASS:
    lcl_bal_log     DEFINITION DEFERRED,
    lcl_log_create  DEFINITION DEFERRED,
    lcl_log_update  DEFINITION DEFERRED,
    lcl_log_display DEFINITION DEFERRED.


DATA:
  "lv_partner           TYPE but000-partner,
  gs_bukrs            TYPE ty_s_bukrs,
  gt_bukrs            TYPE ty_t_bukrs,
  gt_forn             TYPE ty_t_forn,
  gt_cvis_ei_extern_t TYPE cvis_ei_extern_t,
  gs_taxnumber        TYPE ty_s_taxnumber,
  gv_flag_erro        TYPE abap_bool,
  gv_key              TYPE sweinstcou-objkey,
  gv_partner_ret      TYPE bu_partner,
  gs_cvis_ei_extern   TYPE cvis_ei_extern,
  gs_cvis_ei_bp       TYPE cvis_ei_extern,
  gs_cvis_ei_contact  TYPE cvis_ei_extern,
  go_bal_log          TYPE REF TO lcl_bal_log.

INCLUDE lzgf_bupa001d01.
