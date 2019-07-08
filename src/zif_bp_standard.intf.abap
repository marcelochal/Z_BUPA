INTERFACE zif_bp_standard
  PUBLIC .

  INTERFACES:
    if_amdp_marker_hdb,
    if_cvi_common_constants,
    if_fsbp_generic_constants.

* TYPES declaration
  TYPES :

    ty_t_lfa1      TYPE STANDARD TABLE OF lfa1        WITH KEY lifnr
                    WITH UNIQUE SORTED KEY sort_key COMPONENTS lifnr,            " Supplier Master (General Section)
    ty_t_lfb1      TYPE STANDARD TABLE OF lfb1        WITH KEY lifnr bukrs
                    WITH UNIQUE SORTED KEY sort_key COMPONENTS lifnr bukrs      " Vendor Master (Company Code)
                    WITH NON-UNIQUE SORTED KEY sort_key_bukrs COMPONENTS bukrs,
    ty_t_lfm1      TYPE STANDARD TABLE OF lfm1        WITH KEY lifnr ekorg
                    WITH UNIQUE SORTED KEY sort_key COMPONENTS lifnr ekorg,      " Vendor master record purchasing organization data
    ty_t_lfm2      TYPE STANDARD TABLE OF lfm2        WITH KEY lifnr ekorg
                    WITH UNIQUE SORTED KEY sort_key COMPONENTS lifnr ekorg,     " Vendor Master Record: Purchasing Data
    ty_t_lfat      TYPE STANDARD TABLE OF lfat        WITH KEY lifnr taxgr
                    WITH UNIQUE SORTED KEY sort_key COMPONENTS lifnr taxgr,      " Vendor master record (tax groupings)
    ty_t_lfbw      TYPE STANDARD TABLE OF lfbw        WITH KEY lifnr bukrs witht
                    WITH UNIQUE SORTED KEY sort_key COMPONENTS lifnr bukrs witht," Vendor master record (withholding tax types) X
    ty_t_lfbk      TYPE STANDARD TABLE OF lfbk        WITH KEY lifnr banks bankl bankn
                    WITH UNIQUE SORTED KEY sort_key COMPONENTS lifnr banks bankl bankn," Vendor Master (Bank Details)
    ty_t_lfb5      TYPE STANDARD TABLE OF lfb5        WITH KEY lifnr bukrs maber
                    WITH UNIQUE SORTED KEY sort_key COMPONENTS lifnr bukrs maber," Vendor master (dunning data
    ty_t_lfza      TYPE STANDARD TABLE OF lfza        WITH KEY lifnr bukrs empfk
                    WITH UNIQUE SORTED KEY sort_key COMPONENTS lifnr bukrs empfk," Permitted Alternative Payee
    ty_t_wyt3      TYPE STANDARD TABLE OF wyt3        WITH KEY lifnr ekorg ltsnr
                                                               werks parvw parza
                    WITH UNIQUE SORTED KEY sort_key COMPONENTS lifnr ekorg ltsnr
                                                               werks parvw parza," Partner Functions
    ty_t_lfa1_text TYPE STANDARD TABLE OF vmds_lfa1_text
                    WITH DEFAULT KEY
                    WITH UNIQUE SORTED KEY sort_key COMPONENTS lifnr tdkey-id,
    ty_t_lfb1_text TYPE STANDARD TABLE OF vmds_lfb1_text
                    WITH DEFAULT KEY
                    WITH UNIQUE SORTED KEY sort_key COMPONENTS lifnr bukrs tdkey-id,
    ty_t_lfm1_text TYPE STANDARD TABLE OF vmds_lfm1_text
                    WITH DEFAULT KEY
                    WITH UNIQUE SORTED KEY sort_key COMPONENTS lifnr ekorg tdkey-id,
    ty_t_t001_key  TYPE STANDARD TABLE OF t001_key  WITH KEY bukrs
                   WITH UNIQUE SORTED KEY sort_key COMPONENTS bukrs,
    ty_t_t024e_key TYPE STANDARD TABLE OF t024e_key WITH KEY ekorg
                   WITH UNIQUE SORTED KEY sort_key COMPONENTS ekorg,

    BEGIN OF ty_s_vendor_lf_1,
      lfb1      TYPE ty_t_lfb1,   " Vendor Master (Company Code)
      lfm1      TYPE ty_t_lfm1,   " Vendor Master Record purchasing organization data
      lfm2      TYPE ty_t_lfm2,   " Vendor Master Record: Purchasing Data
      lfat      TYPE ty_t_lfat,   " Vendor Master Record (withholding tax types)
      lfbw      TYPE ty_t_lfbw,   " Vendor Master Record (withholding tax types)
      lfbk      TYPE ty_t_lfbk,   " Vendor Master (Bank Details)
      lfb5      TYPE ty_t_lfb5,   " Vendor Master (Dunning data)
      lfza      TYPE ty_t_lfza,   " Permitted Alternative Payee
      wyt3      TYPE ty_t_wyt3,   " Partner Functions
      lfa1_text TYPE ty_t_lfa1_text,
      lfb1_text TYPE ty_t_lfb1_text,
      lfm1_text TYPE ty_t_lfm1_text,
    END OF ty_s_vendor_lf_1.

  TYPES: BEGIN OF ty_s_vendor_lf_2,
           lfa1 TYPE lfa1.                 " Supplier Master (General Section)
      INCLUDE TYPE ty_s_vendor_lf_1.
  TYPES: END OF ty_s_vendor_lf_2.

  TYPES: BEGIN OF ty_s_vendor_lf_3,
           lfa1 TYPE ty_t_lfa1.            " Supplier Master (General Section)
      INCLUDE TYPE ty_s_vendor_lf_1.
  TYPES: END OF ty_s_vendor_lf_3,

  BEGIN OF ty_s_bp_key,
    partner      TYPE bu_partner,
    partner_guid TYPE bu_partner_guid,
    vendor       TYPE lifnr,
    customer     TYPE kunnr,
  END OF ty_s_bp_key.

  TYPES: BEGIN OF ty_s_bp.
      INCLUDE TYPE ty_s_bp_key.
  TYPES: partner_bus   TYPE bus_ei_extern,
         vendor_vmds   TYPE vmds_ei_extern,
         vendor_lf     TYPE ty_s_vendor_lf_2,
         custumer_cmds TYPE cmds_ei_extern.
*         custumer_kn   TYPE ty_s_custumer_kn.
  TYPES: END OF ty_s_bp,

  ty_t_bp     TYPE STANDARD TABLE OF ty_s_bp WITH DEFAULT KEY
        WITH UNIQUE SORTED KEY     partner_sort_key   COMPONENTS partner  vendor
        WITH NON-UNIQUE SORTED KEY vendor_sort_key    COMPONENTS vendor   partner,
    ty_t_bp_key TYPE STANDARD TABLE OF ty_s_bp_key WITH DEFAULT KEY
        WITH UNIQUE SORTED KEY     partner_sort_key  COMPONENTS partner  vendor
        WITH NON-UNIQUE SORTED KEY vendor_sort_key   COMPONENTS vendor   partner
        WITH NON-UNIQUE SORTED KEY customer_sort_key COMPONENTS customer partner.

  " Ranges
  TYPES:
    ty_r_ekorg   TYPE RANGE OF ekorg,  " Purchasing organization
    ty_r_bukrs   TYPE RANGE OF bukrs,  " Company Code
    ty_r_lifnr   TYPE RANGE OF lifnr,  " Account Number of Vendor or Creditor
    ty_r_partner TYPE RANGE OF bu_partner.  " Business Partner Number

ENDINTERFACE.
