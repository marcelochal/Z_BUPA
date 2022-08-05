"! <p class="shorttext synchronized" lang="en">Common BP constants and routines</p>
"! <p class="shorttext synchronized" lang="PT">Constantes e rotinas comuns de BP</p>
INTERFACE zif_bp_standard
  PUBLIC .

  INTERFACES:
    if_cvi_common_constants,
    if_fsbp_generic_constants,
    if_amdp_marker_hdb.

  CONSTANTS:
    "! <p class="shorttext synchronized" lang="en">Object task constants</p>
    BEGIN OF co_task,
      insert        TYPE bus_ei_object_task VALUE if_cvi_common_constants~task_insert,
      modify        TYPE bus_ei_object_task VALUE if_cvi_common_constants~task_modify,
      update        TYPE bus_ei_object_task VALUE if_cvi_common_constants~task_update,
      current_state TYPE bus_ei_object_task VALUE if_cvi_common_constants~task_current_state,
    END OF co_task,

    "! <p class="shorttext synchronized" lang="en">Type of BP</p>
    BEGIN OF co_category_bp_as,
      group  TYPE bu_type VALUE if_cvi_common_constants~bp_as_group,
      org    TYPE bu_type VALUE if_cvi_common_constants~bp_as_org,
      person TYPE bu_type VALUE if_cvi_common_constants~bp_as_person,
    END OF co_category_bp_as,

    "! <p class="shorttext synchronized" lang="en">BP Role, function that a business partner takes on</p>
    BEGIN OF co_rolecategory,
      supplier_purchasing TYPE  bu_partnerrolecat VALUE 'FLVN01', "Supplier Purchasing organization
      supplier_fin        TYPE  bu_partnerrolecat VALUE 'FLVN00', "Supplier (Fin.Accounting)
      customer_sales      TYPE  bu_partnerrolecat VALUE 'FLCU01', "Customer
      customer_fin        TYPE  bu_partnerrolecat VALUE 'FLCU00', "Customer (Fin.Accounting)
    END OF co_rolecategory,

    "! <p class="shorttext synchronized" lang="en">Constants for Tax Number Category in Brasil </p>
    BEGIN OF co_taxtype,
      cnpj          TYPE bptaxtype VALUE 'BR1', " BR1 Brasil: nº CNPJ
      cpf           TYPE bptaxtype VALUE 'BR2', " BR2 Brasil: nº CPF
      ins_stadual   TYPE bptaxtype VALUE 'BR3', " BR3 Brasil: inscrição estadual
      ins_municipal TYPE bptaxtype VALUE 'BR4', " BR4 Brasil: inscrição municipal
    END OF co_taxtype,

    BEGIN OF co_grouping,
      nac_pf        TYPE bu_group VALUE 'NACF', " TAESA_NacPesFis TAESA - Nacional PessFísica (CPF)
      nac_pj        TYPE bu_group VALUE 'NACJ', " TAESA_NacPesJur TAESA - Nacional Pessoa Juridica (CNPJ)
      cliente_ons   TYPE bu_group VALUE 'NACO', " Clientes ONS    TAESA - Clientes ONS
      employee      TYPE bu_group VALUE 'FUNC', " TAESA_Funcionar TAESA - Funcionário - atrib. interna
      gov           TYPE bu_group VALUE 'GOVE', " TAESA_Governo   TAESA - Governo - atrib. interna
      inter_company TYPE bu_group VALUE 'INTE', " Comp TAESA - Inter Company
    END OF co_grouping,

    BEGIN OF co_vendor_planning_group,
      pf  TYPE fdgrv VALUE 'FOR PESS F', " F-PesFísic  Fornecedores Pessoa Física
      pj  TYPE fdgrv VALUE 'FORN NAC',  " F-nacional  Fornecedores Nacionais'
      imp TYPE fdgrv VALUE 'FORN IMP',  " Impostos    Impostos
    END OF co_vendor_planning_group,

    co_country_iso_br TYPE intca              VALUE 'BR',         "Codigo ISO BR telefone
    co_languiso_pt    TYPE laiso              VALUE 'PT'.         "Código de idiomas ISO de 2 dígitos

**********************************************************************
*------------------- V E N D O R  T Y P E S -------------------------*
**********************************************************************
  TYPES:
* TYPES declaration
    "! <p class="shorttext synchronized" lang="en">Type of table LFA1 Supplier Master General Section </p>
    ty_t_lfa1      TYPE STANDARD TABLE OF lfa1        WITH KEY lifnr
          WITH UNIQUE SORTED KEY sort_key COMPONENTS lifnr,                     " Supplier Master (General Section)

    "! <p class="shorttext synchronized" lang="en">Type of table LFB1 Vendor Company Code</p>
    ty_t_lfb1      TYPE STANDARD TABLE OF lfb1        WITH KEY lifnr bukrs
                    WITH UNIQUE SORTED KEY sort_key COMPONENTS lifnr bukrs      " Vendor Master (Company Code)
                    WITH NON-UNIQUE SORTED KEY sort_key_bukrs COMPONENTS bukrs,

    "! <p class="shorttext synchronized" lang="en">Type of table LFM1 Vendor purchasing organization data</p>
    ty_t_lfm1      TYPE STANDARD TABLE OF lfm1        WITH KEY lifnr ekorg
                    WITH UNIQUE SORTED KEY sort_key COMPONENTS lifnr ekorg,     " Vendor master record purchasing organization data

    "! <p class="shorttext synchronized" lang="en">Type of table LFM2 Purchasing Data </p>
    ty_t_lfm2      TYPE STANDARD TABLE OF lfm2        WITH KEY lifnr ekorg
               WITH UNIQUE SORTED KEY sort_key COMPONENTS lifnr ekorg,          " Vendor Master Record: Purchasing Data

    "! <p class="shorttext synchronized" lang="en">Type of table LFAT Vendor tax groupings</p>
    ty_t_lfat      TYPE STANDARD TABLE OF lfat        WITH KEY lifnr taxgr
                    WITH UNIQUE SORTED KEY sort_key COMPONENTS lifnr taxgr,     " Vendor master record (tax groupings)

    "! <p class="shorttext synchronized" lang="en">Type of table LFBW Vendor withholding tax types</p>
    ty_t_lfbw      TYPE STANDARD TABLE OF lfbw        WITH KEY lifnr bukrs witht
                WITH UNIQUE SORTED KEY sort_key COMPONENTS lifnr bukrs witht ,   " Vendor master record (withholding tax types) X

    "! <p class="shorttext synchronized" lang="en">Type of table LFBK Vendor Bank Details </p>
    ty_t_lfbk      TYPE STANDARD TABLE OF lfbk        WITH KEY lifnr banks bankl bankn
               WITH UNIQUE SORTED KEY sort_key COMPONENTS lifnr banks bankl bankn , " Vendor Master (Bank Details)

    "! <p class="shorttext synchronized" lang="en">Type of table LFB5 Vendor dunning data</p>
    ty_t_lfb5      TYPE STANDARD TABLE OF lfb5        WITH KEY lifnr bukrs maber
                     WITH UNIQUE SORTED KEY sort_key COMPONENTS lifnr bukrs maber , " Vendor master (dunning data

    "! <p class="shorttext synchronized" lang="en">Type of table LFZA Vendor Permitted Alternative Payer</p>
    ty_t_lfza      TYPE STANDARD TABLE OF lfza        WITH KEY lifnr bukrs empfk
                     WITH UNIQUE SORTED KEY sort_key COMPONENTS lifnr bukrs empfk , " Permitted Alternative Payer

    "! <p class="shorttext synchronized" lang="en">Type of table WYT3 Vendor Partner Functions</p>
    ty_t_wyt3      TYPE STANDARD TABLE OF wyt3        WITH KEY lifnr ekorg ltsnr
                     werks parvw parza " Partner Functions
                     WITH UNIQUE SORTED KEY sort_key COMPONENTS lifnr ekorg ltsnr
                     werks parvw parza ,

    "! <p class="shorttext synchronized" lang="en">Type of table for LFA1 texts</p>
    ty_t_lfa1_text TYPE STANDARD TABLE OF vmds_lfa1_text
                     WITH DEFAULT KEY
                     WITH UNIQUE SORTED KEY sort_key COMPONENTS lifnr tdkey-id ,

    "! <p class="shorttext synchronized" lang="en">Type of table for LFB1 texts</p>
    ty_t_lfb1_text TYPE STANDARD TABLE OF vmds_lfb1_text
                     WITH DEFAULT KEY
                     WITH UNIQUE SORTED KEY sort_key COMPONENTS lifnr bukrs tdkey-id ,

    "! <p class="shorttext synchronized" lang="en">Type of table for LFM1 texts</p>
    ty_t_lfm1_text TYPE STANDARD TABLE OF vmds_lfm1_text
                     WITH DEFAULT KEY
                     WITH UNIQUE SORTED KEY sort_key COMPONENTS lifnr ekorg tdkey-id ,

    "! <p class="shorttext synchronized" lang="en">Type of key fields of table T001</p>
    ty_t_t001_key  TYPE STANDARD TABLE OF t001_key  WITH KEY bukrs
                     WITH UNIQUE SORTED KEY sort_key COMPONENTS bukrs ,

    "! <p class="shorttext synchronized" lang="en">Type of key fields of table T024E</p>
    ty_t_t024e_key TYPE STANDARD TABLE OF t024e_key WITH KEY ekorg
                     WITH UNIQUE SORTED KEY sort_key COMPONENTS ekorg ,

    "! <p class="shorttext synchronized" lang="en">Structure Vendor LF1</p>
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
    END OF ty_s_vendor_lf_1 ,

    "! <p class="shorttext synchronized" lang="en">Structure Vendor LF2</p>
    BEGIN OF ty_s_vendor_lf_2,
      lfa1 TYPE lfa1.                 " Supplier Master (General Section)
      INCLUDE TYPE ty_s_vendor_lf_1.
  TYPES: END OF ty_s_vendor_lf_2 .

  "! <p class="shorttext synchronized" lang="en">Structure Vendor LF3</p>
  TYPES BEGIN OF ty_s_vendor_lf_3.
  TYPES    lfa1 TYPE ty_t_lfa1.            " Supplier Master (General Section)
  INCLUDE TYPE ty_s_vendor_lf_1.
  TYPES END OF ty_s_vendor_lf_3 .



**********************************************************************
*----------------- C U S T O M E R  T Y P E S -----------------------*
**********************************************************************
  TYPES:
* TYPES declaration
    "! <p class="shorttext synchronized" lang="en">Type of table KNA1 General Data in Customer Master</p>
    ty_t_kna1      TYPE STANDARD TABLE OF kna1        WITH KEY kunnr
        WITH UNIQUE SORTED KEY sort_key             COMPONENTS kunnr,           " General Data in Customer Master

    "! <p class="shorttext synchronized" lang="en">Type of table KNB1 Customer Company Code</p>
    ty_t_knb1      TYPE STANDARD TABLE OF knb1        WITH KEY kunnr bukrs
        WITH UNIQUE     SORTED KEY sort_key         COMPONENTS kunnr bukrs      " Customer Master (Company Code)
        WITH NON-UNIQUE SORTED KEY sort_key_bukrs   COMPONENTS bukrs,

    "! <p class="shorttext synchronized" lang="en">Type of table KNM1 Customer Sales Data</p>
    ty_t_knvv      TYPE STANDARD TABLE OF knvv        WITH KEY kunnr vkorg
        WITH UNIQUE SORTED KEY sort_key             COMPONENTS kunnr vkorg,     " Customer Master Sales Data

    "! <p class="shorttext synchronized" lang="en">Type of table KNAT Customer Tax Groupings</p>
    ty_t_knat      TYPE STANDARD TABLE OF knat        WITH KEY kunnr taxgr
        WITH UNIQUE SORTED KEY sort_key             COMPONENTS kunnr taxgr,     " Customer Master Record (Tax Groupings)

    "! <p class="shorttext synchronized" lang="en">Type of table KNBW Customer withholding tax types</p>
    ty_t_knbw      TYPE STANDARD TABLE OF knbw        WITH KEY kunnr bukrs witht
       WITH UNIQUE SORTED KEY sort_key              COMPONENTS kunnr bukrs witht ,   " Customer master record (withholding tax types) X

    "! <p class="shorttext synchronized" lang="en">Type of table KNBK Customer Bank Details</p>
    ty_t_knbk      TYPE STANDARD TABLE OF knbk        WITH KEY kunnr banks bankl bankn
       WITH UNIQUE SORTED KEY sort_key              COMPONENTS kunnr banks bankl bankn , " Customer Master (Bank Details)

    "! <p class="shorttext synchronized" lang="en">Type of table KNZA Customer Permitted Alternative Payer</p>
    ty_t_knza      TYPE STANDARD TABLE OF knza        WITH KEY kunnr bukrs empfd
        WITH UNIQUE SORTED KEY sort_key             COMPONENTS kunnr bukrs empfd , " Permitted Alternative Payer

    "! <p class="shorttext synchronized" lang="en">Type of table KNVP Customer Partner Functions</p>
    ty_t_knvp      TYPE STANDARD TABLE OF knvp        WITH KEY kunnr vkorg vtweg " Customer Master Partner Functions
                                                               spart parvw parza " Partner Functions
        WITH UNIQUE SORTED KEY sort_key COMPONENTS             kunnr vkorg vtweg
                                                               spart parvw parza ,

    "! <p class="shorttext synchronized" lang="en">Type of table for KNA1 texts</p>
    ty_t_kna1_text TYPE STANDARD TABLE OF cmds_kna1_text WITH KEY kunnr
        WITH UNIQUE SORTED KEY sort_key COMPONENTS kunnr tdkey-id ,

    "! <p class="shorttext synchronized" lang="en">Type of table for KNB1 texts</p>
    ty_t_knb1_text TYPE STANDARD TABLE OF cmds_knb1_text WITH KEY kunnr bukrs
        WITH UNIQUE SORTED KEY sort_key COMPONENTS kunnr bukrs tdkey-id ,

    "! <p class="shorttext synchronized" lang="en">Type of table for KNVV texts</p>
    ty_t_knvv_text TYPE STANDARD TABLE OF cmds_knvv_text WITH KEY kunnr vkorg vtweg
        WITH UNIQUE SORTED KEY sort_key COMPONENTS kunnr vkorg tdkey-id ,

    "! <p class="shorttext synchronized" lang="en">Type of key fields of table TVKO</p>
    ty_t_tvko      TYPE STANDARD TABLE OF tvko_key WITH KEY vkorg
        WITH UNIQUE SORTED KEY sort_key COMPONENTS vkorg,

    ty_t_zj1btxcli TYPE STANDARD TABLE OF zj1btxcli WITH KEY mandt kunnr bukrs codimp
        WITH NON-UNIQUE SORTED KEY sort_key COMPONENTS kunnr bukrs codimp,

    "! <p class="shorttext synchronized" lang="en">Structure Customer </p>
    BEGIN OF ty_s_customer_kn_1,
      knb1      TYPE ty_t_knb1,   " Customer Master (Company Code)
      knvv      TYPE ty_t_knvv,   " Customer Master Sales Data
      knat      TYPE ty_t_knat,   " Customer Master Record (Tax Groupings)
      knbw      TYPE ty_t_knbw,   " Customer Master record (withholding tax types) X
      knbk      TYPE ty_t_knbk,   " Customer Master (Bank Details)
      knza      TYPE ty_t_knza,   " Permitted Alternative Payer
      knvp      TYPE ty_t_knvp,   " Customer Master Partner Functions
      kna1_text TYPE ty_t_kna1_text,    " Central Text
      knb1_text TYPE ty_t_knb1_text,    " Accounting Text
      knvv_text TYPE ty_t_knvv_text,    " Texts Customer Master Sales Data
      zj1btxcli TYPE ty_t_zj1btxcli,    " Taxes for AVC
    END OF ty_s_customer_kn_1 ,

    "! <p class="shorttext synchronized" lang="en">Structure Customer KN2</p>
    BEGIN OF ty_s_customer_kn_2,
      kna1 TYPE kna1.                 " Supplier Master (General Section)
      INCLUDE TYPE ty_s_customer_kn_1.
  TYPES: END OF ty_s_customer_kn_2 .

  "! <p class="shorttext synchronized" lang="en">Structure Customer KN3</p>
  TYPES BEGIN OF ty_s_customer_kn_3.
  TYPES    kna1 TYPE ty_t_kna1.       " General Data in Customer Master
  INCLUDE TYPE ty_s_customer_kn_1.
  TYPES END OF ty_s_customer_kn_3 .

**********************************************************************

  TYPES:
    "! <p class="shorttext synchronized" lang="en">Key Fields for struture BP</p>
    BEGIN OF ty_s_bp_key,
      partner      TYPE bu_partner,
      partner_guid TYPE bu_partner_guid,
      taxnum       TYPE bptaxnum,
      vendor       TYPE lifnr,
      customer     TYPE kunnr,
    END OF ty_s_bp_key .

  "! <p class="shorttext synchronized" lang="en">Structure BP</p>
  TYPES BEGIN OF ty_s_bp.
  INCLUDE TYPE ty_s_bp_key.
  TYPES: partner_bus   TYPE bus_ei_extern,
         vendor_vmds   TYPE vmds_ei_extern,
         vendor_lf     TYPE ty_s_vendor_lf_2,
         customer_cmds TYPE cmds_ei_extern,
         customer_kn   TYPE ty_s_customer_kn_2.
  TYPES END OF ty_s_bp .

  TYPES:
    "! <p class="shorttext synchronized" lang="en">Table BP</p>
    ty_t_bp     TYPE STANDARD TABLE OF ty_s_bp WITH DEFAULT KEY
        WITH NON-UNIQUE SORTED KEY partner_sort_key   COMPONENTS partner  vendor
        WITH NON-UNIQUE SORTED KEY vendor_sort_key    COMPONENTS vendor   partner
        WITH NON-UNIQUE SORTED KEY customer_sort_key  COMPONENTS customer partner ,

    "! <p class="shorttext synchronized" lang="en">Type of key fields of table BP</p>
    ty_t_bp_key TYPE STANDARD TABLE OF ty_s_bp_key WITH DEFAULT KEY
        WITH NON-UNIQUE SORTED KEY partner_sort_key  COMPONENTS partner  vendor
        WITH NON-UNIQUE SORTED KEY vendor_sort_key   COMPONENTS vendor   partner
        WITH NON-UNIQUE SORTED KEY customer_sort_key COMPONENTS customer partner .

  TYPES:
    " Ranges
    "! <p class="shorttext synchronized" lang="en">Range of EKORG</p>
    ty_r_ekorg   TYPE RANGE OF ekorg , " Purchasing organization

    "! <p class="shorttext synchronized" lang="en">Range of VKORG</p>
    ty_r_vkorg   TYPE RANGE OF vkorg , " Sales Organization

    "! <p class="shorttext synchronized" lang="en">Range of BUKRS</p>
    ty_r_bukrs   TYPE RANGE OF bukrs , " Company Code

    "! <p class="shorttext synchronized" lang="en">Range of LIFNR</p>
    ty_r_lifnr   TYPE RANGE OF lifnr , " Account Number of Vendor or Creditor

    "! <p class="shorttext synchronized" lang="en">Range of KUNNR</p>
    ty_r_kunnr   TYPE RANGE OF kunnr , " Customer Number

    "! <p class="shorttext synchronized" lang="en">Range of PARTNER</p>
    ty_r_partner TYPE RANGE OF bu_partner.

  "! <p class="shorttext synchronized" lang="en">Object of message progress indicator</p>
  DATA o_prog_ind TYPE REF TO zcl_progress_indicator .


  CLASS-METHODS:

    "! <p class="shorttext synchronized" lang="en">Get list of fields in table or structure</p>
    get_field_list
      IMPORTING
        !im_r_data       TYPE REF TO data
      EXPORTING
        !ex_t_components TYPE cl_abap_structdescr=>component_table
      RETURNING
        VALUE(r_result)  TYPE ddfields ,
    "! <p class="shorttext synchronized" lang="en">Change Indicator Object of BP structures to I, U, M, D</p>
    change_task
      IMPORTING
        !im_v_task  TYPE bus_ei_object_task
      CHANGING
        !ch_t_table TYPE ANY TABLE OPTIONAL
        !ch_s_struc TYPE any OPTIONAL ,

    "! Get BP From Vendor
    "! Vendor Number @parameter im_v_vendor |
    "! BP @parameter ex_v_partner |
    "! BP Guid @parameter ex_v_partner_guid |
    get_bp_from_vendor
          AMDP OPTIONS READ-ONLY CDS SESSION CLIENT current
      IMPORTING
        VALUE(im_v_vendor)       TYPE lifnr
      EXPORTING
        VALUE(ex_v_partner)      TYPE bu_partner_t
        VALUE(ex_v_partner_guid) TYPE bu_partner_guid ,

    "! <p class="shorttext synchronized" lang="en">Get BP From Tax Number for all data in internal table</p>
    "!
    "! @parameter ch_t_bp | <p class="shorttext synchronized" lang="en"></p>
    get_bp_from_taxnumber
          AMDP OPTIONS READ-ONLY CDS SESSION CLIENT current
      CHANGING
        VALUE(ch_t_bp) TYPE ty_t_bp_key,

    "! <p class="shorttext synchronized" lang="en">Create GUID of a Business Partner in CHAR 32 Format</p>
    "!
    "! @parameter r_result | <p class="shorttext synchronized" lang="en">Return generate GUID</p>
    create_bp_partner_guid RETURNING VALUE(r_result)   TYPE bu_partner_guid_bapi,

    "! <p class="shorttext synchronized" lang="en">Get First Name of full name</p>
    "!
    "! @parameter im_v_full_name | <p class="shorttext synchronized" lang="en">Full name</p>
    "! @parameter r_first_name | <p class="shorttext synchronized" lang="en">Return the first name</p>
    get_first_name
      IMPORTING
        im_v_full_name      TYPE c
      RETURNING
        VALUE(r_first_name) TYPE string,

    "! <p class="shorttext synchronized" lang="en">Get Last Names of full name</p>
    "!
    "! @parameter im_v_full_name | <p class="shorttext synchronized" lang="en">Full Name</p>
    "! @parameter r_last_name | <p class="shorttext synchronized" lang="en">Return the last name</p>
    get_last_name
      IMPORTING
        im_v_full_name     TYPE c
      RETURNING
        VALUE(r_last_name) TYPE string.

ENDINTERFACE.
