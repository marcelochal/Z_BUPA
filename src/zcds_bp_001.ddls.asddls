@AbapCatalog.sqlViewName: 'ZBPV0001'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'View for Business Partner select data'

define view ZCDS_BP_001
    as select from      but000
        left outer join dfkkbptaxnum  as bpTAXNUM
            on bpTAXNUM.partner = but000.partner
        left outer join cvi_vend_link as vend
            on but000.partner_guid = vend.partner_guid
        left outer join cvi_cust_link as cust
            on but000.partner_guid = cust.partner_guid
    {

        key but000.client       as Client,
        key but000.partner      as Partner,
            cust.customer       as Customer,
            vend.vendor         as Vendor,
            but000.type         as Type,
            but000.bu_group     as BuGroup,
            but000.bu_sort1     as BuSort1,
            but000.bu_sort2     as BuSort2,
            but000.title        as Title,
            but000.xblck        as Xblck,
            but000.name_org1    as NameOrg1,
            but000.name_org2    as NameOrg2,
            but000.name_org3    as NameOrg3,
            but000.name_org4    as NameOrg4,
            but000.name_last    as NameLast,
            but000.name_first   as NameFirst,
            but000.name_lst2    as NameLst2,
            but000.name_last2   as NameLast2,
            but000.namemiddle   as Namemiddle,
            but000.name1_text   as Name1Text,
            but000.nickname     as Nickname,
            but000.initials     as Initials,
            but000.persnumber   as Persnumber,
            but000.mc_name1     as McName1,
            but000.mc_name2     as McName2,
            but000.partner_guid as PartnerGuid,
            but000.addrcomm     as Addrcomm,
            but000.natpers      as Natpers,
            but000.nuc_sec      as NucSec,
            but000.par_rel      as ParRel,
            but000.bp_sort      as BpSort,
            but000.kbanks       as Kbanks,
            but000.kbankl       as Kbankl,
            bpTAXNUM.taxtype    as Taxtype,
            bpTAXNUM.taxnum     as Taxnum,
            bpTAXNUM.taxnumxl   as Taxnumxl

    }
