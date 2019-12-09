// CDS Entity - association
//
// Purpose: Sample Association View
//
// Author : Simon Li  Jul 2019
//
// Be aware that each individual view should be put into a separate cds file 
@AbapCatalog.sqlViewName: 'ZV_ORD_HDR'
@AbapCatalog.compiler.compareFilter: true
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Order Header Information'
define view ZCDS_MSDORDER_HDR 
as select from JKAK 
{
    vbeln,
    kunag, 
    erdat,
    vbtyp,
    auart,
    vkorg,
    vtweg,
    spart,
    netwr,
    waerk   
}

@AbapCatalog.sqlViewName: 'ZV_ORD_ITM'
@AbapCatalog.compiler.compareFilter: true
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Order Item Information'
define view ZCDS_MSDORDER_ITM 
as select from JKAP 
{
    vbeln,
    posnr,
    posex,
    poart,
    kunwe,
    drerz,
    pva,
    gueltigvon,
    gueltigbis,
    vbeln_vl,
    posnr_ur 
}

@AbapCatalog.sqlViewName: 'ZV_ORDER_ITEM'
@AbapCatalog.compiler.compareFilter: true
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Order Item Information'
define view ZCDS_MSDORDER_ORDER_ITEM 
as select from ZCDS_MSDORDER_HDR as _header
   inner join ZCDS_MSDORDER_ITM as _OrderItems
         on _header.vbeln = _OrderItems.vbeln  
{
    key vbeln, 
        kunag,
        _OrderItems.posex,
        _OrderItems.posnr,
        _OrderItems.kunwe   
}

-- Association on ABAP CDS Views
@AbapCatalog.sqlViewName: 'ZV_ORDITEM_ASSO1'
@AbapCatalog.compiler.compareFilter: true
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Order Item Information'
define view ZCDS_MSDORDER_ORDITM1 
as select from ZCDS_MSDORDER_HDR as _header
   association to ZCDS_MSDORDER_ITM as _OrderItems
               on _header.vbeln = _OrderItems.vbeln  
{
    key vbeln, 
        erdat,
        kunag,
        vbtyp,
        auart,
        vkorg,
        vtweg,
        spart,
        netwr,
        waerk,
        _OrderItems.posex,
        _OrderItems.posnr   
}

-- Association with cardinality
-- [min..max]
-- max cannot be 0.
-- An asterisk * for max means any number of rows.
-- min can be omitted (set to 0 if omitted).
-- min cannot be *.
-- When an association is used in a WHERE condition, 1 must be specified for max
@AbapCatalog.sqlViewName: 'ZV_ORDITEM_ASSO2'
@AbapCatalog.compiler.compareFilter: true
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Order Item Information'
define view ZCDS_MSDORDER_ORDITM2 
as select from ZCDS_MSDORDER_HDR as _header
   association [1..*] to ZCDS_MSDORDER_ITM as _OrderItems
               on _header.vbeln = _OrderItems.vbeln  
{
    key vbeln, 
        kunag,
        _OrderItems.kunwe,
        _OrderItems.posex,
        _OrderItems.posnr   
}

-- Association ON condition rules
-- When specifying the ON condition with association, following rules are applied.
-- The fields specified in the ON condition should be included in the SELECT list. 
-- In our example, the field VBELN is specified in ON condition and SELECT list.
-- The fields of source data source can be prefixed with $projection instead of data source name.
@AbapCatalog.sqlViewName: 'ZV_ORDITEM_ASSO3'
@AbapCatalog.compiler.compareFilter: true
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Order Item Information'
define view ZCDS_MSDORDER_ORDITM3 
as select from ZCDS_MSDORDER_HDR as _header
   association [1..*] to ZCDS_MSDORDER_ITM as _OrderItems
               on $projection.vbeln = _OrderItems.vbeln  
{ 
    key vbeln,
        kunag,
        _OrderItems.kunwe,
        _OrderItems.posex,
        _OrderItems.posnr       
}

-- To achieve INNER JOIN, you need to define the attribute in the path expression.
@AbapCatalog.sqlViewName: 'ZV_ORDITEM_ASSO4'
@AbapCatalog.compiler.compareFilter: true
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Order Item Information'
define view ZCDS_MSDORDER_ORDITM4 
as select from ZCDS_MSDORDER_HDR as _header
   association [1..*] to ZCDS_MSDORDER_ITM as _OrderItems
               on $projection.vbeln = _OrderItems.vbeln  
{ 
    key vbeln,
        kunag,
        _OrderItems[inner].kunwe,
        _OrderItems[inner].posex,
        _OrderItems[inner].posnr       
}

-- Sample code to consume the views
--DATA(ls_supported) = cl_abap_dbfeatures=>use_features(
--        requested_features = VALUE #( ( cl_abap_dbfeatures=>views_with_parameters ) ) ).
--IF ls_supported IS NOT INITIAL.
--  SELECT * FROM zcds_msdorder_orditm4
--    INTO TABLE @DATA(lt_data).
--
--  SELECT vbeln, _orderitems-posex AS posex, _orderitems-posnr AS posnr
--    FROM zcds_msdorder_orditm4
--    INTO TABLE @DATA(lt_data2).
--ENDIF.

