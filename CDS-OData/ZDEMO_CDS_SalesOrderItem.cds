@AbapCatalog.sqlViewName: 'ZDEMO_SOI'
@AbapCatalog.compiler.compareFilter: true
@AccessControl.authorizationCheck: #CHECK
@EndUserText.label: 'List Reporting for Sales Order Item'
// OData Exposure
// Prerequisites
// The following rules need to be fulfilled for a CDS view:
//
// - The CDS view is defined and the source code of the corresponding DDL source is syntactically 
//   correct.
// - At least one KEY element is defined in the SELECT list of the CDS view. Otherwise, the 
//   OData service generation will fail due to missing KEY elements in the CDS view
// - The name of the CDS view in question does not exceed the maximum length of 26 characters.
@OData.publish: true
define view ZDEMO_CDS_SalesOrderItem 
    as select from SEPM_I_SalesOrderItem_E as Item 
{
	key Item.SalesOrder                             as SalesOrderID, 
	key Item.SalesOrderItem                         as ItemPosition, 
	    Item._SalesOrder._Customer.CompanyName  	as CompanyName,
	    Item.Product                            	as Product, 
	
        @Semantics.currencyCode: true
	    Item.TransactionCurrency                	as CurrencyCode,
	
        @Semantics.amount.currencyCode: 'CurrencyCode'
	    Item.GrossAmountInTransacCurrency       	as GrossAmount, 
	    @Semantics.amount.currencyCode: 'CurrencyCode'
	    Item.NetAmountInTransactionCurrency     	as NetAmount, 
	    @Semantics.amount.currencyCode: 'CurrencyCode'
	    Item.TaxAmountInTransactionCurrency     	as TaxAmount,
	    Item.ProductAvailabilityStatus          	as ProductAvailabilityStatus
}   		
// Results of activation
//
// ABAP Development Tools delegates the activation request to the SADL (Service Adaptation Description
// Language) framework. SADL, in turn, generates several Gateway artifacts that are stored in 
// the back end of the application server AS ABAP and are required for OData service activation
// in the SAP Gateway hub later on:
//
// The actual service artifact with the technical name <CDS_VIEW>_CDS. You can find it as SAP 
// Gateway Business Suite Enablement - Service object (object type: R3TR IWSV)
// An SAP Gateway model (object type: R3TR IWMO) with the name <CDS_VIEW>_CDS
// An ABAP class CL_<CDS_VIEW> that is used to provide model metadata to the SAP Gateway 
// service.