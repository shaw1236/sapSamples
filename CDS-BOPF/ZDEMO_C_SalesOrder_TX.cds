// A consumption view - exposed as an OData Service 
// Technical name: R3TR IWSV ZDEMO_C_SSALESORDER_TX_CDS
// SAP Gateway model: R3TR IWMO ZDEMO_C_SSALESORDER_TX_CDS
// ABAP class R3TR CLAS ZCL_ZDEMO_C_SSALESORDER_TX
@AbapCatalog.sqlViewName: 'ZDEMO_C_SO'
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Sales Order for transactional app'
				
//Repeated annotations from BO view – VIEW scope
@ObjectModel.semanticKey: 'SalesOrder'
				
@ObjectModel.transactionalProcessingDelegated: true

// Repeated annotations from BO view – VIEW scope	
// CRUD
// Go button for (R)ead	
@ObjectModel.createEnabled: true  // (C)reate button
@ObjectModel.deleteEnabled: true  // (D)elete button
@ObjectModel.updateEnabled: true  // (U)pdate by picking up a row/Edit button

// Header Info
// Sales Order(3)				
@UI.headerInfo: { typeName: 'Sales Order', typeNamePlural: 'Sales Orders' }
				
@OData.publish: true
				
define view ZDEMO_C_SalesOrder_TX				
	as select from ZDEMO_I_SalesOrder_TX as Document
{			
	@UI.lineItem.position: 10
	@UI.lineItem: 
	[
	   { type: #FOR_ACTION, position: 1, dataAction: 'BOPF:SET_TO_PAID', label: 'Set to Paid' }
	]			
	//@UI.identification.position: 10
	key Document.SalesOrder,	
				
	@UI.lineItem.position: 20			
	@UI.identification.position: 20
	Document.BusinessPartner,
				
	Document.CurrencyCode,
				
	@UI.lineItem.position: 50		
	@UI.identification.position: 50
	@ObjectModel.readOnly: true    //-- restricts to non-editable field
	Document.GrossAmount,
				
	@UI.lineItem.position: 60			
	@UI.identification.position: 60
	@ObjectModel.readOnly: true    //-- restricts to non-editable field
	Document.NetAmount,
				
	@UI.lineItem.position: 30
	@UI.selectionField.position: 30
	@UI.identification.position: 30
	Document.BillingStatus,
				
	@UI.lineItem.position: 40
	@UI.selectionField.position: 40
	@UI.identification.position: 40
	Document.OverallStatus,
				
	/* Exposing value via associations */ 
	@UI.lineItem:  { value: '.CompanyName', position: 15 }
	Document._BusinessPartner,
				
	Document._Currency,   
	Document._BillingStatus, 
	Document._OverallStatus   
}                        							