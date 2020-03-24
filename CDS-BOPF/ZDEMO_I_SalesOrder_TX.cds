// A CDS BOPF view - exposed as BOPF BO ZDEMO_I_SALSESORDER_TX
// Persistence structure: ZSDEMO_I_SALSESORDER_TX_D
// Combined Structure: ZSDEMO_I_SALSESORDER_TX
// Combines Table Type: ZTDEMO_I_SALSESORDER_TX
// Contants Interface: ZIF_DEMO_I_SALSESORDER_TX_C
@AbapCatalog.sqlViewName: 'ZDEMO_I_SO'
@AbapCatalog.compiler.compareFilter: true
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Sales Order for transactional app'
				
@ObjectModel.semanticKey: 'SalesOrder'
				
@ObjectModel.modelCategory: #BUSINESS_OBJECT 
@ObjectModel.compositionRoot: true  
@ObjectModel.transactionalProcessingEnabled: true  
@ObjectModel.writeActivePersistence: 'ZTAB_SO'
				
@ObjectModel.createEnabled: true
@ObjectModel.deleteEnabled: true 
@ObjectModel.updateEnabled: true				
				
define view ZDEMO_I_SalesOrder_TX 				
	as select from ZTAB_SO as SalesOrder  // the sales order table is the data source for this view				
	association [0..1] to SEPM_I_BusinessPartner            as _BusinessPartner on $projection.BusinessPartner = _BusinessPartner.BusinessPartner				
	association [0..1] to SEPM_I_Currency                   as _Currency        on $projection.CurrencyCode     = _Currency.Currency				
	association [0..1] to SEPM_I_SalesOrderBillingStatus    as _BillingStatus   on $projection.BillingStatus    = _BillingStatus.SalesOrderBillingStatus				
	association [0..1] to Sepm_I_SalesOrdOverallStatus      as _OverallStatus   on $projection.OverallStatus    = _OverallStatus.SalesOrderOverallStatus    				
{        
	@ObjectModel.mandatory: true
	key SalesOrder.salesorder           as SalesOrder, 
				
	@ObjectModel.foreignKey.association: '_BusinessPartner'
	SalesOrder.businesspartner          as BusinessPartner,       
				
	@ObjectModel.readOnly: false			
	@ObjectModel.foreignKey.association: '_Currency'  
	@Semantics.currencyCode: true
	SalesOrder.currencycode             as CurrencyCode, 
				
	@ObjectModel.readOnly: false			
	@Semantics.amount.currencyCode: 'CurrencyCode'				
	SalesOrder.grossamount              as GrossAmount, 

	@ObjectModel.readOnly: false			
	@Semantics.amount.currencyCode: 'CurrencyCode'
	SalesOrder.netamount                as NetAmount, 
				
	@ObjectModel.readOnly: true			
	@ObjectModel.foreignKey.association: '_BillingStatus'
	SalesOrder.billingstatus            as BillingStatus, 
				
	@ObjectModel.foreignKey.association: '_OverallStatus'
	SalesOrder.overallstatus            as OverallStatus,
								
	/* Associations */ 
	_BusinessPartner,
	_Currency,
	_BillingStatus, 
	_OverallStatus     
}