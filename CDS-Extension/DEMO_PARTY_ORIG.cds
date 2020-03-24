@AbapCatalog.sqlViewName: 'SQL_PARTY_ORIG'
@AbapCatalog.compiler.compareFilter: true
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Party Address Data'
					
@Search.searchable: true
					
@OData.publish: true
					
define view DEMO_PARTY_ORIG
	as select from SEPM_I_Party_E as Organization
{
	@UI.lineItem: { importance: #HIGH, label: 'Org ID', position: 10 }
	key   Organization.Party                as OrgID,
					
	@Search.defaultSearchElement: true
	@UI.lineItem: { importance: #HIGH, label: 'Organization', position: 20 }
	Organization.PartyName            as OrgName,
					
	@UI.selectionField.position: 10
	@Search.defaultSearchElement: true
	@Search.fuzzinessThreshold: 0.5     
					
	@UI.lineItem: { label: 'Location', position: 30 }
	Organization.CityName             as City
}				