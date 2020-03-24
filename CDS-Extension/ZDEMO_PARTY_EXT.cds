@AbapCatalog.sqlViewAppendName: 'ZSQL_PARTY_EXT'
@EndUserText.label: 'Party Adress Data Extended'

extend view DEMO_PARTY_ORIG with ZDEMO_PARTY_EXT
{
    // Adding predefined field
    @UI.lineItem: { label: 'Country', position: 35 }
  	@Search.defaultSearchElement: true
  	Organization.CountryName,

  	// Adding a calculated field
  	@UI.lineItem: { label: 'Role', position: 25}
  	@Search.defaultSearchElement: true
  	case Organization.PartyRole
    	when '01' then 'Customer'
    	when '02' then 'Supplier'
    	else 'Unspecified'
  	end as party_role
}						