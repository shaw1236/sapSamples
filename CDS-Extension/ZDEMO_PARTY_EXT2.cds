@AbapCatalog.sqlViewAppendName: 'ZSQL_PARTY_EXT2'
@EndUserText.label: 'Party Address Data Extended with Contact'

extend view DEMO_PARTY_ORIG with ZDEMO_PARTY_EXT2
	association [1..1] to SEPM_I_ContactPerson_E as _PartyContact 
	on Organization.businesspartner = _PartyContact.businesspartner
{
	@UI.lineItem: { label: 'Contact - Email', position: 50 }
	_PartyContact._BusinessPartner.EmailAddress
}