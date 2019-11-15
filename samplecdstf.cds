@EndUserText.label: 'Get Ship-to Address'
define table function ztf_get_shipto_addresses
with parameters 
@Environment.systemField: #CLIENT
  p_client: abap.clnt,
@Environment.systemField: #SYSTEM_DATE
  p_keydate: abap.date
returns {
   key client: s_mandt;
   key SAP_BP_ID: gpnr;
       ADDRESS_NO: adrnr;
       STREET_NAME: stras;
       SAP_HOUSE_NUMBER: hausn;
       SAP_HOUSE_NO_AFFIX: hsnmr2;
       SAP_STREET1: street2;
       SAP_STREET2: ispadrbsnd;
       SAP_SUITE_NUMBER: addrsecnumber;
       SAP_AD_CREATED_ON: erfdate;
       SAP_AD_CREATED_AT: erftime;
       SAP_AD_CREATED_BY: erfuser;
       SAP_AD_CHANGED_ON: aendate;
       SAP_AD_CHANGED_AT: aentime; 
       SAP_AD_CHANGED_BY: aenuser;
       SAP_SEC_ADD_TEXT: addrsecabbrev;
       SAP_SEC_ADD_NO: addrsecnumber;
       SAP_BUILDING_IDENTITY: stock; 
       SAP_CITY: ort01;
       SAP_DISTRICT: ort02;
       SAP_PROVINCE_CODE: regio;
       SAP_POSTAL_CODE: pstlz;
       SAP_GEO_FSA_MARKET: zmsd_fsa;
       SAP_COUNTRY_CODE: land1;
       SAP_PHONE_NUM: ani_telnrp;
       SAP_CELL_NUM: isphandy;
       SAP_EMAIL_ADDRESS: ispemail;
       SAP_FAX_NUMBER: telfx;
       SAP_PO_BOX: pfach;
       SAP_PO_BOX_POSTAL_CODE: pstl2;
       SAP_PO_BOX_CITY: ortpf;
       SAP_TAX_JUR: txjcd;
       SAP_PREDIRECTION: predirectional;
       SAP_POSTDIRECTION: postdirectional;
       SAP_ADDR_AFFIX: adrzus2;
       ROLE: jparvw;
       VALID_FROM: wdat1;
       VALID_TO: wdat2;
}
implemented by method sampleamdpclass=>get_order_shipto_addresses;