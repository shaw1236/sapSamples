report testjosn1.

form test1 using value(request) type string 
                 value(response) type string.   
  DATA: BEGIN OF ls_input,
          app_user_id           TYPE string, " app user id in Web
          idtype                TYPE string, " Id type overriden, default to PREMID
          publication           TYPE string, " Publication from Web
          edition               TYPE string, " Edition from Web
          subscribed_service_id TYPE string, " Susbribed service id in Web
          sap_bp                TYPE string, " sap partner stored in Web
          sap_order             TYPE string, " sap order stored in Web
          filter                TYPE string, " Filter for the query
        END OF ls_input.

  response = '[]'.  " Default to an empty array
  
  "DATA lo_deserializer TYPE REF TO cl_trex_json_deserializer .
  "CREATE OBJECT lo_deserializer.
  DATA(lo_deserializer) = new cl_trex_json_deserializer( ).
  lo_deserializer->deserialize(
          EXPORTING json = request
          IMPORTING abap = ls_input ).
  "...
  DATA: BEGIN OF lt_output OCCURS 30,
          sap_bp                TYPE jkak-gpag,
          sap_order             TYPE jkak-vbeln,
          sap_posex             TYPE jkap-posex,
          publication           TYPE jkap-drerz,
          edition               TYPE jkap-pva,
          app_user_id           TYPE but0id-idnumber,
          subscribed_service_id TYPE jkap-refbeleg,
          token                 TYPE zcc_token_base-token,
          token_original        TYPE zcc_token_base-token_returned,
          card_type             TYPE zcc_token_base-ccins,
          card_number           TYPE zcc_token_base-bs_ccnum,
          expiry_date           TYPE jkapcc-datbi,
          trailing_number       TYPE zcc_token_base-cc_trailing_num,
        END OF lt_output.
  "... ls_input => lt_output[]

  LOOP AT lt_output.
    DATA lo_serializer TYPE REF TO cl_trex_json_serializer.
    CREATE OBJECT lo_serializer
      EXPORTING
        data = lt_output.
    lo_serializer->serialize( ).
    lt_res_line = lo_serializer->get_data( ).
    FREE lo_serializer.

    " sap_posex: "10" -> sap_posex: 10
    REPLACE FIRST OCCURRENCE OF REGEX 'posex:\s*"(\w+)"' IN lt_res_line WITH 'posex: $1'.
    " Example: key: "value" -> "key": "value"
    REPLACE ALL OCCURRENCES OF REGEX '(\w+):' IN lt_res_line WITH '"$1":'.
    APPEND lt_res_line.
  ENDLOOP.

* Assemble the table into an array
  CONCATENATE LINES OF lt_res_line INTO response SEPARATED BY ', '.
  CONCATENATE '[' response ']' INTO response.
ENDFORM.  