** Abap http client to call Rest service and get/post json-formatted data   
**
** Purpose: Sample to use abap http/rest client
**
** Author : Simon Li  Mar 2020
**
report zsample_http_restful_call message-id zhttp_message.

perform get.
perform post.

form get.
    " Create a http client 
    DATA lo_http_client     TYPE REF TO if_http_client.
    cl_http_client=>create_by_destination(
        EXPORTING
            destination              = 'THIS_IS_REST_DEST'    " Logical destination defiend sm59
        IMPORTING
            client                   = lo_http_client         " HTTP Client Abstraction
        EXCEPTIONS
            argument_not_found       = 1
            destination_not_found    = 2
            destination_no_authority = 3
            plugin_not_active        = 4
            internal_error           = 5
            OTHERS                   = 6
    ).
    cl_abap_unit_assert=>assert_subrc( exp = 0 msg = 'Failed to create http client' ).
    cl_abap_ubit_assert=>assert_bound( act = lo_http_client msg = 'Failed to bind http client' ).

    " Set HTTP version - no needed, the latest should be HTTP/1.1
    "lo_http_client->request->set_version( if_http_request=>co_protocol_version_1_1 ).

    " Set the route & query string  
    data(lv_url) = '/getshareddata?type=2&device_id=TTDATA::test1'.
    cl_http_utility=>set_request_uri(
        EXPORTING
            request = lo_http_client->request    " HTTP Framework (iHTTP) HTTP Request
            uri     = lv_url                     " URI String (in the Form of /path?query-string)
    ).
 
    " Create REST client instance
    data(lo_rest_client) = new cl_rest_http_client( io_http_client = lo_http_client ).
    cl_abap_ubit_assert=>assert_bound( act = lo_rest_client msg = 'Failed to bind rest client' ).

    " Autorization key needed
    lo_rest_client->if_rest_client~set_request_header(
        EXPORTING
            iv_name  = 'apkey'
            iv_value = 'Beaver:13263r6vdhrvxe6324326436242443'
    ).

    " HTTP GET
    lo_rest_client->if_rest_client~get( ).
  
    " HTTP response  - if_rest_entity 
    data(lo_response) = lo_rest_client->if_rest_client~get_response_entity( ).
    cl_abap_ubit_assert=>assert_bound( act = lo_response msg = 'No response' ).
    
    " HTTP return status   
    DATA(http_status_code) = lo_response->get_header_field( '~status_code' ).
    cl_abap_ubit_assert=>assert_equal( act = http_status_code exp = 200 msg = |Error: { http_status_code }|).
    
    " HTTP JSON return string   
    DATA(json_response) = lo_response->get_string_data( ).
    
    " as per cl_trex_json_deserializer requires
    REPLACE ALL OCCURRENCES OF REGEX '"(\w+)":' IN json_response WITH '$1:'.
    
    " Class to convert the JSON to an ABAP sttructure
    DATA: BEGIN OF ls_in_exp,
            app_user_id           TYPE string, " app user id in Web
            idtype                TYPE string, " Id type overriden, default to PREMID
            publication           TYPE string, " Publication from Web
            edition               TYPE string, " Edition from Web
            filter                TYPE string, " Filter for the query
          END OF ls_in_exp.

    DATA(lo_deserializer) = new cl_trex_json_deserializer( ).
    lo_deserializer->deserialize( EXPORTING json = json_response IMPORTING abap = ls_in_exp ).

    write: / ls_in_exp.
endform. 

form post.
    data: begin of ls_out_act,
             device_id type string default 'TTDATA:test2',
             category  type string default 'Medical',
          end of ls_out_act.

    DATA(lo_serializer) = new cl_trex_json_serialize( data = ls_out_act ).
    lo_serializer->serialize( ).
    data(pay_load) = lo_serializer->get_data( ).
    FREE lo_serializer.

    DATA lo_http_client TYPE REF TO if_http_client.
    cl_http_client=>create_by_destination(
        EXPORTING
            destination              = 'THIS_IS_REST_DEST'    " Logical destination defined in tcode sm59
        IMPORTING
            client                   = lo_http_client    " HTTP Client Abstraction
        EXCEPTIONS
            argument_not_found       = 1
            destination_not_found    = 2
            destination_no_authority = 3
            plugin_not_active        = 4
            internal_error           = 5
            OTHERS                   = 6
    ).
    cl_abap_unit_assert=>assert_subrc( exp = 0 msg = 'Failed to create http client' ).
    cl_abap_ubit_assert=>assert_bound( act = lo_http_client msg = 'Failed to bind http client' ).

    " Set HTTP version
    "lo_http_client->request->set_version( if_http_request=>co_protocol_version_1_1 ).

    data(lv_route) = '/register'.
    cl_http_utility=>set_request_uri(
        EXPORTING
            request = lo_http_client->request    " HTTP Framework (iHTTP) HTTP Request
            uri     = lv_route                   " URI String (in the Form of /path?query-string)
    ).

    " Create REST client instance
    data lo_rest_client  = new cl_rest_http_client( io_http_client = lo_http_client ).
    cl_abap_ubit_assert=>assert_bound( act = lo_rest_client msg = 'Failed to bind rest client' ).

    " Autorization key needed
    lo_rest_client->if_rest_client~set_request_header(
        EXPORTING
            iv_name  = 'apkey'
            iv_value = 'Beaver:13263r6vdhrvxe6324326436242443'
    ).

    " Set Payload or body (JSON)
    data(lo_request) = lo_rest_client->if_rest_client~create_request_entity( ).
    lo_request->set_content_type( iv_media_type = if_rest_media_type=>gc_appl_json ).  " contentType: "application/json"
    lo_request->set_string_data( pay_load ).
    
    " POST
    lo_rest_client->if_rest_resource~post( lo_request ).

    " Collect response
     data(lo_response) = lo_rest_client->if_rest_client~get_response_entity( ).
     cl_abap_ubit_assert=>assert_bound( act = lo_response ).
    
     " HTTP return status   
     DATA(http_status_code) = lo_response->get_header_field( '~status_code' ).
     cl_abap_ubit_assert=>assert_number_between( lower = 200 upper = 299 act = http_status_code msg = |Error: { http_status_code }|).
    
     data(content_length) = lo_response->get_header_field( 'content-length' ).
     data(location) = lo_response->get_header_field( 'location' ).
     data(content_type) = lo_response->get_header_field( 'content-type' ).
     
     " HTTP JSON return string   
     DATA(json_response) = lo_response->get_string_data( ).
    
     " {"sap_bp": "test1", "idtype": 2} to {sap_bp: "test1", idtype: 2} as cl_trex_json_deserializer requires
     REPLACE ALL OCCURRENCES OF REGEX '"(\w+)":' IN json_response WITH '$1:'.
     
     " Class to convert the JSON to an ABAP sttructure
    DATA: BEGIN OF ls_in_exp,
            result TYPE string, 
            message type string,
          END OF ls_in_exp.

    DATA(lo_deserializer) = new cl_trex_json_deserializer( ).
    lo_deserializer->deserialize( EXPORTING json = json_response IMPORTING abap = ls_in_exp ).

    write: / ls_in_exp-result, ls_in_exp-message.
endform. 