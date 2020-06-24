** Abap http client to get/post xml-formatted data   
**
** Purpose: Sample to use abap http client
**
** Author : Simon Li  Mar 2020
**
* https://www.sapdatasheet.org/abap/clas/cl_abap_unit_assert.html
report zsample_http_call message-id zhttp_message.

constants c_merchantId  type char12 default '123456789'.              " dummy
constants c_passcode    type string default 'Eghdsahiu523171gdjaxd'.  " dummy
constants c_destination type char80 default 'THIS_IS_API_DEST'.       " dummy

perform go using c_merchtid c_passcode c_destination.

form go using value(i_merchantId) type c
              value(i_passcode)   type c
              value(i_dest)       type c.
    " Create HTTP client as per the destination
    DATA lo_client TYPE REF TO if_http_client.
    CALL METHOD cl_http_client=>create_by_destination
        EXPORTING
            destination = i_dest
        IMPORTING
            client      = lo_client
        EXCEPTIONS
            argument_not_found = 1
            destination_not_found = 2
            destination_no_authority = 3
            plugin_not_active = 4
            internal_error = 5
            OTHERS = 6.
    cl_abap_unit_assert=>assert_subrc( exp = 0 msg = 'Failed to create http client' ).
    cl_abap_ubit_assert=>assert_bound( act = lo_client msg = 'Failed to bind http client' ).

    " https://www.sapdatasheet.org/abap/clas/if_http_client.html
    " Method: GET/POST
    lo_client->request->set_method( if_http_request=>co_request_method_get ).  " this is defaultm can be omitted 
    "POST: if_http_request=>co_request_method_post

    " Construct the URL without https:// ......(remaining details are maintained in SM59)
    data(lv_ccnum) = '1111111111111111'.        " dummy card
    data(lv_datbi) = '20201231'.            
    DATA(lv_query_string) = |?BACKEND=1| &&
                            |&merchId={ i_merchantId }| && 
                            |&passcode={ i_passcode }|  && 
                            |&card_number={ lv_ccnum }| &&
                            |&exp_month= { lv_datbi+4(2) }| &&
                            |&exp_year={ lv_datbi(4) }|.

    " Set URL to request
    cl_http_utility=>set_request_uri( request = lo_client->request uri = lv_query_string ).

    " Send the HTTP request
    CALL METHOD lo_client->send
        EXCEPTIONS
            http_communication_failure = 1
            http_invalid_state = 2
            http_processing_failed = 3
            http_invalid_timeout = 4
            OTHERS = 5.
    cl_abap_unit_assert=>assert_subrc( exp = 0 msg = 'Failed to send' ).

    " HTTP call receive
    CALL METHOD lo_client->receive
        EXCEPTIONS
            http_communication_failure = 1
            http_invalid_state = 2
            http_processing_failed = 3
            OTHERS = 4.
    cl_abap_unit_assert=>assert_subrc( exp = 0  msg = 'Failed to receive' ).

    " get status of the response
    CALL METHOD lo_client->response->get_status
        IMPORTING
            code   = data(lv_statuscode)
            reason = data(lv_errorreason).
    cl_abap_usit_assert=>assert_number_between( lower = 200 upper = 299 act = lv_statuscode 
                                                msg = |({ lv_statuscode }, { lv_errorreason })| ).

    " Get Response data
    DATA lv_bin TYPE xstring.
    lv_bin = lo_client->response->get_data( ).  
    CALL METHOD lo_client->close
        EXCEPTIONS
            http_invalid_state = 1
            OTHERS             = 2.
    cl_abap_unit_assert=>assert_subrc( exp = 0 msg = 'Failed to close the session' ).

    " POST
    "lo_client->request->set_method( if_http_request=>co_request_method_post ).  
    "cl_http_utility=>set_request_uri( request = lo_client->request ).
    "DATA(lv_xml_str) = '<data><id>dummy</id></data>'.
    "CALL FUNCTION 'SCMS_STRING_TO_XSTRING'
    "    EXPORTING
    "        text   = lv_xml_str " variable type string
    "    IMPORTING
    "        buffer = lv_bin.    " variable type xstring
    "lo_client->response->set_data( lv_bin ).
    "lo_client->send( ).
    "lo_client->receive(  ).
    "lo_client->response->get_status(  ).
    "lo_client->close(  ).

    "
    " Display data
    "
    " Convert the binary to XML
    " Structure srt_xml_data: 
    "   TAG_NAME		STRING	
    "   TAG_VALUE		STRING	
    "   TAG_LEVEL		INT4	
    "   TAG_TYPE		CHAR
    DATA lt_xml_data TYPE srt_xml_data_tab.
    CALL FUNCTION 'SRTUTIL_CONVERT_XML_TO_TABLE'
        EXPORTING
            xdoc = lv_bin
        IMPORTING
            data = lt_xml_data.

    DATA lo_salv_table TYPE REF TO cl_salv_table.
    TRY.
        cl_salv_table=>factory( IMPORTING r_salv_table = lo_salv_table CHANGING t_table = lt_xml_data ).

        "...
        lo_salv_table->display( ).
    
        CATCH cx_root INTO data(lo_cx_root).                  
            data(lv_message) = lo_cx_root->get_text( ).
            MESSAGE lv_message TYPE 'E'.
    ENDTRY.
endform.

"SM59 Settings:
"url: https://hostname.com/payment?BACKEND=1&merchantId=xxxx&passcode=blahblah&card_number=ccnum&exp_month=10&exp_year=2020
"1. Connection Type: G
"2. Target host: hostname.com
"3. Path prefix: /payment
"5. SSL certificate: DFAULT SSL Client(standard)

"SMICM Settings:
" Make sure HTTPS port(443) is activated