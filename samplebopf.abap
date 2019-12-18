** BOPF Media Customer Action CRUD: Create/Change/Display 
**
** Purpose: Sample to use BOPF Object (MAM_CUSTOMER)
**
** Author : Simon Li  Jul 2017
**
CLASS lcl_bopf_common DEFINITION CREATE PUBLIC.
  PROTECTED SECTION.
    DATA mo_txn_mngr TYPE REF TO /bobf/if_tra_transaction_mgr.
    DATA mo_svc_mngr TYPE REF TO /bobf/if_tra_service_manager.
    DATA mo_bo_conf  TYPE REF TO /bobf/if_frw_configuration.
 
    METHODS:
      constructor RAISING /bobf/cx_frw.
ENDCLASS.

CLASS lcl_bopf_common IMPLEMENTATION.
  METHOD constructor.
    "Obtain a reference to the BOPF transaction manager:
    me->mo_txn_mngr =
      /bobf/cl_tra_trans_mgr_factory=>get_transaction_manager( ).

    "Obtain a reference to the BOPF service manager:
    me->mo_svc_mngr =
      /bobf/cl_tra_serv_mgr_factory=>get_service_manager(
        /bobf/if_demo_customer_c=>sc_bo_key ).

    "Access the metadata for the /BOBF/MAM_CUSTOMER BO:
    me->mo_bo_conf =
      /bobf/cl_frw_factory=>get_configuration(
        /bobf/if_demo_customer_c=>sc_bo_key ).
  ENDMETHOD.                 " METHOD constructor
ENDCLASS.

CLASS lcl_demo DEFINITION inheriting from lcl_bopf_common FINAL CREATE PRIVATE.
  PUBLIC SECTION.
    CLASS-METHODS:
      create_customer IMPORTING iv_customer_id TYPE /bobf/demo_customer_id,
      display_customer IMPORTING iv_customer_id TYPE /bobf/demo_customer_id,
      change_customer IMPORTING iv_customer_id TYPE /bobf/demo_customer_id.
  
  PRIVATE SECTION.
    METHODS:
      constructor RAISING /bobf/cx_frw,
      display_messages IMPORTING io_message
                                   TYPE REF TO /bobf/if_frw_message,

      get_customer_for_id IMPORTING iv_customer_id
                                      TYPE /bobf/demo_customer_id
                          RETURNING VALUE(rv_customer_key)
                                      TYPE /bobf/conf_key
                          RAISING /bobf/cx_frw,

      get_node_table IMPORTING iv_key TYPE /bobf/conf_key
                               iv_node_key TYPE /bobf/obm_node_key
                               iv_edit_mode TYPE /bobf/conf_edit_mode 
                                 DEFAULT /bobf/if_conf_c=>sc_edit_read_only
                     RETURNING VALUE(rr_data) TYPE REF TO data
                     RAISING /bobf/cx_frw,
                    
      get_node_row IMPORTING iv_key TYPE /bobf/conf_key
                             iv_node_key TYPE /bobf/obm_node_key
                             iv_edit_mode TYPE /bobf/conf_edit_mode 
                               DEFAULT /bobf/if_conf_c=>sc_edit_read_only
                             iv_index TYPE i DEFAULT 1
                    RETURNING VALUE(rr_data) TYPE REF TO data
                    RAISING /bobf/cx_frw,
                    
      get_node_table_by_assoc IMPORTING iv_key TYPE /bobf/conf_key
                                        iv_node_key TYPE /bobf/obm_node_key
                                        iv_assoc_key TYPE /bobf/obm_assoc_key
                                        iv_edit_mode TYPE /bobf/conf_edit_mode 
                                          DEFAULT /bobf/if_conf_c=>sc_edit_read_only
                              RETURNING VALUE(rr_data) TYPE REF TO data
                              RAISING /bobf/cx_frw,
                    
      get_node_row_by_assoc IMPORTING iv_key TYPE /bobf/conf_key
                                      iv_node_key TYPE /bobf/obm_node_key
                                      iv_assoc_key TYPE /bobf/obm_assoc_key
                                      iv_edit_mode TYPE /bobf/conf_edit_mode 
                                        DEFAULT /bobf/if_conf_c=>sc_edit_read_only
                                      iv_index TYPE i DEFAULT 1
                            RETURNING VALUE(rr_data) TYPE REF TO data
                            RAISING /bobf/cx_frw.                      
ENDCLASS.

" The key is provided to us via the BO’s constants interface 
" (/BOBF/IF_DEMO_CUSTOMER_C)
CLASS lcl_demo IMPLEMENTATION.
    METHOD constructor.
      super->constructor( ).   " Get all the manager references
    ENDMETHOD.                 " METHOD constructor

    METHOD display_messages.
      "Sanity check:
      CHECK io_message IS BOUND.
    
      "Output each of the messages in the collection:
      DATA lt_messages TYPE /bobf/t_frw_message_k.
      io_message->get_messages( IMPORTING et_message = lt_messages ).
      LOOP AT lt_messages ASSIGNING FIELD-SYMBOLS(<ls_message>).
        DATA(lv_msg_text) = <ls_message>-message->get_text( ).
        WRITE: / lv_msg_text.
      ENDLOOP.
    ENDMETHOD.                 " METHOD display_messages

    METHOD get_customer_for_id.
      "Instantiate the test driver class:
      "DATA lo_driver        TYPE REF TO lcl_demo.
      DATA(lo_driver)  = new lcl_demo( ).       
    
      "Though we could conceivably lookup the customer using an SQL query,
      "the preferred method of selection is a BOPF query:
      DATA(lt_parameters) = VALUE /bobf/t_frw_query_selparam(
         ( attribute_name = 
           /bobf/if_demo_customer_c=>sc_query_attribute-root-select_by_attributes-customer_id
           sign = 'I' option = 'EQ' low = iv_customer_id )
         )
    
      DATA lt_customer_keys TYPE /bobf/t_frw_key.
      CALL METHOD lo_driver->mo_svc_mngr->query
        EXPORTING
          iv_query_key            = 
            /bobf/if_demo_customer_c=>sc_query-root-select_by_attributes
          it_selection_parameters = lt_parameters
        IMPORTING
          et_key                  = lt_customer_keys.
    
        "Return the matching customer's KEY value:
      READ TABLE lt_customer_keys INDEX 1 ASSIGNING field-symbols(<customer_key>).
      IF sy-subrc EQ 0.
        rv_customer_key = <customer_key>-key.
      ENDIF.
    ENDMETHOD.                 " METHOD get_customer_for_id

    METHOD get_node_table.
 
      "Lookup the node's configuration:
      DATA ls_node_conf TYPE /bobf/s_confro_node.
      CALL METHOD mo_bo_conf->get_node
        EXPORTING
          iv_node_key = iv_node_key
        IMPORTING
          es_node     = ls_node_conf.
    
      "Use the node configuration metadata to create the result table:
      CREATE DATA rr_data TYPE (ls_node_conf-data_table_type).
      FIELD-SYMBOLS <lt_data> TYPE INDEX TABLE.
      ASSIGN rr_data->* TO <lt_data>.

      "Retrieve the target node:
      DATA(lt_key) = VALUE /bobf/t_frw_key( ( key = iv_key ) ).

      DATA lo_change    TYPE REF TO /bobf/if_tra_change.
      DATA lo_message   TYPE REF TO /bobf/if_frw_message.
      CALL METHOD me->mo_svc_mngr->retrieve
        EXPORTING
          iv_node_key = iv_node_key
          it_key      = lt_key
        IMPORTING
          eo_message  = lo_message
          eo_change   = lo_change
          et_data     = <lt_data>.
    
        "Check the results:
      IF lo_message IS BOUND AND lo_message->check( ) EQ abap_true.
        me->display_messages( lo_message ).
        RAISE EXCEPTION TYPE /bobf/cx_dac.
      ENDIF.
    ENDMETHOD.                 " METHOD get_node_table
    
    METHOD get_node_row.
    
      "Lookup the node data:
      DATA lr_t_data TYPE REF TO data.
      lr_t_data =
          get_node_table( iv_key       = iv_key
                          iv_node_key  = iv_node_key
                          iv_edit_mode = iv_edit_mode ).
    
      IF lr_t_data IS NOT BOUND.
        RAISE EXCEPTION TYPE /bobf/cx_dac.
      ENDIF.
    
      "Try to pull the record at the specified index:
      FIELD-SYMBOLS <lt_data> TYPE INDEX TABLE.
      ASSIGN lr_t_data->* TO <lt_data>.
      
      FIELD-SYMBOLS <row> TYPE ANY.
      READ TABLE <lt_data> INDEX iv_index ASSIGNING <row>.
      IF sy-subrc EQ 0.
        rr_data = REF #( <row> ).
      ELSE.
        RAISE EXCEPTION TYPE /bobf/cx_dac.
      ENDIF.
    ENDMETHOD.                 " METHOD get_node_row
    
    METHOD get_node_table_by_assoc.
    
      "Lookup the association metadata to find out more
      "information about the target sub-node:
      DATA ls_association TYPE /bobf/s_confro_assoc.
      CALL METHOD me->mo_bo_conf->get_assoc
        EXPORTING
          iv_assoc_key = iv_assoc_key
          iv_node_key  = iv_node_key
        IMPORTING
          es_assoc     = ls_association.
    
      IF ls_association-target_node IS NOT BOUND.
        RAISE EXCEPTION TYPE /bobf/cx_dac.
      ENDIF.
    
      "Use the node configuration metadata to create the result table:
      DATA ls_node_conf TYPE /bobf/s_confro_node.
      ls_node_conf = ls_association-target_node->*.
    
      CREATE DATA rr_data TYPE (ls_node_conf-data_table_type).
      FIELD-SYMBOLS <lt_data> TYPE INDEX TABLE.
      ASSIGN rr_data->* TO <lt_data>.
    
      "Retrieve the target node:
      DATA(lt_key) = VALUE /bobf/t_frw_key( ( key = iv_key ) ).
    
      DATA lo_change  TYPE REF TO /bobf/if_tra_change.
      DATA lo_message TYPE REF TO /bobf/if_frw_message.
      CALL METHOD mo_svc_mngr->retrieve_by_association
        EXPORTING
          iv_node_key    = iv_node_key
          it_key         = lt_key
          iv_association = iv_assoc_key
          iv_fill_data   = abap_true
        IMPORTING
          eo_message     = lo_message
          eo_change      = lo_change
          et_data        = <lt_data>.
    
        "Check the results:
      IF lo_message IS BOUND AND lo_message->check( ) EQ abap_true.
        display_messages( lo_message ).
        RAISE EXCEPTION TYPE /bobf/cx_dac.
      ENDIF.
    ENDMETHOD.                 " METHOD get_node_table_by_assoc
    
    METHOD get_node_row_by_assoc.
    
      "Lookup the node data:
      DATA lr_t_data TYPE REF TO data.
      lr_t_data =
          get_node_table_by_assoc( iv_key       = iv_key
                                   iv_node_key  = iv_node_key
                                   iv_assoc_key = iv_assoc_key
                                   iv_edit_mode = iv_edit_mode ).
    
      IF lr_t_data IS NOT BOUND.
        RAISE EXCEPTION TYPE /bobf/cx_dac.
      ENDIF.
    
      "Try to pull the record at the specified index:
      FIELD-SYMBOLS <lt_data> TYPE INDEX TABLE.
      ASSIGN lr_t_data->* TO <lt_data>.
      
      FIELD-SYMBOLS <row> TYPE ANY.
      READ TABLE <lt_data> INDEX iv_index ASSIGNING <row>.
      IF sy-subrc EQ 0.
        rr_data = REF #( <row> ).
      ELSE.
        RAISE EXCEPTION TYPE /bobf/cx_dac.
      ENDIF.
    ENDMETHOD.                 " METHOD get_node_row_by_assoc
    
    METHOD create_customer.
        "Method-Local Data Declarations:
        "Use the BOPF API to create a new customer record:
        TRY.
          "Instantiate the driver class:
          DATA(lo_driver) = new lcl_demo( ).
    
          "Build the 'ROOT' node:
          DATA(lr_s_root) = new /bobf/s_demo_customer_hdr_k( ).
          lr_s_root->key = /bobf/cl_frw_factory=>get_new_key( ).
          lr_s_root->customer_id    = iv_customer_id.
          lr_s_root->sales_org      = 'SALE'.
          lr_s_root->cust_curr      = 'CAD'.
          lr_s_root->address_contry = 'CA'.
          lr_s_root->address        = '1234 Bloor Street'.

          DATA lt_mod TYPE /bobf/t_frw_modification.
          FIELD-SYMBOLS <mod> LIKE LINE OF lt_mod.    
          APPEND INITIAL LINE TO lt_mod ASSIGNING <mod>.
          <mod>-node        = /bobf/if_demo_customer_c=>sc_node-root.
          <mod>-change_mode = /bobf/if_frw_c=>sc_modify_create.
          <mod>-key         = lr_s_root->key.
          <Mod>-data        = lr_s_root.
    
          "Build the 'ROOT_TEXT' node:
          DATA lr_s_txt TYPE REF TO /bobf/s_demo_short_text_k.
          CREATE DATA lr_s_txt.
          lr_s_txt->key      = /bobf/cl_frw_factory=>get_new_key( ).
          lr_s_txt->text     = 'Sample Customer Record'.
          lr_s_txt->language = sy-langu.
    
          APPEND INITIAL LINE TO lt_mod ASSIGNING <mod>.
          <mod>-node        = /bobf/if_demo_customer_c=>sc_node-root_text.
          <mod>-change_mode = /bobf/if_frw_c=>sc_modify_create.
          <mod>-source_node = /bobf/if_demo_customer_c=>sc_node-root.
          <mod>-association = /bobf/if_demo_customer_c=>sc_association-root-root_text.
          <mod>-source_key  = lr_s_root->key.
          <mod>-key         = lr_s_txt->key.
          <mod>-data        = lr_s_txt.
    
          "Build the 'ROOT_LONG_TEXT' node:
          "If you look at the node type for this node, you'll notice that
          "it's a "Delegated Node". In other words, it is defined in terms
          "of the /BOBF/DEMO_TEXT_COLLECTION business object. The following
          "code accounts for this indirection.
          DATA lr_s_txt_hdr TYPE REF TO /bobf/s_demo_longtext_hdr_k.
          CREATE DATA lr_s_txt_hdr.
          lr_s_txt_hdr->key = /bobf/cl_frw_factory=>get_new_key( ).
    
          APPEND INITIAL LINE TO lt_mod ASSIGNING <mod>.
          <mod>-node            = /bobf/if_demo_customer_c=>sc_node-root_long_text.
          <mod>-change_mode     = /bobf/if_frw_c=>sc_modify_create.
          <mod>-source_node     = /bobf/if_demo_customer_c=>sc_node-root.
          <mod>-association     = 
            /bobf/if_demo_customer_c=>sc_association-root-root_long_text.
          <mod>-source_key      = lr_s_root->key.
          <mod>-key             = lr_s_txt_hdr->key.
          <mod>-data            = lr_s_txt_hdr.
    
          "Create the 'CONTENT' node:
          DATA lr_s_txt_cont TYPE REF TO /bobf/s_demo_longtext_item_k.
          CREATE DATA lr_s_txt_cont.
          lr_s_txt_cont->key          = /bobf/cl_frw_factory=>get_new_key( ).
          lr_s_txt_cont->language     = sy-langu.
          lr_s_txt_cont->text_type    = 'MEMO'.
          lr_s_txt_cont->text_content = 'Demo customer created via BOPF API.'.
    
          APPEND INITIAL LINE TO lt_mod ASSIGNING <mod>.
          <mod>-node        = 
            lo_driver->mo_bo_conf->query_node( 
              iv_proxy_node_name = 'ROOT_LONG_TXT.CONTENT' ).
          <mod>-change_mode = /bobf/if_frw_c=>sc_modify_create.
          <mod>-source_node = /bobf/if_demo_customer_c=>sc_node-root_long_text.
          <mod>-source_key  = lr_s_txt_hdr->key.
          <mod>-key         = lr_s_txt_cont->key.
          <mod>-data        = lr_s_txt_cont.
          <mod>-association =
            lo_driver->mo_bo_conf->query_assoc(
              iv_node_key   = /bobf/if_demo_customer_c=>sc_node-root_long_text
              iv_assoc_name = 'CONTENT' ).
    
          "
          " ==> Action
          "    
          "Create the customer record:
          " Once the modification table is filled out, we can call the MODIFY() 
          " method to insert the record(s). Assuming all is successful, we can 
          " then commit the transaction by calling the SAVE() method on the 
          " /BOBF/IF_TRA_TRANSACTION_MANAGER instance. Should any errors occur, 
          " we can display the error messages using methods of the /BOBF/IF_FRW_MESSAGE 
          " object reference which is returned from both methods. This is evidenced by 
          " the simple utility method DISPLAY_MESSAGES() shown below. That’s pretty much 
          " all there is to it.
          DATA lo_change   TYPE REF TO /bobf/if_tra_change.
          DATA lo_message  TYPE REF TO /bobf/if_frw_message.
          CALL METHOD lo_driver->mo_svc_mngr->modify
            EXPORTING
              it_modification = lt_mod
            IMPORTING
              eo_change       = lo_change
              eo_message      = lo_message.
    
          "Check for errors:
          IF lo_message IS BOUND AND lo_message->check( ) EQ abap_true.
            lo_driver->display_messages( lo_message ).
            RETURN.
          ENDIF.
    
          "Apply the transactional changes:
          DATA lv_rejected TYPE boole_d.
          CALL METHOD lo_driver->mo_txn_mngr->save
            IMPORTING
              eo_message  = lo_message
              ev_rejected = lv_rejected.
    
          IF lv_rejected EQ abap_true.
            lo_driver->display_messages( lo_message ).
            RETURN.
          ENDIF.
    
          "If we get to here, then the operation was successful:
          WRITE: / 'Customer', iv_customer_id, 'created successfully.'.

        CATCH /bobf/cx_frw INTO DATA(lx_bopf_ex).
          DATA(lv_err_msg) = lx_bopf_ex->get_text( ).
          WRITE: / lv_err_msg.
        ENDTRY.
      ENDMETHOD.                 " METHOD create_customer
      
      METHOD display_customer.

        "Try to display the selected customer:
        TRY.
          "Instantiate the test driver class:
          DATA(lo_driver) = new lcl_demo( ).
    
          "Lookup the customer's key attribute using a query:
          DATA(lv_customer_key) = lo_driver->get_customer_for_id( iv_customer_id ).
    
          "Display the header-level details for the customer:
          DATA lr_s_root TYPE REF TO /bobf/s_demo_customer_hdr_k.
          lr_s_root ?=
            lo_driver->get_node_row( 
                      iv_key = lv_customer_key
                      iv_node_key = /bobf/if_demo_customer_c=>sc_node-root
                      iv_index = 1 ).
    
          WRITE: / 'Display Customer', lr_s_root->customer_id.
          ULINE.
          WRITE: / 'Sales Organization:', lr_s_root->sales_org.
          WRITE: / 'Address:', lr_s_root->address.
          SKIP.
    
          "Traverse to the ROOT_TEXT node to display the customer short text:
          DATA lr_s_text TYPE REF TO /bobf/s_demo_short_text_k.
          lr_s_text ?=
            lo_driver->get_node_row_by_assoc( 
    
              iv_key = lv_customer_key
              iv_node_key = /bobf/if_demo_customer_c=>sc_node-root
              iv_assoc_key = /bobf/if_demo_customer_c=>sc_association-root-root_text
              iv_index = 1 ).
          WRITE: / 'Short Text:', lr_s_text->text.
        CATCH /bobf/cx_frw INTO data(lx_bopf_ex).
          data(lv_err_msg) = lx_bopf_ex->get_text( ).
          WRITE: / lv_err_msg.
        ENDTRY.
      ENDMETHOD.                 " METHOD display_customer

      METHOD change_customer.
    
        "Try to change the address on the selected customer:
        TRY.
          "Instantiate the test driver class:
          data(lo_driver) = new lcl_demo( ).
    
          "Access the customer ROOT node:
          DATA lv_customer_key TYPE /bobf/conf_key.
          lv_customer_key = lo_driver->get_customer_for_id( iv_customer_id ).
    
          DATA lr_s_root TYPE REF TO /bobf/s_demo_customer_hdr_k.
          lr_s_root ?=
            lo_driver->get_node_row( iv_key = lv_customer_key
                                     iv_node_key = /bobf/if_demo_customer_c=>sc_node-root
                                     iv_edit_mode = /bobf/if_conf_c=>sc_edit_exclusive
                                     iv_index = 1 ).
    
          "Change the address string on the customer:
          lr_s_root->address = '1234 Davenport Ave.'.
    
          DATA lt_mod  TYPE /bobf/t_frw_modification.
          APPEND INITIAL LINE TO lt_mod ASSIGNING field-symbols(<mod>).
          <mod>-node        = /bobf/if_demo_customer_c=>sc_node-root.
          <mod>-change_mode = /bobf/if_frw_c=>sc_modify_update.
          <mod>-key         = lr_s_root->key.
          <mod>-data        = lr_s_root.
    
          "Update the customer record:
          DATA lo_change  TYPE REF TO /bobf/if_tra_change.
          DATA lo_message TYPE REF TO /bobf/if_frw_message.
          CALL METHOD lo_driver->mo_svc_mngr->modify
            EXPORTING
              it_modification = lt_mod
            IMPORTING
              eo_change       = lo_change
              eo_message      = lo_message.
    
          "Check for errors:
          IF lo_message IS BOUND AND lo_message->check( ) EQ abap_true.
            lo_driver->display_messages( lo_message ).
            RETURN.
          ENDIF.
          
          "Apply the transactional changes:
          DATA lv_rejected TYPE boole_d.
          CALL METHOD lo_driver->mo_txn_mngr->save
            IMPORTING
              eo_message  = lo_message
              ev_rejected = lv_rejected.
    
          IF lv_rejected EQ abap_true.
            lo_driver->display_messages( lo_message ).
            RETURN.
          ENDIF.
    
          "If we get to here, then the operation was successful:
          WRITE: / 'Customer', iv_customer_id, 'updated successfully.'.
        CATCH /bobf/cx_frw INTO DATA(lx_bopf_ex).
          DATA(lv_err_msg) = lx_bopf_ex->get_text( ).
          WRITE: / lv_err_msg.
        ENDTRY.
      ENDMETHOD.                 " METHOD change_customer
    ENDCLASS.