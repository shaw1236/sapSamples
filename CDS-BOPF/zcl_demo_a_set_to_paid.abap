CLASS zcl_demo_a_set_to_paid DEFINITION
	PUBLIC
	INHERITING FROM /bobf/cl_lib_a_supercl_simple
	FINAL
	CREATE PUBLIC.
	
	PUBLIC SECTION. 
        " interfaces /bobf/if_frw_action is in the supper class 
	    METHODS /bobf/if_frw_action~execute REDEFINITION.
	PROTECTED SECTION.
	PRIVATE SECTION.
ENDCLASS.
	
CLASS zcl_demo_a_set_to_paid IMPLEMENTATION.
	
  METHOD /bobf/if_frw_action~execute.
	" Typed with node's combined table type
    DATA(lt_sales_order) = VALUE ztdemo_i_salesorder_tx( ).  " Combined table type
    
    " all attributes of the root node are represented by the combined structure’s component.
	" READING BO data ----------------------------------------------
	" Retrieve the data of the requested node instance
	io_read->retrieve(
	  EXPORTING
		iv_node         = is_ctx-node_key
		it_key          = it_key
	  IMPORTING
		et_data         = lt_sales_order
	).
	
	" WRITING BO data ---------------------------------------------
	LOOP AT lt_sales_order ASSIGNING FIELD-SYMBOL(<sales_order>).		
	  " Set the attribue billing_status to new value
	  <sales_order>-billingstatus = 'P'.  " PAID
			
	  " Set the attribue overall_status to new value
	  IF <sales_order>-overallstatus = 'N' OR <sales_order>-overallstatus = ' '.
		<sales_order>-overallstatus  = 'P'.  " PAID
	  ENDIF.
					
	  " Update the changed data (billig_status) of the node instance
	  io_modify->update(
	    EXPORTING
		  iv_node               = is_ctx-node_key
		  iv_key                = <sales_order>-key
		  iv_root_key           = <sales_order>-root_key
		  is_data               = REF #( <sales_order>-node_data )
		  it_changed_fields     = VALUE #(
		    ( zif_demo_i_salesorder_tx_c=>sc_node_attribute-zdemo_i_salesorder_tx-billingstatus )
		    ( zif_demo_i_salesorder_tx_c=>sc_node_attribute-zdemo_i_salesorder_tx-overallstatus )
		  )
	  ).
	ENDLOOP.					
  ENDMETHOD.
ENDCLASS.								