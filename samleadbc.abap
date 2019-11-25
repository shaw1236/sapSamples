
** ABAP Database Connectivity Demo 
**
** Purpose: Sample for data access via ADBC
**
** Author : Simon Li  Jul 2018 adapted from openSAP course
**
REPORT ztest_adbc_simple.
   TYPES: BEGIN OF ty_res,
            bp_id              TYPE snwd_bpa-bp_id,
            company_name       TYPE snwd_bpa-company_name,
            currency_code      TYPE snwd_so-currency_code,
            total_gross_amount TYPE snwd_so-gross_amount,
          END OF ty_res. 
   DATA lt_result TYPE STANDARD TABLE OF ty_res WITH EMPTY KEY.
   
   DATA(lv_stmt) = 
             |SELECT BP_ID, COMPANY_NAME, SO.CURRENCY_CODE, | &&
             | SUM( SO.GROSS_AMOUNT ) as TOTAL_GROSS_AMOUNT | &&
             |FROM SNWD_BPA AS BPA | &&
             | INNER JOIN SNWD_SO AS SO | &&
             | ON SO.BUYER_GUID = BPA.NODE_KEY | &&
             |GROUP BY BP_ID, COMPANY_NAME, SO.CURRENCY_CODE|.

   TRY.
     "1. create statement object
     DATA(lo_stmt) = NEW cl_sql_statement( ).  " CL_SQL_STATEMENT

     "2. execute the query
     DATA(lo_res) = lo_stmt->execute_query( lv_stmt ).  "CL_SQL_RESULT
     
     "3. set the output parameter
     lo_res->set_param_table( REF #( lt_result ) ).
     
     "4. fetch the result
     lo_res->next_package( ).
     
     "5. release the resources
     lo_res->close( ).
   CATCH cx_sql_exception INTO DATA(lo_exception).
     " do some meaningful error handling
     WRITE: / lo_exception->sql_message.
   ENDTRY.
  
   zcl_demo_output=>display_data( lt_result ).
