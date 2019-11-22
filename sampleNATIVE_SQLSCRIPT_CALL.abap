** Native SAPScript Call from ADBC
**
** Purpose: Call the native store procedure from ADBC
**
** Author : Simon Li  Nov 2019
**

** How to establish a secondary database connection:
* https://blogs.sap.com/2012/04/11/test-16/
*  Preconditions
* -- a database specific client is intalled on all the app servers
* -- tcode sm30 to main table dbcon
*    dbms hdb for Hana, ora for Oracle
*    user Name, DB password, conn.info: host:3<instanceNumber>15
* -- alternative tcode: DBACOCKPIT on the same table dbcon
*
*  Access the data on the secondary database, e.g. 'AB1'
*  -- via Open SQL by supplying the additional syntax of CONNECTION (dbcon)
*    SELECT * FROM sflight CONNECTION ('AB1')
*  -- via Native SQL 
*    EXEC SQL.
*      connect to 'AB1' as 'AB1' -- use dbcon 'AB1'
*    ENDEXEC.
*    EXEC SQL.
*      open dbcur for select * from sflight where mandt = :sy-mandt and carrid = 'LH'
*    ENDEXEC.
*    DO.
*      EXEC SQL.
*        fetch next dbcur into :ls_sflight
*      ENDEXEC.
*      IF sy-subrc NE 0.
*        EXIT.
*      ELSE.
*        APPEND ls_sflight TO lt_sflight.
*      ENDIF.
*    ENDDO.
*    EXEC SQL.
*      close dbcur -- close cursor
*    ENDEXEC.
*    EXEC SQL.
*      disconnect 'AB1'  -- disconnect 2nd connection
*    ENDEXEC.
*  -- via Native SQL ADBC as coded by the following report
report ztest_native_sqlscript_call.

parameters p_conn type dbcon-con_name obligatory default 'AB1'.

constants con_table TYPE string value 'ztestproc_tab'.
constants con_proc  TYPE string value 'ztestproc'.
constants(temp_table) = 'ztestproc_p1'.  

types:
* result table structure
  begin of result_t,
    key   type i,
    value type string,
  end of result_t,
  result_tt type standard table of result_t.

start-of-selection.
  perform main.

form main.
*  data:
* ADBC
*  sqlerr_ref type ref to cx_sql_exception,
*  stmt_ref   type ref to cl_sql_statement,
  try. 
    data(lo_con_ref) = cl_sql_connection=>get_connection( p_conn ).
    data(lo_stmt_ref) = con_ref->create_statement( ).

*************************************
** Setup test and procedure
*************************************
* sql console or script
*=======================================================================
* DROP TABLE ztestproc_tab;
* CREATE TABLE ztestproc_tab( key INT PRIMARY KEY, value NVARCHAR(255) );
* INSERT INTO ztestproc_tab VALUES(1, 'Test value' );
* DROP PROCEDURE ztestproc
* CREATE PROCEDURE ztestproc( OUT t1 ztestproc_tab ) 
*   READS SQL DATA AS 
*   BEGIN 
*     t1 = SELECT * FROM ztestproc_tab; 
*   END;
*=======================================================================

* Create test table
    data(lv_sql) = |DROP TABLE { con_tab }|.
    try.
      lo_stmt_ref->execute_ddl( lv_sql ).
      catch cx_sql_exception.
    endtry.
 
    lv_sql = |CREATE TABLE { con_table }( key INT PRIMARY KEY, value NVARCHAR(255) )|.
    lo_stmt_ref->execute_ddl( lv_sql ).
 
    lv_sql = |INSERT INTO { con_table } VALUES(1, 'Test value' )|.
    lo_stmt_ref->execute_update( lv_sql ).

* Create test procedure with one output parameter
    try.
      lv_sql = |DROP PROCEDURE { con_proc }|.
      lo_stmt_ref->execute_ddl( lv_sql ).
      catch cx_sql_exception.
    endtry.
 
    lv_sql = 
      |CREATE PROCEDURE { con_proc }( OUT t1 { con_table } ) | &&
      |READS SQL DATA AS | &&
      |BEGIN | &&
      |  t1 = SELECT * FROM ztestproc_tab; | &&
      |END|.
    lo_stmt_ref->execute_ddl( lv_sql ).

*************************************
** Execution time
*************************************
    perform execute_with_transfer_table using lo_stmt_ref.
    perform execute_with_gen_temptables using lo_stmt_ref.
   
    lo_con_ref->close( ).
 
    catch cx_sql_exception into data(lo_sqlerr_ref).
      perform handle_sql_exception using lo_sqlerr_ref.
  endtry.
endform.

form test_abap_hana_native_call.
  data lt_result type result_tt.
    " ABAP calls a native procedure
  try.
    CALL DATABASE PROCEDURE ztestproc
      "EXPORTING 
      IMPORTING 
        t1 = lt_result.
    
    data(lv_record_count) = lines( lt_result ).
    write: / 'ABAP HANA NATIVE PROCEDURE CALL:', 
           / 'Row count: ', lv_record_count.       
    perform output_result using lt_result.

    catch CX_SY_DB_PROCEDURE_CALL into data(lo_exception).
      data(lv_message) = lo_exception->get_text( ).
      MESSAGE lv_message TYPE 'E'.
      write: / 'Error:', lv_message.
  endtry.
endform.

form execute_with_transfer_table using value(io_stmt_ref) type ref to cl_sql_statement.
*  data result_tab type table of result_t.
*  data lr_result type ref to data.
*  get reference of result_tab into lr_result.

* Create transfer table for output parameter
* this table is used to transfer data for parameter 1 of proc ztestproc
* for each procedure a new transfer table has to be created
* when the procedure is executed via result view, this table is not needed
* If the procedure has more than one table type parameter, a transfer table is
* needed for each parameter
* Transfer tables for input parameters have to be filled first before the call
* is executed

* sql console or script
*=======================================================================
* DROP TABLE ztestproc_p1;
* CREATE GLOBAL TEMPORARY COLUMN TABLE ztestproc_p1( key int, value NVARCHAR(255);
* TRUNCATE TABLE ztestproc_p1;
* CALL ztestproc( ztestproc_p1 ) WITH OVERVIEW;
* SELECT * FROM ztestproc_p1;
*=======================================================================

  data(lv_sql) = |DROP TABLE { temp_table }|.
  try.
    io_stmt_ref->execute_ddl( lv_sql ).
    catch cx_sql_exception.
  endtry.
 
  lv_sql = |CREATE GLOBAL TEMPORARY COLUMN TABLE { temp_table }( key int, value NVARCHAR(255) )|. 
  io_stmt_ref->execute_ddl( lv_sql ).
  
* clear output table in session
* should be done each time before the procedure is called
  lv_sql = |TRUNCATE TABLE { temp_table }|.
  io_stmt_ref->execute_ddl( lv_sql ).

* execute procedure call
  lv_sql = | CALL { con_proc }( { temp_table } ) WITH OVERVIEW|.
  data(lo_res_ref) = io_stmt_ref->execute_query( lv_sql ).
  lo_res_ref->close( ).

* read result for output parameter from output transfer table
  lv_sql = |SELECT * FROM { temp_table }|.
  lo_res_ref = io_stmt_ref->execute_query( lv_sql ).

* assign internal output table
  data lt_result type result_tt.
  lo_res_ref->set_param_table( REF #( lt_result ) ).

* get the complete result set in the internal table
  data(lv_row_cnt) = res_ref->next_package( ).
  write: / 'EXECUTE WITH TRANSFER TABLE:', 
         / 'Row count: ', lv_row_cnt.
  
  perform output_result using lt_result.
endform.

form execute_with_gen_temptables using value(io_stmt_ref) type ref to cl_sql_statement.
* mapping between procedure output parameters
* and generated temporary tables
  types:
    begin of s_outparams,
      param_name type string,
      temptable_name type string,
    end of s_outparams.

* sql console or script
*=======================================================================
* CALL ztestproc(null) WITH OVERVIEW;
* SELECT * FROM <temptable_name>;
*=======================================================================

* call the procedure which returns the mapping between procedure parameters
* and the generated temporary tables
  data(lv_sql) = |CALL { con_proc }(null) WITH OVERVIEW|.
  data(lo_res_ref) = io_stmt_ref->execute_query( lv_sql ).

  data lt_outparam type standard table of s_outparams.
*  data lr_outparam type ref to data.
*  data lr_result type ref to data.
*  field-symbols <outparam> type s_outparams.
*  get reference of lt_outparam into lr_outparam.
*  res_ref->set_param_table( lr_outparam ).
  lo_res_ref->set_param_table( ref #( lt_outparam ) ).
  lo_res_ref->next_package( ).

* get the temporary table name for the parameter T1
  read table lt_outparam assigning field-symbols(<outparam>) with key param_name = 'T1'.
  assert sy-subrc = 0.

* retrieve the procedure output from the generated temporary table
  lv_sql = |SELECT * FROM { <outparam>-temptable_name }|.
  lo_res_ref = io_stmt_ref->execute_query( lv_sql ).

  data lt_result type result_tt.
  "clear result_tab.
  "get reference of result_tab into lr_result.
  lo_res_ref->set_param_table( REF# ( lt_result ) ).
  data(lv_row_cnt) = lo_res_ref->next_package( ).
 
  write: / 'EXECUTE WITH GENERATED TEMP TABLES:', 
         / 'Row count:', lv_row_cnt.
  perform output_result using lt_result.
endform.

form handle_sql_exception using p_sqlerr_ref type ref to cx_sql_exception.
  format color col_negative.
  
  if p_sqlerr_ref->db_error = 'X'.
    write: / 'SQL error occured:', p_sqlerr_ref->sql_code, "#EC NOTEXT
           / p_sqlerr_ref->sql_message.
  else.
    write: / 'Error from DBI (details in dev-trace):', "#EC NOTEXT
           p_sqlerr_ref->internal_error.
  endif.
endform.

form output_result using it_result type result_tt.
  write / 'Result table:'.
  loop at it_result assigning field-symbols(<result>).
    write: / <result>-key, <result>-value.
  endloop.
endform.

*Output:
*EXECUTE WITH TRANSFER TABLE:
*Row count: 1
*Result table:
* 1 Test value
*EXECUTE WITH GENERATED TEMP TABLES:
*Row count: 1
*Result table
* 1 Test value
