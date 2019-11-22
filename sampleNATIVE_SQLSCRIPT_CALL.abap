** Native SAPScript Call from ADBC
**
** Purpose: Call the native store procedure from ADBC
**
** Author : Simon Li  Nov 2019
**
report ztest_native_sqlscript_call.

parameters p_con type dbcon-con_name default 'DEFAULT'.

types:
* result table structure
  begin of result_t,
    key   type i,
    value type string,
  end of result_t.

data:
* ADBC
  sqlerr_ref type ref to cx_sql_exception,
  con_ref    type ref to cl_sql_connection,
  stmt_ref   type ref to cl_sql_statement,
  res_ref    type ref to cl_sql_result_set,
* results
  result_tab type table of result_t,
  row_cnt type i.

start-of-selection.
  try.
    con_ref = cl_sql_connection=>get_connection( p_con ).
    stmt_ref = con_ref->create_statement( ).

*************************************
** Setup test and procedure
*************************************
* Create test table
    try.
      stmt_ref->execute_ddl( 'DROP TABLE ztestproc_tab' ).
      catch cx_sql_exception.
    endtry.
 
    stmt_ref->execute_ddl(
      'CREATE TABLE ztestproc_tab( key INT PRIMARY KEY, value NVARCHAR(255) )' ).
 
    stmt_ref->execute_update(
      'INSERT INTO ztestproc_tab VALUES(1, ''Test value'' )' ).

* Create test procedure with one output parameter
    try.
      stmt_ref->execute_ddl( 'DROP PROCEDURE ztestproc' ).
      catch cx_sql_exception.
    endtry.
 
    stmt_ref->execute_ddl(
      `CREATE PROCEDURE ztestproc( OUT t1 ztestproc_tab ) ` &&
      `READS SQL DATA AS ` &&
      `BEGIN ` &&
      `  t1 = SELECT * FROM ztestproc_tab; ` &&
      `END`
    ).

*************************************
** Execution time
*************************************
   perform execute_with_transfer_table.
   perform execute_with_gen_temptables.
   con_ref->close( ).
 
   catch cx_sql_exception into sqlerr_ref.
     perform handle_sql_exception using sqlerr_ref.
 endtry.

form execute_with_transfer_table.
  data lr_result type ref to data.
* Create transfer table for output parameter
* this table is used to transfer data for parameter 1 of proc zrs_testproc
* for each procedure a new transfer table has to be created
* when the procedure is executed via result view, this table is not needed
* If the procedure has more than one table type parameter, a transfer table is
* needed for each parameter
* Transfer tables for input parameters have to be filled first before the call
* is executed
  try.
    stmt_ref->execute_ddl( 'DROP TABLE ztestproc_p1' ).
    catch cx_sql_exception.
  endtry.
 
  stmt_ref->execute_ddl(
    'CREATE GLOBAL TEMPORARY COLUMN TABLE ztestproc_p1( key int, value NVARCHAR(255) )'
  ).
  
* clear output table in session
* should be done each time before the procedure is called
  stmt_ref->execute_ddl( 'TRUNCATE TABLE ztestproc_p1' ).

* execute procedure call
  res_ref = stmt_ref->execute_query( 'CALL ztestproc( ztestproc_p1 ) WITH OVERVIEW' ).
  res_ref->close( ).

* read result for output parameter from output transfer table
  res_ref = stmt_ref->execute_query( 'SELECT * FROM ztestproc_p1' ).

* assign internal output table
  clear result_tab.
  get reference of result_tab into lr_result.
  res_ref->set_param_table( lr_result ).

* get the complete result set in the internal table
  row_cnt = res_ref->next_package( ).
  write: / 'EXECUTE WITH TRANSFER TABLE:', / 'Row count: ', row_cnt.
  
  perform output_result.
endform.

form execute_with_gen_temptables.
* mapping between procedure output parameters
* and generated temporary tables
  types:
    begin of s_outparams,
      param_name type string,
      temptable_name type string,
    end of s_outparams.

  data lt_outparam type standard table of s_outparams.
  data lr_outparam type ref to data.
  data lr_result type ref to data.
  field-symbols <ls_outparam> type s_outparams.

* call the procedure which returns the mapping between procedure parameters
* and the generated temporary tables
  res_ref = stmt_ref->execute_query( 'CALL ztestproc(null) WITH OVERVIEW' ).

  clear lt_outparam.
  get reference of lt_outparam into lr_outparam.
  res_ref->set_param_table( lr_outparam ).
  res_ref->next_package( ).

* get the temporary table name for the parameter T1
  read table lt_outparam assigning <ls_outparam> with key param_name = 'T1'.
  assert sy-subrc = 0.

* retrieve the procedure output from the generated temporary table
  res_ref = stmt_ref->execute_query( 'SELECT * FROM ' && <ls_outparam>-temptable_name ).

  clear result_tab.
  get reference of result_tab into lr_result.
  res_ref->set_param_table( lr_result ).
  row_cnt = res_ref->next_package( ).
 
  write: / 'EXECUTE WITH GENERATED TEMP TABLES:', / 'Row count:', row_cnt.
  perform output_result.
endform.

form handle_sql_exception using p_sqlerr_ref type ref to cx_sql_exception.
  format color col_negative.
  
  if p_sqlerr_ref->db_error = 'X'.
    write: / 'SQL error occured:', p_sqlerr_ref->sql_code, "#EC NOTEXT
           / p_sqlerr_ref->sql_message.
  else.
    write:
      / 'Error from DBI (details in dev-trace):', "#EC NOTEXT
        p_sqlerr_ref->internal_error.
  endif.
endform.

form output_result.
  write / 'Result table:'.

  loop at result_tab assigning field-symbols(<ls>).
     write: / <ls>-key, <ls>-value.
  endloop.
endform.

*Output:
*EXECUTE WITH TRANSFER TABLE:
*Row count: 1
*Result table:
* 1 Test value
*EXECUTE WITH GENERATED TEMP TABLES:
*Row count: 1
*Result table_
* 1 Test value
