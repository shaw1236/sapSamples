** ABAP Calling Database Procedure
**
** Purpose: Sample to show how to call a database procedure written in SQL Script from 
**          an ABAP report 
**
** Author : Simon Li  March 22, 2020
**
** Steps:
**  1. Create or replace a database procedure via ADBC
**  2. Obtain an api and create a procedure proxy
**  3. Call Database Procedure <procedure>
**  4. Price change assertion 
**
** Exception: cx_sy_db_procedure_sql_error <= cx_sy_db_procedure_call <= cx_dynamic_check  
REPORT zdemo_call_db_procedure.

CLASS proxy_demo DEFINITION.
  PUBLIC SECTION.
    CLASS-METHODS main.

  PRIVATE SECTION.
    CONSTANTS con_proc_name TYPE if_dbproc_proxy_basic_types=>ty_db_name
                        VALUE `ABAP_DEMO_INCPRICE`.
    CONSTANTS con_prox_name TYPE if_dbproc_proxy_basic_types=>ty_abap_name
                        VALUE `ZABAP_DEMO_INCPRICE_PROXY`.
    CLASS-METHODS create_db_procedure_ADBC.
ENDCLASS.

CLASS proxy_demo IMPLEMENTATION.
  METHOD main.
    DATA lv_incprice TYPE sflight-price.

    " Check the feature
    IF NOT cl_abap_dbfeatures=>use_features(
             EXPORTING
               requested_features =
                 VALUE #( (
                  cl_abap_dbfeatures=>call_database_procedure ) ) ).
      cl_demo_output=>display( `Current database does not support CALL DATABASE PROCEDURE` ).
      LEAVE PROGRAM.
    ENDIF.

    cl_demo_input=>request( CHANGING field = lv_incprice ).
    IF incprice IS INITIAL.
      RETURN.
    ENDIF.

    SELECT price FROM sflight UP TO 1 ROWS INTO @DATA(lv_price_before)
      ORDER BY carrid, connid, fldate.
    ENDSELECT.

    " "_SYS_BIC"."ABAP_DEMO_INCPRICE"
    create_db_procedure_ADBC( ).

    "the internal table params is used to define the mapping between the parameter interface and ABAP data 
    "types. The parameter names are also modified.
    DATA(params) =
      VALUE if_dbproc_proxy_basic_types=>ty_param_override_t(
            ( db_name   = 'INC'         " (IN inc DECIMAL(15,2)) in procedure
              abap_name = 'INCREASE'    " EXPORTING increase = lv_incprice in abap
              descr     = cl_abap_typedescr=>describe_by_name( 'SFLIGHT-PRICE' ) 
            ) 
        ).   " DATA lv_incprice TYPE sflight-price.
    " db_name: in/out in the procedure
    " abap_name: abap importing/exporting parameter
    " descr    : abap type for the parameter of abap_name  

    TRY.
        " The API is created using the factory method GET_PROXY_PUBLIC_API from the factory class 
        " CL_DBPROC_PROXY_FACTORY.
        DATA(api) = cl_dbproc_proxy_factory=>get_proxy_public_api( if_proxy_name = con_prox_name ).
        api->delete( ).

        " The proxy is created using the method CREATE_PROXY of interface IF_DBPROC_PROXY_PUBLIC_API
        api->create_proxy( EXPORTING
                             if_proc_schema    = '_SYS_BIC'
                             it_param_override = params
                             if_proc_name      = con_proc_name ).
        COMMIT CONNECTION default.

        TRY.
            CALL DATABASE PROCEDURE (con_prox_name)
              EXPORTING increase = lv_incprice.

          CATCH cx_sy_db_procedure_sql_error INTO DATA(lo_db_exc).
            cl_demo_output=>display( lo_db_exc->get_text( ) ).
            RETURN.

          catch CX_SY_DB_PROCEDURE_CALL.
            RETURN.
        ENDTRY.
        api->delete( ).

      CATCH cx_dbproc_proxy INTO DATA(lo_api_exc).
        cl_demo_output=>display( lo_api_exc->get_text( ) ).
        RETURN.
    ENDTRY.

    SELECT price FROM sflight INTO @DATA(lv_price_after) UP TO 1 ROWS
      ORDER BY carrid, connid, fldate.
    ENDSELECT.
    " assertion
    IF lv_price_after - lv_price_before = lv_incprice.
      cl_demo_output=>display( `Price increased succesfully` ).
    ENDIF.
  ENDMETHOD.

  METHOD create_db_procedure_ADBC.
    DATA(sql) = NEW cl_sql_statement( ).

    "ADBC is used to create the same database procedure
    TRY.   " DDL, drop the procedure
        sql->execute_ddl( `DROP PROCEDURE ` && `"_SYS_BIC"."` && con_proc_name && `"` ).
        "sql->execute_ddl( |DROP PROCEDURE "_SYS_BIC"."{ con_proc_name }"| ).
      CATCH cx_sql_exception ##NO_HANDLER.
    ENDTRY.

    TRY.
        sql->execute_ddl(
           `CREATE PROCEDURE  `
        && `"_SYS_BIC"."` && con_proc_name && `"`
        "&& |"_SYS_BIC"."{ con_proc_name }"|
        && ` (IN inc DECIMAL(15,2)) AS `
        && `BEGIN `
        && `  UPDATE sflight SET price = price + :inc WHERE mandt = '` && sy-mandt && `'; `
        "&& |  UPDATE sflight SET price = price + :inc WHERE mandt = '{ sy-mandt }'; |
        && `END` ).

      CATCH cx_sql_exception INTO DATA(lo_err).
        cl_demo_output=>display( lo_err->get_text( ) ).
    ENDTRY.
  ENDMETHOD.
ENDCLASS.

START-OF-SELECTION.
  demo=>main( ).