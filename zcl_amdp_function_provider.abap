** ABAP AMDP Framework for CDS Table Function 
**
** Purpose: Sample for implementind CDS table functions and using them
**
** Author : Simon Li  May 2019
**
CLASS zcl_amdp_function_provider DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    types: ty_char256 type c length 256,
           tt_flights type table of zflights.
    interfaces: if_amdp_marker_hdb,
                if_oo_adt_classrun.
    class-methods: get_flights for table function ztf_flights,
                   get_data for table function ztf_get_data,
                   get_travel_data for table function ztf_travel_541,
                   get_rank_data for table function ztf_rank_001.
    class-methods:
                   call_1 exporting value(e_name) type ty_char256
                                    value(et_flights) type tt_flights.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.

*** DCL - Analytic Privilege
** Role ZCDS_001_ROLE
*//@AccessControl.authorizationCheck: #NOT_REQUIRED -- checked
*//@AccessControl.authorizationCheck: #CHECK   -- checked
*//@AccessControl.authorizationCheck: #NOT_ALLOWED -- ignored
*@EndUserText.label: 'CDS 001 Role'
*@MappingRole: true
*define role ZCDS_001_ROLE {
**    --grant select on zcds_001; -- full access
**    grant select on zcds_001
**      -- where carrid like 'A%' or carrid like 'C_' or carrid = 'UA'; -- content only
**      -- where (carrid) = aspect pfcg_auth(zs_carrid, zcarrid, actvt='03'); -- PFCG only
**      where (carrid) = aspect pfcg_auth(zs_carrid, zcarrid, actvt='03') or -- content + PFCG
**         carrid like 'A_';  
**         
*   grant select on ztf_get_data 
*      where class = 'A' or class = 'B';      
*}

*** DDL - Object: Dictionary
** Table ZTRAVEL_541 
*@EndUserText.label : 'Database table for travel data 541'
*@AbapCatalog.enhancementCategory : #NOT_EXTENSIBLE -- EXTENSIBLE_CHARACTER, CHARACTER_NUMERIC, ANY
*@AbapCatalog.tableCategory : #TRANSPARENT -- #GLOBAL_TEMPORY
*@AbapCatalog.deliveryClass : #C -- #A, #L, #E, #S, #W  
*@AbapCatalog.dataMaintenance : #LIMITED -- ALLOWED, NOT ALLOWED

*define table ztravel_541 {
*  key client      : abap.clnt not null;
*  key travel_id   : /dmo/travel_id not null;
*  agency_id       : /dmo/agency_id;
*  customer_id     : /dmo/customer_id;
*  begin_date      : /dmo/begin_date;
*  end_date        : /dmo/end_date;
*  @Semantics.amount.currencyCode : 'ztravel_541.currency_code'
*  booking_fee     : /dmo/booking_fee;
*  @Semantics.amount.currencyCode : 'ztravel_541.currency_code'
*  total_price     : /dmo/total_price;
*  currency_code   : /dmo/currency_code;
*  description     : /dmo/description;
*  overall_status  : /dmo/overall_status;
*  created_by      : syuname;
*  created_at      : timestampl;
*  last_changed_by : syuname;
*  last_changed_at : timestampl;
*}
** Table ZFLIGHTS
*@EndUserText.label : 'Flights'
*@AbapCatalog.enhancementCategory : #NOT_EXTENSIBLE
*@AbapCatalog.tableCategory : #TRANSPARENT
*@AbapCatalog.deliveryClass : #A -- #A, #C, #L, #G, #E, #S, #W  
*@AbapCatalog.dataMaintenance : #LIMITED
*define table zflights {
* key mandt    : abap.clnt;
* key carrid   : abap.char(3);
* key connid   : abap.char(5)
* key fldate   : abap.dats;
* fltime   : abap.tims;
* city_from: abap.char(32);
* city_to  : abap.char(32);
*}

CLASS zcl_amdp_function_provider IMPLEMENTATION.
   method if_oo_adt_classrun~main.
      try.
        call_1( importing e_name = data(l_name)
                          et_flights = data(lt_flights)
              ).
        out->write( |Name: { l_name }| ).
        loop at lt_flights into data(ls_flight) from 1 to 1.
          "out->write( |{ ls_flight-carrid }, { ls_flight-connid }| ).
        endloop.

        select * from zcds_flights into table @data(lt_flights2).
        "select * from ZV_FLIGHTS into table @data(lt_flights3).
        select * from ztf_get_data( p_class = 'A' ) into table @data(lt_data).
        loop at lt_data into data(ls_data).
          out->write( |{ ls_data-class }, { ls_data-val }| ).
        endloop.

        catch cx_root into data(lo_root).
            data(lv_message) = lo_root->get_text( ).
            out->write( |Message: { lv_message }| ).
      endtry.
   endmethod.
   method call_1 by database procedure
                 for hdb
                 language sqlscript
                 options read-only
                 using zcl_amdp_sqlscript_flow=>get_flights_proc
                       zcl_amdp_sqlscript_flow=>return_name.
      declare l_carrid varchar( 3 ) = '';
      e_name = ''; :et_flights.delete( );

      call "ZCL_AMDP_SQLSCRIPT_FLOW=>RETURN_NAME"( name => :e_name );


      call "ZCL_AMDP_SQLSCRIPT_FLOW=>GET_FLIGHTS_PROC"(
                     in_carrid  => :l_carrid,
                     et_flights => :et_flights
                  );
   endmethod.
   method get_flights by database function for hdb
                      language sqlscript
                      options read-only
                      using zflights.
     lt = select '120' as client,
                 carrid,
                 connid,
                 fldate,
                 fltime,
                 city_from,
                 city_to
            from zflights;

     return :lt;
   endmethod.

   method get_travel_data by database function for hdb
                      language sqlscript
                      options read-only
                      using ztravel_541.
/* ztf_travel_541
@EndUserText.label: 'Travel 541'
define table function ztf_travel_541
returns {
  key client      : abap.clnt;
  key travel_id   : /dmo/travel_id;
  t_id            : abap.int4;
  agency_id       : /dmo/agency_id;
  customer_id     : /dmo/customer_id;
  begin_date      : /dmo/begin_date;
  end_date        : /dmo/end_date;
  booking_fee     : /dmo/booking_fee;
  total_price     : /dmo/total_price;
  currency_code   : /dmo/currency_code;
  description     : /dmo/description;
  overall_status  : /dmo/overall_status;
}
implemented by method zcl_amdp_function_provider=>get_travel_data;
*/
     et = select
                 client,
                 ltrim(travel_id, '0') as travel_id,
                 ROW_NUMBER() OVER () AS T_ID,
                 ltrim(agency_id, '0') as agency_id,
                 ltrim(customer_id, '0') as customer_id,
                 begin_date,
                 end_date,
                 booking_fee,
                 total_price,
                 currency_code,
                 description,
                 overall_status
           from ztravel_541
           order by client, to_int(travel_id);

     begin -- check the overall status
        declare idx int default 0;
        declare lv_current date = to_date(now());

        for idx in 1..record_count( :et ) do
            if :et.end_date[ idx ] < lv_current then
               et.overall_status[ idx ] = 'T'; -- all ended
            end if;
        end for;
     end;
     return :et;
   endmethod.

   method get_rank_data by database function for hdb
                        language sqlscript
                        options read-only.
/*
@EndUserText.label: 'Rank'
define table function ztf_rank_001
returns {
   client    : abap.clnt;
   row_id    : abap.int4;
   class     : abap.char(10);
   val       : abap.int4;
   row_num   : abap.int4;
   rank      : abap.int4;
   dense_rank: abap.int4;
}
implemented by method zcl_amdp_function_provider=>get_rank_data;
*/
    declare lt table(class CHAR(10), val INT, offset INT);

    INSERT INTO :lT VALUES('A', 1, 1);
    INSERT INTO :lT VALUES('A', 3, 3);
    INSERT INTO :lT VALUES('B', 7, 1);
    INSERT INTO :lT VALUES('C', 2, 1);
    INSERT INTO :lT VALUES('A', 5, null);
    INSERT INTO :lT VALUES('A', 5, 2);
    INSERT INTO :lT VALUES('A', 10, 0);
    INSERT INTO :lT VALUES('C', 4, 2);
    INSERT INTO :lT VALUES('B', 1, 3);
    INSERT INTO :lT VALUES('B', 1, 1);
    INSERT INTO :lT VALUES('C', 1, 5);

    begin
        declare ls row like :lt;
        ls.class = 'A'; ls.val = 9; ls.offset = 3;
        insert into :lt values(:ls.class, :ls.val, :ls.offset);
        --:lt.insert( :ls ); -- cannot be mixed with DML
    end;
    begin
        declare ls row (class char(10), val int, offset int);
        ls = row(trim(' A '), 9, null); ls.val = :ls.val + 1;
        insert into :lt values(:ls.class, :ls.val, :ls.offset);
    end;

    return
        SELECT '100' client,
           ROW_NUMBER() OVER () AS row_id,
           class,
           val,
           ROW_NUMBER() OVER (PARTITION BY class ORDER BY val) AS row_num,
           RANK() OVER (PARTITION BY class ORDER BY val) AS rank,
           DENSE_RANK() OVER (PARTITION BY class ORDER BY val) AS dense_rank
    FROM :lT;
  endmethod.
  method get_data by database function for hdb
                        language sqlscript
                        options read-only.
/*
@EndUserText.label: 'Get Data'
@ClientHandling.type: #CLIENT_INDEPENDENT
@AccessControl.authorizationCheck: #CHECK
define table function ztf_get_data
with parameters p_class: abap.char(10)
returns {
  class: abap.char(10);
  val:   abap.int4;
}
implemented by method zcl_amdp_function_provider=>get_data;
*/
    declare lt table(class CHAR(10), val INT, offset INT);

    INSERT INTO :lT VALUES('A', 1, 1);
    INSERT INTO :lT VALUES('A', 3, 3);
    INSERT INTO :lT VALUES('B', 7, 1);
    INSERT INTO :lT VALUES('C', 2, 1);
    INSERT INTO :lT VALUES('A', 5, null);
    INSERT INTO :lT VALUES('A', 5, 2);
    INSERT INTO :lT VALUES('A', 10, 0);
    INSERT INTO :lT VALUES('C', 4, 2);
    INSERT INTO :lT VALUES('B', 1, 3);
    INSERT INTO :lT VALUES('B', 1, 1);
    INSERT INTO :lT VALUES('C', 1, 5);

    temp_t = select class, val from :lt;
    if :p_class is null or :p_class = '*' then
    else
       begin
         declare filter varchar( 132 );
         --filter = :column || ' = ''' || :value || '''';
         filter = 'class' || ' = ''' || :p_class || '''';
         --filter = ' class = ''A'' ';
         temp_t = apply_filter(:temp_t, :filter);
       end;
    end if;

    if 1 = 0 then
      begin -- remove the duplicate line -- delete adjacent duplicates from ... compare...
        --lt1 = select class, val, count( * ) from :temp_t group by class, val;
        et = select class, val from
               (select class, val, count( * ) from :temp_t group by class, val);
        return :et;
      end;
    else
      begin -- remove the duplucates and return the top
        declare idx, pos int;
        declare et table(class char(10), val int) search key( class );
        temp_t2 = select class, val from :temp_t order by class, val desc;
        for idx in 1..record_count( :temp_t2 ) do
          pos = :et.search(class, :temp_t2.class[idx]);
          if :pos < 0 then
             :et.insert((:temp_t2.class[idx], :temp_t2.val[idx]), -pos);
          end if;
        end for;
        return :et;
      end;
    end if;

  endmethod.
endclass.