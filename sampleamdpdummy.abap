** ABAP Managed Database Procedure and CDS Table Function 
**
** Purpose: Sample for how to use sqlscript
**
** Author : Simon Li  Sep 2018
**
CLASS zcl_amdp_dummy DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
  INTERFACES if_amdp_marker_hdb.
  
  class-methods get_dummy_data for table function ztf_dummy_data.
  class-methods get_dummy_dp.

  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.

CLASS zcl_amdp_dummy IMPLEMENTATION.
  method get_dummy_dp by database procedure
                           for hdb
                           language sqlscript
                           options read-only
                           using sflight but000.
    -- This is a comment
    /* This is multiple lines comment
    ...
    */

    -- Hana Data Type
    -- 1. Date Time: DATE, TIME, SECONDDATE(YYYYMMDDHHMMSS), TIMESTAMP
    -- 2. Numeric: TINYINT, SMALLINGT, INTEGER, BIGINT, SMALLDECIMAL, DECIMAL, REAL, DOUBLE
    -- 3. Boolean: Boolean(TRUE, FALSE)
    -- 4. Character: VARCHAR, NVARCHAR, ALPHANUM, SHORTTEXT
    -- 5. Binary: VARBINARY
    -- 6. Large Objects: BLOB, CLOB, NCLOB, TEXT
    -- 7. Multi-Value: ARRAY
    
    declare int1 smallint := 0;
    declare int2 integer := 10;                -- integer
    declare dataset_carrid char(3) := 'LH1';
    declare dataset_connid char(4) := '0001';
    declare arrInt integer array := array(1, 2, 3); -- array of integer
    declare dec decimal := 10.2;              -- Decimals
    declare lv_current_date date := '00000000'; -- date 
    declare lv_current_time time := '000000';   -- time 
    declare nvarchar1 nvarchar(10) := 'Test';  -- unicode varchar
    declare char2 char(20) := 'Test';          -- fixed length
    declare varchar3 varchar(30) := 'tt1';     -- variable length
    declare tab1 table(id integer, name char(40)); -- table
    declare dataSet sflight; -- define the variables
    
    int1 := 100;
    --dataset.carrid := 'LH';
    --dataSet.connid := '0400';
    
    dataSet = select top 1 * from sflight
       where carrid = :dataset_carrid  
         and connid = :dataset_connid;

    select current_date, curremt_time into lv_current_date, lv_current_time from dummy;

    tab1 = select 12 as id, 'Simon' as name from dummy
           union all 
           select 22 as id, 'James' as name from dummy; 

    tab1 = select partner, name1 from but000
       where crdat = :lv_current_date;      
  endmethod.

*  @EndUserText.label: 'Dummy Table Function'
*  define table function ztf_dummy_data
*  with parameters 
*  @Environment.systemField: #CLIENT
*  p_client: abap.clnt
*  returns {
*  client: abap.clnt;
*  literal: abap.char(40);
*  number: abap.int4;
*  date: abap.dats;
*  time: abap.tims;  
*}
*implemented by method zcl_amdp_test=>get_dummy_data;
  method get_dummy_data by database function
                           for hdb
                           language sqlscript
                           options read-only.
    -- This is a comment
    /* This is multiple lines comment
    ...
    declare int1 int := 0;
    declare int2 integer := 10;                -- integer
    declare dec decimal  := 10.2;              -- Decimals
    declare lv_current_date date := '00000000'; -- date 
    declare lv_current_time time := '000000';   -- time 
    declare nvarchar1 nvarchar(10) := 'Test';  -- unicode varchar
    declare char2 char(20) := 'Test';          -- fixed length
    declare varchar3 varchar(30) := 'tt1';     -- variable length
    declare tab1 table(id integer, name char(40)); -- table
    declare bpmaster but0000;                  -- define the variables
  
    tab1 = select 12 as id, 'Simon' as name from dummy
           union all 
           select 22 as id, 'James' as name from dummy; 
  */

    lt_dummy = select :p_client as client,
                      'Test 1' as literal,
                      (1 + 10) as number,
                      current_date as date,
                      current_time as time
                  from dummy 
               union all
               select :p_client as client,
                      'Test 2' as literal,
                      (100 + 10) as number,
                      add_days(current_date, 10) as date,
                      current_time as time
                  from dummy;

    return :lt_dummy;
  endmethod.
ENDCLASS.