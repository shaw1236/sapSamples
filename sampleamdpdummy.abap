CLASS zcl_amdp_dummy DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
  INTERFACES if_amdp_marker_hdb.
  
  class-methods: get_dummy_data for table function zget_dummy_data.
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
    declare arrInt integer array;              -- array of integer
    declare dec decimals := 10.2;              -- Decimals
    declare lv_current_date date := '00000000; -- date 
    declare lv_current_time time := '000000;   -- time 
    declare nvarchar1 nvarchar(10) := 'Test';  -- unicode varchar
    declare char2 char(20) := 'Test';          -- fixed length
    declare varchar3 varchar(30): = 'tt1';     -- variable length
    declare tab1 table(id: integer, name: char(40)); -- table
    declare dataSet sflight; -- define the variables
    
    int1 := 100;
    dataset.carrid := 'LH';
    dataSet.connid := '0400';
    
    select * into dataSet from sflight
       where carrid = :dataset.carrid  
         and connid = :dataset.connid;

    select current_date, curremt_time into lv_current_date, lv_current_time from dummy;

    select partner, name1 into tab1 from but000
       where crdat = :lv_current_date;      
  endmethod.

  method get_dummy_data by database function
                           for hdb
                           language sqlscript
                           options read-only
                           using but000.
    -- This is a comment
    /* This is multiple lines comment
    ...
    */
    declare int1 int := 0;
    declare int2 integer := 10;                -- integer
    declare dec decimals := 10.2;              -- Decimals
    declare lv_current_date date := '00000000; -- date 
    declare lv_current_time time := '000000;   -- time 
    declare nvarchar1 nvarchar(10) := 'Test';  -- unicode varchar
    declare char2 char(20) := 'Test';          -- fixed length
    declare varchar3 varchar(30): = 'tt1';     -- variable length
    declare tab1 table(id: integer, name: char(40)); -- table
    declare bpmaster but0000;                  -- define the variables
 
    lt_dummy = select 'Test 1' as literal,
                      (1 + 10) as number,
                      current_date as date,
                      current_time as time
                  from dummy 
               union all
               select 'Test 2' as literal,
                      (1 + 10) as number,
                      current_date as date,
                      current_time as time
                  from dummy;

    return :lt_dummy;
  endmethod.
ENDCLASS.