** ABAP Managed Database Procedure - SQL Script Collection
**
** Purpose: Samples for SQL Script used an AMDP class
**
** Author : Simon Li  Jul 2019
**
** Lots of redundancy just shows how to write sql script in programming 
** condition(if-then-else), loop(loop, for-do, while-do), cursor(implicit or explicit)
** array, scalar or table variables, filter, in, exists, exception, parallel
**
** The syntax has been checked in ADT with some running result
** With this, you should gain good confidence in coding AMDP class and HANA code push-down 
CLASS zcl_amdp_sqlscript_flow_collection DEFINITION
PUBLIC
FINAL
CREATE PUBLIC .

PUBLIC SECTION.
  interfaces if_oo_adt_classrun.
  interfaces if_amdp_marker_hdb.
  types: tt_flights type table of zflights.
  types: t_time type c length 12.
  types: ty_char32 type c length 32, ty_char64(64), ty_char128(128).
  types: ty_char256(256), ty_1024(1024), ty_char2048(2048).
  class-methods:
             define_1,
             gettime importing value(i_offset) type i default 0
                     exporting value(e_date) type d value(e_time) type t,
             date_1 exporting value(e_date) type d value(e_time) type t,
             cursor_1 exporting value(ot) type tt_flights,
             table_1 exporting value(ot) type tt_flights,
             LOOP_1,
             CONDITION_1,
             array_1,
             in_1,
             exists_1,
             FILTER_1,
             exception_1,
             exception_2,
             injection_1,
             parallel_1,
             rank_1,
             function_1,
             nested_block_if Importing value(inval) type i
                             exporting value(val) type i,
             nested_block_while exporting value(val) type i,
             nested_block_for Importing value(inval) type i
                             exporting value(val) type i,
             nested_block_loop Importing value(inval) type i
                             exporting value(val) type i,
             cursor_proc,
             foreach_proc,
             cursor_update,
             get_flights_proc importing value(in_carrid) type zflights-carrid
                              exporting value(et_flights) type tt_flights,
             RETURN_NAME exporting value(name) type ty_char256.
PROTECTED SECTION.
PRIVATE SECTION.
ENDCLASS.

CLASS zcl_amdp_flow_test IMPLEMENTATION.
  method if_oo_adt_classrun~main.
        try.
           out->write( 'Start' ).
           call method return_name importing
                                name = data(lv_name).
           out->write( |Name: { lv_name } | ).
           " Name: ZCL_AMDP_FLOW_TEST=>RETURN_NAME, schema name: SAPABAP

           call method date_1 importing
                                e_date = data(lv_date)
                                e_time = data(lv_time).
           out->write( |Date: { lv_date }, time: { lv_time } | ).
           "table_1( importing ot = data(lt_out) ).
           cursor_1( importing ot = data(lt_out) ).
           loop at lt_out into data(ls_data) from 1 to 5.
              out->write( |{ ls_data-carrid }, { ls_data-connid }, { ls_data-fldate } | ).
           endloop.

           data(lv_inval) = 1.
           nested_block_if( exporting inval = lv_inval importing val = data(lv_val) ).
           nested_block_while( importing val = lv_val ).

           nested_block_loop( exporting inval = lv_inval importing val = lv_val ).
           out->write( |OUT:[{ lv_val }]| ).

           catch cx_root into data(lo_root).
          data(lv_text) = lo_root->get_text(  ).
          "out->write( |Error: { lv_text }| ).
        endtry.
    endmethod.

    method return_name by database procedure
                                   for hdb
                                   language sqlscript
                                   options read-only.
      name = ::CURRENT_OBJECT_SCHEMA || '.' || ::CURRENT_OBJECT_NAME || ' Line: '
             || ::CURRENT_LINE_NUMBER;
    endmethod.

    method get_flights_proc by database procedure
                                   for hdb
                                   language sqlscript
                                   options read-only
                                   using zflights.
       et_flights = select * from zflights where carrid = :in_carrid;
       if  ::ROWCOUNT = 0 then
       end if;
    endmethod.

    method nested_block_if by database procedure
                                   for hdb
                                   language sqlscript
                                   options read-only.
        DECLARE a INT = 1;
        DECLARE v INT = 0;
        DECLARE EXIT HANDLER FOR SQLEXCEPTION
        BEGIN
          val = :a;
        END;
        v = 1 /(1-:inval);
        IF :a = 1 THEN
          DECLARE a INT = 2;
          DECLARE EXIT HANDLER FOR SQLEXCEPTION
          BEGIN
              val = :a;
          END;
          v = 1 /(2-:inval);
          IF :a = 2 THEN
              DECLARE a INT = 3;
              DECLARE EXIT HANDLER FOR SQLEXCEPTION
              BEGIN
                  val = :a;
              END;
              v = 1 / (3-:inval);
          END IF;
          v = 1 / (4-:inval);
        END IF;
        v = 1 / (5-:inval);
      endmethod.

      method nested_block_while by database procedure
                                   for hdb
                                   language sqlscript
                                   options read-only.
         DECLARE v int = 2;
         declare idx int default :v;

         val = 0;
         WHILE v > 0 DO
            DECLARE a INT = 0;
            a = :a + 1;
            val = :val + :a;
            v = :v - 1;
         END WHILE;

         val = 1;
         WHILE :idx BETWEEN 5 AND 15 DO
            idx = :idx + 1;
            val = 5;
         END WHILE;

      endmethod.

      method nested_block_for by database procedure
                                   for hdb
                                   language sqlscript
                                   options read-only.
           declare mytab1 table(a int) = (select 1 a from dummy);
           declare mytab2 table(a int) = (select 2 a from dummy);
           declare mytab3 table(a int) default (select 3 a from dummy);

           DECLARE a1 int default 0;
           DECLARE a2 int default 0;
           DECLARE a3 int default 0;
           DECLARE v1 int default 1;
           DECLARE v2 int default 1;
           DECLARE v3 int default 1;
           DECLARE CURSOR C FOR SELECT * FROM :mytab1;
           FOR R as C DO
              DECLARE CURSOR C FOR SELECT * FROM :mytab2;
              a1 = :a1 + R.a;
              FOR R as C DO
                  DECLARE CURSOR C FOR SELECT * FROM :mytab3;
                  a2 = :a2 + R.a;
                  FOR R as C DO
                     a3 = :a3 + R.a;
                  END FOR;
              END FOR;
           END FOR;
           IF inval = 1 THEN
              val = :a1;
           ELSEIF inval = 2 THEN
              val = :a2;
           ELSEIF inval = 3 THEN
              val = :a3;
           END IF;
      endmethod.

      method nested_block_loop by database procedure
                                   for hdb
                                   language sqlscript
                                   options read-only.
           declare mytab1 table(a int) = (select 1 a from dummy);
           declare mytab2 table(a int) = (select 2 a from dummy);
           declare mytab3 table(a int) default (select 3 a from dummy);

           DECLARE a1 int;
           DECLARE a2 int;
           DECLARE a3 int;
           DECLARE v1 int default 1;
           DECLARE v2 int default 1;
           DECLARE v3 int default 1;
           DECLARE CURSOR C FOR SELECT * FROM :mytab1;
           OPEN C;
           FETCH C into a1;
           CLOSE C;
           LOOP
              DECLARE CURSOR C FOR SELECT * FROM :mytab2;
              OPEN C;
              FETCH C into a2;
              CLOSE C;
              LOOP
                  DECLARE CURSOR C FOR SELECT * FROM :mytab3;
                  OPEN C;
                  FETCH C INTO a3;
                  CLOSE C;
                  IF :v2 = 1 THEN
                      BREAK;
                  END IF;
              END LOOP;
              IF :v1 = 1 THEN
                 BREAK;
              END IF;
           END LOOP;
           IF :inval = 1 THEN
               val = :a1;
           ELSEIF :inval = 2 THEN
               val = :a2;
           ELSEIF :inval = 3 THEN
               val = :a3;
           END IF;
      endmethod.

      method cursor_proc by database procedure
                                   for hdb
                                   language sqlscript
                                   options read-only.
         declare books table (isbn varchar(20), title varchar(20), price double, crcy varchar(20));
         DECLARE v_isbn  VARCHAR(20);
         DECLARE v_title VARCHAR(20);
         DECLARE v_price DOUBLE;
         DECLARE v_crcy VARCHAR(20);
         declare ins_msg varchar(132) = '';

         DECLARE CURSOR c_cursor1 (v_isbn VARCHAR(20)) FOR
            SELECT isbn, title, price, crcy FROM :books
              WHERE isbn = :v_isbn ORDER BY isbn;

         OPEN c_cursor1('978-3-86894-012-1');
         IF c_cursor1::ISCLOSED THEN
             ins_msg = 'WRONG: cursor not open';
             return;
         ELSE
             ins_msg = 'OK: cursor open';
         END IF;
         FETCH c_cursor1 INTO v_isbn, v_title, v_price, v_crcy;
         IF c_cursor1::NOTFOUND THEN
              ins_msg = 'WRONG: cursor contains no valid data';
         ELSE
              ins_msg = 'OK: cursor contains valid data';
         END IF;
         CLOSE c_cursor1;
      endmethod.

      method foreach_proc by database procedure
                                   for hdb
                                   language sqlscript
                                   options read-only.
         declare books table (isbn varchar(20), title varchar(20), price double, crcy varchar(20));
         DECLARE v_isbn    VARCHAR(20) = '';
         DECLARE CURSOR c_cursor1 (v_isbn VARCHAR(20)) FOR
              SELECT isbn, title, price, crcy FROM :books
                ORDER BY isbn;

         FOR cur_row AS c_cursor1(v_isbn) DO
            declare ins_msg varchar(132);
            ins_msg = 'book title is: ' || cur_row.title;
         END FOR;

      endmethod.

      method cursor_update by database procedure
                                   for hdb
                                   language sqlscript
                                   options read-only
                                   using zflights.
           declare idx int;
           --DECLARE CURSOR cur FOR SELECT * FROM zflights where carrid = 'zz' FOR UPDATE;
           --FOR r AS cur DO
           --   IF r.carrid > 'zz' THEN
           --     UPDATE zflights SET fldate = add_days(fldate, 1) WHERE CURRENT OF cur;
           --   ELSEif r.carrid = '..' then
           --     DELETE FROM zflights WHERE CURRENT OF cur;
           --   END IF;
           --END FOR;
          DECLARE CURSOR cur FOR SELECT * FROM zflights where carrid = 'zz';
          FOR r AS cur DO
          end for;

          for idx in 1..1 do
             declare t1 table( i1 int );
             insert into :t1 values(1);
             --SAVEPOINT save1;  -- not supprt in read-only
             insert into :t1 values(2);
             --ROLLBACK TO SAVEPOINT save1;  -- not supported
             select * from :t1;
             --RELEASE SAVEPOINT save1;     -- not supported
          end for;
          -- do begin -- not supported
             idx = :idx + 1;
          -- end;

      endmethod.

      method injection_1 by database procedure
                                   for hdb
                                   language sqlscript
                                   options read-only.
        declare tabname, field, old_val, new_val varchar(20);
        declare sqlstr nclob;
        declare mycond condition for sql_error_code 10001;
        if is_sql_injection_safe(field) <> 1 then
           signal mycond set message_text = 'Invalid field ' || field;
        end if;
        sqlstr := 'UPDATE "' || escape_double_quotes(:tabname) || '" SET ' || field ||
                  ' = ''' || escape_single_quotes(:new_val) || ''' WHERE ' || field ||
                  ' = ''' || escape_single_quotes(:old_val) || '''';
        -- exec(:sqlstr); -- DSQL is not support in AMDP

        SELECT IS_SQL_INJECTION_SAFE('tab,le') "safe" FROM DUMMY; -- 0
        SELECT IS_SQL_INJECTION_SAFE('CREATE STRUCTURED PRIVILEGE', 3) "safe" FROM DUMMY; -- 1
        SELECT ESCAPE_SINGLE_QUOTES('Str''ing') "string_literal" FROM DUMMY; -- Str''ing
        SELECT ESCAPE_DOUBLE_QUOTES('TAB"LE') "table_name" FROM DUMMY;  -- TAB""LE
      endmethod.

      method parallel_1 by database procedure
                                   for hdb
                                   language sqlscript
                                   options read-only.
         begin
           select ::CURRENT_LINE_NUMBER from dummy;

           BEGIN PARALLEL EXECUTION
              select 1 from dummy;
              select 2 from dummy;
              select 3 from dummy;
              select 4 from dummy;
              select 5 from dummy;
           end;
         end;

         begin
           declare arr int array = array(1, 2,3 ,4, 5, 6);
           tab = unnest( :arr ) as ( A );
           BEGIN PARALLEL EXECUTION
              SELECT * FROM :tab s where s.A = 1; -- INTO CTAB1;
              SELECT * FROM :tab s where s.A = 2; -- INTO CTAB2;
              SELECT * FROM :tab s where s.A = 3; -- INTO CTAB3;
              SELECT * FROM :tab s where s.A = 4; -- INTO CTAB4;
              SELECT * FROM :tab s where s.A = 5; -- INTO CTAB5;
           END;
         end;
      endmethod.

      method array_1 by database procedure
                                   for hdb
                                   language sqlscript
                                   options read-only.
         DECLARE tab TABLE (A NVARCHAR(10), B INTEGER);
         DECLARE id NVARCHAR(10) ARRAY;

         BEGIN -- assignment and return an Element of an Array
            DECLARE arr TINYINT ARRAY = ARRAY(1,2,3);
            DECLARE index_array INTEGER ARRAY = ARRAY(1,2);
            DECLARE value TINYINT;
            arr[:index_array[1]] = :arr[:index_array[2]];
            value = :arr[:index_array[1]];
            select :value from dummy;
         END;

         begin -- array_agg function, column -> array
           declare val_b int array;
           --tab = SELECT A , B FROM tab1;
           id  = ARRAY_AGG(:tab.A);
           id  = ARRAY_AGG(:tab.A ORDER BY B DESc);
           val_b = array_agg(:tab.b);
         end;
         begin
            declare CTAB table(A NVARCHAR(10));
            INSERT INTO :CTAB VALUES ('A1');
            INSERT INTO :CTAB VALUES (NULL);
            INSERT INTO :CTAB VALUES ('A2');
            INSERT INTO :CTAB VALUES (NULL);
            tab = SELECT A, 1 b FROM :ctab;
            id  = ARRAY_AGG(:tab.A ORDER BY A DESC NULLS FIRST);
            tab2 = UNNEST(:id) AS (A); -- array -> table tab2( A like :id )
            SELECT * FROM :tab2;
         END;
         BEGIN -- TRIM_ARRAY function removes elements from the end of an array
            DECLARE array_id Integer ARRAY := ARRAY(1, 2, 3, 4);
            array_id = TRIM_ARRAY(:array_id, 2);  -- remove last 2 elements
            rst      = UNNEST(:array_id) as ("ID");
         END; -- result: 1, 2
         BEGIN -- CARDINALITY function returns the highest index of a set element
            DECLARE array_id Integer ARRAY;
            declare n integer;
            n = CARDINALITY(:array_id); -- n = 0
            array_id[20] = NULL;
            n = CARDINALITY(:array_id); -- n = 20,  elements 1-19 to NULL
            If CARDINALITY(:array_id) > 0 THEN
               n = 1 ;
            ELSE
               n = 0;
            END IF;
         END;
         BEGIN -- The CONCAT function concatenates two arrays or ||
           DECLARE id1,id2,id3, id4, id5, card INTEGER ARRAY;
           id1[1]  = 0; id2[1]  = 1;
           id3  = CONCAT(:id1, :id2);
           id4  = subarray(:id3, 1, 2);  -- sub array
           id4  = :id1 || :id2;
           rst0  = UNNEST(:id3) WITH ORDINALITY AS ("ID", "SEQ");
           id5  = :id4 || ARRAY_AGG(:rst0."ID" ORDER BY "SEQ");
           rst1 = UNNEST(:id5 || CONCAT(:id1, :id2) || CONCAT(CONCAT(:id1, :id2), CONCAT(:id1, :id2))) WITH ORDINALITY AS ("ID", "SEQ");
           outtab = SELECT SEQ, ID FROM :rst1 ORDER BY SEQ;
         END;

      endmethod.

      method exception_1 by database procedure
                                   for hdb
                                   language sqlscript
                                   options read-only.

         DECLARE A INT = 10;
         DECLARE CONTINUE HANDLER FOR SQLEXCEPTION BEGIN -- Catch the exception
            SELECT ::SQL_ERROR_CODE, ::SQL_ERROR_MESSAGE FROM DUMMY;
         END;

         A = 1 / 0; -- An exception will be thrown
         SELECT :A FROM DUMMY; -- Continue from this statement after handling the exception

         BEGIN
            A = 1 / 0; -- An exception throwing
            A = :A + 1; -- Continue from this statement after handling the exception
         END;
         SELECT :A FROM DUMMY; -- Result: 11

         BEGIN
            DECLARE A INT = 0;
            DECLARE CONTINUE HANDLER FOR SQLEXCEPTION
               SELECT ::SQL_ERROR_CODE, ::SQL_ERROR_MESSAGE FROM DUMMY;

            IF A = 1 / 0 THEN -- An error occurs
               A = 1;
            ELSE
               A = 2;
            END IF;
            SELECT :A FROM DUMMY; -- Continue from here, Result: 0
         END;

         BEGIN
           declare tab table(a int);
           DECLARE CONTINUE HANDLER FOR SQLEXCEPTION BEGIN END;
           INSERT INTO :TAB VALUES(1);
           INSERT INTO :TAB VALUES(1 / 0); -- An error thrown
           SELECT ::ROWCOUNT FROM DUMMY; -- 1, not 0 END;
         END;

         BEGIN
           DECLARE CONTINUE HANDLER FOR SQL_ERROR_CODE 12346 BEGIN END;
           BEGIN
              DECLARE CONTINUE HANDLER FOR SQL_ERROR_CODE 12345
              BEGIN
                 SIGNAL SQL_ERROR_CODE 12346;
                 SELECT ::SQL_ERROR_CODE FROM DUMMY; -- 12346, not 12345
              END;
              SIGNAL SQL_ERROR_CODE 12345;
           END;
         END;

         BEGIN
           DECLARE A INT = 10;
           DECLARE CONTINUE HANDLER FOR SQLEXCEPTION
           BEGIN
             SELECT :A FROM DUMMY; -- Result: 10
           END;    A = 1 / 0;
           SELECT :A FROM DUMMY; -- Result: 10
         END;
      endmethod.

      method exception_2 by database procedure
                                   for hdb
                                   language sqlscript
                                   options read-only.
        begin
            declare l_int int;
            DECLARE division_by_zero CONDITION FOR SQL_ERROR_CODE 304;
            DECLARE EXIT HANDLER FOR division_by_zero
                SELECT ::SQL_ERROR_CODE, ::SQL_ERROR_MESSAGE FROM DUMMY;
            l_int = 1/ 0;
         end;
         begin
           DECLARE invalid_input CONDITION;
           DECLARE invalid_input2 CONDITION FOR SQL_ERROR_CODE 10000; --  must be within 10000 to 19999s
           SIGNAL invalid_input;  -- raise exception
           SIGNAL invalid_input2; -- raise exception
           SIGNAL invalid_input SET MESSAGE_TEXT = 'Invalid input arguments'; -- message ... raising
         end;
         begin
            declare start_date date;
            declare current_dat date;
            DECLARE invalid_input CONDITION FOR SQL_ERROR_CODE 10000;
            DECLARE EXIT HANDLER FOR invalid_input
                SELECT ::SQL_ERROR_CODE, ::SQL_ERROR_MESSAGE FROM DUMMY;
            select top 1 to_date( current_date) into current_dat from dummy;
            IF :start_date > :current_dat THEN
                SIGNAL invalid_input SET MESSAGE_TEXT = 'START_DATE = ' || :start_date
                                                        ||' > ' || :current_dat;
            END IF;
         end;
         begin
            DECLARE EXIT HANDLER FOR SQLEXCEPTION
               --RESIGNAL;
               RESIGNAL SET MESSAGE_TEXT = 'New message from ' ||::SQL_ERROR_MESSAGE;
            outtab = SELECT 1/0 as I FROM dummy;
         end;
         begin
            declare mytab table(a int primary key);
            -- DECLARE EXIT HANDLER FOR SQLEXCEPTION
            DECLARE EXIT HANDLER FOR SQL_ERROR_CODE 301
              SELECT ::SQL_ERROR_CODE, ::SQL_ERROR_MESSAGE FROM DUMMY;
            INSERT INTO :MYTAB VALUES (1);
            INSERT INTO :MYTAB VALUES (1);  -- expected unique violation error: 301
            -- will not be reached
         END;
         begin
           declare mytab table(i int primary key);
           DECLARE myVar INT;
           DECLARE EXIT HANDLER FOR SQL_ERROR_CODE 1299
           BEGIN
             SELECT 0 INTO myVar FROM DUMMY;
             SELECT ::SQL_ERROR_CODE, ::SQL_ERROR_MESSAGE FROM DUMMY;
             SELECT :myVar FROM DUMMY;
           END;
           SELECT I INTO myVar FROM :MYTAB; --NO_DATA_FOUND exception
           SELECT 'NeverReached_noContinueOnErrorSemantics' FROM DUMMY;
         end;
         BEGIN -- user defined
           declare mytab table(i int primary key);
           DECLARE MYCOND CONDITION FOR SQL_ERROR_CODE 301;
           DECLARE EXIT HANDLER FOR MYCOND
              SELECT ::SQL_ERROR_CODE, ::SQL_ERROR_MESSAGE FROM DUMMY;
              INSERT INTO :MYTAB VALUES (1);
              INSERT INTO :MYTAB VALUES (1);  -- expected unique violation error: 301
              -- will not be reached
         END;
         BEGIN -- resignal
           declare mytab table( i int );
           DECLARE MYCOND CONDITION FOR SQL_ERROR_CODE 10001;
           DECLARE EXIT HANDLER FOR MYCOND
              resignal; -- SELECT ::SQL_ERROR_CODE, ::SQL_ERROR_MESSAGE FROM DUMMY;
           INSERT INTO :MYTAB VALUES (1);
           SIGNAL MYCOND SET MESSAGE_TEXT = 'my error';    -- will not be reached
         END;
         begin -- nested block exception
           declare MYTAB table(I INTEGER PRIMARY KEY);
           BEGIN
              DECLARE EXIT HANDLER FOR SQLEXCEPTION RESIGNAL
                 SET MESSAGE_TEXT = 'level 1';
              BEGIN
                 DECLARE EXIT HANDLER FOR SQLEXCEPTION RESIGNAL
                    SET MESSAGE_TEXT = 'level 2';
                 INSERT INTO :MYTAB VALUES (1);
                 BEGIN
                   DECLARE EXIT HANDLER FOR SQLEXCEPTION RESIGNAL
                   SET MESSAGE_TEXT = 'level 3';
                   INSERT INTO :MYTAB VALUES (1);  -- expected unique violation error: 301
                   -- will not be reached
                 END;
              END;
            END;
         end;

      endmethod.

      method gettime by database procedure
                                   for hdb
                                   language sqlscript
                                   options read-only.
         DECLARE EXIT HANDLER FOR SQLEXCEPTION
         BEGIN
            e_date = '00000000'; e_time = '000000';
         END;

         select to_dats(add_days(to_date(current_date), :i_offset)), -- to_dats: yyyymmdd
               replace(to_time(current_time), ':', '') -- to_time: hh:mm:ss
               -- to_time(current_time)
               into e_date, e_time
               from dummy;
         if  ::ROWCOUNT < 1 then
             signal sql_error_code 10011 set  message_text = 'assertion failed';
         end if;
      endmethod.

      method date_1 by database procedure
                                   for hdb
                                   language sqlscript
                                   options read-only
                                   using zcl_amdp_flow_test=>gettime.
         declare lv_offset int default 10;
         call "ZCL_AMDP_FLOW_TEST=>GETTIME"(
            i_offset => lv_offset,
            e_date   => :e_date,
            e_time   => :e_time
         );

      endmethod.

      method table_1 by database procedure
                                   for hdb
                                   language sqlscript
                                   options read-only
                                   using zflights
                                   zcl_amdp_flow_test=>get_flights_proc.
         declare l_date date;
         declare filter varchar(132) = 'carrid = ''LH''';
         DECLARE CONTINUE HANDLER FOR SQLEXCEPTION BEGIN -- Catch the exception
             SELECT ::SQL_ERROR_CODE, ::SQL_ERROR_MESSAGE FROM DUMMY;
         END;
         select :filter, ::CURRENT_LINE_NUMBER from dummy;

         begin
            declare in_carrid varchar(3) default 'LH';
            call "ZCL_AMDP_FLOW_TEST=>GET_FLIGHTS_PROC"(
               in_carrid  => :in_carrid,
               et_flights => :et_flights
            );
         end;

         if 1 = 0 then
            lt = select * from zflights;
            ot = apply_filter(:lt, :filter); -- Applying a filter to a table variable
         else --  Applying a filter to a persistent table
            ot = apply_filter(zflights, :filter);
         end if;

         if not is_empty( :ot ) then
            ot.fldate[1] = '20201230';
            if record_count( :ot ) > 2 then
               declare l_cdate varchar(10);
               select to_dats(current_date) into l_cdate from dummy;
               ot.fldate[2] = :l_cdate;
            end if;
         else
            :ot.(carrid, connid).insert(('ZZ', '100'));
         end if;
      endmethod.

      method filter_1 by database procedure
                                   for hdb
                                   language sqlscript
                                   options read-only
                                   using zflights
                                   zcl_amdp_flow_test=>get_flights_proc.
         declare filter varchar( 132 ) = 'carrid like ''L%''';
         --DECLARE EXIT HANDLER FOR SQLEXCEPTION
         DECLARE EXIT HANDLER FOR SQL_ERROR_CODE 304
           SELECT ::SQL_ERROR_CODE, ::SQL_ERROR_MESSAGE FROM DUMMY;

         -- Applying a filter to a persistent table
         temp_procedures = APPLY_FILTER(zflights,:filter);
         procedures = SELECT carrid, connid, fldate FROM :temp_procedures;

         -- Using a table variable
         temp_procedures2 = select carrid, connid, fldate from zflights;
         procedures2 = apply_filter(:temp_procedures2, :filter);

         begin
            declare in_carrid varchar(3) default 'LH';
            call "ZCL_AMDP_FLOW_TEST=>GET_FLIGHTS_PROC"(
               in_carrid  => :in_carrid,
               et_flights => :et_flights
            );
         end;
      endmethod.

      method in_1  by database procedure
                                   for hdb
                                   language sqlscript
                                   options read-only.
          declare i integer default 10;
          declare key int = 2;
          declare val varchar(20) default 'H2O';
          declare mytable table(a int) default select 1 a from dummy union all select 2 a from dummy;
          declare lt table(id int, city varchar( 32 ), date date);
          declare a int;
          declare b varchar(32);
          declare c date;
          declare d int default 10;
          declare arr int array;
          DECLARE CONTINUE HANDLER FOR SQLEXCEPTION BEGIN -- Catch the exception
             SELECT ::SQL_ERROR_CODE, ::SQL_ERROR_MESSAGE FROM DUMMY;
          END;

      -- single expression on the left-hand side
          IF :i IN (1, 2, 3, 6, 8, 11, 12, 100) THEN i = :i + 1; END IF;

      -- multiple expressions on the left-hand side
          IF (:key, :val) NOT IN ((1, 'H2O'), (2, 'H2O'), (3, 'abc'), (5, 'R2D2'), (6, 'H2O'), (7, 'H2O')) THEN
              key = 100; val = 'Mine';
          END IF;

      -- subquery on the right-hand side
          IF :i NOT IN (SELECT a FROM :mytable) THEN i = :i - 10; END IF;

      -- subquery using table variable
          IF (:a, :b, :c) IN (SELECT id, city, date from :lt where id < :d) THEN
             a = null; b = 'Toronto'; c = '00000000';
          END IF;

      -- subquery using table function
          FOR i IN 1 .. CARDINALITY(:arr) DO
              IF :arr[:i] IN (1, 2, 3) THEN
                  break;
              end if;
          end for;
      endmethod.

      method exists_1  by database procedure
                                   for hdb
                                   language sqlscript
                                   options read-only.
          declare i integer default 10;
          declare mytable table(a int) default ( select 1 a from dummy union all select 2 a from dummy);
          IF i < 3 and EXISTS (SELECT * FROM :mytable WHERE a = 1) THEN
             i = 2;
          END IF;

          IF NOT EXISTS (SELECT * FROM :mytable WHERE a in (1, 2)) THEN
              i = 4;
          END IF;

      endmethod.

      method cursor_1 by database procedure
                                   for hdb
                                   language sqlscript
                                   options read-only
                                   using zflights
                                   ZCL_AMDP_FLOW_TEST=>GET_FLIGHTS_PROC.
         declare init_date constant date default '00000000';
         declare l_date date default init_date;

         DECLARE CURSOR cur FOR SELECT * from zflights;
         declare output row like :ot;
         declare l_int int;
         declare l_char char( 10 );
         declare l_char2 varchar( 10 );
         declare l_char3 nvarchar( 10 );
         declare l_dec decimal;

         declare mytab table(a int) = select 1 a from dummy;
         DECLARE i INT;
         DECLARE CURSOR mycur WITH HOLD FOR SELECT * FROM :mytab;

         begin
            declare in_carrid varchar(3) default 'LH';
            call "ZCL_AMDP_FLOW_TEST=>GET_FLIGHTS_PROC"(
               in_carrid  => :in_carrid,
               et_flights => :et_flights
            );
         end;

         OPEN mycur;
         -- ROLLBACK no support in read-only;
         FETCH mycur INTO i;
         CLOSE mycur;
         SELECT :i as i FROM DUMMY;

         if 1 = 1 then
           declare ot_1 table like :ot;
          -- implicit curso
          for aflight as cur do
            :ot.(carrid, connid, fldate).insert((:aflight.carrid, :aflight.connid, :aflight.fldate));
            if cur::rowcount >= 4 then
                break;
            end if;
          end for;
         else
         -- explicit cursor
            open cur;
            loop
              declare aflight row like :cur;
              declare lv_var auto = 100;
              fetch cur into aflight;
              if cur::NOTFOUND then
                 break;
              end if;
              l_char = :aflight.connid; l_char2 = :aflight.carrid; l_date = :aflight.fldate;

              :ot.(carrid, connid, fldate).insert((:aflight.carrid, :aflight.connid, :aflight.fldate));
              if cur::rowcount > 3 then
                break;
              end if;
            end loop;

            if not cur::isclosed then
               close cur;
            end if;
         end if;
      endmethod.

      METHOD CONdition_1 by database procedure
                                   for hdb
                                   language sqlscript
                                   options read-only.
        declare idx smallint default 0;
        declare result char;
        idx = null;
        if idx is null then
          idx = 0;
        end if;
        IF :idx BETWEEN -100 AND 100 THEN
          result = 'X';
        ELSE
          result = 'O';
        END IF;

      endmethod.

      method loop_1 by database procedure
                                   for hdb
                                   language sqlscript
                                   options read-only.
        declare idx, sum int;

        sum = 0;
        for idx in 1..10 do
           declare lv_var date;
           sum = :sum + idx;
        end for;
        FOR idx IN 0 .. 10 DO
          IF :idx < 3 THEN
              CONTINUE;
          END IF;
          IF :idx = 5 THEN
              BREAK;
          END IF;
        END FOR;

        idx = 1; sum = 0;
        while idx > 15 do
            declare lv_var auto = 10;
            sum = :sum + 1 + :lv_var;
            idx = :idx + 1;
            if :idx = 5 then
              break;
            end if;
         end while;

         idx = 1;
         WHILE :idx < 5 DO
             declare v_index2 auto = 0;
             WHILE :v_index2 < 5 DO
                 v_index2 = :v_index2 + 1;
             END WHILE;
             idx = :idx + 1;
        END WHILE;

         loop
            declare lv_var int default 100;
            if idx = 6 then
               continue;
            elseif idx = 7 then
               idx = :idx + 1;
               continue;
            end if;

            sum = :sum + idx + :lv_var;
            idx = :idx + 1;
            if idx > 10 then
               break;
            end if;
         end loop;
      endmethod.

      method define_1 by database procedure
                                   for hdb
                                   language sqlscript
                                   options read-only.
         declare init_date constant date default '00000000';
         declare l_date date default init_date;
         declare l_date2 auto = init_date;
         declare tab2 table(k varchar(20), v int) = select 'Tag' k, 1 v from dummy;

         declare tab_key table(k integer primary key, v varchar(20) not null);
         declare tab_key2 table(k1 int, k2 int, v varchar(20) not null, primary key( k1, k2));
         declare tab_key3 table(k1 int not null, k2 int, v varchar(20) not null) search key(k1);
         declare tab_key4 table(k1 int, k2 int, v varchar(20) not null) search key( k1, k2);
         declare l_int int;
         declare l_key int;
         declare l_char char( 10 );
         declare l_char2 varchar( 10 );
         declare l_char3 nvarchar( 10 );
         declare l_dec decimal;
         DECLARE lt TABLE(key int not null, count int) SEARCH KEY(key);
         DECLARE search_result int;

         declare line_num int = ::CURRENT_LINE_NUMBER;

         begin
            declare filter varchar(100);
            filter = 'k like ''Tag%''';
            t2 = apply_filter(:tab2, :filter);
         end;

         begin
            l_key = 2;
            search_result = :lt.SEARCH(key, l_key);
            BEGIN AUTONOMOUS TRANSACTION -- ACID
               if is_empty( :lt ) then
                  search_result = 1;
                  :lt.INSERT((l_key, 0), search_result);
               elseIF search_result < 0 THEN
                  search_result = 0 - :search_result;
                  :lt.INSERT((l_key, 0), search_result);
               end if;
               lt.count[search_result] = :lt.count[search_result] + 1;
            end;
         end;

         begin
            :tab2.(k, v).insert(('1', 100));
            --:tab_key.insert((1, '1'));
            :tab_key3.insert((1, 2, 'Test'));
            :tab_key4.insert((1, 2, 'Test'));
            l_int = :tab_key3.search((k1, k2), (1, 2) );
         end;

         l_int = 10; l_char = 'mine';

         select current_date into l_date from dummy;
      endmethod.

      method rank_1 by database procedure
                             for hdb
                             language sqlscript
                             options read-only.
              declare ProductSales table(ProdName VARCHAR(50), Type VARCHAR(20), Sales INT);
 
              INSERT INTO :ProductSales VALUES('Tee Shirt','Plain',21); 
              INSERT INTO :ProductSales VALUES('Tee Shirt','Lettered',22); 
              INSERT INTO :ProductSales VALUES('Tee Shirt','Team logo',30); 
              INSERT INTO :ProductSales VALUES('Hoodie','Plain',60); 
              INSERT INTO :ProductSales VALUES('Hoodie','Lettered',65); 
              INSERT INTO :ProductSales VALUES('Hoodie','Team logo',80); 
              INSERT INTO :ProductSales VALUES('Ballcap','Vintage',60);
              INSERT INTO :ProductSales VALUES('Ballcap','Plain',8); 
              INSERT INTO :ProductSales VALUES('Ballcap','Lettered',40); 
              INSERT INTO :ProductSales VALUES('Ballcap','Team logo',40); 
              lt_data = SELECT ProdName, Type, Sales, 
                            RANK() OVER (PARTITION BY ProdName ORDER BY Sales DESC) AS Rank, 
                            DENSE_RANK() OVER (PARTITION BY ProdName ORDER BY Sales DESC) AS dense_rank, 
                            ROW_NUMBER() OVER (PARTITION BY ProdName ORDER BY Sales DESC) AS row_num
                            FROM :ProductSales
                            ORDER BY ProdName, Type;
              begin
                  declare tab table(COL1 DOUBLE, COL2 DOUBLE);
                  INSERT INTO :Tab VALUES(900, 10); 
                  INSERT INTO :Tab VALUES(400, 50); 
                  INSERT INTO :Tab VALUES(700, 30); 
                  INSERT INTO :Tab VALUES(200, 40);
                  SELECT NTH_VALUE (COL1, 2 ORDER BY COL2) FROM :Tab; -- COL1 = 700                  
                end;  
            endmethod.
             
            method function_1 by database procedure
                             for hdb
                             language sqlscript
                             options read-only.
                declare l_str1, l_str2, l_str3, l_str4 varchar(30);
                declare l_in1, l_in2 integer = 0;
                declare l_date1, l_date2 date;
                declare l_timestamp1, l_timestamp2 timestamp;
                declare arrs_1, arrs_2 varchar(30) array;
                declare l_b1, l_b2 binary(64);
                declare l_alphanum1, l_alphanum2 alphanum(20) default '123';
                declare l_bool1, l_bool2 boolean;
                declare l_double1, l_double2 double;
                declare l_vb1, l_vb2 varbinary(16);
                 
                select current_date, current_timestamp into l_date1, l_timestamp1 from dummy;
                l_str1 = INITCAP('Hello SQLScript World');
       
                -- left, right, substring, concat, replace, length
                l_str2 = left( l_str1, 3 );
                l_str2 = replace(l_str1, 'o', '0');
                l_str3 = concat( l_str1, l_str2 );
                l_str3 = concat_naz( l_str1, l_str2 ); 
                l_str3 = trim(l_str1);
                l_str3 = IFNULL(:l_str1, :l_str2);
                
                l_str3 = trim(leading '0' from l_str1);
                l_str3 = trim(trailing '$' from l_str1);
                l_str3 = trim(both '#' from l_str1);
                l_str4 = concat_naz( l_str1, null );  -- l_str4 = l_str1
                l_str2 = right( l_str1, 4 );
                l_str2 = substring(l_str1, 2, 4);
                l_in1 = length(l_str1);
                l_str2 = LPAD(:l_str1, 15, '=====');
                l_str2 = RPAD(:l_str1, 15, '12345');
                l_str3 = RTRIM('endabAabbabab','ab');
                l_str3 = LTRIM('####endabAabbabab','##');
                l_str4 = GENERATE_PASSWORD(32);  -- varchar(128)
                
                l_str2 = ESCAPE_DOUBLE_QUOTES(:l_str1);
                l_str2 = ESCAPE_SINGLE_QUOTES(:l_str1);
                l_in2 = IS_SQL_INJECTION_SAFE(:l_str2);
                l_str3 = ISOWEEK(TO_DATE('2011-05-30', 'YYYY-MM-DD')); --2011-W22
                 
                l_str4 =  GREATEST('aa', 'ab', 'ba', 'bb'); -- bb
                
                l_vb1 = HASH_MD5(to_binary('abcd'));
                l_vb2 = HASH_SHA256(TO_BINARY('database'));
                l_vb1 = HEXTOBIN('608da975');
                 
                l_in1 = HEXTONUM('0xffff');  
                l_in1 = greatest( 100, 200, 10, 23 );   -- 200
                
                 
                -- 2014-04-01 -> 01/04/2014  
                l_str2 = REPLACE_REGEXPR('([[:digit:]]{4})-([[:digit:]]{2})-([[:digit:]]{2})' IN '2014-04-01' 
                                          WITH '\3/\2/\1' OCCURRENCE ALL);  
              
                -- ascii, coalesce
                l_in1 = ASCII(l_str1);  -- ascii('H')
                l_str3 = coalesce(l_str1, l_str2);
                arrs_1[1] = 'Toronto'; arrs_1[2] = 'Chichago'; arrs_1[3] = 'Tokoyo';
                
                -- cast
                l_in1 = cast('12345' as integer);
                l_str2 = cast(l_date1 as varchar(20));
                l_alphanum1 = cast( l_str2 as alphanum );
                l_str2 = cast(to_decimal(1234.899, 8, 2) as varchar(10)); 
       
                -- abap_alphanum, abap_numc
                l_str2 = abap_alphanum('12345', 10 );   -- 0000012345
                l_str2 = abap_alphanum( 'x12345', 10 );   -- x12345
                l_str2 = abap_numc( 12, 3 );  -- 012
                l_str2 = abap_numc( 1234, 3 ); -- 234
                
                -- conversion
                l_alphanum1 = to_alphanum(l_str2);
                l_bool1 = to_boolean('trUE');
                l_date1 = TO_DATS('2020-01-05');  -- ABAP Date string 20200105
                l_date1 = TO_DATE('2020-01-05', 'YYYY-MM-DD');
                l_str2 = cast(to_decimal(12345.671, 10, 2) as varchar(20));
                l_double1 = to_double('12.33');  
                l_in2 = to_int('10');
                l_in2 = to_integer('1023.30');
                l_str2 = cast(TO_FIXEDCHAR('Ant', 2) as varchar(30));
                l_str2 = TO_VARCHAR(TO_DATE('2009/12/31'), 'YY-MM-DD');  
                l_str2 = TO_NVARCHAR(TO_DATE('2009/12/31'), 'YY-MM-DD');  
                arrs_2 = trim_array(:arrs_1, 1);
                l_str2 = numtohex(:l_in1); -- integer to hex varchar string 
                
                
                l_in2 = OCCURRENCES_REGEXPR('([[:digit:]])' flag 'i' IN 'a1b2'); -- 2
                
                  
                -- abap_lower, abap_upper, ucase, lcase
                l_str2 = lcase(l_str1);
                l_str2 = ucase(l_str1);
                l_str2 = lower(l_str1);
                l_str2 = UPPER(l_str1); 
                l_str2 = abap_lower(l_str1);
                l_str2 = abap_upper(l_str1);
                 
                -- case
                l_str2 = case l_str1
                            when '1' then 'one'
                            when '2' then 'two'
                            else 'unknown'
                         end;
                l_str2 = case when l_in1 > 0 then 'Positive'
                              when l_in1 = 0 then 'Zero' 
                              when l_in1 < 0 then 'Negative'
                              else 'nunknown'
                          end;             
                
                -- math
                l_in2 = mod(:l_in1, 2);
                l_in2 = sign(:l_in1);
                l_in2 = power(10, 2);
                l_in2 = round(10.20, 1, ROUND_HALF_DOWN);
                l_double2 = exp(1.0);
                l_in2 = ceil(:l_double1);
                l_in2 = floor(:l_double1);
                
                l_vb1 = NEWUID();  -- UID, SYSUUID
                
                l_in2 = UMINUS(765); -- -765 
                l_in2 = abs(:l_in2);
                l_in2 = UNICODE('#');
                l_str3 = nchar(65);
                
                l_double1 = NDIV0(100, 10); -- return 10.00
                l_in1 = NDIV0(100, 0); -- return 0
                select ndiv0(1, 0) as zero into l_str3 from dummy;   
                
                -- Returns a pseudo-random DOUBLE value in the range of 0 to less than 1.0. 
                l_double1 = rand();
                l_double1 = RAND_SECURE();
                 
                   
                -- to_date, add_days, add_months, add_years
                l_timestamp1 = now( );
                
                l_date1 = TO_DATE ('2020-01-05', 'YYYY-MM-DD');
                l_date2 = ADD_DAYS(:l_date1, 10);
                l_date2 = add_months(:l_date1, 2);
                l_date2 = add_months_last(:l_date1, 2);
                l_date2 = add_months_last(TO_DATE('2009-02-28', 'YYYY-MM-DD'), 1); -- 03/01/2009
                l_timestamp2 = add_seconds(:l_timestamp1, 60); 
                -- factory calendar table SAP<SID>.TFACS
                l_date2 = ADD_WORKDAYS('CA', '2014-01-09', 1, 'FCTEST'); -- FCTEST.TFACS
                l_in2 = WORKDAYS_BETWEEN('CA', '2014-01-09', '2014-01-10' , 'FCTEST'); 
             
                l_str3 = dayname(:l_date1);
                l_str3 = monthname(:l_date1);
                
                l_in1 = dayofmonth(:l_date1);
                l_in1 = dayofyear(:l_date1);
                
                l_in1 = year(:l_date1);
                l_in1 = month(:l_date1);
                --l_in1 = day(:l_date1);
                l_str2 = quarter(:l_date1);
                l_in1 = hour(:l_date1);
                l_in1 = minute(:l_date1);
                l_in1 = second(:l_date1);
                  
                l_date2 = add_years(:l_date1, 2); 
                l_in1 = years_between(:l_date1, :l_date2);
                l_in1 = months_between(:l_date1, :l_date2);
                l_in1 = days_between(:l_date1, :l_date2);
                l_in1 = seconds_between(:l_date1, :l_date2);
                l_double1 = NANO100_BETWEEN(:l_date1, :l_date2);
                
                -- the last day of the month 
                l_date2 = last_day(:l_date1);
                -- Next day
                l_date2 = NEXT_DAY(:l_date1); 
                
                l_date2 = nullif( :l_date1, :l_date2);
                -- EXTRACT( {YEAR | MONTH | DAY | HOUR | MINUTE | SECOND} FROM <date> )
                l_in2 = extract(year from :l_date1);
                
                begin
                  declare l_TIMESTAMP TIMESTAMP; 
                  declare l_seconddate seconddate;
                  declare l_time1 time;
                  l_seconddate = TO_SECONDDATE ('2010-01-11 13:30:00', 'YYYY-MM-DD HH24:MI:SS'); 
                  l_time1 = TO_TIME ('08:30 AM', 'HH:MI AM');
                  l_TIMESTAMP1 = TO_TIMESTAMP ('2010-01-11 13:30:00', 'YYYY-MM-DD HH24:MI:SS');
                  l_TIMESTAMP1 = UTCTOLOCAL(:l_TIMESTAMP1);
                end;
                
                -- week, Weeks start Monday and end Sunday
                l_in1 = week(l_date1);
                
                -- weekday representing Monday(0) through to Sunday(6).
                l_in1 = WEEKDAY(l_date1);
                
                -- binary
                l_b1 = to_binary( 'This' );
                             
        endmethod.
ENDCLASS.