/**
    Purpose: SQL string functions
    Author : Simon Li
    Date   : April 10, 2020
*/
do begin
   declare l_s auto = 'Test s123 h467';
   select NEWUID() as id, trim(:l_s) as f0, left(:l_s, 5) as t1, length(:l_s) as "len", 
          right(rtrim(:l_s), 6) f2, substring(:l_s, 2, 4) f3, 
          ltrim(substring(:l_s, 2)) f4, 
          CONCAT(:l_s, '$null') as f5, CONCAT(:l_s, null) as "f5.1", CONCAT_NAZ(:l_s, 'null') as "f5.1", 
          CONCAT_NAZ(:l_s, null) as "f5.2", :l_s || '$Test' as "f5.3", :l_s || null as "f5.4",
          COALESCE(null, :l_s, 'Test', 'T2') as "Coalesce", IFNULL(null, 'Test') as "ifnull",
          case left(:l_s,1)
             when 'T' then '<Test>'
             when 'E' then 'never'
             else 'Unknown'
          end as "Simple Case",
          case when :l_s like 'Test%' then 'Option 1'
               when :l_s like 'T_est' then 'Option 2'
               else 'Others'
          end "Select Case",
          lpad(trim(left(:l_s, 5)), 10, '=') f6, rpad(:l_s, 15, '*') as f7,
          ucase(:l_s) as "u1", lcase(:l_s) as "l1", initcap(substring(:l_s, 2)) as "cap",
          replace(:l_s, 'e', '') as "re",
          REPLACE_REGEXPR('(\w+)\s(.*)' IN :l_s with 'Mine\2') as "re2",
          LOCATE(:l_s, 'T', 1) as "loc",  LOCATE(:l_s, 'Test', -1) as "loc_back",
          ABAP_NUMC(:l_s, 10) as "abap_num", abap_alphanum('1234', 10) as "abap_num2",
          abap_lower(:l_s) as "abap_l", abap_upper(:l_s) as "abap_u"
          from dummy;
end;
     