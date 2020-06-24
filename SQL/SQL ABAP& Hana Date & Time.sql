/**
    Purpose: show ABAP and HANA date/time conversion and some date functions
    Author : Simon Li
    Date   : April 10, 2020  
    ABAP:  YYYYMMDD, HHmmSS, YYYYMMDDHHmmSS
    HANA:  YYYY-MM-DD, HH:MI:SS, YYYY-MM-DD HH:MI:SS.nnnnnnnnn
*/
do begin
     declare abap_date nvarchar(8) = to_dats(current_date);
     --declare abap_time nvarchar(6) = replace(cast(current_time as nvarchar(8)), ':', '');
     declare abap_time nvarchar(6) = REPLACE_REGEXPR('([[:digit:]]{2}):([[:digit:]]{2}):([[:digit:]]{2})' IN current_time 
                                     WITH '\1\2\3' OCCURRENCE ALL); 
     declare abap_timestamp nvarchar(14) = REPLACE_REGEXPR('([[:digit:]]{4})-([[:digit:]]{2})-([[:digit:]]{2}) ([[:digit:]]{2}):([[:digit:]]{2}):([[:digit:]]{2}).*' 
                                       IN cast(current_timestamp as nvarchar(20)) 
                                       WITH '\1\2\3\4\5\6' OCCURRENCE ALL);
     
     declare hana_date date default to_date('20191003');
     declare hana_time time default to_time('102153');
     declare hana_timestamp timestamp = TO_TIMESTAMP (:abap_date || :abap_time, 'YYYYMMDDHH24MISS'); 
     
     select :abap_date "Abap Date", :abap_time "Abap Time", 
            :hana_date "HANA Date", :hana_time "HANA Time", 
            :abap_timestamp "Abap TimeStamp", :hana_timestamp "HANA Timestamp" from dummy;
     
     select dayofmonth(current_date) "Day", dayname(current_date) "DayName", WEEKDAY(current_date) "WeekDay",
       year(current_date) || week(current_date) "YearWeek",
       dayname('2010-11-02') as "DayName", hour('19:09:10') as "Hour",
       to_dats('2019-11-21') "ABAP_Date", replace(cast(current_time as nvarchar(8)), ':', '') as "ABAP_Time",
       REPLACE_REGEXPR('(\d\d):(\d{2}):(\d{2})' IN current_time with '\1\2\3' occurrence all) as "ABAP_Time2", 
       year(current_date) as "Year", month(ADD_DAYS(current_date, 50)) "AddDays", MONTHNAME(ADD_MONTHS(now(), 19)) "MonthName\",  
       hour(current_time) as "Hour", minute(current_time) as "Min", second(current_time) "Sec", 
       add_days(last_day(add_months(current_date, -1)),1) "FirstDay",
       TO_DATE(year(current_date) || '-' || month(current_date) || '-01') as "FirstDay2", 
       next_day(last_day(add_months(current_date, -1))) "Firstay3",
       LAST_DAY(current_date) as "LastDay",
       to_dats(current_date) as "ABAP_Date" from dummy;
end;   
