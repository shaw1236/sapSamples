** ABAP Managed Database Procedure and CDS Table Function 
**
** Purpose: Sample to AMDP and CDS view
**
** Author : Simon Li  Jul 2018
**
CLASS zcl_test_amdp_helper DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    types: begin of ty_termination,
           mandt      type jkap-mandt,
           vbeln      type jkap-vbeln,
           posex      type jkap-posex,
           posnr_ur   type jkap-posnr_ur,
           start_date type jkap-gueltigvon,
           posnr      type jkap-posnr,
         end of ty_termination,
         ty_ytermination_tab type standard table of ty_termination. " with non-unique key mandt vbeln posex.

    types:  begin of ty_order_status,
           mandt    TYPE s_mandt,
           vbeln    TYPE avnr,       "Order
           posex    TYPE posex_isp,  "ItemEx
           posnr_ur TYPE apnr_ur,    "Original item 1:1 -> ItemEx
           posnr    TYPE apnr,       "ItemInt for linkage
           status   TYPE char1,      "Status
           delta    TYPE char1,      "delta change flag
         end of ty_order_status,
         ty_order_status_tab TYPE standard table of ty_order_status.

    INTERFACES if_amdp_marker_hdb. " introduce AMDP for Hana 

    class-methods: get_payment_document  for table function ztf_payment_document,
                   get_order_status_data for table function ztf_order_status_data.
                 
  " Test function table "ztf_order_status_data"
  "   1. se16n
  "   2. pass the parameters
  "      select * from ztf_order_status_data(p_client = @sy-mandt, p_keydate = @sy-datum)
  "        into @data(lt_order_status). 
  "   3. just direct openSQL call as p_client and p_keydate have been defaulted
  "      select * from ztf_order_status_data
  "        into @data(lt_order_status).
  "   for 2 & 3:
  "    loop at lt_order_status assigning field-symbols(<order_status>).
  "      write: / <order_status>-vblen, <order_status>-posex, <order_status>-status.
  "    endloop.
    class-methods: get_order_status_proc
                      importing value(p_client)        type s_mandt
                                value(p_keydate)       type sydatum
                                value(p_inc_delta)     type abap_bool default abap_true
                      exporting value(et_order_status) type ty_order_status_tab.
  " Test procedure "get_order_status_proc"
  "    try.
  "      zcl_test_amdp_helper=>get_order_status_proc( exporting
  "                                                     p_client  = sy-mandt
  "                                                     p_keydate = sy-datum
  "                                                   importing 
  "                                                     et_order_status = data(lt_order_status)
  "                                                 ).
  "    " RAISING CX_AMDP_ERROR
  "      catch cx_amdp_error into data(lo_amdp_error).
  "        data(lv_text) = lo_amdp_error->get_text().
  "    endtry.

    class-methods get_termination_per_keydate
                    importing value(i_client)     type s_mandt
                              value(i_keydate)    type sydatum
                              value(i_offset)     type int4 default 1825   " 365 * 5 days 
                    exporting value(et_term_data) type ty_termination_tab.

    class-methods get_payment_document_single
                    importing value(i_client) type s_mandt
                              value(i_opbel) type opbel_kk
                    exporting value(e_opbel) type opbel_kk
                              value(e_blart) type blart_kk
                              value(e_budat) type budat_kk.

    class-methods check_termination
                    importing value(i_client) type s_mandt
                              value(i_vbeln)  type avnr
                              value(i_posex)  type posex_isp
                              value(i_date)   type sydatum
                    exporting value(result)   type abap_bool.

    class-methods check_suspension
                    importing value(i_client) type s_mandt
                              value(i_vbeln)  type avnr
                              value(i_posex)  type posex_isp
                              value(i_date)   type sydatum
                    exporting value(result)   type abap_bool.

    class-methods check_redirection
                    importing value(i_client) type s_mandt
                              value(i_vbeln)  type avnr
                              value(i_posex)  type posex_isp
                              value(i_date)   type sydatum
                    exporting value(result)   type abap_bool.

    class-methods check_start
                    importing value(i_client) type s_mandt
                              value(i_vbeln)  type avnr
                              value(i_posex)  type posex_isp
                              value(i_date)   type sydatum
                    exporting value(result)   type int1.

    class-methods check_end
                    importing value(i_client) type s_mandt
                              value(i_vbeln)  type avnr
                              value(i_posex)  type posex_isp
                              value(i_date)   type sydatum
                    exporting value(result)   type abap_bool.

  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.

CLASS zcl_test_amdp_helper IMPLEMENTATION.
  method get_termination_per_keydate by database procedure 
                                     for hdb
                                     language sqlscript
                                     options read-only
                                     using jkap jkkuend.
    declare lv_start_date date := '00000000';
    lv_start_date := i_keydate - i_offset;

    -- BEZUGGRD  - Purchase Reason
    -- UNTBRGRD  - Reason for Suspension 
    -- KUENDGRD  - Reason for end of delivery
    lt_term_r = 
      select _jkap.mandt,
             _jkap.vbeln,
             posex,
             posnr_ur,
             gueltigvon as start_date,
             _jkap.posnr 
      from jkap as _jkap inner join jkkuend as _term
      on _jkap.mandt = _term.mandt and
         _jkap.vbeln = _term.vbeln and
         _jkap.posnr_ur = _term.posnr 
      where _jkap.mandt     = :i_client  " Specific client
        and poart           = 'R'        " A Rediection
        and _jkap.kunedgrd  = 'T01'      " Reason for end of delivery
        and xkuend_akt      = 'X'
        and dats_days_between(gueltigbis, liefendei) <= 7  " Tight validation, will be terminated within a week
        and gueltigvon between :lv_start_date and :i_keydate;

    lt_term_tmp =  
      select _term.mandt,
             _term.vbeln,
             _jkap.posex,
             _jkap.posnr_ur,
             liefendei as start_date,
             max(_jkap.posnr) as posnr
      from jkkuend as _term inner join jkap as _jkap
        on _term.mandt = _jkap.mandt and
           _term.vbeln = _jkap.vbeln and
           _term.posnr = _jkap.posnr_ur and
           _term.kuendgrd = _jkap.kuendgrd and -- end delivery reason, might be 1:n 
           _jkap.poart in ('NP', 'KP', 'PP')
      where _term.mandt = :i_client
        and xkuend_akt  = 'X'
        and liefendei between :lv_state_date and :i_keydate                             
      group by _term.mandt, _term.vbeln, _jkap.posex, _jkap.posnr_ur, liefendei;

    et_term_data = 
      select * from :lt_term_r
      union
      select * from :lt_term_tmp;
      order by mandt, vbeln, posex; 

    --select * into et_term_data        
    --  from :lt_term_data
    --  order by mandt, vbeln, posex; 
  endmethod.

  method get_payment_document by database function 
                              for hdb
                              language sqlscript
                              options read-only
                              using dfkkko dfkkop.
       --define table function ztf_get_payment_document
       --returns {
       --           key client: s_mandt;
       --           key opbel:  opbel_kk;
       --               gpart:  gpart_kk;
       --               order:  avnr;
       --               augbl:  augbl_kk;
       --               blart:  blart_kk;
       --               budat:  budat_kk;
       --         }

       -- Get the cleared documents                       
       lt_cleaned = select mandt as client,
                           opbel,
                           gpart,
                           --substring( vtref, 10, 10 ) as vtref,
                           right(vtref, 10) as order,
                           augbl,
                           blart,
                           budat,
                           augrd
                  from dfkkop
                  where augst = '9'   -- Cleared Item only
                    and augrd in ('01', '08', '15');

       -- case 1 for cleared reason '01'
       lt_dataset1 = select
                        _augrd01.client,
                        _augrd01.opbel,
                        _augrd01.gpart,
                        _augrd01.order,
                        _augrd01.augbl,
                        _ko.blart,
                        _ko.budat
                     from :lt_cleaned as _augrd01 inner join dfkkko as _ko
                       on _augrd01.client = _ko.mandt and
                          _augrd01.augbl = _ko.opbel
                     where augrd = '01';
       
       -- case 2 for cleared reason '08' or '15'
       lt_dataset_tmp = select
                        _augrd08.client,
                        _augrd08.opbel,
                        _augrd08.gpart,
                        _augrd08.order,
                        _op.opbel as augbl
                     from :lt_cleaned as _augrd08 inner join dfkkop as _op
                       on _augrd08.client = _op.mandt and
                          _augrd08.augbl  = _op.augbl
                       where _augrd08.augrd in ('08', '15')
                         and _op.opbel not in (select opbel from :lt_cleaned);

       lt_dataset2 = select
                        _tmp.client,
                        _tmp.opbel,
                        _tmp.gpart,
                        _tmp.order,
                        _tmp.augbl,
                        _ko.blart,
                        _ko.budat
                     from :lt_dataset_tmp as _tmp inner join dfkkko as _ko
                       on _tmp.client = _ko.mandt and
                          _tmp.augbl = _ko.opbel
                     where _ko.herkf IN ('05', '06', '25', '55');

       return
             select * from :lt_dataset1
             union all
             select * from :lt_dataset2;
    endmethod.

    method get_order_shipto_addresses by database function for hdb
                                      language sqlscript
                                      options read-only
                                      using jkvap jgtadra jgtsadr.
      lt_addr_general =
        select _addr.mandt,
               _addr.gpnr,    -- BP
               _addr.jparvw,  -- Role
               max(adrnr) as role_adrnr_max
        from jgtadra as _addr inner join jkvap _jk
          on _addr.mandt = _jk.mandt and
             _addr.gpnr  = _jk.kunwe
        where _addr.mandt = :p_client
          and _jk.vbtyp = 'C'  -- Order
          and _jk.trvog = '0'  -- Subscription order
          and _jk.drerz in ('GAM', 'NY', 'GOP', 'GIG')
          and _addr.jparvw in ('WE', 'GP')  -- need GP and WE roles
          and _addr.wdat1 <= :p_keydate
          and _addr.wdat2 >= :p_keydate
        group by _addr.mandt, gpnr, jparvw; -- unique BP and role

      lt_addr_slice =
        select --distinct
               _gp_addr.mandt,
               _gp_addr.gpnr,
               coalesce(_sh_addr.jparvw, _gp_addr.jparvw) as jparvw,
               coalesce(_sh_addr.role_adrnr_max, _gp_addr.role_adrnr_max) as adrnr
        from :lt_addr_general as _gp_addr left outer join :lt_addr_general as _sh_addr
          on _gp_addr.mandt   = _sh_addr.mandt and
             _gp_addr.gpnr    = _sh_addr.gpnr and
             _sh_addr.jparvw  = 'WE' -- link to a "WE" address
        where _gp_addr.jparvw = 'GP'; -- The drive record is "GP" record

      return
        select
               _t2.mandt as client,
               _slice.gpnr as SAP_BP_ID,
               _t1.adrnr as ADDRESS_NO,
               _t2.stras as STREET_NAME,
               _t2.hausn as SAP_HOME_NUMBER,
               _t2.hsnmr2 as SAP_HOUSE_NO_AFFIX,
               _t2.street2 as SAP_STREET1,
               _t2.ispadrbsnd as SAP_STREET2,
               _t2.addrsecnumber as SAP_SUITE_NUMBER,
               _t2.erfdate as SAP_AD_CREATED_ON,
               _t2.erftime as SAP_AD_CREATED_AT,
               _t2.erfuser as SAP_AD_CREATED_BY,
               _t2.aendate as SAP_AD_CHANGED_ON,
               _t2.aentime as SAP_AD_CHANGED_AT,
               _t2.aenuser as SAP_AD_CHANGED_BY,
               _t2.addrsecabbrev as SAP_SEC_ADD_TEXT,
               _t2.addrsecnumber as SAP_SEC_ADD_NO,
               _t2.stock as SAP_BUILDING_IDENTITY,
               _t2.ort01 as SAP_CITY,
               _t2.ort02 as SAP_DISTRICT,
               _t2.regio as SAP_PROVINCE_CODE,
               _t2.pstlz as SAP_POSTAL_CODE,
               concat('AF', substring(_t2.pstlz, 1, 3))  as SAP_GEO_FSA_MARKET,
               _t2.land1 as SAP_COUNTRY_CODE,
               substring(_t2.ani_telnrp, 3, 25) as SAP_PHONE_NUM,
               _t2.isphandy as SAP_CELL_NUM,
               _t2.ispemail as SAP_EMAIL_ADDRESS,
               _t2.telfx as SAP_FAX_NUMBER,
               _t2.pfach as SAP_PO_BOX,
               _t2.pstl2 as SAP_PO_BOX_POSTAL_CODE,
               _t2.ortpf as SAP_PO_BOX_CITY,
               _t2.txjcd as SAP_TAX_JUR,
               _t2.predirectional as SAP_PREDIRECTION,
               _t2.postdirectional as SAP_POSTDIRECTION,
               _t2.adrzus2 as SAP_ADDR_AFFIX,
               _t1.jparvw as ROLE,
               _t1.wdat1 as VALID_FROM,
               _t1.wdat2 as VALID_TO
        from :lt_addr_slice as _slice
           inner join jgtsadr as _t2
             on _slice.mandt = _t2.mandt and
                _slice.adrnr = _t2.adrnr
           inner join jgtadra as _t1
             on _slice.mandt  = _t1.mandt and
                _slice.gpnr   = _t1.gpnr and
                _slice.jparvw = _t1.jparvw and
                _slice.adrnr  = _t1.adrnr
        order by _slice.gpnr;
    endmethod.

    method get_address by database function for hdb
                          language sqlscript
                          options read-only
                          using jgtadra jgtsadr.
      lt_addr_general =
        select mandt,
               gpnr,    -- BP
               jparvw,  -- Role
               max(adrnr) as role_adrnr_max
        from jgtadra
        where jparvw in ('WE', 'GP')  -- need GP and WE roles
          and wdat1 <= :p_keydate
          and wdat2 >= :p_keydate
        group by mandt, gpnr, jparvw;

      lt_addr_slice =
        select --distinct
               _gp_addr.mandt,
               _gp_addr.gpnr,
               case -- use ship-to address if we have, otherwise use GP address
                 when _sh_addr.role_adrnr_max is not null then _sh_addr.jparvw
                 else _gp_addr.jparvw
               end as jparvw,
               case -- use ship-to address if we have, otherwise use GP address
                 when _sh_addr.role_adrnr_max is not null then _sh_addr.role_adrnr_max
                 else _gp_addr.role_adrnr_max
               end as adrnr
        from :lt_addr_general as _gp_addr left outer join :lt_addr_general as _sh_addr
          on _gp_addr.gpnr = _sh_addr.gpnr and
             _sh_addr.jparvw = 'WE'
        where _gp_addr.jparvw = 'GP';
      /*
      lt_addr_slice =
        select mandt,
               gpnr,
               max(adrnr) as adrnr_max
          from jgtadra
          group by mandt, gpnr;
      */

      return
        select _addr.mandt as client,
               _slice.gpnr as bp,
               _slice.jparvw as role,
               _addr.adrnr,
               _addr.name1,
               _addr.name2,
               substring(_addr.ani_telnrp, 3, 15) as phone,
               concat('AF', substring(_addr.pstlz, 1, 3))  as fsa,
               wdat1 as valid_from,
               wdat2 as valid_to
        from jgtsadr as _addr
           inner join :lt_addr_slice as _slice
             on _addr.adrnr = _slice.adrnr
           inner join jgtadra as _addr_slice
             on _slice.gpnr = _addr_slice.gpnr and
                _slice.jparvw = _addr_slice.jparvw and
                _slice.adrnr = _addr_slice.adrnr
        order by _slice.gpnr;
    endmethod.
    
  method get_order_status_data by database function for hdb
                               language sqlscript
                               options read-only
                               using zcl_test_amdp_helper=>get_order_status_proc.
    -- mandt: mandt;
    -- vbeln: avnr;          -- Order
    -- posex: posex_isp;     -- ItemEx
    -- posnr_ur: apnr_ur;    -- Original item 1:1 -> ItemEx
    -- posnr: apnr;          -- ItemInt for linkage
    -- status: abap.char(1); -- Status
    -- delta: abap.char(1);  -- delta change flag
    -- Status:
    -- T: (T)ermination
    -- U: (S)uspension
    -- R: (R)estart
    -- S: (S)tart
    -- E: (E)nd
    -- N: (N)ormal

    -- Vacation Stop
    -- Suspension: R alone, R + U, U (vacation stop exempted)
    -- termination: R(reason: T01) + T, T (vacation stop exempted)
    call
     "ZCL_TEST_AMDP_HELPER=>GET_ORDER_STATUS_PROC"(
            p_client        => :p_client,
            p_keydaye       => :p_keydate,
            et_order_status => :lt_order_status );

    return :lt_order_status;
  endmethod.
  
  method get_order_status_proc by database procedure 
                               for hdb
                               language sqlscript
                               options read-only
                               using jkvap jkak jkap
                                     ZCL_TEST_AMDP_HELPER=>GET_TERMINATION_PER_KEYDATE.

    ----------------------------------------------------------------
    -- Termination --
    ----------------------------------------------------------------
    --declare lt_term_all table ty_termination_tab;                               
    call
     "ZCL_TEST_AMDP_HELPER=>GET_TERMINATION_PER_KEYDATE"(
            i_client     => :p_client,
            i_keydaye    => :p_keydate,
            et_term_data => :lt_term_all );
   
    -- Just ensure the dataset will be unique per vbeln/posex
    lt_term_set =
        select mandt,
               vbeln,
               posex,
               posnr_ur,
               posnr,
               'T' as status,   
               ' ' as delta
        from :lt_term_all
        where start_date = :p_keydate;

    ----------------------------------------------------------------
    -- Suspension/Redirection --
    ----------------------------------------------------------------
    lt_susp_set =
        select mandt,
               vbeln,
               posex,
               posnr_ur,
               posnr,
               --case poart
               --  when 'U' then 'U'  -- suspension
               --  when 'R' then 'R'  -- redirection
               --end as status,
               'U' as status,
               ' ' as delta
        from jkap as _jkap
        where mandt = :p_client
          -- suspensioon/redirection, R alone, R+U, U exception
          and ( poart = 'R' or poart = 'U' and zzposnr_ur = '000000' )
          and gueltigv/on = :p_keydate  -- suspension/redirection starts
          and not exists ( select posnr from :lt_term_all  -- already terminated
                           where mandt = _jkap.mandt
                             and vbeln = _jkap.vbeln
                             and posex = _jkap.posex
                         );

    ----------------------------------------------------------------
    -- Start/Restart --
    ----------------------------------------------------------------
    lt_start_set =
        select mandt,
               vbeln,
               posex,
               posnr_ur,
               posnr,
               case
                 when posnr = posnr_ur then 'S'  -- start
                 else 'R'                        -- restart
              end as status,
              ' ' as delta
        from jkvap as _jkap
        where mandt = :p_client
          and vbtyp = 'C'  -- Order
          and trvog = '0'  -- Subscription order
          and poart in ('NP', 'KP', 'PP')   -- normal item
          and gueltigvon = :p_keydate    " New subscription starts
          and not exists ( select posnr from :lt_term_all  -- already terminated
                           where mandt = _jkap.mandt
                             and vbeln = _jkap.vbeln
                             and posex = _jkap.posex
                         )
          and not exists ( select posnr from jkap as _ur  -- in the middle of suspension/redirection
                           where mandt = _jkap.mandt
                             and vbeln = _jkap.vbeln
                             and posex = _jkap.posex
                             and poart in ('R', 'U')
                             and gueltigvon <= :p_keydate
                             and gueltigbis >= :p_keydate 
                         );

    ----------------------------------------------------------------
    -- Natural End --
    ----------------------------------------------------------------
    lt_end_set =
        select mandt,
               vbeln,
               posex,
               posnr_ur,
               posnr,
               'E' as status,
               ' ' as delta
        from jkvap as _jkap
        where mandt = :p_client
          and vbtyp = 'C'  -- Order
          and trvog = '0'  -- Subscription order
          and poart in ('NP', 'KP', 'PP')   -- normal item
          and gueltigbis = :p_keydate    -- naturally ends
          and not exists ( select posnr from jkap      -- no further item afterwards
                             where mandt = _jkap.mandt 
                               and vbeln = _jkap.vbeln
                               and posex = _jkap.posex
                               and gueltigvon > _jkap.gueltigbis 
                         ) 
          and not exists ( select posnr from :lt_term_all  -- already terminated
                           where mandt = _jkap.mandt
                             and vbeln = _jkap.vbeln
                             and posex = _jkap.posex
                         )
          and not exists ( select posnr from jkap as _ur   -- in the middle of suspension
                           where mandt = _jkap.mandt
                             and vbeln = _jkap.vbeln
                             and posex = _jkap.posex
                             and poart in ('R', 'U')
                             and gueltigvon <= :p_keydate
                             and gueltigbis >= :p_keydate 
                         );

    ----------------------------------------------------------------
    -- Action set collection --
    ----------------------------------------------------------------
    lt_action_set = 
        select * from :lt_term_set  -- terminated
        union all
        select * from :lt_susp_set  -- suspended
        union all
        select * from :lt_start_set -- new start or restart
        union all
        select * from :lt_end_set   -- naturally ended

    ----------------------------------------------------------------
    -- Item change --
    ----------------------------------------------------------------
    if p_inc_delta = 'X' then
      lt_change_set =
        select _jkap.mandt,
               _jkap.vbeln,
                 posex,
               posnr_ur,
               _jkap.posnr,
               case
                 when _term.posnr is not null then 'T'                  -- most unlikely
                 when _jkap.poart = 'U' or _jkjap.poart = 'R' then 'U'  -- unlikely
                 else 'N'  -- normal
               end as status,
               'X' as delta
        from jkap as _jkap inner join jkak as _jkak on
          _jkap.mandt = _jkak.mandt and
          _jkap.vbeln = _jkak.vbeln
        left outer join :lt_term_all as _term on
          _jkap.mandt = _term.mandt and
          _jkap.vbeln = _term.vbeln and
          _jkap.posex = _term.posex
        where _jkap.mandt = :p_client
          and vbtyp = 'C'  -- Order
          and trvog = '0'  -- Subscription order
          and _jkap.aenuser not in ( 'REDWOOD' )   -- skip changes from REDWOOD
          and not exists( select vbeln from :lt_action_set ) -- not caught previously
          and (_jkap.erfdate = :p_keydate or
               _jkap.aendate = :p_keydate);
    ----------------------------------------------------------------
      et_order_status = select * from :lt_action_set
                        union all
                        select * from :lt_change_set;
                        end if;
    else
      et_order_status = :lt_action_set; 
    end if;                                 
  endmethod.

  method check_termination by Database procedure for hdb
                           language sqlscript
                           options read-only
                           using jkap
                           ZCL_TEST_AMDP_HELPER=>GET_TERMINATION_PER_KEYDATE.
    declare lv_count integer := 0;
    result := ' ';   -- abap_false

    call
     "ZCL_TEST_AMDP_HELPER=>GET_TERMINATION_PER_KEYDATE"(
            i_client     => :i_client,
            i_keydaye    => :i_date,
            et_term_data => :lt_term_all );
   
    select count( posnr ) into lv_count 
      from :lt_term_all
      where mandt      = :i_client 
        and vbeln      = :i_vbeln
        and posex      = :i.posex
        and start_date = :i_date;
    if lv_count > 0 then
       result := 'X';   -- abap_true
    end if;
  endmethod.

  method check_suspension by Database procedure for hdb
                           language sqlscript
                           options read-only
                           using jkap
                           ZCL_TEST_AMDP_HELPER=>GET_TERMINATION_PER_KEYDATE.
    declare lv_count integer := 0;
    result := ' ';   -- abap_false

    call
     "ZCL_TEST_AMDP_HELPER=>GET_TERMINATION_PER_KEYDATE"(
            i_client     => :i_client,
            i_keydaye    => :i_date,
            et_term_data => :lt_term_all );
   
    select count(posnr) into lv_count
    from jkap as _jkap
    where mandt = :i_client
      and vbeln = :i_vbeln
      and posex = :i_posex
      and poart = 'U'  -- suspensioon
      and gueltigvon = :i_date
      and not exists ( select posnr from :lt_term_all
                         where mandt = _jkap.mandt
                           and vbeln = _jkap.vbeln
                           and posex = _jkap.posex
                     );

    if lv_count > 0 then
       result := 'X';   -- abap_true
    end if;

  endmethod.

  method check_redirection by Database procedure for hdb
                           language sqlscript
                           options read-only
                           using jkap
                           ZCL_TEST_AMDP_HELPER=>GET_TERMINATION_PER_KEYDATE.

    declare lv_count integer := 0;
    result := ' ';   -- abap_false

    call
     "ZCL_TEST_AMDP_HELPER=>GET_TERMINATION_PER_KEYDATE"(
            i_client     => :i_client,
            i_keydaye    => :i_date,
            et_term_data => :lt_term_all );

    select count(posnr) into lv_count
    from jkap as _jkap
    where mandt = :i_client
      and vbeln = :i_vbeln
      and posex = :i_posex
      and poart = 'R'   -- redirection
      and gueltigvon = :i_date
      and not exists ( select posnr from :lt_term_all
                         where mandt = _jkap.mandt
                           and vbeln = _jkap.vbeln
                           and posex = _jkap.posex
                     );
    if lv_count > 0 then
       result = 'X';   -- abap_true
    end if;

  endmethod.

  method check_start by Database procedure for hdb
                           language sqlscript
                           options read-only
                           using jkap
                           ZCL_TEST_AMDP_HELPER=>GET_TERMINATION_PER_KEYDATE.
    -- This procedure is for start or restart
    declare lv_posnr nvarchar( 6 ) := '0';
    declare lv_posnr_ur nvarchar( 6 ) := '';

    call
     "ZCL_TEST_AMDP_HELPER=>GET_TERMINATION_PER_KEYDATE"(
            i_client     => :i_client,
            i_keydaye    => :i_date,
            et_term_data => :lt_term_all );

    select top 1
       posnr, posnr_ur
       into lv_posnr, lv_posnr_ur
    from jkap as _jkap
    where mandt = :i_client
      and vbeln = :i_vbeln
      and posex = :i_posex
      and poart in ('NP', 'KP', 'PP')   -- normal item
      and gueltigvon = :i_date
      and not exists ( select posnr from :lt_term_all
                         where mandt = _jkap.mandt
                           and vbeln = _jkap.vbeln
                           and posex = _jkap.posex
                     )
      and not exists ( select posnr from jkap as _ur
                         where mandt = _jkap.mandt
                           and vbeln = _jkap.vbeln
                           and posex = _jkap.posex
                           and poart in ('R', 'U')
                           and gueltigvon <= :i_date
                           and gueltigbis >= :i_date 
                     );

    if lv_posnr != '0' and lv_posnr = lv_posnr_ur then
        result := 1;  -- start
    elseif lv_posnr != 0 then
        result := 2;  -- restart
    else
        result := 0;  -- not
    end if;

  endmethod.

  method check_end by Database procedure for hdb
                           language sqlscript
                           options read-only
                           using jkap 
                           ZCL_TEST_AMDP_HELPER=>GET_TERMINATION_PER_KEYDATE.
    declare lv_count integer := 0;

    call
     "ZCL_TEST_AMDP_HELPER=>GET_TERMINATION_PER_KEYDATE"(
            i_client     => :i_client,
            i_keydaye    => :i_date,
            et_term_data => :lt_term_all );

    select count(posnr) into lv_count
    from jkap as _jkap
    where mandt = :i_client
      and vbeln = :i_vbeln
      and posex = :i_posex
      and poart in ('NP', 'KP', 'PP')   -- normal item
      and gueltigbis = :i_date
      and not exists ( select posnr from jkap
                         where mandt = _jkap.mandt
                           and vbeln = _jkap.vbeln
                           and posex = _jkap.posex
                           and gueltigvon > _jkap.gueltigbis 
                     )
      and not exists ( select posnr from :lt_term_all
                         where mandt = _jkap.mandt
                           and vbeln = _jkap.vbeln
                           and posex = _jkap.posex
                     )
      and not exists ( select posnr from jkap as _ur
                         where mandt = _jkap.mandt
                           and vbeln = _jkap.vbeln
                           and posex = _jkap.posex
                           and poart in ('R', 'U')
                           and gueltigvon <= :i_date
                           and gueltigbis >= :i_date 
                     );
    if lv_count > 0 then
        result := 'X';  -- abap_true
    else
        result := ' ';  -- abap_fals
    end if;

  endmethod.

  method get_payment_document_single by database PROCEDURE FOR HDB
                                     LANGUAGE sqlscript
                                     OPTIONS read-only
                                     using dfkkko dfkkop.
    declare lv_count integer := 0;

    lt_mdfkkop = select top 1
                    mandt, augbl, augrd from dfkkop
                    where mandt = :i_client
                      and opbel = :i_opbel
                      and augst = '9'  -- Cleaned document
                      and augrd in ( '01', '08', '15' );

    -- Check rows of the returning dataset
    select count( augbl ) into lv_count
      from :lt_mdfkkop;
    if lv_count < 1 then
       return;  -- no row found, the payment data is blank
    end if;

    -- For reason code '04'
    select top 1
       opbel, blart, budat into e_opbel, e_blart, e_budat
       from :lt_mdfkkop as _vop inner join dfkkko as _ko on
       _vop.augbl = _ko.opbel
       where augrd = '01';

    if e_opbel is not null and e_opbel != ' ' and length( e_opbel ) = 12 then
       return;  -- found a good document
    end if;

    -- For reason copde '08' and '15'
    lt_mdfkkop2 = select _vop.mandt, opbel
                    from :lt_mdfkkop as _vop inner join dfkkop as _op on
                    _vop.mandt = _op.mandt and
                    _vop.augbl = _op.augbl
                    where _vop.augrd in ( '08', '15' )
                      and opbel != :i_opbel;

    select top 1
       _op.opbel, blart, budat into e_opbel, e_blart, e_budat
       from :lt_mdfkkop2 as _op inner join dfkkko as _ko
       on _op.mandt = _ko.mandt and
          _op.opbel = _ko.opbel
       where herkf in ( '05', '06', '25', '55' );

  endmethod.
ENDCLASS.