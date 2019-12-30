** IDoc Sender
**
** Purpose: Sample code for IDoc 
**
** Author : Simon Li  Oct 2018
**
** https://help.sap.com/viewer/8f3819b0c24149b5959ab31070b64058/7.0.37/en-US/4ab074b6aa3a1997e10000000a421937.html
* Processing IDocs
* Configuring Ports
* Defining Partners
* Processing Tests
* Monitoring
* Archiving IDocs
* Structure, Documentation, and Definition of IDoc Types
REPORT ZTEST_IDOC_SEND.

PARAMETERS: p_matnr TYPE mara-matnr,
            p_msgtyp TYPE edidc-mestyp,
            p_rcvprn TYPE edidc-rcvprn.

* retrieve app. data from DB
SELECT matnr, mtart FROM mara 
  INTO TABLE @DATA(lt_mara) 
  WHERE matnr = @p_matnr.

* Master idoc control record
*DATA ls_edidc TYPE edidc.
*ls_edidc-rcvprt = 'LS'.          " Logic System
*ls_edidc-rcvprn = p_rcvprn.      " Receiver Partner Profile A00000000089(BD82)
*ls_edidc-idoctp = 'ZAZTSTIDOC'.  " IDoc type (defined in WE30)
*ls_edidc-mestyp = p_msgtyp.      " Message type (defined in WE81)
data(ls_edidc) = value edidc(
                                rcvprt = 'LS'
                                rcvprn = p_rcvprn      " Receiver Partner Profile A00000000089(BD82)
                                idoctp = 'ZATESTIDOC'  " IDoc type (defined in WE30)
                                mestyp = p_msgtyp      " Message type (defined in WE81/WE82)
                            ).

* master idoc data records
DATA lt_data TYPE TABLE OF edidd.
LOOP AT lt_mara assigning FIELD-SYMBOLS(<imara>).
  append initial line to lt_data assigning field-symbols(<data>).
  <data>-segnam = 'Z1TESTSEG'.  " Segment created in WE31
  <data>-sdata  = <imara>.      " Assign the data
ENDLOOP.

" Send out 
CALL FUNCTION 'MASTER_IDOC_DISTRIBUTE'
  EXPORTING
    master_idoc_control = ls_edidc
  TABLES
    communication_idoc_control = DATA(lt_edidc)
    master_idoc_data = lt_data
COMMIT WORK.

* Extend the existing partner profiles. 
* Partner number: TESTVEND
* Partner type: LI
* Partner function: VD
* Message type: ORDERS
* Basic type: ORDERS01
* Enhancement: Z1ORDERS
* Receiver port: TESTPORT
* Output mode: Transfer IDocs immediately, do not start subsystem
* Permitted agent: aN SAP user, type US (individual user), language EN
* Application: EF (purchasing)
* Output type: NEW
* Process code: ME10
* Release: segment -> Enhancement(Extension Type), segments -> IDoc Type

* Enhance the function module IDOC_OUTPUT_ORDERS in the additional project ZM06E001.
* SAP extension: MM06E001
* Function exit EXIT_SAPLEINM_002 (Customer enhancement of data segments for outbound purchasing document)
* Include ZXM06U02
* Segment Z1TEST1
* SAPLXM06/LXM06TOP
* Customer Top Include ZXM06TOP
form add_idoc_exttype_out tables int_edidd structure edidd.
  field-symbols <z1test1> TYPE z1test1.
  data lt_edidd like int_edidd[].
  data lv_added type abap_bool value abap_false.

  loop at int_edidd.
    append int_edidd to lt_edidd.
    case init_edidd-segnam.
      "when 'Z1TEST1'.  " Dummy code for inbound
      when 'E1EDP19'.  " Append the custom extension segement defined in WE30/31
        CLEAR int_edidd. 
        int_edidd-segnam = 'Z1TEST1'.     "Name the ext type segement

        assign int_edidd-sdata to <z1test1> casting.     "edidd               
        <z1test1>-kontinent = 'Canada'.
        APPEND int_edidd to lt_edidd.
        lv_added = abap_true.
    ENDCASE.
  endloop.
  " The DOCNUM/SEGNUM/PSGNUM/HLEVEL/DTINT2 of EDIDD will be assigned afterwards
  if lv_added = abap_true.  " check whether the customer segenment has been added
    int_edidd[] = lt_edidd.
  endif.
endform.
*https://help.sap.com/viewer/8f3819b0c24149b5959ab31070b64058/7.0.37/en-US/42ba446c-a611-405a-a4a3-c1c681b21b7b.html  
* BOR as subtype of IDOCAPPL  