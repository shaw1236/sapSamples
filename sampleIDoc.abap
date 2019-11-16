** IDoc Sender
**
** Purpose: Sample code for IDoc 
**
** Author : Simon Li  Oct 2018
**
REPORT ZTEST_IDOC_SEND.

PARAMETERS: p_matnr TYPE mara-matnr,
            p_msgtyp TYPE edidc-mestyp,
            p_rcvprn TYPE edidc-rcvprn.

* retrieve app. data from DB
SELECT matnr, mtart FROM mara 
  INTO TABLE @DATA(lt_mara) 
  WHERE matnr = @p_matnr.

* Master idoc control record
DATA ls_edidc TYPE edidc.
ls_edidc-rcvprt = 'LS'.          " Logic System
ls_edidc-rcvprn = p_rcvprn.      " Receiver Partner Profile A00000000089(BD82)
ls_edidc-idoctp = 'ZAZTSTIDOC'.  " IDoc type (defined in WE30)
ls_edidc-mestyp = p_msgtyp.      " Message type (defined in WE81)

* master idoc data records
DATA: ls_data TYPE edidd, lt_data TYPE TABLE OF edidd.
LOOP AT lt_mara assigning FIELD-SYMBOLS(<imara>).
  ls_data-segnam = 'ZAZTSTSEG'.  " Segment created in WE31
  ls_data-sdata  = <imara>.      " Assign the data
  APPEND ls_data to lt_data.
ENDLOOP.

" Send out 
CALL FUNCTION 'MASTER_IDOC_DISTRIBUTE'
  EXPORTING
    master_idoc_control = ls_edidc
  TABLES
    communication_idoc_control = DATA(lt_edidc)
    master_idoc_data = lt_data
COMMIT WORK.