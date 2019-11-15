REPORT ZTEST_EVENTS 
    LINE-SIZE 75 
    LINE-COUNT 30(3) 
    NO STANDARD PAGE HEADING
    MESSAGE-ID ZWWW. 

Tables: MARA. 
		
SELECT-OPTIONS: MATS FOR MARA-MATNR OBLIGATORY. 

INITIALIZATION. 
  MATS-SELECTION = 'I'.
  MATS-OPTION = 'EQ'.
  MATS-LOW = '1'. 
  MATS-HIGH = '500'. 
  APPEND MATS.

AT SELECTION-SCREEN OUTPUT.  " PBO
  loop at screen.
  endloop.

AT SELECTION-SCREEN.         " PAI
  IF MATS-LOW = ' '. 
    MESSAGE I000(ZKMESSAGE). 
  ELSEIF MATS-HIGH = ' '. 
    MESSAGE I001(ZKMESSAGE). 
  ENDIF. 

AT SELECTION-SCREEN ON VALUE-REQUEST FOR mats-low.
  PERFORM f4_help_mat USING 'MATS-LOW'.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR mats-high.
  PERFORM f4_help_mat USING 'MATS-HIGH'.

TOP-OF-PAGE. " header per page
  WRITE: / 'CLASSICAL REPORT CONTAINING GENERAL MATERIAL DATA FROM THE TABLE MARA' COLOR 7. 
  ULINE. 
  WRITE: / 'MATERIAL' COLOR 1, 

  24 'INDUSTRY' COLOR 2, 
  38 'UNITS' COLOR 3, 
  53 'MATERIAL TYPE' COLOR 4. 
  ULINE.

END-OF-PAGE.  " foot per page

START-OF-SELECTION. 
  SELECT MATNR, MBRSH, MEINS. MTART FROM MARA  
    INTO TABLE @data(lt_ma) 
    WHERE MATNR IN @MATS. 

  LOOP AT lt_ma ASSIGNING field-symbols <ma>. 
    WRITE: / <ma>-MATNR, 
           25 wa_ma-MBRSH, 
           40 wa_ma-MEINS, 
           55 wa_ma-MTART. 
  ENDLOOP. 

END-OF-SELECTION. 
  ULINE. 
  WRITE: / 'CLASSICAL REPORT HAS BEEN CREATED' COLOR 7.
  ULINE. 
  SKIP. 

FORM f4_help_mat USING value(i_field)  TYPE  help_info-dynprofld.
  DATA: BEGIN OF lt_f4help occurs 200,
          key  TYPE char10,
          text TYPE char80,
        END OF lt_f4help.
        
  "... fill in lt_f4help[]

  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
    EXPORTING
      retfield    = 'KEY'
      dynpprog    = sy-repid
      dynpnr      = sy-dynnr
      dynprofield = i_field
      value_org   = 'S'
    TABLES
      value_tab   = lt_f4help.
ENDFORM.                         

*******************************************************************************
*CLASSICAL REPORT CONTAINING GENERAL MATERIAL DATA FROM THE TABLE MARA
*______________________________________________________________________
*MATERIAL  INDUSTRY   UNITS   MATERIAL TYPE
*______________________________________________________________________
* 23       1          EA       ROH
* 59       M          PC       HALB
*...
*______________________________________________________________________
*CLASSICAL REPORT HAS BEEN CREATED
*______________________________________________________________________