** ABAP Report Event Sequence 
**
** Purpose: Sample to use different events
**
** Author : Simon Li  Jul 2010
**
REPORT ZTEST_EVENTS 
    LINE-SIZE 75 
    LINE-COUNT 30(3) 
    NO STANDARD PAGE HEADING
    MESSAGE-ID ZWWW. 

TYPE-POOLS icon.

Tables MARA. 
TABLES: sscrfields, smp_dyntxt.
  
SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE text-001.
SELECT-OPTIONS MATS FOR MARA-MATNR OBLIGATORY. 
SELECTION-SCREEN END OF BLOCK b1.

SELECTION-SCREEN FUNCTION KEY: 1, 2.  
" The numbering i must be between 1 and 5

*At Pf<nn>  (Obsolete)
*A user chooses a function code PF<nn> (<nn> can be between 01 and 24)
*should only be for temporary test versions. In production programs

AT USER-COMMAND.
*<statements>.
*If a user chooses a function code during list processing that is neither processed by the system, or PICK or PF<nn>, 
*the system triggers the event AT USER-COMMAND. For this event, you must define your own GUI status for a list.

INITIALIZATION.
  smp_dyntxt-text = 'LH'.
  smp_dyntxt-icon_id = icon_plane.
  sscrfields-functxt_01 = smp_dyntxt.

  smp_dyntxt-text = 'UA's.
  sscrfields-functxt_02 = smp_dyntxt.

  MATS-SELECTION = 'I'.
  MATS-OPTION = 'EQ'.
  MATS-LOW = '1'. 
  MATS-HIGH = '500'. 
  APPEND MATS.

AT SELECTION-SCREEN OUTPUT.  " PBO
  loop at screen.
    case screen-group.
      when 'OUT'.  " Output only, no input/change
        screen-input = '0'.
        modify screen.
        
      when 'PAS'.  " Password, no display
        screen-invisible = '1'.
        modify screen.

      when 'TST'.   " switch it off for production
        screen-active = '0'.
        modify screen.
    endcase.
  endloop.

AT SELECTION-SCREEN.         " PAI
  CASE sscrfields-UCOMM.
    WHEN 'FC01'.
      " Write your logic for the first push button
      exit.
    
    WHEN 'FC02'.
     " Write your logic for the second push button
     exit.
  ENDCASE

  AUTHORITY-CHECK OBJECT 'Z_MATS'
    ID 'MAT' FIELD mats-low ID 'ACTVT' FIELD '03'.
  IF sy-subrc <> 0.
    MESSAGE 'No authorization' TYPE 'E' DISPLAY LIKE 'S'.
  ENDIF.

  IF MATS-LOW = ' '. 
    MESSAGE I286(00) WITH 'The low value is required'. 
  ELSEIF MATS-HIGH = ' '. 
    MESSAGE I286(00) WITH 'The high value is requitred'. 
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