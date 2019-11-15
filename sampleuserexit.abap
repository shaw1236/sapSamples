** User Exit Lists
**
** Purpose: user exit lists.
**
** Author:  Simon Li  Sep 2005
REPORT zgen_user_exit NO STANDARD PAGE HEADING LINE-SIZE 170 LINE-COUNT 66.

*--------------------------------------------------------------------
* TYPE-POOLS
*--------------------------------------------------------------------
TYPE-POOLS: abap, icon.

*--------------------------------------------------------------------
* CONSTANTS
*--------------------------------------------------------------------
CONSTANTS: con_container  TYPE scrfname VALUE 'GM_CONTAINER'.

CONSTANTS: BEGIN OF c_incl,
            function VALUE 'F',
            customer VALUE 'C',
            example  VALUE 'E',
            sap_form VALUE 'S',
            cli_form VALUE 'X',
            sap_top  VALUE 'T',
            cli_top  VALUE 'Y',
          END OF c_incl.
*--------------------------------------------------------------------
* INCLUDES
*--------------------------------------------------------------------
INCLUDE mj_tvarv_sign.
INCLUDE mj_tvarv_opti.

*--------------------------------------------------------------------
* TABLES
*--------------------------------------------------------------------
TABLES: tadir, modact, modattr, modsap.

*--------------------------------------------------------------------
* USER-TYPES
*--------------------------------------------------------------------
*TYPES: icon_type(4) TYPE c.
TYPES: icon_type TYPE isp_ampel.

TYPES: BEGIN OF ty_cmod,
         project_name   TYPE modact-name,           " POSPLAUS
*         status(4)      TYPE c,
         status         TYPE icon_type,
         cnam           TYPE modattr-cnam,
         cdat           TYPE modattr-cdat,
         anam           TYPE modattr-anam,
         adat           TYPE modattr-adat,
         exit_name      TYPE modsap-name,                   " JHPA0004
         component_name TYPE modsap-member,         " EXIT_SAPLJHPA_006
         impl_flag(1),
         implemented    TYPE icon_type,
        END OF ty_cmod,
       tab_cmod TYPE TABLE OF ty_cmod.

*--------------------------------------------------------------------
* g l o b a l   d a t a
*--------------------------------------------------------------------
CLASS lcl_event_receiver DEFINITION DEFERRED.

DATA: ok_code LIKE sy-ucomm, g_repid LIKE sy-repid.

* declare reference variables to the alv grid and the container
DATA: grid             TYPE REF TO cl_gui_alv_grid,
      custom_container TYPE REF TO cl_gui_custom_container.

DATA  event_receiver TYPE REF TO lcl_event_receiver.

DATA: gt_fieldcat      TYPE lvc_t_fcat,
      gs_layout        TYPE lvc_s_layo,
      gt_sort          TYPE lvc_t_sort.

*--------------------------------------------------------------------
DATA: mem_cmod  TYPE tab_cmod,
      gt_cmod   TYPE tab_cmod.

DATA: wa_cmod   TYPE ty_cmod,
      wa_modact TYPE modact,
      wa_modsap TYPE modsap.

****************************************************************
* LOCAL CLASSES: Definition
****************************************************************
*===============================================================
* class lcl_event_receiver: local class to handle event DOUBLE_CLICK
*
* Definition:
* ~~~~~~~~~~~
CLASS lcl_event_receiver DEFINITION.

  PUBLIC SECTION.
    METHODS:

    handle_double_click
        FOR EVENT double_click OF cl_gui_alv_grid
            IMPORTING e_row e_column.

  PRIVATE SECTION.
ENDCLASS.                    "lcl_event_receiver DEFINITION
*
* lcl_event_receiver (Definition)
*===============================================================

****************************************************************
* LOCAL CLASSES: Implementation
****************************************************************
*===============================================================
* class lcl_event_receiver (Implementation)
*
* event DOUBLE_CLICK is caught
*
CLASS lcl_event_receiver IMPLEMENTATION.

  METHOD handle_double_click.
    FIELD-SYMBOLS <line> TYPE ty_cmod.

    READ TABLE gt_cmod ASSIGNING <line> INDEX e_row-index.
    CHECK sy-subrc = 0.

    CASE e_column-fieldname.
      WHEN 'PROJECT_NAME' OR 'STATUS' OR 'CNAM' OR 'CDAT'.
        PERFORM bdc_cmod_project USING <line>.
*        SET PARAMETER ID 'MON_KUN' FIELD <line>-project_name.
*        CALL TRANSACTION 'CMOD' AND SKIP FIRST SCREEN.

      WHEN 'EXIT_NAME'.
*        SET PARAMETER ID 'OB1' FIELD <line>-exit_name.
*        SUBMIT sapicss_ AND RETURN
*          WITH key1low = <line>-exit_name.
        PERFORM bdc_cmod_exit USING <line>.

      WHEN 'COMPONENT_NAME' OR 'IMPLEMENTED'.
        PERFORM call_function USING <line>-component_name.
*        SET PARAMETER ID 'LIB' FIELD <line>-component_name.
*        CALL TRANSACTION 'SE37' AND SKIP FIRST SCREEN.

      WHEN OTHERS.
        PERFORM bdc_cmod_project USING <line>.
    ENDCASE.
  ENDMETHOD.                           "handle_double_click

ENDCLASS.                    "lcl_event_receiver IMPLEMENTATION
*
* lcl_event_receiver (Implementation)
*===================================================================

*--------------------------------------------------------------------
* Screen.
*--------------------------------------------------------------------
*----------------- SELECTION-SCREEN ----------------------------------*
SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE text-001.
SELECT-OPTIONS: so_proj FOR modact-name.
SELECT-OPTIONS: so_exit FOR modact-member.
SELECT-OPTIONS: so_comp FOR modsap-member.
SELECTION-SCREEN END OF BLOCK b1.

SELECTION-SCREEN SKIP.

SELECTION-SCREEN BEGIN OF BLOCK b2 WITH FRAME TITLE text-002.
SELECT-OPTIONS: so_user FOR modattr-cnam MATCHCODE OBJECT j_1iusnam.
SELECT-OPTIONS: so_date FOR modattr-cdat.
SELECTION-SCREEN END OF BLOCK b2.

SELECTION-SCREEN BEGIN OF BLOCK b3 WITH FRAME TITLE text-003.
SELECT-OPTIONS: so_user2 FOR modattr-anam MATCHCODE OBJECT j_1iusnam.
SELECT-OPTIONS: so_date2 FOR modattr-adat.
SELECTION-SCREEN END OF BLOCK b3.

************************************************************************
* AT SELECTION-SCREEN - F4 Helps
************************************************************************
AT SELECTION-SCREEN ON VALUE-REQUEST FOR so_proj-low.
  PERFORM f4_help_project USING 'SO_PROJ-LOW' 'NAME'.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR so_proj-high.
  PERFORM f4_help_project USING 'SO_PROJ-HIGH' 'NAME'.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR so_exit-low.
  PERFORM f4_help_exit USING 'SO_EXIT-LOW' 'NAME'.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR so_exit-high.
  PERFORM f4_help_exit USING 'SO_EXIT-LOW' 'NAME'.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR so_comp-low.
  PERFORM f4_help_comp USING 'SO_COMP-LOW' 'MEMBER'.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR so_comp-high.
  PERFORM f4_help_comp USING 'SO_COMP-HIGHT' 'MEMBER'.

************************************************************************
* Load-Of-Program
************************************************************************
LOAD-OF-PROGRAM.
  PERFORM load_into_mem.

************************************************************************
* Initialization
************************************************************************
INITIALIZATION.
  g_repid = sy-repid.
  PERFORM layout_field_catalog CHANGING gt_fieldcat.
  PERFORM layout_sort_build CHANGING gt_sort.

************************************************************************
* Start-of-Selection
************************************************************************
START-OF-SELECTION.
  PERFORM selection.
* Set initial dynpro
  SET SCREEN 100.

************************************************************************
* Screen 0100 PBO/PAI
************************************************************************
*PROCESS BEFORE OUTPUT.
* MODULE STATUS_0100.
**
*PROCESS AFTER INPUT.
* MODULE USER_COMMAND_0100.
*---------------------------------------------------------------------*
*       FORM SELECTION                                                *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
FORM selection.

  LOOP AT mem_cmod INTO wa_cmod
    WHERE project_name IN so_proj
      AND exit_name IN so_exit
      AND component_name IN so_comp
      AND cnam IN so_user
      AND cdat IN so_date
      AND anam IN so_user2
      AND adat IN so_date2.

    APPEND wa_cmod TO gt_cmod.

  ENDLOOP.
ENDFORM.                    "selection
*&---------------------------------------------------------------------*
*&      Form  load_into_mem
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM load_into_mem .
  DATA wa_modattr TYPE modattr.
  DATA fname TYPE rs38l-name.

  SELECT * FROM modact INTO wa_modact WHERE member > space.
    SELECT SINGLE * FROM modattr INTO wa_modattr
      WHERE name = wa_modact-name.

    SELECT * FROM modsap INTO wa_modsap
      WHERE name = wa_modact-member(8)
        AND member LIKE 'EXIT_%'.
      wa_cmod-project_name   = wa_modact-name.

      IF wa_modattr-status = 'A'.
        wa_cmod-status       = icon_activate.
      ELSE.
        wa_cmod-status       = icon_deactivate.
      ENDIF.

      wa_cmod-cnam           = wa_modattr-cnam.
      wa_cmod-cdat           = wa_modattr-cdat.

      wa_cmod-anam           = wa_modattr-anam.
      wa_cmod-adat           = wa_modattr-adat.

      wa_cmod-exit_name      = wa_modsap-name.
      wa_cmod-component_name = wa_modsap-member.

      fname = wa_modsap-member.
      PERFORM get_impl_flag USING fname CHANGING wa_cmod-impl_flag.
      IF wa_cmod-impl_flag = abap_true.
        wa_cmod-implemented = icon_led_green.
      ELSE.
        wa_cmod-implemented = icon_space.
      ENDIF.
      APPEND wa_cmod TO mem_cmod.
    ENDSELECT.
  ENDSELECT.
ENDFORM.                    " load_into_mem
*---------------------------------------------------------------------*
*       FORM EXIT_PROGRAM                                             *
*---------------------------------------------------------------------*
FORM exit_program.
  CALL METHOD custom_container->free.
  CALL METHOD cl_gui_cfw=>flush.
  IF sy-subrc NE 0.
* add your handling, for example
    CALL FUNCTION 'POPUP_TO_INFORM'
      EXPORTING
        titel = g_repid
        txt2  = sy-subrc
        txt1  = 'Error in FLush'(500).
  ENDIF.
  LEAVE PROGRAM.
ENDFORM.                    "exit_program
*&---------------------------------------------------------------------*
*&      Module  STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_0100 OUTPUT.
  SET PF-STATUS 'ST100'.
  SET TITLEBAR 'TI100'.

  g_repid = sy-repid.
* �1.Create one ALV Control that shows the first table.
  IF custom_container IS INITIAL.

* create a custom container control for our ALV Control
    CREATE OBJECT custom_container
        EXPORTING
            container_name = con_container
        EXCEPTIONS
            cntl_error = 1
            cntl_system_error = 2
            create_error = 3
            lifetime_error = 4
            lifetime_dynpro_dynpro_link = 5.

    IF sy-subrc NE 0.
* add your handling, for example
      CALL FUNCTION 'POPUP_TO_INFORM'
        EXPORTING
          titel = g_repid
          txt2  = sy-subrc
          txt1  = 'The control could not be created'(510).
    ENDIF.

* create an instance of alv control
    CREATE OBJECT grid
         EXPORTING i_parent = custom_container.
*
* Set a titlebar for the grid control
*
    gs_layout-grid_title = 'Globe User Exit'(100).

    CALL METHOD grid->set_table_for_first_display
      EXPORTING
        is_layout       = gs_layout
      CHANGING
        it_outtab       = gt_cmod
        it_fieldcatalog = gt_fieldcat
        it_sort         = gt_sort.

********
* ->Create Object to receive events and link them to handler methods.
* When the ALV Control raises the event for the specified instance
* the corresponding method is automatically called.
*
    CREATE OBJECT event_receiver.
    SET HANDLER event_receiver->handle_double_click FOR grid.

    CALL METHOD cl_gui_control=>set_focus
      EXPORTING
        control = grid.

* Control Framework flushes at the end of PBO automatically!
  ENDIF.
ENDMODULE.                             " STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0100 INPUT.
  CASE ok_code.
    WHEN 'BACK'.
      PERFORM exit_program.
    WHEN 'EXIT'.
      PERFORM exit_program.
  ENDCASE.
  CLEAR ok_code.
ENDMODULE.                             " USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*&      Form  layout_field_catalog
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM layout_field_catalog CHANGING lt_fieldcat TYPE lvc_t_fcat.
  DATA ls_fieldcat TYPE lvc_s_fcat.

* Project Name
  CLEAR ls_fieldcat.
  ls_fieldcat-fieldname = 'PROJECT_NAME'.
  ls_fieldcat-tooltip   = 'CMOD project name'.
  ls_fieldcat-coltext   = 'Project Name'.
  ls_fieldcat-seltext   = ls_fieldcat-coltext.
  ls_fieldcat-key       = 'X'.
  ls_fieldcat-outputlen = 12.
  APPEND ls_fieldcat TO lt_fieldcat.

* Status
  CLEAR ls_fieldcat.
  ls_fieldcat-fieldname = 'STATUS'.
  ls_fieldcat-tooltip   = 'Active/Inactive flag'.
  ls_fieldcat-coltext   = 'ST'.
  ls_fieldcat-seltext   = ls_fieldcat-coltext.
*ICON_2 ICON_ACTIVATE                  '@3C@'."  Activate
*ICON_2 ICON_DEACTIVATE                '@8I@'."  Stop
  ls_fieldcat-icon      = 'X'.   "ICON_ACTIVATE
  ls_fieldcat-outputlen = 2.
  APPEND ls_fieldcat TO lt_fieldcat.

* Created Name
  CLEAR ls_fieldcat.
  ls_fieldcat-fieldname = 'CNAM'.
  ls_fieldcat-tooltip   = 'Author of the project'.
  ls_fieldcat-coltext   = 'Author'.
  ls_fieldcat-seltext   = ls_fieldcat-coltext.
  ls_fieldcat-outputlen = 12.
  APPEND ls_fieldcat TO lt_fieldcat.

* Created Date
  CLEAR ls_fieldcat.
  ls_fieldcat-fieldname = 'CDAT'.
  ls_fieldcat-tooltip   = 'Date when the project was created'.
  ls_fieldcat-coltext   = 'Created On'.
  ls_fieldcat-seltext   = ls_fieldcat-coltext.
  ls_fieldcat-outputlen = 12.
  APPEND ls_fieldcat TO lt_fieldcat.

* Changed Name
  CLEAR ls_fieldcat.
  ls_fieldcat-fieldname = 'ANAM'.
  ls_fieldcat-tooltip   = 'Name who last changed the project'.
  ls_fieldcat-coltext   = 'Changed By'.
  ls_fieldcat-seltext   = ls_fieldcat-coltext.
  ls_fieldcat-outputlen = 12.
  APPEND ls_fieldcat TO lt_fieldcat.

* Changed Date
  CLEAR ls_fieldcat.
  ls_fieldcat-fieldname = 'ADAT'.
  ls_fieldcat-tooltip   = 'Date when the last change happened'.
  ls_fieldcat-coltext   = 'Changed On'.
  ls_fieldcat-seltext   = ls_fieldcat-coltext.
  ls_fieldcat-outputlen = 12.
  APPEND ls_fieldcat TO lt_fieldcat.

* User Exit
  CLEAR ls_fieldcat.
  ls_fieldcat-fieldname = 'EXIT_NAME'.
  ls_fieldcat-tooltip   = 'User exit name'.
  ls_fieldcat-coltext   = 'User Exit'.
  ls_fieldcat-seltext   = ls_fieldcat-coltext.
  ls_fieldcat-outputlen = 10.
  APPEND ls_fieldcat TO lt_fieldcat.

* Implemented
  CLEAR ls_fieldcat.
  ls_fieldcat-fieldname = 'IMPLEMENTED'.
  ls_fieldcat-tooltip   = 'Implemention flag'.
  ls_fieldcat-coltext   = 'IM'.
  ls_fieldcat-seltext   = ls_fieldcat-coltext.
  ls_fieldcat-icon      = 'X'.   "ICON_ACTIVATE
  ls_fieldcat-outputlen = 2.
  APPEND ls_fieldcat TO lt_fieldcat.

* Component
  CLEAR ls_fieldcat.
  ls_fieldcat-fieldname = 'COMPONENT_NAME'.
  ls_fieldcat-tooltip   = 'Function name of the component'.
  ls_fieldcat-coltext   = 'Component Name'.
  ls_fieldcat-seltext   = ls_fieldcat-coltext.
  ls_fieldcat-outputlen = 32.
  APPEND ls_fieldcat TO lt_fieldcat.

ENDFORM.                    " layout_field_catalog
*---------------------------------------------------------------------*
*       FORM LAYOUT_SORT_BUILD                                        *
*---------------------------------------------------------------------*
*       Sorting                                                       *
*---------------------------------------------------------------------*
*  -->  LT_SORT                                                       *
*---------------------------------------------------------------------*
FORM layout_sort_build CHANGING lt_sort TYPE lvc_t_sort.
  DATA ls_sort TYPE lvc_s_sort.

* Project Name
  ls_sort-fieldname = 'PROJECT_NAME'.
  ls_sort-spos      = 1.
  ls_sort-up        = 'X'.
* LS_SORT-SUBTOT    = 'X'.
  APPEND ls_sort TO lt_sort.

** Status
*  ls_sort-fieldname = 'STATUS'.
*  ls_sort-spos      = 2.
*  ls_sort-up        = 'X'.
** LS_SORT-SUBTOT    = 'X'.
*  APPEND ls_sort TO lt_sort.

* User Exit
*  ls_sort-fieldname = 'EXIT_NAME'.
*  ls_sort-spos      = 2.
*  ls_sort-up        = 'X'.
* LS_SORT-SUBTOT    = 'X'.
  APPEND ls_sort TO lt_sort.
ENDFORM.  " LAYOUT_sort_build
*&---------------------------------------------------------------------*
*&      Form  f4_help_f4_help_project
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f4_help_project USING i_field  TYPE  help_info-dynprofld
                           i_retfield TYPE  dfies-fieldname.
  TYPES: BEGIN OF ty_f4_help,
            name TYPE modtext-name,
            modtext TYPE modtext-modtext,
         END OF ty_f4_help,
         tab_f4_help TYPE TABLE OF ty_f4_help.

  DATA: r_project TYPE RANGE OF modact-name,
       w_project LIKE LINE OF r_project.

  DATA: dynpfields TYPE TABLE OF dynpread INITIAL SIZE 100
                                          WITH HEADER LINE.
  CLEAR dynpfields. REFRESH dynpfields.
  dynpfields-fieldname = i_field.
  APPEND dynpfields.
*read field content component from selection screen
  CALL FUNCTION 'DYNP_VALUES_READ'
    EXPORTING
      dyname     = sy-cprog
      dynumb     = sy-dynnr
    TABLES
      dynpfields = dynpfields
    EXCEPTIONS
      OTHERS     = 11.
  IF sy-subrc <> 0.
    EXIT.  " could not find the field in the slection screen
  ENDIF.

  LOOP AT dynpfields WHERE fieldvalue IS NOT INITIAL.
    TRANSLATE dynpfields-fieldvalue TO UPPER CASE.
    CONCATENATE dynpfields-fieldvalue '*' INTO w_project-low.
    w_project-sign = con_sign_incl.
    w_project-option = con_op_cp.
    APPEND w_project TO r_project.
  ENDLOOP.

  DATA:  it_f4help TYPE tab_f4_help,
         wa_f4 TYPE ty_f4_help.

  REFRESH it_f4help.
  SELECT name modtext FROM modtext INTO TABLE it_f4help
    FOR ALL ENTRIES IN mem_cmod
    WHERE name = mem_cmod-project_name
      AND name IN r_project.

  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
    EXPORTING
      retfield    = i_retfield
      dynpprog    = sy-repid
      dynpnr      = sy-dynnr
      dynprofield = i_field
      value_org   = 'S'
    TABLES
      value_tab   = it_f4help.
ENDFORM.                                                    "f4_help_project
*&---------------------------------------------------------------------*
*&      Form  f4_help_Exit
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_0138   text
*      -->P_0139   text
*----------------------------------------------------------------------*
FORM f4_help_exit USING i_field    TYPE  help_info-dynprofld
                        i_retfield TYPE  dfies-fieldname.

  TYPES: BEGIN OF ty_f4_help,
           name TYPE modsapt-name,
           modtext TYPE modsapt-modtext,
         END OF ty_f4_help,
         tab_f4_help TYPE TABLE OF ty_f4_help.

  DATA: r_exit TYPE RANGE OF modsapt-name,
        w_exit LIKE LINE OF r_exit.

  DATA: dynpfields TYPE TABLE OF dynpread INITIAL SIZE 100
                                          WITH HEADER LINE.
  CLEAR dynpfields. REFRESH dynpfields.
  dynpfields-fieldname = i_field.
  APPEND dynpfields.
*read field content component from selection screen
  CALL FUNCTION 'DYNP_VALUES_READ'
    EXPORTING
      dyname     = sy-cprog
      dynumb     = sy-dynnr
    TABLES
      dynpfields = dynpfields
    EXCEPTIONS
      OTHERS     = 11.
  IF sy-subrc <> 0.
    EXIT.  " could not find the field in the slection screen
  ENDIF.

  LOOP AT dynpfields WHERE fieldvalue IS NOT INITIAL.
    TRANSLATE dynpfields-fieldvalue TO UPPER CASE.
    CONCATENATE dynpfields-fieldvalue '*' INTO w_exit-low.
    w_exit-sign = con_sign_incl.
    w_exit-option = con_op_cp.
    APPEND w_exit TO r_exit.
  ENDLOOP.

  DATA:  it_f4help TYPE tab_f4_help,
         wa_f4 TYPE ty_f4_help.

  REFRESH it_f4help.
  SELECT name modtext FROM modsapt INTO TABLE it_f4help
    FOR ALL ENTRIES IN mem_cmod
    WHERE sprsl = sy-langu AND name = mem_cmod-exit_name AND name IN r_exit.
  SORT it_f4help BY name.

  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
    EXPORTING
      retfield    = i_retfield
      dynpprog    = sy-repid
      dynpnr      = sy-dynnr
      dynprofield = i_field
      value_org   = 'S'
    TABLES
      value_tab   = it_f4help.

ENDFORM.                    " f4_help_Exit
*&---------------------------------------------------------------------*
*&      Form  f4_help_comp
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_0183   text
*      -->P_0184   text
*----------------------------------------------------------------------*
FORM f4_help_comp USING i_field    TYPE  help_info-dynprofld
                        i_retfield TYPE  dfies-fieldname.

  TYPES: BEGIN OF ty_f4_help,
           member      TYPE modsap-member,
           name        TYPE modsap-name,
           implemented TYPE isp_ampel,
         END OF ty_f4_help,
         tab_f4_help TYPE TABLE OF ty_f4_help.

  DATA: r_comp TYPE RANGE OF modsap-member,
        w_comp LIKE LINE OF r_comp.

  DATA: dynpfields TYPE TABLE OF dynpread INITIAL SIZE 100
                                          WITH HEADER LINE.
  CLEAR dynpfields. REFRESH dynpfields.
  dynpfields-fieldname = i_field.
  APPEND dynpfields.
*read field content component from selection screen
  CALL FUNCTION 'DYNP_VALUES_READ'
    EXPORTING
      dyname     = sy-cprog
      dynumb     = sy-dynnr
    TABLES
      dynpfields = dynpfields
    EXCEPTIONS
      OTHERS     = 11.
  IF sy-subrc <> 0.
    EXIT.  " could not find the field in the slection screen
  ENDIF.

  LOOP AT dynpfields WHERE fieldvalue IS NOT INITIAL.
    TRANSLATE dynpfields-fieldvalue TO UPPER CASE.
    CONCATENATE dynpfields-fieldvalue '*' INTO w_comp-low.
    w_comp-sign = con_sign_incl.
    w_comp-option = con_op_cp.
    APPEND w_comp TO r_comp.
  ENDLOOP.

  DATA:  it_f4help TYPE tab_f4_help.
  FIELD-SYMBOLS <f4> TYPE ty_f4_help.

  REFRESH it_f4help.
  SELECT member name FROM modsap INTO TABLE it_f4help
    FOR ALL ENTRIES IN mem_cmod
    WHERE member = mem_cmod-component_name AND member IN r_comp.

  LOOP AT it_f4help ASSIGNING <f4>.
    READ TABLE mem_cmod INTO wa_cmod WITH KEY component_name = <f4>-member.
    IF sy-subrc = 0.
      <f4>-implemented = wa_cmod-implemented.
    ENDIF.
  ENDLOOP.
  SORT it_f4help BY member.


  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
    EXPORTING
      retfield    = i_retfield
      dynpprog    = sy-repid
      dynpnr      = sy-dynnr
      dynprofield = i_field
      value_org   = 'S'
    TABLES
      value_tab   = it_f4help.
ENDFORM.                    " f4_help_comp
*&---------------------------------------------------------------------*
*&      Form  BDC_cmod_project
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM bdc_cmod_project USING cmod TYPE ty_cmod.

  DATA: bdctab TYPE TABLE OF bdcdata,
        bdc_wa TYPE bdcdata.

* BDC Program
  CLEAR bdc_wa.
  bdc_wa-program = 'SAPMSMOD'.
  bdc_wa-dynpro = '1010'.
  bdc_wa-dynbegin = 'X'.
  APPEND bdc_wa TO bdctab.

* BDC cursor
  CLEAR bdc_wa.
  bdc_wa-fnam = 'BDC_CURSOR'.
  bdc_wa-fval = 'MOD0-NAME'.
  APPEND bdc_wa TO bdctab.

* Cursor value
  CLEAR bdc_wa.
  bdc_wa-fnam = 'MOD0-NAME'.
  bdc_wa-fval = cmod-project_name.
  APPEND bdc_wa TO bdctab.

* BDC OKCODE
  CLEAR bdc_wa.
  bdc_wa-fnam = 'BDC_OKCODE'.
  bdc_wa-fval = '=SHOW'.
  APPEND bdc_wa TO bdctab.

*'A' Display screen
*'E' Display only if an error occurs
*'N' Do not display
*'P' Do not display; debugging possible
  CALL TRANSACTION 'CMOD' USING bdctab MODE 'E'.
ENDFORM.                    " BDC_cmod_project
*&---------------------------------------------------------------------*
*&      Form  bdc_cmod_exit
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM bdc_cmod_exit USING cmod TYPE ty_cmod.
  DATA: bdctab TYPE TABLE OF bdcdata,
        bdc_wa TYPE bdcdata.

*******************************
* PART I
*******************************
* BDC Program
  CLEAR bdc_wa.
  bdc_wa-program = 'SAPMSMOD'.
  bdc_wa-dynpro = '1010'.
  bdc_wa-dynbegin = 'X'.
  APPEND bdc_wa TO bdctab.

* BDC Cursor
  CLEAR bdc_wa.
  bdc_wa-fnam = 'BDC_CURSOR'.
  bdc_wa-fval = 'MOD0-NAME'.
  APPEND bdc_wa TO bdctab.
* Cursor value
  CLEAR bdc_wa.
  bdc_wa-fnam = 'MOD0-NAME'.
  bdc_wa-fval = cmod-project_name.
  APPEND bdc_wa TO bdctab.
* Top menu
  CLEAR bdc_wa.
  bdc_wa-fnam = 'MODF-HEAD'.
  bdc_wa-fval = 'X'.
  APPEND bdc_wa TO bdctab.
* BDC OKCODE
  CLEAR bdc_wa.
  bdc_wa-fnam = 'BDC_OKCODE'.
  bdc_wa-fval = '=LSAP'.
  APPEND bdc_wa TO bdctab.

*******************************
* PART II
*******************************
* BDC Program
  bdc_wa-program = 'SAPICSS_'.
  bdc_wa-dynpro = '0200'.
  bdc_wa-dynbegin = 'X'.
  APPEND bdc_wa TO bdctab.

* BDC Cursor
  CLEAR bdc_wa.
  bdc_wa-fnam = 'BDC_CURSOR'.
  bdc_wa-fval = 'KEY1-LOW'.
  APPEND bdc_wa TO bdctab.

* Cursor value
  CLEAR bdc_wa.
  bdc_wa-fnam = 'KEY1-LOW'.
  bdc_wa-fval = cmod-exit_name.
  APPEND bdc_wa TO bdctab.
*
*        CLEAR bdc_wa.
*        bdc_wa-fnam = 'LOOPC'.
*        bdc_wa-fval = '       200'.
*        APPEND bdc_wa TO bdctab.

* BDC OKCODE
  CLEAR bdc_wa.
  bdc_wa-fnam = 'BDC_OKCODE'.
  bdc_wa-fval = '=AUSF'.
  APPEND bdc_wa TO bdctab.
*'A' Display screen
*'E' Display only if an error occurs
*'N' Do not display
*'P' Do not display; debugging possible
*        CALL TRANSACTION 'CMOD' USING bdctab MODE 'A'.
  CALL TRANSACTION 'CMOD' USING bdctab MODE 'E'.
ENDFORM.                    " bdc_cmod_exit
*&---------------------------------------------------------------------*
*&      Form  call_function
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->FUNCTION_NAME  text
*----------------------------------------------------------------------*
FORM call_function  USING function_name TYPE c.
  CALL FUNCTION 'RS_TOOL_ACCESS'
    EXPORTING
      operation           = 'SHOW'  " EDIT
      object_name         = function_name
      object_type         = 'FUNC'
    EXCEPTIONS
      not_executed        = 1
      invalid_object_type = 2
      OTHERS              = 3.
  IF sy-subrc <> 0.
    MESSAGE s007(sbf_bw) WITH function_name.
  ENDIF.
ENDFORM.                    " call_function
*&---------------------------------------------------------------------*
*&      Form  get_impl_flag
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_WA_MODSAP_MEMBER  text
*      <--P_WA_CMOD_IMPL_FLAG  text
*----------------------------------------------------------------------*
FORM get_impl_flag  USING fname TYPE rs38l-name
                    CHANGING impl_flag TYPE c.

  DATA: l_incl_names TYPE TABLE OF smod_names INITIAL SIZE 4,
        w_incl_names LIKE LINE OF l_incl_names.

  impl_flag = abap_false.

  w_incl_names-itype = c_incl-example.  APPEND w_incl_names TO l_incl_names.
  w_incl_names-itype = c_incl-customer. APPEND w_incl_names TO l_incl_names.
  CALL FUNCTION 'MOD_FUNCTION_INCLUDE'
    EXPORTING
      funcname   = fname
    TABLES
      incl_names = l_incl_names
    EXCEPTIONS
      OTHERS     = 4.
  CHECK sy-subrc = 0.

  LOOP AT l_incl_names INTO w_incl_names WHERE itype = c_incl-customer.

    SELECT SINGLE name FROM trdir INTO w_incl_names-iname
                       WHERE name = w_incl_names-iname.
    IF sy-subrc = 0.
      impl_flag = abap_true.
      EXIT.
    ENDIF.
  ENDLOOP.

ENDFORM.                    " get_impl_flag