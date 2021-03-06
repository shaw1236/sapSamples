** Generic tools for tables
**
** Purpose: List the domain tables
**
** Author : Simon Li  Jul 2008
**
REPORT  zgen_domain_table NO STANDARD PAGE HEADING LINE-SIZE 120.

TYPE-POOLS: abap.

TABLES: dd03vt.

*DATA g_salv_table TYPE REF TO cl_salv_table.

DATA gt_data TYPE TABLE OF dd03vt.

** heading
*DATA g_display TYPE REF TO cl_salv_display_settings.

* Function
*DATA g_functions TYPE REF TO cl_salv_functions.

** Column heading
*DATA g_columns TYPE REF TO cl_salv_columns_table.
*DATA g_column TYPE REF TO cl_salv_column_table.

* Layout
*DATA g_layout TYPE REF TO cl_salv_layout.

*DATA key TYPE salv_s_layout_key.

*
* DATA g_selections TYPE REF TO cl_salv_selections.

*DATA g_events TYPE REF TO cl_salv_events_table.

*DATA ok_code TYPE sy-ucomm.

* Events of cl_salv_events_table
* if_salv_events_functions~before_salv_function
*    IMPORTING e_salv_function.

* if_salv_events_functions~after_salv_function
*    IMPORTING e_salv_function.

* if_salv_events_functions~added_function
*    IMPORTING e_salv_function.

* if_salv_events_list~top_of_page
*    IMPORTING r_top_of_page page table_index.

* if_salv_events_list~end_of_page
*    IMPORTING r_end_of_page page.

* if_salv_events_actions_table~double_click
*    IMPORTING row column.

* if_salv_events_actions_table~link_click
*    IMPORTING row column.

*----------------------------------------------------------------------*
* CLASS lcl_handle_events DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_handle_events DEFINITION.
  PUBLIC SECTION.
    METHODS:
       on_double_click FOR EVENT double_click OF cl_salv_events_table
          IMPORTING row column.
ENDCLASS. "lcl_handle_events DEFINITION
*----------------------------------------------------------------------*
* CLASS lcl_handle_events IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_handle_events IMPLEMENTATION.
  METHOD on_double_click.
    FIELD-SYMBOLS <alv_data> LIKE LINE OF gt_data.
    READ TABLE gt_data INDEX row ASSIGNING <alv_data>.
    CHECK sy-subrc = 0.

    CALL FUNCTION 'RS_DD_SHOW'
      EXPORTING
        objname                    = <alv_data>-tabname
        objtype                    = 'T'
*   POPUP                      = ' '
*   SECNAME                    =
*   MONITOR_ACTIVATE           = 'X'
* IMPORTING
*   FCODE                      =
     EXCEPTIONS
       object_not_found           = 1
       object_not_specified       = 2
       permission_failure         = 3
       type_not_valid             = 4
       OTHERS                     = 5.
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
              WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.

  ENDMETHOD. "on_double_click
ENDCLASS. "lcl_handle_events IMPLEMENTATION

DATA event_handler TYPE REF TO lcl_handle_events.

SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE text-001.
SELECT-OPTIONS s_domain FOR dd03vt-domname OBLIGATORY DEFAULT 'LOGSYS'.
SELECTION-SCREEN END OF BLOCK b1.

SELECTION-SCREEN BEGIN OF BLOCK b2 WITH FRAME TITLE text-002.
SELECT-OPTIONS s_tab    FOR dd03vt-tabname.
SELECT-OPTIONS s_class  FOR dd03vt-tabclass DEFAULT 'TRANSP'.
SELECTION-SCREEN END OF BLOCK b2.

LOAD-OF-PROGRAM.

START-OF-SELECTION.

  PERFORM selection.
  PERFORM alv_display.
  
*&---------------------------------------------------------------------*
*&      Form  selection
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM selection.
  SELECT * FROM dd03vt INTO TABLE gt_data
    WHERE tabname IN s_tab
      AND tabclass IN s_class
      AND domname IN s_domain
      AND ddlanguage = sy-langu.
ENDFORM.                    "selection
*&---------------------------------------------------------------------*
*&      Form  alv_display
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM alv_display.
  DATA cx_root TYPE REF TO cx_root.
  DATA message TYPE string.

  DATA lo_salv_table TYPE REF TO cl_salv_table.

*  SET EXTENDED CHECK OFF.
*  break sli.
*  SET EXTENDED CHECK ON.
  TRY.
      cl_salv_table=>factory( IMPORTING r_salv_table = lo_salv_table CHANGING t_table = gt_data ).

* All functions in GUI status
      data(lo_functions) = lo_salv_table->get_functions( ).
      lo_functions->set_all( abap_true ).
*     ALV-Toolbar
*      lo_salv_table->set_screen_status(
*        report        = 'SAPLSLVC_FULLSCREEN'
*        pfstatus      = 'STANDARD_FULLSCREEN'
*        set_functions = lo_salv_table->c_functions_all ).

**
*      data(lo_display) = lo_salv_table->get_display_settings( ).
*      lo_display->set_striped_pattern( cl_salv_display_settings=>true ).
*      lo_display->set_list_header( sy-title ).

** langu
*      data(lo_columns) = lo_salv_table->get_columns( ).
*      lo_column ?= lo_columns->get_column( 'LANGU' ).
*      lo_column->set_visible( abap_false ).
*
** Objver
*      lo_column ?= lo_columns->get_column( 'OBJVERS' ).
*      lo_column->set_visible( abap_false ).

      data(lo_layout) = lo_salv_table->get_layout( ).
      DATA ls_key TYPE salv_s_layout_key
      ls_key-report = sy-repid.
      lo_layout->set_key( ls_key ).
      lo_layout->set_default( abap_true ).
      lo_layout->set_save_restriction( cl_salv_layout=>restrict_none ).

* event
      data(lo_event) = lo_salv_table->get_event( ).
      
      CREATE OBJECT event_handler.
      SET HANDLER event_handler->on_double_click FOR lo_event.

      lo_salv_table->display( ).

    CATCH cx_root INTO data(lo_cx_root).                    "#EC CATCH_ALL
      data(lv_message) = lo_cx_root->get_text( ).
      MESSAGE lv_message TYPE 'E'.
  ENDTRY.
ENDFORM.                    "al_display
