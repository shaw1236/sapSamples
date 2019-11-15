** Class read from/stored to a file persistence or clipboard 
**
** Purpose: Sample to exchange json <=> file, clipboard
**
** Author : Simon Li  Jul 2018
**
CLASS LCL_JSON_FILE DEFINITION.
  PUBLIC SECTION.

    CLASS-METHODS:
      string_to_file IMPORTING 
                       VALUE(i_long_string) TYPE STRING
                       VALUE(i_file) TYPE CSEQUENCE,
      STRING_TO_CLIPBOARD IMPORTING 
                            VALUE(i_long_string) TYPE STRING
                            VALUE(i_file) TYPE CSEQUENCE DEFAULT '',
      file_to_string IMPORTING 
                       VALUE(i_file) TYPE CSEQUENCE
                       RETURNING value(result) TYPE STRING,
      CLIPBOARD_TO_STRING IMPORTING 
                            VALUE(i_file) TYPE CSEQUENCE DEFAULT ''
                          RETURNING value(result) TYPE STRING.                
                                 
ENDCLASS. "LCL_JSON_FILE DEFINITION

CLASS LCL_JSON_FILE IMPLEMENTATION.
  METHOD string_to_file.
    "DATA lv_xstring TYPE xstring.
    CALL FUNCTION 'SCMS_STRING_TO_XSTRING'
      EXPORTING
        text   = i_long_string
      IMPORTING
        buffer = data(lv_xstring)
      EXCEPTIONS
        failed = 1
        OTHERS = 2.
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
              WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.

    CONSTANTS con_line_size TYPE i VALUE 255.
    DATA: ls_xtab TYPE x LENGTH con_line_size,
          lt_xtab LIKE STANDARD TABLE OF ls_xtab.

    "DATA lv_strlen TYPE i.
    data(lv_strlen) = xstrlen( lv_xstring ).
    "DATA lv_iterations TYPE i.
    data(lv_iterations) = lv_strlen DIV con_line_size. " same number here as the length of ls_xtab.

    DO lv_iterations TIMES.
      ls_xtab = lv_xstring(con_line_size).
      INSERT ls_xtab INTO TABLE lt_xtab.
      lv_xstring = lv_xstring+con_line_size.
    ENDDO.
    IF lv_xstring IS NOT INITIAL.
*      lv_laststrlen = xstrlen( lv_xstring ).
      ls_xtab = lv_xstring.
      INSERT ls_xtab INTO TABLE lt_xtab.
    ENDIF.

    DATA lv_file TYPE string.
    lv_file = i_file.
    IF lv_file CS '\'.
      CALL FUNCTION 'GUI_DOWNLOAD'
        EXPORTING
          bin_filesize = lv_strlen
          filename     = lv_file
          filetype     = 'BIN'
        TABLES
          data_tab     = lt_xtab.
    ELSE.
      OPEN DATASET lv_file IN BINARY MODE FOR OUTPUT.
      LOOP AT lt_xtab INTO ls_xtab.
        lv_strlen = xstrlen( ls_xtab ).
        TRANSFER ls_xtab TO lv_file LENGTH lv_strlen.
      ENDLOOP.
      CLOSE DATASET lv_file.
    ENDIF.
  ENDMETHOD.

  METHOD STRING_TO_CLIPBOARD.
    CHECK i_file IS NOT INITIAL OR i_long_string IS NOT INITIAL.
    IF i_long_string IS INITIAL.
      i_long_string = LCL_JSON_FILE=>file_to_string( i_file ).
    ENDIF.

* CL_GUI_FRONTEND_SERVICES=>CLIPBOARD_EXPORT
    FIELD-SYMBOLS <table> TYPE STANDARD TABLE.

* send data to frontend
    TYPES ty_text20000 TYPE c LENGTH 20000.
    DATA lt_data TYPE TABLE OF ty_text20000.
    APPEND i_long_string TO lt_data.
    ASSIGN lt_data TO <table>.

    CALL FUNCTION 'DP_CONTROL_ASSIGN_TABLE'
      EXPORTING
        h_cntl                 = handle->h_control
        medium                 = cndp_medium_r3table
        propertyname           = 'ClipBoardDataTable'
      TABLES
        data                   = <table>
      EXCEPTIONS
        dp_error_create        = 1
        dp_error_send_data     = 2
        dp_error_assign        = 3
        dp_error_invalid_param = 4
        dp_error_tabname       = 5
        OTHERS                 = 6.
    IF sy-subrc <> 0.
      RAISE cntl_error.
    ENDIF.

* fill file table at frontend
    "DATA lv_rc TYPE i.
    CALL METHOD handle->call_method
      EXPORTING
        method  = 'ClipboardExport'
        p_count = 0
      IMPORTING
        result  = data(lv_rc)
      EXCEPTIONS
        OTHERS  = 1.
    IF sy-subrc <> 0.
      RAISE cntl_error.
    ENDIF.

    CALL METHOD cl_gui_cfw=>flush
      EXCEPTIONS
        cntl_system_error = 1
        cntl_error        = 2
        OTHERS            = 3.
    IF sy-subrc <> 0 OR lv_rc = -1.
      RAISE cntl_error.
    ENDIF.
  ENDMETHOD.                    

  METHOD file_to_string.
    CONSTANTS con_line_size TYPE i VALUE 255.
    DATA: ls_xtab TYPE x LENGTH con_line_size, 
          lt_xtab LIKE STANDARD TABLE OF ls_xtab.

    DATA lv_file TYPE string.
    lv_file = i_file.

    "DATA lv_strlen TYPE i VALUE IS INITIAL.

    IF lv_file CS '\'.
      CALL FUNCTION 'GUI_UPLOAD'
        EXPORTING
          filename   = lv_file
          filetype   = 'BIN'
        IMPORTING
          filelength = data(lv_strlen)
        TABLES
          data_tab   = lt_xtab.
    ELSE.
      DATA lv_actlen TYPE i.
      OPEN DATASET lv_file IN BINARY MODE FOR INPUT.
      DO.
        READ DATASET lv_file INTO ls_xtab MAXIMUM LENGTH con_line_size ACTUAL LENGTH lv_actlen.
        IF sy-subrc <> 0.
          EXIT.
        ENDIF.
        lv_strlen = lv_strlen + lv_actlen.
        APPEND ls_xtab TO lt_xtab.
      ENDDO.
      CLOSE DATASET lv_file.
    ENDIF.

    CALL FUNCTION 'SCMS_BINARY_TO_STRING'
      EXPORTING
        input_length = lv_strlen
      IMPORTING
        text_buffer  = result
      TABLES
        binary_tab   = lt_xtab
      EXCEPTIONS
        failed       = 1
        OTHERS       = 2.
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE 'E' NUMBER sy-msgno
              WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.
  ENDMETHOD.

  METHOD CLIPBOARD_TO_STRING.
* CL_GUI_FRONTEND_SERVICES=>CLIPBOARD_IMPORT

    CLASS cl_gui_control DEFINITION LOAD.

    DATA table_ref TYPE REF TO data.
    FIELD-SYMBOLS <table> TYPE STANDARD TABLE.

    DATA lt_data TYPE TABLE OF char255.
* send data to frontend
    CALL FUNCTION 'DP_STRETCH_SIMPLE_TABLE'
      EXPORTING
        copy_lines             = ' '
      IMPORTING
        stretched_data_ref     = table_ref
      TABLES
        data                   = lt_data
      EXCEPTIONS
        dp_error_multiple_cols = 1
        dp_error_not_charlike  = 2.
    IF sy-subrc = 0.
      ASSIGN table_ref->* TO <table>.
    ELSE.
      ASSIGN lt_data TO <table>.
    ENDIF.

    CALL FUNCTION 'DP_CONTROL_ASSIGN_TABLE'
      EXPORTING
        h_cntl                 = handle->h_control
        medium                 = cndp_medium_r3table
        propertyname           = 'ClipBoardDataTable'
      TABLES
        data                   = <table>
      EXCEPTIONS
        dp_error_create        = 1
        dp_error_send_data     = 2
        dp_error_assign        = 3
        dp_error_invalid_param = 4
        dp_error_tabname       = 5
        OTHERS                 = 6.
    IF sy-subrc <> 0.
      RAISE cntl_error.
    ENDIF.

* fill file table at frontend
    CALL METHOD handle->call_method
      EXPORTING
        method  = 'ClipboardImport'
        p_count = 0
      EXCEPTIONS
        OTHERS  = 1.
    IF sy-subrc <> 0.
      RAISE cntl_error.
    ENDIF.

* get  table from frontend
    CALL FUNCTION 'DP_CONTROL_GET_TABLE'
      EXPORTING
        h_cntl                 = handle->h_control
        propertyname           = 'ClipBoardDataTable'
        medium                 = cndp_medium_r3table
      TABLES
        data                   = <table>
      EXCEPTIONS
        dp_error_create        = 1
        dp_error_get_property  = 2
        dp_error_get_data      = 3
        dp_error_invalid_param = 4
        OTHERS                 = 5.
    IF sy-subrc <> 0.
      RAISE cntl_error.
    ENDIF.

    CONCATENATE LINES OF <table>[] INTO result.
    CONDENSE result.

    IF i_file IS NOT INITIAL.
      CALL METHOD zcl_json_ws_util=>string_to_file
        EXPORTING
          i_long_string = result
          i_file        = i_file.
    ENDIF.
  ENDMETHOD.                    "
ENDCLASS.