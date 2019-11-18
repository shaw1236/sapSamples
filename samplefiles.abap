** Data read from/stored to a file  
**
** Purpose: Sample to exchange data <=> file
**
** Author : Simon Li  Jul 2017
**
** https://github.com/sapmentors/abap2xlsx
** https://launchpad.support.sap.com/#/notes/933420/E
**
**  !!Code needs to be adjusted before using it!!! 
REPORT ZTEST_FILES 

parameters p_file type char256 obligatory lower case.

types ty_tab type standard table of char2048.
data gt_data type ty_tab.

at selection-screen on value-request for p_file.
*  call function 'F4_FILENAME'
*    exporting
*      field_name = 'P_FILE'
*    importing
*      file_name  = p_file.
   perform f4_excel_file changing p_file.

start-of-selection
  perform readFile using p_file changing gt_data.

* Read a flat file
form readFile using value(i_file) type clike
              changing et_data type ty_tab.
   DATA lv_file type string.
   lv_file = i_file.
   IF lv_file CS '\'.
      "CL_GUI_FRONTEND_SERVICES=>GUI_UPLOAD"
      CALL FUNCTION 'GUI_UPLOAD'
        EXPORTING
          filename   = lv_file
          filetype   = 'ASC'  "'BIN'
        IMPORTING
          filelength = data(lv_strlen)
        TABLES
          data_tab   = et_data.
    ELSE.
      DATA ls_data LIKE LINE OF et_data.
      OPEN DATASET lv_file IN TEXT MODE FOR INPUT.
      DO.
        READ DATASET lv_file INTO ls_data.
        IF sy-subrc <> 0.
          EXIT.
        ENDIF.
        APPEND ls_data TO et_data.
      ENDDO.
      CLOSE DATASET lv_file.
    ENDIF.
ENDFORM.
* Write a flat file
form writeFile using value(i_file) type clike
                     it_data type ty_tab.  
    DATA lv_file TYPE string.
    lv_file = i_file.
    IF lv_file CS '\'.
      "CL_GUI_FRONTEND_SERVICES=>GUI_DOWNLOAD"
      CALL FUNCTION 'GUI_DOWNLOAD'
        EXPORTING
          "bin_filesize = lv_strlen
          filename     = lv_file
          filetype     = 'ASC'   "'BIN'
        TABLES
          data_tab     = it_data.
    ELSE.
      OPEN DATASET lv_file IN TEXT MODE FOR OUTPUT.
      LOOP AT it_data INTO data(ls_data).
        TRANSFER ls_data TO lv_file.
      ENDLOOP.
      CLOSE DATASET lv_file.
    ENDIF.
ENDFORM.
* file search help
FORM f4_excel_file changing e_file type c.  
  data(lv_title) = |Select Excel File, e.g. *.xlsx|.
  constants(con_defaultextension) = |.xlsx|.
  constants(con_filefilter) = `Excel Files (*.xlsx)|*.xlsx`.
  data lt_tab type filetable.
  data lv_returncode type i.

  data lv_workdir type path.
  lv_ workdir = 'C:\'.

  cl_gui_frontend_services=>directory_browse( EXPORTING initial_folder  = lv_workdir
                                              CHANGING  selected_folder = lv_workdir ).
  
  call method cl_gui_frontend_services=>file_open_dialog
    exporting
      window_title      = lv_title
      default_extension = con_defaultextension
*     default_filename  =
*     file_filter       = filefilter
*     with_encoding     =
      initial_directory = lv_workdir
      multiselection    = space
    changing
      file_table        = lt_tab
      rc                = lv_returncode
*     user_action       =
*     file_encoding     =
    exceptions
     file_open_dialog_failed = 1
     cntl_error        = 2
     error_no_gui      = 3
     not_supported_by_gui    = 4
     others            = 5.
  if sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
              WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  endif.

  read table lt_tab assigning field-symbol(<selectedfilename>) index 1.
  if sy-subrc = 0.
    p_file = <selectedfilename>-filename.
  endif.
EDNFORM.
* Read an excel file
form readExcelFile using value(iv_file) type clike
                         value(iv_heading) type i
                   changing ev_rows type i
                            et_data type any table. 

* https://launchpad.support.sap.com/#/notes/933420                            
* ALSMEX_TABLINE 
* 1	ROW	- Lines 		NUMC	4	(1 - 9999)
* 2	COL	- Column		NUMC	4	(1 - 9999)
* 3	VALUE -	Value		CHAR50      50
*
* FM ALSM_EXCEL_TO_INTERNAL_TABLE is enhanced by more rows and long contents
* => COPY "ALSM_EXCEL_TO_INTERNAL_TABLE" to "ZALSM_EXCEL_TO_INTERNAL_TABLE"
* ZALSMEX_TABLINE 
* 1	ROW	- Lines 		NUM10	   (1 - 9999999999) 
* 2	COL	- Column		NUM4	   (1 - 9999)
* 3	VALUE -	Value		CHAR2048   2K
    DATA: lo_table    TYPE REF TO cl_abap_tabledescr,
          lo_struct   TYPE REF TO cl_abap_structdescr,
          lt_comp     TYPE cl_abap_structdescr=>component_table.
    lo_table ?= cl_abap_structdescr=>describe_by_data( et_data ).
    lo_struct ?= lo_table->get_table_line_type( ).
    lt_comp    = lo_struct->get_components( ).
    data(lv_end_col) = LINES( lt_comp ).

    DATA lv_file TYPE localfile.
    lv_file = i_file.

    data lv_begin type i.
    lv_begin = iv_heading + 1. 
    data(iv_end) = 99999.

* !!!The fields of table ET_DATA need match the column sequences of the excel file

*   Upload this packet
    "CALL FUNCTION 'KCD_EXCELL_OLE_TO_INT_CONVERT'  " VALUE: CHAR32
    CALL FUNCTION 'ZALSM_EXCEL_TO_INTERNAL_TABLE'
      EXPORTING
        filename                = lv_file
        i_begin_col             = 1
        i_begin_row             = lv_begin
        i_end_col               = lv_end_col
        i_end_row               = iv_end
      TABLES
        intern                  = data(lt_exceldata)
      EXCEPTIONS
        inconsistent_parameters = 1
        upload_ole              = 2
        OTHERS                  = 3.
*   something wrong, exit
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
              WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.
  
    FIELD-SYMBOLS <field> TYPE ANY. 
*   Move from Row, Col to Flat Structure
    LOOP AT lt_exceldata INTO data(ls_exceldata).
      " Append new row
      AT NEW row.
        APPEND INITIAL LINE TO et_data ASSIGNING field-symbols(<data>).
      ENDAT.
 
      " component and its value
      ASSIGN COMPONENT ls_exceldata-col OF STRUCTURE <data> TO <field>.
      IF sy-subrc EQ 0.
        <field> = ls_exceldata-VALUE.
      ENDIF.
 
      " add the row count
      AT END OF row.
        IF <data> IS NOT INITIAL.
          ev_rows = ev_rows + 1.
        ENDIF.
      ENDAT.
    ENDLOOP. 
ENDFORM.  
* Read an excel alternatively
form readExcelFile2 using value(iv_file) type clike
                          value(iv_heading) type i
                   changing ev_rows type i
                            et_data type any table. 
  data lv_head type xfeld.
  if iv_heading > 0.
    lv_head = abap_true.
  else.
    lv_head = abap_false.
  endif.
  data lv_file TYPE rlgrap-filename.
  lv_file = iv_file.

  DATA lt_raw TYPE truxs_t_text_data.
  " Convert Excel Data to SAP internal Table Data 
  CALL FUNCTION 'TEXT_CONVERT_XLS_TO_SAP'
    EXPORTING
*     I_FIELD_SEPERATOR        =
      i_line_header            =  lv_head
      i_tab_raw_data           =  lt_raw       " WORK TABLE
      i_filename               =  lv_file
    TABLES
      i_tab_converted_data     = et_data[]     "ACTUAL DATA
   EXCEPTIONS
      conversion_failed        = 1
      OTHERS                   = 2.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

  ev_rows = lines( et_data ).
endform.
* Read an excel file from open source abap2xlsx
form readExcelFileabap2xlsx using value(iv_file) type clike
                                  value(iv_heading) type i
                            changing ev_rows type i
                                     et_data type any table. 
* installation 
* please refer to https://github.com/sapmentors/abap2xlsx/wiki/SAPLink-installation
   data: lo_excel type ref to zcl_excel.
   data lv_begin type i default is initial.
   lv_begin = iv_heading + 1.
   try.
     perform abap2xlsxread using p_file space space changing lo_excel.

* Parsed data are usually contained in the following ITAB:
    et_data = LO_EXCEL->WORKSHEETS->WORKSHEET[1]->COLLECTION[lv_begin]->SHEET_CONTENT.
    ev_rows = lines( et_data ).

    catch ZCX_EXCEL into data(lo_CX_EXCEL).
       data(lv_message) = lo_cx_excel->get_text( ).
       write: / | Error: { lv_message } |.  
   endtry.     
endform.

form abap2xlsxread using value(i_file)     type csequence
                         value(i_from_app) type abap_bool
                         value(i_huge)     type abap_bool
                   changing e_excel        type ref to zcl_excel
                   raising zcx_excel.
* Use the reader instance as a local variable in a separate unit,
* so its memory will be released after leaving the unit.
   data lo_reader type ref to zif_excel_reader.
   if i_huge = abap_true.
     create object lo_reader type zcl_excel_reader_huge_file.
   else.
     create object lo_reader type zcl_excel_reader_2007.
   endif.
   e_excel = lo_reader->load_file( i_filen    = i_filename
                                   i_from_app = i_from_applserver ).
endform.                    