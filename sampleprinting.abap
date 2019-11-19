report zprint.

FORM print_smartform using value(i_form) TYPE formname 
                           it_data type ty_data.
  CALL FUNCTION 'SSF_FUNCTION_MODULE_NAME'
    EXPORTING
      formname                 = i_form
*     VARIANT                  = ' '
*     DIRECT_CALL              = ' '
    IMPORTING
      FM_NAME                  = data(lv_fname)
    EXCEPTIONS
      NO_FORM                  = 1
      NO_FUNCTION_MODULE       = 2
      OTHERS                   = 3.
  IF sy-subrc <> 0.
    MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
            WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.

  CALL FUNCTION 'SSF_GET_DEVICE_TYPE'
    EXPORTING
      i_language          = sy-langu
      i_application       = 'SAPDEFAULT'
    IMPORTING
      e_devtype           = data(lv_devtype).

  data ls_output_options TYPE ssfcompop.
  clear ls_output_options.
  ls_output_options-tdprinter = lv_devtype.
  ls_control_parameters-no_dialog = 'X'.
  call function fm_name
    EXPORTING 
      output_options     = ls_output_options
      DATA               = it_data
    EXCEPTIONS
      FORMATTING_ERROR           = 1
      INTERNAL_ERROR             = 2
      SEND_ERROR                 = 3
      USER_CANCELED              = 4
      OTHERS                     = 5.
   IF SY-SUBRC <> 0.
     MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
             WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
   ENDIF.
ENDFORM.

form print_sapscript value(i_form) TYPE formname 
                           it_data type ty_data.
  CALL FUNCTION 'OPEN_FORM'
    EXPORTING
      APPLICATION = 'TX'
      DEVICE      = 'PRINTER'
      DIALOG      = 'X'
      FORM        = i_form
      LANGUAGE    = SY-LANGU  
    EXCEPTIONS
      CANCELED = 1
      DEVICE   = 2
      FORM     = 3
      OPTIONS  = 4
      UNCLOSED = 5
      MAIL_OPTIONS = 6
      ARCHIVE_ERROR = 7
      INVALID_FAX_NUMBER = 8
      MORE_PARAMS_NEEDED_IN_BATCH = 9
      SPOOL_ERROR = 10
      CODEPAGE = 11
      OTHERS = 12.
  IF sy-subrc <> 0.
    MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
            WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.

  CALL FUNCTION 'WRITE_FORM'
    EXPORTING
      ELEMENT  = '100'
      FUNCTION = 'SET'
      TYPE     = 'BODY'
      WINDOW   = 'MAIN'
    EXCEPTIONS
      ELEMENT = 1
      FUNCTION = 2
      TYPE = 3
      UNOPENED = 4
      UNSTARTED = 5
      WINDOW = 6
      BAD_PAGEFORMAT_FOR_PRINT = 7
      SPOOL_ERROR = 8
      CODEPAGE = 9
      OTHERS = 10.
  IF sy-subrc <> 0.
    MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
            WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.

  CALL FUNCTION 'CLOSE_FORM'
    EXCEPTIONS
    UNOPENED = 1
    BAD_PAGEFORMAT_FOR_PRINT = 2
    SEND_ERROR = 3
    SPOOL_ERROR = 4
    CODEPAGE = 5
    OTHERS = 6.
  IF sy-subrc <> 0.
    MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
            WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.
endform.

FORM save_smartform_pdf using value(i_form) TYPE formname
                              value(i_file) type csequence 
                              it_data type ty_data.
  CALL FUNCTION 'SSF_FUNCTION_MODULE_NAME'
    EXPORTING
      formname                 = i_form
*     VARIANT                  = ' '
*     DIRECT_CALL              = ' '
    IMPORTING
      FM_NAME                  = data(lv_fname)
    EXCEPTIONS
      NO_FORM                  = 1
      NO_FUNCTION_MODULE       = 2
      OTHERS                   = 3.
  IF sy-subrc <> 0.
    MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
            WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.

  CALL FUNCTION 'SSF_GET_DEVICE_TYPE'
  EXPORTING
    i_language          = sy-langu
    i_application       = 'SAPDEFAULT'
  IMPORTING
    e_devtype           = data(lv_devtype).

  data ls_output_options TYPE ssfcompop.
  ls_output_options-tdprinter = lv_devtype.
  ls_control_parameters-no_dialog = 'X'.
  ls_control_parameters-getotf = 'X'.
  call function fm_name
    EXPORTING 
      output_options     = ls_output_options
      DATA               = it_data
    IMPORTING
      job_output_info    = data(ls_job_output_info)
    EXCEPTIONS
      FORMATTING_ERROR           = 1
      INTERNAL_ERROR             = 2
      SEND_ERROR                 = 3
      USER_CANCELED              = 4
      OTHERS                     = 5.
  IF SY-SUBRC <> 0.
    MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
            WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.

*.........................CONVERT TO OTF TO PDF.......................*
  CALL FUNCTION 'CONVERT_OTF_2_PDF'   " 'CONVERT_OTF'.
    IMPORTING
      bin_filesize   = data(lv_bin_filesize)
    TABLES
      otf            = ls_job_output_info-otfdata
      lines          = data(lt_lines)
    EXCEPTIONS
      err_conv_not_possible  = 1
      err_otf_mc_noendmarker = 2
      OTHERS                            = 3.
  IF SY-SUBRC <> 0.
     MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
             WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.

  DATA lv_file TYPE string.
  lv_file = i_file.
  IF lv_file CS '\'.
    CALL FUNCTION 'GUI_DOWNLOAD'
      EXPORTING
        bin_filesize = lv_bin_filesize
        filename     = lv_file
        filetype     = 'BIN'
      TABLES
        data_tab     = lt_lines.
  ELSE.
    OPEN DATASET lv_file IN BINARY MODE FOR OUTPUT.
    LOOP AT lt_line INTO data(ls_line).
      data(lv_strlen) = xstrlen( ls_line ).
      TRANSFER ls_line TO lv_file LENGTH lv_strlen.
    ENDLOOP.
    CLOSE DATASET lv_file.
  ENDIF.
ENDFORM.

form print_adobeform using value(i_form) type formname
                           it_data type ty_data. 
*https://wiki.scn.sap.com/wiki/display/ABAP/Adobe+Forms+from+Scratch
* Adobe form - interface and form
*Master Pages
* Every form design contains at least one master page that Live Cycle Designer creates automatically.
* Master pages define the orientation and dimensions of body pages.
* Master pages are responsible for formatting body pages.
* Provide a background and layout format for more than one of the body pages in a form design.
* Each master page is created with a default content area that covers the whole page.

*Body pages
* Body pages represent the pages of a form.
* Each body page derives its page size and orientation from a master page.
* Each body page is associated with the default master page that LiveCycle Designer creates.
* You can choose which master page to assign to a body page.

*Content areas on body page
* Content areas define where objects can be placed or laid down on body pages.
* When you design a form, you cannot place an object on a body page unless 
* it is inside the area bounded by a content area.
* You can add content areas to master pages only.

*Sub Forms on body page
* Subforms are container objects that you can use to group form design objects including: fields,address,images etc.
* A subform provides anchoring, layout, and geometry management for objects.
* You can also configure subform objects to be repeatable.

* Sets the output parameters and opens the spool job
  DATA LS_OUTPUTPARAMS TYPE SFPOUTPUTPARAMS.
  CALL FUNCTION 'FP_JOB_OPEN'
    CHANGING
      IE_OUTPUTPARAMS       = LS_OUTPUTPARAMS
    EXCEPTIONS
      CANCEL                = 1
      USAGE_ERROR           = 2
      SYSTEM_ERROR          = 3
      INTERNAL_ERROR        = 4
      OTHERS                = 5.
    IF SY-SUBRC <> 0.
      MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
              WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
    ENDIF.
 
* Get the name of the generated function module
    CALL FUNCTION 'FP_FUNCTION_MODULE_NAME'
      EXPORTING
        I_NAME             = i_form
      IMPORTING
        E_FUNCNAME         = data(lv_fm_name).
    IF SY-SUBRC <> 0.
      MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
              WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
   ENDIF.
 
* Call the generated function module
   DATA LS_DOCPARAMS TYPE SFPDOCPARAMS.
   CALL FUNCTION lv_fm_name
     EXPORTING
       /1BCDWB/DOCPARAMS      = LS_DOCPARAMS
       DATA                   = it_data
*   IMPORTING
*     /1BCDWB/FORMOUTPUT       =
     EXCEPTIONS
       USAGE_ERROR           = 1
       SYSTEM_ERROR          = 2
       INTERNAL_ERROR        = 3.
   IF SY-SUBRC <> 0.
     MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
             WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
   ENDIF.
 
* Close the spool job
   CALL FUNCTION 'FP_JOB_CLOSE'
*    IMPORTING
*    E_RESULT             =
     EXCEPTIONS
       USAGE_ERROR           = 1
       SYSTEM_ERROR          = 2
       INTERNAL_ERROR        = 3
       OTHERS                = 4.
   IF SY-SUBRC <> 0.
     MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
             WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
   ENDIF.
endform.