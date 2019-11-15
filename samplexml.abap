** Name   : ztest_xml_file
** Purpose: XML File read and write
** Author : Simon Li
** Date   : Oct 20, 2007
**======================================================================
REPORT  ztest_xml_file NO STANDARD PAGE HEADING LINE-SIZE 150.

TYPE-POOLS: ixml, sabc.

CONSTANTS con_max_xml_len     TYPE i VALUE 255.
CONSTANTS con_xml_test_file   TYPE string VALUE '/filetransfer/sapwrite/Test/test_xml_file.xml'.

TYPES: BEGIN OF ty_person,
         firstname TYPE string,
         lastname  TYPE string,
       END OF ty_person.

DATA: BEGIN OF unix_command,
        compress(80)       VALUE 'compress',
        uncompress(80)     VALUE 'uncompress',
        extension(10)      VALUE '.Z',

*        compress(80)       VALUE 'gzip',
*        uncompress(80)     VALUE 'gunzip',
*        extension(10)      VALUE '.gz',

*        compress(80)       VALUE 'bzip2',
*        uncompress(80)     VALUE 'bunzip2',
*        extension(10)      VALUE '.bz2',
        END OF unix_command.

DATA: g_xml_table TYPE solix_tab,
      g_xml_size  TYPE i.

DATA g_ixml          TYPE REF TO if_ixml.
DATA g_streamfactory TYPE REF TO if_ixml_stream_factory.
DATA g_document      TYPE REF TO if_ixml_document.
DATA g_istream       TYPE REF TO if_ixml_istream.
DATA g_ostream       TYPE REF TO if_ixml_ostream.
DATA g_parser        TYPE REF TO if_ixml_parser.

DATA g_file    TYPE string VALUE 'c:\work\test_out.xml'.

DATA gt_person TYPE TABLE OF ty_person WITH HEADER LINE.

INITIALIZATION.

* using the iXML library
  g_ixml = cl_ixml=>create( ).

* Creating a stream factory and streams
  g_streamfactory = g_ixml->create_stream_factory( ).

* Creating a document
  g_document = g_ixml->create_document( ).

  gt_person-lastname = 'Zhange'. gt_person-firstname = '3'. APPEND gt_person.
  gt_person-lastname = 'Lee'. gt_person-firstname = '4'. APPEND gt_person.
  gt_person-lastname = 'Wang'. gt_person-firstname = '5'. APPEND gt_person.
  gt_person-lastname = 'Zhao'. gt_person-firstname = '6'. APPEND gt_person.

  DO 35 TIMES.
    gt_person-firstname = sy-index. gt_person-lastname = sy-index. APPEND gt_person.
  ENDDO.

  CONCATENATE con_xml_test_file unix_command-extension INTO g_file.

START-OF-SELECTION.
  break sli.

  PERFORM generate_xml_data.
  PERFORM write_xml_file.

  REFRESH: g_xml_table, gt_person.

  PERFORM read_xml_file.
  PERFORM get_xml_data.

  LOOP AT gt_person.
    WRITE: / gt_person-firstname, gt_person-lastname.
  ENDLOOP.
*&---------------------------------------------------------------------*
*&      Form  write_xml_file
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM write_xml_file.

  DATA lv_file TYPE authb-filename.
  lv_file = g_file.
  CALL FUNCTION 'AUTHORITY_CHECK_DATASET'
    EXPORTING
*     PROGRAM                =
      activity               = sabc_act_write
      filename               = lv_file
   EXCEPTIONS
     no_authority           = 1
     activity_unknown       = 2
     OTHERS                 = 3.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

  DATA cx_root TYPE REF TO cx_root.
  DATA message TYPE string.

  DATA xml_line  LIKE LINE OF g_xml_table.
* Rendering into a table-based stream
  DATA actlen    TYPE i.

  TRY.
*      OPEN DATASET g_file IN BINARY MODE FOR OUTPUT FILTER 'compress'.
*      OPEN DATASET g_file IN BINARY MODE FOR OUTPUT FILTER 'gzip'.
      OPEN DATASET g_file IN BINARY MODE FOR OUTPUT FILTER unix_command-compress.
      LOOP AT g_xml_table INTO xml_line.
        IF g_xml_size > con_max_xml_len.
          actlen = con_max_xml_len.
        ELSE.
          actlen = g_xml_size.
        ENDIF.

        TRANSFER xml_line TO g_file LENGTH actlen.
        SUBTRACT actlen FROM g_xml_size.
      ENDLOOP.

      CLOSE DATASET g_file.

    CATCH cx_root INTO cx_root.
      CLOSE DATASET g_file.
      message = cx_root->get_text( ).
      WRITE : / message.

  ENDTRY.
ENDFORM.                    "write_xml_file
*&---------------------------------------------------------------------*
*&      Form  read_xml_file
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM read_xml_file.
  DATA lv_file TYPE authb-filename.
  lv_file = g_file.
  CALL FUNCTION 'AUTHORITY_CHECK_DATASET'
    EXPORTING
*     PROGRAM                =
      activity               = sabc_act_read
      filename               = lv_file
   EXCEPTIONS
     no_authority           = 1
     activity_unknown       = 2
     OTHERS                 = 3.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.
  DATA cx_root TYPE REF TO cx_root.
  DATA message TYPE string.

  DATA xml_line  LIKE LINE OF g_xml_table.
* Rendering into a table-based stream
  DATA actlen    TYPE i.
  DATA end_reading VALUE IS INITIAL.

  TRY.
*      OPEN DATASET g_file IN BINARY MODE FILTER 'uncompress' FOR INPUT.
*      OPEN DATASET g_file IN BINARY MODE FILTER 'gunzip' FOR INPUT.
      OPEN DATASET g_file IN BINARY MODE FOR INPUT FILTER unix_command-uncompress.

      DO.
*      Read XML line into internal table.
        READ DATASET g_file MAXIMUM LENGTH con_max_xml_len
             INTO xml_line ACTUAL LENGTH actlen.
        IF sy-subrc <> 0.
          end_reading = 'X'.
        ENDIF.
        ADD    actlen TO g_xml_size.
        APPEND xml_line TO g_xml_table.

        IF end_reading = 'X'.
          EXIT.
        ENDIF.
      ENDDO.
      CLOSE DATASET g_file.

    CATCH cx_root INTO cx_root.
      CLOSE DATASET g_file.
      message = cx_root->get_text( ).
      WRITE : / message.

  ENDTRY.
ENDFORM.                    "read_xml_file
*&---------------------------------------------------------------------*
*&      Form  generate_xml_data
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM generate_xml_data.
** Creating a document to hold the DOM tree
  DATA: group   TYPE REF TO if_ixml_element,
        person  TYPE REF TO if_ixml_element,
        element TYPE REF TO if_ixml_element.

  group   = g_document->create_simple_element(
            name = 'Group'
            parent = g_document ). " this is the root node!

  LOOP AT gt_person.
    person  = g_document->create_simple_element(
              name = 'Person'
              parent = group ).

    element = g_document->create_simple_element(
                name = 'firstname'
                value = gt_person-firstname
                parent = person ). " parent is NOT initial!

    element = g_document->create_simple_element(
                name = 'lastname'
                value = gt_person-lastname
                parent = person )." parent is NOT initial!
  ENDLOOP.

* Rendering into a table-based stream
  g_ostream = g_streamfactory->create_ostream_itable( g_xml_table ).
  g_document->render( ostream = g_ostream  recursive = 'X' ).

* use only the first xml_size bytes of the xml table!!
  g_xml_size = g_ostream->get_num_written_raw( ).
ENDFORM.                    "generate_xml_data
*&---------------------------------------------------------------------*
*&      Form  get_xml_data
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM get_xml_data.

* Wrap the table containing the file into a stream.
  g_istream = g_streamfactory->create_istream_itable(
                                      table = g_xml_table
                                      size  = g_xml_size ).
* Creating an XML parser
  g_parser = g_ixml->create_parser( stream_factory = g_streamfactory
                                  istream        = g_istream
                                  document       = g_document ).

* Parsing an XML document DOM-based
  g_parser->parse( ).

  DATA: group   TYPE REF TO if_ixml_element,
        node    TYPE REF TO if_ixml_node,
        element TYPE REF TO if_ixml_node.

  DATA: name TYPE string, val TYPE string.

  group  = g_document->get_root_element( ).
  name = group->get_name( ).

  node = group->get_first_child( ).

* Go through the XML tree.
  WHILE node IS NOT INITIAL.
    name = node->get_name( ).
    val  = node->get_value( ).

    IF name = 'Person'.
      element = node->get_first_child( ).
      WHILE element IS NOT INITIAL.
        name  = element->get_name( ).
        val   = element->get_value( ).
        CASE name.
          WHEN 'firstname'.
            gt_person-firstname = val.
          WHEN 'lastname'.
            gt_person-lastname = val.
        ENDCASE.
        element = element->get_next( ).
      ENDWHILE.
      APPEND gt_person.
    ENDIF.

    node = node->get_next( ).

  ENDWHILE.
ENDFORM.                    "get_xml_data