** SAP HR Reporting 
**
** Purpose: Sample to use PNPCE LDB
**
** Author : Simon Li  Nov 2008
**
** https://wiki.scn.sap.com/wiki/display/ERPHCM/List+of+TABLES++in+SAP+HCM
REPORT  ZPNPCE.

INFOTYPES: 0001, 0006.

NODES: PERAS, PERSON.

*TABLES PERNR.

DATA: begin of gt_data occurs 2000,
        PERNR type P0006-PERNR, 
        UNAME type P0001-UNAME, 
        STRAS type p0006-STRAS, 
        ORT01 type P0006-ORT01,
      end of gt_data.

START-OF-SELECTION.

GET PERAS.
  RP-PROVIDE-FROM-LAST: P0001 SPACE PN-BEGDA PN-ENDDA,
                        P0006 SPACE PN-BEGDA PN-ENDDA.
  clear gt_data.
  MOVE-CORRESPONDING: p0001 TO gt_data,
                      p0006 to gt_data.
  append gt_data.
  
start-of-selection.
  perform alv_show.

form alv_show.
  DATA lo_salv_table TYPE REF TO cl_salv_table.
  TRY.
     cl_salv_table=>factory( IMPORTING r_salv_table = lo_salv_table CHANGING t_table = gt_data[] ).
     data(lo_functions) = lo_salv_table->get_functions( ).
     lo_functions->set_all( abap_true ).

     lo_salv_table->display( ).

    CATCH cx_root INTO data(lo_cx_root).                    "#EC CATCH_ALL
      data(lv_message) = lo_cx_root->get_text( ).
      MESSAGE lv_message TYPE 'E'.
  ENDTRY.
endform.  