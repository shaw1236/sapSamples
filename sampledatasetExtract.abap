** Extract datasets
**
** Purpose: Sample code for extracting data with datasets 
**
** Author : Simon Li  Oct 2014
**
* https://help.sap.com/saphelp_xrpm450/helpdata/en/9f/db9f0535c111d1829f0000e829fbfe/content.htm?no_cache=true
*
* Once you have declared the possible record types as field groups and defined their structure,
* FIELD-GROUPS: HEADER, fg1, fg2, ..., fgn.
* INSERT <struc1>-fld1 <struc1>-fld2 <struc2>-fld1 ... INTO HEADER.
* fields in fg2 are exactly same as those in HEADER
* INSERT <struc1>-fld3 <struc2>-fld2 ... INTO fg1.   
* INSERT <struc1>-fld4 ... INTO fg3.
* INSERT ... INTO fgn. 
* you can fill the extract dataset using the following statements: EXTRACT fg.
* When the first EXTRACT statement occurs in a program, the system creates the extract dataset and adds the first
* extract record to it. In each subsequent EXTRACT statement, the new extract record is added to the dataset.
* Each extract record contains exactly those fields that are contained in the fg field group, plus perhaps the
* fields of the header field group. The fields occur as a sort key at the beginning. 

* If you do not explicitly specify an fg field group, the EXTRACT statement is a shortened form of the statement 
* EXTRACT header.
*
* When you extract the data, the record is filled with the current values of the corresponding fields.
* As soon as the system has processed the first EXTRACT statement for an fg field group, the structure of 
* the corresponding extract record in the extract dataset is fixed. You can no longer insert new fields into
* the fg and header field groups. If you try to modify one of the field groups afterwards and use it in 
* another EXTRACT statement, a runtime error occurs.
*
* By processing EXTRACT statements several times using different field groups, you fill the extract dataset 
* with records of different length and structure. Since you can modify field groups dynamically up to their first 
* usage in an EXTRACT statement, extract datasets provide the advantage that you need not determine the structure
* at the beginning of the program.
REPORT zdemo_extract_extract.

NODES: spfli, sflight.

* Define three field groups, header is for sorting
FIELD-GROUPS: header, flight_info, flight_date.

* The INSERT statement assigns fields to two of the field groups.

* "header":
* contains threee fields spfli-carrid, spfli-connid, sflight-fldate.

* "flight_info"
* The records of the field group "flight_info" consist of five fields: 
* spfli-carrid, spfli-connid, sflight-fldate, spfli-cityfrom, and spfli-cityto.
* The first three fields belong to the prefixed field group header. 

* "flight_date" 
* The records of the field group consist only of the three fields 
* of the header field group. 
INSERT: spfli-carrid spfli-connid sflight-fldate INTO header,
        spfli-cityfrom spfli-cityto INTO flight_info.

START-OF-SELECTION.

* During the GET events, the system fills the extract dataset of flight_info 
GET spfli.  " fields carrid connid cityfrom cityto
  EXTRACT flight_info.

* the system fills the extract dataset of flight_date 
GET sflight. " fields fldate
  EXTRACT flight_date.  " fill in header and flight_date

end-of-selection.
sort. " based on the header fields

loop at.
    at flight_date.
        write: / spfli-carrid, spfli-connid, sflight-fldate.
    endat.
    write: / spfli-cityfrom, spfli-cityto.
endloop.

*REPORT ZMSD_KEY_EXTRACT.
*
** MSD Case:
*TABLES: JKAK, JKAP, JKEP, JKKD.
*
*FIELD-GROUPS: HEADER, ORDER, ITEM.
*
*INSERT JKAK-VBELN JKAP-POSEX JKAP-POSNR INTO HEADER.
*
*INSERT JKAK-AUART JKAK-GPAG JKAK-WAERK JKAK-KNUMV JKAK-BNAME 
*       JKAK-POSNR_LAST JKAK-POSEX _LAST INTO ORDER.
*
*INSERT JKAP-DRERZ JKAP-PVA JKAP-POART JKAP-KUNWE
*       JKAP-GUELTIGVON JKAP-GUELTIGBIS JKAP-BEZUGSTYP
*       JKAP-POSNR_UR JKAP-VBELN_VL JKAP-REFBELEG JKAP-LIEFERART   
*       JKKD-KDGRP JKKD-KONDA JKKD-RCODE JKKD-PLTYP
*       JKKD-FKPER JKKD-XAUTO_REN JKKD-XPAYMODE_FIX JKEP-BEZPER
*       JKEP-ETMENGE JKEP-ETMEINS 
*       JKEP-NETWR JKEP-MWSBP JKEP-NETPR INTO ITEM.
*
*SELECT-OPTIONS SO_VBELN FOR JKAK-VBELN OBLIGATORY.
* 
*SELECT * FROM JKAK 
*  WHERE VBELN IN SO_VBELN.
*   
*  SELECT * FROM JKAP
*    WHERE VBELN = JKAK-VBELN.
*    
*    SELECT SINGLE * FROM JKKD
*      WHERE VBELN = JKAP-VBELN.
*        AND POSNR = JKAP-POSNR.
*
*    SELECT SINGLE * FROM JKEP
*      WHERE VBELN = JKAP-VBELN.
*        AND POSNR = JKAP-POSNR
*        AND ETENR = JKAP-ETENR_LAST.
*
*    EXTRACT ITEM.  " fill in header and item     
*  ENDSELECT. " End of JKAP
*  EXTRACT ORDER.   " fill in order
*ENDSELECT. " End of JKAK
*
*SORT.  " Sort by VBELN POSEX POSNR
*
*CLEAR: JKAK, JKAP, JKKD, JKEP.
*LOOP.
*  AT ORDER.
*    WRITE: / JKAK-VBELN, JKAP-POSEX, JKAP-POSNR, JKAK-GPAG.
*  ENDAT.
*    
*  WRITE: / JKAP-DRERZ, JKAP-PVA, JKAP-POART, JKAP-KUNWE.
*ENDLOOP.