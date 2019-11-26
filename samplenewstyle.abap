** ABAP New PRogram Style for Open SQL 
**
** Purpose: Sample to some new style coding
**
** Author : Simon Li  Nov 2018
**
report test_report no standard page heading line-size 120 message-id zme.

tables jkap.

* load-of-program -> initialization -> pbo -> dialog mode -> pai -> 
* pai on -> start-of-selection -> end-of-selection
at selection-screen begin of block b1.
parameters p_keydat TYPE sy-datum obligatory.
select-options so_vbeln for jkap-vbeln.
at selection-screen end of block b1.

load-of-program.

initialization.
   AUTHORITY-CHECK OBJECT auth_obj 
    ID id1 DUMMY
    ID ACTIV_AUTH '01'.
   if sy-subrc <> 0.
      message 'No authority to run' type 'E' display like 'S'.
   endif.

   p_keydat = sy-datum.
   p_keydat(4) = p_keydat(4) - 1.

* PBO
at selection-screen output.
  loop at screen.
  endloop.

* PAI
at selection-screen.

at selection-screen on so_vbeln-low.

at line-selection.

start-of-selection.
   perform test1.
   
end-of-selection.

form test1.
  select mandt,
         vbeln,
         posnr,
         posex,
         poart,
         guelgivon,
         guelgibis
    from jkap into table @data(lt_data)
    using client '135'
    where vbeln in @so_vbeln
      and poart in ('U', 'R')
      and guelgivon <= @p_keydat
      and guelgibis >= @p_keydat.

  loop at lt_data assigning field-symbols <data>.
  "loop at lt_data into data(ls_data).
    <data>-guelgibis = <data>-guelgibis + 1.
  endloop.
endform. 