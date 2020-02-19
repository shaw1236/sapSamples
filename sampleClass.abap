** ABAP OO: interface, class, event, encapsulation, inheritance, polymorphism, etc  
**
** Purpose: Sample to create class, interface and use them
**
** Author : Simon Li  Jan 2019
**
* 1. Create two interfaces: if_display_area, if_display_szie
*    define two apis: get_area() and get_size() respectively
* 2. Integrate the two interfaces into a new interface if_display
* 3. Create a class rectanlge to implement the interface methods,
*    track any new creation on the class and instance level
* 4. Create a sub class square to overwrite the two methods
* 5. Create a class to receive the events and collect the rectange instanaces
* 6. some Sample code for how to use the class and instances
* 7. A friend class is created for testing
interface zif_display_area.
   methods get_area returning value(result) type i. 
endinterface.

interface zif_display_size.
   methods get_size returning value(result) type i. 
endinterface.

interface zif_display.
   constants math_pi type f value '3.1415926'.
   interfaces: zif_display_area, if_display_size.
endinterface.

* CLASS zcl_rectangle DEFINITION public create public global friends zcl_test_rectangle.
class zcl_test_rectangle definition deferred.
class zcl_rectangle definition friends zcl_test_rectangle.
  public section.  " access to any 
    " Events
    class-events increased.
    events rectangle_created.

    interfaces zif_display.  " interface
    aliases disp_area for zif_display_area~get_area. " alias method

    class-methods: class_constructor,
                   get_count_of_instances returning value(result) type i.

    methods: constructor importing value(length) type i value(width) type i,
             get_length returning value(length) type i,
             get_width returning value(width) type i,
             set_length importing value(length) type i,
             set_width importing value(width) type i. 

  protected section. " access to same class or derived classes
    data: length type i,
          width type i.

  private section.   " access to the same class only
    class-data number_of_instance type i.
endclass.

* CLASS zcl_square DEFINITION inheriting from zcl_rectangle PUBLIC FINAL CREATE PUBLIC .
class zcl_square definition inheriting from zcl_rectangle final.
  public section.
    methods: constructor importing value(side) type i,
             get_side returning value(side) type i,
             set_side importing value(side) type i.

    methods: zif_display_area~get_area redefinition,
             zif_display_size~get_size redefinition.
endclass.

class zcl_rectangle implementaion.
    method class_constructor. 
      number_of_instance = 0.
      
      "data(lo_rec) = new zcl_rectangle( length = 10 width = 2 ). 
      "data(lv_area) = lo_rec->disp_area( ).  " alias 
    endmethod.
    
    method get_count_of_instances. 
      result = number_of_instance. 
    endmethod.
    
    method constructor.
      me->length = length.
      me->width = width.
      raise event rectangle_created. " publish the event to the listeners

      " class wide
      number_of_instance = number_of_instance + 1.
      raise event increased.         " publish the class event to the listeners
    endmethod.

    method get_length. 
      length = me->length. 
    endmethod.  
    
    method get_width.  
      width = me->width.   
    endmethod.
    
    method set_length. 
      me->length = length. 
    endmethod.
    
    method set_width.  
      me->width = width.   
    endmethod. 
    
    method zif_display_area~get_area. 
      result = length * width. 
    endmethod.
    
    method zif_display_size~get_size.
      result = ( length + width ) * 2.
    endmethod.  
endclass.

class zcl_square implementaion.
  method constructor.
    call method super->constructor( length = side width = side ). 
  endmethod.
  
  method get_side. 
    side = length. 
  endmethod.
  
  method set_side. 
    length = width = side. 
  endmethod.
  
  method zif_display_area~get_area.
    "result = super->if_display_area~get_area( ).
    result = length * length.
  endmethod.

  method zif_display_size~get_size.
    result = length * 4.
  endmethod.
endclass.

* https://help.sap.com/doc/abapdocu_751_index_htm/7.51/de-DE/index.htm?file=abapclass_for_testing.htm
* CLASS class DEFINITION FOR TESTING [RISK LEVEL {CRITICAL|DANGEROUS|HARMLESS}]
*                                    [DURATION   {SHORT|MEDIUM|LONG}].
* CLASS zcl_test_rectangle DEFINITION for testing duration medium risk level harmless PUBLIC FINAL CREATE PUBLIC .
class zcl_test_rectangle definition final for testing 
  duration medium 
  risk level harmless.
  
  public section.
    interfaces if_oo_adt_classrun.
    class-methods checking.
endclass.

CLASS zcl_test_rectangle IMPLEMENTATION.
  method if_oo_adt_classrun~main.
    try.
      out->write( `Verifying...` ).
      checking( ).
      catch cx_root.
    endtry.
  endmethod.
  method checking.
    DATA(lo_rect) = new zcl_rectangle( length = 10 width = 20 ).

    cl_abap_unit_assert=>assert_equals( act = lo_rect->length exp = 10 ).
    cl_abap_unit_assert=>assert_equals( act = lo_rect->width exp = 20 ).
    cl_abap_unit_assert=>assert_equals( act = lo_rect->disp_area( ) exp = 200 ).
    cl_abap_unit_assert=>assert_equals( act = lo_rect->zif_display_size~get_size( ) exp = 60 ).
    cl_abap_unit_assert=>assert_equals( act = zcl_rectangle=>number_of_instance exp = 1 ).

    data lo_rect2 type ref to zcl_rectangle. 
    lo_rect2 = new zcl_square( 5 ).
    data lo_square1 type ref to zcl_square.
    try.
      lo_square1 ?= lo_rect2. " Widening cast
      cl_abap_unit_assert=>assert_bound( act = lo_rect2 ).
      cl_abap_unit_assert=>assert_bound( act = lo_square1 ).
      cl_abap_unit_assert=>assert_equals( act = lo_square1->get_side( ) exp = 5 ).
      cl_abap_unit_assert=>assert_equals( act = zcl_rectangle=>number_of_instance exp = 2 ).
      CATCH cx_sy_move_cast_error.
        cl_abap_unit_assert=>abort( msg = | cast error | ).
    endtry.
  endmethod.  
 ENDCLASS.

class zcl_collector definition.
  public section.
    interfaces if_oo_adt_classrun.
    types rectangle_tab type table of ref to zcl_rectangle with empty key.
    methods constructor.
    methods get_rectangles returning value(result) type rectangle_tab.

  private section.
    " Event handler for rectangle created
    methods add_rectangle for event rectangle_created of zcl_rectangle
              importing sender.

    data t_rectangle type rectangle_tab. 
endclass.

CLASS zcl_collector IMPLEMENTATION.
  method constructor.
    " Registering for an event
    set handler add_rectangle for all instances.
  endmethod.
  method get_rectangles. result = t_rectangle[]. endmethod.
  method add_rectangle. append sender to t_rectangle. endmethod.

  method if_oo_adt_classrun~main.
     try.
          test1( out = out ).
      catch cx_root into data(lo_root).
          data(lv_message) = lo_root->get_text( ).
          out->write( | message: { lv_message }| ).
     endtry.
  endmethod.

  method test1.
    " Set up the collector to listen the creation of rectangles
    "create object lo_collector.
    data(lo_collector) = new zcl_collector( ).

    DATA: lo_rec1 TYPE REF TO zcl_rectangle,
          lo_rec2 TYPE REF TO zcl_rectangle,
          lif_rec  TYPE REF TO zif_display.

    DATA: lo_square1 TYPE REF TO zcl_square,
          lo_square2 TYPE REF TO zcl_square.

    "lo_rec1 = new rectangle( length = 12 width = 2 ). " collector handler called
    "lo_rec1 = NEW #( length = 12 width = 2 ).
    create object lo_rec1 exporting
                             length = 12
                             width = 2.
    out->write( |The number of rectangle collected is { lines( lo_collector->get_rectangles( ) ) }| ).
    lo_square1 = new zcl_square( side = 6 ).  " collector handler called
    out->write( |The number of rectangle collected is { lines( lo_collector->get_rectangles( ) ) }| ).
                            
    " Narrowing cast: down
    " a derived class instance to interface, abstract, supper class
    lif_rec = lo_square1.
    data(lv_area) = lif_rec->zif_display_area~get_area( ).
    out->write( |area = { lv_area }| ).
    lo_rec2 = lo_square1.  " Narrowing cast again

    " Widening cast: up
    " interface, abstract, supper class to a derived class variable
    try.
      lo_square2 ?= lif_rec.        " widening cast I
      "move lo_rec2 ?to lo_square2.  " alternatively, widening cast II
      lo_square2 ?= lo_rec2.
      CATCH cx_sy_move_cast_error.
      " react on that cast error
    ENDTRY.

    lif_rec = lo_rec1.
    lv_area = lif_rec->zif_display_area~get_area( ).
    out->write( |area = { lv_area } | ).

    data(lv_count) = zcl_rectangle=>get_count_of_instances( ).
    out->write( |The number of rectangle created is { lv_count }| ).

    lv_count = lines( lo_collector->get_rectangles( ) ).
    out->write( |The number of rectangle collected is { lv_count }| ).

    field-symbols <rect> TYPE REF TO zcl_rectangle.
    loop at lo_collector->get_rectangles( ) assigning <rect>.
      data(lv_size) = <rect>->zif_display_size~get_size( ).
      data(lv_length) = <rect>->get_length( ).
      data(lv_width) = <rect>->get_width( ).

      try.
          lo_square2 ?= <rect>.
          out->write( | side: { lv_length }, { lv_size }| ).
        CATCH cx_sy_move_cast_error.
          out->write( |length: { lv_length }, width: { lv_width }, { lv_size } | ).
      endtry.
    endloop.
  endmethod.
ENDCLASS.