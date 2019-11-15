* 1. Create two interfaces: if_display_area, if_display_szie
*    define two apis: get_area() and get_size() respectively
* 2. Integrate the two interfaces into a new interface if_display
* 3. Create a class rectanlge to implement the interface methods,
*    track any new creation on the class and instance level
* 4. Create a sub class square to overwrite the two methods
* 5. Create a class to receive the events and collect the rectange instanaces
* 6. some Sample code for how to use the class and instances

interface if_display_area.
   methods get_area returning value(result) type i. 
endinterface.

interface if_display_size.
   methods get_size returning value(result) type i. 
endinterface.

interface if_display.
   constants math_pi type f value '3.1415926'.
   interfaces: if_display_area, if_display_size.
endinterface.

class rectanlge definition.
  public section.  " access to any 
    " Events
    class-events increased.
    events rectangle_created.

    interfaces if_display.  " interface
    aliases disp_area for if_display_area~get_area. " alias method

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

class square definition inheriting from rectanlge.
  public section.
    methods: constructor importing value(side) type i,
             get_side returning value(side) type i,
             set_side importing value(side) type i.

    methods: if_display_area~get_area redefinition,
             if_display_size~get_size redefinition.
endclass.

class rectanlge implementaion.
    method class_constructor. number_of_instance = 0. endmethod.
    method get_count_of_instances. result = number_of_instance. endmethod.
    method constructor.
      me->length = length.
      me->width = width.
      raise event rectangle_created. " publish the event to the listners

      " class wide
      add 1 to number_of_instance.
      raise event increased.         " publish the class event to the listners
    endmethod.
    method get_length. length = me->length. endmethod.  
    method get_width.  width = me->width.   endmethod.
    method set_length. me->length = length. endmethod.
    method set_width.  me->width = width.   endmethod. 
    method iif_display_area~get_area. 
      result = length * width. 
    endmethod.
    method iif_display_size~get_size.
      result = ( length + width ) * 2.
    endmethod.  
endclass.

class square implementaion.
  method constructor.
    call method super->constructor( length = side width = side ). 
  endmethod.
  method get_side. side = length. endmethod.
  method set_side. length = side. endmethod.
  method iif_display_area~get_area.
    "result = super->if_display_area~get_area( ).
    result = length * length.
  endmethod.
  method iif_display_size~get_size.
    result = length * 4.
  endmethod.
endclass.

class collector definition.
  public section.
    methods constructor.
    methods get_rectangles returning value(result) type table of ref to rectanlge.

  private section.
    " Event handler for rectangle created
    methods add_rectangle for event rectangle_created of rectangle
              importing sender.

    data t_rectangle type table of ref to rectangle. 
endclass.
class collector implementation.  
  method constructor.
    " Registering for an event
    set handler add_rectangle for all instances.
  endmethod.
  method get_rectangles. result = t_rectangle[]. endmethod.
  method add_rectangle. append sender to t_rectangle. endmethod.
endclass.

form test.
    DATA: lo_collector TYPE REF TO collector.
    " Set up the collector to listen the creation of rectangles
    "create object lo_collector.
    lo_collector = new lo_collector( ).
        
    DATA: lo_rec1 TYPE REF TO rectangle,
          lo_rec2 TYPE REF TO rectangle,
          lif_rec  TYPE REF TO display.

    DATA: lo_square1 TYPE REF TO square,
          lo_square2 TYPE REF TO square.

    "lo_rec1 = new rectange( length = 12 width = 2 ). " collector handler called
    create object lo_rec1 exporting 
                             length = 12
                             width = 2.
    lo_square1 = new square( side = 6 ).  " collector handler called
    
    " Narrowing cast: down
    " a derived class instance to interface, abstract, supper class 
    lif_rec = lo_square1.
    data(lv_area) = lif_rec->if_display_area~get_area( ).
    lv_area = lif_rec->disp_area( ).  " alias 
    lo_rec2 = lo_square1.  " Narrowing cast again

    " Widening cast: up 
    " interface, abstract, supper class to a derived class variable
    try.
      lo_square2 ?= lif_rec.        " widening cast I
      move lo_rec2 ?to lo_square2.  " alternatively, widening cast II

    CATCH cx_sy_move_cast_error.
      " react on that cast error
  ENDTRY.

  lif_rec = lo_rect1.
  lv_area = lif_rec->if_display_area~get_area( ).

  data(lv_count) - rectangle=>get_count_of_instances( ).
  
  loop at lo_collector.get_rectangles( ) assigning field-symbols <rect>.
    data(lv_size) = <rect>->iif_display_size~get_size( ).
    data(lv_length) = <rect>->get_lenth( ).
    data(lv_width) = <rect>->get_width( ).

    try.
       lo_square2 ?= <rect>.
       write: / 'side:', lv_length, lv_size.
    CATCH cx_sy_move_cast_error. 
       write: / 'length:', lv_length, 'width:' lv_width, lv_size.
  endloop.
endform.
