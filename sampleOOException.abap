** ABAP OO Exception 
**
** Purpose: Sample to define an exception class and how to use it
**
** Author : Simon Li  Feb 2020
**
** A kind reminder: in the real coding, the simulation part is not 
** needed. The code mirrors what SAP has and only written out for 
** referrence and better understanding.
REPORT ZTEST_OO_EXCEPTION NO STANDARD PAGE HEADING.

*==========================================================
*
* Begin of Simulation
*
* Part of Interface for Accessing Message Texts
interface IF_MESSAGE.
    methods: GET_TEXT returning value(result) type string, 
             GET_LONGTEXT returning value(result) type string.
endinterface.

* Serializable Object
interface IF_SERIALIZABLE_OBJECT.
endinterface.

* Abstract Superclass for All Global Exceptions
class CX_ROOT definition Abstract.
    public section.
        interfaces: if_message,IF_SERIALIZABLE_OBJECT.
        constants CX_ROOT type SOTR_CONC value '16AA9A3937A9BB56E10000000A11447B'. " char32
        data: TEXTID type SOTR_CONC,
              PREVIOUS type ref to cx_root,
              KERNEL_ERRID type S380ERRID,  " char30
              IS_RESUMABLE type abap_bool.
      
        aliases: GET_LONGTEXT for IF_MESSAGE~GET_LONGTEXT,
                 GET_TEXT     for IF_MESSAGE~GET_TEXT. 
        methods: CONSTRUCTOR importing TEXTID   like TEXTID optional
                                       PREVIOUS like PREVIOUS optional,
                 GET_SOURCE_POSITION exporting PROGRAM_NAME type SYREPID
                                               INCLUDE_NAME TYPE SYREPID
                                               SOURCE_LINE type i. 
  protected section.  
  private section.
      data INTERNAL_SOURCE_POS type SCX_SRCPOS.  "(int4, int4)
endclass.

class cx_root implementation.
    METHOD constructor.
        IF textid IS INITIAL.
           me->textid = cx_root.
        ELSE.
           me->textid = textid.
        ENDIF.
        me->previous = previous.
    ENDMETHOD.

    METHOD if_message~get_text .
        CALL METHOD cl_message_helper=>get_text_for_message
           EXPORTING
             text   = me
           RECEIVING
             result = result.
    ENDMETHOD.
    METHOD if_message~get_longtext.
        " RETURNING result
        CALL METHOD cl_message_helper=>get_longtext_for_message
           EXPORTING
              text   = me
              preserve_newlines = preserve_newlines
           RECEIVING
              result = result.
    ENDMETHOD.

    METHOD get_source_position.
    " EXPORTING program_name include_name source_line
        CALL 'GET_SOURCE_POSITION'
            ID 'INTERNAL'     FIELD internal_source_pos
            ID 'PROGRAM_NAME' FIELD program_name
            ID 'INCLUDE_NAME' FIELD include_name
            ID 'SOURCE_LINE'  FIELD source_line.
    endmethod.
endclass.

* Exceptions with Static and Dynamic Check of RAISING Clause
class CX_STATIC_CHECK definition Abstract inheriting from cx_root.
    public section.
        methods CONSTRUCTOR importing TEXTID   like TEXTID optional
                                      PREVIOUS like PREVIOUS optional. 
endclass.
class CX_STATIC_CHECK implementation.
    method CONSTRUCTOR .
        CALL METHOD SUPER->CONSTRUCTOR
            EXPORTING
                TEXTID = TEXTID
                PREVIOUS = PREVIOUS.
    endmethod.
endclass.

* Exceptions with Dynamic Check Only of the RAISING Clause
class CX_DYNAMIC_CHECK definition Abstract inheriting from cx_root.
    public section.
        methods CONSTRUCTOR importing TEXTID   like TEXTID optional
                                      PREVIOUS like PREVIOUS optional.  
endclass.
class CX_DYNAMIC_CHECK implementation.
    method CONSTRUCTOR .
        CALL METHOD SUPER->CONSTRUCTOR
            EXPORTING
                TEXTID = TEXTID
                PREVIOUS = PREVIOUS.
    endmethod.
endclass.

* Exception with no Check of the RAISING Clause
class CX_NO_CHECK definition Abstract inheriting from cx_root.
    public section.
        methods CONSTRUCTOR importing TEXTID   like TEXTID optional
                                      PREVIOUS like PREVIOUS optional. 
endclass.
class CX_NO_CHECK implementation.
    method CONSTRUCTOR .
        CALL METHOD SUPER->CONSTRUCTOR
            EXPORTING
                TEXTID = TEXTID
                PREVIOUS = PREVIOUS.
    endmethod.
endclass.
*
* End of Simulation
*
*==========================================================
*==========================================================

* Definition of our exception class
class cx_sample definition inheriting from CX_STATIC_CHECK.
    public section.
        "data code type i.
        "data msg type string.
        methods CONSTRUCTOR importing TEXTID like TEXTID optional
                                      PREVIOUS like PREVIOUS optional
                                      code type i optional 
                                      msg type string optional,
                get_code returning value(code) type i,
                get_message return value(msg) type string.
    private section.
        data code type i.
        data msg type string.  
endclass.
class cx_sample implementation.
    method CONSTRUCTOR.
        super->constructor( exporting textid = textid 
                                      previous = previous 
                          ).
        me->code = code.
        me->msg = msg.
        if textid is initial.
            me->textid = me->msg.
        endif.
    endmethod.
    method get_code.
        code = me->code.
    endmethod.
    method get_message.
        msg = me->msg.
    endmethod.
endclass.

class cx_sample2 definition inheriting from CX_DYNAMIC_CHECK.
endclass.

* How to test our exception class
class cl_test definition.
    public section.
      class-methods: main,
                     pre_process raising RESUMABLE(cx_sample2). 
                     process returing value(result) type i raising cx_sample.
endclass.
class cl_test implementation.
    method pre_process.
        try.
            "...
            RAISE RESUMABLE EXCEPTION TYPE cx_sample2.
            " Or DATA lo_cx_sample2 = NEW cx_sample2( ).
            "    RAISE RESUMABLE EXCEPTION lo_cx_sample2.
            write: / 'Demo, resumed ...'.
            "...
        " cleanup = catch plus raise again if resumption is not possible
        cleanup into data(lo_cx).  " If possibly resumed, this cleanup won't be called
           write: / lo_cx->get_text( ). 
        endtry.
    endmethod.
    method process.
        result = 0.
        "...
        try.
            result = 4.
            write: / |Coming here, result = { result }|.
            raise exception type cx_sample 
                exporting
                    code = 3034
                    msg = 'This is a test for ABAP OO Exception'.
            write: / 'Will never reach here'.
            "...
            cleanup. " This will be called before the exception handling is propagated
                " clean up data and maintain the variable state
                result = 0.
                write: / |Coming here, result = { result }|.
        endtry.
    endmethod.
    method main.
        try.
            pre_process( ).
            data(lv_result) = process( ).
          catch before unwind cx_sample2 into data(lo_cx_sample2).
            if lo_cx_sample2->IS_RESUMABLE = abap_true.
                resume.
            endif.    
          catch cx_sample into data(lo_cx_sample).
            write: / 'Result:', lv_result,
                     'Code:', lo_cx_sample->get_code(),
                     'Message:', lo_cx_sample->get_message(). 
          catch cx_root into data(lo_cx_root).
            write: / "Catch all'.
            write: / lo_cx_root->get_text( ).
            write: / "It won't be reached in this sample, but it is still a good practice".
        endtry.
    endmethod.
endclass.

start-of-selection.
    cl_test=>main( ).