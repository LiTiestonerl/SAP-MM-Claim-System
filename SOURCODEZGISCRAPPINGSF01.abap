*&---------------------------------------------------------------------*
*& Include          ZGISCRAPPINGSF01
*&---------------------------------------------------------------------*
CLASS ZCL_EVENT_HANDLER DEFINITION.
  PUBLIC SECTION.
    METHODS: handle_event, handle_double_click, handle_toolbar.
    DATA: gv_okcode TYPE sy-ucomm.
ENDCLASS.

CLASS ZCL_EVENT_HANDLER IMPLEMENTATION.
  METHOD handle_event.
    " X# lý s# ki#n ng##i dùng
    CASE gv_okcode.
      WHEN 'POST'.
        PERFORM post_document.
      WHEN 'CHECK'.
        PERFORM check_document.
      WHEN 'HOLD'.
        PERFORM hold_document.
      WHEN OTHERS.
        PERFORM handle_other_commands.
    ENDCASE.
  ENDMETHOD.

  METHOD handle_double_click.
    " X# lý s# ki#n nh#p #úp chu#t
    PERFORM handle_double_click_event.
  ENDMETHOD.

  METHOD handle_toolbar.
    " X# lý s# ki#n t# thanh công c#
    PERFORM handle_toolbar_event.
  ENDMETHOD.
ENDCLASS.
