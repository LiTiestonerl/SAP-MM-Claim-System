*&---------------------------------------------------------------------*
*& Include          ZGISCRAPPINGSI01
*&---------------------------------------------------------------------*
CLASS ZCL_SYSTEM_HANDLER DEFINITION.
  PUBLIC SECTION.
    METHODS: handle_error, handle_success.
    DATA: gv_message TYPE string.
ENDCLASS.

CLASS ZCL_SYSTEM_HANDLER IMPLEMENTATION.
  METHOD handle_error.
    " X# lý l#i
    gv_message = 'An error occurred'.
    MESSAGE gv_message TYPE 'E'.
  ENDMETHOD.

  METHOD handle_success.
    " X# lý thông báo thành công
    gv_message = 'Operation successful'.
    MESSAGE gv_message TYPE 'S'.
  ENDMETHOD.
ENDCLASS.
