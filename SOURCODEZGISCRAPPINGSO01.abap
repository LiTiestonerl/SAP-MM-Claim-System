*&---------------------------------------------------------------------*
*& Include          ZGISCRAPPINGSO01
*&---------------------------------------------------------------------*
CLASS ZCL_DATA_PROCESSOR DEFINITION.
  PUBLIC SECTION.
    METHODS: process_data, post_document.
    DATA: gv_data TYPE string.
ENDCLASS.

CLASS ZCL_DATA_PROCESSOR IMPLEMENTATION.
  METHOD process_data.
    " X# lý d# li#u
    gv_data = 'Processed Data'.
  ENDMETHOD.

  METHOD post_document.
    " ##ng tài li#u
    MESSAGE 'Document posted' TYPE 'S'.
  ENDMETHOD.
ENDCLASS.
