*&---------------------------------------------------------------------*
*& Include          ZGISCRAPPINGST01
*&---------------------------------------------------------------------*
CLASS ZCL_DATA_HANDLER DEFINITION.
  PUBLIC SECTION.
    METHODS: prepare_data, validate_data.
    DATA: gv_status TYPE char1.
    DATA: gt_data TYPE TABLE OF ZTGSU06. " B#ng d# li#u c#n x# lý
ENDCLASS.

CLASS ZCL_DATA_HANDLER IMPLEMENTATION.
  METHOD prepare_data.
    " L#y d# li#u t# b#ng ZTGSU06
    SELECT * FROM ZTGSU06 INTO TABLE gt_data.
    IF sy-subrc = 0.
      gv_status = 'D'. " D# li#u #ã s#n sàng
    ELSE.
      gv_status = 'E'. " L#i khi l#y d# li#u
    ENDIF.
  ENDMETHOD.

  METHOD validate_data.
    " Ki#m tra d# li#u
    IF gt_data IS INITIAL.
      MESSAGE 'D# li#u không có' TYPE 'E'.
    ENDIF.
  ENDMETHOD.
ENDCLASS.
