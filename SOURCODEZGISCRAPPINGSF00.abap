*&---------------------------------------------------------------------*
*& Include          ZGISCRAPPINGSF00
*&---------------------------------------------------------------------*
CLASS ZCL_ALV_DISPLAY DEFINITION.
  PUBLIC SECTION.
    METHODS: display_alv, set_field_catalog.
    DATA: go_grid TYPE REF TO cl_gui_alv_grid,      " ##i t##ng ALV
          lt_fieldcat TYPE lvc_t_fcat,              " Field catalog cho ALV
          lt_data TYPE TABLE OF ZTGSU06,            " D# li#u hi#n th# ALV
          go_container TYPE REF TO cl_gui_container. " Container cho ALV
ENDCLASS.

CLASS ZCL_ALV_DISPLAY IMPLEMENTATION.

  " Ph##ng th#c hi#n th# ALV
  METHOD display_alv.
    " T#o ##i t##ng container (n#u ch#a có)
    CREATE OBJECT go_container
      EXPORTING
        container_name = 'ALV_CONTAINER'.   " Tên container hi#n th# ALV trên màn hình

    " T#o ALV grid v#i container #ã kh#i t#o
    CREATE OBJECT go_grid
      EXPORTING
        i_parent = go_container.  " Liên k#t v#i container

  ENDMETHOD.

  " Ph##ng th#c thi#t l#p field catalog cho ALV
  METHOD set_field_catalog.
    CLEAR lt_fieldcat.

    " C#u hình các tr##ng hi#n th# trong ALV
    APPEND VALUE #( fieldname = 'MATNR' seltext = 'Material Number' ) TO lt_fieldcat.
    APPEND VALUE #( fieldname = 'ZSTR_LOCA' seltext = 'Storage Location' ) TO lt_fieldcat.
    APPEND VALUE #( fieldname = 'ZSTOCK' seltext = 'Stock Quantity' ) TO lt_fieldcat.

    " N#u có thêm các tr##ng khác c#n hi#n th#, hãy thêm chúng # #ây.
    " Ví d#: APPEND VALUE #( fieldname = 'FIELDNAME' seltext_l = 'Field Description' ) TO lt_fieldcat.
  ENDMETHOD.

ENDCLASS.
