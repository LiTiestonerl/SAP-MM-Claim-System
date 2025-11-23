*&---------------------------------------------------------------------*
*& Include          ZGISCRAPPINGS2_O01
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*& Module STATUS_0100 OUTPUT
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
MODULE status_0100 OUTPUT.
 SET PF-STATUS 'S001'.
 SET TITLEBAR 'T001' WITH 'GOODS ISSUE MATERIAL DOCUMENT'.
 PERFORM setup_listbox.
ENDMODULE.
*&---------------------------------------------------------------------*
*& Module INIT_CONTROL OUTPUT
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
MODULE init_control OUTPUT.
  PERFORM get_date.
  IF go_grid_01 IS BOUND.
    CALL METHOD go_grid_01->refresh_table_display.
  ELSE.
    PERFORM alv_create_100_cont.
    PERFORM alv_grid_display USING 'GO_GRID_01'.
  ENDIF.
ENDMODULE.
*&---------------------------------------------------------------------*
*& Module SET_CURSOR_FIELD_HEADER OUTPUT
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
MODULE set_cursor_field_header OUTPUT.
  IF gv_cursor_field_header IS NOT INITIAL.
    SET CURSOR FIELD gv_cursor_field_header.
    CLEAR gv_cursor_field_header.
  ENDIF.
ENDMODULE.

*&---------------------------------------------------------------------*
*& Module SET_CURSOR_FIELD_ITEM OUTPUT
*&---------------------------------------------------------------------*
MODULE set_cursor_field_item OUTPUT.
  IF gv_has_error_cell = abap_true.
    CALL METHOD go_grid_01->set_current_cell_via_id
      EXPORTING
        is_column_id = gs_col_id
        is_row_no    = gv_error_row_id.
    CLEAR: gv_has_error_cell, gv_error_row_id, gs_col_id, gv_error_message.
  ENDIF.
ENDMODULE.
