*&---------------------------------------------------------------------*
*& Include          ZPG_MM_CLAIMAMOUNTO01
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*& Module STATUS_0100 OUTPUT
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
MODULE status_0100 OUTPUT.

  IF gs_edit = ''.
    SET PF-STATUS 'S001' EXCLUDING 'SAVE'.
    SET TITLEBAR 'T001' WITH 'Claim Management System'.
  ELSEIF gs_edit = 'X' .
    SET PF-STATUS 'S001' EXCLUDING 'CREATE'.
    SET TITLEBAR 'T001' WITH 'Claim Management System'.
  ENDIF.
ENDMODULE.
*&---------------------------------------------------------------------*
*& Module INIT_CONTROL OUTPUT
*&---------------------------------------------------------------------*
*& create and display alv
*&---------------------------------------------------------------------*
MODULE init_control OUTPUT.
  IF go_grid_01 IS BOUND.
    CALL METHOD go_grid_01->refresh_table_display.
  ELSE.
    PERFORM alv_create_100_cont.
    PERFORM alv_grid_display USING 'GO_GRID_01'.
  ENDIF.
ENDMODULE.

*&---------------------------------------------------------------------*
*& Module SCREEN_MODIFY OUTPUT
*&---------------------------------------------------------------------*
*& modify header
*&---------------------------------------------------------------------*
MODULE screen_modify OUTPUT.
  LOOP AT SCREEN.
    IF screen-group1 = 'GR1'.
      CASE gs_edit.
        WHEN 'X'.
          screen-input = '1'.
          screen-required = '1'.
          screen-invisible = '0'.
        WHEN ''.
          screen-input = '0'.
          screen-required = '0'.
          screen-invisible = '0'.
      ENDCASE.

    ELSEIF screen-group1 = 'GR3'.
      IF gs_header-supplier IS INITIAL.
        CASE gs_edit.
          WHEN 'X'.
            screen-input = '0'.
            screen-invisible = '1'.  " Hi#n th# nh#ng không s#a
          WHEN ''.
            screen-input = '0'.
            screen-invisible = '0'.  " Hi#n th# và cho phép s#a
        ENDCASE.
      ENDIF.
    ELSEIF screen-group1 = 'GR2'.
      CASE gs_edit.
          WHEN 'X'.
            screen-input = '1'.
          WHEN ''.
            screen-input = '0'.
        ENDCASE.
    ENDIF.

    MODIFY SCREEN.
  ENDLOOP.

ENDMODULE.

*&---------------------------------------------------------------------*
*& Module SET_CURSOR_FIELD OUTPUT
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
    CALL METHOD go_grid_01->refresh_table_display.
    CALL METHOD go_grid_01->set_current_cell_via_id
      EXPORTING
        is_column_id = gs_col_id
        is_row_no    = gv_error_row_id.
    CLEAR: gv_has_error_cell, gv_error_row_id, gs_col_id, gv_error_message.
  ENDIF.
ENDMODULE.
