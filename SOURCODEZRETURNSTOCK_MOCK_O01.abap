*&---------------------------------------------------------------------*
*& Include          ZPG_MM_RETURN_STOCKO01
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*& Module STATUS_0100 OUTPUT
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
MODULE status_0100 OUTPUT.
  IF gs_po_edit = ''.
    SET PF-STATUS 'S001' .
    SET TITLEBAR 'T001' WITH 'Create Purchase Order'.
  ELSEIF gs_po_edit = 'X' .
    SET PF-STATUS 'S001' .
    SET TITLEBAR 'T001' WITH 'Change Purchase Order'.
  ENDIF.
ENDMODULE.
*&---------------------------------------------------------------------*
*& Module INIT_CONTROL OUTPUT
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
MODULE init_control OUTPUT.
  DATA(lv_mode_now) = COND string(
                        WHEN r_crepo  = 'X' THEN 'CREPO'
                        WHEN r_postgr = 'X' THEN 'POSTGR'
                        WHEN r_postiv = 'X' THEN 'POSTIV'
                        ELSE '' ).

  IF go_grid_01 IS INITIAL.
    gv_alv_mode = lv_mode_now.
    PERFORM alv_create_100_cont.
    PERFORM alv_grid_display USING 'GO_GRID_01'.
  ELSEIF gv_alv_mode <> lv_mode_now.
    gv_alv_mode = lv_mode_now.
    PERFORM alv_grid_display USING 'GO_GRID_01'.
  ELSE.
    "PERFORM alv_refresh USING 'GO_GRID_01'.
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
*&
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
*&---------------------------------------------------------------------*
*& Module MODIFY_HEADER_FIELD OUTPUT
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
MODULE modify_header_field OUTPUT.
  LOOP AT SCREEN.
    IF screen-group1 = 'GR1'.
      CASE gs_po_edit.
        WHEN ''.
          screen-input = '1'.
          screen-invisible = '0'.
        WHEN 'X'.
          IF gs_po_header-ebeln IS NOT INITIAL.
            " Cho phép hi#n th# khi #ã có PO number
            screen-input = '0'.
            screen-invisible = '0'.
          ELSE.
            screen-input = '0'.
            screen-invisible = '1'.
          ENDIF.
      ENDCASE.
    ELSEIF screen-group1 = 'GR2'.
      CASE gs_po_edit.
        WHEN ''.
          screen-input = '0'.
          screen-invisible = '1'.
        WHEN 'X'.
          screen-input = '1'.
          screen-invisible = '0'.
      ENDCASE.
    ENDIF.

    MODIFY SCREEN.
  ENDLOOP.
ENDMODULE.
*&---------------------------------------------------------------------*
*& Module STATUS_0200 OUTPUT
*&---------------------------------------------------------------------*
MODULE status_0200 OUTPUT.
  SET PF-STATUS 'S000'.
  SET TITLEBAR 'T001' WITH 'Post Good Receipt'.
ENDMODULE.
*&---------------------------------------------------------------------*
*& Module STATUS_0300 OUTPUT
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
MODULE status_0300 OUTPUT.
  SET PF-STATUS 'S000'.
  SET TITLEBAR 'T001' WITH 'Post Invoice'.
ENDMODULE.
*&---------------------------------------------------------------------*
*& Module MODIFY_HEADER_FIELD_300 OUTPUT
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
MODULE modify_header_field_300 OUTPUT.
  LOOP AT SCREEN.
    IF gs_invoice_header-bukrs IS NOT INITIAL.
      IF screen-group1 = 'HD1'.
        screen-input = '1'.
        screen-invisible = '0'.
      ELSEIF screen-group1 = 'HD2'.
        screen-input = '0'.
        screen-invisible = '0'.
      ENDIF.
    ELSE.
      IF screen-group1 = 'HD1'.
        screen-input = '1'.
        screen-invisible = '0'.
      ELSEIF screen-group1 = 'HD2'.
        screen-input = '0'.
        screen-invisible = '1'.
      ENDIF.
    ENDIF.

    MODIFY SCREEN.
  ENDLOOP.
ENDMODULE.
*&---------------------------------------------------------------------*
*& Module STATUS_0400 OUTPUT
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
MODULE status_0400 OUTPUT.
  SET PF-STATUS 'S000'.
  SET TITLEBAR 'T001' WITH 'Payment Information'.
ENDMODULE.
*&---------------------------------------------------------------------*
*& Module INIT_CONTROL_300 OUTPUT
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
MODULE init_control_300 OUTPUT.
  DATA(lv_mode_now_300) = COND string(
                        WHEN r_crepo  = 'X' THEN 'CREPO'
                        WHEN r_postgr = 'X' THEN 'POSTGR'
                        WHEN r_postiv = 'X' THEN 'POSTIV'
                        ELSE '' ).

  IF go_grid_01 IS INITIAL.
    gv_alv_mode = lv_mode_now_300.
    PERFORM alv_create_100_cont.
    PERFORM alv_grid_display USING 'GO_GRID_01'.
  ELSEIF gv_alv_mode <> lv_mode_now_300.
    gv_alv_mode = lv_mode_now_300.
    PERFORM alv_grid_display USING 'GO_GRID_01'.
  ELSE.
    "PERFORM alv_refresh USING 'GO_GRID_01'.
  ENDIF.
ENDMODULE.
*&---------------------------------------------------------------------*
*& Module INIT_CONTROL_200 OUTPUT
*&---------------------------------------------------------------------*
MODULE init_control_200 OUTPUT.
  DATA(lv_mode_now_200) = COND string(
                        WHEN r_crepo  = 'X' THEN 'CREPO'
                        WHEN r_postgr = 'X' THEN 'POSTGR'
                        WHEN r_postiv = 'X' THEN 'POSTIV'
                        ELSE '' ).

  IF go_grid_01 IS INITIAL.
    PERFORM get_date.
    gv_alv_mode = lv_mode_now_200.
    PERFORM alv_create_100_cont.
    PERFORM alv_grid_display USING 'GO_GRID_01'.
  ELSEIF gv_alv_mode <> lv_mode_now_200.
    gv_alv_mode = lv_mode_now_200.
    PERFORM alv_grid_display USING 'GO_GRID_01'.
  ELSE.
    "PERFORM alv_refresh USING 'GO_GRID_01'.
  ENDIF.
ENDMODULE.

MODULE status_0500 OUTPUT.
  SET PF-STATUS 'S000'.
  SET TITLEBAR 'T001' WITH 'Purchase Order History'.
ENDMODULE.

MODULE init_control_500 OUTPUT.
  DATA(lv_mode_now_500) = 'POHIS'.

  IF go_grid_01 IS INITIAL.
    gv_alv_mode = lv_mode_now_500.
    PERFORM alv_create_100_cont.
    PERFORM alv_grid_display USING 'GO_GRID_01'.
  ELSEIF gv_alv_mode <> lv_mode_now_500.
    gv_alv_mode = lv_mode_now_500.
    PERFORM alv_grid_display USING 'GO_GRID_01'.
  ELSE.
    " nothing
  ENDIF.
ENDMODULE.
