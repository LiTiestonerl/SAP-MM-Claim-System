*&---------------------------------------------------------------------*
*& Include          ZPG_MM_CLAIMAMOUNTI01
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0100 INPUT.
  CASE gv_okcode.
    WHEN 'SAVE'.
      IF go_grid_01 IS BOUND.
        go_grid_01->check_changed_data( ). " L#y d# li#u #ã ch#nh s#a
      ENDIF.

    WHEN 'EXECUTE'.
      PERFORM post_bapi.
      "PERFORM alv_grid_display USING 'GO_GRID_01'.
    WHEN 'CHECK'.
      IF go_grid_01 IS BOUND.
        go_grid_01->check_changed_data( ).
      ENDIF.
      PERFORM validation_receipt CHANGING lv_validation_result.
      IF lv_validation_result = abap_true.
        "n#u t#t c# #úng h#t thì fetch tên c#a gl account v# b#ng
        PERFORM get_gl_name.
        PERFORM get_vendor_name.
        perform get_cal_balane.
      ENDIF.

    WHEN 'CHANGE'.
      IF gs_edit = ''.
        gs_edit = 'X'.
        "PERFORM alv_grid_display USING 'GO_GRID_01'.
        LOOP AT gt_fieldcat ASSIGNING FIELD-SYMBOL(<ls_fcat>).
          IF <ls_fcat>-fieldname <> 'SHORT_TEXT' AND <ls_fcat>-fieldname <> 'LOC_DOC_CURR'.
            <ls_fcat>-edit = gs_edit.

          ENDIF.
        ENDLOOP.
        CALL METHOD go_grid_01->set_frontend_fieldcatalog
          EXPORTING
            it_fieldcatalog = gt_fieldcat.
        PERFORM alv_outtab_display USING 'GO_GRID_01'.
      ELSE.
        PERFORM show_popup_change.
        "PERFORM alv_grid_display USING 'GO_GRID_01'.

        LOOP AT gt_fieldcat ASSIGNING FIELD-SYMBOL(<ls_fcat1>).
          IF <ls_fcat1>-fieldname <> 'SHORT_TEXT'.
            <ls_fcat1>-edit = gs_edit.
          ENDIF.
        ENDLOOP.

        CALL METHOD go_grid_01->set_frontend_fieldcatalog
          EXPORTING
            it_fieldcatalog = gt_fieldcat.
        PERFORM alv_outtab_display USING 'GO_GRID_01'.
      ENDIF.
  ENDCASE.

  CLEAR gv_okcode.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  EXIT_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE exit_command_0100 INPUT.
  CASE gv_okcode.
    WHEN 'BACK' OR 'CANC'.
      LEAVE TO SCREEN 0.
    WHEN 'EXIT'.
      LEAVE PROGRAM.
    WHEN OTHERS.
  ENDCASE.
ENDMODULE.
