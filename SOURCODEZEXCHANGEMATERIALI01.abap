*&---------------------------------------------------------------------*
*& Include          ZPG_MM_RETURN_STOCKI01
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  EXIT_COMMAND  INPUT
*&---------------------------------------------------------------------*
MODULE exit_command INPUT.
  CASE gv_okcode.
    WHEN 'BACK' OR 'CANC'.
      LEAVE TO SCREEN 0.
    WHEN 'EXIT'.
      LEAVE PROGRAM.
    WHEN OTHERS.
  ENDCASE.
  CLEAR : gv_okcode.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
MODULE user_command_0100 INPUT.
  CASE gv_okcode.

    WHEN 'POST'.
      IF go_grid_01 IS BOUND.
        go_grid_01->check_changed_data( ).
      ENDIF.
      IF gs_po_edit = 'X'.
        PERFORM change_po USING gs_po_header-ebeln.
      ELSE.
        PERFORM post_bapi.
      ENDIF.

    WHEN 'CHECK'.
      gv_validation_result = abap_true.

      IF go_grid_01 IS BOUND.
        go_grid_01->check_changed_data( ).
      ENDIF.

      IF gs_po_edit = 'X'.   "====================== CHANGE PO ======================
        DATA(lv_ebeln_key) = |{ gs_po_header-ebeln ALPHA = IN }|.
        gs_po_header-ebeln = lv_ebeln_key.

        IF lv_ebeln_key IS INITIAL.
          CLEAR: gv_po_loaded_100, gt_po_item.
          PERFORM alv_refresh USING 'GO_GRID_01'.
          RETURN.
        ENDIF.

        IF gv_po_loaded_100 IS INITIAL
         OR gv_po_loaded_100 <> lv_ebeln_key
         OR gt_po_item IS INITIAL.

          PERFORM validate_po_header CHANGING gv_validation_result.
          IF gv_validation_result = abap_false.
            RETURN.
          ENDIF.

          PERFORM get_item_from_db.
          gv_po_loaded_100 = lv_ebeln_key.

        ELSE.
          PERFORM validate_po_header CHANGING gv_validation_result.
        ENDIF.

      ELSE.               "====================== CREATE PO ======================
        PERFORM validate_po_header CHANGING gv_validation_result.
        IF gv_validation_result = abap_false.
          RETURN.
        ENDIF.

        IF gt_po_item IS NOT INITIAL.
          IF gv_grid_changed = abap_true.
            PERFORM get_short_text.
            LOOP AT gt_po_item INTO DATA(ls_po_item).
              DATA(lv_material_currency) = ls_po_item-zcurrency.
              DATA(lv_plant) = ls_po_item-zplant.
            ENDLOOP.
            CLEAR gv_grid_changed.
          ENDIF.
        ENDIF.
      ENDIF.
    WHEN 'CHANGE'.
      CLEAR: gt_po_item.
      DATA(lv_current_mode) = gs_po_edit.
      PERFORM show_popup_change CHANGING gs_po_edit.

      IF gs_po_edit = lv_current_mode.
        EXIT.
      ENDIF.

      " N#u chuy#n sang CREATE -> xóa header và item
      IF gs_po_edit IS INITIAL. " CREATE mode
        CLEAR: gs_po_header.
        CLEAR: gt_po_item[].
      ENDIF.

      " --- Set l#i field catalog theo mode ---
      DATA(lv_is_create) = xsdbool( gs_po_edit IS INITIAL ).
      DATA(lv_is_edit) = xsdbool( gs_po_edit = 'X' ).

      LOOP AT gt_po_fieldcat ASSIGNING FIELD-SYMBOL(<ls_fcat>).

        CASE <ls_fcat>-fieldname.
          WHEN 'ZMATE_DES'.                            "Short Text luôn khóa
            <ls_fcat>-edit = abap_false.
          WHEN 'MATNR'.                            "Material: ch# cho nh#p # CREATE
            <ls_fcat>-edit = COND abap_bool( WHEN lv_is_create = abap_true
                                             THEN abap_true ELSE abap_false ).
          WHEN OTHERS.
            <ls_fcat>-edit = abap_true.
        ENDCASE.
      ENDLOOP.

      IF go_grid_01 IS BOUND.
        go_grid_01->set_frontend_fieldcatalog( EXPORTING it_fieldcatalog = gt_po_fieldcat ).
        go_grid_01->set_ready_for_input( i_ready_for_input = 1 ).
        PERFORM alv_refresh USING 'GO_GRID_01'.
      ENDIF.
  ENDCASE.

  CLEAR gv_okcode.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0200  INPUT
*&---------------------------------------------------------------------*
MODULE user_command_0200 INPUT.
  CASE gv_okcode.
    WHEN 'CHECK'.
      gv_validation_result = abap_true.

      IF go_grid_01 IS BOUND.
        go_grid_01->check_changed_data( ).
      ENDIF.
      DATA(lv_po_200) = |{ gs_receipt_header-ebeln ALPHA = IN }|.
      gs_receipt_header-ebeln = lv_po_200.

      IF lv_po_200 IS INITIAL.
        CLEAR: gv_po_loaded_200, gt_receipt_item.
        PERFORM alv_refresh USING 'GO_GRID_01'.
        RETURN.
      ENDIF.

      IF gv_po_loaded_200 IS INITIAL
       OR lv_po_200 <> gv_po_loaded_200
       OR gt_receipt_item IS INITIAL.

        PERFORM validate_receipt_header CHANGING gv_validation_result.
        IF gv_validation_result = abap_false.
          RETURN.
        ENDIF.

        PERFORM get_data_200.
        gv_po_loaded_200 = lv_po_200.

      ELSE.
        PERFORM validate_receipt_header CHANGING gv_validation_result.
      ENDIF.
    WHEN 'POST'.
      gv_validation_result = abap_true.
      IF go_grid_01 IS BOUND.
        go_grid_01->check_changed_data( ).
      ENDIF.
      PERFORM  validate_receipt_header CHANGING gv_validation_result.
      IF gv_validation_result = abap_true.
        PERFORM post_bapi_receipt.
      ENDIF.
  ENDCASE.

  CLEAR gv_okcode.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0300  INPUT
*&---------------------------------------------------------------------*
MODULE user_command_0300 INPUT.

  CASE gv_okcode.
    WHEN 'CHECK'.
      gv_validation_result = abap_true.
      IF go_grid_01 IS BOUND.
        go_grid_01->check_changed_data( ).
      ENDIF.
      DATA(lv_ebeln_curr) = |{ gs_invoice_header-ebeln ALPHA = IN }|.
      gs_invoice_header-ebeln = lv_ebeln_curr.

      IF lv_ebeln_curr IS INITIAL.
        CLEAR: gv_ebeln_loaded_300, gt_invoice_item.
        PERFORM alv_refresh USING 'GO_GRID_01'.
        RETURN.
      ENDIF.

      IF gv_ebeln_loaded_300 IS INITIAL.
        PERFORM get_data_300.
        gv_ebeln_loaded_300 = lv_ebeln_curr.

      ELSEIF lv_ebeln_curr <> gv_ebeln_loaded_300.
        PERFORM get_data_300.
        gv_ebeln_loaded_300 = lv_ebeln_curr.
      ELSE.
        IF gt_invoice_item IS INITIAL.
          PERFORM get_data_300.
        ELSE.
          PERFORM validate_receipt_header CHANGING gv_validation_result.
        ENDIF.
      ENDIF.
" After PO has been loaded/validated:
      IF lv_ebeln_curr IS INITIAL OR gt_invoice_item IS INITIAL.
        CLEAR gv_balance.
          gv_show_balance = abap_false.
        ELSE.
          PERFORM recalc_balance_300 CHANGING gv_balance.
          gv_show_balance = abap_true.
      ENDIF.

    WHEN 'POST'.
      IF go_grid_01 IS BOUND.
        go_grid_01->check_changed_data( ).
      ENDIF.
      PERFORM validation_invoice CHANGING gv_validation_result.
      IF gv_validation_result = abap_true.
        PERFORM post_bapi_invoice.
      ENDIF.
    WHEN OTHERS.
  ENDCASE.

  CLEAR gv_okcode.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0400  INPUT
*&---------------------------------------------------------------------*
MODULE user_command_0400 INPUT.
  CASE gv_okcode.
    WHEN 'CHECK'.
      PERFORM validation_payment CHANGING gv_validation_result.
      IF gv_validation_result = abap_true.
        "PERFORM get_data_invoice. " get data tu invoice cho man 300
      ENDIF.
    WHEN 'POST'.
      PERFORM validation_payment CHANGING gv_validation_result.
      IF gv_validation_result = abap_true.
        PERFORM view_payment.
      ENDIF.
  ENDCASE.

  CLEAR gv_okcode.
ENDMODULE.

MODULE user_command_0600 INPUT.
  CASE gv_okcode.
    WHEN 'REFRESH'.
      IF r_hist = 'X'.
        PERFORM get_po_history.
      ELSEIF r_mdoc = 'X'.
        PERFORM get_material_docs.       " NEW
      ENDIF.
      PERFORM alv_refresh USING 'GO_GRID_01'.
  ENDCASE.
  CLEAR gv_okcode.
ENDMODULE.
