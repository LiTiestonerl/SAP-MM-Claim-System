*&---------------------------------------------------------------------*
*& Include          ZGISCRAPPINGS2_I01
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0100 INPUT.
 CASE gv_okcode.
    WHEN 'POST'.
      PERFORM post_bapi.
    WHEN 'CHECK'.
       IF go_grid_01 IS BOUND.
        go_grid_01->check_changed_data( ).
      ENDIF.
       PERFORM validation CHANGING lv_validation_result.
      IF lv_validation_result = abap_true.
        "n#u t#t c# #úng h#t thì fetch tên c#a gl account v# b#ng
        PERFORM get_material_name.
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
  CLEAR : gv_okcode.
ENDMODULE.
