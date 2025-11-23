*&---------------------------------------------------------------------*
*& Include          ZPG_MM_RETURN_STOCKC01
*&---------------------------------------------------------------------*
*---------------------------------------------------------------------*
*  Local class: ALV event handler
*---------------------------------------------------------------------*
CLASS lcl_event_handler DEFINITION.
  PUBLIC SECTION.
    METHODS constructor
      IMPORTING
        io_grid  TYPE REF TO cl_gui_alv_grid
        it_table TYPE REF TO data.

    METHODS set_table_ref
      IMPORTING
        it_table TYPE REF TO data.

    METHODS handle_user_command
      FOR EVENT user_command OF cl_gui_alv_grid
      IMPORTING e_ucomm.

    METHODS handle_toolbar
      FOR EVENT toolbar OF cl_gui_alv_grid
      IMPORTING e_object e_interactive.

    METHODS on_data_changed
      FOR EVENT data_changed OF cl_gui_alv_grid
      IMPORTING er_data_changed.

  PRIVATE SECTION.
    DATA: mo_grid      TYPE REF TO cl_gui_alv_grid,
          mt_table     TYPE REF TO data,
          mv_decfmt    TYPE c LENGTH 1,
          mv_df_inited TYPE abap_bool.

    METHODS update_table_from_mod_cells
      IMPORTING
        er_data_changed TYPE REF TO cl_alv_changed_data_protocol.

    METHODS init_decimal_format.
ENDCLASS.

*---------------------------------------------------------------------*
*  Implementation
*---------------------------------------------------------------------*
CLASS lcl_event_handler IMPLEMENTATION.

  METHOD constructor.
    mo_grid       = io_grid.
    mt_table      = it_table.
    mv_df_inited  = abap_false.
    me->init_decimal_format( ).
  ENDMETHOD.

  METHOD set_table_ref.
    mt_table = it_table.
  ENDMETHOD.

  METHOD handle_user_command.
    FIELD-SYMBOLS: <tab> TYPE STANDARD TABLE.

    CASE e_ucomm.
      WHEN '&DEL'.
        DATA: lt_rows TYPE lvc_t_row,
              ls_row  TYPE lvc_s_row,
              lv_ans  TYPE c.

        mo_grid->get_selected_rows( IMPORTING et_index_rows = lt_rows ).
        CHECK lt_rows IS NOT INITIAL.

        CALL FUNCTION 'POPUP_TO_CONFIRM'
          EXPORTING
            titlebar              = 'Confirm Deletion'
            text_question         = 'Do you really want to delete selected rows?'
            text_button_1         = 'Yes'
            text_button_2         = 'No'
            default_button        = '2'
            display_cancel_button = ''
          IMPORTING
            answer                = lv_ans.
        CHECK lv_ans = '1'.

        ASSIGN mt_table->* TO <tab>.
        CHECK <tab> IS ASSIGNED.

        LOOP AT lt_rows INTO ls_row FROM lines( lt_rows ) TO 1 STEP -1.
          DELETE <tab> INDEX ls_row-index.
        ENDLOOP.

        DATA(ls_stbl) = VALUE lvc_s_stbl( row = abap_true col = abap_true ).
        mo_grid->refresh_table_display( is_stable = ls_stbl ).

      WHEN '&ADD'.
        FIELD-SYMBOLS <new> TYPE any.
        ASSIGN mt_table->* TO <tab>.
        CHECK <tab> IS ASSIGNED.

        APPEND INITIAL LINE TO <tab> ASSIGNING <new>.
        mo_grid->set_ready_for_input( i_ready_for_input = 1 ).

        DATA(ls_stbl2) = VALUE lvc_s_stbl( row = abap_true col = abap_true ).
        mo_grid->refresh_table_display( is_stable = ls_stbl2 ).
    ENDCASE.
     PERFORM alv_refresh USING 'GO_GRID_01'.
  ENDMETHOD.

  METHOD handle_toolbar.
    IF gv_alv_mode = 'POHIS' OR gv_alv_mode = 'MDOC'.
      RETURN.
    ENDIF.

    DATA ls_button TYPE stb_button.

    CLEAR ls_button.
    ls_button-function  = '&ADD'.
    ls_button-icon      = icon_insert_row.
    ls_button-quickinfo = 'Add New Row'.
    ls_button-text      = 'Add'.
    ls_button-butn_type = '0'.
    APPEND ls_button TO e_object->mt_toolbar.

    CLEAR ls_button.
    ls_button-function  = '&DEL'.
    ls_button-icon      = icon_delete_row.
    ls_button-quickinfo = 'Delete Selected Row(s)'.
    ls_button-text      = 'Delete'.
    ls_button-butn_type = '0'.
    APPEND ls_button TO e_object->mt_toolbar.
  ENDMETHOD.

  METHOD init_decimal_format.
    SELECT SINGLE dcpfm
      INTO @mv_decfmt
      FROM usr01
      WHERE bname = @sy-uname.

    IF sy-subrc <> 0.
      CLEAR mv_decfmt.
    ENDIF.

    mv_df_inited = abap_true.
  ENDMETHOD.

  METHOD update_table_from_mod_cells.
    FIELD-SYMBOLS: <tab> TYPE STANDARD TABLE,
                   <row> TYPE any,
                   <fld> TYPE any.
    DATA: ls_good TYPE lvc_s_modi,
          lv_val  TYPE string,
          lo_type TYPE REF TO cl_abap_typedescr.

    IF mv_df_inited IS INITIAL.
      me->init_decimal_format( ).
    ENDIF.

    ASSIGN mt_table->* TO <tab>.
    CHECK <tab> IS ASSIGNED.

    LOOP AT er_data_changed->mt_good_cells INTO ls_good.

      READ TABLE <tab> INDEX ls_good-row_id ASSIGNING <row>.
      CHECK sy-subrc = 0 AND <row> IS ASSIGNED.

      ASSIGN COMPONENT ls_good-fieldname OF STRUCTURE <row> TO <fld>.
      CHECK sy-subrc = 0 AND <fld> IS ASSIGNED.

      lv_val = ls_good-value.
      lo_type = cl_abap_typedescr=>describe_by_data( <fld> ).

      IF lo_type->type_kind = cl_abap_typedescr=>typekind_int
      OR lo_type->type_kind = cl_abap_typedescr=>typekind_packed
      OR lo_type->type_kind = cl_abap_typedescr=>typekind_float
      OR lo_type->type_kind = cl_abap_typedescr=>typekind_num.

        CONDENSE lv_val NO-GAPS.
        IF lv_val IS INITIAL.
          CLEAR <fld>.
          CONTINUE.
        ENDIF.

        IF lo_type->type_kind = cl_abap_typedescr=>typekind_num.
          IF lv_val CA 'ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz+-.,'.
            CONTINUE.
          ENDIF.
        ELSE.
          IF mv_decfmt = 'X'.
            REPLACE ALL OCCURRENCES OF '.' IN lv_val WITH ''.
            REPLACE ALL OCCURRENCES OF ',' IN lv_val WITH '.'.
          ELSE.
            REPLACE ALL OCCURRENCES OF ',' IN lv_val WITH ''.
          ENDIF.
        ENDIF.

        TRY.
            <fld> = lv_val.
          CATCH cx_sy_conversion_no_number cx_sy_move_cast_error.
            CONTINUE.
        ENDTRY.

      ELSE.
        <fld> = lv_val.
      ENDIF.

    ENDLOOP.
  ENDMETHOD.


  METHOD on_data_changed.
    me->update_table_from_mod_cells( er_data_changed ).
    gv_grid_changed = abap_true.
    DATA lv_subr TYPE rs38l_fnam.
    CASE gv_alv_mode.
      WHEN 'CREPO'.   lv_subr = 'VALIDATE_CREPO_100'.
      WHEN 'POSTGR'.  lv_subr = 'VALIDATE_POSTGR_200'.
      WHEN 'POSTIV'.  lv_subr = 'VALIDATE_POSTIV_300'.
      WHEN OTHERS.    RETURN.
    ENDCASE.
    IF gv_alv_mode = 'POSTIV'.
      PERFORM recalc_balance_300 IN PROGRAM (sy-repid) IF FOUND CHANGING gv_balance.
    ENDIF.
    IF lv_subr IS NOT INITIAL.
      PERFORM (lv_subr) IN PROGRAM (sy-repid) IF FOUND USING er_data_changed gv_validation_result.
    ENDIF.
  ENDMETHOD.

ENDCLASS.
