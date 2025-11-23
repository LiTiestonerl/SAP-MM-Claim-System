*&---------------------------------------------------------------------*
*& Include          ZGISCRAPPINGS1_C01
*&---------------------------------------------------------------------*
" Definition of a local class to handle ALV grid events
CLASS lcl_event_handler DEFINITION.
  PUBLIC SECTION.
    METHODS:
      " Constructor to receive ALV grid and data reference
      constructor
        IMPORTING
          io_grid  TYPE REF TO cl_gui_alv_grid
          it_table TYPE REF TO data,
      " Method to handle user commands (e.g., button clicks)
      handle_user_command
        FOR EVENT user_command OF cl_gui_alv_grid
        IMPORTING e_ucomm,
      " Method to handle custom toolbar (add/delete buttons)
      handle_toolbar
        FOR EVENT toolbar OF cl_gui_alv_grid
        IMPORTING e_object e_interactive,
      on_data_changed
        FOR EVENT data_changed OF cl_gui_alv_grid
        IMPORTING er_data_changed.
  PRIVATE SECTION.
    " References to the ALV grid and internal table
    DATA:
      mo_grid  TYPE REF TO cl_gui_alv_grid,
      mt_table TYPE REF TO data.
ENDCLASS.
" Implementation of the local class
CLASS lcl_event_handler IMPLEMENTATION.
  " Constructor method: assign input parameters to class attributes
  METHOD constructor.
    mo_grid  = io_grid.
    mt_table = it_table.
  ENDMETHOD.
  " Handle user commands (toolbar buttons)
  METHOD handle_user_command.
    CASE e_ucomm.
        " Case: Delete selected rows
      WHEN '&DEL'.
        DATA: lt_rows   TYPE lvc_t_row, " To store selected row indices
              ls_row    TYPE lvc_s_row,
              lv_answer TYPE c.        " To store popup answer

        FIELD-SYMBOLS: <table>    TYPE STANDARD TABLE,
                       <any_line> TYPE any.
        " Get selected rows from ALV
        CALL METHOD mo_grid->get_selected_rows
          IMPORTING
            et_index_rows = lt_rows.
        " Exit if no rows selected
        CHECK lt_rows IS NOT INITIAL.
        " Confirm deletion with popup
        CALL FUNCTION 'POPUP_TO_CONFIRM'
          EXPORTING
            titlebar              = 'Confirm Deletion'
            text_question         = 'Do you really want to delete selected rows?'
            text_button_1         = 'Yes'
            text_button_2         = 'No'
            default_button        = '2'
            display_cancel_button = ''
          IMPORTING
            answer                = lv_answer.
        " Proceed only if user confirms
        CHECK lv_answer = '1'.
        " Assign internal table reference
        ASSIGN mt_table->* TO <table>.
        CHECK <table> IS ASSIGNED.
        " Loop from bottom to top to delete rows by index
        LOOP AT lt_rows INTO ls_row FROM lines( lt_rows ) TO 1 STEP -1.
          READ TABLE <table> INDEX ls_row-index ASSIGNING <any_line>.
          IF sy-subrc = 0.
            DELETE <table> INDEX ls_row-index.
          ENDIF.
        ENDLOOP.
        " Refresh ALV display after deletion
        CALL METHOD mo_grid->refresh_table_display.
        " Case: Add a new row
      WHEN '&ADD'.
        FIELD-SYMBOLS: <new_line> TYPE any.
        " Assign internal table reference
        ASSIGN mt_table->* TO <table>.
        CHECK <table> IS ASSIGNED.
        " Append empty line to internal table
        APPEND INITIAL LINE TO <table> ASSIGNING <new_line>.
        " Refresh ALV display after addition
        CALL METHOD mo_grid->refresh_table_display.
    ENDCASE.
  ENDMETHOD.

  " Add custom buttons to the ALV toolbar
  METHOD handle_toolbar.
    DATA: ls_button TYPE stb_button.
    " Exit if edit mode is not active

    " Add delete button
    CLEAR ls_button.
    ls_button-function   = '&DEL'.                " Function code
    ls_button-icon       = icon_delete_row.       " Icon for delete
    ls_button-quickinfo  = 'Delete Selected Row(s)'.  " Tooltip
    ls_button-text       = 'Delete'.              " Button text
    ls_button-butn_type  = '0'.                   " Pushbutton
    APPEND ls_button TO e_object->mt_toolbar.

    " Add add button
    CLEAR ls_button.
    ls_button-function   = '&ADD'.                " Function code
    ls_button-icon       = icon_insert_row.       " Icon for insert
    ls_button-quickinfo  = 'Add New Row'.         " Tooltip
    ls_button-text       = 'Add'.                 " Button text
    ls_button-butn_type  = '0'.                   " Pushbutton
    APPEND ls_button TO e_object->mt_toolbar.

  ENDMETHOD.
  METHOD on_data_changed.
    DATA: lv_value TYPE c LENGTH 1,
          lv_text  TYPE char40.

    LOOP AT er_data_changed->mt_good_cells INTO DATA(ls_good).
      IF ls_good-fieldname = 'STOCK_TYPE'.
        lv_value = ls_good-value.

        CASE lv_value.
          WHEN 'U'. lv_text = 'Unrestricted Use'.
          WHEN 'Q'. lv_text = 'Quality Inspection'.
          WHEN 'B'. lv_text = 'Blocked Stock'.
          WHEN OTHERS. lv_text = ''.
        ENDCASE.

        CALL METHOD er_data_changed->modify_cell
          EXPORTING
            i_row_id    = ls_good-row_id
            i_fieldname = 'STOCK_TYPE'
            i_value     = lv_text.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.

ENDCLASS.
