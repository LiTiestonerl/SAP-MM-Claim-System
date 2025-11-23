*&---------------------------------------------------------------------*
*& Include          ZPG_MM_CLAIMAMOUNTC01
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

      handle_data_changed
        FOR EVENT data_changed OF cl_gui_alv_grid
        IMPORTING er_data_changed e_ucomm,
      handle_data_changed_finished
        FOR EVENT data_changed_finished OF cl_gui_alv_grid
        IMPORTING e_modified et_good_cells.

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
    IF gs_edit <> 'X'.
      RETURN.
    ENDIF.

    " Add add button
    CLEAR ls_button.
    ls_button-function   = '&ADD'.                " Function code
    ls_button-icon       = icon_insert_row.       " Icon for insert
    ls_button-quickinfo  = 'Add New Row'.         " Tooltip
    ls_button-text       = 'Add'.                 " Button text
    ls_button-butn_type  = '0'.                   " Pushbutton
    APPEND ls_button TO e_object->mt_toolbar.

    " Add delete button
    CLEAR ls_button.
    ls_button-function   = '&DEL'.                " Function code
    ls_button-icon       = icon_delete_row.       " Icon for delete
    ls_button-quickinfo  = 'Delete Selected Row(s)'.  " Tooltip
    ls_button-text       = 'Delete'.              " Button text
    ls_button-butn_type  = '0'.                   " Pushbutton
    APPEND ls_button TO e_object->mt_toolbar.


  ENDMETHOD.

    METHOD handle_data_changed.
  DATA: ls_mod   LIKE LINE OF er_data_changed->mt_mod_cells,
        lv_saknr TYPE saknr,
        lv_dc    TYPE c LENGTH 1,
        lv_fname TYPE c LENGTH 30.
  FIELD-SYMBOLS: <tab>  TYPE STANDARD TABLE,
                 <row>  TYPE any,
                 <cell> TYPE any.

  ASSIGN mt_table->* TO <tab>.
  IF <tab> IS NOT ASSIGNED.
    RETURN.
  ENDIF.

  LOOP AT er_data_changed->mt_mod_cells INTO ls_mod.
    lv_fname = ls_mod-fieldname.

    READ TABLE <tab> ASSIGNING <row> INDEX ls_mod-row_id.
    IF sy-subrc <> 0 OR <row> IS NOT ASSIGNED.
      CONTINUE.
    ENDIF.

    ASSIGN COMPONENT lv_fname OF STRUCTURE <row> TO <cell>.
    IF <cell> IS NOT ASSIGNED.
      CONTINUE.
    ENDIF.

    CASE lv_fname.
      WHEN 'GL_ACCOUNT'.

  lv_saknr = |{ ls_mod-value ALPHA = IN }|.
  <cell>   = lv_saknr.

  " --- fill SHORT_TEXT theo BUKRS + SPRAS ---
  DATA(lv_txt20) = VALUE skat-txt20( ).

  SELECT SINGLE b~txt20
    INTO @lv_txt20
    FROM skb1 AS a
    INNER JOIN skat AS b
      ON a~saknr = b~saknr
   WHERE a~bukrs = @gs_header-company_code
     AND a~saknr = @lv_saknr
     AND b~spras = @sy-langu.

  FIELD-SYMBOLS: <cell_st> TYPE any.
  ASSIGN COMPONENT 'SHORT_TEXT' OF STRUCTURE <row> TO <cell_st>.
  IF <cell_st> IS ASSIGNED.
    <cell_st> = lv_txt20.
  ENDIF.




      WHEN 'DC_INDICATOR'.
        lv_dc = ls_mod-value.
        TRANSLATE lv_dc TO UPPER CASE.
        IF lv_dc = 'S' OR lv_dc = 'H'.
          <cell> = lv_dc.
        ELSE.
          <cell> = ls_mod-value.
        ENDIF.

      WHEN OTHERS.
        <cell> = ls_mod-value.
    ENDCASE.
  ENDLOOP.
ENDMETHOD.



  METHOD handle_data_changed_finished.
    DATA ls_stbl TYPE lvc_s_stbl.
    ls_stbl-row = 'X'.
    ls_stbl-col = 'X'.
    IF mo_grid IS BOUND.
      mo_grid->refresh_table_display( is_stable = ls_stbl ).
    ENDIF.
  ENDMETHOD.





ENDCLASS.
