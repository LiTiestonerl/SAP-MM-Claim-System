*&---------------------------------------------------------------------*
*& Include  ZPG_MM_DOC_HISTORY_O01
*&---------------------------------------------------------------------*
FORM display_hist_alv USING it_hist TYPE ty_t_hist.

  DATA: lo_funcs TYPE REF TO cl_salv_functions_list,
        lo_sorts TYPE REF TO cl_salv_sorts,
        lo_cols  TYPE REF TO cl_salv_columns_table,
        lo_col   TYPE REF TO cl_salv_column,
        lx_err   TYPE REF TO cx_root.

  TRY.
      cl_salv_table=>factory(
        IMPORTING r_salv_table = go_salv
        CHANGING  t_table      = it_hist ).

      " Toolbar
      lo_funcs = go_salv->get_functions( ).
      lo_funcs->set_all( abap_true ).

      " --- Sorting: do NOT sort by POSTDATE so it repeats on each row ---
      lo_sorts = go_salv->get_sorts( ).
      lo_sorts->clear( ).
      lo_sorts->add_sort( columnname = 'DOCNO' ).      " stable order
*     lo_sorts->add_sort( columnname = 'CATEGORY' ).   " optional

      " Columns
      lo_cols = go_salv->get_columns( ).

      lo_col = lo_cols->get_column( 'CATEGORY' ).  lo_col->set_short_text( 'Type' ).
      lo_col = lo_cols->get_column( 'DOCNO' ).     lo_col->set_long_text( 'Document No.' ).
      lo_col = lo_cols->get_column( 'DOCYEAR' ).   lo_col->set_short_text( 'Year' ).
      lo_col = lo_cols->get_column( 'MOVETYPE' ).  lo_col->set_short_text( 'MvT' ).
      lo_col = lo_cols->get_column( 'MOVETEXT' ).  lo_col->set_long_text( 'Movement Text' ).
      lo_col->set_output_length( 35 ).             " a bit wider for readability
      lo_col = lo_cols->get_column( 'MAT_TEXT' ).  lo_col->set_long_text( 'Material Short Text' ).

      lo_cols->get_column( 'AMOUNT' )->set_alignment( if_salv_c_alignment=>right ).
      lo_cols->get_column( 'QTY'    )->set_alignment( if_salv_c_alignment=>right ).

      " Zebra + title
      go_salv->get_display_settings( )->set_striped_pattern( abap_true ).

      DATA: lv_part  TYPE lvc_title,
            lv_title TYPE lvc_title.
      IF p_view = c_view_gi.
        lv_part = 'GI Scrapping'.
      ELSEIF p_view = c_view_ret.
        lv_part = 'Return Stock (161)'.
      ELSEIF p_view = c_view_clm.
        lv_part = 'Claim Amount'.
      ELSEIF p_view = c_view_101.
        lv_part = 'Good Receipt (101)'.
      ELSEIF p_view = c_view_122.
        lv_part = 'Return delivery (122)'.
      ELSEIF p_view = c_view_exc.
        lv_part = 'Exchange Material (101/161)'.
      ENDIF.
      CONCATENATE 'History –' lv_part INTO lv_title SEPARATED BY space.
      go_salv->get_display_settings( )->set_list_header( lv_title ).

      go_salv->display( ).

    CATCH cx_salv_msg
          cx_salv_not_found
          cx_salv_existing
          cx_salv_data_error
          INTO lx_err.
      MESSAGE lx_err->get_text( ) TYPE 'S' DISPLAY LIKE 'E'.
  ENDTRY.

ENDFORM.
