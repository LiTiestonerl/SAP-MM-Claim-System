*&---------------------------------------------------------------------*
*& Include          ZGISCRAPPINGS2_F00
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*& Form alv_create_100_cont
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM alv_create_100_cont .
  DATA(lo_custom_cont) = NEW cl_gui_custom_container(
      repid = sy-repid
      dynnr = sy-dynnr
      container_name = 'GO_CUSTOM_CONT_01' ).
  "Instantiates the ALV grid control inside that container.
  go_grid_01 = NEW #( i_parent = lo_custom_cont ).
ENDFORM.
*&---------------------------------------------------------------------*
*& Form alv_grid_display
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> P_
*&---------------------------------------------------------------------*
FORM alv_grid_display  USING  pv_grid_nm TYPE fieldname.
  PERFORM: alv_layout         USING pv_grid_nm,
           alv_variant        USING pv_grid_nm,
           alv_toolbar        USING pv_grid_nm,
           alv_fieldcatalog   USING pv_grid_nm,
           alv_event          USING pv_grid_nm,
           alv_outtab_display USING pv_grid_nm.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form alv_layout
*&---------------------------------------------------------------------*
*& ALV Layout
*&---------------------------------------------------------------------*
FORM alv_layout  USING    pv_grid_nm.
  CLEAR: gv_grid_title, gs_layout.

  "-- Set title of ALV
  PERFORM alv_set_gridtitle USING pv_grid_nm.

  gs_layout = VALUE #( sel_mode   = 'A'
                       "stylefname = 'STYLE'
                       smalltitle = abap_on
                       cwidth_opt = abap_on
                       zebra      = abap_true
                       grid_title = gv_grid_title ).
ENDFORM.

*&---------------------------------------------------------------------*
*& Form alv_set_gridtitle
*&---------------------------------------------------------------------*
*& Set Title of ALV
*&---------------------------------------------------------------------*
FORM alv_set_gridtitle  USING  pv_grid_nm TYPE fieldname.
  DATA: lv_entry TYPE i,
        lv_title TYPE itex132.

  CLEAR: lv_entry, lv_title.

  lv_title = COND #( WHEN pv_grid_nm = 'GO_GRID_01' THEN '' ).

  lv_entry = lines( COND #( WHEN pv_grid_nm = 'GO_GRID_01' THEN gt_item ) ).

  "--Convert ALV Grid Title
  PERFORM set_alv_title_with_total USING lv_title lv_entry
                                CHANGING gv_grid_title.
ENDFORM.

*&---------------------------------------------------------------------*
*& Form set_alv_title_with_total
*&---------------------------------------------------------------------*
*& Convert ALV Grid Title
*&---------------------------------------------------------------------*
FORM set_alv_title_with_total  USING    pv_title TYPE itex132
                                        pv_entry TYPE i
                               CHANGING pv_alvt1 TYPE lvc_title.
  "-- Configuring ALV Title including Total
  CONSTANTS: lc_total_text  TYPE string VALUE '(Total: ',
             lc_right_paren TYPE string VALUE ')'.

  CLEAR: pv_alvt1.
  WRITE pv_entry TO pv_alvt1.
  CONDENSE pv_alvt1.

  pv_alvt1 = |{ pv_title } { lc_total_text } { pv_alvt1 } { lc_right_paren } |.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form alv_variant
*&---------------------------------------------------------------------*
*& ALV Variant
*&---------------------------------------------------------------------*
FORM alv_variant  USING    pv_grid_nm TYPE fieldname.

  CLEAR: gs_variant.

  gs_variant = VALUE #( report   = sy-repid
                        username = sy-uname
                        handle   = COND #( WHEN pv_grid_nm = 'GO_GRID_01' THEN '01' ) ).
ENDFORM.

*&---------------------------------------------------------------------*
*& Form alv_toolbar
*&---------------------------------------------------------------------*
*& Set ALV Toolbar
*&---------------------------------------------------------------------*
FORM alv_toolbar  USING    pv_grid_nm.
  CLEAR gt_exclude.
  CASE pv_grid_nm.
    WHEN 'GO_GRID_01'.
      PERFORM get_alv_exclude_tb_func USING gt_exclude.
  ENDCASE.
ENDFORM.

*&---------------------------------------------------------------------*
*& Form get_alv_exclude_tb_func
*&---------------------------------------------------------------------*
*& Set exclude button of toolbar
*&---------------------------------------------------------------------*
FORM get_alv_exclude_tb_func  USING    pt_exclude TYPE ui_functions.
   pt_exclude = VALUE ui_functions(
      ( cl_gui_alv_grid=>mc_fc_loc_append_row )
      ( cl_gui_alv_grid=>mc_fc_loc_insert_row )
      ( cl_gui_alv_grid=>mc_fc_loc_delete_row )
      ( cl_gui_alv_grid=>mc_fc_loc_copy_row   )
      ( cl_gui_alv_grid=>mc_fc_filter )
  ).
ENDFORM.

*&---------------------------------------------------------------------*
*& Form alv_fieldcatalog
*&---------------------------------------------------------------------*
*& Set Field Catalog
*&---------------------------------------------------------------------*
FORM alv_fieldcatalog  USING    pv_grid_nm TYPE fieldname.
  CLEAR gt_fieldcat.
  CASE pv_grid_nm.
    WHEN 'GO_GRID_01'.
      PERFORM alv_fieldcatalog_01 CHANGING gt_fieldcat.
  ENDCASE.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form alv_fieldcatalog_01
*&---------------------------------------------------------------------*
*& alv_fieldcatalog_01
*&---------------------------------------------------------------------*
FORM alv_fieldcatalog_01 CHANGING pt_fieldcat TYPE lvc_t_fcat.

  DATA ls_fcat TYPE lvc_s_fcat.

  DEFINE _add_fieldcat.
    CLEAR ls_fcat.
    ls_fcat-fieldname = &1.

    IF ls_fcat-fieldname = 'MATERIAL_SHORT_TEXT'.
      ls_fcat-edit        = abap_false.
      ls_fcat-f4availabl  = abap_false.
    ELSE.
      ls_fcat-edit        = abap_true.
      ls_fcat-f4availabl  = abap_true.
      ls_fcat-ref_table   = 'ZGIITEM'.
      ls_fcat-ref_field   = &1.
    ENDIF.

    ls_fcat-just        = &2.
    ls_fcat-col_opt     = &3.
    ls_fcat-coltext     = &4.
    ls_fcat-seltext     = &4.
    ls_fcat-tooltip     = &4.
    ls_fcat-scrtext_l   = &4.
    ls_fcat-scrtext_m   = &4.
    ls_fcat-scrtext_s   = &4.
    ls_fcat-fix_column  = &5.
    ls_fcat-icon        = abap_on.

    APPEND ls_fcat TO pt_fieldcat.
  END-OF-DEFINITION.

  " Add fieldcatalog
  _add_fieldcat:
  'PLANT'               'L' abap_on 'Plant'               abap_off,
  'SLOC'                'L' abap_on 'Sloc'                abap_off,
  'MATERIAL_NUMBER'     'C' abap_on 'Material Number'     abap_off,
  'MATERIAL_SHORT_TEXT' 'R' abap_on 'Material Short Text' abap_off,
  'STOCK_TYPE'          'R' abap_on 'Stock Type'          abap_off,
  'QTY_IN_UNE'          'L' abap_on 'Qty in Une'          abap_off,
  'EUN'                 'L' abap_on 'EUN'                 abap_off.

  "--- Gán dropdown handle cho STOCK_TYPE
  FIELD-SYMBOLS <f> TYPE lvc_s_fcat.
  LOOP AT pt_fieldcat ASSIGNING <f>.
    CASE <f>-fieldname.
      WHEN 'STOCK_TYPE'.
        <f>-drdn_hndl   = 1.
        <f>-drdn_alias  = abap_true.
    ENDCASE.
  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form alv_event
*&---------------------------------------------------------------------*
*& Set event ALV
*&---------------------------------------------------------------------*
FORM alv_event  USING pv_grid_nm TYPE fieldname.
  CHECK go_grid_01 IS BOUND.

  IF go_event_handler_01 IS INITIAL.
    CREATE OBJECT go_event_handler_01
      EXPORTING
        io_grid  = go_grid_01
        it_table = REF #( gt_item ).
  ENDIF.

  SET HANDLER:
    go_event_handler_01->handle_user_command FOR go_grid_01,
    go_event_handler_01->handle_toolbar      FOR go_grid_01.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form alv_outtab_display
*&---------------------------------------------------------------------*
*& Display ALV
*&---------------------------------------------------------------------*
FORM alv_outtab_display  USING pv_grid_nm TYPE fieldname.
  DATA: lt_dropdown TYPE lvc_t_dral,
        ls_dropdown TYPE lvc_s_dral,
        ls_fcat     TYPE lvc_s_fcat.

  FIELD-SYMBOLS: <lfs_grid> TYPE REF TO cl_gui_alv_grid.

  ASSIGN (pv_grid_nm) TO <lfs_grid>.
  IF <lfs_grid> IS NOT ASSIGNED OR <lfs_grid> IS NOT BOUND.
    MESSAGE 'ALV Grid not initialized' TYPE 'S' DISPLAY LIKE 'E'.
    RETURN.
  ENDIF.

  DATA(lo_grid) = <lfs_grid>.

  " Cho STOCK_TYPE dropdown
  READ TABLE gt_fieldcat INTO ls_fcat WITH KEY fieldname = 'STOCK_TYPE'.
  IF sy-subrc = 0.
    ls_fcat-edit      = abap_true.
    ls_fcat-drdn_hndl = 1.
    MODIFY gt_fieldcat FROM ls_fcat INDEX sy-tabix.
  ENDIF.

  TRY.
      lo_grid->set_ready_for_input( 1 ).
      lo_grid->set_table_for_first_display(
        EXPORTING
          i_buffer_active    = abap_true
          i_bypassing_buffer = abap_true
          i_save             = 'A'
          i_default          = abap_true
          is_layout          = gs_layout
          is_variant         = gs_variant
          it_toolbar_excluding = gt_exclude
        CHANGING
          it_outtab          = gt_item
          it_fieldcatalog    = gt_fieldcat
      ).
      " handle = 2 (Stock Type)
      CLEAR ls_dropdown.
      ls_dropdown-handle = 1.
      ls_dropdown-int_value = 'F'. ls_dropdown-value = 'Unrestricted-use'.
      APPEND ls_dropdown
      TO lt_dropdown.

      ls_dropdown-int_value = 'X'. ls_dropdown-value = 'Quality inspection'.
      APPEND ls_dropdown
      TO lt_dropdown.

      ls_dropdown-int_value = 'S'. ls_dropdown-value = 'Blocked'.
      APPEND ls_dropdown
      TO lt_dropdown.

      " Gán dropdown vào ALV
      lo_grid->set_drop_down_table(
        EXPORTING it_drop_down_alias = lt_dropdown ).

      cl_gui_control=>set_focus( control = lo_grid ).
      cl_gui_cfw=>flush( ).
    CATCH cx_root INTO DATA(lx_error).
      MESSAGE lx_error->get_text( ) TYPE 'S' DISPLAY LIKE 'E'.
  ENDTRY.
ENDFORM.
