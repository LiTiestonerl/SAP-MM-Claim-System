*&---------------------------------------------------------------------*
*& Include          ZPG_MM_RETURN_STOCKF00
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
*& ALV grid display
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
                       "stylefname = 'CELL_STYLE'
                       smalltitle = abap_on
                       cwidth_opt = abap_on
                       zebra      = abap_true
                       grid_title = gv_grid_title ).
ENDFORM.
*&---------------------------------------------------------------------*
*& Form alv_set_gridtitle
*&---------------------------------------------------------------------*
FORM alv_set_gridtitle  USING  pv_grid_nm TYPE fieldname.
  DATA: lv_entry TYPE i,
        lv_title TYPE itex132.

  CLEAR: lv_entry, lv_title.

  CASE gv_alv_mode.
    WHEN 'CREPO'.   " Create PO screen (100)
      lv_title = COND #( WHEN pv_grid_nm = 'GO_GRID_01' THEN '' ).
      lv_entry = lines( COND #( WHEN pv_grid_nm = 'GO_GRID_01' THEN gt_po_item ) ).

    WHEN 'POSTGR'.  " Goods Receipt screen (200)
      lv_title = COND #( WHEN pv_grid_nm = 'GO_GRID_01' THEN '' ).
      lv_entry = lines( COND #( WHEN pv_grid_nm = 'GO_GRID_01' THEN gt_receipt_item ) ).

    WHEN 'POSTIV'.  " Invoice screen (300)
      lv_title = COND #( WHEN pv_grid_nm = 'GO_GRID_01' THEN '' ).
      lv_entry = lines( COND #( WHEN pv_grid_nm = 'GO_GRID_01' THEN gt_invoice_item ) ).

    WHEN 'POHIS'.  " Purchase Order History
      lv_title = COND #( WHEN pv_grid_nm = 'GO_GRID_01' THEN |(Vendor: { p_hist })| ).
      lv_entry = lines( COND #( WHEN pv_grid_nm = 'GO_GRID_01' THEN gt_po_hist ) ).
    WHEN 'MDOC'.   " NEW
      lv_title = COND #( WHEN pv_grid_nm = 'GO_GRID_01' THEN |(Vendor: { p_mdoc })| ).
      lv_entry = lines( COND #( WHEN pv_grid_nm = 'GO_GRID_01' THEN gt_matdoc ) ).

    WHEN OTHERS.
      CLEAR: lv_title, lv_entry.
  ENDCASE.

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
  CONSTANTS: lc_total_text  TYPE string VALUE 'Return Stock System (Total: ',
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
  pt_exclude = VALUE #( ( cl_gui_alv_grid=>mc_fc_filter )
  ( cl_gui_alv_grid=>mc_fc_loc_insert_row )
  ( cl_gui_alv_grid=>mc_fc_loc_delete_row )
  ( cl_gui_alv_grid=>mc_fc_loc_append_row )
  ( cl_gui_alv_grid=>mc_fc_loc_copy )
  ( cl_gui_alv_grid=>mc_fc_loc_copy_row )
  ( cl_gui_alv_grid=>mc_fc_loc_undo )
  ( cl_gui_alv_grid=>mc_fc_loc_paste_new_row )
  ( cl_gui_alv_grid=>mc_fc_loc_paste )
  ( cl_gui_alv_grid=>mc_fc_loc_cut )
  ( cl_gui_alv_grid=>mc_fc_help )
  ( cl_gui_alv_grid=>mc_fc_info )


  ).
ENDFORM.
*&---------------------------------------------------------------------*
*& Form alv_fieldcatalog
*&---------------------------------------------------------------------*
FORM alv_fieldcatalog  USING    pv_grid_nm TYPE fieldname.
  " Clear all fieldcatalogs first
  CLEAR: gt_po_fieldcat,
         gt_gr_fieldcat,
         gt_inv_fieldcat,
         gt_receipt_fieldcat,
         gt_hist_fieldcat.

  CASE pv_grid_nm.
    WHEN 'GO_GRID_01'.
      CASE gv_alv_mode.
        WHEN 'CREPO'.
          PERFORM alv_fieldcatalog_01 CHANGING gt_po_fieldcat.
        WHEN 'POSTGR'.
          PERFORM alv_fieldcatalog_01 CHANGING gt_gr_fieldcat.
        WHEN 'POSTIV'.
          PERFORM alv_fieldcatalog_01 CHANGING gt_inv_fieldcat.
        WHEN 'POHIS'.           " Purchase Order History
          PERFORM alv_fieldcatalog_01 CHANGING gt_hist_fieldcat.
        WHEN 'MDOC'.
          PERFORM alv_fieldcatalog_01 CHANGING gt_mdoc_fieldcat.
        WHEN OTHERS.
          " no-op
      ENDCASE.
    WHEN OTHERS.
      " no-op for other grid names
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
    ls_fcat-fieldname  = &1.
    ls_fcat-coltext    = &2.
    ls_fcat-seltext    = &2.
    ls_fcat-scrtext_l  = &2.
    ls_fcat-scrtext_m  = &2.
    ls_fcat-scrtext_s  = &2.
    ls_fcat-just       = &3.
    ls_fcat-col_opt    = abap_on.
    IF r_crePO = 'X'.
      ls_fcat-ref_table  = 'ZTGSU06'.
    ELSEIF r_postGR = 'X'.
      ls_fcat-ref_table  = 'ZITEM_MIRO'.
    ELSEIF r_postiv = 'X'.
      ls_fcat-ref_table  = 'ZPAYMENT_ITEM'.
    ENDIF.
    ls_fcat-ref_field  = &1.
    ls_fcat-edit       = &4.
    ls_fcat-f4availabl = &5.
    ls_fcat-drdn_alias = abap_true.
    APPEND ls_fcat TO pt_fieldcat.
  END-OF-DEFINITION.

  "--- Build fieldcatalog theo ch# ## ---
  CASE gv_alv_mode..
    WHEN 'CREPO'.
      _add_fieldcat:
     'MATNR'      'Material'         'L' abap_true  abap_true,
     'ZMATE_DES'  'Short Text'       'L' abap_false abap_false,
     'BSTMG'      'PO Quantity'      'R' abap_true  abap_false,
     'ZUNIT'      'Base Unit'        'C' abap_false abap_false,
     'EINDT'      'Delivery Date'    'L' abap_true  abap_true,
     'BPREI'      'Net Price'        'R' abap_false  abap_false,
     'ZCURRENCY'  'Currency'         'L' abap_false  abap_false,
     'PEINH'      'Price unit'       'R' abap_false  abap_false,
     'MATKL'      'Material Group'   'L' abap_false  abap_false,
     'ZPLANT'     'Plant'            'L' abap_true  abap_true,
     'RETPO'      'Returns Item'     'C' abap_true  abap_false.
    WHEN 'POSTGR'.
      _add_fieldcat:
 "    'LINE'           'Line'             'L' abap_false abap_false,
     'SHORT_TEXT'     'Short Text'             'L' abap_false  abap_false,
     'QTY_IN_UNE'     'PO Quantity'            'R' abap_false  abap_false,
     'UOM'            'UoM'                    'C' abap_false  abap_true,
     'MOVEMENT_TYPE'  'Movement Type'          'L' abap_true   abap_true,
     'STOCK_TYPE'     'Stock Type'             'L' abap_true   abap_true,
     'PLANT'          'Plant'                  'L' abap_false  abap_true,
     'VENDOR'         'Vendor'                 'L' abap_false  abap_true,
     'DELIVERY_COMPL' 'Delivery Compl. Ind.'   'C' abap_true   abap_false,
     'QUANTITY'       'Quantity'               'R' abap_false  abap_false,
     'DEL_NOTE_QTY'   'Del. Note Qty'          'R' abap_false  abap_false,
     'RETURNS_ITEM'   'Returns Item'           'C' abap_false  abap_false.
    WHEN 'POSTIV'.
      _add_fieldcat:
     'MATERIAL'     'Material'        'L' abap_false  abap_true,   " MATERIAL
     'SHORT_TEXT'   'Short Text'      'L' abap_false  abap_false,  " SHORT_TEXT
     'AMOUNT'       'Amount'          'R' abap_false  abap_false,  " AMOUNT
     'QUANTITY'     'Quantity'        'R' abap_false  abap_false,  " QUANTITY
     'UOM'          'UoM'             'C' abap_false  abap_true,
     'TAX_CODE'     'Tax Code'        'C' abap_true   abap_false.  " TAX_CODE
    WHEN 'POHIS'.
      _add_fieldcat:
     'EBELN'      'PO Number'      'L' abap_false  abap_true,
     'EBELP'      'PO Item'        'L' abap_false  abap_true,
     'ERNAM'      'Created By'     'L' abap_false  abap_false,
     'ERDAT'      'Created On'     'L' abap_false  abap_true,
     'LIFNR'      'Vendor'         'L' abap_false  abap_true,
     'MATNR'      'Material'       'L' abap_false  abap_true,
     'ZMATE_DES'  'Short Text'     'L' abap_false  abap_false,
     'BSTMG'      'Quantity'       'R' abap_false  abap_false,
     'ZUNIT'      'UoM'            'C' abap_false  abap_false,
     'BPREI'      'Net Price'      'R' abap_false  abap_false,
     'ZCURRENCY'  'Currency'       'L' abap_false  abap_false,
     'ZPLANT'     'Plant'          'L' abap_false  abap_true.
    WHEN 'MDOC'.
      _add_fieldcat:
       'MBLNR' 'Mat. Doc'            'L' abap_false abap_false,
       'MJAHR' 'Year'                'L' abap_false abap_false,
       'ZEILE' 'Item'                'R' abap_false abap_false,
       'BUDAT' 'Posting Date'        'L' abap_false abap_false,
       'CPUDT' 'Entry Date'          'L' abap_false abap_false,
       'CPUTM' 'Time'                'L' abap_false abap_false,
       'USNAM' 'User'                'L' abap_false abap_false,
       'BWART' 'MvT'                 'L' abap_false abap_false,
       'MATNR' 'Material'            'L' abap_false abap_true,
       'SHORT_TEXT' 'Short Text'     'L' abap_false abap_false,
       'WERKS' 'Plant'               'L' abap_false abap_true,
       'LGORT' 'SLoc'                'L' abap_false abap_true,
       'MENGE' 'Qty'                 'R' abap_false abap_false,
       'MEINS' 'UoM'                 'C' abap_false abap_true,
       'EBELN' 'PO'                  'L' abap_false abap_true,
       'EBELP' 'PO Item'             'R' abap_false abap_true,
       'LIFNR' 'Vendor'              'L' abap_false abap_true.
    WHEN OTHERS.
  ENDCASE.

  "--- Gán quan h# ##nh d#ng & checkbox (dùng chung) ---
  FIELD-SYMBOLS <f> TYPE lvc_s_fcat.
  LOOP AT pt_fieldcat ASSIGNING <f>.
    CASE <f>-fieldname.
      WHEN 'QTY_IN_UNE' OR 'QUANTITY' OR 'DEL_NOTE_QTY'.
        <f>-qfieldname = COND lvc_fname(
                           WHEN r_postGR = 'X' THEN 'UOM'
                           WHEN r_postIV = 'X' THEN 'UOM'
                           WHEN r_crePO = 'X' THEN 'ZUNIT' ).
      WHEN 'BPREI'.
        <f>-cfieldname = 'ZCURRENCY'.         " price # currency (crePO and postIV)
      WHEN 'RETPO' OR 'RETURNS_ITEM'." OR 'DELIVERY_COMPL'.
        <f>-checkbox  = abap_true.
        <f>-outputlen = 1.
      WHEN 'MOVEMENT_TYPE'.
        <f>-drdn_hndl = 1. " Handle cho dropdown Movement Type
      WHEN 'STOCK_TYPE'.
        <f>-drdn_hndl = 2. " Handle cho dropdown Stock Type
        "WHEN 'MWSKZ'.
      WHEN 'TAX_CODE'.
        <f>-drdn_hndl = 3. " N#u mu#n dropdown cho Tax Code
      WHEN 'DELIVERY_COMPL'.
        <f>-drdn_hndl = 4. " N#u mu#n dropdown cho Delivery Compl. Ind.
    ENDCASE.
  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form alv_event
*&---------------------------------------------------------------------*
*& Set event ALV
*&---------------------------------------------------------------------*
FORM alv_event  USING pv_grid_nm TYPE fieldname.
  FIELD-SYMBOLS: <o_grid> TYPE REF TO cl_gui_alv_grid.
  DATA: lr_table TYPE REF TO data,
        lo_grid  TYPE REF TO cl_gui_alv_grid. " bi#n object reference th#t

  ASSIGN (pv_grid_nm) TO <o_grid>.
  CHECK <o_grid> IS ASSIGNED AND <o_grid> IS BOUND.

  lo_grid = <o_grid>. " Gán sang bi#n reference th#t

  CASE gv_alv_mode.
    WHEN 'CREPO'.
      lr_table = REF #( gt_po_item ).
    WHEN 'POSTGR'.
      lr_table = REF #( gt_receipt_item ).
    WHEN 'POSTIV'.
      lr_table = REF #( gt_invoice_item ).
    WHEN 'POHIS'.
      lr_table = REF #( gt_po_hist ).
    WHEN 'MDOC'.
      lr_table = REF #( gt_matdoc ).
    WHEN OTHERS.
      lr_table = REF #( gt_invoice_item ).

  ENDCASE.

  CHECK lr_table IS BOUND.

  IF go_event_handler_01 IS BOUND.
    FREE go_event_handler_01.
  ENDIF.

  CREATE OBJECT go_event_handler_01
    EXPORTING
      io_grid  = lo_grid
      it_table = lr_table.

  SET HANDLER go_event_handler_01->on_data_changed   FOR go_grid_01.
  SET HANDLER go_event_handler_01->handle_toolbar    FOR go_grid_01.
  SET HANDLER go_event_handler_01->handle_user_command FOR go_grid_01.

  go_event_handler_01->set_table_ref(
    COND #(
      WHEN r_crePO  = 'X' THEN REF #( gt_po_item )
      WHEN r_postGR = 'X' THEN REF #( gt_receipt_item )
      WHEN r_postIv = 'X' THEN REF #( gt_invoice_item )
      WHEN r_hist   = 'X' THEN REF #( gt_po_hist )
      WHEN r_mdoc   = 'X' THEN REF #( gt_matdoc )
      ELSE REF #( gt_po_item )
    )
  ).
ENDFORM.

*&---------------------------------------------------------------------*
*& Form alv_outtab_display
*&---------------------------------------------------------------------*
*& Display ALV
*&---------------------------------------------------------------------*
FORM alv_outtab_display  USING pv_grid_nm TYPE fieldname.
  DATA: lt_fieldcatalog TYPE lvc_t_fcat,
        lt_dropdown     TYPE lvc_t_dral,
        ls_dropdown     TYPE lvc_s_dral,
        ls_fcat         TYPE lvc_s_fcat.

  FIELD-SYMBOLS: <lfs_grid>  TYPE REF TO cl_gui_alv_grid,
                 <lt_outtab> TYPE ANY TABLE.

  ASSIGN (pv_grid_nm) TO <lfs_grid>.
  IF <lfs_grid> IS NOT ASSIGNED OR <lfs_grid> IS NOT BOUND.
    MESSAGE 'ALV Grid is not properly initialized (field-symbol)' TYPE 'S' DISPLAY LIKE 'E'.
    RETURN.
  ENDIF.

  DATA(lo_grid) = <lfs_grid>.
  IF lo_grid IS INITIAL OR lo_grid IS NOT BOUND.
    MESSAGE 'ALV Grid is not properly initialized (grid object)' TYPE 'S' DISPLAY LIKE 'E'.
    RETURN.
  ENDIF.

  CASE gv_alv_mode.
    WHEN 'CREPO'.
      lt_fieldcatalog = gt_po_fieldcat.
      ASSIGN gt_po_item      TO <lt_outtab>.
    WHEN 'POSTGR'.
      lt_fieldcatalog = gt_gr_fieldcat.
      ASSIGN gt_receipt_item TO <lt_outtab>.
    WHEN 'POSTIV'.
      lt_fieldcatalog = gt_inv_fieldcat.
      ASSIGN gt_invoice_item TO <lt_outtab>.
    WHEN 'POHIS'.
      lt_fieldcatalog = gt_hist_fieldcat.
      ASSIGN gt_po_hist TO <lt_outtab>.
    WHEN 'MDOC'.
      lt_fieldcatalog = gt_mdoc_fieldcat.
      ASSIGN gt_matdoc   TO <lt_outtab>.
    WHEN OTHERS.
  ENDCASE.

  IF <lt_outtab> IS NOT ASSIGNED.
    MESSAGE 'Outtab is not assigned (check radio & data tables)' TYPE 'S' DISPLAY LIKE 'E'.
    RETURN.
  ENDIF.

  "GÁN DROPDOWN HANDLE CHO CÁC C#T
  READ TABLE lt_fieldcatalog INTO ls_fcat WITH KEY fieldname = 'MOVEMENT_TYPE'.
  IF sy-subrc = 0.
    ls_fcat-edit      = abap_true.
    ls_fcat-drdn_hndl = 1.
    MODIFY lt_fieldcatalog FROM ls_fcat INDEX sy-tabix.
  ENDIF.

  READ TABLE lt_fieldcatalog INTO ls_fcat WITH KEY fieldname = 'STOCK_TYPE'.
  IF sy-subrc = 0.
    ls_fcat-edit       = abap_true.
    ls_fcat-drdn_hndl  = 2.
    ls_fcat-drdn_alias = abap_true.
    MODIFY lt_fieldcatalog FROM ls_fcat INDEX sy-tabix.
  ENDIF.

  READ TABLE lt_fieldcatalog INTO ls_fcat WITH KEY fieldname = 'TAX_CODE'.
  IF sy-subrc = 0.
    ls_fcat-edit       = abap_true.
    ls_fcat-drdn_hndl  = 3.
    ls_fcat-drdn_alias = abap_true.
    MODIFY lt_fieldcatalog FROM ls_fcat INDEX sy-tabix.
  ENDIF.

  READ TABLE lt_fieldcatalog INTO ls_fcat WITH KEY fieldname = 'DELIVERY_COMPL'.
  IF sy-subrc = 0.
    ls_fcat-edit      = abap_true.
    ls_fcat-drdn_hndl = 4.
    MODIFY lt_fieldcatalog FROM ls_fcat INDEX sy-tabix.
  ENDIF.

  TRY.
      lo_grid->set_ready_for_input(
        i_ready_for_input = COND #( WHEN gs_edit = 'X' THEN 1 ELSE 0 ) ).

      lo_grid->set_table_for_first_display(
        EXPORTING
          i_buffer_active       = abap_true
          i_bypassing_buffer    = abap_true
          i_save                = 'A'
          i_default             = abap_true
          is_layout             = gs_layout
          is_variant            = gs_variant
          it_toolbar_excluding  = gt_exclude
        CHANGING
          it_outtab             = <lt_outtab>
          it_fieldcatalog       = lt_fieldcatalog ).

      "----- Danh sách dropdown alias -----
      CLEAR lt_dropdown.

      " handle = 1 (Movement Type) — no alias (just key)
      CLEAR ls_dropdown.
      ls_dropdown-handle = 1.
      ls_dropdown-value  = '101'. APPEND ls_dropdown TO lt_dropdown.
      ls_dropdown-value  = '161'. APPEND ls_dropdown TO lt_dropdown.
      ls_dropdown-value  = '122'. APPEND ls_dropdown TO lt_dropdown.

      " handle = 2 (Stock Type)
      CLEAR ls_dropdown.
      ls_dropdown-handle     = 2.
      ls_dropdown-int_value  = 'F'.
      ls_dropdown-value      = 'Unrestricted Use'. " Hi#n th#
      APPEND ls_dropdown TO lt_dropdown.

      ls_dropdown-int_value  = '2'.
      ls_dropdown-value      = 'Quality Inspection'.
      APPEND ls_dropdown TO lt_dropdown.

      ls_dropdown-int_value  = '3'.
      ls_dropdown-value      = 'Blocked Stock'.
      APPEND ls_dropdown TO lt_dropdown.

      " handle = 3 (Tax Code) — alias optional
      CLEAR ls_dropdown.
      ls_dropdown-handle    = 3.
      ls_dropdown-int_value = 'V0'. ls_dropdown-value = 'V0 (0%)'.   APPEND ls_dropdown TO lt_dropdown.
      ls_dropdown-int_value = 'V1'. ls_dropdown-value = 'V1 (10%)'.  APPEND ls_dropdown TO lt_dropdown.
      ls_dropdown-int_value = 'A0'. ls_dropdown-value = 'A0 ...'.    APPEND ls_dropdown TO lt_dropdown.
      ls_dropdown-int_value = 'A1'. ls_dropdown-value = 'A1 ...'.    APPEND ls_dropdown TO lt_dropdown.
      ls_dropdown-int_value = 'XI'. ls_dropdown-value = 'Input Tax'.    APPEND ls_dropdown TO lt_dropdown.
      ls_dropdown-int_value = 'XO'. ls_dropdown-value = 'Output Tax.'.    APPEND ls_dropdown TO lt_dropdown.

      " handle = 4 (Delivery Compl. Ind.)
      CLEAR ls_dropdown.
      ls_dropdown-handle = 4.
      ls_dropdown-value  = 'Set automanically'.  APPEND ls_dropdown TO lt_dropdown.
      ls_dropdown-value  = 'Set'.                APPEND ls_dropdown TO lt_dropdown.
      ls_dropdown-value  = 'Do not set'.         APPEND ls_dropdown TO lt_dropdown.

      lo_grid->set_drop_down_table(
        EXPORTING it_drop_down_alias = lt_dropdown ).

      cl_gui_control=>set_focus( control = lo_grid ).
      cl_gui_cfw=>flush( ).

    CATCH cx_root INTO DATA(lx_error).
      MESSAGE lx_error->get_text( ) TYPE 'S' DISPLAY LIKE 'E'.
  ENDTRY.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form alv_refresh
*&---------------------------------------------------------------------*
FORM alv_refresh USING pv_grid_nm TYPE fieldname.

  FIELD-SYMBOLS: <o_grid> TYPE REF TO cl_gui_alv_grid.
  ASSIGN (pv_grid_nm) TO <o_grid>.
*  CHECK <o_grid> IS ASSIGNED AND <o_grid> IS BOUND.
*
*  DATA(ls_stbl) = VALUE lvc_s_stbl( row = abap_true col = abap_true ).
*
*  TRY.
*      <o_grid>->set_ready_for_input(
*        i_ready_for_input = COND i( WHEN gs_edit = 'X' THEN 1 ELSE 0 ) ).
*
*      <o_grid>->refresh_table_display(
*        EXPORTING is_stable = ls_stbl ).
*
*      cl_gui_cfw=>flush( ).
*
*    CATCH cx_root INTO DATA(lx).
*      MESSAGE lx->get_text( ) TYPE 'S' DISPLAY LIKE 'E'.
*  ENDTRY.

  IF <o_grid> IS BOUND.
    DATA: ls_stable TYPE lvc_s_stbl.
    CALL METHOD go_grid_01->set_frontend_layout
      EXPORTING
        is_layout = gs_layout.

    ls_stable-row = abap_true.
    ls_stable-col = abap_true.

    CALL METHOD go_grid_01->refresh_table_display
      EXPORTING
        is_stable = ls_stable.     " With Stable Rows/Columns

    IF gs_layout-smalltitle = abap_on AND gv_grid_title IS NOT INITIAL.
      PERFORM alv_set_gridtitle USING 'GO_GRID_01'.
      go_grid_01->set_gridtitle( i_gridtitle = gv_grid_title ).
    ENDIF.

  ENDIF.

ENDFORM.
