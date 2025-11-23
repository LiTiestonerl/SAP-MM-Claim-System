*&---------------------------------------------------------------------*
*& Include          ZGISCRAPPINGS1_F02
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*& Form setup_listbox
*&---------------------------------------------------------------------*
FORM setup_listbox .
  DATA: lt_values TYPE vrm_values,
        ls_value  TYPE vrm_value.

  IF gs_header-print_type IS INITIAL.
    gs_header-print_type = 'IND'.
  ENDIF.

  CLEAR lt_values.
  ls_value-key  = 'IND'.  ls_value-text = 'Individual Slip'.                         APPEND ls_value TO lt_values.
  ls_value-key  = 'INSP'. ls_value-text = 'Individual Slip with Inspection Text'.    APPEND ls_value TO lt_values.
  ls_value-key  = 'COL'.  ls_value-text = 'Collective Slip'.                         APPEND ls_value TO lt_values.

  CALL FUNCTION 'VRM_SET_VALUES'
    EXPORTING
      id              = 'GS_HEADER-PRINT_TYPE'
      values          = lt_values
    EXCEPTIONS
      id_illegal_name = 1
      OTHERS          = 2.
  IF sy-subrc <> 0.
    MESSAGE 'Error setting listbox' TYPE 'S' DISPLAY LIKE 'E'.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form get_material_name
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
FORM get_material_name .
  TYPES: BEGIN OF ty_makt_map,
           matnr TYPE matnr,
           maktx TYPE maktx,
         END OF ty_makt_map.

  DATA: lt_matnr  TYPE SORTED TABLE OF matnr WITH UNIQUE KEY table_line,
        lt_makt   TYPE HASHED TABLE OF ty_makt_map WITH UNIQUE KEY matnr,
        ls_makt   TYPE ty_makt_map.

  FIELD-SYMBOLS: <fs_item> TYPE LINE OF ty_t_item.

  LOOP AT gt_item ASSIGNING <fs_item>.
    DATA(lv_matnr) = CONV matnr( <fs_item>-material_number ).
    IF lv_matnr IS NOT INITIAL.
      "lv_matnr = |{ lv_matnr ALPHA = IN }|.
      INSERT lv_matnr INTO TABLE lt_matnr.
    ENDIF.
  ENDLOOP.

  IF lt_matnr IS INITIAL.
    RETURN.
  ENDIF.

  SELECT matnr, maktx
    FROM makt
    INTO TABLE @lt_makt
    FOR ALL ENTRIES IN @lt_matnr
    WHERE matnr = @lt_matnr-table_line
      AND spras = @sy-langu.

  LOOP AT gt_item ASSIGNING <fs_item>.
    DATA(lv_matnr_item) = <fs_item>-material_number.
    READ TABLE lt_makt INTO ls_makt WITH KEY matnr = lv_matnr_item.
    IF sy-subrc = 0.
      <fs_item>-material_short_text = ls_makt-maktx.
      "<fs_item>-qty_in_une = <fs_item>-qty_in_une * 1000.
    ENDIF.
  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form get_date
*&---------------------------------------------------------------------*
FORM get_date .
  DATA: lv_today TYPE sy-datum.

  " L#y ngày hi#n t#i t# h# th#ng
  lv_today = sy-datum.

  " Gán vào field t##ng #ng trong header
  gs_header-doc_date     = lv_today.
  gs_header-posting_date = lv_today.
ENDFORM.
