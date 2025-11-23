*&---------------------------------------------------------------------*
*& Include          ZGISCRAPPINGS1_F03
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*& Form validation
*&---------------------------------------------------------------------*
FORM validation  CHANGING cv_validation_result.

  " Validate Header Fields
  PERFORM validate_header CHANGING cv_validation_result.
  " Validate Item Fields
  PERFORM validate_item CHANGING cv_validation_result.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form validate_Header Fields
*&---------------------------------------------------------------------*
FORM validate_header CHANGING cv_validation_result TYPE abap_bool.

  " Gi# ##nh h#p l# ban ##u
  cv_validation_result = abap_true.
  " Ki#m tra ngày ch#ng t#
  IF gs_header-doc_date IS INITIAL.
    MESSAGE ID 'ZGISCRAPPINGS_MSG' TYPE 'S' NUMBER '006' DISPLAY LIKE 'E'.
    gv_cursor_field_header = 'GS_HEADER-DOC_DATE'.
    cv_validation_result = abap_false.
    RETURN.
  ENDIF.
  " Ki#m tra ngày h#ch toán
  IF gs_header-posting_date IS INITIAL.
    MESSAGE ID 'ZGISCRAPPINGS_MSG' TYPE 'S' NUMBER '006' DISPLAY LIKE 'E'.
    gv_cursor_field_header = 'GS_HEADER-POSTING_DATE'.
    cv_validation_result = abap_false.
    RETURN.
  ENDIF.
  " Ki#m tra lo#i in (print_type)
  IF gs_header-print_type IS INITIAL .
    MESSAGE ID 'ZGISCRAPPINGS_MSG' TYPE 'S' NUMBER '015' DISPLAY LIKE 'E'.
    gv_cursor_field_header = 'GS_HEADER-PRINT_TYPE'.
    cv_validation_result = abap_false.
    RETURN.
  ENDIF.
ENDFORM.

*&---------------------------------------------------------------------*
*& Form validate_item
*&---------------------------------------------------------------------*
FORM validate_item CHANGING cv_validation_result TYPE abap_bool.

  DATA: lv_idx TYPE sy-tabix.

  CLEAR: gv_has_error_cell, gv_error_row_id, gs_col_id.
  cv_validation_result = abap_true.

  LOOP AT gt_item INTO gs_item.
    lv_idx = sy-tabix.

    " 1. Plant b#t bu#c
    IF gs_item-plant IS INITIAL.
      gv_error_message = 'Plant is required. Please enter a valid plant code.'.
      gs_col_id = 'PLANT'.
      PERFORM handle_cell_error.
      cv_validation_result = abap_false.
      EXIT.
    ENDIF.
    " 2. Storage location b#t bu#c
    IF gs_item-sloc IS INITIAL.
      gv_error_message = 'Storage location is required. Please enter a valid storage location.'.
      gs_col_id = 'SLOC'.
      PERFORM handle_cell_error.
      cv_validation_result = abap_false.
      EXIT.
    ENDIF.
    " 3. Material number b#t bu#c
    IF gs_item-material_number IS INITIAL.
      gv_error_message = 'Material number is required. Please enter a valid material number to continue.'.
      gs_col_id = 'MATERIAL_NUMBER'.
      PERFORM handle_cell_error.
      cv_validation_result = abap_false.
      EXIT.
    ENDIF.
    " 4. Quantity > 0
    IF gs_item-qty_in_une IS INITIAL OR gs_item-qty_in_une <= 0.
      gv_error_message = 'Quantity must be greater than zero. Please enter a valid quantity.'.
      gs_col_id = 'QTY_IN_UNE'.
      PERFORM handle_cell_error.
      cv_validation_result = abap_false.
      EXIT.
    ENDIF.
    " 5. ##n v# tính b#t bu#c
    IF gs_item-eun IS INITIAL.
      gv_error_message = 'Base unit of measure is required. Please enter a valid unit.'.
      gs_col_id = 'EUN'.
      PERFORM handle_cell_error.
      cv_validation_result = abap_false.
      EXIT.
    ENDIF.
    " 6. Stock type b#t bu#c
    IF gs_item-stock_type IS INITIAL.
      gv_error_message = 'Stock type is required. Please enter a valid stock type.'.
      gs_col_id = 'STOCK_TYPE'.
      PERFORM handle_cell_error.
      cv_validation_result = abap_false.
      EXIT.
    ENDIF.
    IF cv_validation_result = abap_true.
      PERFORM check_data_item_exists CHANGING cv_validation_result.
      IF cv_validation_result = abap_false.
        RETURN.
      ENDIF.
    ENDIF.

  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form check_data_item_exists
*&---------------------------------------------------------------------*
FORM check_data_item_exists CHANGING cv_validation_result TYPE abap_bool.
  DATA: lv_idx   TYPE sy-tabix.

  LOOP AT gt_item INTO gs_item.
    lv_idx = sy-tabix.
    " 1. Ki#m tra t#n t#i Material trong MARA
    SELECT SINGLE matnr
      FROM mara INTO @DATA(lv_matnr)
      WHERE matnr = @gs_item-material_number.
    IF sy-subrc <> 0.
      gv_error_message = 'Material does not exist. Please check the material number.'.
      gs_col_id-fieldname = 'MATERIAL_NUMBER'.
      PERFORM handle_cell_error.
      cv_validation_result = abap_false.
      EXIT.
    ENDIF.
    " 2. Ki#m tra t#n t#i Plant trong T001W
    SELECT SINGLE werks
  FROM t001w INTO @DATA(lv_plant)
  WHERE werks = @gs_item-plant.
    IF sy-subrc <> 0.
      gv_error_message = 'Plant does not exist. Please check the plant code.'.
      gs_col_id-fieldname = 'PLANT'.
      PERFORM handle_cell_error.
      cv_validation_result = abap_false.
      EXIT.
    ENDIF.

    " 3. Ki#m tra t#n t#i Storage Location trong T001L
    SELECT SINGLE lgort
      FROM t001l INTO @DATA(lv_sloc)
      WHERE werks = @gs_item-plant
        AND lgort = @gs_item-sloc.
    IF sy-subrc <> 0.
      gv_error_message = 'Storage Location does not exist in the specified Plant.'.
      gs_col_id-fieldname = 'SLOC'.
      PERFORM handle_cell_error.
      cv_validation_result = abap_false.
      EXIT.
    ENDIF.


  ENDLOOP.


ENDFORM.

*&---------------------------------------------------------------------*
*& Form handle_cell_error
*&---------------------------------------------------------------------*
FORM handle_cell_error.

  DATA: lv_dummy_row    TYPE i,
        lv_dummy_col    TYPE i,
        lv_dummy_value  TYPE c,
        ls_row_id       TYPE lvc_s_row,
        ls_col_id_dummy TYPE lvc_s_col,
        ls_row_no       TYPE lvc_s_roid.

  CALL METHOD go_grid_01->get_current_cell
    IMPORTING
      e_row     = lv_dummy_row
      e_col     = lv_dummy_col
      e_value   = lv_dummy_value
      es_row_id = ls_row_id
      es_col_id = ls_col_id_dummy
      es_row_no = ls_row_no.

  gv_error_row_id = ls_row_no.
  gv_has_error_cell = abap_true.

  MESSAGE gv_error_message TYPE 'S' DISPLAY LIKE 'E'.

ENDFORM.
