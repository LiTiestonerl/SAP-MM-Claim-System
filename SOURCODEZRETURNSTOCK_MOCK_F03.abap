*&---------------------------------------------------------------------*
*& Include          ZPG_MM_RETURN_STOCKF03
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*& Form validate_Header Fields
*&---------------------------------------------------------------------*
FORM validate_po_header CHANGING cv_validation_result TYPE abap_bool.
  DATA: lv_idx TYPE i.
  IF gs_po_edit = 'X'.
    IF gs_po_header-ebeln IS INITIAL.
      MESSAGE ID 'ZEXCHANGEMATERIAL' TYPE 'S' NUMBER '029' DISPLAY LIKE 'E'.
      gv_cursor_field_header = 'GS_PO_HEADER-EBELN'.
      cv_validation_result = abap_false.
      RETURN.
    ENDIF.
  ELSE.
    " Ki#m tra Vendor (lifnr)
    IF gs_po_header-lifnr IS INITIAL.
      MESSAGE ID 'ZEXCHANGEMATERIAL' TYPE 'S' NUMBER '002' DISPLAY LIKE 'E'.
      gv_cursor_field_header = 'GS_PO_HEADER-LIFNR'.
      cv_validation_result = abap_false.
      RETURN.
    ENDIF.

    " Ki#m tra Company Code (bukrs)
    IF gs_po_header-bukrs IS INITIAL.
      MESSAGE ID 'ZEXCHANGEMATERIAL' TYPE 'S' NUMBER '004' DISPLAY LIKE 'E'.
      gv_cursor_field_header = 'GS_PO_HEADER-BUKRS'.
      cv_validation_result = abap_false.
      RETURN.
    ENDIF.

    " Ki#m tra Purchasing Organization (ekorg)
    IF gs_po_header-ekorg IS INITIAL.
      MESSAGE ID 'ZEXCHANGEMATERIAL' TYPE 'S' NUMBER '020' DISPLAY LIKE 'E'.
      gv_cursor_field_header = 'GS_PO_HEADER-EKORG'.
      cv_validation_result = abap_false.
      RETURN.
    ENDIF.

    " Ki#m tra Purchasing Group (ekgrp)
    IF gs_po_header-ekgrp IS INITIAL.
      MESSAGE ID 'ZEXCHANGEMATERIAL' TYPE 'S' NUMBER '020' DISPLAY LIKE 'E'.
      gv_cursor_field_header = 'GS_PO_HEADER-EKGRP'.
      cv_validation_result = abap_false.
      RETURN.
    ENDIF.
    IF cv_validation_result = abap_true.
      PERFORM check_data_header_exists CHANGING cv_validation_result.
      IF cv_validation_result = abap_false.
        RETURN.
      ENDIF.
    ENDIF.

  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form check_master_data_header_exists
*&---------------------------------------------------------------------*
FORM check_data_header_exists CHANGING cv_validation_result TYPE abap_bool.

  " Check Company Code
  SELECT SINGLE bukrs FROM t001 INTO @DATA(lv_bukrs)
    WHERE bukrs = @gs_po_header-bukrs.
  IF sy-subrc <> 0.
    MESSAGE ID 'ZEXCHANGEMATERIAL' TYPE 'S' NUMBER '023' DISPLAY LIKE 'E'. " 'Company code does not exists!
    gv_cursor_field_header = 'GS_PO_HEADER-COMPANY_CODE'.
    cv_validation_result = abap_false.
    RETURN.
  ENDIF.

  " Check Supplier
  DATA(lv_lifnr1) = |{ gs_po_header-lifnr ALPHA = IN }|.
  SELECT SINGLE lifnr FROM lfa1 INTO @DATA(lv_lifnr)
    WHERE lifnr = @lv_lifnr1.
  IF sy-subrc <> 0.
    MESSAGE ID 'ZEXCHANGEMATERIAL' TYPE 'S' NUMBER '024' DISPLAY LIKE 'E'. " 'Company code does not exists!
    gv_cursor_field_header = 'GS_PO_HEADER-SUPPLIER'.
    cv_validation_result = abap_false.
    RETURN.
  ENDIF.

  " Check Purchasing Organization
  DATA(lv_ekorg1) = |{ gs_po_header-ekorg ALPHA = IN }|.
  SELECT SINGLE ekorg FROM t024e INTO @DATA(lv_ekorg)
    WHERE ekorg = @lv_ekorg1.
  IF sy-subrc <> 0.
    MESSAGE ID 'ZEXCHANGEMATERIAL' TYPE 'S' NUMBER '027' DISPLAY LIKE 'E'.
    gv_cursor_field_header = 'GS_PO_HEADER-EKORG'.    " ho#c 'GS_PO_HEADER-EKORG' tùy tên field
    cv_validation_result = abap_false.
    RETURN.
  ENDIF.

  " Check Purchasing Group
  DATA(lv_ekgrp1) = gs_po_header-ekgrp.
  SELECT SINGLE ekgrp FROM t024 INTO @DATA(lv_ekgrp)
    WHERE ekgrp = @lv_ekgrp1.
  IF sy-subrc <> 0.
    MESSAGE ID 'ZEXCHANGEMATERIAL' TYPE 'S' NUMBER '028' DISPLAY LIKE 'E'.
    gv_cursor_field_header = 'GS_PO_HEADER-EKGRP'.  " ho#c 'GS_PO_HEADER-EKGRP'
    cv_validation_result = abap_false.
    RETURN.
  ENDIF.

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
*&---------------------------------------------------------------------*
*& Form validate_receipt_header
*&---------------------------------------------------------------------*
*& VALIDATION RECEIPT FOR SCREEN 200
*&---------------------------------------------------------------------*
FORM validate_receipt_header CHANGING cv_validation_result TYPE abap_bool.
  IF gs_receipt_header-ebeln IS INITIAL .
    MESSAGE ID 'ZEXCHANGEMATERIAL' TYPE 'S' NUMBER '005' DISPLAY LIKE 'E'. " Invalid PO NUMBER
    gv_cursor_field_header  = 'GS_RECEIPT_HEADER-EBELN'.
    cv_validation_result = abap_false.
    RETURN.
  ENDIF.

  IF gs_receipt_header-bldat IS INITIAL OR
       gs_receipt_header-bldat > sy-datum.
    MESSAGE ID 'ZEXCHANGEMATERIAL' TYPE 'S' NUMBER '025' DISPLAY LIKE 'E'. " Invalid document date
    gv_cursor_field_header  = 'GS_RECEIPT_HEADER-BLDAT'.
    cv_validation_result = abap_false.
    RETURN.
  ENDIF.

  IF gs_receipt_header-budat IS INITIAL OR
     gs_receipt_header-budat > sy-datum.
    MESSAGE ID 'ZEXCHANGEMATERIAL' TYPE 'S' NUMBER '006' DISPLAY LIKE 'E'. " Invalid posting date
    gv_cursor_field_header  = 'GS_RECEIPT_HEADER-BUDAT'.
    cv_validation_result = abap_false.
    RETURN.
  ENDIF.

  IF cv_validation_result = abap_true.
    PERFORM check_receipt_header_exists CHANGING cv_validation_result.
    IF cv_validation_result = abap_false.
      RETURN.
    ENDIF.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form check_receipt_header_exists
*&---------------------------------------------------------------------*
FORM check_receipt_header_exists  CHANGING cv_validation_result TYPE abap_bool.
  "Check PO Number (Receipt Number)
  SELECT SINGLE ebeln
    FROM ekpo
    INTO @DATA(lv_po_number)
    WHERE ebeln = @gs_receipt_header-ebeln.

  IF sy-subrc <> 0.
    MESSAGE ID 'ZEXCHANGEMATERIAL' TYPE 'S' NUMBER '025' DISPLAY LIKE 'E'. " 'PO number does not exist!'
    gv_cursor_field_header = 'GS_RECEIPT_HEADER-EBELN'.
    cv_validation_result = abap_false.
    RETURN.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form validation_invoice
*&---------------------------------------------------------------------*
*& VALIDATION FOR SCREEN 300
*&---------------------------------------------------------------------*
FORM validation_invoice CHANGING cv_validation_result TYPE abap_bool.
  cv_validation_result = abap_true.  " Gi# ##nh validation thành công ban ##u
  PERFORM validate_invoice_header CHANGING cv_validation_result.
  IF cv_validation_result = abap_false.
    RETURN.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form validate_invoice_header
*&---------------------------------------------------------------------*
*& VALIDATION FOR SCREEN 300
*&---------------------------------------------------------------------*
FORM validate_invoice_header CHANGING cv_validation_result TYPE abap_bool.

  IF gs_invoice_header-bukrs IS INITIAL.
    gv_cursor_field_header  = 'GS_INVOICE_HEADER-BUKRS'.
    MESSAGE ID 'ZEXCHANGEMATERIAL' TYPE 'S' NUMBER '004' DISPLAY LIKE 'E'.
    cv_validation_result = abap_false.
    RETURN.
  ENDIF.

  IF gs_invoice_header-ebeln IS INITIAL.
    gv_cursor_field_header = 'GS_INVOICE_HEADER-EBELN'.
    MESSAGE ID 'ZEXCHANGEMATERIAL' TYPE 'S' NUMBER '005' DISPLAY LIKE 'E'.
    cv_validation_result = abap_false.
    RETURN.
  ENDIF.

  IF gs_invoice_header-bldat IS INITIAL OR gs_invoice_header-bldat > sy-datum.
    gv_cursor_field_header = 'GS_INVOICE_HEADER-BLDAT'.
    MESSAGE ID 'ZEXCHANGEMATERIAL' TYPE 'S' NUMBER '014' DISPLAY LIKE 'E'.
    cv_validation_result = abap_false.
    RETURN.
  ENDIF.

  IF gs_invoice_header-budat IS INITIAL OR gs_invoice_header-budat < gs_invoice_header-bldat.
    gv_cursor_field_header = 'GS_INVOICE_HEADER-BUDAT'.
    MESSAGE ID 'ZEXCHANGEMATERIAL' TYPE 'S' NUMBER '006' DISPLAY LIKE 'E'.
    cv_validation_result = abap_false.
    RETURN.
  ENDIF.

  IF gs_invoice_header-zamount IS INITIAL OR gs_invoice_header-zamount <= 0.
    gv_cursor_field_header = 'GS_INVOICE_HEADER-ZAMOUNT'.
    MESSAGE ID 'ZEXCHANGEMATERIAL' TYPE 'S' NUMBER '008' DISPLAY LIKE 'E'.
    cv_validation_result = abap_false.
    RETURN.
  ENDIF.

  IF gs_invoice_header-zcurrency IS INITIAL.
    gv_cursor_field_header = 'GS_INVOICE_HEADER-ZCURRENCY'.
    MESSAGE ID 'ZEXCHANGEMATERIAL' TYPE 'S' NUMBER '009' DISPLAY LIKE 'E'.
    cv_validation_result = abap_false.
    RETURN.
  ENDIF.
  IF gs_invoice_header-tax_code IS INITIAL.
    gv_cursor_field_header = 'GS_INVOICE_HEADER-TAX_CODE'.
    MESSAGE 'Please enter Tax Code' TYPE 'S' DISPLAY LIKE 'E'.
    cv_validation_result = abap_false.
    RETURN.
  ENDIF.
  IF cv_validation_result = abap_true.
    PERFORM check_invoice_header_exists CHANGING cv_validation_result.
    IF cv_validation_result = abap_false.
      RETURN.
    ENDIF.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form check_invoice_header_exists
*&---------------------------------------------------------------------*
*& VALIDATION FOR SCREEN 300
*&---------------------------------------------------------------------*
FORM check_invoice_header_exists CHANGING cv_validation_result TYPE abap_bool.

  " Check Company Code
  SELECT SINGLE bukrs
    FROM t001
    INTO @DATA(lv_bukrs)
    WHERE bukrs = @gs_invoice_header-bukrs.

  IF sy-subrc <> 0.
    MESSAGE ID 'ZEXCHANGEMATERIAL' TYPE 'S' NUMBER '023' DISPLAY LIKE 'E'. " Company Code does not exist!
    gv_cursor_field_header = 'GS_INVOICE_HEADER-BUKRS'.
    cv_validation_result = abap_false.
    RETURN.
  ENDIF.

  " 2) Check PO Number trong EKKO (b#ng chu#n) – l#c header ch#a b# xoá
  DATA: lv_ebeln TYPE ebeln,
        ls_ekko  TYPE ekko.

  lv_ebeln = gs_invoice_header-ebeln.
  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
    EXPORTING
      input  = lv_ebeln
    IMPORTING
      output = lv_ebeln.

  SELECT SINGLE ebeln, bukrs, loekz
    INTO CORRESPONDING FIELDS OF @ls_ekko
    FROM ekko
    WHERE ebeln = @lv_ebeln.

  IF sy-subrc <> 0 OR ls_ekko-ebeln IS INITIAL OR ls_ekko-loekz = 'L'.
    " Không tìm th#y PO trong chu#n, ho#c header #ã b# #ánh d#u xoá
    MESSAGE ID 'ZEXCHANGEMATERIAL' TYPE 'S' NUMBER '026' DISPLAY LIKE 'E'. " PO number does not exist!
    gv_cursor_field_header = 'GS_INVOICE_HEADER-EBELN'.
    cv_validation_result = abap_false.
    RETURN.
  ENDIF.

  " 3) (Khuy#n ngh#) Check PO thu#c #úng Company Code ng##i dùng nh#p
  IF gs_invoice_header-bukrs IS NOT INITIAL
     AND ls_ekko-bukrs IS NOT INITIAL
     AND ls_ekko-bukrs <> gs_invoice_header-bukrs.
    MESSAGE |PO { lv_ebeln } belongs to company code { ls_ekko-bukrs }, not { gs_invoice_header-bukrs }.|
            TYPE 'S' DISPLAY LIKE 'E'.
    gv_cursor_field_header = 'GS_INVOICE_HEADER-EBELN'.
    cv_validation_result = abap_false.
    RETURN.
  ENDIF.
  " 4) Check Tax Code t#n t#i
  IF gs_invoice_header-tax_code IS INITIAL.
    MESSAGE 'Please enter Tax Code' TYPE 'S' DISPLAY LIKE 'E'.
    gv_cursor_field_header = 'GS_INVOICE_HEADER-TAX_CODE'.
    cv_validation_result   = abap_false.
    RETURN.
  ENDIF.

  " Chu#n hoá Tax Code (ALPHA conversion không c#n vì là CHAR2/CHAR3)
  DATA(lv_tax) = gs_invoice_header-tax_code.

  SELECT SINGLE mwskz
    FROM t007a
    INTO @DATA(lv_mwskz)
    WHERE mwskz = @lv_tax.

  IF sy-subrc <> 0.
    MESSAGE  |Tax Code { lv_tax } does not exist.| TYPE 'S' DISPLAY LIKE 'E'.
    gv_cursor_field_header = 'GS_INVOICE_HEADER-TAX_CODE'.
    cv_validation_result   = abap_false.
    RETURN.
  ENDIF.
ENDFORM.

FORM validate_invoice_items CHANGING cv_validation_result TYPE abap_bool.

  LOOP AT gt_invoice_item INTO gs_invoice_item.
    IF gs_invoice_item-material IS INITIAL.
      gv_cursor_field_header = 'GS_INVOICE_ITEM-MATERIAL'.
      MESSAGE 'Material is required' TYPE 'S' DISPLAY LIKE 'E'.
      cv_validation_result = abap_false.
      EXIT.
    ENDIF.

    IF gs_invoice_item-amount IS INITIAL OR gs_invoice_item-amount <= 0.
      gv_cursor_field_header = 'GS_INVOICE_ITEM-AMOUNT'.
      MESSAGE 'Amount is required and must be positive' TYPE 'S' DISPLAY LIKE 'E'.
      cv_validation_result = abap_false.
      EXIT.
    ENDIF.

    IF gs_invoice_item-quantity IS INITIAL OR gs_invoice_item-quantity <= 0.
      gv_cursor_field_header = 'GS_INVOICE_ITEM-QUANTITY'.
      MESSAGE 'Quantity is required and must be positive' TYPE 'S' DISPLAY LIKE 'E'.
      cv_validation_result = abap_false.
      EXIT.
    ENDIF.

    IF gs_invoice_item-short_text IS INITIAL.
      gv_cursor_field_header = 'GS_INVOICE_ITEM-PO_TEXT'.
      MESSAGE 'PO short text is required' TYPE 'S' DISPLAY LIKE 'E'.
      cv_validation_result = abap_false.
      EXIT.
    ENDIF.

*    IF gs_invoice_item-tax_code IS INITIAL.
*      gv_cursor_field_header = 'GS_INVOICE_ITEM-TAX_CODE'.
*      MESSAGE 'Tax code is required' TYPE 'S' DISPLAY LIKE 'E'.
*      cv_validation_result = abap_false.
*      EXIT.
*    ENDIF.
  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form validation_payment
*&---------------------------------------------------------------------*
*& VALIDATION FOR SCREEN 400
*&---------------------------------------------------------------------*
FORM validation_payment CHANGING cv_validation_result TYPE abap_bool.
  cv_validation_result = abap_true.  " Gi# ##nh validation thành công ban ##u
  IF gs_payment_header-lifnr IS INITIAL.
    MESSAGE ID 'ZEXCHANGEMATERIAL' TYPE 'S' NUMBER '002' DISPLAY LIKE 'E'.  " Vendor and material are required fields
    gv_cursor_field_header = 'GS_PAYMENT_HEADER-LIFNR'.
    cv_validation_result = abap_false.
  ENDIF.
  IF gs_payment_header-bukrs IS INITIAL.
    MESSAGE ID 'ZEXCHANGEMATERIAL' TYPE 'S' NUMBER '004' DISPLAY LIKE 'E'.  " Company Code is missing
    gv_cursor_field_header = 'GS_PAYMENT_HEADER-BUKRS'.
    cv_validation_result = abap_false.
  ENDIF.

  IF cv_validation_result = abap_true.
    PERFORM check_payment_exists CHANGING cv_validation_result.
    IF cv_validation_result = abap_false.
      RETURN.
    ENDIF.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form check_payment_exists (Screen 300)
*&---------------------------------------------------------------------*
FORM check_payment_exists CHANGING cv_exists TYPE abap_bool.

  DATA: lv_bukrs TYPE bukrs,
        lv_lifnr TYPE lifnr,
        lv_dummy TYPE c.

  cv_exists = abap_false.

  lv_bukrs = gs_payment_header-bukrs.
  lv_lifnr = gs_payment_header-lifnr.

  SELECT SINGLE bukrs
    INTO @lv_dummy
    FROM t001
    WHERE bukrs = @lv_bukrs.
  IF sy-subrc <> 0.
    MESSAGE |Company Code { lv_bukrs } không t#n t#i.| TYPE 'S' DISPLAY LIKE 'E'.
    RETURN.
  ENDIF.

  DATA(lv_lifnr1) = |{ lv_lifnr ALPHA = IN }|.
  SELECT SINGLE lifnr
    INTO @lv_dummy
    FROM lfa1
    WHERE lifnr = @lv_lifnr1.
  IF sy-subrc <> 0.
    MESSAGE |Vendor { lv_lifnr } không t#n t#i.| TYPE 'S' DISPLAY LIKE 'E'.
    RETURN.
  ENDIF.

  cv_exists = abap_true.

ENDFORM.
*&---------------------------------------------------------------------*
*& VALIDATE_CREPO_100
*&---------------------------------------------------------------------*
FORM validate_crepo_100
  USING er_data_changed TYPE REF TO cl_alv_changed_data_protocol
        lv_validation_result TYPE abap_bool.

  TYPES: BEGIN OF ty_done,
           row_id TYPE i,
         END OF ty_done.

  DATA: lt_done TYPE SORTED TABLE OF ty_done WITH UNIQUE KEY row_id,
        ls_done TYPE ty_done,
        lv_msg  TYPE string.

  FIELD-SYMBOLS: <ls_row> TYPE ty_po_item.
*    CALL FUNCTION 'SAPGUI_SET_FUNCTIONCODE'
*     EXPORTING
*       FUNCTIONCODE                 = 'ENTER'
*     EXCEPTIONS
*       FUNCTION_NOT_SUPPORTED       = 1
*       OTHERS                       = 2
*              .
*    IF sy-subrc <> 0.
** Implement suitable error handling here
*    ENDIF.

  LOOP AT er_data_changed->mt_good_cells INTO DATA(ls_good).

    " Ch#ng x# lý trùng dòng
    ls_done-row_id = ls_good-row_id.
    READ TABLE lt_done WITH KEY row_id = ls_done-row_id TRANSPORTING NO FIELDS.
    IF sy-subrc = 0.
      CONTINUE.
    ENDIF.
    INSERT ls_done INTO TABLE lt_done.

    " L#y dòng #ang s#a
    READ TABLE gt_po_item INDEX ls_good-row_id ASSIGNING <ls_row>.
    IF sy-subrc <> 0 OR <ls_row> IS NOT ASSIGNED.
      CONTINUE.
    ENDIF.

    " ====== Validate Required Field ======

    IF <ls_row>-matnr IS INITIAL.
      lv_msg = |Material is required (line { ls_good-row_id })|.
      lv_validation_result = abap_false.
      PERFORM add_err_text USING er_data_changed 'MATNR' ls_good-row_id lv_msg 'E'.
      CONTINUE.
    ENDIF.

    IF <ls_row>-bstmg IS INITIAL OR <ls_row>-bstmg <= 0.
      lv_msg = |PO Quantity must be > 0 (line { ls_good-row_id })|.
      lv_validation_result = abap_false.
      PERFORM add_err_text USING er_data_changed 'BSTMG' ls_good-row_id lv_msg 'E'.
      CONTINUE.
    ENDIF.

*    IF <ls_row>-zunit IS INITIAL.
*      lv_msg = |Base Unit (UoM) is required (line { ls_good-row_id })|.
*      lv_validation_result = abap_false.
*      PERFORM add_err_text USING er_data_changed 'ZUNIT' ls_good-row_id lv_msg 'E'.
*      CONTINUE.
*    ENDIF.

    IF <ls_row>-eindt IS INITIAL.
      lv_msg = |Delivery Date is required (line { ls_good-row_id })|.
      lv_validation_result = abap_false.
      PERFORM add_err_text USING er_data_changed 'EINDT' ls_good-row_id lv_msg 'E'.
      CONTINUE.
    ENDIF.

*    IF <ls_row>-bprei IS INITIAL OR <ls_row>-bprei <= 0.
*      lv_msg = |Net Price must be > 0 (line { ls_good-row_id })|.
*      lv_validation_result = abap_false.
*      PERFORM add_err_text USING er_data_changed 'BPREI' ls_good-row_id lv_msg 'E'.
*      CONTINUE.
*    ENDIF.

*    IF <ls_row>-zcurrency IS INITIAL.
*      lv_msg = |Currency is required (line { ls_good-row_id })|.
*      lv_validation_result = abap_false.
*      PERFORM add_err_text USING er_data_changed 'ZCURRENCY' ls_good-row_id lv_msg 'E'.
*      CONTINUE.
*    ENDIF.

*    IF <ls_row>-peinh IS INITIAL OR <ls_row>-peinh <= 0.
*      lv_msg = |Price unit must be > 0 (line { ls_good-row_id })|.
*      lv_validation_result = abap_false.
*      PERFORM add_err_text USING er_data_changed 'PEINH' ls_good-row_id lv_msg 'E'.
*      CONTINUE.
*    ENDIF.

    IF <ls_row>-matkl IS INITIAL.
      lv_msg = |Material Group is required (line { ls_good-row_id })|.
      lv_validation_result = abap_false.
      PERFORM add_err_text USING er_data_changed 'MATKL' ls_good-row_id lv_msg 'E'.
      CONTINUE.
    ENDIF.

    IF <ls_row>-zplant IS INITIAL.
      lv_msg = |Plant is required (line { ls_good-row_id })|.
      lv_validation_result = abap_false.
      PERFORM add_err_text USING er_data_changed 'ZPLANT' ls_good-row_id lv_msg 'E'.
      CONTINUE.
    ENDIF.

    IF <ls_row>-zplant IS INITIAL.
      lv_msg = |Storage Location is required (line { ls_good-row_id })|.
      lv_validation_result = abap_false.
      PERFORM add_err_text USING er_data_changed 'ZSTR_LOCA' ls_good-row_id lv_msg 'E'.
      CONTINUE.
    ENDIF.

    " ====== Validate Master Data ======
    DATA: lv_matnr TYPE matnr,
          lv_uom   TYPE msehi,
          lv_curr  TYPE waers.

    " 1) Material
    lv_matnr = <ls_row>-matnr.
    SELECT SINGLE matnr FROM mara INTO @DATA(dummy_mat)
      WHERE matnr = @lv_matnr.
    IF sy-subrc <> 0.
      lv_msg = |Material { lv_matnr } does not exist (line { ls_good-row_id })|.
      lv_validation_result = abap_false.
      PERFORM add_err_text USING er_data_changed 'MATNR' ls_good-row_id lv_msg 'E'.
      CONTINUE.
    ENDIF.

*    " 2) UoM
*    lv_uom = <ls_row>-zunit.
*    CALL FUNCTION 'CONVERSION_EXIT_CUNIT_INPUT'
*      EXPORTING
*        input          = lv_uom
*      IMPORTING
*        output         = lv_uom
*      EXCEPTIONS
*        unit_not_found = 1
*        OTHERS         = 2.
*    IF sy-subrc <> 0.
*      lv_msg = |UoM { <ls_row>-zunit } is invalid (line { ls_good-row_id })|.
*      lv_validation_result = abap_false.
*      PERFORM add_err_text USING er_data_changed 'ZUNIT' ls_good-row_id lv_msg 'E'.
*      CONTINUE.
*    ENDIF.
*
*    SELECT SINGLE msehi FROM t006 INTO @DATA(dummy_uom)
*      WHERE msehi = @lv_uom.
*    IF sy-subrc <> 0.
*      lv_msg = |UoM { <ls_row>-zunit } is invalid (line { ls_good-row_id })|.
*      lv_validation_result = abap_false.
*      PERFORM add_err_text USING er_data_changed 'ZUNIT' ls_good-row_id lv_msg 'E'.
*      CONTINUE.
*    ENDIF.

*    " 3) Currency
*    lv_curr = <ls_row>-zcurrency.
*    SELECT SINGLE waers FROM tcurc INTO @DATA(dummy_curr)
*      WHERE waers = @lv_curr.
*    IF sy-subrc <> 0.
*      lv_msg = |Currency { lv_curr } is invalid (line { ls_good-row_id })|.
*      lv_validation_result = abap_false.
*      PERFORM add_err_text USING er_data_changed 'ZCURRENCY' ls_good-row_id lv_msg 'E'.
*      CONTINUE.
*    ENDIF.

    " 4) Material Group
    SELECT SINGLE matkl FROM t023 INTO @DATA(dummy_matkl)
      WHERE matkl = @<ls_row>-matkl.
    IF sy-subrc <> 0.
      lv_msg = |Material Group { <ls_row>-matkl } is invalid (line { ls_good-row_id })|.
      lv_validation_result = abap_false.
      PERFORM add_err_text USING er_data_changed 'MATKL' ls_good-row_id lv_msg 'E'.
      CONTINUE.
    ENDIF.

    " 5) Plant
    SELECT SINGLE werks FROM t001w INTO @DATA(dummy_plant)
      WHERE werks = @<ls_row>-zplant.
    IF sy-subrc <> 0.
      lv_msg = |Plant { <ls_row>-zplant } is invalid (line { ls_good-row_id })|.
      lv_validation_result = abap_false.
      PERFORM add_err_text USING er_data_changed 'ZPLANT' ls_good-row_id lv_msg 'E'.
      CONTINUE.
    ENDIF.

  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*& VALIDATE_POSTGR_200
*&---------------------------------------------------------------------*
FORM validate_postgr_200
  USING er_data_changed TYPE REF TO cl_alv_changed_data_protocol
        lv_validation_result TYPE abap_bool.

  TYPES: BEGIN OF ty_done,
           row_id TYPE i,
         END OF ty_done.

  DATA: lt_done TYPE SORTED TABLE OF ty_done WITH UNIQUE KEY row_id,
        ls_done TYPE ty_done,
        lv_msg  TYPE string,
        lv_ok   TYPE abap_bool.

  FIELD-SYMBOLS: <r>   TYPE any,
                 <cmp> TYPE any.

  DATA: get_fc_field TYPE lvc_fname,
        lv_fc_uom    TYPE lvc_fname.

  DEFINE _resolve_fc.
    get_fc_field = &1.
    READ TABLE gt_gr_fieldcat WITH KEY fieldname = get_fc_field TRANSPORTING NO FIELDS.
    IF sy-subrc <> 0.
      CLEAR get_fc_field.
    ENDIF.
  END-OF-DEFINITION.

  LOOP AT er_data_changed->mt_good_cells INTO DATA(ls_good).

    CLEAR: get_fc_field, lv_fc_uom.

    ls_done-row_id = ls_good-row_id.
    READ TABLE lt_done WITH KEY row_id = ls_done-row_id TRANSPORTING NO FIELDS.
    IF sy-subrc = 0.
      CONTINUE.
    ENDIF.
    INSERT ls_done INTO TABLE lt_done.

    READ TABLE gt_receipt_item INDEX ls_good-row_id ASSIGNING <r>.
    IF sy-subrc <> 0 OR <r> IS NOT ASSIGNED.
      CONTINUE.
    ENDIF.

    ASSIGN COMPONENT 'MATERIAL' OF STRUCTURE <r> TO <cmp>.
    _resolve_fc 'MATERIAL'.
    IF <cmp> IS ASSIGNED AND <cmp> IS INITIAL.
      lv_msg = |Material is required (line { ls_good-row_id })|.
      lv_validation_result = abap_false.
      PERFORM add_err_text USING er_data_changed get_fc_field ls_good-row_id lv_msg 'E'.
      CONTINUE.
    ENDIF.

    ASSIGN COMPONENT 'QUANTITY' OF STRUCTURE <r> TO <cmp>.
    _resolve_fc 'QUANTITY'.
    IF <cmp> IS ASSIGNED AND ( <cmp> IS INITIAL OR <cmp> <= 0 ).
      lv_msg = |Quantity must be > 0 (line { ls_good-row_id })|.
      lv_validation_result = abap_false.
      PERFORM add_err_text USING er_data_changed get_fc_field ls_good-row_id lv_msg 'E'.
      CONTINUE.
    ENDIF.

    ASSIGN COMPONENT 'UOM' OF STRUCTURE <r> TO <cmp>.
    IF <cmp> IS ASSIGNED.
      lv_fc_uom = 'UOM'.
    ELSE.
      ASSIGN COMPONENT 'MEINS' OF STRUCTURE <r> TO <cmp>.
      IF <cmp> IS ASSIGNED.
        lv_fc_uom = 'MEINS'.
      ENDIF.
    ENDIF.

    IF lv_fc_uom IS NOT INITIAL.
      _resolve_fc lv_fc_uom.
    ENDIF.

    IF <cmp> IS ASSIGNED AND <cmp> IS INITIAL.
      lv_msg = |UoM is required (line { ls_good-row_id })|.
      lv_validation_result = abap_false.
      PERFORM add_err_text USING er_data_changed lv_fc_uom ls_good-row_id lv_msg 'E'.
      CONTINUE.
    ENDIF.

    DATA: lv_ebeln TYPE ebeln,
          lv_ebelp TYPE ebelp,
          lv_matnr TYPE matnr,
          lv_werks TYPE werks_d,
          lv_bwart TYPE bwart,
          lv_lifnr TYPE lifnr.

    ASSIGN COMPONENT 'PURCHASE_ORDER' OF STRUCTURE <r> TO <cmp>.
    IF <cmp> IS ASSIGNED.
      lv_ebeln = |{ <cmp> ALPHA = IN }|.
    ENDIF.

    ASSIGN COMPONENT 'ITEM' OF STRUCTURE <r> TO <cmp>.
    IF <cmp> IS ASSIGNED.
      lv_ebelp = |{ <cmp> ALPHA = IN }|.
    ENDIF.

    ASSIGN COMPONENT 'MATERIAL' OF STRUCTURE <r> TO <cmp>.
    IF <cmp> IS ASSIGNED.
      lv_matnr = <cmp>.
    ENDIF.

    ASSIGN COMPONENT 'PLANT' OF STRUCTURE <r> TO <cmp>.
    IF <cmp> IS ASSIGNED.
      lv_werks = <cmp>.
    ENDIF.

    ASSIGN COMPONENT 'MOVEMENT_TYPE' OF STRUCTURE <r> TO <cmp>.
    IF <cmp> IS ASSIGNED.
      lv_bwart = <cmp>.
    ENDIF.

    ASSIGN COMPONENT 'VENDOR' OF STRUCTURE <r> TO <cmp>.
    IF <cmp> IS ASSIGNED.
      lv_lifnr = <cmp>.
    ENDIF.

    IF lv_ebeln IS NOT INITIAL.
      SELECT SINGLE ebeln FROM ekko INTO @DATA(dummy_po)
        WHERE ebeln = @lv_ebeln.
      IF sy-subrc <> 0.
        lv_msg = |PO { lv_ebeln } not found (line { ls_good-row_id })|.
        lv_validation_result = abap_false.
        PERFORM add_err_text USING er_data_changed 'PURCHASE_ORDER' ls_good-row_id lv_msg 'E'.
        CONTINUE.
      ENDIF.
    ENDIF.

    IF lv_matnr IS NOT INITIAL.
      SELECT SINGLE matnr FROM mara INTO @DATA(dummy_mat)
        WHERE matnr = @lv_matnr.
      IF sy-subrc <> 0.
        lv_msg = |Material { lv_matnr } not found (line { ls_good-row_id })|.
        lv_validation_result = abap_false.
        PERFORM add_err_text USING er_data_changed 'MATERIAL' ls_good-row_id lv_msg 'E'.
        CONTINUE.
      ENDIF.
    ENDIF.

    IF lv_werks IS NOT INITIAL.
      SELECT SINGLE werks FROM t001w INTO @DATA(dummy_plant)
        WHERE werks = @lv_werks.
      IF sy-subrc <> 0.
        lv_msg = |Plant { lv_werks } not found (line { ls_good-row_id })|.
        lv_validation_result = abap_false.
        PERFORM add_err_text USING er_data_changed 'PLANT' ls_good-row_id lv_msg 'E'.
        CONTINUE.
      ENDIF.
    ENDIF.

    IF lv_lifnr IS NOT INITIAL.
      SELECT SINGLE lifnr FROM lfa1 INTO @DATA(dummy_vendor)
        WHERE lifnr = @lv_lifnr.
      IF sy-subrc <> 0.
        lv_msg = |Vendor { lv_lifnr } not found (line { ls_good-row_id })|.
        lv_validation_result = abap_false.
        PERFORM add_err_text USING er_data_changed 'VENDOR' ls_good-row_id lv_msg 'E'.
        CONTINUE.
      ENDIF.
    ENDIF.

    IF lv_bwart IS NOT INITIAL.
      SELECT SINGLE bwart FROM t156 INTO @DATA(dummy_mt)
        WHERE bwart = @lv_bwart.
      IF sy-subrc <> 0.
        lv_msg = |Movement type { lv_bwart } invalid (line { ls_good-row_id })|.
        lv_validation_result = abap_false.
        PERFORM add_err_text USING er_data_changed 'MOVEMENT_TYPE' ls_good-row_id lv_msg 'E'.
        CONTINUE.
      ENDIF.
    ENDIF.

  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*& VALIDATE_POSTIV_300
*&---------------------------------------------------------------------*
FORM validate_postiv_300
  USING er_data_changed TYPE REF TO cl_alv_changed_data_protocol
        lv_validation_result.

  TYPES: BEGIN OF ty_done,
           row_id TYPE i,
         END OF ty_done.

  DATA: lt_done TYPE SORTED TABLE OF ty_done WITH UNIQUE KEY row_id,
        ls_done TYPE ty_done,
        lv_msg  TYPE string.

  FIELD-SYMBOLS: <ls_row> TYPE ty_invoice_item.  " ##i type n#u b#n khác

  LOOP AT er_data_changed->mt_good_cells INTO DATA(ls_good).

    " Ch#ng validate trùng row
    ls_done-row_id = ls_good-row_id.
    READ TABLE lt_done WITH KEY row_id = ls_done-row_id TRANSPORTING NO FIELDS.
    IF sy-subrc = 0.
      CONTINUE.
    ENDIF.
    INSERT ls_done INTO TABLE lt_done.

    " L#y dòng #ang s#a
    READ TABLE gt_invoice_item INDEX ls_good-row_id ASSIGNING <ls_row>.
    IF sy-subrc <> 0 OR <ls_row> IS NOT ASSIGNED.
      CONTINUE.
    ENDIF.

    "================= BASIC RULES =================

    " 1) MATERIAL required
    IF <ls_row>-material IS INITIAL.
      lv_msg = |Material is required (line { ls_good-row_id })|.
      lv_validation_result = abap_false.
      PERFORM add_err_text USING er_data_changed 'MATERIAL' ls_good-row_id lv_msg 'E'.
      CONTINUE.
    ENDIF.

    " 2) At least one of AMOUNT / QUANTITY required
    IF ( <ls_row>-amount   IS INITIAL OR <ls_row>-amount   = 0 )
    AND ( <ls_row>-quantity IS INITIAL OR <ls_row>-quantity = 0 ).
      lv_msg = |Enter Amount or Quantity (line { ls_good-row_id })|.
      lv_validation_result = abap_false.
      PERFORM add_err_text USING er_data_changed 'AMOUNT'  ls_good-row_id lv_msg 'E'.
      PERFORM add_err_text USING er_data_changed 'QUANTITY' ls_good-row_id lv_msg 'E'.
      CONTINUE.
    ENDIF.

    " 3) QUANTITY > 0 (n#u nh#p) & UOM required khi có QUANTITY
    IF <ls_row>-quantity IS NOT INITIAL.
      IF <ls_row>-quantity <= 0.
        lv_msg = |Quantity must be > 0 (line { ls_good-row_id })|.
        lv_validation_result = abap_false.
        PERFORM add_err_text USING er_data_changed 'QUANTITY' ls_good-row_id lv_msg 'E'.
        CONTINUE.
      ENDIF.

      IF <ls_row>-uom IS INITIAL.
        lv_msg = |UoM is required when Quantity provided (line { ls_good-row_id })|.
        lv_validation_result = abap_false.
        PERFORM add_err_text USING er_data_changed 'UOM' ls_good-row_id lv_msg 'E'.
        CONTINUE.
      ENDIF.
    ENDIF.

    " 4) AMOUNT <> 0 (n#u nh#p)
    "    N#u mu#n b#t > 0 cho Invoice và < 0 cho Credit Memo thì thay #i#u ki#n t#i #ây.
    IF <ls_row>-amount IS NOT INITIAL AND <ls_row>-amount = 0.
      lv_msg = |Amount must be non-zero (line { ls_good-row_id })|.
      lv_validation_result = abap_false.
      PERFORM add_err_text USING er_data_changed 'AMOUNT' ls_good-row_id lv_msg 'E'.
      CONTINUE.
    ENDIF.

    "================= MASTER DATA CHECKS =================

    " 5) MATERIAL in MARA
    SELECT SINGLE matnr FROM mara INTO @DATA(dummy_mat)
      WHERE matnr = @<ls_row>-material.
    IF sy-subrc <> 0.
      lv_msg = |Material { <ls_row>-material } not found (line { ls_good-row_id })|.
      lv_validation_result = abap_false.
      PERFORM add_err_text USING er_data_changed 'MATERIAL' ls_good-row_id lv_msg 'E'.
      CONTINUE.
    ENDIF.

    " 6) UOM in T006 (n#u có UOM)
    IF <ls_row>-uom IS NOT INITIAL.
      DATA(lv_uom) = <ls_row>-uom.
      CALL FUNCTION 'CONVERSION_EXIT_CUNIT_INPUT'
        EXPORTING input = lv_uom
        IMPORTING output = lv_uom
        EXCEPTIONS unit_not_found = 1 OTHERS = 2.
      IF sy-subrc <> 0.
        lv_msg = |UoM { <ls_row>-uom } is invalid (line { ls_good-row_id })|.
        lv_validation_result = abap_false.
        PERFORM add_err_text USING er_data_changed 'UOM' ls_good-row_id lv_msg 'E'.
        CONTINUE.
      ENDIF.

      SELECT SINGLE msehi FROM t006 INTO @DATA(dummy_uom)
        WHERE msehi = @lv_uom.
      IF sy-subrc <> 0.
        lv_msg = |UoM { <ls_row>-uom } is invalid (line { ls_good-row_id })|.
        lv_validation_result = abap_false.
        PERFORM add_err_text USING er_data_changed 'UOM' ls_good-row_id lv_msg 'E'.
        CONTINUE.
      ENDIF.
    ENDIF.

    " 7) TAX_CODE in T007A (n#u có)
    IF <ls_row>-tax_code IS NOT INITIAL.
      SELECT SINGLE mwskz FROM t007a INTO @DATA(dummy_tax)
        WHERE mwskz = @<ls_row>-tax_code.
      IF sy-subrc <> 0.
        lv_msg = |Tax Code { <ls_row>-tax_code } is invalid (line { ls_good-row_id })|.
        lv_validation_result = abap_false.
        PERFORM add_err_text USING er_data_changed 'TAX_CODE' ls_good-row_id lv_msg 'E'.
        CONTINUE.
      ENDIF.
    ENDIF.
  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*&  Common helper: add free-text error to protocol
*&---------------------------------------------------------------------*
FORM add_err_text
  USING    er_data_changed TYPE REF TO cl_alv_changed_data_protocol
           pv_field        TYPE lvc_fname
           pv_rowid        TYPE i
           pv_text         TYPE string
           pv_msgty        TYPE symsgty.

  DATA: lv1 TYPE string,
        lv2 TYPE string,
        lv3 TYPE string,
        lv4 TYPE string.

  DATA: lv_len  TYPE i,
        lv_off  TYPE i,
        lv_take TYPE i.

  CLEAR: lv1, lv2, lv3, lv4.

  lv_len = strlen( pv_text ).

  DO 4 TIMES.
    lv_off = ( sy-index - 1 ) * 50.
    IF lv_off >= lv_len.
      EXIT.
    ENDIF.

    lv_take = lv_len - lv_off.
    IF lv_take > 50.
      lv_take = 50.
    ENDIF.

    CASE sy-index.
      WHEN 1. lv1 = pv_text+lv_off(lv_take).
      WHEN 2. lv2 = pv_text+lv_off(lv_take).
      WHEN 3. lv3 = pv_text+lv_off(lv_take).
      WHEN 4. lv4 = pv_text+lv_off(lv_take).
    ENDCASE.
  ENDDO.

  er_data_changed->add_protocol_entry(
    EXPORTING
      i_msgid     = '00'
      i_msgno     = '398'
      i_msgty     = pv_msgty
      i_fieldname = pv_field
      i_row_id    = pv_rowid
      i_msgv1     = lv1
      i_msgv2     = lv2
      i_msgv3     = lv3
      i_msgv4     = lv4 ).

ENDFORM.
