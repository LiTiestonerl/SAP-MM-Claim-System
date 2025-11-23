*&---------------------------------------------------------------------*
*& Include          ZPG_MM_RETURN_STOCKF02
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*& Form get_item_from_db
*&---------------------------------------------------------------------*
FORM get_item_from_db .

  DATA lv_ebeln TYPE ebeln.

  CLEAR gt_po_item.

  " 1) Check & chu#n hoá EBELN
  IF gs_po_header-ebeln IS INITIAL.
    MESSAGE 'Please enter Purchase Order Number.' TYPE 'S' DISPLAY LIKE 'E'.
    RETURN.
  ENDIF.

  lv_ebeln = gs_po_header-ebeln.
  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
    EXPORTING
      input  = lv_ebeln
    IMPORTING
      output = lv_ebeln.
  gs_po_header-ebeln = lv_ebeln.

  " 2) Header t# EKKO
  SELECT SINGLE lifnr, bukrs, ekorg, ekgrp
         INTO (@gs_po_header-lifnr,
               @gs_po_header-bukrs,
               @gs_po_header-ekorg,
               @gs_po_header-ekgrp)
    FROM ekko
    WHERE ebeln = @lv_ebeln.

  IF sy-subrc <> 0.
    MESSAGE |PO { gs_po_header-ebeln } not found.| TYPE 'S' DISPLAY LIKE 'E'.
    RETURN.
  ENDIF.

  " 3) Items t# EKPO (+ earliest EKET-EINDT) -> gt_po_item
  SELECT
      p~ebelp                  AS po_item,
      p~matnr                  AS matnr,
      p~txz01                  AS zmate_des,
      p~menge                  AS bstmg,
      p~meins                  AS zunit,
      p~peinh                  AS peinh,
      p~netpr                  AS bprei,
      p~matkl                  AS matkl,
      MIN( s~eindt )           AS eindt,
      p~werks                  AS zplant,
      "p~lgort                  AS zstr_loca,
      h~waers                  AS zcurrency,
      p~retpo                  AS retpo
    FROM ekpo AS p
    INNER JOIN ekko AS h
      ON h~ebeln = p~ebeln
    LEFT OUTER JOIN eket AS s
      ON s~ebeln = p~ebeln
     AND s~ebelp = p~ebelp
    WHERE p~ebeln = @lv_ebeln
      AND p~loekz <> 'L'
    GROUP BY
*      p~ebelp, p~matnr, p~txz01, p~menge, p~meins,
*      p~peinh, p~netpr, p~matkl, p~werks,
*      p~retpo, p~lgort, h~waers
    p~ebelp, p~matnr, p~txz01, p~menge, p~meins,
    p~peinh, p~netpr, p~matkl, p~werks,
    p~retpo, h~waers
    INTO CORRESPONDING FIELDS OF TABLE @gt_po_item.

  IF gt_po_item IS INITIAL.
    MESSAGE |No items found for PO { gs_po_header-ebeln }.| TYPE 'S' DISPLAY LIKE 'E'.
    RETURN.
  ENDIF.

  SORT gt_po_item BY po_item.
  PERFORM alv_refresh USING 'GO_GRID_01'.
ENDFORM.



*&---------------------------------------------------------------------*
*& Form get_short_text
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
FORM get_short_text .
  TYPES: BEGIN OF ty_makt_map,
           matnr TYPE matnr,
           maktx TYPE maktx,
         END OF ty_makt_map.

  DATA: lt_matnr TYPE SORTED TABLE OF matnr WITH UNIQUE KEY table_line,
        lt_makt  TYPE HASHED TABLE OF ty_makt_map WITH UNIQUE KEY matnr,
        ls_makt  TYPE ty_makt_map.

  FIELD-SYMBOLS: <fs_item> TYPE LINE OF ty_t_po_item.

  LOOP AT gt_po_item ASSIGNING <fs_item>.
    IF <fs_item>-matnr IS NOT INITIAL.
      "lv_matnr = |{ lv_matnr ALPHA = IN }|.
      INSERT <fs_item>-matnr INTO TABLE lt_matnr.
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

  SELECT ekpo~matnr, ekpo~netpr, ekko~waers, ekpo~peinh, mara~meins, ekpo~infnr
  FROM ekpo
  INNER JOIN mara ON ekpo~matnr = mara~matnr
  INNER JOIN ekko ON ekko~ebeln = ekpo~ebeln
  FOR ALL ENTRIES IN @lt_matnr
  WHERE ekpo~matnr = @lt_matnr-table_line
    AND ekpo~infnr IS not INITIAL
  INTO TABLE @DATA(lt_po_item1).
  SORT lt_po_item1 ASCENDING BY matnr.


  LOOP AT gt_po_item ASSIGNING <fs_item>.
    DATA(lv_matnr_item) = <fs_item>-matnr.

    READ TABLE lt_makt INTO ls_makt WITH KEY matnr = lv_matnr_item.
    IF sy-subrc = 0.
      <fs_item>-zmate_des = ls_makt-maktx.
    ENDIF.

    READ TABLE lt_po_item1 INTO DATA(ls_po_item1)
                             WITH KEY matnr = lv_matnr_item
                             BINARY SEARCH.
    IF sy-subrc = 0.
      <fs_item>-zunit = ls_po_item1-meins.
      <fs_item>-bprei = ls_po_item1-netpr.
      <fs_item>-zcurrency = ls_po_item1-waers.
      <fs_item>-peinh = ls_po_item1-peinh.
    ENDIF.
  ENDLOOP.
  PERFORM alv_refresh USING 'GO_GRID_01'.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form get_data_po
*&---------------------------------------------------------------------*
FORM get_data_200.

  DATA: lv_lifnr TYPE lifnr.

  TYPES: BEGIN OF ty_ekpo,
           ebelp TYPE ekpo-ebelp,
           matnr TYPE ekpo-matnr,
           txz01 TYPE ekpo-txz01,
           meins TYPE ekpo-meins,
           werks TYPE ekpo-werks,
           elikz TYPE ekpo-elikz,
           retpo TYPE ekpo-retpo,
           menge TYPE ekpo-menge,
           loekz TYPE ekpo-loekz,
         END OF ty_ekpo.

  TYPES: BEGIN OF ty_wemng,
           ebelp TYPE eket-ebelp,
           wemng TYPE eket-wemng,
         END OF ty_wemng.

  DATA: lt_ekpo  TYPE STANDARD TABLE OF ty_ekpo,
        ls_ekpo  TYPE ty_ekpo,
        lt_wemng TYPE HASHED TABLE OF ty_wemng WITH UNIQUE KEY ebelp.

  CLEAR: gs_receipt_item, gt_receipt_item.

  " 1) Check input
  IF gs_receipt_header-ebeln IS INITIAL.
    MESSAGE 'Vui lòng nh#p Purchase Order Number.' TYPE 'S' DISPLAY LIKE 'E'.
    RETURN.
  ENDIF.

  " 2) Header: Vendor
  SELECT SINGLE lifnr INTO @lv_lifnr
    FROM ekko
    WHERE ebeln = @gs_receipt_header-ebeln.
  IF lv_lifnr IS INITIAL.
    MESSAGE |PO { gs_receipt_header-ebeln } không có Vendor.| TYPE 'S' DISPLAY LIKE 'E'.
    RETURN.
  ENDIF.

  " 3) Item EKPO (b# item b# xoá)
  SELECT ebelp, matnr, txz01, meins, werks, elikz, retpo, menge, loekz
    FROM ekpo
    INTO TABLE @lt_ekpo
    WHERE ebeln = @gs_receipt_header-ebeln
      AND loekz <> 'L'.
  IF lt_ekpo IS INITIAL.
    MESSAGE |PO { gs_receipt_header-ebeln } không có dòng.| TYPE 'S' DISPLAY LIKE 'E'.
    RETURN.
  ENDIF.

  " 4) T#ng GR (EKET)
  CLEAR lt_wemng.
  SELECT ebelp, SUM( wemng ) AS wemng
    FROM eket
    WHERE ebeln = @gs_receipt_header-ebeln
    GROUP BY ebelp
    INTO TABLE @lt_wemng.

  " 5) Map sang ALV
  LOOP AT lt_ekpo INTO ls_ekpo.

    DATA(lv_wemng) = VALUE eket-wemng( ).
    lv_wemng = VALUE #( lt_wemng[ ebelp = ls_ekpo-ebelp ]-wemng OPTIONAL ).

    CLEAR gs_receipt_item.
    gs_receipt_item-purchase_order = gs_receipt_header-ebeln.
    gs_receipt_item-item           = ls_ekpo-ebelp.
    gs_receipt_item-line           = ls_ekpo-ebelp.
    gs_receipt_item-material       = ls_ekpo-matnr.
    gs_receipt_item-short_text     = ls_ekpo-txz01.
    gs_receipt_item-uom            = ls_ekpo-meins.
    gs_receipt_item-plant          = ls_ekpo-werks.
    gs_receipt_item-vendor         = lv_lifnr.

    " Default GR
    IF ls_ekpo-retpo = 'X'.
      gs_receipt_item-movement_type  = '161'.
    ELSE.
      gs_receipt_item-movement_type  = '101'.
    ENDIF.
    gs_receipt_item-stock_type     = 'Unrestricted'.

    " === Del.Compl. Ind. hi#n th# nh# MIGO ===
    " MIGO dùng domain MIGO_ELIKZ (A/M/space):
    " - EKPO-ELIKZ = ''  -> 'A' (Set automatically)
    " - EKPO-ELIKZ = 'X' -> 'M' (Set manually)
    IF ls_ekpo-elikz IS INITIAL.
      gs_receipt_item-delivery_compl = 'A'.
    ELSE.
      gs_receipt_item-delivery_compl = 'M'.
    ENDIF.

    gs_receipt_item-returns_item = COND #( WHEN ls_ekpo-retpo = 'X' THEN 'X' ELSE space ).

    gs_receipt_item-qty_in_une = ls_ekpo-menge.
    gs_receipt_item-quantity   = ls_ekpo-menge - lv_wemng.
    IF gs_receipt_item-quantity < 0.
      gs_receipt_item-quantity = 0.
    ENDIF.

    gs_receipt_item-del_note_qty = 0.

    APPEND gs_receipt_item TO gt_receipt_item.
  ENDLOOP.

  SORT gt_receipt_item BY item.
  " Sau khi loop qua và fetch data v# s# ch#y refresh ## modify alv
  PERFORM alv_refresh USING 'GO_GRID_01'.
  " Khóa các dòng có move type là 101 (retuen không ###c tick)
  PERFORM lock_101_rows_in_alv.
ENDFORM.

*&---------------------------------------------------------------------*
*& Form get_data_300
*&---------------------------------------------------------------------*
FORM get_data_300.
  DATA:lv_ebeln TYPE ebeln.
  lv_ebeln = |{ gs_invoice_header-ebeln ALPHA = IN }|.
  gs_invoice_header-ebeln = lv_ebeln.

  CLEAR: gs_invoice_item, gt_invoice_item, gs_vendor.

  DATA: ls_ekko TYPE ekko,
        lt_ekpo TYPE TABLE OF ekpo,
        ls_lfa1 TYPE lfa1,
        ls_adrc TYPE adrc.

  " 0) Check input
  IF lv_ebeln IS INITIAL.
    MESSAGE 'Vui lòng nh#p PO Number.' TYPE 'S' DISPLAY LIKE 'E'.
    PERFORM alv_refresh USING 'GO_GRID_01'.
    RETURN.
  ENDIF.

  " 1) Header EKKO
  SELECT SINGLE * INTO @ls_ekko
    FROM ekko
    WHERE ebeln = @lv_ebeln.
  IF sy-subrc <> 0.
    MESSAGE |PO { lv_ebeln } không t#n t#i.| TYPE 'S' DISPLAY LIKE 'E'.
    PERFORM alv_refresh USING 'GO_GRID_01'.
    RETURN.
  ENDIF.

  gs_invoice_header-bukrs     = ls_ekko-bukrs.
  gs_invoice_header-zcurrency = ls_ekko-waers.
  IF gs_invoice_header-bldat IS INITIAL. gs_invoice_header-bldat = sy-datum. ENDIF.
  IF gs_invoice_header-budat IS INITIAL. gs_invoice_header-budat = sy-datum. ENDIF.

  " 2) Vendor info (hi#n th#)
  IF ls_ekko-lifnr IS NOT INITIAL.
    SELECT SINGLE * INTO @ls_lfa1 FROM lfa1 WHERE lifnr = @ls_ekko-lifnr.
    IF sy-subrc = 0 AND ls_lfa1-adrnr IS NOT INITIAL.
      SELECT SINGLE * INTO @ls_adrc FROM adrc WHERE addrnumber = @ls_lfa1-adrnr.
    ENDIF.
    gs_vendor-name    = ls_lfa1-name1.
    gs_vendor-street  = ls_adrc-street.
    gs_vendor-city    = ls_adrc-city1.
    gs_vendor-country = ls_adrc-country.
    CONCATENATE ls_adrc-street ls_adrc-house_num1 ls_adrc-city1 ls_adrc-post_code1
                INTO gs_vendor-address SEPARATED BY space.
  ENDIF.

  " 3) L#y EKPO cho các dòng Return (RETPO='X'), b# dòng xoá
  SELECT ebelp, matnr, txz01, meins, menge, peinh, netpr, mwskz
    FROM ekpo
    INTO CORRESPONDING FIELDS OF TABLE @lt_ekpo
    WHERE ebeln = @lv_ebeln
      AND loekz <> 'L'
      AND retpo  = 'X'.
  IF lt_ekpo IS INITIAL.
    MESSAGE |PO { lv_ebeln } không có dòng Return.| TYPE 'S' DISPLAY LIKE 'E'.
    PERFORM alv_refresh USING 'GO_GRID_01'.
    RETURN.
  ENDIF.

  " 4) Map vào ALV + tính ti#n (Amount = NETPR * MENGE)
  DATA: lv_total_amount TYPE p DECIMALS 2 VALUE '0.00'.

  FIELD-SYMBOLS:
    <s_ekpo>  TYPE ekpo,
    <f_item>  TYPE any,
    <f_mat>   TYPE any,
    <f_txt>   TYPE any,
    <f_qty>   TYPE any,
    <f_uom>   TYPE any,
    <f_mwskz> TYPE any,
    <f_amt>   TYPE any.

  LOOP AT lt_ekpo ASSIGNING <s_ekpo>.
    CLEAR gs_invoice_item.
    ASSIGN COMPONENT 'ITEM' OF STRUCTURE gs_invoice_item TO <f_item>.
    IF <f_item> IS ASSIGNED.  <f_item> = <s_ekpo>-ebelp.  ENDIF.

    ASSIGN COMPONENT 'MATERIAL'   OF STRUCTURE gs_invoice_item TO <f_mat>.
    IF <f_mat>   IS ASSIGNED. <f_mat>   = <s_ekpo>-matnr.  ENDIF.
    ASSIGN COMPONENT 'SHORT_TEXT' OF STRUCTURE gs_invoice_item TO <f_txt>.
    IF <f_txt>   IS ASSIGNED. <f_txt>   = <s_ekpo>-txz01.  ENDIF.
    ASSIGN COMPONENT 'QUANTITY'   OF STRUCTURE gs_invoice_item TO <f_qty>.
    IF <f_qty>   IS ASSIGNED. <f_qty>   = <s_ekpo>-menge.  ENDIF.
    ASSIGN COMPONENT 'UOM'        OF STRUCTURE gs_invoice_item TO <f_uom>.
    IF <f_uom>   IS ASSIGNED. <f_uom>   = <s_ekpo>-meins.  ENDIF.
    ASSIGN COMPONENT 'TAX_CODE'   OF STRUCTURE gs_invoice_item TO <f_mwskz>.
    IF <f_mwskz> IS ASSIGNED. <f_mwskz> = <s_ekpo>-mwskz. ENDIF.

    DATA(lv_unit_price)  = <s_ekpo>-netpr.
    DATA:lv_line_amount TYPE p DECIMALS 2.
    lv_line_amount = lv_unit_price * <s_ekpo>-menge.

    ASSIGN COMPONENT 'AMOUNT' OF STRUCTURE gs_invoice_item TO <f_amt>.
    IF <f_amt> IS ASSIGNED. <f_amt> = lv_line_amount. ENDIF.

    lv_total_amount = lv_total_amount + lv_line_amount.
    APPEND gs_invoice_item TO gt_invoice_item.
  ENDLOOP.

  " 5) T#ng ti#n header
  FIELD-SYMBOLS <f_hdr_amt> TYPE any.
  ASSIGN COMPONENT 'ZAMOUNT' OF STRUCTURE gs_invoice_header TO <f_hdr_amt>.
  IF <f_hdr_amt> IS ASSIGNED. <f_hdr_amt> = lv_total_amount. ENDIF.

  IF gt_invoice_item IS INITIAL.
    MESSAGE 'Không có dòng Return ## l#p hóa ##n.' TYPE 'S' DISPLAY LIKE 'W'.
  ENDIF.

  PERFORM alv_refresh USING 'GO_GRID_01'.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form get_date
*&---------------------------------------------------------------------*
FORM get_date .
  DATA: lv_today TYPE sy-datum.

  " L#y ngày hi#n t#i t# h# th#ng
  lv_today = sy-datum.

  " Gán vào field t##ng #ng trong header
  gs_receipt_header-bldat =  lv_today.
  gs_receipt_header-budat =  lv_today.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form get_po_history
*&---------------------------------------------------------------------*
FORM get_po_history .

  CLEAR gt_po_hist.

  IF p_hist IS INITIAL.
    MESSAGE 'Please enter vendor (LIFNR) for history.' TYPE 'S' DISPLAY LIKE 'E'.
    RETURN.
  ENDIF.

  DATA(lv_lifnr) = p_hist.
  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
    EXPORTING input  = lv_lifnr
    IMPORTING output = lv_lifnr.
  p_hist = lv_lifnr.

  " Select header + item rows for that vendor (skip deleted headers/items)
  SELECT a~ebeln,
         b~ebelp,
         a~ernam,
"        a~erdat,                 " <-- uses ERDAT from EKKO (change if your EKKO uses another date field)
         a~aedat AS erdat,      " <-- alternative: uncomment & adapt if your system uses AEDAT (replace above)
         a~lifnr,
         b~matnr,
         b~txz01   AS zmate_des,
         b~menge   AS bstmg,
         b~meins   AS zunit,
         b~netpr   AS bprei,
         a~waers   AS zcurrency,
         b~werks   AS zplant
    FROM ekko AS a
    INNER JOIN ekpo AS b
      ON b~ebeln = a~ebeln
   WHERE a~lifnr = @lv_lifnr
     AND a~loekz <> 'L'
     AND b~loekz <> 'L'
    INTO CORRESPONDING FIELDS OF TABLE @gt_po_hist.

  IF gt_po_hist IS INITIAL.
    MESSAGE |No Purchase Orders found for vendor { p_hist }.| TYPE 'S' DISPLAY LIKE 'W'.
  ENDIF.

  SORT gt_po_hist BY ebeln ebelp.

  " Set ALV mode so layout/fieldcat/display will treat this list as history
  gv_alv_mode = 'POHIS'.

ENDFORM.
