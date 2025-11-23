*&---------------------------------------------------------------------*
*& Include          ZPG_MM_CLAIMAMOUNTF02
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*& Form get_name_enter
*&---------------------------------------------------------------------*
*& Fetch vendor & GL descriptions, populate item table
*&---------------------------------------------------------------------*
FORM get_name_enter .
  SELECT SINGLE lfa1~lifnr       AS supplier,
                lfb1~bukrs       AS company_code,
                lfa1~adrnr AS address,
                lfa1~stras AS street,
                lfa1~ort01 AS city,
                lfa1~name1 AS name,
                lfa1~land1 AS country
    INTO @DATA(ls_vendor)
    FROM lfa1
    INNER JOIN lfb1 ON lfa1~lifnr = lfb1~lifnr
    WHERE lfa1~lifnr = @gs_header-supplier.

  MOVE-CORRESPONDING ls_vendor TO gs_vendor.

  SELECT DISTINCT a~bukrs,
         a~saknr,
         b~txt20 AS short_text
    INTO TABLE @DATA(lt_gl_text)
    FROM skb1 AS a
    INNER JOIN skat AS b ON a~saknr = b~saknr
    FOR ALL ENTRIES IN @gt_item
    WHERE a~bukrs = @gs_header-company_code
      AND a~saknr = @gt_item-gl_account
      AND b~spras = @sy-langu.

  LOOP AT gt_item INTO gs_item.
    READ TABLE lt_gl_text INTO DATA(ls_text)
      WITH KEY bukrs = gs_header-company_code
               saknr = gs_item-gl_account.
    IF sy-subrc = 0.
      gs_item-short_text    = ls_text-short_text.
      gs_item-loc_doc_curr  = gs_item-amount_doc_curr.
      MODIFY gt_item FROM gs_item.
    ENDIF.
  ENDLOOP.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form get_gl_name
*&---------------------------------------------------------------------*
*& Fetch GL account names (one-by-one) and update each item
*&---------------------------------------------------------------------*
FORM get_gl_name .
  DATA: lv_idx        TYPE sy-tabix,
        lv_short_text TYPE skat-txt20.
  LOOP AT gt_item INTO gs_item.
    lv_idx = sy-tabix.
    CLEAR lv_short_text.
    SELECT SINGLE txt20
      INTO lv_short_text
      FROM skb1 AS a
      INNER JOIN skat AS b
        ON a~saknr = b~saknr
      WHERE a~bukrs = gs_header-company_code
        AND a~saknr = gs_item-gl_account
        AND b~spras = sy-langu.

    IF sy-subrc = 0.
      gs_item-short_text = lv_short_text.
      gs_item-loc_doc_curr = gs_item-amount_doc_curr.
      MODIFY gt_item FROM gs_item INDEX lv_idx.
    ENDIF.

  ENDLOOP.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form get_vendor_name
*&---------------------------------------------------------------------*
*& Retrieve and validate vendor details; shows message if invalid or not found
*&---------------------------------------------------------------------*
FORM get_vendor_name .
  IF gs_header-supplier IS NOT INITIAL AND
       gs_header-company_code IS NOT INITIAL.
    DATA(lv_lifnr) = |{ gs_header-supplier ALPHA = IN }|.

    SELECT SINGLE lifnr AS supplier,
                  adrnr,
                  ort01,
                  name1,
                  land1
      INTO @DATA(ls_vendor)
      FROM lfa1
      WHERE lifnr = @lv_lifnr.


    IF sy-subrc = 0.
      gs_vendor-name     = ls_vendor-supplier.
      gs_vendor-address  = ls_vendor-adrnr.
      gs_vendor-street   = ls_vendor-name1.
      gs_vendor-city     = ls_vendor-ort01.
      gs_vendor-country  = ls_vendor-land1.
    ELSE.
      MESSAGE 'No vendor information found for the specified Purchase Order (PO)' TYPE 'S' DISPLAY LIKE 'E'.

    ENDIF.

  ELSE.
    MESSAGE ID 'ZCLAIMAMOUNT_MSG' TYPE 'S' NUMBER '001' DISPLAY LIKE 'E'.  " Please enter a vendor number
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form get_cal_balane
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
FORM get_cal_balane .

  DATA: lv_doc_type  TYPE c LENGTH 2,
        lv_balance   TYPE ztgsu06-zamount,   "KI#U S# (CURR/DEC)
        lv_item_amt  TYPE ztgsu06-zamount,
        lv_bal_text  TYPE char30.            "TEXT HI#N TH#

  "--- Xác ##nh lo#i ch#ng t# & chu#n hóa s# t#ng # header (S#) ---
  CASE gs_header-transaction_type.
    WHEN 'G'.    " Vendor Credit
      lv_doc_type              = 'KG'.
      gs_header-balance    = abs( gs_header-amount ).
    WHEN 'R'.    " Vendor Invoice
      lv_doc_type              = 'KR'.
      gs_header-balance    = abs( gs_header-amount ) * -1.
    WHEN OTHERS.
      MESSAGE 'Invalid transaction type.' TYPE 'E'.
      EXIT.
  ENDCASE.

  "--- B#t ##u t# balance header ---
  lv_balance = gs_header-balance.

  "--- C#ng d#n các item theo DC indicator ---
  LOOP AT gt_item INTO gs_item.
    CASE gs_item-dc_indicator.
      WHEN 'S'. "Debit -> âm
        lv_item_amt = abs( CONV ztgsu06-zamount( gs_item-amount_doc_curr ) ).
      WHEN 'H'. "Credit -> d##ng
        lv_item_amt = - abs( CONV ztgsu06-zamount( gs_item-amount_doc_curr ) ).
      WHEN OTHERS.
        MESSAGE |Indicator không h#p l# # dòng { sy-tabix }| TYPE 'S' DISPLAY LIKE 'E'.
        EXIT.
    ENDCASE.
    lv_balance = lv_balance + lv_item_amt.
  ENDLOOP.

  "--- L#u l#i: 1) s# ## tính toán, 2) text ## hi#n th# n#u mu#n ---
  gs_header-balance = lv_balance.
  WRITE lv_balance TO lv_bal_text CURRENCY gs_header-currency.
  CONDENSE lv_bal_text.
  gs_header-balance = lv_bal_text.

ENDFORM.
