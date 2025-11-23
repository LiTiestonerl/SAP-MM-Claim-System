*&---------------------------------------------------------------------*
*& Include          ZPG_MM_CLAIMAMOUNTF01
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*& Form show_popup_change
*&---------------------------------------------------------------------*
*& Show a popup to confirm if the user really wants to leave without saving data.
*&---------------------------------------------------------------------*

FORM show_popup_change .
  DATA : ld_ans TYPE c.
  CALL FUNCTION 'POPUP_TO_CONFIRM'
    EXPORTING
      text_question  = 'Data has not been saved. Do you really want to leave?'
      text_button_1  = 'Yes'(001)
      text_button_2  = 'No'(002)
    IMPORTING
      answer         = ld_ans
    EXCEPTIONS
      text_not_found = 1
      OTHERS         = 2.
  IF ld_ans = '1'.
    gs_edit = ''.
  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form fetch_data
*&---------------------------------------------------------------------*
*& Load header and item data from table ztgsu06_test for company code US00.
*&---------------------------------------------------------------------*
FORM fetch_data.

  SELECT SINGLE bukrs                    AS company_code,
                lifnr                    AS supplier,
                transaction_type         AS transaction_type,
                bldat                    AS invoice_date,
                budat                    AS posting_date,
                zamount                  AS amount,
                zcurrency                AS currency
    INTO @DATA(ls_head)
    FROM ztgsu06_test
    WHERE bukrs = 'US00'.

  MOVE-CORRESPONDING ls_head TO gs_header.

  SELECT ztgsu06_test~bukrs,
         ztgsu06_test~gl_account,
         ztgsu06_test~dc_indicator,
         ztgsu06_test~business_area,
         ztgsu06_test~cost_center,
         ztgsu06_test~zamount
    INTO TABLE @DATA(lt_item)
    FROM ztgsu06_test
    WHERE ztgsu06_test~bukrs = 'US00'.

  CLEAR gt_item.

  LOOP AT lt_item INTO DATA(ls_item).
    gs_item-gl_account       = ls_item-gl_account.
    gs_item-dc_indicator     = ls_item-dc_indicator.
    gs_item-amount_doc_curr  = ls_item-zamount.
    gs_item-loc_doc_curr     = ls_item-zamount.
    gs_item-business_area    = ls_item-business_area.
    gs_item-cost_center      = ls_item-cost_center.
    gs_item-currency         = gs_header-currency.
    APPEND gs_item TO gt_item.
  ENDLOOP.

  PERFORM get_name_enter.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form post_bapi
*&---------------------------------------------------------------------*
*& Post accounting document using BAPI_ACC_DOCUMENT_POST based on collected data.
*&---------------------------------------------------------------------*
FORM post_bapi .

  DATA: lv_obj_type TYPE bapiache09-obj_type,
        lv_obj_key  TYPE bapiache09-obj_key,
        lv_obj_sys  TYPE bapiache09-obj_sys,
        lt_return   TYPE TABLE OF bapiret2,
        ls_return   TYPE bapiret2.

  DATA: ls_header     TYPE bapiache09,
        lt_accountgl  TYPE TABLE OF bapiacgl09,
        lt_accountpay TYPE TABLE OF bapiacap09,
        lt_currency   TYPE TABLE OF bapiaccr09,
        ls_accountgl  TYPE bapiacgl09,
        ls_accountpay TYPE bapiacap09,
        ls_currency   TYPE bapiaccr09.

  DATA: lv_total_debit  TYPE wrbtr VALUE 0,
        lv_total_credit TYPE wrbtr VALUE 0.

  " Ki#m tra transaction type
  IF gs_header-transaction_type IS INITIAL.
    MESSAGE 'Transaction Type is required.' TYPE 'E'.
    EXIT.
  ENDIF.

  ls_header-username    = sy-uname.
  ls_header-comp_code   = gs_header-company_code.
  ls_header-doc_date    = gs_header-invoice_date.
  ls_header-pstng_date  = gs_header-posting_date.
  ls_header-header_txt  = 'Invoice posted via BAPI'.
  ls_header-ref_doc_no  = 'BAPI_AUTO_01'.
  "Sets the document type depending on transaction type (KG = Vendor Credit, KR = Vendor Invoice).
  CASE gs_header-transaction_type.
    WHEN 'G'. ls_header-doc_type = 'KG'.
    WHEN 'R'. ls_header-doc_type = 'KR'.
    WHEN OTHERS.
      MESSAGE 'Invalid transaction type.' TYPE 'E'.
      EXIT.
  ENDCASE.

  CLEAR: lt_accountgl, lt_accountpay, lt_currency.

  DATA(lv_idx) = 1.
  LOOP AT gt_item INTO gs_item.
    DATA: lv_itemno     TYPE bapiacgl09-itemno_acc,
          lv_gl_account TYPE saknr,
          lv_vendor_no  TYPE lifnr.
    lv_itemno = lv_idx.
    lv_idx += 1.

    CLEAR ls_currency.
    ls_currency-itemno_acc = lv_itemno.
    ls_currency-currency   = gs_header-currency.
    " If document type is KR, it must be Debit (S). Otherwise, throw error.
    CASE ls_header-doc_type.

      WHEN 'KR'. " G/L document
        IF gs_item-dc_indicator <> 'S'.
          MESSAGE |KR only accepts Debit (S), line { lv_itemno } is invalid| TYPE 'S' DISPLAY LIKE 'E'.
          EXIT.
        ENDIF.

        CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
          EXPORTING
            input  = gs_item-gl_account
          IMPORTING
            output = lv_gl_account.

        CLEAR ls_accountgl.
        ls_accountgl-itemno_acc = lv_itemno.
        ls_accountgl-gl_account = lv_gl_account.
        ls_accountgl-comp_code  = gs_header-company_code.
        ls_accountgl-costcenter = gs_item-cost_center.
        ls_accountgl-bus_area   = gs_item-business_area.
        ls_accountgl-item_text  = gs_item-short_text.
        APPEND ls_accountgl TO lt_accountgl.

        ls_currency-amt_doccur = abs( gs_item-amount_doc_curr ).
        lv_total_credit += abs( gs_item-amount_doc_curr ).
        " Similarly, if KG, it must be Credit (H).
      WHEN 'KG'. " Vendor Invoice
        IF gs_item-dc_indicator <> 'H'.
          MESSAGE |KG only accepts Credit (H). Line { lv_itemno } is invalid.| TYPE 'S' DISPLAY LIKE 'E'.

          EXIT.
        ENDIF.
        " Formats the GL account number into SAP internal format (adds leading zeroes).
        CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
          EXPORTING
            input  = gs_item-gl_account
          IMPORTING
            output = lv_gl_account.

        CLEAR ls_accountgl.
        ls_accountgl-itemno_acc = lv_itemno.
        ls_accountgl-gl_account = lv_gl_account.
        ls_accountgl-comp_code  = gs_header-company_code.
        ls_accountgl-costcenter = gs_item-cost_center.
        ls_accountgl-bus_area   = gs_item-business_area.
        ls_accountgl-item_text  = gs_item-short_text.
        APPEND ls_accountgl TO lt_accountgl.  " Appends the G/L line to the G/L account posting table.

        ls_currency-amt_doccur = -1 * abs( gs_item-amount_doc_curr ).
        lv_total_debit += abs( gs_item-amount_doc_curr ).

    ENDCASE.

    APPEND ls_currency TO lt_currency.
  ENDLOOP.
  " If no debit line exists, adds vendor line to balance it.
  IF ls_header-doc_type = 'KR' AND lv_total_debit = 0.
    lv_itemno = lv_idx.
    "Prepares and appends payable line item.
    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        input  = gs_header-supplier
      IMPORTING
        output = lv_vendor_no.

    CLEAR ls_accountpay.
    ls_accountpay-itemno_acc = lv_itemno.
    ls_accountpay-vendor_no  = lv_vendor_no.
    ls_accountpay-comp_code  = gs_header-company_code.
    ls_accountpay-bline_date = gs_header-invoice_date.
    ls_accountpay-item_text  = ''.
    APPEND ls_accountpay TO lt_accountpay.

    CLEAR ls_currency.
    ls_currency-itemno_acc  = lv_itemno.
    ls_currency-currency    = gs_header-currency.
    ls_currency-amt_doccur  = -1 * lv_total_credit.
    APPEND ls_currency TO lt_currency.

    lv_total_debit = lv_total_credit.

  ELSEIF ls_header-doc_type = 'KG' AND lv_total_credit = 0. " The reverse logic applies for KG if no credit lines exist.
    lv_itemno = lv_idx.
    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        input  = gs_header-supplier
      IMPORTING
        output = lv_vendor_no.

    CLEAR ls_accountpay.
    ls_accountpay-itemno_acc = lv_itemno.
    ls_accountpay-vendor_no  = lv_vendor_no.
    ls_accountpay-comp_code  = gs_header-company_code.
    ls_accountpay-bline_date = gs_header-invoice_date.
    ls_accountpay-item_text  = ''.
    APPEND ls_accountpay TO lt_accountpay.

    CLEAR ls_currency.
    ls_currency-itemno_acc  = lv_itemno.
    ls_currency-currency    = gs_header-currency.
    ls_currency-amt_doccur  = lv_total_debit.
    APPEND ls_currency TO lt_currency.

    lv_total_credit = lv_total_debit.
  ENDIF.

  " === Check cân b#ng
  IF lv_total_debit <> lv_total_credit.
    MESSAGE |Amount not balanced: Debit = { lv_total_debit }, Credit = { lv_total_credit }| TYPE 'S' DISPLAY LIKE 'E'.
    EXIT.
  ENDIF.
  " Posts the document using SAP standard BAPI.
  CALL FUNCTION 'BAPI_ACC_DOCUMENT_POST'
    EXPORTING
      documentheader = ls_header
    IMPORTING
      obj_type       = lv_obj_type
      obj_key        = lv_obj_key
      obj_sys        = lv_obj_sys
    TABLES
      accountgl      = lt_accountgl
      accountpayable = lt_accountpay
      currencyamount = lt_currency
      return         = lt_return.
  " Checks if there are errors in the return table.
  READ TABLE lt_return INTO ls_return WITH KEY type = 'E'.
  IF sy-subrc = 0.
    LOOP AT lt_return INTO ls_return.
      WRITE: / ls_return-type, ls_return-id, ls_return-number, ls_return-message.
    ENDLOOP.
    MESSAGE '# Error occurred. Document could not be posted.'  TYPE 'S' DISPLAY LIKE 'E'.

    EXIT.
  ENDIF.
  " Commits the transaction to the database.
  CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
    EXPORTING
      wait = 'X'.

  MESSAGE |# Posted successfully! Document number: { lv_obj_key }| TYPE 'S'.
*  " Opens the vendor line item display (FBL1N) for the posted vendor.
*  SET PARAMETER ID 'LIF' FIELD gs_header-supplier.
*  SET PARAMETER ID 'BUK' FIELD gs_header-company_code.
*  CALL TRANSACTION 'FBL1N' AND SKIP FIRST SCREEN.
  " Hi#n th# MESSAGE tr##c, sau #ó chuy#n màn hình
  WAIT UP TO 2 SECONDS.

  SET PARAMETER ID 'LIF' FIELD gs_header-supplier.
  SET PARAMETER ID 'BUK' FIELD gs_header-company_code.
  LEAVE TO TRANSACTION 'FBL1N'.

ENDFORM.
