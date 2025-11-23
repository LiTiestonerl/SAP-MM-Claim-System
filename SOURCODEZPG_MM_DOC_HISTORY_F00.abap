*&---------------------------------------------------------------------*
*& Include  ZPG_MM_DOC_HISTORY_F00
*&---------------------------------------------------------------------*

FORM get_gi_history  USING    iv_sdate TYPE sy-datum
                              iv_edate TYPE sy-datum
                     CHANGING ct_hist  TYPE ty_t_hist.

  " GI Scrapping = 551; include 552 as reversal (flag it)
  SELECT s~mblnr, s~mjahr, s~bwart, s~werks, s~lgort, s~matnr,
         s~menge, s~meins, s~dmbtr,
         h~bldat, h~budat, h~usnam, h~xblnr
    FROM mseg AS s
    INNER JOIN mkpf AS h
      ON h~mblnr = s~mblnr AND h~mjahr = s~mjahr
    WHERE s~bwart IN ('551','552')
      AND h~budat BETWEEN @iv_sdate AND @iv_edate
    INTO TABLE @DATA(lt_mm).

  LOOP AT lt_mm ASSIGNING FIELD-SYMBOL(<ls>).
    APPEND VALUE ty_hist(
      category    = c_view_gi
      docno       = <ls>-mblnr
      docyear     = <ls>-mjahr
      docdate     = <ls>-bldat
      postdate    = <ls>-budat
      movetype    = <ls>-bwart
      plant       = <ls>-werks
      sloc        = <ls>-lgort
      matnr       = <ls>-matnr
      qty         = <ls>-menge
      uom         = <ls>-meins
      amount      = <ls>-dmbtr
      created_by  = <ls>-usnam
      reference   = <ls>-xblnr
      reversal    = COND #( WHEN <ls>-bwart = '552' THEN 'X' ELSE '' ) )
    TO ct_hist.

    IF <ls>-matnr IS NOT INITIAL.
      INSERT <ls>-matnr INTO TABLE gt_matnr.
    ENDIF.
    INSERT <ls>-bwart INTO TABLE gt_bwart.
  ENDLOOP.

ENDFORM.


FORM get_ret_history USING    iv_sdate TYPE sy-datum
                              iv_edate TYPE sy-datum
                     CHANGING ct_hist  TYPE ty_t_hist.

  " Return Stock (161)
  SELECT s~mblnr, s~mjahr, s~bwart, s~werks, s~lgort, s~matnr,
         s~menge, s~meins, s~dmbtr, s~ebeln,
         h~bldat, h~budat, h~usnam, h~xblnr
    FROM mseg AS s
    INNER JOIN mkpf AS h
      ON h~mblnr = s~mblnr AND h~mjahr = s~mjahr
    WHERE s~bwart = '161'
      AND h~budat BETWEEN @iv_sdate AND @iv_edate
    INTO TABLE @DATA(lt_ret).

  LOOP AT lt_ret ASSIGNING FIELD-SYMBOL(<ls>).
    APPEND VALUE ty_hist(
      category    = c_view_ret
      docno       = <ls>-mblnr
      docyear     = <ls>-mjahr
      docdate     = <ls>-bldat
      postdate    = <ls>-budat
      movetype    = <ls>-bwart
      plant       = <ls>-werks
      sloc        = <ls>-lgort
      matnr       = <ls>-matnr
      qty         = <ls>-menge
      uom         = <ls>-meins
      amount      = <ls>-dmbtr
      ebeln       = <ls>-ebeln
      created_by  = <ls>-usnam
      reference   = <ls>-xblnr )
    TO ct_hist.

    IF <ls>-matnr IS NOT INITIAL.
      INSERT <ls>-matnr INTO TABLE gt_matnr.
    ENDIF.
    INSERT <ls>-bwart INTO TABLE gt_bwart.
    IF <ls>-ebeln IS NOT INITIAL.
      INSERT <ls>-ebeln INTO TABLE gt_ebeln.
    ENDIF.
  ENDLOOP.

  " Vendor by PO header (EKKO-LIFNR) – batched
  IF gt_ebeln IS NOT INITIAL.
    SELECT ebeln, lifnr
      FROM ekko
      FOR ALL ENTRIES IN @gt_ebeln
      WHERE ebeln = @gt_ebeln-table_line
      INTO TABLE @DATA(lt_po_lifnr).

    LOOP AT lt_po_lifnr ASSIGNING FIELD-SYMBOL(<lp>).
      INSERT <lp>-lifnr INTO TABLE gt_lifnr.
    ENDLOOP.

    LOOP AT ct_hist ASSIGNING FIELD-SYMBOL(<h>)
         WHERE category = c_view_ret AND ebeln IS NOT INITIAL AND lifnr IS INITIAL.
      READ TABLE lt_po_lifnr ASSIGNING FIELD-SYMBOL(<pl>)
        WITH KEY ebeln = <h>-ebeln.
      IF sy-subrc = 0.
        <h>-lifnr = <pl>-lifnr.
      ENDIF.
    ENDLOOP.
  ENDIF.

ENDFORM.


FORM get_clm_history
  USING    iv_sdate TYPE sy-datum
           iv_edate TYPE sy-datum
  CHANGING ct_hist  TYPE ty_t_hist.

  TYPES: BEGIN OF ty_bkpf,
           bukrs TYPE bkpf-bukrs,
           belnr TYPE bkpf-belnr,
           gjahr TYPE bkpf-gjahr,
           bldat TYPE bkpf-bldat,
           budat TYPE bkpf-budat,
           usnam TYPE bkpf-usnam,
           xblnr TYPE bkpf-xblnr,
           waers TYPE bkpf-waers,
         END OF ty_bkpf.

  TYPES: BEGIN OF ty_bseg,
           bukrs TYPE bseg-bukrs,
           belnr TYPE bseg-belnr,
           gjahr TYPE bseg-gjahr,
           lifnr TYPE bseg-lifnr,
           shkzg TYPE bseg-shkzg,
           dmbtr TYPE bseg-dmbtr,
         END OF ty_bseg.

  DATA: lt_bkpf TYPE STANDARD TABLE OF ty_bkpf,
        ls_bkpf TYPE ty_bkpf,
        lt_bseg TYPE STANDARD TABLE OF ty_bseg,
        ls_bseg TYPE ty_bseg,
        ls_hist TYPE ty_hist.

  DATA: lv_lifnr TYPE lifnr,
        lv_amt   TYPE bseg-dmbtr.

  CLEAR ct_hist.

  " 1) L#y các ch#ng t# FI lo#i KG trong kho#ng ngày post
  SELECT bukrs belnr gjahr bldat budat usnam xblnr waers
    INTO TABLE lt_bkpf
    FROM bkpf
    WHERE blart = 'KG'
      AND budat BETWEEN iv_sdate AND iv_edate.

  IF lt_bkpf IS INITIAL.
    RETURN.
  ENDIF.

  " 2) L#y dòng vendor t##ng #ng (KOART = 'K')
  SELECT bukrs belnr gjahr lifnr shkzg dmbtr
    INTO TABLE lt_bseg
    FROM bseg
    FOR ALL ENTRIES IN lt_bkpf
    WHERE bukrs = lt_bkpf-bukrs
      AND belnr = lt_bkpf-belnr
      AND gjahr = lt_bkpf-gjahr
      AND koart = 'K'.

  " 3) Map m#i ch#ng t# -> 1 dòng l#ch s#, c#ng/tr# DMBTR theo SHKZG
  LOOP AT lt_bkpf INTO ls_bkpf.

    CLEAR: lv_lifnr, lv_amt.

    LOOP AT lt_bseg INTO ls_bseg
         WHERE bukrs = ls_bkpf-bukrs
           AND belnr = ls_bkpf-belnr
           AND gjahr = ls_bkpf-gjahr.

      IF lv_lifnr IS INITIAL AND ls_bseg-lifnr IS NOT INITIAL.
        lv_lifnr = ls_bseg-lifnr.
      ENDIF.

      IF ls_bseg-shkzg = 'H'.
        lv_amt = lv_amt - ls_bseg-dmbtr.
      ELSE.
        lv_amt = lv_amt + ls_bseg-dmbtr.
      ENDIF.
    ENDLOOP.

    CLEAR ls_hist.
    ls_hist-category   = c_view_clm.
    ls_hist-docno      = ls_bkpf-belnr.
    ls_hist-docyear    = ls_bkpf-gjahr.
    ls_hist-docdate    = ls_bkpf-bldat.
    ls_hist-postdate   = ls_bkpf-budat.
    ls_hist-movetype   = ''.        " n/a
    ls_hist-plant      = ''.        " n/a
    ls_hist-sloc       = ''.        " n/a
    ls_hist-matnr      = ''.        " n/a
    ls_hist-qty        = 0.         " n/a
    ls_hist-uom        = ''.        " n/a
    ls_hist-amount     = lv_amt.    " Amount in Local Crcy (DMBTR #ã +-)
    ls_hist-currency   = ls_bkpf-waers. " ho#c ## tr#ng n#u b#n ch# mu#n local
    ls_hist-ebeln      = ''.        " n#u c#n có th# enrich thêm
    ls_hist-lifnr      = lv_lifnr.
    ls_hist-created_by = ls_bkpf-usnam.
    ls_hist-reference  = ls_bkpf-xblnr.
    ls_hist-reversal   = ''.        " không áp d#ng # #ây

    APPEND ls_hist TO ct_hist.

    IF NOT lv_lifnr IS INITIAL.
      INSERT lv_lifnr INTO TABLE gt_lifnr.
    ENDIF.

  ENDLOOP.

ENDFORM.


*&---------------------------------------------------------------------*
*& New: GR 101 and Return Delivery 122, via generic helper
*&---------------------------------------------------------------------*
FORM get_101_history USING    iv_sdate TYPE sy-datum
                               iv_edate TYPE sy-datum
                     CHANGING  ct_hist  TYPE ty_t_hist.
  PERFORM get_mm_by_bwart USING '101' iv_sdate iv_edate CHANGING ct_hist.
ENDFORM.

FORM get_122_history USING    iv_sdate TYPE sy-datum
                               iv_edate TYPE sy-datum
                     CHANGING  ct_hist  TYPE ty_t_hist.
  PERFORM get_mm_by_bwart USING '122' iv_sdate iv_edate CHANGING ct_hist.
ENDFORM.


FORM get_mm_by_bwart USING    iv_bwart TYPE bwart
                               iv_sdate TYPE sy-datum
                               iv_edate TYPE sy-datum
                     CHANGING  ct_hist  TYPE ty_t_hist.

  " Generic MM history by movement type (includes PO/vendor enrichment)
  SELECT s~mblnr, s~mjahr, s~bwart, s~werks, s~lgort, s~matnr,
         s~menge, s~meins, s~dmbtr, s~ebeln,
         h~bldat, h~budat, h~usnam, h~xblnr
    FROM mseg AS s
    INNER JOIN mkpf AS h
      ON h~mblnr = s~mblnr AND h~mjahr = s~mjahr
    WHERE s~bwart = @iv_bwart
      AND h~budat BETWEEN @iv_sdate AND @iv_edate
    INTO TABLE @DATA(lt_mm).

  LOOP AT lt_mm ASSIGNING FIELD-SYMBOL(<ls>).
    APPEND VALUE ty_hist(
      category    = iv_bwart
      docno       = <ls>-mblnr
      docyear     = <ls>-mjahr
      docdate     = <ls>-bldat
      postdate    = <ls>-budat
      movetype    = <ls>-bwart
      plant       = <ls>-werks
      sloc        = <ls>-lgort
      matnr       = <ls>-matnr
      qty         = <ls>-menge
      uom         = <ls>-meins
      amount      = <ls>-dmbtr
      ebeln       = <ls>-ebeln
      created_by  = <ls>-usnam
      reference   = <ls>-xblnr )
    TO ct_hist.

    IF <ls>-matnr IS NOT INITIAL.
      INSERT <ls>-matnr INTO TABLE gt_matnr.
    ENDIF.
    INSERT <ls>-bwart INTO TABLE gt_bwart.
    IF <ls>-ebeln IS NOT INITIAL.
      INSERT <ls>-ebeln INTO TABLE gt_ebeln.
    ENDIF.
  ENDLOOP.

  " Vendor mapping via EKKO # LIFNR (batched)
  IF gt_ebeln IS NOT INITIAL.
    SELECT ebeln, lifnr
      FROM ekko
      FOR ALL ENTRIES IN @gt_ebeln
      WHERE ebeln = @gt_ebeln-table_line
      INTO TABLE @DATA(lt_po_lifnr).

    LOOP AT lt_po_lifnr ASSIGNING FIELD-SYMBOL(<lp>).
      INSERT <lp>-lifnr INTO TABLE gt_lifnr.
    ENDLOOP.

    LOOP AT ct_hist ASSIGNING FIELD-SYMBOL(<h>)
         WHERE category = iv_bwart AND ebeln IS NOT INITIAL AND lifnr IS INITIAL.
      READ TABLE lt_po_lifnr ASSIGNING FIELD-SYMBOL(<pl>)
        WITH KEY ebeln = <h>-ebeln.
      IF sy-subrc = 0.
        <h>-lifnr = <pl>-lifnr.
      ENDIF.
    ENDLOOP.
  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*& Exchange Material: documents that contain BOTH 101 and 161
*&---------------------------------------------------------------------*
FORM get_exchange_history USING    iv_sdate TYPE sy-datum
                                     iv_edate TYPE sy-datum
                           CHANGING  ct_hist  TYPE ty_t_hist.

  " --- Step 1: find docs that have 101 within the range
  TYPES: BEGIN OF ty_doc_key,
           mblnr TYPE mblnr,
           mjahr TYPE mjahr,
         END OF ty_doc_key.

  DATA: lt_101  TYPE HASHED TABLE OF ty_doc_key WITH UNIQUE KEY mblnr mjahr,
        lt_161  TYPE HASHED TABLE OF ty_doc_key WITH UNIQUE KEY mblnr mjahr,
        lt_both TYPE HASHED TABLE OF ty_doc_key WITH UNIQUE KEY mblnr mjahr.

  SELECT DISTINCT s~mblnr, s~mjahr
    FROM mseg AS s
    INNER JOIN mkpf AS h
      ON h~mblnr = s~mblnr AND h~mjahr = s~mjahr
    WHERE s~bwart = '101'
      AND h~budat BETWEEN @iv_sdate AND @iv_edate
    INTO TABLE @lt_101.

  " --- Step 2: find docs that have 161 within the range
  SELECT DISTINCT s~mblnr, s~mjahr
    FROM mseg AS s
    INNER JOIN mkpf AS h
      ON h~mblnr = s~mblnr AND h~mjahr = s~mjahr
    WHERE s~bwart = '161'
      AND h~budat BETWEEN @iv_sdate AND @iv_edate
    INTO TABLE @lt_161.

  " --- Step 3: intersection # docs that contain BOTH 101 and 161
  LOOP AT lt_101 ASSIGNING FIELD-SYMBOL(<k>).
    READ TABLE lt_161 WITH KEY mblnr = <k>-mblnr mjahr = <k>-mjahr TRANSPORTING NO FIELDS.
    IF sy-subrc = 0.
      INSERT <k> INTO TABLE lt_both.
    ENDIF.
  ENDLOOP.

  IF lt_both IS INITIAL.
    RETURN. " nothing to add
  ENDIF.

  " --- Step 4: fetch only the 101/161 items from those docs
  SELECT s~mblnr, s~mjahr, s~bwart, s~werks, s~lgort, s~matnr,
         s~menge, s~meins, s~dmbtr, s~ebeln,
         h~bldat, h~budat, h~usnam, h~xblnr
    FROM mseg AS s
    INNER JOIN mkpf AS h
      ON h~mblnr = s~mblnr AND h~mjahr = s~mjahr
    FOR ALL ENTRIES IN @lt_both
    WHERE s~mblnr = @lt_both-mblnr
      AND s~mjahr = @lt_both-mjahr
      AND s~bwart IN ( '101', '161' )
    INTO TABLE @DATA(lt_mm).

  LOOP AT lt_mm ASSIGNING FIELD-SYMBOL(<ls>).
    APPEND VALUE ty_hist(
      category    = c_view_exc         " label the row as Exchange
      docno       = <ls>-mblnr
      docyear     = <ls>-mjahr
      docdate     = <ls>-bldat
      postdate    = <ls>-budat
      movetype    = <ls>-bwart
      plant       = <ls>-werks
      sloc        = <ls>-lgort
      matnr       = <ls>-matnr
      qty         = <ls>-menge
      uom         = <ls>-meins
      amount      = <ls>-dmbtr
      ebeln       = <ls>-ebeln
      created_by  = <ls>-usnam
      reference   = <ls>-xblnr )
      TO ct_hist.

    IF <ls>-matnr IS NOT INITIAL.
      INSERT <ls>-matnr INTO TABLE gt_matnr.
    ENDIF.
    INSERT <ls>-bwart INTO TABLE gt_bwart.
    IF <ls>-ebeln IS NOT INITIAL.
      INSERT <ls>-ebeln INTO TABLE gt_ebeln.
    ENDIF.
  ENDLOOP.

  " --- Step 5: vendor enrichment (via PO header)
  IF gt_ebeln IS NOT INITIAL.
    SELECT ebeln, lifnr
      FROM ekko
      FOR ALL ENTRIES IN @gt_ebeln
      WHERE ebeln = @gt_ebeln-table_line
      INTO TABLE @DATA(lt_po_lifnr).

    LOOP AT lt_po_lifnr ASSIGNING FIELD-SYMBOL(<lp>).
      INSERT <lp>-lifnr INTO TABLE gt_lifnr.
    ENDLOOP.

    LOOP AT ct_hist ASSIGNING FIELD-SYMBOL(<h>)
         WHERE category = c_view_exc AND ebeln IS NOT INITIAL AND lifnr IS INITIAL.
      READ TABLE lt_po_lifnr ASSIGNING FIELD-SYMBOL(<pl>)
           WITH KEY ebeln = <h>-ebeln.
      IF sy-subrc = 0.
        <h>-lifnr = <pl>-lifnr.
      ENDIF.
    ENDLOOP.
  ENDIF.

ENDFORM.
