*&---------------------------------------------------------------------*
*& Include  ZPG_MM_DOC_HISTORY_I01
*& Helpers (no IS ASSIGNED on data objs)
*&---------------------------------------------------------------------*

FORM fill_view_listbox.
  DATA: lt_values TYPE vrm_values,
        ls_value  TYPE vrm_value.

  CLEAR lt_values.

  ls_value-key  = c_view_gi.
  ls_value-text = 'GI Scrapping'.
  APPEND ls_value TO lt_values.

  ls_value-key  = c_view_ret.
  ls_value-text = 'Return Stock (161)'.
  APPEND ls_value TO lt_values.

  ls_value-key  = c_view_clm.
  ls_value-text = 'Claim Amount (Credit Memo)'.
  APPEND ls_value TO lt_values.

  ls_value-key  = c_view_101.
  ls_value-text = 'Good Receipt (101)'.
  APPEND ls_value TO lt_values.

  ls_value-key  = c_view_122.
  ls_value-text = 'Return Delivery (122)'.
  APPEND ls_value TO lt_values.

  ls_value-key  = c_view_exc.
  ls_value-text = 'Exchange Material (101/161)'.
  APPEND ls_value TO lt_values.

  CALL FUNCTION 'VRM_SET_VALUES'
    EXPORTING
      id     = 'P_VIEW'
      values = lt_values.

  IF p_view IS INITIAL.
    p_view = c_view_gi.
  ENDIF.
ENDFORM.



FORM validate_dates USING    iv_bldat TYPE bldat
                             iv_budat TYPE budat
                    CHANGING ev_ok    TYPE abap_bool.
  ev_ok = abap_true.

  IF iv_bldat IS INITIAL AND iv_budat IS INITIAL.
    MESSAGE 'Fill Document Date or Posting Date' TYPE 'S' DISPLAY LIKE 'E'.
    ev_ok = abap_false.
    RETURN.
  ENDIF.

  IF iv_bldat IS NOT INITIAL AND iv_budat IS NOT INITIAL AND iv_bldat > iv_budat.
    MESSAGE 'Document Date cannot be greater than Posting Date' TYPE 'S' DISPLAY LIKE 'E'.
    ev_ok = abap_false.
    RETURN.
  ENDIF.
ENDFORM.


FORM enrich_texts CHANGING ct_hist TYPE ty_t_hist.

  FIELD-SYMBOLS: <h> TYPE ty_hist.

  " Build unique keys and normalize MATNR/LIFNR
  DATA: lt_matnr TYPE STANDARD TABLE OF matnr WITH DEFAULT KEY,
        lt_lifnr TYPE STANDARD TABLE OF lifnr WITH DEFAULT KEY,
        lt_bwart TYPE STANDARD TABLE OF bwart WITH DEFAULT KEY.

  DATA lv_matnr TYPE matnr.
  DATA lv_lifnr TYPE lifnr.
  DATA lv_bwart TYPE bwart.

  LOOP AT ct_hist ASSIGNING <h>.
    IF <h>-matnr IS NOT INITIAL.
      lv_matnr = <h>-matnr.
      CALL FUNCTION 'CONVERSION_EXIT_MATN1_INPUT'
        EXPORTING input  = lv_matnr
        IMPORTING output = lv_matnr.
      <h>-matnr = lv_matnr.
      APPEND lv_matnr TO lt_matnr.
    ENDIF.

    IF <h>-lifnr IS NOT INITIAL.
      lv_lifnr = <h>-lifnr.
      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
        EXPORTING input  = lv_lifnr
        IMPORTING output = lv_lifnr.
      <h>-lifnr = lv_lifnr.
      APPEND lv_lifnr TO lt_lifnr.
    ENDIF.

    IF <h>-movetype IS NOT INITIAL.
      lv_bwart = <h>-movetype.
      APPEND lv_bwart TO lt_bwart.
    ENDIF.
  ENDLOOP.

  SORT lt_matnr. DELETE ADJACENT DUPLICATES FROM lt_matnr.
  SORT lt_lifnr. DELETE ADJACENT DUPLICATES FROM lt_lifnr.
  SORT lt_bwart. DELETE ADJACENT DUPLICATES FROM lt_bwart.

  " 1) Material texts (sy-langu; fallback any)
  DATA lt_makt TYPE STANDARD TABLE OF makt WITH DEFAULT KEY.
  IF lt_matnr IS NOT INITIAL.
    SELECT matnr, maktx
      FROM makt
      FOR ALL ENTRIES IN @lt_matnr
      WHERE matnr = @lt_matnr-table_line
        AND spras = @sy-langu
      INTO TABLE @lt_makt.

    DATA lt_missing_mat TYPE STANDARD TABLE OF matnr WITH DEFAULT KEY.
    LOOP AT lt_matnr INTO lv_matnr.
      READ TABLE lt_makt WITH KEY matnr = lv_matnr TRANSPORTING NO FIELDS.
      IF sy-subrc <> 0.
        APPEND lv_matnr TO lt_missing_mat.
      ENDIF.
    ENDLOOP.
    IF lt_missing_mat IS NOT INITIAL.
      SELECT matnr, maktx
        FROM makt
        FOR ALL ENTRIES IN @lt_missing_mat
        WHERE matnr = @lt_missing_mat-table_line
        INTO TABLE @DATA(lt_makt_any).
      APPEND LINES OF lt_makt_any TO lt_makt.
      SORT lt_makt BY matnr. DELETE ADJACENT DUPLICATES FROM lt_makt COMPARING matnr.
    ENDIF.
  ENDIF.

  " 2) Movement texts (sy-langu; fallback 'E')
  DATA lt_t156t TYPE STANDARD TABLE OF t156t WITH DEFAULT KEY.
  IF lt_bwart IS NOT INITIAL.
    SELECT bwart, btext
      FROM t156t
      FOR ALL ENTRIES IN @lt_bwart
      WHERE bwart = @lt_bwart-table_line
        AND spras = @sy-langu
      INTO TABLE @lt_t156t.

    DATA lt_missing_b TYPE STANDARD TABLE OF bwart WITH DEFAULT KEY.
    LOOP AT lt_bwart INTO lv_bwart.
      READ TABLE lt_t156t WITH KEY bwart = lv_bwart TRANSPORTING NO FIELDS.
      IF sy-subrc <> 0.
        APPEND lv_bwart TO lt_missing_b.
      ENDIF.
    ENDLOOP.
    IF lt_missing_b IS NOT INITIAL.
      SELECT bwart, btext
        FROM t156t
        FOR ALL ENTRIES IN @lt_missing_b
        WHERE bwart = @lt_missing_b-table_line
          AND spras = 'E'
        INTO TABLE @DATA(lt_t156t_e).
      APPEND LINES OF lt_t156t_e TO lt_t156t.
      SORT lt_t156t BY bwart. DELETE ADJACENT DUPLICATES FROM lt_t156t COMPARING bwart.
    ENDIF.
  ENDIF.

  " 3) Vendor names
  DATA lt_lfa1 TYPE STANDARD TABLE OF lfa1 WITH DEFAULT KEY.
  IF lt_lifnr IS NOT INITIAL.
    SELECT lifnr, name1
      FROM lfa1
      FOR ALL ENTRIES IN @lt_lifnr
      WHERE lifnr = @lt_lifnr-table_line
      INTO TABLE @lt_lfa1.
  ENDIF.

  " Write back + per-row fallback for movement text
  FIELD-SYMBOLS: <m> TYPE makt, <t> TYPE t156t, <v> TYPE lfa1.
  LOOP AT ct_hist ASSIGNING <h>.
    IF <h>-matnr IS NOT INITIAL.
      READ TABLE lt_makt ASSIGNING <m> WITH KEY matnr = <h>-matnr.
      IF sy-subrc = 0.
        <h>-mat_text = <m>-maktx.
      ELSE.
        SELECT SINGLE maktx FROM makt INTO @<h>-mat_text
          WHERE matnr = @<h>-matnr AND spras = @sy-langu.
        IF sy-subrc <> 0.
          SELECT SINGLE maktx FROM makt INTO @<h>-mat_text
            WHERE matnr = @<h>-matnr.
        ENDIF.
      ENDIF.
    ENDIF.

    IF <h>-movetype IS NOT INITIAL.
      READ TABLE lt_t156t ASSIGNING <t> WITH KEY bwart = <h>-movetype.
      IF sy-subrc = 0.
        <h>-movetext = <t>-btext.
      ELSE.
        " Per-row fallback: first sy-langu, then 'E'
        SELECT SINGLE btext FROM t156t INTO @<h>-movetext
          WHERE bwart = @<h>-movetype AND spras = @sy-langu.
        IF sy-subrc <> 0.
          SELECT SINGLE btext FROM t156t INTO @<h>-movetext
            WHERE bwart = @<h>-movetype AND spras = 'E'.
        ENDIF.
      ENDIF.
    ENDIF.

    IF <h>-lifnr IS NOT INITIAL.
      READ TABLE lt_lfa1 ASSIGNING <v> WITH KEY lifnr = <h>-lifnr.
      IF sy-subrc = 0.
        <h>-vendor_name = <v>-name1.
      ELSE.
        SELECT SINGLE name1 FROM lfa1 INTO @<h>-vendor_name
          WHERE lifnr = @<h>-lifnr.
      ENDIF.
    ENDIF.
  ENDLOOP.

ENDFORM.
