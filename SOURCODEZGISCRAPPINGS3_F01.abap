*&---------------------------------------------------------------------*
*& Include          ZGISCRAPPINGS2_F01
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*& Form post_bapi
*&---------------------------------------------------------------------*
FORM post_bapi .
  DATA: lt_return   TYPE TABLE OF bapiret2,
        ls_return   TYPE bapiret2,
        lv_mat_doc  TYPE bapi2017_gm_head_ret-mat_doc,
        lv_doc_year TYPE bapi2017_gm_head_ret-doc_year.

  DATA: ls_header TYPE bapi2017_gm_head_01,
        lt_item   TYPE TABLE OF bapi2017_gm_item_create,
        ls_item   TYPE bapi2017_gm_item_create,
        ls_code   TYPE bapi2017_gm_code.

  DATA: lv_print_type       TYPE c LENGTH 4,
        lv_print_after_post TYPE abap_bool VALUE abap_false.

  lv_print_type = gs_header-print_type.
  IF lv_print_type = 'IND' OR lv_print_type = 'INSP' OR lv_print_type = 'COL'.
    lv_print_after_post = abap_true.
  ENDIF.

  ls_header-pstng_date = COND dats( WHEN gs_header-posting_date IS INITIAL
                                    THEN sy-datum ELSE gs_header-posting_date ).
  ls_header-doc_date   = COND dats( WHEN gs_header-doc_date    IS INITIAL
                                    THEN sy-datum ELSE gs_header-doc_date ).
  ls_header-pr_uname   = sy-uname.

  CLEAR ls_code.
  ls_code-gm_code = '03'.  " GI

  CLEAR lt_item.
  LOOP AT gt_item INTO gs_item.
    IF gs_item-material_number IS INITIAL
    AND gs_item-plant          IS INITIAL
    AND gs_item-sloc           IS INITIAL
    AND gs_item-qty_in_une     IS INITIAL.
      CONTINUE.
    ENDIF.

    CLEAR ls_item.
    ls_item-material   = gs_item-material_number.  " MATNR #ã chu#n hoá # n#i khác (n#u c#n)
    ls_item-plant      = gs_item-plant.
    ls_item-stge_loc   = gs_item-sloc.
    ls_item-move_type  = '551'.                    " Scrapping (Unrestricted)
    ls_item-entry_qnt  = gs_item-qty_in_une.
    ls_item-entry_uom  = gs_item-eun.
    APPEND ls_item TO lt_item.
  ENDLOOP.

  IF lt_item IS INITIAL.
    MESSAGE ID 'ZGISCRAPPINGS_MSG' TYPE 'S' NUMBER '000' DISPLAY LIKE 'E'.
    RETURN.
  ENDIF.

  "=== G#i BAPI ===
  CALL FUNCTION 'BAPI_GOODSMVT_CREATE'
    EXPORTING
      goodsmvt_header  = ls_header
      goodsmvt_code    = ls_code
    IMPORTING
      materialdocument = lv_mat_doc
      matdocumentyear  = lv_doc_year
    TABLES
      goodsmvt_item    = lt_item
      return           = lt_return.

  READ TABLE lt_return WITH KEY type = 'A' TRANSPORTING NO FIELDS.
  IF sy-subrc = 0 OR lv_mat_doc IS INITIAL.
    LOOP AT lt_return INTO ls_return WHERE type = 'A' OR type = 'E'.
      MESSAGE ID ls_return-id TYPE ls_return-type NUMBER ls_return-number
              WITH ls_return-message_v1 ls_return-message_v2
                   ls_return-message_v3 ls_return-message_v4.
    ENDLOOP.
    RETURN.
  ENDIF.

  CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
    EXPORTING
      wait = abap_true.

  "MESSAGE |Goods Issue posted successfully: { lv_mat_doc }/{ lv_doc_year }|
  "        TYPE 'S'.
  MESSAGE ID 'ZGISCRAPPINGS_MSG' TYPE 'S' NUMBER '014'.
  DATA: lv_mblnr TYPE c LENGTH 10,
        lv_mjahr TYPE c LENGTH 4.

  lv_mblnr = |{ lv_mat_doc  ALPHA = IN }|.
  lv_mjahr = |{ lv_doc_year ALPHA = IN }|.

  DATA(lv_opened) = abap_false.
  TRY.
      CALL FUNCTION 'MIGO_DIALOG'
        EXPORTING
          i_action = 'A03'     " Display
          i_refdoc = 'R02'     " Material Document
          i_mblnr  = lv_mblnr
          i_mjahr  = lv_mjahr.
      lv_opened = abap_true.
    CATCH cx_sy_dyn_call_illegal_func cx_root.
      lv_opened = abap_false.
  ENDTRY.

  IF lv_opened = abap_false.
    SET PARAMETER ID 'MBN' FIELD lv_mblnr.   " Material Document
    SET PARAMETER ID 'MJA' FIELD lv_mjahr.   " Year
    CALL TRANSACTION 'MIGO' AND SKIP FIRST SCREEN.
  ENDIF.
ENDFORM.
