*&---------------------------------------------------------------------*
*& Include          ZGISCRAPPING_BAPI
*&---------------------------------------------------------------------*
FORM post_scrapping
    USING
      it_scrap   TYPE table of zs_scrap_input
      iv_bldat   TYPE ztgsu06-bldat
      iv_budat   TYPE ztgsu06-budat
      iv_bktxt   TYPE ztgsu06-bktxt
    CHANGING
      ev_success TYPE abap_bool
      ev_message TYPE string
      ev_matdoc  TYPE bapi2017_gm_head_ret-mat_doc.

  CALL FUNCTION 'post_scrapping'
    EXPORTING
      it_scrap = it_scrap
      iv_bldat = iv_bldat
      iv_budat = iv_budat
      iv_bktxt = iv_bktxt
    CHANGING
      ev_success = ev_success
      ev_message = ev_message
      ev_matdoc  = ev_matdoc.

ENDFORM.
