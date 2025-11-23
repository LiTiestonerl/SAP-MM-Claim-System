*&---------------------------------------------------------------------*
*& Report ZGISCRAPPINGS
*&---------------------------------------------------------------------*
REPORT zgiscrappings.


TYPES: BEGIN OF zs_scrap_input,
         matnr     TYPE matnr,        " Mã v#t t#
         zmate_des TYPE maktx,        " Mô t# ng#n v#t t#
         ok        TYPE abap_bool,    " Checkbox OK
         zstock    TYPE labst,        " S# l##ng
         zunit     TYPE meins,        " ##n v#
         zstr_loca TYPE lgort_d,      " Kho
         insmk     TYPE insmk,        " Stock type
         zplant    TYPE werks_d,      " Nhà máy
       END OF zs_scrap_input.

INCLUDE zgiscrapping_bapi.


DATA: go_container TYPE REF TO cl_gui_custom_container,
      go_grid      TYPE REF TO cl_gui_alv_grid,
      lt_data      TYPE STANDARD TABLE OF zs_scrap_input,
      lt_fieldcat  TYPE lvc_t_fcat,
      ls_data      TYPE zs_scrap_input,
      ev_success   TYPE abap_bool,
      ev_message   TYPE string,
      ev_matdoc    TYPE bapi2017_gm_head_ret-mat_doc.

SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-001.
  PARAMETERS: p_bldat  TYPE ztgsu06-bldat,
              p_budat  TYPE ztgsu06-budat,
              p_slipno TYPE ztgsu06-z_slipno,
              p_bktxt  TYPE ztgsu06-bktxt.
SELECTION-SCREEN END OF BLOCK b1.

START-OF-SELECTION.

  CALL FUNCTION 'post_scrapping'
    EXPORTING
      iv_bldat = p_bldat
      iv_budat = p_budat
      iv_bktxt = p_bktxt
    CHANGING
      ev_success = ev_success
      ev_message = ev_message
      ev_matdoc  = ev_matdoc.

  IF ev_success = abap_true.
    CLEAR lt_data.
    DO 10 TIMES.
      CLEAR ls_data.
      APPEND ls_data TO lt_data.
    ENDDO.
    CALL SCREEN 100.
  ELSE.
    MESSAGE ev_message TYPE 'E'.
  ENDIF.

*&---------------------------------------------------------------------*
*& FORM: set_field_catalog
*&---------------------------------------------------------------------*
FORM set_field_catalog.
  CLEAR lt_fieldcat.

  APPEND VALUE #( fieldname = 'MATNR'     seltext = 'Material Number'  scrtext_l = 'Material Number'     edit = 'X' ) TO lt_fieldcat.
  APPEND VALUE #( fieldname = 'ZIMATE_DES' seltext = 'Material Short Text' scrtext_l = 'Material Short Text'  edit = 'X' ) TO lt_fieldcat.
  APPEND VALUE #( fieldname = 'OK'        seltext = 'OK'      scrtext_l = 'OK'              checkbox = 'X' edit = 'X' ) TO lt_fieldcat.
  APPEND VALUE #( fieldname = 'ZSTOCK'    seltext = 'Qty in UNE'  scrtext_l = 'Qty in UNE'          edit = 'X' ) TO lt_fieldcat.
  APPEND VALUE #( fieldname = 'ZUNIT'     seltext = 'EUn'     scrtext_l = 'EUn'              edit = 'X' ) TO lt_fieldcat.
  APPEND VALUE #( fieldname = 'ZSTR_LOCA' seltext = 'Sloc'    scrtext_l = 'Sloc'              edit = 'X' ) TO lt_fieldcat.
  APPEND VALUE #( fieldname = 'INSMK'     seltext = 'Stock Type'  scrtext_l = 'Stock Type'          edit = 'X' ) TO lt_fieldcat.
  APPEND VALUE #( fieldname = 'ZPLANT'    seltext = 'Plant'     scrtext_l = 'Plant'            edit = 'X' ) TO lt_fieldcat.
ENDFORM.

*&---------------------------------------------------------------------*
*& Module STATUS_0100 OUTPUT
*&---------------------------------------------------------------------*
MODULE status_0100 OUTPUT.
  IF go_container IS INITIAL.
    CREATE OBJECT go_container
      EXPORTING
        container_name = 'ALV_CONTAINER'.
    CREATE OBJECT go_grid
      EXPORTING
        i_parent = go_container.
    PERFORM set_field_catalog.
    DATA(ls_layout) = VALUE lvc_s_layo( edit = 'X' ).
    CALL METHOD go_grid->set_table_for_first_display
      EXPORTING
        is_layout       = ls_layout
      CHANGING
        it_outtab       = lt_data
        it_fieldcatalog = lt_fieldcat.
  ENDIF.
ENDMODULE.
