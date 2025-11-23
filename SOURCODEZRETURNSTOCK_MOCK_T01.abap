*&---------------------------------------------------------------------*
*& Include          ZPG_MM_RETURN_STOCKT01
*&---------------------------------------------------------------------*
" Declaration of the ALV event handler class
CLASS:  lcl_event_handler DEFINITION DEFERRED.
DATA: gv_validation_result TYPE abap_bool VALUE abap_true.
DATA : gv_okcode TYPE sy-ucomm.
DATA gv_alv_mode TYPE c LENGTH 10.
DATA gv_grid_changed TYPE abap_bool.
"---------------------------------------------------------------------
" ALV-related data declarations
"---------------------------------------------------------------------
DATA: gs_po_edit          TYPE char1 VALUE '',
      gs_edit             TYPE char1 VALUE 'X',                 " Edit flag for ALV
      gv_mode             TYPE string,                        " ALV display mode (edit/view)
      gv_grid_title       TYPE lvc_title,                     " ALV grid title
      gs_layout           TYPE lvc_s_layo,                    " Layout settings
      gs_variant          TYPE disvariant,                    " Layout variant
      gt_exclude          TYPE ui_functions,                  " Functions to exclude from toolbar
      gt_sort             TYPE lvc_t_sort,                    " Sorting rules
      gt_filter           TYPE lvc_t_filt,                    " Filter rules
      gt_po_fieldcat      TYPE lvc_t_fcat,                    " Field catalog
      gt_gr_fieldcat      TYPE lvc_t_fcat,
      gt_inv_fieldcat     TYPE lvc_t_fcat,
      gt_receipt_fieldcat TYPE lvc_t_fcat,
      go_grid_01          TYPE REF TO cl_gui_alv_grid,        " ALV grid object ( screen 100 )
      go_event_handler_01 TYPE REF TO lcl_event_handler.      " Event handler for ALV events

"---------------------------------------------------------------------
" Error handling data declarations
"---------------------------------------------------------------------

DATA: gv_error_message  TYPE string,          " Error message content
      gv_has_error_cell TYPE abap_bool,       " Boolean: any cell has error
      gv_error_row_id   TYPE lvc_s_roid,      " Row ID with error
      gs_col_id         TYPE lvc_s_col.       " Column ID with error

" Line number where the error occurred
DATA: gv_error_line TYPE i.

" Cursor field name for header screen
DATA: gv_cursor_field_header TYPE scrfname.

" Screen 0100
TYPES : BEGIN OF ty_po_header,
          lifnr TYPE ztgsu06-lifnr,  " vendor
          bukrs TYPE ztgsu06-bukrs, " company_code
          ekorg TYPE ztgsu06-ekorg, " purchase organ
          ekgrp TYPE ztgsu06-ekgrp, " pur group
          ebeln TYPE ztgsu06-ebeln, " po_number
        END OF ty_po_header,
        ty_t_po_header TYPE STANDARD TABLE OF ty_po_header.

TYPES : BEGIN OF ty_po_item,
          po_item   TYPE ebelp,
          matnr     TYPE ztgsu06-matnr,        "Material Number
          zmate_des TYPE ztgsu06-zmate_des,    "Short Text
          bstmg     TYPE ztgsu06-bstmg,        "PO Quantity
          zunit     TYPE ztgsu06-zunit,        "UOM (##n v# tính)
          peinh     TYPE ztgsu06-peinh,        "Price Unit
          bprei     TYPE ztgsu06-bprei,        "Net Price
          matkl     TYPE ztgsu06-matkl,        "Material Group
          eindt     TYPE ztgsu06-eindt,        "Delivery Date
          zplant    TYPE ztgsu06-zplant,       "Plant
          "ZSTR_LOCA type ztgsu06-ZSTR_LOCA,    "Storage Location
          zcurrency TYPE ztgsu06-zcurrency,    "Currency
          retpo     TYPE ztgsu06-retpo,        "Returns Item (X or blank)
        END OF ty_po_item,
        ty_t_po_item TYPE STANDARD TABLE OF ty_po_item.

DATA : gs_po_header TYPE ty_po_header,
       gt_po_item   TYPE ty_t_po_item,
       gs_po_item   TYPE ty_po_item.


"Screen 0200

TYPES : BEGIN OF ty_receipt_header ,
          ebeln TYPE ztgsu06-ebeln, " po_number
          bldat TYPE ztgsu06-bldat, " document date
          budat TYPE ztgsu06-budat, " posting date
        END OF ty_receipt_header,
        ty_t_receipt_header TYPE STANDARD TABLE OF ty_receipt_header.

TYPES : ty_receipt_item   TYPE zitem_miro.
TYPES: ty_t_receipt_item TYPE STANDARD TABLE OF zitem_miro.
DATA : gs_receipt_header TYPE ty_receipt_header.
DATA : gt_receipt_item   TYPE ty_t_receipt_item,
       gs_receipt_item   TYPE ty_receipt_item.
"Screen 0300

TYPES : BEGIN OF ty_invoice_header ,
          bukrs      TYPE ztgsu06-bukrs,   " company code
          ebeln      TYPE ztgsu06-ebeln,   " po number
          bldat      TYPE ztgsu06-bldat,   " invoice date
          budat      TYPE ztgsu06-budat,   " posting date
          zamount    TYPE ztgsu06-zamount, " invoice amount
          zcurrency  TYPE ztgsu06-zcurrency,        "Currency
          tax_amount TYPE ztgsu06-tax_amount, " tax amount
          tax_code   TYPE ztgsu06-tax_code, " tax code
        END OF ty_invoice_header ,
        ty_t_invocie_header TYPE STANDARD TABLE OF   ty_invoice_header.
types: ty_invoice_item   TYPE zpayment_item,
        ty_t_invoice_item TYPE STANDARD TABLE OF zpayment_item.
TYPES : BEGIN OF ty_vendor ,
          lifnr   TYPE ztgsu06-lifnr,     "
          name    TYPE ztgsu06-zname,     " Vendor name
          street  TYPE ztgsu06-zstreet,   " Street address
          city    TYPE ztgsu06-zcity,     " City
          address TYPE ztgsu06-zaddress,  " Full address
          country TYPE ztgsu06-zcountry,  " Country
        END OF ty_vendor,
        ty_t_vendor TYPE STANDARD TABLE OF ty_vendor.

" Data object to hold vendor information
DATA : gs_vendor TYPE ty_vendor.
DATA : gs_layout_0300    TYPE lvc_s_layo,
       gt_fieldcat_0300  TYPE lvc_t_fcat,
       gs_invoice_header TYPE ty_invoice_header,
       gt_invoice_item   TYPE ty_t_invoice_item,
       gs_invoice_item   TYPE ty_invoice_item.
"BI#n nh# t#m
DATA gv_ebeln_loaded_300 TYPE ebeln.
DATA gv_po_loaded_200 TYPE ebeln.
DATA gv_po_loaded_100 TYPE ebeln.
"Screen 0400
TYPES : BEGIN OF ty_infor_header,
          bukrs TYPE ztgsu06-bukrs,   " company code
          lifnr TYPE ztgsu06-lifnr,   " supplier
        END OF ty_infor_header,
        ty_t_infor_item TYPE STANDARD TABLE OF ty_infor_header.
DATA : gs_payment_header        TYPE ty_infor_header.

 "Screen 0500
TYPES: BEGIN OF ty_po_hist,
         ebeln      TYPE ebeln,
         ebelp      TYPE ebelp,
         ernam      TYPE ernam,
         erdat      TYPE erdat,
         lifnr      TYPE lifnr,
         matnr      TYPE matnr,
         zmate_des  TYPE ekpo-txz01,
         bstmg      TYPE ekpo-menge,
         zunit      TYPE ekpo-meins,
         bprei      TYPE ekpo-netpr,
         zcurrency  TYPE ekko-waers,
         zplant     TYPE ekpo-werks,
       END OF ty_po_hist.

DATA: gt_po_hist       TYPE STANDARD TABLE OF ty_po_hist,
      gs_po_hist       TYPE ty_po_hist,
      gt_hist_fieldcat TYPE lvc_t_fcat.

SELECTION-SCREEN BEGIN OF BLOCK block1 WITH FRAME TITLE TEXT-001.
  PARAMETERS: r_crePO  RADIOBUTTON GROUP grp1 DEFAULT 'X', "Create Purchase Order
              r_postGR RADIOBUTTON GROUP grp1,             "Post Good Receipt
              r_postIv RADIOBUTTON GROUP grp1,             "Post Invoice
              r_payI4  RADIOBUTTON GROUP grp1,             "Payment Info
              r_hist   RADIOBUTTON GROUP grp1,             "Purchase Order History
              p_hist   TYPE lifnr MODIF ID HST.        "Vendor for history
SELECTION-SCREEN END OF BLOCK block1.
