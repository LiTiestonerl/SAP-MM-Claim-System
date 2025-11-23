*&---------------------------------------------------------------------*
*& Include          ZPG_MM_CLAIMAMOUNTT01
*&---------------------------------------------------------------------*
TYPE-POOLS: lvc.
" Declaration of the ALV event handler class
CLASS:  lcl_event_handler DEFINITION DEFERRED.
" Variable to store result of validation (True/False)
DATA:   lv_validation_result TYPE abap_bool VALUE abap_true.
" Variable for OK code (used in PAI for user command)
DATA:   gv_okcode TYPE sy-ucomm.


"---------------------------------------------------------------------
" Header structure definition and declaration
"---------------------------------------------------------------------
TYPES : BEGIN OF ty_header,
          transaction_type TYPE ztgsu06_test-transaction_type,    " Transaction type (e.g., KR, KG)
          company_code     TYPE ztgsu06-bukrs,                    " Company code
          supplier         TYPE ztgsu06-lifnr,                    " Vendor code
          invoice_date     TYPE bseg-h_bldat,                     " Invoice date
          posting_date     TYPE bseg-h_budat,                     " Posting date
          amount           TYPE ztgsu06-zamount,                  " Document total amount
          currency         TYPE ztgsu06-zcurrency,                " Currency
          balance          TYPE string,                  " Balance
        END OF ty_header,
        ty_t_header TYPE STANDARD TABLE OF ty_header.
DATA : gs_header TYPE ty_header.

"---------------------------------------------------------------------
" Item structure definition and declaration
"---------------------------------------------------------------------
TYPES : BEGIN OF ty_item,
          gl_account      TYPE ztgsu06-gl_account,           " G/L account number
          short_text      TYPE ztgsu06-zshort_txt,           " Line item text
          dc_indicator    TYPE ztgsu06_test-dc_indicator,    " Debit/Credit indicator (S/H)
          amount_doc_curr TYPE p DECIMALS 2,              " Amount in document currency
          loc_doc_curr    TYPE p DECIMALS 2,              " Local currency amount
          business_area   TYPE ztgsu06_test-business_area,   " Business area
          cost_center     TYPE ztgsu06_test-cost_center,     " Cost center
          currency        TYPE waers,                        " Currency
        END OF ty_item,
        ty_t_item TYPE STANDARD TABLE OF ty_item.

" Data objects for item line and table
DATA : gs_item TYPE ty_item,
       gt_item TYPE ty_t_item.
"---------------------------------------------------------------------
" Vendor structure definition and declaration
"---------------------------------------------------------------------
TYPES : BEGIN OF ty_vendor ,
          name    TYPE ztgsu06-zname,     " Vendor name
          street  TYPE ztgsu06-zstreet,   " Street address
          city    TYPE ztgsu06-zcity,     " City
          address TYPE ztgsu06-zaddress,  " Full address
          country TYPE ztgsu06-zcountry,  " Country
        END OF ty_vendor,
        ty_t_vendor TYPE STANDARD TABLE OF ty_vendor.

" Data object to hold vendor information
DATA : gs_vendor TYPE ty_vendor.

"---------------------------------------------------------------------
" Account structures and working data
"---------------------------------------------------------------------
" Structures from custom Z/Y tables for GL and transaction data
DATA: gs_gl_account TYPE ygl_account,
      gs_acc_tran   TYPE yaccount_trans.
"account
TYPES : ty_account_trans   TYPE yaccount_trans,
        ty_t_account_trans TYPE STANDARD TABLE OF yaccount_trans.
" Data objects for transaction details
DATA  : gs_account_trans TYPE ty_account_trans,
        gt_account_trans TYPE ty_t_account_trans.

"---------------------------------------------------------------------
" ALV-related data declarations
"---------------------------------------------------------------------
DATA: gs_edit             TYPE char1 VALUE '',                 " Edit flag for ALV
      gv_mode             TYPE string,                        " ALV display mode (edit/view)
      gv_grid_title       TYPE lvc_title,                     " ALV grid title
      gs_layout           TYPE lvc_s_layo,                    " Layout settings
      gs_variant          TYPE disvariant,                    " Layout variant
      gt_exclude          TYPE ui_functions,                  " Functions to exclude from toolbar
      gt_sort             TYPE lvc_t_sort,                    " Sorting rules
      gt_filter           TYPE lvc_t_filt,                    " Filter rules
      gt_fieldcat         TYPE lvc_t_fcat,                    " Field catalog
      go_grid_01          TYPE REF TO cl_gui_alv_grid,        " ALV grid object
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
