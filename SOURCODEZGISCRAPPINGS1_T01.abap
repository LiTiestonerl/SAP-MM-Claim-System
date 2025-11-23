*&---------------------------------------------------------------------*
*& Include          ZGISCRAPPINGS1_T01
*&---------------------------------------------------------------------*
" Declaration of the ALV event handler class
CLASS:  lcl_event_handler DEFINITION DEFERRED.
DATA: lv_validation_result TYPE abap_bool VALUE abap_true.
DATA : gv_okcode TYPE sy-ucomm.

"HEADER
TYPES: BEGIN OF ty_header,
         doc_date      TYPE zgiheader-bldat,
         material_slip TYPE zgiheader-slip_no,
         posting_date  TYPE zgiheader-budat,
         head_text     TYPE zgiheader-bktxt,
         print_type    TYPE char4,
       END OF ty_header,
       ty_t_header TYPE STANDARD TABLE OF ty_header.


" ITEM
TYPES: BEGIN OF ty_item,
         plant               TYPE zgiitem-plant,
         sloc                TYPE zgiitem-sloc,
         material_number     TYPE zgiitem-material_number,
         material_short_text TYPE zgiitem-material_short_text,
         stock_type          TYPE zgiitem-stock_type,
         qty_in_une          TYPE zgiitem-qty_in_une,
         eun                 TYPE zgiitem-eun,
       END OF ty_item,
       ty_t_item TYPE STANDARD TABLE OF ty_item.

DATA : gs_header TYPE ty_header,
       gt_item   TYPE ty_t_item,
       gs_item   TYPE ty_item.


"---------------------------------------------------------------------
" ALV-related data declarations
"---------------------------------------------------------------------
DATA: gv_grid_title       TYPE lvc_title,                     " ALV grid title
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
