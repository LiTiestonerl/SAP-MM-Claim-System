*&---------------------------------------------------------------------*
*& Include  ZPG_MM_DOC_HISTORY_T01
*&---------------------------------------------------------------------*

TYPE-POOLS vrm.

CONSTANTS:
  c_view_gi  TYPE char3 VALUE 'GI',
  c_view_ret TYPE char3 VALUE 'RET',
  c_view_clm TYPE char3 VALUE 'CLM',
  c_view_101  TYPE char3 VALUE '101',
  c_view_122  TYPE char3 VALUE '122',
  c_view_exc  TYPE char3 VALUE 'EXC'.


" Unified output row (one SALV for all)
TYPES: BEGIN OF ty_hist,
         category      TYPE c LENGTH 3,        " GI/RET/CLM
         docno         TYPE char20,            " MBLNR/BELNR
         docyear       TYPE gjahr,
         docdate       TYPE bldat,
         postdate      TYPE budat,
         movetype      TYPE bwart,             " blank for CLM
         movetext      TYPE char30,
         plant         TYPE werks_d,
         sloc          TYPE lgort_d,
         matnr         TYPE matnr,
         mat_text      TYPE maktx,
         qty           TYPE menge_d,
         uom           TYPE meins,
         amount        TYPE wrbtr,             " use for CLM or MSEG-DMBTR
         currency      TYPE waers,
         ebeln         TYPE ebeln,
         lifnr         TYPE lifnr,
         vendor_name   TYPE name1_gp,
         created_by    TYPE usnam,
         reference     TYPE xblnr,
         reversal      TYPE c LENGTH 1,        " 'X' if reversal
       END OF ty_hist.

TYPES ty_t_hist TYPE STANDARD TABLE OF ty_hist WITH DEFAULT KEY.

DATA: gt_hist TYPE ty_t_hist,
      gv_ok     TYPE abap_bool,
      go_salv   TYPE REF TO cl_salv_table.


" Ranges/keys for batched text lookups
TYPES: ty_t_matnr TYPE HASHED TABLE OF matnr WITH UNIQUE KEY table_line,
       ty_t_bwart TYPE HASHED TABLE OF bwart WITH UNIQUE KEY table_line,
       ty_t_lifnr TYPE HASHED TABLE OF lifnr WITH UNIQUE KEY table_line,
       ty_t_ebeln TYPE HASHED TABLE OF ebeln WITH UNIQUE KEY table_line.

DATA: gt_matnr TYPE ty_t_matnr,
      gt_bwart TYPE ty_t_bwart,
      gt_lifnr TYPE ty_t_lifnr,
      gt_ebeln TYPE ty_t_ebeln.
