*&---------------------------------------------------------------------*
*& Report ZHISTORY
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT ZHISTORY.

INCLUDE zpg_mm_doc_history_t01.   " Types, constants, globals
INCLUDE zpg_mm_doc_history_s01.   " Selection-screen & dropdown fill
INCLUDE zpg_mm_doc_history_f00.   " Data fetchers (GI/RET/CLM)
INCLUDE zpg_mm_doc_history_o01.   " SALV display
INCLUDE zpg_mm_doc_history_i01.   " Helpers (messages, lookups)

START-OF-SELECTION.
