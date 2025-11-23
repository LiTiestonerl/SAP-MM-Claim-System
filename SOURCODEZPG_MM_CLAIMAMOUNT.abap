***********************************************************************************
* Program name       : Claim Management System
* Type               : Online
* Tcode              : ZCLAM
* Description        : Claim Management System
***********************************************************************************
*                          Modification Log
*----------------------------------------------------------------------------------
*  No      CSR/Issue No.       Date          Authors       Description
***********************************************************************************
*Comment:
*
***********************************************************************************

REPORT ZPG_MM_CLAIMAMOUNT.


" Declare global data: internal tables, structures, constants...
INCLUDE  zpg_mm_claimamountt01.
" Define ALV handler class and event handling (e.g. toolbar click, user command)
INCLUDE  zpg_mm_claimamountc01.
" Utility FORM routines: e.g. formatting, error message, popup confirmation , main process
INCLUDE  zpg_mm_claimamountf01.
" Get descriptive names
INCLUDE  zpg_mm_claimamountf02.
" Validate input data
INCLUDE  zpg_mm_claimamountf03.
" Screen 100 input handling
INCLUDE  zpg_mm_claimamounti01.
" Screen 100 output handling
INCLUDE  zpg_mm_claimamounto01.
" Build ALV
INCLUDE  zpg_mm_claimamountf00.

*INITIALIZATION.
*  PERFORM fetch_data.

START-OF-SELECTION.
  CALL SCREEN 100. " call screen 100
