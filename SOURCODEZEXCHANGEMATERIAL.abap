*&---------------------------------------------------------------------*
*& Report ZRETURNSTOCK
*&---------------------------------------------------------------------*
REPORT ZEXCHANGEMATERIAL.

* Header/item for screens
INCLUDE ZEXCHANGEMATERIALT01.

* Button alv evens handle
INCLUDE ZEXCHANGEMATERIALC01.

* Alv grid
INCLUDE ZEXCHANGEMATERIALF00.

* Main program, bapi handle
INCLUDE ZEXCHANGEMATERIALF01.

* Get and validate data
INCLUDE ZEXCHANGEMATERIALF02.

* Validate process
INCLUDE ZEXCHANGEMATERIALF03.

* UI control, status screens
INCLUDE ZEXCHANGEMATERIALO01.
* User command
INCLUDE ZEXCHANGEMATERIALI01.

START-OF-SELECTION.
  PERFORM main_program.
