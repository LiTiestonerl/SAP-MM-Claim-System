*&---------------------------------------------------------------------*
*& Report ZRETURNSTOCK
*&---------------------------------------------------------------------*
REPORT ZRETURNSTOCK.

* Header/item for screens
INCLUDE ZRETURNSTOCKT01.

* Button alv evens handle
INCLUDE ZRETURNSTOCKC01.

* Alv grid
INCLUDE ZRETURNSTOCKF00.

* Main program, bapi handle
INCLUDE ZRETURNSTOCKF01.

* Get and validate data
INCLUDE ZRETURNSTOCKF02.

* Validate process
INCLUDE ZRETURNSTOCKF03.

* UI control, status screens
INCLUDE ZRETURNSTOCKO01.

* User command
INCLUDE ZRETURNSTOCKI01.

START-OF-SELECTION.
  PERFORM main_program.
