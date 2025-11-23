*&---------------------------------------------------------------------*
*& Report ZINSERT_DATA_ZTABLE
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT ZINSERT_DATA_ZTABLE.

DATA: lt_data TYPE TABLE OF ZTGSU06,
      ls_data TYPE ZTGSU06.

SELECT MANDT,
       BUKRS,
       BLDAT,
       BUDAT,
       MATNR,
       MATKL,
       LIFNR,
       EBELN,
       EKORG,
       ZUSER_ID,
       ZAMOUNT,
       ZCACU_TAX,
       ZCURRENCY,
       ZNAME,
       ZADDRESS,
       ZSTREET,
       ZCITY,
       ZCOUNTRY,
       ZSHORT_TXT,
       SHKZG,
       GSBER,
       KOSTL,
       Z_SLIPNO,
       BKTXT,
       TXZ01,
       S_MEINS,
       SOBKZ,
       PEINH,
       RETPO,
       WAERS,
       INSMK,
       LFIMG,
       ERDAT,
       ERZET,
       ERNAM,
       AEDAT,
       AEZET,
       AENAM
  INTO TABLE @lt_data
  FROM ZTGSU06.

IF sy-subrc = 0.
  LOOP AT lt_data INTO ls_data.
    WRITE: / ls_data-MANDT, ls_data-BUKRS, ls_data-BLDAT, ls_data-BUDAT, ls_data-MATNR.
  ENDLOOP.
ELSE.
  WRITE: 'No data found.'.
ENDIF.
