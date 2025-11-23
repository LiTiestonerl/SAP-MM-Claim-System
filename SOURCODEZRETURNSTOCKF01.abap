*&---------------------------------------------------------------------*
*& Include          ZPG_MM_RETURN_STOCKF01
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*& Form main_program
*&---------------------------------------------------------------------*
*& Main Program
*&---------------------------------------------------------------------*
FORM main_program .
  IF r_crePO = 'X'.
    CALL SCREEN 100.
  ELSEIF r_postGR = 'X'.
    CALL SCREEN 200.
  ELSEIF r_postIv = 'X'.
    CALL SCREEN 300.
  ELSEIF r_hist = 'X'.
    PERFORM get_po_history.
    IF gt_po_hist IS NOT INITIAL.          "only open when we actually have data
      CALL SCREEN 600.
    ENDIF.
  ELSEIF r_mdoc = 'X'.                    " NEW
    PERFORM get_material_docs.
    IF gt_matdoc IS NOT INITIAL.
      CALL SCREEN 600.                    " reuse same ALV screen
    ENDIF.
  ELSE.
    CALL SCREEN 400.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form post_bapi
*&---------------------------------------------------------------------*
FORM post_bapi .

  "=== Khai báo d# li#u ===
  DATA: ls_poheader    TYPE bapimepoheader,
        ls_poheaderx   TYPE bapimepoheaderx,
        lt_poitem      TYPE STANDARD TABLE OF bapimepoitem,
        lt_poitemx     TYPE STANDARD TABLE OF bapimepoitemx,
        lt_poschedule  TYPE STANDARD TABLE OF bapimeposchedule,
        lt_poschedulex TYPE STANDARD TABLE OF bapimeposchedulx,
        lt_pocond      TYPE STANDARD TABLE OF bapimepocond,
        lt_pocondx     TYPE STANDARD TABLE OF bapimepocondx,
        lt_return      TYPE STANDARD TABLE OF bapiret2,
        ls_return      TYPE bapiret2.

  DATA: lv_po_number TYPE ebeln,
        lv_success   TYPE abap_bool VALUE abap_true,
        lv_testrun   TYPE c VALUE space,
        lv_curr      TYPE waers VALUE 'USD',
        lv_cnt       TYPE i,
        lv_itemno    TYPE ebelp,
        lv_matnr     TYPE matnr,
        lv_lifnr     TYPE lifnr.

  "=== Ki#m tra d# li#u Header ===
  IF gs_po_header-lifnr IS INITIAL
  OR gs_po_header-bukrs IS INITIAL
  OR gs_po_header-ekorg IS INITIAL
  OR gs_po_header-ekgrp IS INITIAL.
    MESSAGE 'Missing header data (Vendor/Company/Org/Group).' TYPE 'S' DISPLAY LIKE 'E'.
    RETURN.
  ENDIF.

  "=== X# lý d# li#u Header ===
  lv_lifnr = gs_po_header-lifnr.
  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
    EXPORTING
      input  = lv_lifnr
    IMPORTING
      output = lv_lifnr.

  IF sy-tcode = 'ZRETR'.
    DATA(lv_docType) = 'ZRTN'.
  ELSEIF sy-tcode = 'ZEXCH'.
    lv_docType = 'ZEXC'.
  ENDIF.

  ls_poheader = VALUE #(
    doc_type  = lv_docType
    vendor    = lv_lifnr
    purch_org = gs_po_header-ekorg
    pur_group = gs_po_header-ekgrp
    comp_code = gs_po_header-bukrs
    incoterms1 = 'FOB'
    incoterms2 = 'FOB'
    shiptype = 'SEA'
    shipcond = 'CIF'
  ).

  ls_poheaderx = VALUE #(
    doc_type  = 'X'
    vendor    = 'X'
    purch_org = 'X'
    pur_group = 'X'
    comp_code = 'X'
    incoterms1 = 'X'
    incoterms2 = 'X'
    shiptype = 'X'
    shipcond = 'X'
  ).

  "=== Gán currency n#u có trong item ##u tiên ===
  IF gt_po_item IS NOT INITIAL.
    READ TABLE gt_po_item INTO DATA(ls_first) INDEX 1.
    IF sy-subrc = 0 AND ls_first-zcurrency IS NOT INITIAL.
      lv_curr = ls_first-zcurrency.
    ENDIF.
  ENDIF.

  ls_poheader-currency  = lv_curr.
  ls_poheaderx-currency = 'X'.

  "=== X# lý các dòng PO item ===
  LOOP AT gt_po_item INTO DATA(ls_item_data).
    IF ls_item_data-matnr IS INITIAL
    OR ls_item_data-zplant IS INITIAL
    OR ls_item_data-bstmg IS INITIAL
    OR ls_item_data-zunit IS INITIAL.
      CONTINUE.
    ENDIF.

    ADD 1 TO lv_cnt.
    lv_itemno = lv_cnt * 10.
    lv_matnr  = ls_item_data-matnr.

    CALL FUNCTION 'CONVERSION_EXIT_MATN1_INPUT'
      EXPORTING
        input  = lv_matnr
      IMPORTING
        output = lv_matnr.

    "=== POITEM ===
    APPEND VALUE #(
      po_item    = lv_itemno
      material   = lv_matnr
      plant      = ls_item_data-zplant
      quantity   = ls_item_data-bstmg
      po_unit    = ls_item_data-zunit
      matl_group = ls_item_data-matkl
      net_price  = ls_item_data-bprei
      price_unit = ls_item_data-peinh
      short_text = COND #( WHEN ls_item_data-zmate_des IS NOT INITIAL THEN ls_item_data-zmate_des )
      ret_item   = COND #( WHEN ls_item_data-retpo = 'X' THEN 'X' )
    ) TO lt_poitem.

    "=== POITEMX ===
    APPEND VALUE #(
      po_item     = lv_itemno
      po_itemx    = 'X'
      material    = 'X'
      plant       = 'X'
      quantity    = 'X'
      po_unit     = 'X'
      matl_group  = 'X'
      net_price   = 'X'
      price_unit  = 'X'
      short_text  = COND #( WHEN ls_item_data-zmate_des IS NOT INITIAL THEN 'X' )
      ret_item    = COND #( WHEN ls_item_data-retpo = 'X' THEN 'X' )
    ) TO lt_poitemx.

    "=== SCHEDULE ===
    APPEND VALUE #(
      po_item       = lv_itemno
      sched_line    = '0001'
      delivery_date = COND #( WHEN ls_item_data-eindt IS NOT INITIAL THEN ls_item_data-eindt ELSE sy-datum )
      quantity      = ls_item_data-bstmg
    ) TO lt_poschedule.

    APPEND VALUE #(
      po_item       = lv_itemno
      sched_line    = '0001'
      po_itemx      = 'X'
      sched_linex   = 'X'
      delivery_date = 'X'
      quantity      = 'X'
    ) TO lt_poschedulex.

    "=== CONDITION ===
    APPEND VALUE #(
      itm_number   = lv_itemno
      cond_type    = 'PB00'
      cond_value   = ls_item_data-bprei
      currency     = lv_curr
      cond_unit    = ls_item_data-zunit
      cond_p_unt   = ls_item_data-peinh
    ) TO lt_pocond.

    APPEND VALUE #(
      itm_number   = lv_itemno
      cond_type    = 'PB00'
    ) TO lt_pocondx.

  ENDLOOP.

  "=== Ki#m tra l#i d# li#u item ===
  IF lt_poitem IS INITIAL.
    MESSAGE 'No valid line items to create PO.' TYPE 'S' DISPLAY LIKE 'E'.
    RETURN.
  ENDIF.

  "=== G#i BAPI ===
  CALL FUNCTION 'BAPI_PO_CREATE1'
    EXPORTING
      poheader         = ls_poheader
      poheaderx        = ls_poheaderx
      testrun          = lv_testrun
    IMPORTING
      exppurchaseorder = lv_po_number
    TABLES
      poitem           = lt_poitem
      poitemx          = lt_poitemx
      poschedule       = lt_poschedule
      poschedulex      = lt_poschedulex
      pocond           = lt_pocond
      pocondx          = lt_pocondx
      return           = lt_return.

  "=== X# lý k#t qu# ===
  LOOP AT lt_return INTO ls_return.
    IF ls_return-type = 'E' OR ls_return-type = 'A'.
      lv_success = abap_false.
    ENDIF.

    MESSAGE ID ls_return-id TYPE 'S'
            NUMBER ls_return-number
            WITH ls_return-message_v1 ls_return-message_v2
                 ls_return-message_v3 ls_return-message_v4
            DISPLAY LIKE ls_return-type.
  ENDLOOP.

  IF lv_success = abap_true AND lv_po_number IS NOT INITIAL.
    CALL FUNCTION 'BAPI_TRANSACTION_COMMIT' EXPORTING wait = 'X'.
    MESSAGE |PO { lv_po_number } created successfully.| TYPE 'S'.
    SET PARAMETER ID 'BES' FIELD lv_po_number.
    CALL TRANSACTION 'ME23N' AND SKIP FIRST SCREEN.
  ELSE.
    CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
    MESSAGE 'PO creation failed. See the error above (RETURN).' TYPE 'E'.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&  Change PO by BAPI_PO_CHANGE  (update price, qty, date, currency)
*&---------------------------------------------------------------------*
FORM change_po USING iv_ebeln TYPE ebeln.

  "========================
  " 1) Khai báo
  "========================
  DATA: lt_item      TYPE STANDARD TABLE OF bapimepoitem,
        ls_item      TYPE bapimepoitem,
        lt_itemx     TYPE STANDARD TABLE OF bapimepoitemx,
        ls_itemx     TYPE bapimepoitemx,
        lt_schedule  TYPE STANDARD TABLE OF bapimeposchedule,
        ls_schedule  TYPE bapimeposchedule,
        lt_schedulex TYPE STANDARD TABLE OF bapimeposchedulx,
        ls_schedulex TYPE bapimeposchedulx,
        lt_return    TYPE STANDARD TABLE OF bapiret2,
        ls_return    TYPE bapiret2,
        lv_ok        TYPE abap_bool VALUE abap_true.

  DATA: lt_poheadtext TYPE STANDARD TABLE OF bapimepotextheader,
*      lt_poitmtext  TYPE STANDARD TABLE OF bapimepotextitem,
        lt_textlines  TYPE STANDARD TABLE OF bapimepotext,
        ls_textline   TYPE bapimepotext.

  FIELD-SYMBOLS: <ls_src>   TYPE any,
                 <fs_matnr>,
                 <fs_retpo>,
                 <fs_werks>, <fs_lgort>,
                 <fs_bstmg>, <fs_zunit>,
                 <fs_bprei>, <fs_peinh>,
                 <fs_eindt>.

  DATA: lv_matnr     TYPE matnr,
        lv_retpo_val TYPE c LENGTH 1,
        lv_werks     TYPE werks_d,
        lv_lgort     TYPE lgort_d,
        lv_bstmg     TYPE bapimepoitem-quantity,
        lv_zunit     TYPE meins,
        lv_bprei     TYPE bapimepoitem-net_price,
        lv_peinh     TYPE bapimepoitem-price_unit,
        lv_eindt     TYPE bapimeposchedule-delivery_date.

  DATA(lv_ebeln_norm) = iv_ebeln.
  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
    EXPORTING
      input  = lv_ebeln_norm
    IMPORTING
      output = lv_ebeln_norm.

  " L#y EBELP cu#i ## +10 khi thêm
  SELECT MAX( ebelp ) INTO @DATA(lv_last_item)
    FROM ekpo
   WHERE ebeln = @lv_ebeln_norm.
  IF lv_last_item IS INITIAL.
    lv_last_item = 0.
  ENDIF.

  " Default ##n v#/sloc/peinh n#u thi#u
  DATA(ls_tpl_ekpo) = VALUE ekpo( ).
  SELECT SINGLE * INTO @ls_tpl_ekpo
    FROM ekpo
   WHERE ebeln = @lv_ebeln_norm.

  "========================
  " 2) Snapshot DB (EKPO + EKET)
  "========================
  TYPES: BEGIN OF ty_db,
           ebelp TYPE ekpo-ebelp,
           matnr TYPE ekpo-matnr,
           werks TYPE ekpo-werks,
           lgort TYPE ekpo-lgort,
           retpo TYPE ekpo-retpo,
           meins TYPE ekpo-meins,
           netpr TYPE ekpo-netpr,
           peinh TYPE ekpo-peinh,
           menge TYPE ekpo-menge,
           eindt TYPE eket-eindt,
         END OF ty_db.

  DATA: lt_db TYPE STANDARD TABLE OF ty_db WITH EMPTY KEY,
        ls_db TYPE ty_db.

  SELECT a~ebelp, a~matnr, a~werks, a~lgort, a~retpo,
         a~meins, a~netpr, a~peinh, a~menge, b~eindt
    INTO TABLE @lt_db
    FROM ekpo AS a
    LEFT OUTER JOIN eket AS b
      ON b~ebeln = a~ebeln
     AND b~ebelp = a~ebelp
     AND b~etenr = '0001'
   WHERE a~ebeln = @lv_ebeln_norm
     AND a~loekz = @space.

  "========================
  " 3) Chu#n hoá WANT t# gt_po_item
  "========================
  TYPES: BEGIN OF ty_key,
           matnr TYPE ekpo-matnr,
           werks TYPE ekpo-werks,
           lgort TYPE ekpo-lgort,
           retpo TYPE ekpo-retpo,
           meins TYPE ekpo-meins,
           netpr TYPE ekpo-netpr,
           peinh TYPE ekpo-peinh,
           eindt TYPE eket-eindt,
           bstmg TYPE ekpo-menge,
         END OF ty_key.

  TYPES: BEGIN OF ty_want,
           key TYPE ty_key,
         END OF ty_want.

  TYPES: BEGIN OF ty_need,
           key   TYPE ty_key,
           count TYPE i,
         END OF ty_need.
  DATA: lt_need     TYPE STANDARD TABLE OF ty_need WITH EMPTY KEY,
        ls_need     TYPE ty_need,
        ls_prev_key TYPE ty_key,
        lv_count    TYPE i.

  CLEAR: lt_need, ls_need, ls_prev_key, lv_count.


  DATA: lt_want TYPE STANDARD TABLE OF ty_want WITH EMPTY KEY,
        ls_want TYPE ty_want.

  LOOP AT gt_po_item ASSIGNING <ls_src>.
    CLEAR: lv_matnr, lv_retpo_val, lv_werks, lv_lgort,
           lv_bstmg, lv_zunit, lv_bprei, lv_peinh, lv_eindt.

    ASSIGN COMPONENT 'MATNR' OF STRUCTURE <ls_src> TO <fs_matnr>.
    IF <fs_matnr> IS ASSIGNED AND <fs_matnr> IS NOT INITIAL.
      lv_matnr = <fs_matnr>.
    ENDIF.
    IF lv_matnr IS INITIAL.
      CONTINUE.
    ENDIF.

    ASSIGN COMPONENT 'RETPO' OF STRUCTURE <ls_src> TO <fs_retpo>.
    IF <fs_retpo> IS ASSIGNED AND <fs_retpo> IS NOT INITIAL.
      lv_retpo_val = <fs_retpo>.
    ELSE.
      CLEAR lv_retpo_val.
    ENDIF.
    IF lv_retpo_val = '-'. CLEAR lv_retpo_val. ENDIF.

    ASSIGN COMPONENT 'WERKS' OF STRUCTURE <ls_src> TO <fs_werks>.
    lv_werks = COND #( WHEN <fs_werks> IS ASSIGNED AND <fs_werks> IS NOT INITIAL
                       THEN <fs_werks> ELSE ls_tpl_ekpo-werks ).

    ASSIGN COMPONENT 'LGORT' OF STRUCTURE <ls_src> TO <fs_lgort>.
    lv_lgort = COND #( WHEN <fs_lgort> IS ASSIGNED AND <fs_lgort> IS NOT INITIAL
                       THEN <fs_lgort> ELSE ls_tpl_ekpo-lgort ).

    ASSIGN COMPONENT 'BSTMG' OF STRUCTURE <ls_src> TO <fs_bstmg>.
    IF <fs_bstmg> IS ASSIGNED AND <fs_bstmg> IS NOT INITIAL.
      lv_bstmg = <fs_bstmg>.
    ENDIF.

    ASSIGN COMPONENT 'ZUNIT' OF STRUCTURE <ls_src> TO <fs_zunit>.
    lv_zunit = COND #( WHEN <fs_zunit> IS ASSIGNED AND <fs_zunit> IS NOT INITIAL
                       THEN <fs_zunit> ELSE ls_tpl_ekpo-meins ).

    ASSIGN COMPONENT 'BPREI' OF STRUCTURE <ls_src> TO <fs_bprei>.
    IF <fs_bprei> IS ASSIGNED AND <fs_bprei> IS NOT INITIAL.
      lv_bprei = <fs_bprei>.
    ENDIF.

    ASSIGN COMPONENT 'PEINH' OF STRUCTURE <ls_src> TO <fs_peinh>.
    lv_peinh = COND #( WHEN <fs_peinh> IS ASSIGNED AND <fs_peinh> IS NOT INITIAL
                       THEN <fs_peinh> ELSE ls_tpl_ekpo-peinh ).

    ASSIGN COMPONENT 'EINDT' OF STRUCTURE <ls_src> TO <fs_eindt>.
    IF <fs_eindt> IS ASSIGNED AND <fs_eindt> IS NOT INITIAL.
      lv_eindt = <fs_eindt>.
    ENDIF.

    CLEAR ls_want.
    ls_want-key-matnr = lv_matnr.
    ls_want-key-werks = lv_werks.
    ls_want-key-lgort = lv_lgort.
    ls_want-key-retpo = COND ekpo-retpo( WHEN lv_retpo_val = 'X' THEN 'X' ELSE space ).
    ls_want-key-meins = lv_zunit.
    ls_want-key-netpr = lv_bprei.
    ls_want-key-peinh = lv_peinh.
    ls_want-key-eindt = lv_eindt.
    ls_want-key-bstmg = lv_bstmg.
    APPEND ls_want TO lt_want.
  ENDLOOP.

  SORT lt_want BY
    key-matnr key-werks key-lgort key-retpo
    key-meins key-netpr key-peinh key-eindt key-bstmg.

  LOOP AT lt_want INTO ls_want.
    IF sy-tabix = 1 OR ls_want-key <> ls_prev_key.
      IF sy-tabix > 1.
        ls_need-key   = ls_prev_key.
        ls_need-count = lv_count.
        APPEND ls_need TO lt_need.
      ENDIF.
      ls_prev_key = ls_want-key.
      lv_count    = 1.
    ELSE.
      lv_count = lv_count + 1.
    ENDIF.
  ENDLOOP.

  IF lines( lt_want ) > 0.
    ls_need-key   = ls_prev_key.
    ls_need-count = lv_count.
    APPEND ls_need TO lt_need.
  ENDIF.


  "========================
  " 4) Gom DB theo key (fix ASSIGNING)
  "========================
  TYPES: BEGIN OF ty_bucket,
           key    TYPE ty_key,
           ebelps TYPE STANDARD TABLE OF ebelp WITH EMPTY KEY,
         END OF ty_bucket.
  DATA: lt_db_buckets TYPE STANDARD TABLE OF ty_bucket WITH EMPTY KEY.

  LOOP AT lt_db INTO ls_db.
    DATA(ls_db_key) = VALUE ty_key(
                         matnr = ls_db-matnr
                         werks = ls_db-werks
                         lgort = ls_db-lgort
                         retpo = ls_db-retpo
                         meins = ls_db-meins
                         netpr = ls_db-netpr
                         peinh = ls_db-peinh
                         eindt = ls_db-eindt
                         bstmg = ls_db-menge ).
    READ TABLE lt_db_buckets ASSIGNING FIELD-SYMBOL(<b>)
         WITH KEY key = ls_db_key.
    IF sy-subrc = 0.
      APPEND ls_db-ebelp TO <b>-ebelpS.
    ELSE.
      APPEND VALUE ty_bucket(
               key    = ls_db_key
               ebelps = VALUE #( ( ls_db-ebelp ) ) ) TO lt_db_buckets.
    ENDIF.
  ENDLOOP.

  "========================
  " 5) Tính ph#n xoá/thêm
  "========================
  DATA: lt_to_delete TYPE STANDARD TABLE OF ebelp     WITH EMPTY KEY,
        lt_to_add    TYPE STANDARD TABLE OF ty_key    WITH EMPTY KEY.

  LOOP AT lt_db_buckets ASSIGNING <b>.
    DATA(db_cnt) = lines( <b>-ebelpS ).
    READ TABLE lt_need INTO ls_need WITH KEY key = <b>-key.
    DATA(want_cnt) = COND i( WHEN sy-subrc = 0 THEN ls_need-count ELSE 0 ).
    DATA(paired)   = COND i( WHEN want_cnt > 0 THEN
                               COND i( WHEN db_cnt <= want_cnt THEN db_cnt ELSE want_cnt )
                             ELSE 0 ).

    " DB d# => xoá
    IF db_cnt > paired.
      SORT <b>-ebelps BY table_line DESCENDING.

      LOOP AT <b>-ebelps INTO DATA(lv_del)
           FROM paired + 1.
        APPEND lv_del TO lt_to_delete.
      ENDLOOP.
    ENDIF.


    " WANT d# => c#n thêm
    IF want_cnt > paired.
      DO want_cnt - paired TIMES.
        APPEND <b>-key TO lt_to_add.
      ENDDO.
    ENDIF.

    " D#n NEED #ã x# lý
    IF want_cnt > 0.
      DELETE lt_need WHERE key = <b>-key.
    ENDIF.
  ENDLOOP.

  " Các key còn l#i trong WANT (ch#a có # DB) => add h#t
  LOOP AT lt_need INTO ls_need.
    DO ls_need-count TIMES.
      APPEND ls_need-key TO lt_to_add.
    ENDDO.
  ENDLOOP.

  "========================
  " 6) Build b#ng BAPI
  "========================
  " --- DELETE ---
  LOOP AT lt_to_delete INTO DATA(lv_ebelp_del).
    CLEAR: ls_item, ls_itemx.
    ls_item-po_item    = lv_ebelp_del.
    ls_item-delete_ind = 'X'.
    APPEND ls_item TO lt_item.

    ls_itemx-po_item    = lv_ebelp_del.
    ls_itemx-po_itemx   = 'X'.
    ls_itemx-delete_ind = 'X'.
    APPEND ls_itemx TO lt_itemx.
  ENDLOOP.

  " --- ADD ---
  LOOP AT lt_to_add INTO DATA(ls_add_key).
    CLEAR: ls_item, ls_itemx.
    lv_last_item = lv_last_item + 10.
    DATA(lv_ebelp_new) = lv_last_item.
    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        input  = lv_ebelp_new
      IMPORTING
        output = lv_ebelp_new.

    " POITEM
    ls_item-po_item   = lv_ebelp_new.
    ls_item-material  = ls_add_key-matnr.
    ls_item-plant     = ls_add_key-werks.
    ls_item-stge_loc  = ls_add_key-lgort.
    ls_item-po_unit   = COND meins( WHEN ls_add_key-meins IS INITIAL
                                    THEN ls_tpl_ekpo-meins ELSE ls_add_key-meins ).
    IF ls_add_key-bstmg IS NOT INITIAL. ls_item-quantity   = ls_add_key-bstmg. ENDIF.
    IF ls_add_key-netpr IS NOT INITIAL. ls_item-net_price  = ls_add_key-netpr. ENDIF.
    IF ls_add_key-peinh IS NOT INITIAL. ls_item-price_unit = ls_add_key-peinh. ENDIF.
    IF ls_add_key-retpo = 'X'.         ls_item-ret_item   = 'X'.           ENDIF.
    " N#u h# th#ng yêu c#u:
    " ls_item-item_cat  = '0'.

    " POITEMX
    ls_itemx-po_item   = lv_ebelp_new.
    ls_itemx-po_itemx  = 'X'.
    ls_itemx-material  = 'X'.
    ls_itemx-plant     = 'X'.
    ls_itemx-stge_loc  = 'X'.
    ls_itemx-po_unit   = 'X'.
    IF ls_add_key-bstmg IS NOT INITIAL. ls_itemx-quantity   = 'X'. ENDIF.
    IF ls_add_key-netpr IS NOT INITIAL. ls_itemx-net_price  = 'X'. ENDIF.
    IF ls_add_key-peinh IS NOT INITIAL. ls_itemx-price_unit = 'X'. ENDIF.
    IF ls_add_key-retpo = 'X'.         ls_itemx-ret_item   = 'X'. ENDIF.
    " ls_itemx-item_cat  = 'X'.

    APPEND ls_item  TO lt_item.
    APPEND ls_itemx TO lt_itemx.

    " POSCHEDULE
    IF ls_add_key-eindt IS NOT INITIAL OR ls_add_key-bstmg IS NOT INITIAL.
      CLEAR: ls_schedule, ls_schedulex.
      ls_schedule-po_item     = lv_ebelp_new.
      ls_schedule-sched_line  = '0001'.
      IF ls_add_key-eindt IS NOT INITIAL. ls_schedule-delivery_date = ls_add_key-eindt. ENDIF.
      IF ls_add_key-bstmg IS NOT INITIAL. ls_schedule-quantity     = ls_add_key-bstmg. ENDIF.

      ls_schedulex-po_item     = lv_ebelp_new.
      ls_schedulex-sched_line  = '0001'.
      ls_schedulex-po_itemx    = 'X'.
      ls_schedulex-sched_linex = 'X'.
      IF ls_add_key-eindt IS NOT INITIAL. ls_schedulex-delivery_date = 'X'. ENDIF.
      IF ls_add_key-bstmg IS NOT INITIAL. ls_schedulex-quantity      = 'X'. ENDIF.

      APPEND ls_schedule  TO lt_schedule.
      APPEND ls_schedulex TO lt_schedulex.
    ENDIF.
  ENDLOOP.

  " Không có thay ##i => thoát
  IF lt_item IS INITIAL AND lt_itemx IS INITIAL
   AND lt_schedule IS INITIAL AND lt_schedulex IS INITIAL.
    MESSAGE 'There are no changes to update the PO.' TYPE 'S'.
    RETURN.
  ENDIF.

  "========================
  " 7) G#i BAPI
  "========================
  CALL FUNCTION 'BAPI_PO_CHANGE'
    EXPORTING
      purchaseorder = lv_ebeln_norm
    TABLES
      return        = lt_return
      poitem        = lt_item
      poitemx       = lt_itemx
      poschedule    = lt_schedule
      poschedulex   = lt_schedulex.

  LOOP AT lt_return INTO ls_return.
    IF ls_return-type = 'E' OR ls_return-type = 'A'.
      lv_ok = abap_false.
    ENDIF.
    MESSAGE ID ls_return-id TYPE 'S' NUMBER ls_return-number
            WITH ls_return-message_v1 ls_return-message_v2
                 ls_return-message_v3 ls_return-message_v4
            DISPLAY LIKE ls_return-type.
  ENDLOOP.

  IF lv_ok = abap_true.
    CALL FUNCTION 'BAPI_TRANSACTION_COMMIT' EXPORTING wait = 'X'.
    MESSAGE |PO { lv_ebeln_norm } has been successfully updated.| TYPE 'S'.
  ELSE.
    CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
    MESSAGE |C#p nh#t PO { lv_ebeln_norm } failed. See the error above.| TYPE 'E'.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form show_popup_change
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
FORM show_popup_change CHANGING pv_edit TYPE c.
  DATA: lv_ans TYPE c,
        lv_msg TYPE string.

  IF pv_edit = 'X'.
    lv_msg = 'You are currently in CHANGE mode. Do you want to switch to CREATE mode?'.
  ELSE.
    lv_msg = 'You are currently in CREATE mode. Do you want to switch to CHANGE mode?'.
  ENDIF.

  CALL FUNCTION 'POPUP_TO_CONFIRM'
    EXPORTING
      text_question  = lv_msg
      text_button_1  = 'Yes'(001)
      text_button_2  = 'No'(002)
    IMPORTING
      answer         = lv_ans
    EXCEPTIONS
      text_not_found = 1
      OTHERS         = 2.

  " N#u ng##i dùng ch#n Yes thì ##i ch# ##
  IF lv_ans = '1'.
    IF pv_edit = 'X'.
      pv_edit = ''.
    ELSE.
      pv_edit = 'X'.
    ENDIF.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form post_bapi_receipt
*&---------------------------------------------------------------------*
FORM post_bapi_receipt.

  DATA:
    lv_mat_doc_str TYPE c LENGTH 20,
    lv_doc_year    TYPE bapi2017_gm_head_ret-mat_doc,
    lt_return      TYPE STANDARD TABLE OF bapiret2,
    ls_return      TYPE bapiret2,
    ls_mat_doc     TYPE bapi2017_gm_head_ret.

  DATA: ls_head TYPE bapi2017_gm_head_01,
        ls_code TYPE bapi2017_gm_code,
        lt_item TYPE STANDARD TABLE OF bapi2017_gm_item_create,
        ls_item TYPE bapi2017_gm_item_create.


  FIELD-SYMBOLS <ls_rec> TYPE ty_receipt_item.

  "=== 1) Header ===
  CLEAR: ls_head, ls_code.
  ls_head-pstng_date = gs_receipt_header-budat.
  ls_head-doc_date   = gs_receipt_header-bldat.
  ls_head-ref_doc_no = gs_receipt_header-ebeln.
  ls_code-gm_code    = '01'.   " GR for PO

  "=== 2) Build items from EKPO ===
  CLEAR lt_item.

  LOOP AT gt_receipt_item ASSIGNING <ls_rec>.

    CLEAR ls_item.

    "---- Get data directly from EKPO based on Purchase Order (PO)
    SELECT SINGLE ebeln, ebelp, matnr, werks, menge, meins, lgort
      INTO (@ls_item-po_number, @ls_item-po_item, @ls_item-material, @ls_item-plant, @ls_item-entry_qnt, @ls_item-entry_uom, @ls_item-stge_loc)
      FROM ekpo
      WHERE ebeln = @<ls_rec>-purchase_order
        AND ebelp = @<ls_rec>-item.

    " Check if data is found for the PO item
    IF sy-subrc <> 0.
      MESSAGE |Line { sy-tabix }: No information found in EKPO for PO { <ls_rec>-purchase_order } and Item { <ls_rec>-item }| TYPE 'E'.
      CONTINUE.
    ENDIF.

    " Chu#n hóa Purchase Order Item (ebelp)
    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        input  = <ls_rec>-item
      IMPORTING
        output = ls_item-po_item.

    " Chu#n hóa Material Number (matnr)
    IF <ls_rec>-material IS NOT INITIAL.
      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
        EXPORTING
          input  = <ls_rec>-material
        IMPORTING
          output = ls_item-material.
    ENDIF.

    "---- Movement type + reference PO flag
    " X# lý cho Movement Type 161, 101, 122 linh ho#t
    IF <ls_rec>-returns_item = 'X'.  " N#u là Return PO
      ls_item-move_type = '161'.  " GR cho Return PO ch#a có GR
      ls_item-move_reas = '000'.
    ELSEIF <ls_rec>-returns_item = '' AND <ls_rec>-movement_type IS INITIAL.
      ls_item-move_type = '122'.  " GR cho PO #ã có GR
      ls_item-move_reas = '001'.
    ELSEIF <ls_rec>-returns_item = '' AND <ls_rec>-movement_type IS NOT INITIAL.
      ls_item-move_type = <ls_rec>-movement_type. " L#a ch#n linh ho#t gi#a các movement type
      ls_item-move_reas = '000'.  " Gi# lý do m#c ##nh cho Movement Type khác
    ELSE.
      ls_item-move_type = '101'.  " GR thông th##ng
      ls_item-move_reas = '000'.
    ENDIF.

    ls_item-mvt_ind   = 'B'.
    ls_item-no_more_gr = 'X'.
    ls_item-withdrawn = 'X'.

    "---- Ki#m tra n#u thi#u UOM (Unit of Measure) và Plant
    IF ls_item-plant IS INITIAL OR ls_item-entry_uom IS INITIAL.
      " Fetch data from EKPO if missing
      SELECT SINGLE werks, meins
        INTO (@ls_item-plant, @ls_item-entry_uom)
        FROM ekpo
        WHERE ebeln = @<ls_rec>-purchase_order
          AND ebelp = @<ls_rec>-item.
    ENDIF.

    "---- Check if Material Number is valid
    IF ls_item-material IS INITIAL.
      MESSAGE |Line { sy-tabix }: Missing Material Number for PO { <ls_rec>-purchase_order } and Item { <ls_rec>-item }| TYPE 'E'.
      CONTINUE.
    ENDIF.

    "---- L#y Storage Location (LGORT) n#u thi#u
    IF ls_item-stge_loc IS INITIAL.
      SELECT SINGLE lgort
        INTO @ls_item-stge_loc
        FROM mard
        WHERE matnr = @ls_item-material
          AND werks = @ls_item-plant.
    ENDIF.

    "---- Ki#m tra d# li#u c#t lõi (Qty/UoM/Plant/PO/Item)
    IF ls_item-entry_qnt IS INITIAL OR ls_item-entry_qnt <= 0
    OR ls_item-entry_uom IS INITIAL
    OR ls_item-plant     IS INITIAL
    OR ls_item-stge_loc  IS INITIAL
    OR ls_item-po_number IS INITIAL
    OR ls_item-po_item   IS INITIAL.
      MESSAGE |Line { sy-tabix }: Missing mandatory data (Qty/UoM/Plant/PO/Item).| TYPE 'S' DISPLAY LIKE 'E'.
      CONTINUE.
    ENDIF.

    " Append item to the item table
    APPEND ls_item TO lt_item.
  ENDLOOP.

  " If there are no valid items to post, exit
  IF lt_item IS INITIAL.
    MESSAGE 'No valid line items to post GR.' TYPE 'S' DISPLAY LIKE 'E'.
    RETURN.
  ENDIF.

  "=== 3) G#i BAPI_GOODSMVT_CREATE ===
  CALL FUNCTION 'BAPI_GOODSMVT_CREATE'
    EXPORTING
      goodsmvt_header  = ls_head
      goodsmvt_code    = ls_code
    IMPORTING
      goodsmvt_headret = ls_mat_doc
      materialdocument = lv_doc_year
    TABLES
      goodsmvt_item    = lt_item
      return           = lt_return.

  "=== 4) Log & Commit / Rollback
  DATA(lv_ok) = abap_true.
  LOOP AT lt_return INTO ls_return.
    IF ls_return-type = 'E' OR ls_return-type = 'A'.
      lv_ok = abap_false.
    ENDIF.
    MESSAGE ID ls_return-id TYPE 'S' NUMBER ls_return-number
            WITH ls_return-message_v1 ls_return-message_v2
                 ls_return-message_v3 ls_return-message_v4
            DISPLAY LIKE ls_return-type.
  ENDLOOP.

  IF lv_ok = abap_true AND ls_mat_doc IS NOT INITIAL.
    CALL FUNCTION 'BAPI_TRANSACTION_COMMIT' EXPORTING wait = 'X'.
    WRITE ls_mat_doc TO lv_mat_doc_str.
    MESSAGE |Goods Receipt posted: { lv_mat_doc_str } / { lv_doc_year }| TYPE 'S'.
  ELSE.
    CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
    MESSAGE 'Posting GR failed. See the error message above.' TYPE 'E'.
  ENDIF.

ENDFORM.

*FORM post_bapi_receipt .
*
*  DATA:
*        lv_mat_doc_str TYPE c LENGTH 20,
*        lv_doc_year    TYPE bapi2017_gm_head_ret-mat_doc,
*        lt_return      TYPE STANDARD TABLE OF bapiret2,
*        ls_return      TYPE bapiret2,
*        ls_mat_doc     TYPE bapi2017_gm_head_ret.
*
*  DATA: ls_head TYPE bapi2017_gm_head_01,
*        ls_code TYPE bapi2017_gm_code,
*        lt_item TYPE STANDARD TABLE OF bapi2017_gm_item_create,
*        ls_item TYPE bapi2017_gm_item_create.
*
*  FIELD-SYMBOLS <ls_rec> TYPE ty_receipt_item.
*
*  "=== 1) Header ===
*  CLEAR: ls_head, ls_code.
*  ls_head-pstng_date = gs_receipt_header-budat.
*  ls_head-doc_date   = gs_receipt_header-bldat.
*  ls_head-ref_doc_no = gs_receipt_header-ebeln.
*  ls_code-gm_code    = '01'.   " GR for PO
*
*  "=== 2) Build items ===
*  CLEAR lt_item.
*
*  LOOP AT gt_receipt_item ASSIGNING <ls_rec>.
*    CLEAR ls_item.
*
*    "---- Chu#n hoá key
*    DATA: lv_ebeln TYPE ebeln,
*          lv_ebelp TYPE ebelp,
*          lv_matnr TYPE matnr,
*          lv_meins TYPE meins,
*          lv_werks TYPE werks_d,
*          lv_lgort TYPE lgort_d.
*
*    lv_ebeln = <ls_rec>-purchase_order.
*    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
*      EXPORTING
*        input  = lv_ebeln
*      IMPORTING
*        output = lv_ebeln.
*
*    lv_ebelp = COND ebelp( WHEN <ls_rec>-item IS NOT INITIAL
*                           THEN <ls_rec>-item
*                           ELSE <ls_rec>-line ).   " fallback n#u dùng LINE
*    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
*      EXPORTING
*        input  = lv_ebelp
*      IMPORTING
*        output = lv_ebelp.
*
*    IF <ls_rec>-material IS NOT INITIAL.
*      lv_matnr = <ls_rec>-material.
*      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
*        EXPORTING
*          input  = lv_matnr
*        IMPORTING
*          output = lv_matnr.
*    ENDIF.
*
*    "---- Movement type + ch# báo tham chi#u PO
*    ls_item-move_type = COND bwart(
*                          WHEN <ls_rec>-movement_type IS NOT INITIAL
*                          THEN <ls_rec>-movement_type
*                          ELSE '101' ).
*    ls_item-mvt_ind   = 'B'.
*    ls_item-po_number = lv_ebeln.
*    ls_item-po_item   = lv_ebelp.
*    ls_item-no_more_gr = 'X'.
*    ls_item-withdrawn = 'X'.
*
*    "---- Quantity & UoM
*    ls_item-entry_qnt = COND menge_d(
*                          WHEN <ls_rec>-del_note_qty IS NOT INITIAL THEN <ls_rec>-del_note_qty
*                          WHEN <ls_rec>-quantity     IS NOT INITIAL THEN <ls_rec>-quantity
*                          ELSE <ls_rec>-qty_in_une ).
*    ls_item-entry_uom = <ls_rec>-uom.
*    ls_item-plant     = <ls_rec>-plant.
*
*    "---- B# sung t# EKPO n#u thi#u
*    IF ls_item-plant IS INITIAL OR ls_item-entry_uom IS INITIAL.
*      SELECT SINGLE werks, meins, lgort
*        INTO (@lv_werks, @lv_meins, @lv_lgort)
*        FROM ekpo
*        WHERE ebeln = @lv_ebeln
*          AND ebelp = @lv_ebelp.
*      IF ls_item-plant     IS INITIAL AND lv_werks IS NOT INITIAL.
*        ls_item-plant     = lv_werks.
*      ENDIF.
*      IF ls_item-entry_uom IS INITIAL AND lv_meins IS NOT INITIAL.
*        ls_item-entry_uom = lv_meins.
*      ENDIF.
*    ELSE.
*      " N#u #ã có Plant/UoM t# ALV, v#n th# l#y LGORT t# EKPO
*      SELECT SINGLE lgort
*        INTO @lv_lgort
*        FROM ekpo
*        WHERE ebeln = @lv_ebeln
*          AND ebelp = @lv_ebelp.
*    ENDIF.
*
*    "---- Xác ##nh Storage Location (LGORT) theo chu#i #u tiên
*    " 1) EKPO-LGORT (#ã l#y # trên)
*    " 2) MARC-LGPRO (GR SLoc m#c ##nh c#a v#t t# t#i plant)
*    IF lv_lgort IS INITIAL AND lv_matnr IS NOT INITIAL AND ls_item-plant IS NOT INITIAL.
*      SELECT SINGLE lgpro
*        INTO @lv_lgort
*        FROM marc
*        WHERE matnr = @lv_matnr
*          AND werks = @ls_item-plant.
*    ENDIF.
*    " 3) B#t k# sloc t#n t#i trong plant (ch#n nh# nh#t)
*    IF lv_lgort IS INITIAL AND ls_item-plant IS NOT INITIAL.
*      SELECT SINGLE MIN( lgort )
*        INTO @lv_lgort
*        FROM t001l
*        WHERE werks = @ls_item-plant.
*    ENDIF.
*
*    " N#u h# th#ng B#T BU#C SLoc (nh# thông báo b#n g#p) => ph#i có
*    IF lv_lgort IS INITIAL.
*      MESSAGE |Dòng { sy-tabix }: Không xác ##nh ###c Storage Location| TYPE 'S' DISPLAY LIKE 'E'.
*      CONTINUE.
*    ENDIF.
*    ls_item-stge_loc = lv_lgort.
*
*    " Material (optional cho GR theo PO)
*    IF lv_matnr IS NOT INITIAL.
*      ls_item-material = lv_matnr.
*    ENDIF.
*
*    "---- Ki#m tra d# li#u c#t lõi (không yêu c#u LGORT n#u b#n mu#n n#i l#ng)
*    IF ls_item-entry_qnt IS INITIAL OR ls_item-entry_qnt <= 0
*    OR ls_item-entry_uom IS INITIAL
*    OR ls_item-plant     IS INITIAL
*    OR ls_item-po_number IS INITIAL
*    OR ls_item-po_item   IS INITIAL.
*      MESSAGE |Dòng { sy-tabix }: Thi#u d# li#u b#t bu#c (Qty/UoM/Plant/PO/Item).| TYPE 'S' DISPLAY LIKE 'E'.
*      CONTINUE.
*    ENDIF.
*
*    APPEND ls_item TO lt_item.
*  ENDLOOP.
*
*  IF lt_item IS INITIAL.
*    MESSAGE 'Không có dòng h#p l# ## post GR.' TYPE 'S' DISPLAY LIKE 'E'.
*    RETURN.
*  ENDIF.
*
*  "=== 3) G#i BAPI ===
*  CALL FUNCTION 'BAPI_GOODSMVT_CREATE'
*    EXPORTING
*      goodsmvt_header  = ls_head
*      goodsmvt_code    = ls_code
*    IMPORTING
*      materialdocument = lv_doc_year
*      GOODSMVT_HEADRET   = ls_mat_doc
*    TABLES
*      goodsmvt_item    = lt_item
*      return           = lt_return.
*
*  "=== 4) Log & Commit / Rollback
*  DATA(lv_ok) = abap_true.
*  LOOP AT lt_return INTO ls_return.
*    IF ls_return-type = 'E' OR ls_return-type = 'A'.
*      lv_ok = abap_false.
*    ENDIF.
*    MESSAGE ID ls_return-id TYPE 'S' NUMBER ls_return-number
*            WITH ls_return-message_v1 ls_return-message_v2
*                 ls_return-message_v3 ls_return-message_v4
*            DISPLAY LIKE ls_return-type.
*  ENDLOOP.
*
*  IF lv_ok = abap_true AND ls_mat_doc IS NOT INITIAL.
*    CALL FUNCTION 'BAPI_TRANSACTION_COMMIT' EXPORTING wait = 'X'.
*    WRITE ls_mat_doc TO lv_mat_doc_str.
*    MESSAGE |Goods Receipt posted: { lv_mat_doc_str } / { lv_doc_year }| TYPE 'S'.
*  ELSE.
*    CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
*    MESSAGE 'Post GR th#t b#i. Xem thông báo l#i # trên.' TYPE 'E'.
*  ENDIF.
*
*ENDFORM.
*&---------------------------------------------------------------------*
*& Form post_bapi_invoice
*&---------------------------------------------------------------------*
FORM post_bapi_invoice .

  DATA: lv_ebeln        TYPE ebeln,
        lv_ivdoc        TYPE rbkp-belnr,
        lv_ivyear       TYPE rbkp-gjahr,
        lv_itemno       TYPE char6 VALUE '000001',
        lv_total_amount TYPE bapi_incinv_create_header-gross_amount,
        lv_ok           TYPE abap_bool VALUE abap_true.

  DATA: ls_head TYPE bapi_incinv_create_header,
        lt_item TYPE STANDARD TABLE OF bapi_incinv_create_item,
        ls_item TYPE bapi_incinv_create_item,
        lt_ret  TYPE STANDARD TABLE OF bapiret2,
        ls_ret  TYPE bapiret2.

  " Các bi#n liên quan ##n GR
  DATA: lv_gr_doc  TYPE mblnr,
        lv_gr_year TYPE gjahr,
        lv_gr_item TYPE mseg-zeile,
        lv_gr_qty  TYPE menge_d.

  FIELD-SYMBOLS <ls_src> TYPE ty_invoice_item.

  "=== Chu#n hóa PO number ===
  lv_ebeln = gs_invoice_header-ebeln.
  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
    EXPORTING
      input  = lv_ebeln
    IMPORTING
      output = lv_ebeln.

  "=== Header c#a ch#ng t# Credit Memo ===
  CLEAR ls_head.
*  IF <ls_src>-movement_type = '161'.  " N#u là Return PO
*    ls_head-doc_type      = 'RE'. " Credit Memo
*  ELSE.
  ls_head-doc_type      = 'RE'.
*  ENDIF.
  ls_head-invoice_ind   = 'X'.
*  ls_head-calc_tax_ind  = 'X'.
  ls_head-pmnttrms      = '0001'.
  ls_head-comp_code     = gs_invoice_header-bukrs.
  ls_head-doc_date      = gs_invoice_header-bldat.
  ls_head-pstng_date    = gs_invoice_header-budat.
  ls_head-currency      = gs_invoice_header-zcurrency.
  ls_head-ref_doc_no    = lv_ebeln.


*  IF <ls_src>-movement_type = '161'.  " N#u là Return PO
*    ls_head-header_txt    = 'Credit Memo for GR'.
*  ELSE.
    ls_head-header_txt    = 'Post Invoice for GR'.
*  ENDIF.


  LOOP AT gt_invoice_item ASSIGNING <ls_src>.
    IF <ls_src>-quantity IS INITIAL AND <ls_src>-amount IS INITIAL.
      CONTINUE.
    ENDIF.

    "=== Chu#n hóa item number ===
    DATA(lv_po_item) = <ls_src>-item.
    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        input  = lv_po_item
      IMPORTING
        output = lv_po_item.

    "=== Gán d# li#u vào item c#a Credit Memo ===
    CLEAR ls_item.
    ls_item-invoice_doc_item = lv_itemno.
    ls_item-po_number        = lv_ebeln.
    ls_item-po_item          = lv_po_item.
    ls_item-quantity         = <ls_src>-quantity.
    ls_item-po_unit          = COND #( WHEN <ls_src>-uom IS NOT INITIAL THEN <ls_src>-uom ELSE 'EA' ).
    ls_item-item_amount      = abs( <ls_src>-amount ).
    ls_item-item_text        = <ls_src>-short_text.
    ls_item-tax_code         = COND #( WHEN <ls_src>-tax_code IS NOT INITIAL
                                       THEN <ls_src>-tax_code ELSE gs_invoice_header-tax_code ).
    ls_item-taxjurcode       = 'TX0000000'.

    APPEND ls_item TO lt_item.
    lv_total_amount = lv_total_amount + ls_item-item_amount.
    lv_itemno = lv_itemno + 1.
  ENDLOOP.

  IF lt_item IS INITIAL.
    MESSAGE 'Không có dòng nào h#p l# ## post.' TYPE 'S' DISPLAY LIKE 'E'.
    RETURN.
  ENDIF.

  ls_head-gross_amount = lv_total_amount.

  "=== G#i BAPI t#o ch#ng t# ===
  CALL FUNCTION 'BAPI_INCOMINGINVOICE_CREATE'
    EXPORTING
      headerdata       = ls_head
    IMPORTING
      invoicedocnumber = lv_ivdoc
      fiscalyear       = lv_ivyear
    TABLES
      itemdata         = lt_item
      return           = lt_ret.

  LOOP AT lt_ret INTO ls_ret.
    MESSAGE ID ls_ret-id TYPE ls_ret-type NUMBER ls_ret-number
            WITH ls_ret-message_v1 ls_ret-message_v2
                 ls_ret-message_v3 ls_ret-message_v4.
    IF ls_ret-type = 'E' OR ls_ret-type = 'A'.
      lv_ok = abap_false.
    ENDIF.
  ENDLOOP.

  IF lv_ok = abap_true AND lv_ivdoc IS NOT INITIAL.
    CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
      EXPORTING
        wait = 'X'.
    MESSAGE |Credit memo posted: { lv_ivdoc }/{ lv_ivyear }| TYPE 'S'.
    SET PARAMETER ID 'RBN' FIELD lv_ivdoc.
    SET PARAMETER ID 'GJR' FIELD lv_ivyear.
    CALL TRANSACTION 'MIR4' AND SKIP FIRST SCREEN.
  ELSE.
    CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
    MESSAGE 'Post th#t b#i. Xem log # trên.' TYPE 'E'.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form get_ir_qty
*&---------------------------------------------------------------------*
"==================== HÀM L#Y QTY IR RÒNG ====================
FORM get_ir_qty USING    iv_ebeln TYPE ebeln
                         iv_ebelp TYPE ebelp
                CHANGING ev_qty_ir TYPE menge_d.
  CLEAR ev_qty_ir.
  SELECT shkzg, menge
    FROM ekbe
    WHERE ebeln = @iv_ebeln
      AND ebelp = @iv_ebelp
      AND vgabe = '2'          "Invoice Receipt
    INTO TABLE @DATA(lt_ekbe).

  LOOP AT lt_ekbe ASSIGNING FIELD-SYMBOL(<ls_ekbe>).
    IF <ls_ekbe>-shkzg = 'S'.   "Invoice
      ev_qty_ir += <ls_ekbe>-menge.
    ELSEIF <ls_ekbe>-shkzg = 'H'. "Credit memo #ã ghi
      ev_qty_ir -= <ls_ekbe>-menge.
    ENDIF.
  ENDLOOP.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form view_payment
*&---------------------------------------------------------------------*
FORM view_payment .
  " Gán các giá tr# cho transaction FBL1N
  SET PARAMETER ID 'BUK' FIELD gs_payment_header-bukrs.  " Company Code
  SET PARAMETER ID 'LIF' FIELD gs_payment_header-lifnr.  " Vendor (Supplier)

  " G#i transaction FBL1N và b# qua màn hình ##u tiên
  CALL TRANSACTION 'FBL1N' AND SKIP FIRST SCREEN.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form lock_101_rows_in_alv
*&---------------------------------------------------------------------*
*& Khóa các dòng có move type là 101 (retuen không ###c tick)
*&---------------------------------------------------------------------*
FORM lock_101_rows_in_alv.

*  DATA: lt_to_lock TYPE STANDARD TABLE OF lvc_fname WITH EMPTY KEY,
*        lv_fname   TYPE lvc_fname.
*
*  lt_to_lock = VALUE #( ( 'SHORT_TEXT' )
*                        ( 'QTY_IN_UNE' )
*                        ( 'UOM' )
*                        ( 'MOVEMENT_TYPE' )
*                        ( 'STOCK_TYPE' )
*                        ( 'PLANT' )
*                        ( 'VENDOR' )
*                        ( 'DELIVERY_COMPL' )
*                        ( 'QUANTITY' )
*                        ( 'DEL_NOTE_QTY' )
*                        ( 'RETURNS_ITEM' ) ).
*
*  FIELD-SYMBOLS: <s> TYPE any.
*
*  LOOP AT gt_receipt_item ASSIGNING <s>.
*    CLEAR <s>-cell_style.
*
*    IF <s>-movement_type = '101'.
*      LOOP AT lt_to_lock INTO lv_fname.
*        APPEND VALUE lvc_s_styl(
*                 fieldname = lv_fname
*                 style     = cl_gui_alv_grid=>mc_style_disabled )
*          TO <s>-cell_style.
*      ENDLOOP.
*
*      APPEND VALUE lvc_s_styl(
*               fieldname = 'DELIVERY_COMPL'
*               style     = cl_gui_alv_grid=>mc_style_no_f4 )
*        TO <s>-cell_style.
*    ENDIF.
*  ENDLOOP.

ENDFORM.
