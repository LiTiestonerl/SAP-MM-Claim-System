*&---------------------------------------------------------------------*
*& Include  ZPG_MM_DOC_HISTORY_S01
*& Start/End dates + POST handling
*&---------------------------------------------------------------------*

SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-t01.
PARAMETERS: p_sdate TYPE sy-datum DEFAULT sy-datum,   " Start Date
            p_edate TYPE sy-datum DEFAULT sy-datum.   " End Date
PARAMETERS: p_view  TYPE char3 AS LISTBOX VISIBLE LENGTH 28.
SELECTION-SCREEN END OF BLOCK b1.

SELECTION-SCREEN: PUSHBUTTON /1(10) pb_post USER-COMMAND post.

INITIALIZATION.
  pb_post = 'POST'.
  p_view  = c_view_gi.

AT SELECTION-SCREEN OUTPUT.
  PERFORM fill_view_listbox.

AT SELECTION-SCREEN.
  IF sy-ucomm = 'POST'.
    CLEAR gv_ok.
    PERFORM validate_dates USING p_sdate p_edate CHANGING gv_ok.
    IF gv_ok IS INITIAL.
      LEAVE SCREEN.
    ENDIF.

    CLEAR gt_hist.

    CASE p_view.
      WHEN c_view_gi.
        PERFORM get_gi_history  USING p_sdate p_edate CHANGING gt_hist.
      WHEN c_view_ret.
        PERFORM get_ret_history USING p_sdate p_edate CHANGING gt_hist.
      WHEN c_view_clm.
        PERFORM get_clm_history USING p_sdate p_edate CHANGING gt_hist.
      WHEN c_view_101.
        PERFORM get_101_history USING p_sdate p_edate CHANGING gt_hist.
      WHEN c_view_122.
        PERFORM get_122_history USING p_sdate p_edate CHANGING gt_hist.
      WHEN c_view_exc.
        PERFORM get_exchange_history USING p_sdate p_edate CHANGING gt_hist.
      WHEN OTHERS.
        PERFORM get_gi_history  USING p_sdate p_edate CHANGING gt_hist.
    ENDCASE.


    IF gt_hist IS INITIAL.
      MESSAGE 'No data found for given criteria' TYPE 'S'.
      LEAVE SCREEN.
    ENDIF.

    PERFORM enrich_texts CHANGING gt_hist.
    PERFORM display_hist_alv USING gt_hist.
  ENDIF.
