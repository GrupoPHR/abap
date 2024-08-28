FUNCTION ZHCM_TIME_RESULTS_IN_INTERVAL.
*"----------------------------------------------------------------------
*"*"Interfase local
*"  IMPORTING
*"     VALUE(INT_PERNR) TYPE  PERNR_D
*"     VALUE(INT_BEGDA) TYPE  DATUM
*"     VALUE(INT_ENDDA) TYPE  DATUM
*"     VALUE(INT_CLTYP) TYPE  PC2B0-CLTYP DEFAULT 1
*"  TABLES
*"      GET_TBUFF OPTIONAL
*"      GET_BUFFER_DIR OPTIONAL
*"      INT_TIME_RESULTS OPTIONAL
*"  EXCEPTIONS
*"      NO_PERIOD_SPECIFIED
*"      WRONG_CLUSTER_VERSION
*"      NO_READ_AUTHORITY
*"      CLUSTER_ARCHIVED
*"      TECHNICAL_ERROR
*"----------------------------------------------------------------------

  DATA: lt_ptresult TYPE zptxx_result_pernr.
  DATA: wa_ptresult LIKE lt_ptresult.
  DATA: BEGIN OF wa_version.
          INCLUDE STRUCTURE pc2b1.
  DATA: END OF wa_version.

*-----Initialisieren der Schnittstellentabellen
  CLEAR int_time_results.
  REFRESH int_time_results.
*-----Füllen der Periodentabelle
  IF int_begda IS INITIAL AND int_endda IS INITIAL.
    int_begda = int_endda = sy-datum.
  ELSEIF int_begda IS INITIAL.
    int_begda = int_endda.
  ELSEIF int_endda IS INITIAL.
    int_endda = int_begda.
  ENDIF.

  CALL FUNCTION 'HR_PAYROLL_PERIODS_GET'
    EXPORTING
      get_begda       = int_begda
      get_endda       = int_endda
    TABLES
      get_periods     = i549q
    EXCEPTIONS
      no_period_found = 1
      no_valid_permo  = 2.
*-----Überprüfung, ob Periode für Import spezifiziert ist
  IF i549q[] IS INITIAL. RAISE no_period_specified. ENDIF.
*-----Import der Auswertungsergebnisse

  LOOP AT i549q.
*-----Bestimmung der relativen Tage
    IF int_begda GT i549q-begda.
      reday_beg = int_begda   - i549q-begda + 1.
    ELSE.
      reday_beg = i549q-begda - i549q-begda + 1.
    ENDIF.
    IF int_endda LT i549q-endda.
      reday_end = int_endda   - i549q-begda + 1.
    ELSE.
      reday_end = i549q-endda - i549q-begda + 1.
    ENDIF.
*-----Bereitstellen der Auswertungsergebnisse
    time_results-pernr = int_pernr.
    time_results-pabrj = i549q-pabrj.
    time_results-pabrp = i549q-pabrp.
    time_results-begda = i549q-begda.
    time_results-endda = i549q-endda.
    CALL FUNCTION 'HR_TIME_RESULTS_GET'
      EXPORTING
        get_pernr             = time_results-pernr
        get_pabrj             = time_results-pabrj
        get_pabrp             = time_results-pabrp
      IMPORTING
        get_no_record_found   = no_record_found
        get_bezug             = time_results-bezug
        get_kntag             = time_results-kntag
      TABLES
        get_tbuff             = get_tbuff           "XDGAHRK063897
        get_buffer_dir        = get_buffer_dir      "XDGAHRK063897
        get_wpbp              = time_results-wpbp
        get_alp               = time_results-alp
        get_ab                = time_results-ab
        get_sko               = time_results-sko
        get_vert              = time_results-vert
        get_saldo             = time_results-saldo
        get_zes               = time_results-zes
        get_zko               = time_results-zko
        get_fehler            = time_results-fehler
        get_abwkonti          = time_results-abwkonti
        get_psp               = time_results-psp
        get_anwkonti          = time_results-anwkonti
        get_mehr              = time_results-mehr
        get_anwes             = time_results-anwes
        get_rufb              = time_results-rufb
        get_zl                = time_results-zl
        get_urlan             = time_results-urlan
        get_vs                = time_results-vs
        get_cvs               = time_results-cvs
        get_c1                = time_results-c1
        get_at                = time_results-at
        get_pt                = time_results-pt
        get_wst               = time_results-wst
        get_cwst              = time_results-cwst
*{   INSERT         DESK922921                                        1
        get_qtbase            = time_results-qtbase
*}   INSERT
        get_qtacc             = time_results-qtacc
        get_qttrans           = time_results-qttrans
      EXCEPTIONS
        no_period_specified   = 1
        wrong_cluster_version = 2
        no_read_authority     = 3
        cluster_archived      = 4
        technical_error       = 5.
    CASE sy-subrc.
      WHEN 1. RAISE no_period_specified.
      WHEN 2. RAISE wrong_cluster_version.
      WHEN 3. RAISE no_read_authority.
      WHEN 4. RAISE cluster_archived.
      WHEN 5. RAISE technical_error.
      WHEN OTHERS.
        IF no_record_found IS INITIAL.
          IF time_results-bezug-datum LT i549q-begda.       "note520090
            CLEAR time_results-zes[].                       "note520090
            CLEAR time_results-zl[].                        "note520090
            CLEAR time_results-urlan[].                     "note520090
            CLEAR time_results-vs[].                        "note520090
            CLEAR time_results-qtacc[].                     "note520090
            CLEAR time_results-fehler[].                    "note520090
          ELSE.                                             "note520090
            IF time_results-bezug-datum LT int_endda.
              reday_end = time_results-bezug-datum - i549q-begda + 1.
            ENDIF.
            DELETE time_results-zes    WHERE reday LT reday_beg
                                       OR    reday GT reday_end.
            DELETE time_results-zl     WHERE datum LT int_begda
                                       OR    datum GT int_endda.
            DELETE time_results-urlan  WHERE datum LT int_begda
                                       OR    datum GT int_endda.
            DELETE time_results-vs     WHERE datum LT int_begda
                                       OR    datum GT int_endda.
            DELETE time_results-qtacc  WHERE gdate LT int_begda
                                       OR    gdate GT int_endda.
            DELETE time_results-fehler WHERE ldate LT int_begda
                                       OR    ldate GT int_endda.
          ENDIF.                                            "note520090

          CLEAR wa_version.
          CALL FUNCTION 'ZHCM_TIME_RESULTS_VERSION_GET'
            EXPORTING
              get_pernr             = time_results-pernr
              get_pabrj             = time_results-pabrj
              get_pabrp             = time_results-pabrp
            IMPORTING
              get_no_record_found   = no_record_found
              get_version           = wa_version
            EXCEPTIONS
              no_period_specified   = 1
              wrong_cluster_version = 2
              no_read_authority     = 3
              cluster_archived      = 4
              technical_error       = 5.

          CLEAR wa_ptresult.
          MOVE-CORRESPONDING time_results TO wa_ptresult.
          MOVE wa_version TO wa_ptresult-version.
          APPEND wa_ptresult TO int_time_results.
        ENDIF.
    ENDCASE.
  ENDLOOP.

ENDFUNCTION.
