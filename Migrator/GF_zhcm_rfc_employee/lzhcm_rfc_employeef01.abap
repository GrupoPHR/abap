*&---------------------------------------------------------------------*
*&  Include           LZHCM_RFC_EMPLOYEEF01
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  READ_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_PERNR_LIST  text
*      -->P_PRELP       text
*----------------------------------------------------------------------*
FORM read_data  TABLES   p_pernr_list STRUCTURE zcopy_oview_man
                         p_prelp STRUCTURE prelp
                         p_t52mcw STRUCTURE t52mcw
                         p_entit STRUCTURE  zthcm_eq_entit
                         p_log STRUCTURE zhcm_rfc_log.

  DATA: wa_pernr_list LIKE zcopy_oview_man.
  DATA: wa_inftab TYPE t_inftab.
  DATA: t_inft_t TYPE TABLE OF t_inftab.
  DATA: infty_itab_p TYPE REF TO data.
  DATA: l_missing_auth TYPE boole_d.
  DATA: it_prelp TYPE TABLE OF prelp.
  DATA: wa_prelp LIKE prelp.
  DATA: no_auth_check TYPE flag.
  DATA: lv_msg TYPE zhcm_rfc_log-message.

  FIELD-SYMBOLS: <infty_itab_p> TYPE STANDARD TABLE.
  FIELD-SYMBOLS: <ifs>  LIKE LINE OF t_ntab,
                 <wa_p> TYPE any,
                 <fs>   TYPE any.

  GET PARAMETER ID 'ZAUTH_COPY_EE' FIELD no_auth_check.
  IF no_auth_check IS NOT INITIAL.
    no_auth_check = 'X'.
    SET PARAMETER ID 'ZAUTH_COPY_EE' FIELD no_auth_check.
  ENDIF.

  CALL FUNCTION 'ZHCM_GET_INFT_TABLES'
    TABLES
      fm_inft = t_inft_t.

  LOOP AT p_pernr_list INTO wa_pernr_list.

    SELECT * APPENDING CORRESPONDING FIELDS OF TABLE p_t52mcw
      FROM t52mcw WHERE pernr = wa_pernr_list-pernr.
    LOOP AT p_t52mcw WHERE pernr EQ wa_pernr_list-pernr.
      p_t52mcw-pernr = wa_pernr_list-pernr_new.
      MODIFY p_t52mcw.
    ENDLOOP.
    CLEAR: it_prelp.
    LOOP AT t_inft_t INTO wa_inftab.

      CLEAR: infty_itab_p, l_missing_auth.
      UNASSIGN: <infty_itab_p>.

      CREATE DATA infty_itab_p TYPE TABLE OF (wa_inftab-ppnnn).
      CHECK infty_itab_p IS NOT INITIAL.

      ASSIGN infty_itab_p->* TO <infty_itab_p>.
      CHECK sy-subrc = 0.

      CALL FUNCTION 'HR_READ_SUBTYPE'
        EXPORTING
          tclas           = 'A'
          pernr           = wa_pernr_list-pernr
          infty           = wa_inftab-infty
          subty           = '*'
          sprps           = '*'
          begda           = '18000101'
          endda           = '99991231'
          bypass_buffer   = ' '
          no_auth_check   = no_auth_check
          legacy_mode     = ' '
        IMPORTING
          missing_auth    = l_missing_auth
        TABLES
          infty_tab       = <infty_itab_p>
        EXCEPTIONS
          infty_not_found = 1
          invalid_input   = 2
          OTHERS          = 3.
      IF sy-subrc <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
      ENDIF.

      IF l_missing_auth IS NOT INITIAL.
        EXIT.
      ENDIF.

      LOOP AT <infty_itab_p> ASSIGNING <wa_p>.
        UNASSIGN <fs>.
        ASSIGN COMPONENT 'PERNR' OF STRUCTURE <wa_p> TO <fs>.
        <fs> = wa_pernr_list-pernr_new.
        CASE wa_inftab-infty.
          WHEN '0835'.
            PERFORM tab_components USING wa_inftab-ppnnn.
            LOOP AT t_ntab ASSIGNING <ifs>..
              CHECK <ifs>-fieldname(3) EQ 'LIF'.
              UNASSIGN <fs>.
              ASSIGN COMPONENT <ifs>-fieldname OF STRUCTURE <wa_p> TO <fs>.
              IF ( <fs> IS ASSIGNED ) AND ( <fs> IS NOT INITIAL ).
                READ TABLE p_entit WITH KEY entit_old = <fs>.
                IF sy-subrc EQ 0.
                  <fs> = p_entit-entit_new.
                ELSE.
                  CLEAR: lv_msg, l_missing_auth.
                  CONCATENATE text-ent <fs> text-trx INTO lv_msg SEPARATED BY space.
                  PERFORM fill_log TABLES p_log USING 'ERROR' wa_pernr_list lv_msg.
                ENDIF.
              ENDIF.
            ENDLOOP.
          WHEN OTHERS.
        ENDCASE.

        CALL METHOD cl_hr_pnnnn_type_cast=>pnnnn_to_prelp
          EXPORTING
            pnnnn = <wa_p>
          IMPORTING
            prelp = wa_prelp.

        IF wa_prelp IS NOT INITIAL.
          APPEND wa_prelp TO it_prelp.
        ENDIF.

      ENDLOOP.

    ENDLOOP.        "t_inft_t INTO wa_inftab

    IF l_missing_auth IS NOT INITIAL.
      wa_pernr_list-i_okimm = 'ERROR'.
      MODIFY p_pernr_list FROM wa_pernr_list.
    ELSE.
      wa_pernr_list-i_okimm = 'EXITO'.
      MODIFY p_pernr_list FROM wa_pernr_list.
      APPEND LINES OF it_prelp TO p_prelp.
    ENDIF.

    IF ( wa_pernr_list-pernr_d IS NOT INITIAL ).
      READ TABLE t_inft_t INTO wa_inftab WITH KEY infty = '9103'.
      IF sy-subrc EQ 0.
        CLEAR: infty_itab_p, l_missing_auth.
        UNASSIGN: <infty_itab_p>.

        CREATE DATA infty_itab_p TYPE TABLE OF (wa_inftab-ppnnn).
        CHECK infty_itab_p IS NOT INITIAL.

        ASSIGN infty_itab_p->* TO <infty_itab_p>.
        CHECK sy-subrc = 0.

        CALL FUNCTION 'HR_READ_SUBTYPE'
          EXPORTING
            tclas           = 'A'
            pernr           = wa_pernr_list-pernr_d
            infty           = wa_inftab-infty
            subty           = '*'
            sprps           = '*'
            begda           = '18000101'
            endda           = '99991231'
            bypass_buffer   = ' '
            no_auth_check   = no_auth_check
            legacy_mode     = ' '
          IMPORTING
            missing_auth    = l_missing_auth
          TABLES
            infty_tab       = <infty_itab_p>
          EXCEPTIONS
            infty_not_found = 1
            invalid_input   = 2
            OTHERS          = 3.
        IF sy-subrc <> 0.
*   MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*           WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
        ENDIF.

        IF l_missing_auth IS NOT INITIAL.
          EXIT.
        ENDIF.

        LOOP AT <infty_itab_p> ASSIGNING <wa_p>.
          UNASSIGN <fs>.
          ASSIGN COMPONENT 'PERNR' OF STRUCTURE <wa_p> TO <fs>.
          <fs> = wa_pernr_list-pernr_new.

          CALL METHOD cl_hr_pnnnn_type_cast=>pnnnn_to_prelp
            EXPORTING
              pnnnn = <wa_p>
            IMPORTING
              prelp = wa_prelp.
          IF wa_prelp IS NOT INITIAL.
            APPEND wa_prelp TO p_prelp.
          ENDIF.
        ENDLOOP.
      ENDIF.
    ENDIF.

  ENDLOOP.        "p_pernr_list

  CLEAR: t_inft_t.
  UNASSIGN: <infty_itab_p>.

ENDFORM.                    " READ_DATA

*&---------------------------------------------------------------------*
*&      Form  read_assob_hr
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_PERNR_LIST  text
*      -->P_ASSOB_HR    text
*----------------------------------------------------------------------*
FORM read_assob_hr TABLES p_pernr_list STRUCTURE zcopy_oview_man
                          p_assob_hr STRUCTURE zcopy_assobhr.

  CLEAR: p_assob_hr, p_assob_hr[].
  LOOP AT p_pernr_list.
    SELECT * APPENDING CORRESPONDING FIELDS OF TABLE p_assob_hr
      FROM asshr
      INNER JOIN assob
        ON assob~pdsnr EQ asshr~pdsnr
      INNER JOIN pdsnr
        ON pdsnr~pdsnr EQ asshr~pdsnr
      WHERE asshr~pernr EQ p_pernr_list-pernr.
    LOOP AT p_assob_hr WHERE pernr EQ p_pernr_list-pernr.
      p_assob_hr-pernr = p_pernr_list-pernr_new.
      MODIFY p_assob_hr.
    ENDLOOP.
  ENDLOOP.

ENDFORM.                    "read_assob_hr

*&---------------------------------------------------------------------*
*&      Form  READ_CLUSTER
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_PERNR_LIST    text
*      -->P_PAYRESULT     text
*      -->P_PAYCU_RESULT  text
*      -->P_HRPY_WPBP     text
*      -->P_HRPY_RGDIR    text
*----------------------------------------------------------------------*
FORM read_cluster TABLES  p_pernr_list STRUCTURE zcopy_oview_man
                          p_payresult TYPE zhrpaymx_tab_of_results
                          p_paycu_result TYPE zhrpaycu_tab_of_results
                          p_entit STRUCTURE  zthcm_eq_entit
                          p_log STRUCTURE zhcm_rfc_log.

  DATA: wa_pernr_list LIKE zcopy_oview_man.
  DATA: lv_relid LIKE pcl2-relid.
  DATA: lt_paymxresult TYPE paymx_result.
  DATA: wa_payresult LIKE lt_paymxresult.
  DATA: no_auth_check TYPE flag.
  DATA: it_rgdir LIKE TABLE OF rgdir WITH HEADER LINE.
  DATA: wa_payresult_d LIKE lt_paymxresult,
        it_rt          TYPE TABLE OF pc207 WITH HEADER LINE,
        it_rt_d        TYPE TABLE OF pc207 WITH HEADER LINE,
        it_v0          TYPE TABLE OF pc20c WITH HEADER LINE,
        it_v0_d        TYPE TABLE OF pc20c WITH HEADER LINE.
  DATA: wa_wpbp   TYPE hrpy_wpbp,
        wa_rgdir  TYPE hrpy_rgdir.
  DATA: lv_msg TYPE zhcm_rfc_log-message.

  GET PARAMETER ID 'ZAUTH_COPY_EE' FIELD no_auth_check.
  IF no_auth_check IS NOT INITIAL.
    no_auth_check = ' '.
    SET PARAMETER ID 'ZAUTH_COPY_EE' FIELD no_auth_check.
  ELSE.
    no_auth_check = 'X'.
  ENDIF.

  LOOP AT p_pernr_list INTO wa_pernr_list.

    PERFORM read_relid USING wa_pernr_list-molga lv_relid.
    IF wa_pernr_list-pernr_d IS NOT INITIAL.
      REFRESH: it_rgdir.
      PERFORM recd USING wa_pernr_list-pernr_d.
      IF rp-imp-cd-subrc = 0.
        it_rgdir[] = rgdir[].
        REFRESH rgdir.
      ENDIF.
    ENDIF.
    PERFORM recd USING wa_pernr_list-pernr.
    CHECK rp-imp-cd-subrc = 0.

    CLEAR p_paycu_result.
    MOVE wa_pernr_list-pernr_new TO p_paycu_result-perid.
    MOVE-CORRESPONDING ocd-version TO p_paycu_result-version.
    MOVE cd-next_seq TO p_paycu_result-next_seq.
    MOVE cd-last_pay TO p_paycu_result-last_pay.
    MOVE %%_dir2[] TO p_paycu_result-dir[].

    LOOP AT rgdir.

      CLEAR: p_payresult, wa_payresult.
      CALL FUNCTION 'PYXX_READ_PAYROLL_RESULT'
        EXPORTING
          clusterid                    = lv_relid
          employeenumber               = wa_pernr_list-pernr
          sequencenumber               = rgdir-seqnr
*         READ_ONLY_BUFFER             = ' '
          read_only_international      = ' '
          check_read_authority         = no_auth_check
        CHANGING
          payroll_result               = wa_payresult
        EXCEPTIONS
          illegal_isocode_or_clusterid = 1
          error_generating_import      = 2
          import_mismatch_error        = 3
          subpool_dir_full             = 4
          no_read_authority            = 5
          no_record_found              = 6
          versions_do_not_match        = 7
          OTHERS                       = 8.

      IF sy-subrc IS NOT INITIAL.
        EXIT.
      ENDIF.

      IF wa_pernr_list-pernr_d IS NOT INITIAL.
        READ TABLE it_rgdir WITH KEY srtza = 'A' fpper = rgdir-fpper fpend = rgdir-fpend.
        IF sy-subrc IS INITIAL.
          CLEAR: wa_payresult_d, it_rt_d, it_rt, it_v0, it_v0_d.
          REFRESH: it_rt_d, it_rt, it_v0, it_v0_d.
          CALL FUNCTION 'PYXX_READ_PAYROLL_RESULT'
            EXPORTING
              clusterid                    = lv_relid
              employeenumber               = wa_pernr_list-pernr_d
              sequencenumber               = it_rgdir-seqnr
*             READ_ONLY_BUFFER             = ' '
              read_only_international      = ' '
              check_read_authority         = no_auth_check
            CHANGING
              payroll_result               = wa_payresult_d
            EXCEPTIONS
              illegal_isocode_or_clusterid = 1
              error_generating_import      = 2
              import_mismatch_error        = 3
              subpool_dir_full             = 4
              no_read_authority            = 5
              no_record_found              = 6
              versions_do_not_match        = 7
              OTHERS                       = 8.
          wa_payresult-inter-rt[] = wa_payresult_d-inter-rt.
          wa_payresult-inter-v0[] = wa_payresult_d-inter-v0.
        ENDIF.
      ENDIF.

      MOVE-CORRESPONDING rgdir TO wa_payresult-evp.
      IF ( wa_payresult-evp-person IS NOT INITIAL ) AND ( wa_payresult-evp-person EQ wa_pernr_list-pernr ).
        wa_payresult-evp-person = wa_pernr_list-pernr_new.
      ENDIF.
      IF ( wa_payresult-evp-persdata IS NOT INITIAL ) AND ( wa_payresult-evp-persdata EQ wa_pernr_list-pernr ).
        wa_payresult-evp-persdata = wa_pernr_list-pernr_new.
      ENDIF.
      IF ( wa_payresult-inter-versc-person IS NOT INITIAL ) AND ( wa_payresult-inter-versc-person EQ wa_pernr_list-pernr ).
        wa_payresult-inter-versc-person = wa_pernr_list-pernr_new.
      ENDIF.
      IF ( wa_payresult-inter-versc-persdata IS NOT INITIAL ) AND ( wa_payresult-inter-versc-persdata EQ wa_pernr_list-pernr ).
        wa_payresult-inter-versc-persdata = wa_pernr_list-pernr_new.
      ENDIF.
      MOVE wa_pernr_list-pernr_new TO p_payresult-perid.
      MOVE-CORRESPONDING wa_payresult TO p_payresult.
      APPEND p_payresult.

    ENDLOOP.

    IF sy-subrc = 0.
      SELECT * APPENDING CORRESPONDING FIELDS OF TABLE
        p_paycu_result-hrpy_wpbp
        FROM hrpy_wpbp WHERE pernr = wa_pernr_list-pernr.

      LOOP AT p_paycu_result-hrpy_wpbp INTO wa_wpbp.
        wa_wpbp-pernr = wa_pernr_list-pernr_new.
        MODIFY p_paycu_result-hrpy_wpbp FROM wa_wpbp.
      ENDLOOP.

      SELECT * APPENDING CORRESPONDING FIELDS OF TABLE
        p_paycu_result-hrpy_rgdir
        FROM hrpy_rgdir WHERE pernr = wa_pernr_list-pernr.
      LOOP AT p_paycu_result-hrpy_rgdir INTO wa_rgdir.
        wa_rgdir-pernr = wa_pernr_list-pernr_new.
        wa_rgdir-person = wa_pernr_list-pernr_new.
        wa_rgdir-persdata = wa_pernr_list-pernr_new.
        MODIFY p_paycu_result-hrpy_rgdir FROM wa_rgdir.
      ENDLOOP.

      APPEND p_paycu_result.
    ENDIF.

  ENDLOOP.

ENDFORM.                    "read_cluster

*&---------------------------------------------------------------------*
*&      Form  read_relid
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->VALUE(IV_MOLGA)  text
*      -->OV_RELID         text
*----------------------------------------------------------------------*
FORM read_relid USING VALUE(iv_molga) LIKE t500l-molga
                      ov_relid LIKE t500l-relid.

  TABLES: t500l.
  SELECT SINGLE * FROM t500l WHERE molga = iv_molga.
  ov_relid = t500l-relid.

ENDFORM.                    "read_relid

*&---------------------------------------------------------------------*
*&      Form  read_b2
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_PERNR_LIST  text
*      -->P_PTRESULT    text
*----------------------------------------------------------------------*
FORM read_b2 TABLES p_pernr_list STRUCTURE zcopy_oview_man
                    p_ptresult TYPE zhrptxx_tab_of_results
                    p_quoded STRUCTURE zptquoded.

  DATA: time_results TYPE ptm_time_results.
  DATA: e_date LIKE sy-datum VALUE '99991231'.
  DATA: o_date LIKE sy-datum.
  DATA: no_auth_check TYPE flag.
  DATA: it_time_ee TYPE TABLE OF zptxx_result_pernr.
  DATA: it_quoded TYPE TABLE OF zptquoded WITH HEADER LINE.

  GET PARAMETER ID 'ZAUTH_COPY_EE' FIELD no_auth_check.
  IF no_auth_check IS NOT INITIAL.
    no_authority_check_cluster = 'X'.
    no_auth_check = 'X'.
    SET PARAMETER ID 'ZAUTH_COPY_EE' FIELD no_auth_check.
  ENDIF.

  LOOP AT p_pernr_list.
    REFRESH: it_time_ee, it_quoded.
    CALL FUNCTION 'ZHCM_GET_OLDEST_DATE'
      EXPORTING
        pernr           = p_pernr_list-pernr
      IMPORTING
        oldest_date     = o_date
      EXCEPTIONS
        pernr_not_found = 1
        OTHERS          = 2.
    IF sy-subrc <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
    ENDIF.

    CALL FUNCTION 'ZHCM_TIME_RESULTS_IN_INTERVAL'
      EXPORTING
        int_pernr             = p_pernr_list-pernr
        int_begda             = o_date
        int_endda             = e_date
      TABLES
        int_time_results      = it_time_ee
      EXCEPTIONS
        wrong_cluster_version = 1
        no_read_authority     = 2
        cluster_archived      = 3
        technical_error       = 4
        OTHERS                = 5.
    SELECT * INTO CORRESPONDING FIELDS OF TABLE it_quoded
      FROM ptquoded
     WHERE pernr EQ p_pernr_list-pernr.
    IF sy-subrc IS INITIAL.
      LOOP AT it_quoded.
        it_quoded-pernr = p_pernr_list-pernr_new.
        APPEND it_quoded TO p_quoded.
      ENDLOOP.
    ENDIF.
    LOOP AT it_time_ee INTO p_ptresult.
      IF p_ptresult-pernr EQ p_pernr_list-pernr.
        p_ptresult-pernr = p_pernr_list-pernr_new.
        APPEND p_ptresult. CLEAR p_ptresult.
      ENDIF.
    ENDLOOP.
  ENDLOOP.

ENDFORM.                                                    "read_b2
*&---------------------------------------------------------------------*
*&      Form  BUILD_INFTY_RANGE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM build_infty_range .
  REFRESH: ra_infty.
  CLEAR: ra_infty.

  ra_infty-sign = 'I'.
  ra_infty-option = 'EQ'.
  ra_infty-low = '0001'.
  APPEND ra_infty.
  ra_infty-low = '0003'.
  APPEND ra_infty.
  ra_infty-low = '0007'.
  APPEND ra_infty.
  ra_infty-low = '0008'.
  APPEND ra_infty.
  ra_infty-low = '0009'.
  APPEND ra_infty.
  ra_infty-low = '0014'.
  APPEND ra_infty.
  ra_infty-low = '0015'.
  APPEND ra_infty.
  ra_infty-low = '0016'.
  APPEND ra_infty.
  ra_infty-low = '0019'.
  APPEND ra_infty.
  ra_infty-low = '0021'.
  APPEND ra_infty.
  ra_infty-low = '0022'.
  APPEND ra_infty.
  ra_infty-low = '0027'.
  APPEND ra_infty.
*  ra_infty-low = '0034'.
*  APPEND ra_infty.
  ra_infty-low = '0035'.
  APPEND ra_infty.
  ra_infty-low = '0041'.
  APPEND ra_infty.
  ra_infty-low = '0045'.
  APPEND ra_infty.
  ra_infty-low = '0057'.
  APPEND ra_infty.
  ra_infty-low = '0078'.
  APPEND ra_infty.
  ra_infty-low = '0105'.
  APPEND ra_infty.
*  ra_infty-low = '0185'.
*  APPEND ra_infty.
  ra_infty-low = '0267'.
  APPEND ra_infty.
  ra_infty-low = '0302'.
  APPEND ra_infty.
  ra_infty-low = '0416'.
  APPEND ra_infty.
  ra_infty-low = '0804'.
  APPEND ra_infty.
  ra_infty-low = '0829'.
  APPEND ra_infty.
  ra_infty-low = '0830'.
  APPEND ra_infty.
  ra_infty-low = '0832'.
  APPEND ra_infty.
  ra_infty-low = '0834'.
  APPEND ra_infty.
  ra_infty-low = '0835'.
  APPEND ra_infty.
  ra_infty-low = '0836'.
  APPEND ra_infty.
  ra_infty-low = '0838'.
  APPEND ra_infty.
  ra_infty-low = '0839'.
  APPEND ra_infty.
  ra_infty-low = '0847'.
  APPEND ra_infty.
  ra_infty-low = '2001'.
  APPEND ra_infty.
  ra_infty-low = '2006'.
  APPEND ra_infty.
  ra_infty-low = '2010'.
  APPEND ra_infty.
  ra_infty-low = '9101'.
  APPEND ra_infty.
  ra_infty-low = '9102'.
  APPEND ra_infty.
  ra_infty-low = '9103'.
  APPEND ra_infty.
  ra_infty-low = '9104'.
  APPEND ra_infty.
  ra_infty-low = '9105'.
  APPEND ra_infty.
  ra_infty-low = '9106'.
  APPEND ra_infty.

  CLEAR ra_infty.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  TAB_COMPONENTS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_1668   text
*----------------------------------------------------------------------*
FORM tab_components  USING p_in TYPE dntab-tabname.

  REFRESH t_ntab.

  CALL FUNCTION 'NAMETAB_GET'
    EXPORTING
*     LANGU               = SY-LANGU
*     ONLY                = ' '
      tabname             = p_in
*   IMPORTING
*     HEADER              =
*     RC                  =
    TABLES
      nametab             = t_ntab[]
    EXCEPTIONS
      internal_error      = 1
      table_has_no_fields = 2
      table_not_activ     = 3
      no_texts_found      = 4
      OTHERS              = 5.
  IF sy-subrc <> 0.
* Implement suitable error handling here
  ENDIF.

ENDFORM.                    " TAB_COMPONENTS

*&---------------------------------------------------------------------*
*&      Form  FILL_LOG
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_0327   text
*      -->P_WA_PERNR_LIST  text
*      -->P_LV_MSG  text
*----------------------------------------------------------------------*
FORM fill_log  TABLES p_log STRUCTURE zhcm_rfc_log
                USING p_id
                      p_pernr_list TYPE zcopy_oview_man
                      p_msg.

  p_log-id = p_id.
  p_log-pernr_new = p_pernr_list-pernr_new.
  p_log-pernr_old = p_pernr_list-pernr.
  p_log-message = p_msg.
  APPEND p_log.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  READ_1001_CP
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_PERNR_LIST  text
*      -->P_1001  text
*----------------------------------------------------------------------*
FORM read_1001_cp  TABLES   p_pernr_list STRUCTURE zcopy_oview_man
                            p_1000 STRUCTURE hrp1000
                            p_1001 STRUCTURE hrp1001.
  DATA: wa_pernr_list LIKE zcopy_oview_man.
  DATA: it_1001 TYPE TABLE OF hrp1001.
  DATA: wa_1001 LIKE hrp1001.
  LOOP AT  p_pernr_list INTO wa_pernr_list.
    SELECT * APPENDING TABLE it_1001
      FROM hrp1001
     WHERE otype EQ 'P'
       AND objid EQ wa_pernr_list-pernr
       AND plvar EQ '01'
       AND rsign EQ 'A'
       AND relat EQ '209'
       AND sclas EQ 'CP'.
  ENDLOOP.

  p_1001[] = it_1001[].

  LOOP AT p_1001.
    SELECT * APPENDING TABLE it_1001
      FROM hrp1001
     WHERE otype EQ 'CP'
       AND objid EQ p_1001-sobid
       AND plvar EQ '01'
       AND rsign EQ 'B'
       AND relat EQ '209'
       AND sclas EQ 'P'.
  ENDLOOP.

  REFRESH p_1001.
  LOOP AT  p_pernr_list INTO wa_pernr_list.
    LOOP AT it_1001 INTO wa_1001 WHERE objid EQ wa_pernr_list-pernr
                                    OR sobid EQ wa_pernr_list-pernr.
      CLEAR: wa_1001-mandt.
      CASE wa_1001-otype.
        WHEN 'CP'.
          wa_1001-sobid = wa_pernr_list-pernr_new.
        WHEN 'P'.
          wa_1001-objid = wa_pernr_list-pernr_new.
      ENDCASE.
      APPEND wa_1001 TO p_1001.
    ENDLOOP.
  ENDLOOP.

  LOOP AT it_1001 INTO wa_1001 WHERE otype EQ 'CP'.
    SELECT * APPENDING TABLE p_1000
      FROM hrp1000
     WHERE plvar EQ wa_1001-plvar
       AND otype EQ wa_1001-otype
       AND objid EQ wa_1001-objid.
  ENDLOOP.

ENDFORM.
