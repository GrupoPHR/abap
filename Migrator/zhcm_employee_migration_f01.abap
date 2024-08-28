*&---------------------------------------------------------------------*
*&  Include           ZHCM_EMPLOYEE_MIGRATION_F01
*&---------------------------------------------------------------------*

DEFINE zrp-exp-c2-b2.
***************************************
* CLUSTER B2: BDE-DATEN (MONAT)
***************************************
  pcl2-versn = b2-version-number.
  b2-version-saprl  =                                       "L6BK003229
               cl_pt_cluster_util=>get_release( 'SAP_HR' ). "L6BK003229
  b2-version-uname  = sy-uname.
  b2-version-datum  = sy-datum.
  b2-version-uzeit  = sy-uzeit.
  b2-version-pgmid  = sy-repid.
  export b2-version
         zl
         kntag
         pt
         at
*         wst
*         cwst
         saldo
         zes
         zko
         anwkonti
         abwkonti
         fehler
         psp
         wpbp
         ab
         vert
         bezug
         sko
         mehr
         anwes
         rufb
         c1
         alp
         urlan
         vs
         cvs
         qtacc                                              "ALRK007297
         qttrans                                            "ALRK007297
         qtbase                                             "PH4K007041
  to database pcl2(b2)
  id b2-key using pcl2_exp_imp.
  rp-imp-b2-subrc = sy-subrc.
END-OF-DEFINITION.

*&---------------------------------------------------------------------*
*&      Form  GET_EE_MIGRA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_ee_migra TABLES p_chsn_ee STRUCTURE zcopy_oview_man.
  DATA: iv_index TYPE i,
        it_eetab TYPE zcopy_oview_man.

  SELECT * FROM zthcm_eq_migrar
    WHERE pernr_new IN so_pernr
      AND umain_data EQ 'X'.
    p_chsn_ee-pernr = zthcm_eq_migrar-pernr_old.
    p_chsn_ee-pernr_new = zthcm_eq_migrar-pernr_new.
    p_chsn_ee-molga = '32'.
    APPEND p_chsn_ee. CLEAR p_chsn_ee.
  ENDSELECT.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  IMPORT_VIA_RFC
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM import_via_rfc TABLES p_chsn_ee STRUCTURE zcopy_oview_man.

  DATA: it_prelp        TYPE TABLE OF prelp.
  DATA: it_t52mcw       TYPE TABLE OF t52mcw.
  DATA: it_payresult    TYPE          zhrpaymx_tab_of_results.
  DATA: it_ptresult     TYPE          zhrptxx_tab_of_results.
  DATA: it_paycu_result TYPE          zhrpaycu_tab_of_results.
  DATA: it_pcl4_lo      TYPE          zhrpcl4_tab_audit_log_lo.
  DATA: it_pcl4_sh      TYPE          zhrpcl4_tab_audit_log_sh.
  DATA: it_assob_hr     TYPE TABLE OF zcopy_assobhr.
  DATA: it_pernr TYPE TABLE OF zcopy_oview_man,
        wa_pernr TYPE          zcopy_oview_man.
  DATA: it_entit TYPE TABLE OF zthcm_eq_entit.
  DATA: it_qouded TYPE TABLE OF zptquoded.
  DATA: it_1000 TYPE TABLE OF hrp1000.
  DATA: it_1001 TYPE TABLE OF hrp1001.
  DATA: iv_flag.


  REFRESH: it_pernr, gt_log.
  it_pernr[] = p_chsn_ee[].

  SELECT * INTO TABLE it_entit
    FROM zthcm_eq_entit.
  MESSAGE s999(st) WITH text-m06.
  CALL FUNCTION 'ZHCM_EMPLOYEE_RFC' DESTINATION p_rfc
    IMPORTING
      fm_payresult      = it_payresult[]
      fm_paycu_result   = it_paycu_result[]
      fm_pcl4_lo        = it_pcl4_lo[]
      fm_pcl4_sh        = it_pcl4_sh[]
      fm_ptresult       = it_ptresult[]
    TABLES
      fm_pernr_list     = it_pernr
      fm_prelp          = it_prelp
      fm_t52mcw         = it_t52mcw
      fm_assob_hr       = it_assob_hr
      fm_entit          = it_entit
      fm_qouded         = it_qouded
      fm_1000           = it_1000
      fm_1001           = it_1001
      fm_log            = gt_log
    EXCEPTIONS
      no_pernr_found    = 1
      no_pernr_selected = 2
      OTHERS            = 3.

  IF sy-subrc <> 0.
    CLEAR wa_log.
    CASE sy-subrc.
      WHEN 1.
        wa_log-id = 'ERROR'.
        wa_log-message = 'No se encontr  ning n n mero de personal'.
      WHEN 2.
        wa_log-id = 'ERROR'.
        wa_log-message = 'Seleccionar un c digo de personal'.
      WHEN OTHERS.
        wa_log-id = 'ERROR'.
        wa_log-message = 'Error al llamar RFC'.
    ENDCASE.
    APPEND wa_log TO gt_log.
  ELSE.
    MESSAGE s999(st) WITH text-m07.
    CLEAR iv_flag.
    PERFORM upd_master_data TABLES it_pernr
                                   it_prelp
                                   it_t52mcw
                          CHANGING iv_flag.

    PERFORM update_assob_hr TABLES it_pernr
                                   it_assob_hr.

    PERFORM update_cluster TABLES it_pernr
                                  it_payresult
                                  it_paycu_result.

    PERFORM update_ptresult TABLES it_pernr
                                   it_ptresult
                                   it_qouded.

    PERFORM update_1001_cp TABLES it_pernr
                                  it_1000
                                  it_1001.

  ENDIF.

ENDFORM.                    "IMPORT_VIA_RFC


*&---------------------------------------------------------------------*
*&      Form  UPD_MASTER_DATA
*&---------------------------------------------------------------------*
FORM upd_master_data TABLES p_pernr_list STRUCTURE zcopy_oview_man
                            p_prelp      STRUCTURE prelp
                            p_t52mcw     STRUCTURE t52mcw
                            CHANGING p_flag.

  DATA: t_inft_t      TYPE TABLE OF t_inftab.
  DATA: wa_pernr_list LIKE          zcopy_oview_man.
  DATA: wa_inftab     TYPE          t_inftab.
  DATA: wa_prelp      LIKE          prelp.
  DATA: infty_itab_pn TYPE REF TO   data.
  DATA: infty_itab_pa TYPE REF TO   data.

  FIELD-SYMBOLS: <pannnn>.
  FIELD-SYMBOLS: <pnnnn>.

  CALL FUNCTION 'ZHCM_GET_INFT_TABLES'
    TABLES
      fm_inft = t_inft_t.

  SORT p_pernr_list BY pernr ASCENDING.
  SORT p_prelp      BY pernr ASCENDING
                       infty ASCENDING.
  MESSAGE s999(st) WITH text-m08.
  LOOP AT p_pernr_list INTO wa_pernr_list  WHERE i_okimm NE 'ERROR'.
    CLEAR p_flag.

    LOOP AT p_prelp INTO wa_prelp WHERE pernr = wa_pernr_list-pernr_new.

      AT NEW infty.
        CLEAR wa_inftab.
        READ TABLE t_inft_t INTO wa_inftab WITH KEY infty = wa_prelp-infty.
        DELETE FROM (wa_inftab-dbtab)
         WHERE pernr = wa_pernr_list-pernr_new.
      ENDAT.

*      CLEAR wa_inftab.
*      READ TABLE t_inft_t INTO wa_inftab WITH KEY infty = wa_prelp-infty.

*      DELETE FROM (wa_inftab-dbtab) WHERE pernr = wa_pernr_list-pernr.
*      IF sy-subrc EQ 0.
      CREATE DATA infty_itab_pn TYPE (wa_inftab-ppnnn).
      CHECK infty_itab_pn IS NOT INITIAL.

      CREATE DATA infty_itab_pa TYPE (wa_inftab-dbtab).
      CHECK infty_itab_pa IS NOT INITIAL.

      ASSIGN infty_itab_pn->* TO <pnnnn>.
      CHECK sy-subrc = 0.

      ASSIGN infty_itab_pa->* TO <pannnn>.
      CHECK sy-subrc = 0.

      CALL METHOD cl_hr_pnnnn_type_cast=>prelp_to_pnnnn
        EXPORTING
          prelp = wa_prelp
        IMPORTING
          pnnnn = <pnnnn>.

      MOVE-CORRESPONDING <pnnnn> TO <pannnn>.
      CHECK <pannnn> IS NOT INITIAL.
      <pannnn>(3) = sy-mandt.
      INSERT (wa_inftab-dbtab) FROM <pannnn>.

      LOOP AT p_t52mcw WHERE pernr = wa_pernr_list-pernr_new.
        DELETE FROM t52mcw WHERE pernr = wa_pernr_list-pernr_new.
        INSERT t52mcw FROM p_t52mcw.
      ENDLOOP.
      p_flag = 1.
*      ENDIF.
    ENDLOOP.
    CLEAR wa_log.
    IF p_flag EQ 0.
      wa_log-id      = 'ERROR'.
      wa_log-pernr_new   = wa_pernr_list-pernr_new.
      wa_log-pernr_old   = wa_pernr_list-pernr.
      wa_log-message = 'No se importaron datos Maestros'.
*     Para identificar los empelados que no se le copio data maestra
      wa_pernr_list-i_okimm = icon_led_red.
      MODIFY p_pernr_list FROM wa_pernr_list.
    ELSE.
      wa_log-id      = 'EXITO'.
      wa_log-pernr_new   = wa_pernr_list-pernr_new.
      wa_log-pernr_old   = wa_pernr_list-pernr.
      wa_log-message = 'Se importaron datos Maestros Correctamente'.
    ENDIF.
    APPEND wa_log TO gt_log.
  ENDLOOP.
  MESSAGE s999(st) WITH text-m09.
ENDFORM.                    " UPD_MASTER_DATA


*&---------------------------------------------------------------------*
*&      Form  update_assob_hr
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_CHSN_EE  text
*      -->P_ASSOB_HR text
*----------------------------------------------------------------------*
FORM update_assob_hr TABLES p_chsn_ee STRUCTURE zcopy_oview_man
                            p_assob_hr STRUCTURE zcopy_assobhr.

  DATA: it_pdsnr TYPE TABLE OF pdsnr,
        it_asshr TYPE TABLE OF asshr,
        it_assob TYPE TABLE OF assob.
  FIELD-SYMBOLS: <fs> TYPE any,
                 <f1> TYPE any.
  MESSAGE s999(st) WITH text-m10.
  LOOP AT p_chsn_ee WHERE i_okimm NE 'ERROR'.
    REFRESH it_pdsnr.
    UNASSIGN: <fs>, <f1>.
* Proceso de borrado de datos para el empelado.
    SELECT pdsnr INTO CORRESPONDING FIELDS OF TABLE it_pdsnr
      FROM assob_hr
     WHERE pernr EQ p_chsn_ee-pernr_new.
    IF sy-subrc IS INITIAL.
      LOOP AT it_pdsnr ASSIGNING <fs>.
        UNASSIGN <f1>.
        ASSIGN COMPONENT 'PDSNR' OF STRUCTURE <fs> TO <f1> CASTING TYPE pdsnr-pdsnr.
        IF <f1> IS ASSIGNED.
          DELETE FROM pdsnr WHERE pdsnr EQ <f1>.
          DELETE FROM asshr WHERE pdsnr EQ <f1>
                              AND pernr EQ p_chsn_ee-pernr_new.
          DELETE FROM assob WHERE pdsnr EQ <f1>
                              AND pernr EQ p_chsn_ee-pernr_new.
        ENDIF.
      ENDLOOP.
    ENDIF.
*  Inserccion de nuevos datos
    REFRESH: it_pdsnr, it_asshr, it_assob.

    LOOP AT p_assob_hr WHERE pernr EQ p_chsn_ee-pernr_new.
      APPEND INITIAL LINE TO it_pdsnr ASSIGNING <fs>.
      MOVE-CORRESPONDING p_assob_hr TO <fs>.
      UNASSIGN <fs>.
      APPEND INITIAL LINE TO it_asshr ASSIGNING <fs>.
      MOVE-CORRESPONDING p_assob_hr TO <fs>.
      UNASSIGN <fs>.
      APPEND INITIAL LINE TO it_assob ASSIGNING <fs>.
      MOVE-CORRESPONDING p_assob_hr TO <fs>.
      UNASSIGN <fs>.
    ENDLOOP.
    IF it_pdsnr[] IS NOT INITIAL.
      INSERT pdsnr FROM TABLE it_pdsnr.
      IF sy-subrc IS INITIAL.
        INSERT asshr FROM TABLE it_asshr.
        INSERT assob FROM TABLE it_assob.
      ENDIF.
    ENDIF.

  ENDLOOP.
  MESSAGE s999(st) WITH text-m11.
ENDFORM.                    "update_assob_hr
*&---------------------------------------------------------------------*
*&      Form  update_cluster
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_CHSN_EE       text
*      -->P_PAYRESULT     text
*      -->P_PAYCU_RESULT  text
*----------------------------------------------------------------------*
FORM update_cluster TABLES p_chsn_ee STRUCTURE zcopy_oview_man
                           p_payresult    TYPE zhrpaymx_tab_of_results
                           p_paycu_result TYPE zhrpaycu_tab_of_results.

  DATA: lv_relid LIKE pcl2-relid.
  DATA: it_rgdir TYPE TABLE OF pc261.
  MESSAGE s999(st) WITH text-m12.
  LOOP AT p_chsn_ee WHERE i_okimm NE 'ERROR' OR i_okimn NE 'ERROR'.

    PERFORM read_relid USING p_chsn_ee-molga lv_relid.
    PERFORM delete_co USING p_chsn_ee-pernr_new lv_relid.
    PERFORM delete_cu USING p_chsn_ee-pernr_new.

    PERFORM insert_co TABLES p_payresult it_rgdir
                       USING p_chsn_ee-pernr_new lv_relid
                             p_chsn_ee-molga.
    PERFORM insert_cu TABLES p_paycu_result it_rgdir
                       USING p_chsn_ee-pernr_new.
  ENDLOOP.
  MESSAGE s999(st) WITH text-m13.
ENDFORM.                    "update_cluster

*&---------------------------------------------------------------------*
*&      Form  update_ptresult
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_CHSN_EE  text
*      -->P_PTRESULT text
*----------------------------------------------------------------------*
FORM update_ptresult TABLES p_chsn_ee STRUCTURE zcopy_oview_man
                            p_ptresult TYPE zhrptxx_tab_of_results
                            p_quoded STRUCTURE zptquoded.

  RANGES: ra_srtfd FOR pcl2-srtfd.
  MESSAGE s999(st) WITH text-m14.
* Borrado de cluster
  LOOP AT p_chsn_ee.
    IF p_chsn_ee-i_okimm NE 'ERROR'.
      ra_srtfd-sign = 'I'.
      ra_srtfd-option = 'CP'.
      CONCATENATE '*' p_chsn_ee-pernr_new '*' INTO ra_srtfd-low.
      CLEAR ra_srtfd-high.
      APPEND ra_srtfd.
      CLEAR ra_srtfd.
      DELETE FROM ptquoded WHERE pernr EQ p_chsn_ee-pernr_new.
    ELSE.
      DELETE p_ptresult WHERE pernr EQ p_chsn_ee-pernr_new.
      DELETE p_quoded WHERE pernr EQ p_chsn_ee-pernr_new.
    ENDIF.
  ENDLOOP.
  IF ra_srtfd[] IS NOT INITIAL.
    DELETE FROM pcl2 WHERE relid EQ 'B2'
                       AND srtfd IN ra_srtfd.
  ENDIF.

*  Insercion de  datos en cluster
  LOOP AT p_ptresult.

    CLEAR: b2-version, b2-key, kntag, bezug.
    REFRESH: zl, pt, at, wst, cwst, saldo, zes, zko,
             anwkonti, abwkonti, fehler, psp, wpbp,
             ab, vert, sko, mehr, anwes, rufb, c1,
             alp,urlan, vs, cvs, qtacc, qttrans, qtbase.

    MOVE-CORRESPONDING p_ptresult TO b2-key.
    b2-key-cltyp = '1'.
    MOVE p_ptresult-version TO b2-version.
    MOVE p_ptresult-kntag TO kntag.
    MOVE p_ptresult-bezug TO bezug.
    zl[] = p_ptresult-zl[].
    pt[] = p_ptresult-pt[].
    at[] = p_ptresult-at[].
    wst[] = p_ptresult-wst[].
    cwst[] = p_ptresult-cwst[].
    saldo[] = p_ptresult-saldo[].
    zes[] = p_ptresult-zes[].
    zko[] = p_ptresult-zko[].
    anwkonti[] = p_ptresult-anwkonti[].
    abwkonti[] = p_ptresult-abwkonti[].
    fehler[] = p_ptresult-fehler[].
    psp[] = p_ptresult-psp[].
    wpbp[] = p_ptresult-wpbp[].
    ab[] = p_ptresult-ab[].
    vert[] = p_ptresult-vert[].
    sko[] = p_ptresult-sko[].
    mehr[] = p_ptresult-mehr[].
    anwes[] = p_ptresult-anwes[].
    rufb[] = p_ptresult-rufb[].
    c1[] = p_ptresult-c1[].
    alp[] = p_ptresult-alp[].
    urlan[] = p_ptresult-urlan[].
    vs[] = p_ptresult-vs[].
    cvs[] = p_ptresult-cvs[].
    qtacc[] = p_ptresult-qtacc[].
    qttrans[] = p_ptresult-qttrans[].
    qtbase[] = p_ptresult-qtbase[].

    IF ( wst[] IS INITIAL ) AND ( cwst[] IS INITIAL ).
      zrp-exp-c2-b2.
    ELSE.
      rp-exp-c2-b2.
    ENDIF.

    IF rp-imp-b2-subrc IS INITIAL.
      PERFORM update_data(rpppxv00).
    ENDIF.

  ENDLOOP.

  LOOP AT p_quoded.
    CLEAR ptquoded.
    MOVE-CORRESPONDING p_quoded TO ptquoded.
    INSERT ptquoded FROM ptquoded.
  ENDLOOP.
  MESSAGE s999(st) WITH text-m15.
ENDFORM.                    "update_ptresult

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

*  TABLES: T500L.
  SELECT SINGLE * FROM t500l WHERE molga = iv_molga.
  ov_relid = t500l-relid.

ENDFORM.                    "read_relid

*&---------------------------------------------------------------------*
*&      Form  delete_cu
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_PERNR    text
*----------------------------------------------------------------------*
FORM delete_cu USING p_pernr.

  DELETE FROM pcl2 WHERE relid = 'CU'
                     AND srtfd = p_pernr.

ENDFORM.                    "DELETE_CU

*&---------------------------------------------------------------------*
*&      Form  delete_co
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_RGDIR    text
*----------------------------------------------------------------------*
FORM delete_co USING p_pernr p_relid.

  DATA: c_pernr(8), c_seqnr(5).
  DATA: sortfield(40).
  DATA: it_rgdir TYPE STANDARD TABLE OF pc261 WITH HEADER LINE.

  RANGES: sortfd FOR sortfield.

  MOVE p_pernr TO c_pernr.
  MOVE '00000' TO c_seqnr.
  CONCATENATE c_pernr c_seqnr INTO sortfd-low.
  MOVE '99999' TO c_seqnr.
  CONCATENATE c_pernr c_seqnr INTO sortfd-high.
  sortfd-sign = 'I'.
  sortfd-option = 'BT'.
  APPEND sortfd.

  DELETE FROM pcl2
  WHERE relid = p_relid
  AND  srtfd IN sortfd.

ENDFORM.                    "delete_co

*&---------------------------------------------------------------------*
*&      Form  insert_co
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_PAYRESULT  text
*      -->P_RGDIR      text
*      -->P_PERNR      text
*      -->P_CLUSTERID  text
*----------------------------------------------------------------------*
FORM insert_co TABLES p_payresult TYPE zhrpaymx_tab_of_results
                      p_rgdir STRUCTURE pc261
                USING p_pernr p_clusterid p_molga.

  DATA: l_subrc LIKE sy-subrc.

  READ TABLE import_subpools WITH KEY clst_id = p_clusterid
                              INTO subroutine_pool.
  IF sy-subrc <> 0.
    PERFORM create_subroutine_pool USING p_clusterid p_molga
                                CHANGING subroutine_pool-name l_subrc.
    CASE l_subrc.
      WHEN 0.
        subroutine_pool-clst_id = p_clusterid.
        APPEND subroutine_pool TO import_subpools.
      WHEN 1.
        EXIT.
      WHEN 2.
        EXIT.
      WHEN 3.
        EXIT.
      WHEN 4.
        EXIT.
      WHEN OTHERS.
        EXIT.
    ENDCASE.
  ENDIF.

  PERFORM exp_mx IN PROGRAM (subroutine_pool-name)
          TABLES p_payresult p_rgdir
          USING p_pernr p_clusterid IF FOUND.

ENDFORM.                    "insert_co

*&---------------------------------------------------------------------*
*&      Form  insert_cu
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_RGDIR    text
*----------------------------------------------------------------------*
FORM insert_cu TABLES p_paycu_result TYPE zhrpaycu_tab_of_results
                      p_rgdir STRUCTURE pc261
                USING p_pernr.

  DATA: wa_dir TYPE LINE OF zpaycu_result-dir.

  DELETE FROM hrpy_wpbp WHERE pernr = p_pernr.
  DELETE FROM hrpy_rgdir WHERE pernr = p_pernr.

  LOOP AT p_paycu_result WHERE perid = p_pernr.

    MOVE p_pernr TO cd-key.
    MOVE p_paycu_result-last_pay TO cd-last_pay.
    MOVE p_paycu_result-next_seq TO cd-next_seq.
    MOVE p_paycu_result-version-number TO cd-version-number.
    MOVE p_paycu_result-version-number TO pcl2-versn.
    MOVE p_paycu_result-version-uname TO cd-version-uname.
    MOVE p_paycu_result-version-datum TO cd-version-datum.
    MOVE p_paycu_result-version-uzeit TO cd-version-uzeit.
    MOVE p_paycu_result-version-pgmid TO cd-version-pgmid.
    MOVE p_paycu_result-version-molga TO cd-version-molga.
    LOOP AT p_paycu_result-dir INTO wa_dir.
      MOVE-CORRESPONDING wa_dir TO dir2.
      APPEND dir2.
    ENDLOOP.
    MOVE p_rgdir[] TO rgdir[].
    rp-exp-c2-cu.
    IF sy-subrc EQ 0.
      PERFORM update_data(rpppxv00).   "Almacena valores de cluster actual
    ENDIF.
    rp-init-buffer.

    INSERT hrpy_wpbp FROM TABLE p_paycu_result-hrpy_wpbp.
    INSERT hrpy_rgdir FROM TABLE p_paycu_result-hrpy_rgdir.

  ENDLOOP.

ENDFORM.                    "insert_cu

*&---------------------------------------------------------------------*
*&      Form  create_subroutine_pool
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_CLUSTERID  text
*      -->P_SUBRC      text
*----------------------------------------------------------------------*
FORM create_subroutine_pool USING p_clusterid p_molga
                         CHANGING name_subr_pool p_subrc.

  DATA: code_text(80).
  DATA: generated_code LIKE code_text OCCURS 0.
  DATA: wa_ro LIKE hrpystruc.
  DATA: requested_objects LIKE hrpystruc OCCURS 0.
  DATA: mess TYPE string,
        lin  TYPE i,
        wrd  TYPE string.

  CLEAR: generated_code[], code_text, name_subr_pool.
  CALL FUNCTION 'PYXX_RESOLVE_RESULT_STRUCTURE'
    EXPORTING
      clusterid               = p_clusterid
    TABLES
      requested_objects_list  = requested_objects
    EXCEPTIONS
      cluster_not_in_t52relid = 1
      ddictype_does_not_exist = 2.

  IF sy-subrc <> 0.
    MOVE sy-subrc TO p_subrc.
    EXIT.
  ENDIF.

  READ REPORT include_for_export_routine INTO generated_code.

  LOOP AT generated_code INTO code_text.
    REPLACE '*' WITH space INTO code_text(1).
    CASE code_text+1.
      WHEN '$'.

        CLEAR code_text.
        MODIFY generated_code FROM code_text.
        LOOP AT requested_objects INTO wa_ro.
          CASE wa_ro-type.
            WHEN 'u'.

              MESSAGE s003(zcopy_ee) INTO code_text
               WITH wa_ro-component wa_ro-name.

              INSERT code_text INTO generated_code.

            WHEN 'h'.

              MESSAGE s004(zcopy_ee) INTO code_text
               WITH wa_ro-component wa_ro-name.

              INSERT code_text INTO generated_code.

            WHEN OTHERS.
          ENDCASE.
        ENDLOOP.

      WHEN OTHERS.
        MODIFY generated_code FROM code_text.
    ENDCASE.
  ENDLOOP.

  SYNTAX-CHECK FOR generated_code MESSAGE mess LINE lin WORD wrd.

  IF mess IS NOT INITIAL.
    p_subrc = 4.
  ENDIF.

  CATCH SYSTEM-EXCEPTIONS generate_subpool_dir_full = 3.
    GENERATE SUBROUTINE POOL generated_code NAME name_subr_pool.
  ENDCATCH.
  IF sy-subrc NE 0.
    p_subrc = 3.
  ENDIF.

ENDFORM.                    "CREATE_SUBROUTINE_POOL

*&---------------------------------------------------------------------*
*&      Form  DOWNLOAD_LOG
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_P_FILE  text
*----------------------------------------------------------------------*
FORM download_log  USING    p_file.
  DATA: iv_line TYPE char300.
  CONCATENATE p_file 'LOG_MIGRACION_' sy-datum '_' sy-uzeit '.txt' INTO p_file.
  OPEN DATASET p_file FOR OUTPUT IN TEXT MODE ENCODING DEFAULT.

  IF sy-subrc IS NOT INITIAL.
    MESSAGE e999(st) WITH text-m04.
  ENDIF.
  MESSAGE s999(st) WITH text-m18.
  SORT gt_log BY pernr_new message ASCENDING.
  DELETE ADJACENT DUPLICATES FROM gt_log COMPARING ALL FIELDS.
  SORT gt_log BY pernr_new id ASCENDING.

  LOOP AT gt_log INTO wa_log.
    CLEAR iv_line.
    CONCATENATE wa_log-id wa_log-pernr_old wa_log-pernr_new wa_log-message
      INTO iv_line SEPARATED BY cl_abap_char_utilities=>horizontal_tab.
    TRANSFER iv_line TO p_file.
    IF sy-subrc NE 0.
      EXIT.
    ELSE.
      CONTINUE.
    ENDIF.
  ENDLOOP.

  CLOSE DATASET p_file.
  IF sy-subrc NE 0.
    MESSAGE e999(st) WITH text-m04.
  ELSE.
    MESSAGE s999(st) WITH text-m03.
  ENDIF.

  MESSAGE s999(st) WITH text-m19.
ENDFORM.


*&---------------------------------------------------------------------*
*&      Form  HELP_SERVER_DIR
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_P_FILE  text
*----------------------------------------------------------------------*
FORM help_server_dir  CHANGING    p_file.
* constantes locales
  CONSTANTS: lc_p TYPE c VALUE 'p',
             lc_a TYPE dxfields-location VALUE 'a'.

* Variables locales
  DATA: li_host         TYPE STANDARD TABLE OF msxxlist,
        lw_host         TYPE msxxlist,
        lv_path         TYPE dxfields-longpath,
        lv_path2        TYPE string,
        lv_ubicacion(1) TYPE c,
        lv_abend        TYPE c.
*-Nombre del servidor
  CALL FUNCTION 'RFC_GET_LOCAL_SERVERS'
    TABLES
      hosts         = li_host
    EXCEPTIONS
      not_available = 1
      OTHERS        = 2.

  IF sy-subrc IS INITIAL.

*-Nombre del servidor a la estructura
    CLEAR lw_host.
    READ TABLE li_host
               INTO lw_host
               INDEX 1.

*-Obtengo el path
    CALL FUNCTION 'F4_DXFILENAME_TOPRECURSION'
      EXPORTING
        i_location_flag = lc_a
        i_server        = lw_host-name
      IMPORTING
        o_location_flag = lv_ubicacion
        o_path          = lv_path
        abend_flag      = lv_abend
      EXCEPTIONS
        rfc_error       = 1
        error_with_gui  = 2
        OTHERS          = 3.

*-Si se obtiene un path
    IF sy-subrc    IS INITIAL AND
       NOT lv_path IS INITIAL AND
       lv_abend    IS INITIAL.
*      lv_path2 = lv_path.
      CALL FUNCTION 'CACS_SPLIT_PATH'
        EXPORTING
          i_path = lv_path
        IMPORTING
          e_path = lv_path2
*         E_FILENAME       =
*         E_PSERVER        =
        .

*      CONCATENATE lv_path2 'LOG_MIGRACION' sy-datum '.txt' INTO lv_path.
*-Devuelvo ruta al parametro de selecci n
      p_file = lv_path2.

    ENDIF.

  ENDIF.
ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  UPDATE_1001_CP
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_PERNR  text
*      -->P_1001  text
*----------------------------------------------------------------------*
FORM update_1001_cp  TABLES   p_chsn_ee STRUCTURE zcopy_oview_man
                              p_1000 STRUCTURE hrp1000
                              p_1001 STRUCTURE hrp1001.

  DATA: wa_pernr_list LIKE          zcopy_oview_man.
  DATA: it_p1000 TYPE TABLE OF p1000 WITH HEADER LINE.
  DATA: it_p1001 TYPE TABLE OF p1001 WITH HEADER LINE.
  DATA: it_plog TYPE TABLE OF plogi WITH HEADER LINE.
  DATA: it_1001 TYPE TABLE OF hrp1001 WITH HEADER LINE.
  MESSAGE s999(st) WITH text-m16.
  LOOP AT p_1001 INTO it_1001 WHERE otype EQ 'CP'.
    APPEND it_1001.
    DELETE p_1001.
  ENDLOOP.

  CLEAR: it_1001.

  LOOP AT p_chsn_ee INTO wa_pernr_list  WHERE i_okimm NE 'ERROR'.
    REFRESH: it_p1000, it_p1001, it_plog.
    LOOP AT p_1001 WHERE objid EQ wa_pernr_list-pernr_new.
      CLEAR it_p1001.
      MOVE-CORRESPONDING p_1001 TO it_p1001.
      CLEAR: it_p1001-mandt.
      APPEND it_p1001.
      LOOP AT it_1001 WHERE objid EQ p_1001-sobid..
        CLEAR it_p1001.
        MOVE-CORRESPONDING it_1001 TO it_p1001.
        CLEAR: it_p1001-mandt.
        APPEND it_p1001.
        READ TABLE p_1000 WITH KEY objid = it_1001-objid.
        IF sy-subrc EQ 0.
          CLEAR: it_plog, it_p1000.
          MOVE-CORRESPONDING p_1000 TO it_plog.
          MOVE-CORRESPONDING p_1000 TO it_p1000.
          CLEAR: it_plog-mandt, it_p1000-mandt.
          APPEND: it_p1000, it_plog.
        ENDIF.
      ENDLOOP.
    ENDLOOP.

    CALL FUNCTION 'RH_INSERT_INFTY'
      EXPORTING
        vtask               = 'S'
      TABLES
        innnn               = it_p1000[]
      EXCEPTIONS
        no_authorization    = 1
        error_during_insert = 2
        repid_form_initial  = 3
        corr_exit           = 4
        begda_greater_endda = 5
        OTHERS              = 6.

    CALL FUNCTION 'RH_INSERT_INFTY'
      EXPORTING
        vtask               = 'S'
      TABLES
        innnn               = it_p1001[]
      EXCEPTIONS
        no_authorization    = 1
        error_during_insert = 2
        repid_form_initial  = 3
        corr_exit           = 4
        begda_greater_endda = 5
        OTHERS              = 6.

    IF sy-subrc NE 0.
      CLEAR wa_log.
      wa_log-id      = 'ERROR'.
      wa_log-pernr_new   = wa_pernr_list-pernr_new.
      wa_log-pernr_old   = wa_pernr_list-pernr.
      wa_log-message = 'No se importaron las relaciones CP->P'.
*     Para identificar los empelados que no se le copio data maestra
      wa_pernr_list-i_okimm = icon_led_red.
      MODIFY p_chsn_ee FROM wa_pernr_list.
    ELSE.
      TRY .
        INSERT plogi FROM TABLE it_plog.
      CATCH CX_SY_OPEN_SQL_DB.

      ENDTRY.
      CLEAR wa_log.
      wa_log-id      = 'EXITO'.
      wa_log-pernr_new   = wa_pernr_list-pernr_new.
      wa_log-pernr_old   = wa_pernr_list-pernr.
      wa_log-message = 'Se importaron las relaciones CP->P correctamente'.
    ENDIF.
    APPEND wa_log TO gt_log.

  ENDLOOP.
  MESSAGE s999(st) WITH text-m17.
ENDFORM.
