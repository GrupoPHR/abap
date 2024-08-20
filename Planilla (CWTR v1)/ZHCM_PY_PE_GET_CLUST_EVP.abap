FUNCTION ZHCM_PY_PE_GET_CLUST_EVP.
*"----------------------------------------------------------------------
*"*"Interfase local
*"  IMPORTING
*"     REFERENCE(PERNR) TYPE  P_PERNR
*"     REFERENCE(SRTZA) TYPE  SRTZA DEFAULT 'A'
*"     REFERENCE(FPBEG) TYPE  FPBEG
*"     REFERENCE(FPEND) TYPE  FPEND
*"     REFERENCE(OCRSN) TYPE  PAY_OCRSN
*"     REFERENCE(INOCR) TYPE  PAY_OCRSN OPTIONAL
*"  EXPORTING
*"     VALUE(PAYRES) TYPE  PAY99_RESULT
*"     VALUE(RT) TYPE  HRPAY99_RT
*"     VALUE(RGDIR_D) TYPE  HRPY_TT_RGDIR
*"     VALUE(RGDIR_S) TYPE  HRPY_TT_RGDIR
*"     VALUE(SUBRC) TYPE  SYST_SUBRC
*"----------------------------------------------------------------------
*

  CLEAR RT[].
  CLEAR PAYRES.
  CLEAR RGDIR_D[].
  CLEAR RGDIR_S[].
  CLEAR SUBRC.
*
  DATA LT_REVAL TYPE PAY_T_EVAL_PERIOD.
  DATA WA_REVAL TYPE PAY_EVAL_PERIOD.
  DATA WA_RGDIR TYPE PC261.
* Leo el directorio
  PERFORM GET_RGDIR
   TABLES RGDIR_D USING PERNR SUBRC.
*
  CHECK SUBRC EQ 0.
*
  RGDIR_S[] = RGDIR_D[].
*
* Filtro para quedarme con la selección
  DELETE RGDIR_S WHERE SRTZA NE SRTZA. "'A'. "Solo las actuales
  DELETE RGDIR_S WHERE FPBEG LT FPBEG.
  DELETE RGDIR_S WHERE FPEND GT FPEND.
  DELETE RGDIR_S WHERE OCRSN NE OCRSN. "'ZELB'.
  IF ( INOCR IS NOT INITIAL ).
    DELETE RGDIR_S WHERE INOCR NE INOCR. "'ZELB'.
  ENDIF.
*
  READ TABLE RGDIR_S INDEX 1 INTO WA_RGDIR.
  SUBRC = SY-SUBRC.
  CHECK SUBRC EQ 0.
* Evaluo las dependencias
  PERFORM FILL_EVAL
   TABLES RGDIR_D LT_REVAL
    USING WA_RGDIR SUBRC.
*
  CHECK SUBRC EQ 0.
*
  READ TABLE LT_REVAL INDEX 1 INTO WA_REVAL.
  IF ( SY-SUBRC EQ  0 ).
    PERFORM PROCESS_EVP
     TABLES WA_REVAL-EVP RGDIR_D RT "LT_RT
      USING PERNR.
  ENDIF.
* Seteo la RT del Cluster
  PERFORM GET_CLUSTER USING PAYRES PERNR WA_RGDIR.
  CLEAR PAYRES-INTER-RT.
  APPEND LINES OF RT TO PAYRES-INTER-RT.
*
ENDFUNCTION.
*----------------------------------------------------------------------*
FORM GET_RGDIR TABLES PT_RGDIR TYPE HRPY_TT_RGDIR
               USING  P_PERSNR LIKE P0001-PERNR
                      O_SUBRC.
*
  DATA LT_RGDIR TYPE STANDARD TABLE OF PC261.
*
  CLEAR PT_RGDIR[].
*
  CALL FUNCTION 'CU_READ_RGDIR'
    EXPORTING
      PERSNR             = P_PERSNR
      NO_AUTHORITY_CHECK = 'X'
    TABLES
      IN_RGDIR           = LT_RGDIR
    EXCEPTIONS
      NO_RECORD_FOUND    = 1
      OTHERS             = 2.
*
  IF ( SY-SUBRC EQ 0 ).
    APPEND LINES OF LT_RGDIR TO PT_RGDIR.
  ELSE.
    O_SUBRC = SY-SUBRC.
  ENDIF.
*
ENDFORM.
*----------------------------------------------------------------------*
*----------------------------------------------------------------------*
FORM FILL_EVAL TABLES PT_RGDIR TYPE  HRPY_TT_RGDIR
                      PT_EVALR TYPE PAY_T_EVAL_PERIOD
                USING P_RGDIR_LINE TYPE PC261
                      O_SUBRC.
*
  DATA $MC_INCL TYPE BOOLEAN.
*
  CLEAR PT_EVALR[].
  CLEAR O_SUBRC.
*
  CALL FUNCTION 'HRO1_FILL_EVAL_DIR'
    EXPORTING
      PAYROLL_DIR          = PT_RGDIR[]
      RGDIR_LINE           = P_RGDIR_LINE
      ALL_RESULTS_OF_RUN   = $MC_INCL
    IMPORTING
      EVAL_TAB             = PT_EVALR[]
      PAYR_DIR             = PT_RGDIR[]
    EXCEPTIONS
      NO_EVALUATED_PERIODS = 1
      OTHERS               = 2.

  IF ( SY-SUBRC <> 0 ).
    O_SUBRC = SY-SUBRC.
  ENDIF.
*
ENDFORM.
*----------------------------------------------------------------------*
FORM PROCESS_EVP TABLES PT_EVP STRUCTURE PC261
                        PT_RGDIR_F TYPE  HRPY_TT_RGDIR
                        OT_RT  STRUCTURE PC207
                  USING $PERNR.
*
  DATA:
    BEGIN OF LS_RT_2 OCCURS 250.
      INCLUDE STRUCTURE PC207.
    DATA:
*    FPBEG    LIKE XRT-FPBEG,
*    FPEND    LIKE XRT-FPEND,
*    SORTF    LIKE XRT-SORTF,
*    PERNR    LIKE XRT-PERNR,                                "VKIK011129
      SEQNO    LIKE PC200-SEQNO,            " WLIK027494
      DELET(1) TYPE C,
    END OF LS_RT_2.
  DATA RT_2 LIKE STANDARD TABLE OF LS_RT_2.
  DATA WA_2 LIKE LS_RT_2.
  DATA RT_3 LIKE STANDARD TABLE OF LS_RT_2.
  DATA WA_3 LIKE LS_RT_2.
  DATA RT_2_WA   LIKE LINE OF RT_2.
  DATA WA_RT  TYPE PC207.
**
  DATA:
    PRINT_RETRO TYPE BOOLEAN,
    RETROCALC   TYPE BOOLEAN,
    OKAY        TYPE BOOLEAN.
  DATA LS_EVP     TYPE PC261.
  DATA LS_EVP_OLD TYPE PC261.
  DATA LT_RT     TYPE TABLE OF PC207 WITH HEADER LINE.
  DATA LT_RT_OLD TYPE TABLE OF PC207 WITH HEADER LINE.
*
  LOOP AT PT_EVP WHERE SRTZA EQ 'A'.                   "KVHN2363595
    LS_EVP = PT_EVP.

    PERFORM IMPORT_ACTUAL
     TABLES LT_RT
      USING LS_EVP
            $PERNR
            SY-SUBRC.
*
    LOOP AT LT_RT.
      MOVE-CORRESPONDING LT_RT TO RT_2_WA.
      RT_2_WA-SEQNO = LS_EVP-SEQNR.
      APPEND RT_2_WA TO RT_2.
    ENDLOOP.
*
    PERFORM SET_RECALC USING LS_EVP RETROCALC.
*-
    IF ( RETROCALC EQ 'X' ).
*
      PERFORM IMPORT_PREVIOUS
       TABLES LT_RT_OLD
              PT_RGDIR_F
        USING LS_EVP
              $PERNR
     CHANGING LS_EVP_OLD.
*
      LOOP AT LT_RT_OLD.
        MOVE-CORRESPONDING LT_RT_OLD TO RT_2_WA.
        RT_2_WA-ANZHL = - LT_RT_OLD-ANZHL.
        RT_2_WA-BETRG = - LT_RT_OLD-BETRG.
        IF RT_2_WA-BETRG EQ 0 AND RT_2_WA-ANZHL EQ 0.
          RT_2_WA-BETPE = 0.
        ELSE.
          RT_2_WA-BETPE = - LT_RT_OLD-BETPE.
        ENDIF.
        RT_2_WA-SEQNO = LS_EVP_OLD-SEQNR.
        APPEND RT_2_WA TO RT_2.
      ENDLOOP.
*
    ENDIF.
  ENDLOOP.
*
  LOOP AT RT_2 INTO WA_2
  WHERE ANZHL EQ 0
    AND BETPE EQ 0
    AND BETRG EQ 0.
    WA_2-DELET = 'X'.
    MODIFY RT_2 FROM WA_2 INDEX SY-TABIX.
  ENDLOOP.
*
  LOOP AT RT_2 INTO WA_2 WHERE DELET NE 'X'.
    MOVE-CORRESPONDING WA_2 TO WA_RT.
    COLLECT WA_RT INTO OT_RT.
  ENDLOOP.
*
ENDFORM.
*----------------------------------------------------------------------*
*---------------------------------------------------------------------*
FORM IMPORT_ACTUAL TABLES PT_RT STRUCTURE PC207
                   USING VALUE($EVP) LIKE PC261
                         VALUE($PERNR) LIKE P0001-PERNR
                         RC LIKE SY-SUBRC.
*
  DATA WA_RT TYPE PC207.
  CLEAR PT_RT[].
*
  PERFORM GET_CLUSTER_EVP
   TABLES PT_RT USING $PERNR $EVP-SEQNR.
*
ENDFORM.
*---------------------------------------------------------------------*
*---------------------------------------------------------------------*
FORM IMPORT_PREVIOUS TABLES PT_RT STRUCTURE PC207
                            PT_RGDIR_F TYPE  HRPY_TT_RGDIR
                     USING VALUE($EVP) STRUCTURE PC261
                           VALUE($PERNR) LIKE P0001-PERNR
                  CHANGING $EVP_O STRUCTURE PC261.
  DATA WA_RT TYPE PC207.
  DATA IGNORE TYPE BOOLEAN.
  DATA LT_RGDIR_O TYPE STANDARD TABLE OF PC261.
*
  CLEAR PT_RT[].
  CLEAR $EVP_O.
*
  CALL FUNCTION 'CD_READ_PREVIOUS'
    EXPORTING
      IN_RECORD = $EVP
      EXACT     = 'X' "TRUE
    TABLES
      RGDIR     = PT_RGDIR_F
      OUT_RGDIR = LT_RGDIR_O
    EXCEPTIONS
      OTHERS    = 1.

  IF ( SY-SUBRC EQ 0 ).
    READ TABLE LT_RGDIR_O INDEX 1 INTO $EVP_O.
    IF ( SY-SUBRC EQ 0 ).
      PERFORM GET_CLUSTER_EVP
       TABLES PT_RT
        USING $PERNR $EVP_O-SEQNR.
    ENDIF.
  ENDIF.
*
ENDFORM.
*---------------------------------------------------------------------*
FORM SET_RECALC USING VALUE($EVP) STRUCTURE PC261
                            $RECALC TYPE BOOLEAN.
*
  CALL FUNCTION 'CD_RETROCALC_PERIOD'
    EXPORTING
      ENTRY  = $EVP
    IMPORTING
      CALCD  = $RECALC
    EXCEPTIONS
      OTHERS = 1.
*
ENDFORM.
*----------------------------------------------------------------------*
FORM GET_CLUSTER_EVP TABLES P_RT STRUCTURE PC207
                      USING P_PERNR P_SEQNR.
*
* DATA: LT_RESULT TYPE PAYCLRESULT OCCURS 0 WITH HEADER LINE.
  DATA: LT_RESULT TYPE PAY99_RESULT OCCURS 0 WITH HEADER LINE.
*
  CLEAR P_RT.
*
  CALL FUNCTION 'PYXX_READ_PAYROLL_RESULT'
    EXPORTING
      CLUSTERID                    = 'RX'
      EMPLOYEENUMBER               = P_PERNR
      SEQUENCENUMBER               = P_SEQNR
    CHANGING
      PAYROLL_RESULT               = LT_RESULT
    EXCEPTIONS
      ILLEGAL_ISOCODE_OR_CLUSTERID = 1
      ERROR_GENERATING_IMPORT      = 2
      IMPORT_MISMATCH_ERROR        = 3
      SUBPOOL_DIR_FULL             = 4
      NO_READ_AUTHORITY            = 5
      NO_RECORD_FOUND              = 6
      VERSIONS_DO_NOT_MATCH        = 7
      ERROR_READING_ARCHIVE        = 8
      ERROR_READING_RELID          = 9
      OTHERS                       = 10.
  IF ( SY-SUBRC EQ 0 ).
    APPEND LINES OF LT_RESULT-INTER-RT TO P_RT.
  ENDIF.
*
ENDFORM.
*----------------------------------------------------------------------*
*----------------------------------------------------------------------*
FORM GET_CLUSTER USING P_RESULT TYPE PAY99_RESULT
                       P_PERNR
                       P_CLUST STRUCTURE  PC261.
*
  DATA: LT_RESULT TYPE PAY99_RESULT OCCURS 0 WITH HEADER LINE.
*
  CALL FUNCTION 'PYXX_READ_PAYROLL_RESULT'
    EXPORTING
      CLUSTERID                    = 'RX'
      EMPLOYEENUMBER               = P_PERNR
      SEQUENCENUMBER               = P_CLUST-SEQNR
    CHANGING
      PAYROLL_RESULT               = LT_RESULT
    EXCEPTIONS
      ILLEGAL_ISOCODE_OR_CLUSTERID = 1
      ERROR_GENERATING_IMPORT      = 2
      IMPORT_MISMATCH_ERROR        = 3
      SUBPOOL_DIR_FULL             = 4
      NO_READ_AUTHORITY            = 5
      NO_RECORD_FOUND              = 6
      VERSIONS_DO_NOT_MATCH        = 7
      ERROR_READING_ARCHIVE        = 8
      ERROR_READING_RELID          = 9
      OTHERS                       = 10.
  IF ( SY-SUBRC EQ 0 ).
    MOVE-CORRESPONDING LT_RESULT TO P_RESULT.
    MOVE-CORRESPONDING P_CLUST TO P_RESULT-EVP.
  ENDIF.
*
ENDFORM.
*----------------------------------------------------------------------*
