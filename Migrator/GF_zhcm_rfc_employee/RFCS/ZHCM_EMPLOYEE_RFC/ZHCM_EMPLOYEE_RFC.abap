FUNCTION ZHCM_EMPLOYEE_RFC.
*"----------------------------------------------------------------------
*"*"Interfase local
*"  EXPORTING
*"     VALUE(FM_PAYRESULT) TYPE  ZHRPAYMX_TAB_OF_RESULTS
*"     VALUE(FM_PAYCU_RESULT) TYPE  ZHRPAYCU_TAB_OF_RESULTS
*"     VALUE(FM_PCL4_LO) TYPE  ZHRPCL4_TAB_AUDIT_LOG_LO
*"     VALUE(FM_PCL4_SH) TYPE  ZHRPCL4_TAB_AUDIT_LOG_SH
*"     VALUE(FM_PTRESULT) TYPE  ZHRPTXX_TAB_OF_RESULTS
*"  TABLES
*"      FM_PERNR_LIST STRUCTURE  ZCOPY_OVIEW_MAN
*"      FM_PRELP STRUCTURE  PRELP
*"      FM_T52MCW STRUCTURE  T52MCW
*"      FM_ASSOB_HR STRUCTURE  ZCOPY_ASSOBHR
*"      FM_ENTIT STRUCTURE  ZTHCM_EQ_ENTIT
*"      FM_QOUDED STRUCTURE  ZPTQUODED
*"      FM_1000 STRUCTURE  HRP1000
*"      FM_1001 STRUCTURE  HRP1001
*"      FM_LOG STRUCTURE  ZHCM_RFC_LOG
*"  EXCEPTIONS
*"      NO_PERNR_FOUND
*"      NO_PERNR_SELECTED
*"----------------------------------------------------------------------
  IF FM_PERNR_LIST[] IS INITIAL.
    RAISE NO_PERNR_SELECTED.
  ENDIF.

  CLEAR: FM_PRELP, FM_PRELP[], FM_T52MCW, FM_T52MCW[].

  PERFORM read_data TABLES fm_pernr_list
                           fm_prelp
                           fm_t52mcw
                           fm_entit
                           fm_log.

  PERFORM read_assob_hr TABLES fm_pernr_list
                               fm_assob_hr.

  PERFORM read_cluster TABLES fm_pernr_list
                              fm_payresult
                              fm_paycu_result
                              fm_entit
                              fm_log.

  PERFORM read_b2 TABLES fm_pernr_list
                         fm_ptresult
                         fm_qouded.

  PERFORM read_1001_cp TABLES fm_pernr_list
                              fm_1000
                              fm_1001.

ENDFUNCTION.
