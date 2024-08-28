FUNCTION ZHCM_GET_INFT_TABLES.
*"----------------------------------------------------------------------
*"*"Interfase local
*"  TABLES
*"      FM_INFT
*"----------------------------------------------------------------------

  CLEAR: fm_inft, fm_inft[].
*  PERFORM build_infty_range.
  SELECT infty ppnnn dbtab INTO CORRESPONDING FIELDS OF TABLE
    fm_inft FROM t777d WHERE infty IN ra_infty
                         AND papd = 'X'
                         AND dbtab <> space.

ENDFUNCTION.

