FUNCTION ZHCM_GET_OLDEST_DATE.
*"----------------------------------------------------------------------
*"*"Interfase local
*"  IMPORTING
*"     REFERENCE(PERNR) TYPE  PERNR_D
*"  EXPORTING
*"     REFERENCE(OLDEST_DATE) TYPE  DATUM
*"  EXCEPTIONS
*"      PERNR_NOT_FOUND
*"----------------------------------------------------------------------

  SELECT begda INTO oldest_date FROM pa0000
    WHERE pernr = pernr ORDER BY begda ASCENDING.
    EXIT.
  ENDSELECT.

  IF sy-subrc <> 0.
    CLEAR oldest_date.
    RAISE pernr_not_found.
  ENDIF.

ENDFUNCTION.

