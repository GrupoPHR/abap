FUNCTION ZHCM_TIME_RESULTS_VERSION_GET.
*"----------------------------------------------------------------------
*"*"Interfase local
*"  IMPORTING
*"     VALUE(GET_PERNR) TYPE  PERNR_D
*"     VALUE(GET_PERMO) TYPE  T549Q-PERMO OPTIONAL
*"     VALUE(GET_PABRJ) TYPE  T549Q-PABRJ OPTIONAL
*"     VALUE(GET_PABRP) TYPE  T549Q-PABRP OPTIONAL
*"     VALUE(GET_KDATE) TYPE  DATUM OPTIONAL
*"     VALUE(GET_CLTYP) TYPE  PC2B0-CLTYP DEFAULT 1
*"  EXPORTING
*"     VALUE(GET_NO_RECORD_FOUND)
*"     VALUE(GET_VERSION)
*"  EXCEPTIONS
*"      NO_PERIOD_SPECIFIED
*"      WRONG_CLUSTER_VERSION
*"      NO_READ_AUTHORITY
*"      CLUSTER_ARCHIVED
*"      TECHNICAL_ERROR
*"----------------------------------------------------------------------

  DATA: l_day_diff TYPE i.

  CLEAR get_version.

*  PERFORM refresh_interface_tables_vers.
  IF NOT get_kdate IS INITIAL.
    CALL FUNCTION 'HR_PAYROLL_PERIODS_GET'
      EXPORTING
        get_begda       = get_kdate
      IMPORTING
        get_pabrj       = get_pabrj
        get_pabrp       = get_pabrp
      EXCEPTIONS
        no_period_found = 1
        no_valid_permo  = 2.
  ENDIF.
*----- berpr fung, ob Periode f r Import spezifiziert ist
  IF get_pabrj IS INITIAL OR get_pabrp IS INITIAL.
    RAISE no_period_specified.
  ENDIF.
*-----Import der Auswertungsergebnisse
  b2-key-pernr = get_pernr.
  b2-key-pabrj = get_pabrj.
  b2-key-pabrp = get_pabrp.
  b2-key-cltyp = get_cltyp.

  rp-imp-c2-b2-version.

  CASE rp-imp-b2-subrc.
    WHEN 0.               "fehlerfreier Import
      CLEAR get_no_record_found.
      get_version = ob2-version.
*      PERFORM fill_interface_tables_vers.
    WHEN 4.               "kein Satz vorhanden
      get_no_record_found = 'X'.
    WHEN 8.               "falsche Version Cluster B2
      MESSAGE e101(72) WITH get_pernr get_pabrj get_pabrp
                       RAISING wrong_cluster_version.
    WHEN 12.              "keine Leseberechtigung Cluster B2
      MESSAGE e102(72) RAISING no_read_authority.
    WHEN 16.              "Cluster archiviert
      MESSAGE e139(72) WITH get_pernr get_pabrj get_pabrp
                       RAISING cluster_archived.
    WHEN 20.              "technischer Fehler
      MESSAGE e140(72) RAISING technical_error.
  ENDCASE.
*-----Bestimmung Beginn- und Endedatum der importierten Periode
  SELECT * FROM t549q WHERE permo EQ get_permo
                      AND   pabrj EQ get_pabrj
                      AND   pabrp EQ get_pabrp.
    EXIT.
  ENDSELECT.

ENDFUNCTION.
