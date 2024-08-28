*&---------------------------------------------------------------------*
*& Report  ZHCM_EMPLOYEE_MIGRATION
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*
REPORT zhcm_employee_migration.

INCLUDE zhcm_employee_migration_top.
INCLUDE zhcm_employee_migration_s01.
INCLUDE zhcm_employee_migration_f01.

START-OF-SELECTION.
  PERFORM get_ee_migra TABLES gt_data.
  IF gt_data[] IS INITIAL.
    MESSAGE e999(st) WITH TEXT-m01.
  ENDIF.
  MESSAGE s999(st) WITH TEXT-m05.
  PERFORM import_via_rfc TABLES gt_data.

END-OF-SELECTION.
  IF gt_log[] IS INITIAL.
    MESSAGE e999(st) WITH TEXT-m02.
  ENDIF.
  PERFORM download_log USING p_file.
