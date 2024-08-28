*&---------------------------------------------------------------------*
*&  Include           ZHCM_EMPLOYEE_MIGRATION_S01
*&---------------------------------------------------------------------*

SELECTION-SCREEN: BEGIN OF BLOCK b1 WITH FRAME TITLE text-t01.
  SELECT-OPTIONS: so_pernr FOR zthcm_eq_migrar-pernr_new.

  PARAMETERS: p_rfc TYPE rfcdest OBLIGATORY.

SELECTION-SCREEN: END OF BLOCK b1.


SELECTION-SCREEN: BEGIN OF BLOCK b3 WITH FRAME TITLE text-t02.
  PARAMETERS: p_file LIKE rlgrap-filename OBLIGATORY.
SELECTION-SCREEN: END OF BLOCK b3.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_file.
  PERFORM help_server_dir CHANGING p_file.
