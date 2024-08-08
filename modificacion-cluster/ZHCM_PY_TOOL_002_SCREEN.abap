*&---------------------------------------------------------------------*
*&  Include           ZHCM_PY_TOOL_002_SCREEN
*&---------------------------------------------------------------------*
************************************************************
*                     Criterios de Selección
************************************************************
SELECTION-SCREEN BEGIN OF BLOCK frm1
                          WITH FRAME TITLE text-001.
SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT (20) text-p02.
PARAMETERS: w_mol LIKE t500l-molga OBLIGATORY DEFAULT '39'.

SELECTION-SCREEN COMMENT 30(17) text-p04.
PARAMETERS: w_test TYPE xfeld AS CHECKBOX DEFAULT 'X'.
SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN END OF BLOCK frm1.

SELECTION-SCREEN BEGIN OF BLOCK push
                        WITH FRAME TITLE text-002.
SELECTION-SCREEN PUSHBUTTON /1(24) ps_proc1
                        USER-COMMAND uc_genrt.
SELECTION-SCREEN PUSHBUTTON /1(24) ps_proc2
                        USER-COMMAND uc_modtb.
SELECTION-SCREEN PUSHBUTTON /1(24) ps_proc3
                        USER-COMMAND uc_file.
SELECTION-SCREEN END OF BLOCK push.

SELECTION-SCREEN BEGIN OF BLOCK proc
                          WITH FRAME TITLE text-003.

SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT (30) text-p03 MODIF ID lga.
SELECT-OPTIONS p_lgart FOR t512w-lgart MODIF ID lga.
SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN BEGIN OF LINE.
PARAMETERS p_rt RADIOBUTTON GROUP itl
                  DEFAULT 'X' MODIF ID tab.
"Tabla RT
SELECTION-SCREEN COMMENT (21) FOR FIELD p_rt
                                MODIF ID tab.

PARAMETERS p_wpbp RADIOBUTTON GROUP itl MODIF ID tab.
"Tabla WPBP
SELECTION-SCREEN COMMENT 26(21) FOR FIELD p_wpbp
                                    MODIF ID tab.

PARAMETERS p_bt RADIOBUTTON GROUP itl MODIF ID tab.
"Tabla BT
SELECTION-SCREEN COMMENT 52(21) FOR FIELD p_bt
                                    MODIF ID tab.

PARAMETERS p_v0 RADIOBUTTON GROUP itl MODIF ID tab.
 "Tabla V0
SELECTION-SCREEN COMMENT 75(21) FOR FIELD p_v0
                                      MODIF ID tab.
SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN BEGIN OF LINE.
PARAMETERS p_ab RADIOBUTTON GROUP itl MODIF ID tab.
 "Tabla AB
SELECTION-SCREEN COMMENT (21) FOR FIELD p_ab
                                      MODIF ID tab.

PARAMETERS p_crt RADIOBUTTON GROUP itl MODIF ID tab.
 "Tabla CRT
SELECTION-SCREEN COMMENT 26(21) FOR FIELD p_crt
                                      MODIF ID tab.

PARAMETERS p_cltax RADIOBUTTON GROUP itl MODIF ID tab.
 "Tabla CLTAX
SELECTION-SCREEN COMMENT 52(21) FOR FIELD p_cltax
                                          MODIF ID tab.

PARAMETERS p_clhwk RADIOBUTTON GROUP itl MODIF ID tab.
 "Tabla CLHWK
SELECTION-SCREEN COMMENT 75(21) FOR FIELD p_clhwk
                                          MODIF ID tab.
SELECTION-SCREEN END OF LINE.

PARAMETERS: path_rt LIKE rlgrap-filename
                    DEFAULT 'C:\temp\rt.txt'
MODIF ID fil.   "Ruta de archivo plano
PARAMETERS: pat_wpbp LIKE rlgrap-filename
                      DEFAULT 'C:\temp\wpbp.txt'
MODIF ID fil.   "Ruta de archivo plano
PARAMETERS: path_bt LIKE rlgrap-filename
                      DEFAULT 'C:\temp\bt.txt'
MODIF ID fil.   "Ruta de archivo plano
PARAMETERS: path_v0 LIKE rlgrap-filename
                      DEFAULT 'C:\temp\v0.txt'
MODIF ID fil.   "Ruta de archivo plano
PARAMETERS: path_ab LIKE rlgrap-filename
                      DEFAULT 'C:\temp\ab.txt'
MODIF ID fil.   "Ruta de archivo plano
PARAMETERS: path_crt LIKE rlgrap-filename
                      DEFAULT 'C:\temp\crt.txt'
MODIF ID fil.   "Ruta de archivo plano
PARAMETERS: pa_cltax LIKE rlgrap-filename
                      DEFAULT 'C:\temp\cltax.txt'
MODIF ID fil.   "Ruta de archivo plano
PARAMETERS: pa_clhwk LIKE rlgrap-filename
                      DEFAULT 'C:\temp\clhwk.txt'
MODIF ID fil.   "Ruta de archivo plano

SELECTION-SCREEN END OF BLOCK proc.
PARAMETERS u_comm TYPE string
                      DEFAULT 'UC_MODTB' NO-DISPLAY.
"Identifica el nombre de la tabla interna
PARAMETERS hpr_tab(30) DEFAULT 'GT_RT' NO-DISPLAY.
"Identifica el nombre de la tabla de cluster
PARAMETERS hpr_str(30) DEFAULT 'RT' NO-DISPLAY.
"Identifica el nombre de la tabla de cluster
PARAMETERS hpr_ion(1) DEFAULT 'I' NO-DISPLAY.
*********************************************************
*                  V A L I D A C I O N E S
*********************************************************

AT SELECTION-SCREEN ON VALUE-REQUEST FOR path_rt.
  PERFORM get_pc_file CHANGING path_rt.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR pat_wpbp.
  PERFORM get_pc_file CHANGING pat_wpbp.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR path_bt.
  PERFORM get_pc_file CHANGING path_bt.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR path_v0.
  PERFORM get_pc_file CHANGING path_v0.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR path_ab.
  PERFORM get_pc_file CHANGING path_ab.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR path_crt.
  PERFORM get_pc_file CHANGING path_crt.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR pa_cltax.
  PERFORM get_pc_file CHANGING pa_cltax.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR pa_clhwk.
  PERFORM get_pc_file CHANGING pa_clhwk.

LOAD-OF-PROGRAM.

INITIALIZATION.
  PERFORM init_parameters.        "Para ALV

AT SELECTION-SCREEN.
  PERFORM check_ucommand.
  PERFORM global_check_x_display_tab.

AT SELECTION-SCREEN OUTPUT.
  PERFORM modif_screen_output.
