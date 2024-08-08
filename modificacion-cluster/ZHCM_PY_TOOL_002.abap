*&---------------------------------------------------------------------*
*& Report  ZHCM_PY_TOOL_002
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*
REPORT ZHCM_PY_TOOL_002 USING DATABASE pnp
                  NO STANDARD PAGE HEADING
                  LINE-SIZE 100 MESSAGE-ID pn.

* Include de declaraciones globales
INCLUDE ZHCM_PY_TOOL_002_data.
* Parámetros del reporte y validaciones
INCLUDE ZHCM_PY_TOOL_002_screen.

*****************************************************************
*                          S T A R T
*****************************************************************
START-OF-SELECTION.
* Tablas de cc-nóminas
  PERFORM fill_lgart.

  CLEAR:  rgdir.
  REFRESH: rgdir.
  UNASSIGN <gfs_single_tab>.

GET pernr.

  MOVE pernr-pernr TO apern.

  message = 'Cargando resultados de nómina...' .
  CONCATENATE message pernr-pernr INTO message
  SEPARATED BY space.
  CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
    EXPORTING
      text   = message
    EXCEPTIONS
      OTHERS = 1.

* Obtener resultados de cluster empleado actual.
  PERFORM re_cluster USING pernr-pernr.

  IF lines( rgdir ) GT 0.
    LOOP AT rgdir WHERE fpend BETWEEN pn-begda AND pn-endda.

      rx-key-pernr = pernr-pernr.
      rx-key-seqno = rgdir-seqnr.
* Importa cluster anterior.
      rp-imp-c2-cl.
      IF rp-imp-cl-subrc EQ 0.
        CASE u_comm.
* Se generan nuevamente los acumuladoes y bases de promedio
* según lo que esté parametrizado actualmente.
          WHEN 'UC_GENRT'.
* Borrado de registros anteriores de la tabla RT que
*se esta procesando
            DELETE rt WHERE lgart IN p_lgart.
* Genera acumuladores (/1**)
            PERFORM gen_cum.
* Genera bases de promedios (/2**)
            PERFORM gen_bas.
* En ALV se visualizan solamente los /1** y /2** generados
            PERFORM only_cumul_basis.
            SORT rt.
            PERFORM transfer_table USING hpr_str hpr_tab.

          WHEN 'UC_MODTB'.
            PERFORM transfer_table USING hpr_str hpr_tab.

          WHEN 'UC_FILE'.


          WHEN OTHERS.
        ENDCASE.
* Tabla interna con las empleados y secuencias procesadas.
*Se usa para procesarse desde USER_COMMAND
        MOVE: pernr-pernr TO gt_ee-pernr,
              rgdir-seqnr TO gt_ee-seqnr,
              rgdir-fpend TO gt_ee-fpend.
        COLLECT gt_ee.

      ENDIF.

    ENDLOOP.

  ENDIF.

END-OF-SELECTION.

* Funciones en ALV
  CASE u_comm.
    WHEN 'UC_GENRT' OR 'UC_MODTB'.
      PERFORM alv_fieldcatalogue_merge
              USING sy-repid 'ZHCM_PY_TOOL_002_DATA'    "USING sy-repid 'ZCORGDIR_DATA'
              hpr_tab space.
      PERFORM change_fieldcatalogue.
      CHECK: <gfs_single_tab> IS ASSIGNED.
      PERFORM call_reuse_alv_funct TABLES <gfs_single_tab>
                            USING sy-repid 'SET_PF_STATUS'
                                'USER_COMMAND' 'TOP_OF_PAGE'
                                gt_layout gt_fieldcatalogue.
    WHEN OTHERS.
* No se visualiza ALV cuando se está actualizando
*mendiante ALV.
  ENDCASE.
*&---------------------------------------------------------------------
*
*&      Form  re_cluster
*&---------------------------------------------------------------------
*
*       text
*----------------------------------------------------------------------
*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------
*
FORM re_cluster USING empleado LIKE pernr-pernr.

  DATA: w_endda LIKE sy-datum.

  DATA: clave LIKE pcl2-srtfd,
        cant  TYPE i.

  CONCATENATE empleado '%' INTO clave.

  CLEAR: cant.
  SELECT COUNT(*)
  INTO cant
  FROM pcl2
  WHERE relid = 'CL'
        AND srtfd LIKE clave.

  CLEAR: rgdir, rgdir[].

  CHECK: cant <> 0.

  CALL FUNCTION 'CU_READ_RGDIR_NEW'
    EXPORTING
      persnr               = empleado
      check_read_authority = space
    IMPORTING
      molga                = rg_calcmolga
    TABLES
      in_rgdir             = rgdir
    EXCEPTIONS
      OTHERS               = 1.

ENDFORM.                    " re_cluster
*&---------------------------------------------------------------------*
*&      Form  check_conversion_buffer
*&---------------------------------------------------------------------*
*       Invalidate conversion buffer in HRCR if necessary
*----------------------------------------------------------------------*
FORM check_conversion_buffer.

  TYPES: BEGIN OF t_s_seqnr,
           seqnr LIKE pc261-seqnr,
           ntabx LIKE buffer_dir-ntabx,
         END OF t_s_seqnr.
  TYPES: t_t_seqnr TYPE STANDARD TABLE OF t_s_seqnr.

  DATA: seqnrs TYPE t_t_seqnr WITH HEADER LINE.

  PERFORM re500l USING w_mol.
  LOOP AT buffer_dir WHERE sgart = 'P2'
                       AND relid = t500l-relid
                       AND ntabx <> 0
                       AND otabx <  0.
    seqnrs-seqnr = buffer_dir-srtfd+8(5).
    seqnrs-ntabx = buffer_dir-ntabx.
    APPEND seqnrs.
  ENDLOOP.

  CHECK NOT seqnrs[] IS INITIAL.

  CALL FUNCTION 'HR_INV_CONVERSION_BUFFER'
    EXPORTING
      molga  = w_mol
    TABLES
      seqnrs = seqnrs
    EXCEPTIONS
      OTHERS = 0.

ENDFORM.                    " check_conversion_buffer

*---------------------------------------------------------------------*
*       FORM RE500L                                                   *
*---------------------------------------------------------------------*
FORM re500l USING molga LIKE t500l-molga.
  CHECK t500l-molga NE molga.
  SELECT SINGLE * FROM t500l WHERE molga EQ molga.
  CHECK sy-subrc NE 0.
ENDFORM.                                 "RE500L
*&---------------------------------------------------------------------*
*&      Form  gen_cum
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM gen_cum.

* Generacion de conceptos /1
  LOOP AT rt.
    CLEAR ort.
    IF rt-betrg GT 0.
      MOVE-CORRESPONDING rt TO ort.
      LOOP AT it_t512w WHERE molga EQ w_mol AND
                             lgart EQ rt-lgart AND
                             begda LE rgdir-fpend AND
                             endda GE rgdir-fpend.

        rt-betpe = rt-anzhl = 0.
        rt-rte_curr = space.
        "XULP30K054887
        rt-zeinh = space.
        rt-lgart(2) = '/1'.
        pack = 0.

        DO 12 TIMES
          VARYING xfeld FROM it_t512w-kumul(1)
                  NEXT it_t512w-kumul+1(1)
                  RANGE it_t512w.
          "UCXMS
          IF xfeld NE 0.
            xfeld2 = x128.
            DO 8 TIMES.
              pack = pack + 1.
              IF xfeld O xfeld2.
                rt-abart = '*'.
                UNPACK pack TO rt-lgart+2(2).
                COLLECT rt.
              ENDIF.
              xfeld2 = xfeld2 / 2.
            ENDDO.
          ELSE.
            pack = pack + 8.
          ENDIF.
        ENDDO.
      ENDLOOP.
      MOVE-CORRESPONDING ort TO rt.
    ENDIF.
  ENDLOOP.

ENDFORM.                    "gen_cum
*&---------------------------------------------------------------------*
*&      Form  gen_bas
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM gen_bas.

* Generar /2*
  LOOP AT rt.
    CLEAR ort.
    MOVE-CORRESPONDING rt TO ort.
    LOOP AT i51av_2w WHERE molga EQ w_mol
                     AND lgart = rt-lgart
                     AND begda LE rgdir-fpend
                     AND endda GE rgdir-fpend.
      CLEAR rt.
      rt-abart = '*'.
      rt-lgart = i51av_2w-avbase.
      IF i51av_2w-use_rte EQ true.
        rt-betpe = ort-betpe * i51av_2w-percent_rte / 100.
        rt-rte_curr = ort-rte_curr.    "XUL Note 507557
      ENDIF.
      IF i51av_2w-use_num EQ true.
        rt-anzhl = ort-anzhl * i51av_2w-percent_num / 100.
      ENDIF.
      IF i51av_2w-use_amt EQ true.
        rt-betrg = ort-betrg * i51av_2w-percent_amt / 100.
        rt-amt_curr = ort-amt_curr.
      ENDIF.
      COLLECT rt.
    ENDLOOP.
    MOVE-CORRESPONDING ort TO rt.
  ENDLOOP.

ENDFORM.                    "gen_bas
*&---------------------------------------------------------------------*
*&      Form  fill_lgart
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM fill_lgart.

* Clses de tratamiento, acumulación y evaluación
  SELECT *
   APPENDING CORRESPONDING FIELDS OF TABLE it_t512w
     FROM t512w WHERE molga = w_mol.
* Normas de generación de promedios
  SELECT *
   APPENDING CORRESPONDING FIELDS OF TABLE i51av_2w
     FROM t51av_2w  WHERE molga = w_mol..

  IF p_lgart[] IS INITIAL.
* Acumuladores
    p_lgart-sign = 'I'.
    p_lgart-option = 'CP'.
    p_lgart-low = '/1*'.
    APPEND p_lgart.
*Bases de promedios
    p_lgart-sign = 'I'.
    p_lgart-option = 'CP'.
    p_lgart-low = '/2*'.
    APPEND p_lgart.
  ENDIF.

ENDFORM.                    "fill_lgart
*&---------------------------------------------------------------------*
*&      Form  modif_screen_output
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM modif_screen_output.

  CASE u_comm.
    WHEN 'UC_GENRT'.
      field_gr1 = 'FIL'.
      field_gr2 = 'TAB'.
    WHEN 'UC_MODTB'.
      field_gr1 = 'LGA'.
      field_gr2 = 'FIL'.
    WHEN 'UC_FILE'.
      field_gr1 = 'LGA'.
      field_gr2 = 'TAB'.
    WHEN OTHERS.
  ENDCASE.
  ps_proc1 = text-rit.
  ps_proc2 = text-tab.
  ps_proc3 = text-fil.
  PERFORM params_show_hide_fld USING space field_gr1.
  PERFORM params_show_hide_fld USING space field_gr2.

*  LOOP AT SCREEN.
*    IF screen-name = 'PS_PROC1' .
*       screen-active = 0.
*       screen-invisible = 0.
*       MODIFY SCREEN.
*    ENDIF.
*    IF screen-name = 'PS_PROC3' .
*       screen-active = 0.
*       screen-invisible = 0.
*       MODIFY SCREEN.
*    ENDIF.
*    IF screen-name = 'PS_PROC2' .
*       screen-active = 0.
*       screen-invisible = 0.
*       MODIFY SCREEN.
*    ENDIF.
*  ENDLOOP.

ENDFORM.                    "modif_screen_output
*&---------------------------------------------------------------------*
*&      Form  check_ucommand
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM check_ucommand.
  CASE sscrfields-ucomm.
      "Cuando actualizar acumuladores y promedios
    WHEN 'UC_GENRT'.
      u_comm = sscrfields-ucomm.
      "Cuando modificar tablas mediante ALV
    WHEN 'UC_MODTB'.
      u_comm = sscrfields-ucomm.
      "Cuando subir archivo plano a tablas
    WHEN 'UC_FILE'.
      u_comm = sscrfields-ucomm.
    WHEN OTHERS.
  ENDCASE.
ENDFORM.                    "check_ucommand
*&---------------------------------------------------------------------*
*&      Form  params_show_hide_fld
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->VALUE(P_TRIGGERING_PARAM)  text
*      -->VALUE(P_SCREEN_GROUP)      text
*----------------------------------------------------------------------*
FORM params_show_hide_fld USING VALUE(p_triggering_param)
                                VALUE(p_screen_group).
  DATA lv_active LIKE screen-active. "(1) type n.
  DATA lv_invisible LIKE screen-active. "(1) type n.

  IF p_triggering_param EQ 'X'.
    lv_active = 1.
    lv_invisible = 0.
  ELSE.
    lv_active = 0.
    lv_invisible = 1.
  ENDIF.

  LOOP AT SCREEN.
    IF screen-group1 EQ p_screen_group.
      screen-invisible = lv_invisible.
      screen-active = lv_active.
      MODIFY SCREEN.
    ENDIF.
  ENDLOOP.

ENDFORM.                    "params_show_hide_fld
*&---------------------------------------------------------------------*
*&      Form  get_pc_file
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_P_FILE   text
*----------------------------------------------------------------------*
FORM get_pc_file  CHANGING p_p_file.

  DATA lv_window_title TYPE string.
  DATA lv_file_filter TYPE string.
  DATA lt_filetable TYPE filetable.
  DATA ls_filetable TYPE LINE OF filetable.
  DATA lv_rc LIKE sy-subrc.

  lv_window_title = text-004.
  lv_file_filter = '*.txt'.
  CALL METHOD cl_gui_frontend_services=>file_open_dialog
    EXPORTING
      window_title            = lv_window_title
      file_filter             = lv_file_filter
      multiselection          = ' '
    CHANGING
      file_table              = lt_filetable
      rc                      = lv_rc
    EXCEPTIONS
      file_open_dialog_failed = 1
      cntl_error              = 2
      error_no_gui            = 3
      OTHERS                  = 4.

  IF sy-subrc <> 0.
*   MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*              WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ELSE.
    READ TABLE lt_filetable INTO ls_filetable INDEX 1.
    p_p_file = ls_filetable-filename.
  ENDIF.

ENDFORM.                    "get_pc_file
*&---------------------------------------------------------------------*
*&      Form  call_reuse_alv_funct
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_DATATAB                text
*      -->VALUE(P_PROGNAME)        text
*      -->VALUE(P_STATUS)          text
*      -->VALUE(P_USERCOMM)        text
*      -->VALUE(P_TOPOFPAGE)       text
*      -->VALUE(P_LAYOUT)          text
*      -->VALUE(P_FIELDCATALOGUE)  text
*----------------------------------------------------------------------*
FORM call_reuse_alv_funct TABLES p_datatab
                           USING VALUE(p_progname)
                                 VALUE(p_status)
                                 VALUE(p_usercomm)
                                 VALUE(p_topofpage)
                                 VALUE(p_layout)
                                 VALUE(p_fieldcatalogue).

  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
       EXPORTING
*            I_INTERFACE_CHECK           = ' '
            i_callback_program          = p_progname
            i_callback_pf_status_set    = p_status
            i_callback_user_command     = p_usercomm
            i_callback_top_of_page      = p_topofpage
*            i_callback_html_top_of_page = 'HTML_TOP_OF_PAGE'
*            i_callback_html_end_of_list = 'HTML_END_OF_LIST'
*            i_structure_name            = 'ZHCM_EERGDIR'
            is_layout                   = p_layout
            it_fieldcat                 = p_fieldcatalogue
*            it_excluding                = exclude
*            IT_SPECIAL_GROUPS           =
*            it_sort                     = sort_table
*            IT_FILTER                   =
*            IS_SEL_HIDE                 =
*            I_DEFAULT                   = 'X'
*            i_save                      = 'U'
*            is_variant               = variant
*            it_events                = events
*            IT_EVENT_EXIT            = t_event_exit
*            IS_PRINT                 =
*            IS_REPREP_ID             =
*            I_SCREEN_START_COLUMN    = 0
*            I_SCREEN_START_LINE      = 0
*            I_SCREEN_END_COLUMN      = 0
*            I_SCREEN_END_LINE        = 0
*      IMPORTING
*            e_exit_caused_by_caller  = exit_caused_by_caller
*            es_exit_caused_by_user   = exit_caused_by_user
       TABLES
            t_outtab                 = p_datatab
       EXCEPTIONS
            program_error            = 1
            OTHERS                   = 2.

ENDFORM.                    " call_reuse_alv_funct
*&---------------------------------------------------------------------*
*&      Form  alv_fieldcatalogue_merge
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->VALUE(P_PROGNAME)    text
*      -->VALUE(P_INCLUDE)     text
*      -->VALUE(P_TABNAME)     text
*      -->VALUE(P_STRUCTNAME)  text
*----------------------------------------------------------------------*
FORM alv_fieldcatalogue_merge  USING    VALUE(p_progname)
                                        VALUE(p_include)
                                        VALUE(p_tabname)
                                        VALUE(p_structname).

  CLEAR: gt_fieldcatalogue, gt_fieldcatalogue[].
  CALL FUNCTION 'REUSE_ALV_FIELDCATALOG_MERGE'
    EXPORTING
      i_program_name         = p_progname
      i_internal_tabname     = p_tabname
      i_structure_name       = p_structname
      i_client_never_display = space
      i_inclname             = p_include
    CHANGING
      ct_fieldcat            = gt_fieldcatalogue
    EXCEPTIONS
      inconsistent_interface = 1
      program_error          = 2
      OTHERS                 = 3.

ENDFORM.                    " alv_fieldcatalogue_merge
*&---------------------------------------------------------------------*
*&      Form  ONLY_CUMUL_BASIS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM only_cumul_basis.

* Únicamente se visualizan las cc-nóminas /1** y
*/2** en la ALV
* En la rutina USER_COMMAND se unen los nuevos
*acumuladores con los
* demás * conceptos en la RT, para poder
*actualizar el cluster.
    loop at rt where lgart not in p_lgart.
  MOVE-CORRESPONDING rt TO gt_temp_rt.
  MOVE: pernr-pernr TO gt_temp_rt-pernr,
        rgdir-seqnr TO gt_temp_rt-seqnr,
        rgdir-fpend TO gt_temp_rt-fpend.

  DELETE rt.
  APPEND gt_temp_rt.
ENDLOOP.

ENDFORM.                    "ONLY_CUMUL_BASIS

*&---------------------------------------------------------------------*
*&      Form  init_parameters
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM init_parameters.
  PERFORM init_layout USING gt_layout.
ENDFORM.                    "init_parameters
*&---------------------------------------------------------------------*
*&      Form  init_layout
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LAYOUT   text
*----------------------------------------------------------------------*
FORM init_layout  USING p_layout TYPE slis_layout_alv..

  CLEAR p_layout.
  p_layout-colwidth_optimize = 'X'.
  p_layout-key_hotspot       = ' '.
  p_layout-detail_popup      = 'X'.
  p_layout-edit              = 'X'.
*  p_layout-edit_mode         = 'X'.
*  p_layout-zebra             = 'X'.
*  p_layout-detail_titlebar   = 'Título'(004).
*  p_layout-header_text       = 'Empleados'(005).
  p_layout-box_fieldname     = 'XFELD'.

ENDFORM.                    "init_layout
*&---------------------------------------------------------------------*
*&      Form  user_command
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->UC_USER_COMMAND  text
*      -->UC_SELFIELD      text
*----------------------------------------------------------------------*
FORM user_command  USING uc_user_command LIKE sy-ucomm
                         uc_selfield     TYPE slis_selfield.
  DATA: popup_answer.

  CASE uc_user_command.
    WHEN 'SAVE'.          "Guardar entradas en base de datos
      PERFORM pop_up_with_message USING popup_answer.
      IF popup_answer = '1'.
        PERFORM save_entries TABLES <gfs_single_tab>
                                USING hpr_str
                                    uc_user_command.
        uc_selfield-refresh = 'X'.
        MESSAGE s001(zhcm) WITH 'Registros modificados'.
      ELSE.
        MESSAGE s001(zhcm) WITH 'Acción cancelada'.
      ENDIF.
    WHEN 'ADD'.                 "Insertar líneas a la tabla
      PERFORM add_lines TABLES <gfs_single_tab>.
      uc_selfield-refresh = 'X'.
    WHEN 'COPY'.                "Copiar líneas seleccionadas
      PERFORM copy_lines TABLES <gfs_single_tab> USING hpr_tab.
      uc_selfield-refresh = 'X'.
    WHEN 'DELETE'.              "Borrar líneas seleccionadas
      PERFORM delete_lines TABLES <gfs_single_tab>
                            USING hpr_tab.
      uc_selfield-refresh = 'X'.
    WHEN 'DELETE_ALL'.          "Borrar todas las líneas
      CLEAR: <gfs_single_tab>.
      uc_selfield-refresh = 'X'.
    WHEN OTHERS.
  ENDCASE.
ENDFORM.                    "USER_COMMAND
*&---------------------------------------------------------------------*
*&      Form  set_pf_status
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->SPS_EXCLUDING  text
*----------------------------------------------------------------------*
FORM set_pf_status USING sps_excluding TYPE slis_t_extab.
  IF w_test IS INITIAL.
    SET PF-STATUS '0100' EXCLUDING sps_excluding.
  ELSE.
    SET PF-STATUS '0200' EXCLUDING sps_excluding.
  ENDIF.
  SET TITLEBAR  '001'.
ENDFORM.                               "SET_PF_STATUS
*&---------------------------------------------------------------------*
*&      Form  add_lines
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->ALV_TABLE  text
*----------------------------------------------------------------------*
FORM add_lines TABLES alv_table.
  CLEAR alv_table.
  INSERT alv_table INDEX 1.
ENDFORM.                    "add_lines
*&---------------------------------------------------------------------*
*&      Form  global_check_x_display_tab
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM global_check_x_display_tab.

  IF p_rt = 'X'.
    hpr_tab = 'GT_RT'.
    hpr_str = 'RT'.
    hpr_ion = 'I'.
  ELSEIF p_wpbp = 'X'.
    hpr_tab = 'GT_WPBP'.
    hpr_str = 'WPBP'.
    hpr_ion = 'I'.
  ELSEIF p_bt = 'X'.
    hpr_tab = 'GT_BT'.
    hpr_str = 'BT'.
    hpr_ion = 'N'.
  ELSEIF p_v0 = 'X'.
    hpr_tab = 'GT_V0'.
    hpr_str = 'V0'.
    hpr_ion = 'N'.
  ELSEIF p_ab = 'X'.
    hpr_tab = 'GT_AB'.
    hpr_str = 'AB'.
    hpr_ion = 'N'.
  ELSEIF p_crt = 'X'.
    hpr_tab = 'GT_CRT'.
    hpr_str = 'CRT'.
    hpr_ion = 'N'.
  ELSEIF p_cltax = 'X'.
    hpr_tab = 'GT_CLTAX'.
    hpr_str = 'CLTAX'.
    hpr_ion = 'N'.
  ELSEIF p_clhwk = 'X'.
    hpr_tab = 'GT_CLHWK'.
    hpr_str = 'CLHWK'.
    hpr_ion = 'N'.
  ENDIF.

  CASE sscrfields-ucomm.
    WHEN 'UC_GENRT'. "Cuando actualizar acumuladores y prome
            hpr_tab = 'GT_RT'.
      hpr_str = 'RT'.
    WHEN OTHERS.
  ENDCASE.

ENDFORM.                    "global_check_x_display_tab
*&---------------------------------------------------------------------*
*&      Form  transfer_table
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->STR_NAME   text
*      -->INTAB_NAME text
*----------------------------------------------------------------------*
FORM transfer_table USING str_name intab_name.

  FIELD-SYMBOLS: <fs1_str> TYPE table,  "Tabla de cluster
                 <fs2_head1>,   "Cabecera de tabla de cluster
                 <fs2_head2>,   "Cabecera de tabla interna
                 <fs3_intab> TYPE table. "Tabla interna

  DATA: lc_str TYPE string,
        lc_tab TYPE string.

  CONCATENATE: str_name '[]' INTO lc_str,
               intab_name '[]' INTO lc_tab.

  ASSIGN: (lc_str) TO <fs1_str>,          "Tabla de cluster
          (lc_tab) TO <fs3_intab>,
          (intab_name) TO <fs2_head2>.

  CHECK <fs1_str> IS ASSIGNED AND
        <fs3_intab> IS ASSIGNED AND
        <fs2_head2> IS ASSIGNED.

  LOOP AT <fs1_str> ASSIGNING <fs2_head1>.
    MOVE-CORRESPONDING <fs2_head1> TO <fs2_head2>.
    MOVE: pernr-pernr TO <fs2_head2>+1(8),
          rgdir-seqnr TO <fs2_head2>+9(5),
          rgdir-fpend TO <fs2_head2>+14(8).
    APPEND <fs2_head2> TO <fs3_intab>.
  ENDLOOP.

* Tabla para visualizar en ALV
  IF <fs3_intab> IS ASSIGNED.
    ASSIGN <fs3_intab> TO <gfs_single_tab>.
  ENDIF.

ENDFORM.                    "transfer_table
*&---------------------------------------------------------------------*
*&      Form  copy_lines
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->ALV_TABLE  text
*----------------------------------------------------------------------*
FORM copy_lines TABLES alv_table USING intab_name.

  DATA: lc_insert TYPE i.

  LOOP AT alv_table.
    CLEAR lc_insert.
    CHECK alv_table(1) = 'X'.
    lc_insert = sy-tabix + 1.     "Siguiente línea
    CLEAR alv_table(1).           "Limpia check de selección
    INSERT alv_table INDEX lc_insert.
  ENDLOOP.

ENDFORM.                    "copy_lines
*&---------------------------------------------------------------------*
*&      Form  delete_lines
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->ALV_TABLE  text
*      -->INTAB_NAME text
*----------------------------------------------------------------------*
FORM delete_lines TABLES alv_table USING intab_name.

  LOOP AT alv_table.
    CHECK alv_table(1) = 'X'.
    DELETE alv_table.
  ENDLOOP.

ENDFORM.                    "delete_lines
*&---------------------------------------------------------------------*
*&      Form  save_entries
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->ALV_TABLE  text
*----------------------------------------------------------------------*
FORM save_entries TABLES alv_table USING cu_table u_command.

  FIELD-SYMBOLS: <fs_cutable> TYPE table,
                 <fs_cutab_head>.
  DATA: lc_cutable TYPE string.

  LOOP AT gt_ee.
    CLEAR:  rgdir.
    REFRESH: rgdir.

* Obtener resultados de cluster empleado actual.
    PERFORM re_cluster USING gt_ee-pernr.

    rx-key-pernr = gt_ee-pernr.
    rx-key-seqno = gt_ee-seqnr.
* Importa cluster anterior.
    rp-imp-c2-cl.
    IF rp-imp-cl-subrc EQ 0.

* Al usar field-symbol, automáticamente se afectan las
*tablas de cluster
      CONCATENATE cu_table '[]' INTO lc_cutable.
      ASSIGN (lc_cutable) TO <fs_cutable>.

      CHECK sy-subrc = 0.
      CLEAR <fs_cutable>.

      LOOP AT alv_table.

        CHECK alv_table+1(8) = gt_ee-pernr
          AND alv_table+9(5) = gt_ee-seqnr.

        ASSIGN (cu_table) TO <fs_cutab_head>.
        CHECK sy-subrc = 0.
        CLEAR <fs_cutab_head>.

        MOVE-CORRESPONDING alv_table TO <fs_cutab_head>.
        APPEND <fs_cutab_head> TO <fs_cutable>.

      ENDLOOP.

* Cuando se seleccionó el proceso de generar acumuladores
*y bases de promedios se deben retomar los valores
*temporalmente almacenados pertenecientes a la rt
            if u_comm = 'UC_GENRT'.

      LOOP AT gt_temp_rt WHERE pernr = gt_ee-pernr
                           AND seqnr = gt_ee-seqnr.
        ASSIGN (cu_table) TO <fs_cutab_head>.
        CLEAR <fs_cutab_head>.
        MOVE-CORRESPONDING gt_temp_rt TO <fs_cutab_head>.
        APPEND <fs_cutab_head> TO <fs_cutable>.
      ENDLOOP.

    ENDIF.

    rx-key-pernr = gt_ee-pernr.
    pack = gt_ee-seqnr.
    UNPACK pack TO rx-key-seqno.
* Genera encripacion de cluster resultados modificados
    MOVE-CORRESPONDING ocl-version TO cl-version.
    rp-exp-c2-cl.
    IF rp-imp-cl-subrc EQ 0.
* Almacena valores de cluster actual
      PERFORM update_data(rpppxv00).
    ENDIF.
    rp-init-buffer.

  ENDIF.

ENDLOOP.

ENDFORM.                    "save_entries
*&---------------------------------------------------------------------*
*&      Form  change_fieldcatalogue
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM change_fieldcatalogue .

  LOOP AT gt_fieldcatalogue INTO gv_fieldcat_line.
* Ocultar
    PERFORM set_no_out USING hpr_tab 'XFELD' 'X'.
    PERFORM set_fix_col USING hpr_tab 'PERNR' 'X'.
    PERFORM set_fix_col USING hpr_tab 'SEQNR' 'X'.
    PERFORM set_fix_col USING hpr_tab 'FPEND' 'X'.
*    PERFORM set_key_field USING hpr_tab 'PERNR' 'X'.
*    PERFORM set_key_field USING hpr_tab 'SEQNR' 'X'.
*    PERFORM set_key_field USING hpr_tab 'FPEND' 'X'.

    MODIFY gt_fieldcatalogue FROM gv_fieldcat_line
                              INDEX sy-tabix.
  ENDLOOP.

ENDFORM.                    " change_fieldcatalogue
*&---------------------------------------------------------------------*
*&      Form  set_no_out
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->VALUE(SN_TABNAME)    text
*      -->VALUE(SN_FIELDNAME)  text
*      -->VALUE(SN_VALUE)      text
*----------------------------------------------------------------------*
FORM set_no_out USING
     VALUE(sn_tabname)   LIKE gv_fieldcat_line-tabname
     VALUE(sn_fieldname) LIKE gv_fieldcat_line-fieldname
     VALUE(sn_value)     LIKE gv_fieldcat_line-no_out.

  IF gv_fieldcat_line-tabname   EQ sn_tabname AND
     gv_fieldcat_line-fieldname EQ sn_fieldname.
    gv_fieldcat_line-no_out = sn_value.
  ENDIF.

ENDFORM.                               " SET_NO_OUT
*&---------------------------------------------------------------------*
*&      Form  set_key_field
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->VALUE(SN_TABNAME)    text
*      -->VALUE(SN_FIELDNAME)  text
*      -->VALUE(SN_VALUE)      text
*----------------------------------------------------------------------*
FORM set_key_field USING
     VALUE(sn_tabname)   LIKE gv_fieldcat_line-tabname
     VALUE(sn_fieldname) LIKE gv_fieldcat_line-fieldname
     VALUE(sn_value)     LIKE gv_fieldcat_line-no_out.

  IF gv_fieldcat_line-tabname   EQ sn_tabname AND
     gv_fieldcat_line-fieldname EQ sn_fieldname.
    gv_fieldcat_line-key = sn_value.
  ENDIF.

ENDFORM.                    "set_key_field
*&---------------------------------------------------------------------*
*&      Form  SET_FIX_COL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->VALUE(SN_TABNAME)    text
*      -->VALUE(SN_FIELDNAME)  text
*      -->VALUE(SN_VALUE)      text
*----------------------------------------------------------------------*
FORM set_fix_col USING
     VALUE(sn_tabname)   LIKE gv_fieldcat_line-tabname
     VALUE(sn_fieldname) LIKE gv_fieldcat_line-fieldname
     VALUE(sn_value)     LIKE gv_fieldcat_line-key.

  IF gv_fieldcat_line-tabname   EQ sn_tabname AND
     gv_fieldcat_line-fieldname EQ sn_fieldname.
    gv_fieldcat_line-key = sn_value.
  ENDIF.

ENDFORM.                    "SET_FIX_COL
*&---------------------------------------------------------------------*
*&      Form  top_of_page
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM top_of_page.

  DATA: lt_list_header      TYPE slis_t_listheader,
        lt_list_header_line TYPE slis_listheader.

  DATA: lc_datum1(10),
        lc_datum2(10),
        lc_datum3(10).

* LIST HEADING LINE: TYPE H (Titel Groß)
  CLEAR lt_list_header_line.
  lt_list_header_line-typ  = 'H'.
  lt_list_header_line-info = text-top.
  REPLACE '&1' WITH hpr_str INTO lt_list_header_line-info.
  APPEND lt_list_header_line TO lt_list_header.


  CLEAR lt_list_header_line.
  lt_list_header_line-typ  = 'S'.
  IF pn-begda EQ pn-endda.
    lt_list_header_line-key = text-key."Fecha:
    WRITE pn-begda TO lc_datum3 DD/MM/YYYY.
    lt_list_header_line-info = lc_datum3.
    APPEND lt_list_header_line TO lt_list_header.
  ELSE.
    lt_list_header_line-key  = text-dat.
    WRITE pn-begda TO lc_datum1 DD/MM/YYYY.
    WRITE pn-endda TO lc_datum2 DD/MM/YYYY.
* replace '&1' with datum1 into lt_list_header_line-info.
* replace '&2' with datum2 into lt_list_header_line-info.
    CONCATENATE lc_datum1 '-' lc_datum2 INTO
              lt_list_header_line-info SEPARATED BY space.
    APPEND lt_list_header_line TO  lt_list_header.
  ENDIF.

  CLEAR lt_list_header_line.
  lt_list_header_line-typ  = 'S'.
  lt_list_header_line-key = text-lin.
  lt_list_header_line-info = lines( <gfs_single_tab> ).
  APPEND lt_list_header_line TO lt_list_header.

  CALL FUNCTION 'REUSE_ALV_COMMENTARY_WRITE'
       EXPORTING
            it_list_commentary = lt_list_header
*         I_LOGO             =
*         I_END_OF_LIST_GRID =
  .
ENDFORM.                               " TOP_OF_PAGE
*&---------------------------------------------------------------------*
*&      Form  pop_up_with_message
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM pop_up_with_message CHANGING answer.
  CALL FUNCTION 'POPUP_WITH_2_BUTTONS_TO_CHOOSE'
    EXPORTING
      defaultoption = '1'
      diagnosetext1 = '@AH@ ¿Realmente desea modificar?'
*      diagnosetext2 = 'Acetar: se modi los datos de cluster'
      text_option1  = '@0V@ Aceptar'
      text_option2  = '@0W@ Cancelar'
      textline1     = 'Al aceptar se modificará la base de datos'
*      textline2     = 'Cancelar: no se modr datos de cluster'
*      textline3     = ''
      titel         = 'Comfirmar modificación'
    IMPORTING
      answer        = answer.

ENDFORM.                    "pop_up_with_message
