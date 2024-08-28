
*&---------------------------------------------------------------------*
*& Report  YDEMO1
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*
report ydemo1.
tables: tbtcjob, zthcm_eq_migrar.
*&---------------------------------------------------------------------*
selection-screen: begin of block b1 with frame title text-t01.
select-options: so_pernr for zthcm_eq_migrar-pernr_new.
parameters: p_rfc type rfcdest obligatory.
selection-screen: end of block b1.

selection-screen: begin of block b3 with frame title text-t02.
parameters: p_file like rlgrap-filename obligatory.
parameters: p_max type numc4 default '100'.
selection-screen: end of block b3.
*&---------------------------------------------------------------------*


ranges r_pernr  for zthcm_eq_migrar-pernr_new.
data   w_pernr  like zthcm_eq_migrar-pernr_new.
data lv_count like sy-tabix .
data ls_job type tbtcjob.
data lv_job_name like ls_job-jobname.

concatenate 'Migrator-' p_rfc '-' p_max into lv_job_name.

perform job_open using lv_job_name.
*

loop at so_pernr.

  if ( lv_count <= p_max  ).
    add 1 to lv_count.
    append so_pernr to r_pernr.

  endif.

  if ( lv_count eq p_max  ).
    submit zhcm_employee_migration
            user sy-uname via job ls_job-jobname number ls_job-jobcount
            with so_pernr-low in r_pernr
            with p_rfc        eq p_rfc
            with p_file       eq p_file
            and return.

    lv_count = 0.
    clear r_pernr[].
  endif.

endloop.
if ( lines( r_pernr ) > 0 ).
  submit zhcm_employee_migration
        user sy-uname via job ls_job-jobname number ls_job-jobcount
        with so_pernr-low in r_pernr
        with p_rfc        eq p_rfc
        with p_file       eq p_file
        and return.

  lv_count = 0.
  clear r_pernr[].
endif.

perform job_close.

message s499(sy) with 'Creado Job (Migracion) '.
*---------------------------------------------------------------------*
*       FORM JOB_OPEN                                                 *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
*  -->  P_JOBNAME                                                     *
*---------------------------------------------------------------------*
form job_open using p_jobname.
*  CLEAR TBTCJOB.

  ls_job-jobname = p_jobname.

  call function 'JOB_OPEN'
    exporting
      jobname          = ls_job-jobname
    importing
      jobcount         = ls_job-jobcount
    exceptions
      cant_create_job  = 1
      invalid_job_data = 2
      jobname_missing  = 3
      others           = 4.
  if sy-subrc <> 0.
    message id sy-msgid type sy-msgty number sy-msgno
            with sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  endif.

endform.                    " JOB_OPEN
*&---------------------------------------------------------------------*
*&      Form  JOB_CLOSE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form job_close.
*
  call function 'JOB_CLOSE'
    exporting
      jobcount             = ls_job-jobcount
      jobname              = ls_job-jobname
      strtimmed            = 'X'
    exceptions
      cant_start_immediate = 1
      invalid_startdate    = 2
      jobname_missing      = 3
      job_close_failed     = 4
      job_nosteps          = 5
      job_notex            = 6
      lock_failed          = 7
      invalid_target       = 8
      others               = 9.
  if sy-subrc <> 0.
    message id sy-msgid type sy-msgty number sy-msgno
            with sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  endif.


endform.                    " JOB_CLOSE 

"T01	Datos de selección
"T02	Ruta Fichero Log (Server Side)

"P_FILE	Fichero
"P_MAX	Max. Personas Por Proceso
"P_RFC	Destino RFC
"SO_PERNR	Número de PErsonal
