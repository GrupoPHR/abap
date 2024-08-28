*&---------------------------------------------------------------------*
*&  Include           ZHOWN_MACROS
*&---------------------------------------------------------------------*
DEFINE own-rp-exp-c2-cu.

  call function 'STRUCPACK_RELEASE_GET'
    exporting
      i_name_extension = 'SAP_HR'
    importing
      e_release        = g_dummy_for_saprl_cu
    exceptions
      others           = 0.
  cd-version-saprl = g_dummy_for_saprl_cu(4).
   export cd-version
         cd-next_seq
         cd-last_pay
         rgdir
         dir2 from %%_dir2                               "XDOAHRK000000
*         dir2                                            "XDOAHRK000408
  to database pcl2(cu)
  id cd-key." using pcl2_exp_imp.
  if sy-subrc ne 0.
    clear: cd-next_seq,
           rgdir,
           dir2.                                         "XDOAHRK000408
    refresh: rgdir, dir2.                                "XDOAHRK000408
    clear: %%_dir2. refresh %%_dir2.                     "XDOAHRK000000
  endif.
END-OF-DEFINITION.
DEFINE own-rp-exp-c2-co.

   co-version-number = '01'.
   pcl2-versn = '01'.
   export
     co-version                                                      "OBJECTS_FOR_EXPORT
     versc
     wpbp
     abc
     rt
     crt
     bt
     c0
     c1
     v0
     vcp
     alp
     dft
     grt
     ls
     status
     arrrs
     ddntk
     accr
     bentab
     ab
     fund
     average
     modif
     9lredta
     9lrersl
     ladiv
     cosso
     cossa
     cosev
     coabs
     cobtr
     cogar
     cotax
     coava
     copro
     coicc
     cossp
   to database pcl2(co) id rx-key." using pcl2_exp_imp.
   rp-imp-co-subrc = sy-subrc.
 END-OF-DEFINITION.
