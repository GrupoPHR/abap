*report dummy.

*TABLES: pcl1, pcl2.
*
*DATA: BEGIN OF COMMON PART payroll_results_1.
*INCLUDE rpc2rx19.        " PCL2-Data Cluster RG general
*INCLUDE pc2rxmx0.                      " PCL2-Data Cluster MX Mexico
*DATA: END OF COMMON PART.
*
*DATA: BEGIN OF COMMON PART  payroll_results_2 .
*INCLUDE rpc2rx02 .         " PCL2-Data Cluster RG common with Cl. B2
*DATA: END OF COMMON PART.
*
*INCLUDE rpppxd00.         " Data-Definitions PCL1(2)-Buffer
*
*DATA: BEGIN OF COMMON PART buffer.
*INCLUDE rpppxd10.         " Data-Definitions PCL1(2)-Buffer
*DATA: END OF COMMON PART.
*
*DATA: BEGIN OF COMMON PART cluster_directory.
*INCLUDE rpc2cd09.
*DATA: END OF COMMON PART.
*
*INCLUDE rpppxm00.         " Module PCL1(2)-buffer
*
* INCLUDE zhown_macros.
*
*FORM exp_mx TABLES p_payresult TYPE zhrpaymx_tab_of_results
*                   p_rgdir STRUCTURE pc261
*            USING p_pernr p_clusterid.
*
*  DATA: pack TYPE p.
*
*  CLEAR p_rgdir[].
*  LOOP AT p_payresult WHERE perid = p_pernr.
*
*    rp-ref-c2-mx.
*    rx-key-pernr = p_pernr.
*    pack = p_payresult-evp-seqnr.
*    UNPACK pack TO rx-key-seqno.
*
*$
*
*    MOVE-CORRESPONDING p_payresult-evp TO p_rgdir.
*    APPEND p_rgdir.
*
*    rp-exp-c2-mx.
*      IF rp-imp-mx-subrc EQ 0.
*        PERFORM update_data(rpppxv00).   "Almacena valores de cluster actual
*      ENDIF.
*      rp-init-buffer.
*
*  ENDLOOP.
*
*ENDFORM.                    "EXP_CO
