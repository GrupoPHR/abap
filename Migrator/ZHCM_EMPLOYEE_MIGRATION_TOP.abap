*&---------------------------------------------------------------------*
*&  Include           ZHCM_EMPLOYEE_MIGRATION_TOP
*&---------------------------------------------------------------------*

TABLES: zthcm_eq_migrar,
        zthcm_eq_entit,
        ptquoded,
        t500l,
        pcl1,
        pcl2.


TYPES: BEGIN OF t_inftab,
         infty TYPE infotyp,
         ppnnn TYPE ppnnn,
         dbtab TYPE dbtabl,
       END OF t_inftab.

CONSTANTS: include_for_export_routine TYPE progname VALUE 'ZHCM_I_UPDATE_CLUSTER'.

DATA: BEGIN OF gt_log OCCURS 0,
        id      TYPE char10,
        pernr_old   TYPE pernr_d,
        pernr_new   TYPE pernr_d,
        message TYPE camsg,
      END OF gt_log.
*DATA: gt_log TYPE TABLE OF zhcm_rfc_log.
DATA: wa_log LIKE LINE OF gt_log.

DATA: BEGIN OF subroutine_pool,
        clst_id LIKE t500l-relid,
        isocode LIKE t500l-intca,
        name    LIKE tforms-form,
      END OF subroutine_pool.
DATA: import_subpools LIKE subroutine_pool OCCURS 0.

DATA: gt_data TYPE TABLE OF zcopy_oview_man.

DATA: BEGIN OF COMMON PART payroll_results_1.
INCLUDE rpc2rx19.        " PCL2-Data Cluster RG general
INCLUDE pc2rxmx0.                      " PCL2-Data Cluster MX Mexico
DATA: END OF COMMON PART.
*
DATA: BEGIN OF COMMON PART  payroll_results_2 .
INCLUDE rpc2rx02 .         " PCL2-Data Cluster RG common with Cl. B2
DATA: END OF COMMON PART.

INCLUDE rpppxd00.         " Data-Definitions PCL1(2)-Buffer

DATA: BEGIN OF COMMON PART buffer.
INCLUDE rpppxd10.         " Data-Definitions PCL1(2)-Buffer
DATA: END OF COMMON PART.

DATA: BEGIN OF COMMON PART cluster_directory.
INCLUDE rpc2cd09.
DATA: END OF COMMON PART.

*&---------------------------------------------------------------------*
*&  Include
*&---------------------------------------------------------------------*
INCLUDE rpppxm00.         " Module PCL1(2)-buffer

* Audit import/export
INCLUDE rpcbdt00.
INCLUDE rpcblo00.
INCLUDE rpcbsh00.
* B2 clust import/export
INCLUDE rpc2b201.
INCLUDE rpc2b202.
