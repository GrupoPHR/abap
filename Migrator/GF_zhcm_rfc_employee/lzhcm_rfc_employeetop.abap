FUNCTION-POOL ZHCM_RFC_EMPLOYEE.            "MESSAGE-ID ..

TABLES: pcl1,
        pcl2,
        t549a,      "Abrechnungskreise
        t549q,      "Abrechnungsperioden
        t549r.  .

DATA: time_results TYPE zptxx_result_pernr,
      no_record_found(1).
DATA: i549q        LIKE t549q OCCURS 0 WITH HEADER LINE.

DATA: get_bezug_reday(2) TYPE n,
      get_kdate_reday(2) TYPE n,
      reday_beg(2) TYPE n,
      reday_end(2) TYPE n.

DATA: cluster_b1 TYPE ptm_cluster_b1.
DATA: t_ntab TYPE TABLE OF dntab.

RANGES: ra_infty FOR p0000-infty.

DATA: BEGIN OF COMMON PART payroll_results_1.
INCLUDE rpc2rx19.        " PCL2-Data Cluster RG general
INCLUDE pc2rxmx0.        "PCL2-Data Cluster MX Mexico
DATA: END OF COMMON PART.

DATA: BEGIN OF COMMON PART  payroll_results_2 .
INCLUDE rpc2rx02.         " PCL2-Data Cluster RG common with Cl. B2
DATA: END OF COMMON PART.

INCLUDE rpppxd00.         " Data-Definitions PCL1(2)-Buffer

DATA: BEGIN OF COMMON PART buffer.
INCLUDE rpppxd10.         " Data-Definitions PCL1(2)-Buffer
DATA: END OF COMMON PART.

DATA: BEGIN OF COMMON PART cluster_directory.
INCLUDE rpc2cd09.
DATA: END OF COMMON PART.

INCLUDE rpppxm00.         " Module PCL1(2)-buffer

INCLUDE rpcmgr09.

TYPES: BEGIN OF t_inftab,
         infty TYPE infotyp,
         ppnnn TYPE ppnnn,
         dbtab TYPE dbtabl,
       END OF t_inftab.

DATA: g_inftab TYPE t_inftab.

* Audit import/export
INCLUDE rpcbdt00.
INCLUDE rpcblo00.
INCLUDE rpcbsh00.
INCLUDE rpc2b201.
