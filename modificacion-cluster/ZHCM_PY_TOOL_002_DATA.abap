*&---------------------------------------------------------------------*
*&  Include           ZHCM_PY_TOOL_002_DATA
*&---------------------------------------------------------------------*
INFOTYPES: 0001,
           0002.

TABLES: t001,
        t001p,
        pcl1,
        pcl2,
        pc202,
        pernr,
        t500l,
        t549q,
        t512w,
        t51av_2w,
        sscrfields.

TYPE-POOLS: slis.

DATA: BEGIN OF COMMON PART payroll_results_1.
INCLUDE rpc2rx19.        " PCL2-Data Cluster RG general
INCLUDE pc2rxcl0.        " PCL2-Data Cluster RG Chile
DATA: END OF COMMON PART.

DATA: BEGIN OF COMMON PART  payroll_results_2 .
INCLUDE rpc2rx02.
DATA: END OF COMMON PART.

INCLUDE rpppxd00.

DATA: BEGIN OF COMMON PART buffer.
INCLUDE rpppxd10.
DATA: END OF COMMON PART.

DATA: BEGIN OF COMMON PART cluster_directory.
INCLUDE rpc2cd09.
DATA: END OF COMMON PART.

INCLUDE rpppxm00.         " Module PCL1(2)-buffer

DATA: pack         TYPE p,
      w_per_tra     TYPE i,
      apern         LIKE pernr-pernr,
      calcmolga     LIKE t500l-molga VALUE '39',
      rg_calcmolga  LIKE t500l-molga,
      v_permo       LIKE t549q-permo,
      message(80)   TYPE c,
      obegd         TYPE d.

DATA: xfeld TYPE x, xfeld2 TYPE x.

DATA:  x128 TYPE x VALUE '80'.

DATA: field_gr1(3) VALUE 'LGA',
      field_gr2(3) VALUE 'TAB'.

DATA: gt_layout TYPE slis_layout_alv.
DATA: gt_fieldcatalogue TYPE slis_fieldcat_alv OCCURS 0,
      gv_fieldcat_line      TYPE slis_fieldcat_alv.

CONSTANTS: true VALUE 'X'.

* Tabla interna con valores de cc-nominas.
DATA: BEGIN OF it_t512w OCCURS 0.
        INCLUDE STRUCTURE t512w .
DATA: END OF it_t512w .

* Tabla de Promedios.
DATA: i51av_2w LIKE t51av_2w OCCURS 0 WITH HEADER LINE.

* Tablas ultimo cluster
DATA: BEGIN OF ort OCCURS 0.
        INCLUDE STRUCTURE pc207 .
DATA: END OF ort .

**********************************************************
*Tablas de nómina para edición
DATA: BEGIN OF gt_rt OCCURS 0,
        xfeld TYPE xfeld,
        pernr LIKE pernr-pernr,
        seqnr LIKE pc261-seqnr,
        fpend LIKE pc261-fpend.
        INCLUDE STRUCTURE pc207.
DATA: END OF gt_rt.

DATA: BEGIN OF gt_wpbp OCCURS 0,
        xfeld TYPE xfeld,
        pernr LIKE pernr-pernr,
        seqnr LIKE pc261-seqnr,
        fpend LIKE pc261-fpend.
        INCLUDE STRUCTURE pc205.
DATA: END OF gt_wpbp.

DATA: BEGIN OF gt_bt OCCURS 0,
        xfeld TYPE xfeld,
        pernr LIKE pernr-pernr,
        seqnr LIKE pc261-seqnr,
        fpend LIKE pc261-fpend.
        INCLUDE STRUCTURE pc209.
DATA: END OF gt_bt.

DATA: BEGIN OF gt_v0 OCCURS 0,
        xfeld TYPE xfeld,
        pernr LIKE pernr-pernr,
        seqnr LIKE pc261-seqnr,
        fpend LIKE pc261-fpend.
        INCLUDE STRUCTURE pc20c.
DATA: END OF gt_v0.

DATA: BEGIN OF gt_ab OCCURS 0,
        xfeld TYPE xfeld,
        pernr LIKE pernr-pernr,
        seqnr2 LIKE pc261-seqnr,
        fpend LIKE pc261-fpend.
        INCLUDE STRUCTURE pc20i.
DATA: END OF gt_ab.

DATA: BEGIN OF gt_crt OCCURS 0,
        xfeld TYPE xfeld,
        pernr LIKE pernr-pernr,
        seqnr LIKE pc261-seqnr,
        fpend LIKE pc261-fpend.
        INCLUDE STRUCTURE pc208.
DATA: END OF gt_crt.

DATA: BEGIN OF gt_cltax OCCURS 0,
        xfeld TYPE xfeld,
        pernr LIKE pernr-pernr,
        seqnr LIKE pc261-seqnr,
        fpend LIKE pc261-fpend.
        INCLUDE STRUCTURE pc2ra.
DATA: END OF gt_cltax.

DATA: BEGIN OF gt_clhwk OCCURS 0,
        xfeld TYPE xfeld,
        pernr LIKE pernr-pernr,
        seqnr LIKE pc261-seqnr,
        fpend LIKE pc261-fpend.
        INCLUDE STRUCTURE pc2rb.
DATA: END OF gt_clhwk.

DATA: BEGIN OF gt_ee OCCURS 0,
        pernr    LIKE pernr-pernr,
        seqnr    LIKE pc261-seqnr,
        fpend    LIKE pc261-fpend,
      END OF gt_ee.

DATA: gt_temp_rt LIKE gt_rt OCCURS 0 WITH HEADER LINE.

FIELD-SYMBOLS: <gfs_single_tab> TYPE table.

DEFINE rp-imp-c2-cl.
   clear:
     OCL-VERSION,
     VERSC,
     WPBP, WPBP[],
     ABC, ABC[],
     RT, RT[],
     CRT, CRT[],
     BT, BT[],
     C0, C0[],
     C1, C1[],
     V0, V0[],
     VCP, VCP[],
     ALP, ALP[],
     DFT, DFT[],
     GRT, GRT[],
     LS, LS[],
     STATUS,
     ARRRS, ARRRS[],
     DDNTK, DDNTK[],
     ACCR, ACCR[],
     BENTAB, BENTAB[],
     AB, AB[],
     FUND, FUND[],
     AVERAGE, AVERAGE[],
     MODIF, MODIF[],
     CODIST, CODIST[],
     LIFL, LIFL[],
     LIDI, LIDI[],
     CLTAX, CLTAX[],
     CLHWK, CLHWK[].
   import
     CL-VERSION TO OCL-VERSION
     VERSC
     WPBP
     ABC
     RT
     CRT
     BT
     C0
     C1
     V0
     VCP
     ALP
     DFT
     GRT
     LS
     STATUS
     ARRRS
     DDNTK
     ACCR
     BENTAB
     AB
     FUND
     AVERAGE
     MODIF
     CODIST
     LIFL
     LIDI
     CLTAX
     CLHWK
   from database PCL2(CL) id rx-key using pcl2_exp_imp
   IGNORING STRUCTURE BOUNDARIES.
   rp-imp-CL-subrc = sy-subrc.
   CL-VERSION-number = '01'.
   if sy-subrc eq 0 and
      OCL-VERSION-number ne CL-VERSION-number.
      rp-imp-CL-subrc = 8.
   endif.
END-OF-DEFINITION.

"*********************************************************
" export definition for cluster CO Colombia
"*********************************************************
DEFINE rp-exp-c2-cl.

   CL-VERSION-number = '01'.
   pcl2-versn = '01'.
   export
     CL-VERSION
     VERSC
     WPBP
     ABC
     RT
     CRT
     BT
     C0
     C1
     V0
     VCP
     ALP
     DFT
     GRT
     LS
     STATUS
     ARRRS
     DDNTK
     ACCR
     BENTAB
     AB
     FUND
     AVERAGE
     MODIF
     CODIST
     LIFL
     LIDI
     CLTAX
     CLHWK
   to database PCL2(CL) id rx-key using pcl2_exp_imp.
   rp-imp-CL-subrc = sy-subrc.
END-OF-DEFINITION.
 DEFINE rp-imp-c2-cl-o.
   clear:
     OCL-VERSION,
     OVERSC,
     OWPBP, OWPBP[],
     OABC, OABC[],
     ORT, ORT[],
     OCRT, OCRT[],
     OBT, OBT[],
     OC0, OC0[],
     OC1, OC1[],
     OV0, OV0[],
     OVCP, OVCP[],
     OALP, OALP[],
     ODFT, ODFT[],
     OGRT, OGRT[],
     OLS, OLS[],
     OSTATUS,
     OARRRS, OARRRS[],
     ODDNTK, ODDNTK[],
     OACCR, OACCR[],
     OBENTAB, OBENTAB[],
     OAB, OAB[],
     OFUND, OFUND[],
     OAVERAGE, OAVERAGE[],
     OMODIF, OMODIF[],
     OCODIST, OCODIST[],
     OLIFL, OLIFL[],
     OLIDI, OLIDI[],
     OCLTAX, OCLTAX[],
     OCLHWK, OCLHWK[].
   import
     CL-VERSION TO OCL-VERSION VERSC TO OVERSC
     WPBP TO OWPBP
     ABC TO OABC
     RT TO ORT
     CRT TO OCRT
     BT TO OBT
     C0 TO OC0
     C1 TO OC1
     V0 TO OV0
     VCP TO OVCP
     ALP TO OALP
     DFT TO ODFT
     GRT TO OGRT
     LS TO OLS
     STATUS TO OSTATUS
     ARRRS TO OARRRS
     DDNTK TO ODDNTK
     ACCR TO OACCR
     BENTAB TO OBENTAB
     AB TO OAB
     FUND TO OFUND
     AVERAGE TO OAVERAGE
     MODIF TO OMODIF
     CODIST TO OCODIST
     LIFL TO OLIFL
     LIDI TO OLIDI
     CLTAX TO OCLTAX
     CLHWK TO OCLHWK
   from database PCL2(CL) id orx-key using pcl2_exp_imp
   IGNORING STRUCTURE BOUNDARIES.
   rp-imp-CL-subrc = sy-subrc.
   CL-VERSION-number = '01'.
   if sy-subrc eq 0 and
      OCL-VERSION-number ne CL-VERSION-number.
      rp-imp-CL-subrc = 8.
   endif.

 END-OF-DEFINITION.

 "******************************************************
 " refresh definition for cluster CL Chile
 "******************************************************
 DEFINE rp-ref-c2-cl.
   clear:
     RX-KEY,
     OCL-VERSION,
     VERSC,
     WPBP, WPBP[],
     ABC, ABC[],
     RT, RT[],
     CRT, CRT[],
     BT, BT[],
     C0, C0[],
     C1, C1[],
     V0, V0[],
     VCP, VCP[],
     ALP, ALP[],
     DFT, DFT[],
     GRT, GRT[],
     LS, LS[],
     STATUS,
     ARRRS, ARRRS[],
     DDNTK, DDNTK[],
     ACCR, ACCR[],
     BENTAB, BENTAB[],
     AB, AB[],
     FUND, FUND[],
     AVERAGE, AVERAGE[],
     MODIF, MODIF[],
     CODIST, CODIST[],
     LIFL, LIFL[],
     LIDI, LIDI[],
     CLTAX, CLTAX[],
     CLHWK, CLHWK[].
 END-OF-DEFINITION.

 "*********************************************************
 " refresh definition-'O' tables-for cluster CL Chile
 "*********************************************************
 DEFINE rp-ref-c2-cl-o.
   clear:
     ORX-KEY,
     OCL-VERSION,
     OVERSC,
     OWPBP, OWPBP[],
     OABC, OABC[],
     ORT, ORT[],
     OCRT, OCRT[],
     OBT, OBT[],
     OC0, OC0[],
     OC1, OC1[],
     OV0, OV0[],
     OVCP, OVCP[],
     OALP, OALP[],
     ODFT, ODFT[],
     OGRT, OGRT[],
     OLS, OLS[],
     OSTATUS,
     OARRRS, OARRRS[],
     ODDNTK, ODDNTK[],
     OACCR, OACCR[],
     OBENTAB, OBENTAB[],
     OAB, OAB[],
     OFUND, OFUND[],
     OAVERAGE, OAVERAGE[],
     OMODIF, OMODIF[],
     OCODIST, OCODIST[],
     OLIFL, OLIFL[],
     OLIDI, OLIDI[],
     OCLTAX, OCLTAX[],
     OCLHWK, OCLHWK[].
 END-OF-DEFINITION.
