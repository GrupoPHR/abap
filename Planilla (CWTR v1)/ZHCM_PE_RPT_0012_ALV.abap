*&---------------------------------------------------------------------*
*&  Include           ZDEMOLOCA_CW_ALV
*&---------------------------------------------------------------------*
"Estructura para Modelo (Interno)
DATA: BEGIN OF GT_MODEL OCCURS 0,
        ANZHL LIKE PC207-ANZHL,
        BETRG LIKE PC207-BETRG,
        TOTAL LIKE T558D-BETRG,
        PERNR LIKE PERNR-PERNR,
        BUKRS   LIKE PC205-BUKRS, "Sociedad
        BUKRS_D LIKE T001-BUTXT,
        WERKS   LIKE P0001-WERKS, "División de personal
        WERKS_D LIKE T500P-NAME1,
        BTRTL   LIKE PC205-BTRTL, "Subdivisión de personal
        BTRTL_D LIKE T001P-BTEXT,
        KOSTL   LIKE P0001-KOSTL, "Centro de coste
        KOSTL_D LIKE HRCA_COSTC-NAME,
        PERSG   LIKE P0001-PERSG, "Grupo de personal
        PERSG_D LIKE T501T-PTEXT,
        PERSK   LIKE PC205-PERSK, "Área de personal
        PERSK_D LIKE T503T-PTEXT,
        ABKRS   LIKE P0001-ABKRS, "Área de nómina
        ABKRS_D LIKE T549T-ATEXT,
        PLANS   like P0001-PLANS, "posicion
        PLANS_D LIKE P1000-STEXT,
        STELL   like P0001-STELL,
        STELL_D LIKE P1000-STEXT,
        ZZRLAB  LIKE P0001-ZZRLAB,"Régimen Laboral
        ZZRLABD LIKE DD07V-DDTEXT,
        ZZCATT  LIKE P0001-ZZCATT,"Categoría de Trabajador
        ZZCATTD LIKE DD07V-DDTEXT,
        ZZTIPT  LIKE P0001-ZZTIPT,"Tipo de Trabajador
        ZZTIPTD LIKE DD07V-DDTEXT,
        ZZCEST  LIKE P0001-ZZCEST,"Código de Establecimiento
        ZZCESTD LIKE DD07V-DDTEXT,
        ZZCOOC  LIKE P0001-ZZCOOC,"código de ocupación
        ZZCOOCD LIKE DD07V-DDTEXT,
        NACHN   LIKE P0002-NACHN, "Apellido
        NAME2   LIKE P0002-NAME2, "Apellido
        VORNA   LIKE P0002-VORNA, "Nombre
        GESCH   LIKE P0002-GESCH, "sexo
        GBDAT   LIKE P0002-GBDAT, "Fecha de nacimiento
        GBLND   LIKE P0002-GBLND, "País de nacimiento
        GBLND_D LIKE T005T-LANDX,
        NATIO   LIKE P0002-NATIO, "Nacionalidad
        NATIO_D LIKE T005T-NATIO,
        ZZNIVE  LIKE P0002-ZZNIVE, "Nivel Educacional
        ZZNIVED LIKE DD07V-DDTEXT,
        ZZCDIS  LIKE P0002-ZZCDIS, "Codigo de Discapacidad
        ZZIRES  LIKE P0002-ZZIRES, "madre con resp. Familiar
        SCHKZ   like P0007-SCHKZ,  "Regla para plan horario trabajo
        SCHKZ_D LIKE t508s-rtext,
        ZTERF   like P0007-ZTERF, "Status empleado Gest. tiempos
        ZTERF_D like T555V-ZTEXT,
        BANKS   LIKE P0009-BANKS, "país del banco
        BANKS_D LIKE T005T-LANDX,
        BANKL   LIKE P0009-BANKL, "Clave de banco
        BANKL_D LIKE BNKA-BANKA,
        BANKN   LIKE P0009-BANKN, "Nº cuenta bancaria
        ZLSCH   LIKE P0009-ZLSCH, "Vía de pago
        ZZCTAI  LIKE P0009-ZZCTAI, "Cuenta Interbancaria
        CF0141  LIKE P0041-DAT01,  "Fecha ingreso
        CFZC41  LIKE P0041-DAT01,  "Fecha de cese
        ICTYP   LIKE P0185-ICTYP,  "Clase de identificación (clase ID)
        ICTYP_D like T5R06-ICTXT,
        ICNUM   LIKE P0185-ICNUM,  "Número ID
        ZPENS   LIKE P9957-EMFSL,  "Pensiones
        ZPENS_D LIKE T521B-EMFNA,
        ZPENN   LIKE P9957-MTGLN, "Número de asociado
        ZPENF   LIKE P9957-BUDAT, "Fecha de Incripción
        ZPEMS   LIKE P9957-EMFSL, "Pensiones mixtas
        ZPEMS_D LIKE T521B-EMFNA,
        ZPEMN   LIKE P9957-MTGLN, "Número de asociado
        ZPEMF   LIKE P9957-BUDAT, "Fecha de Incripción
      END   OF GT_MODEL.
DATA GT_FIELD_SLIS TYPE SLIS_T_FIELDCAT_ALV.
DATA WA_FIELD_SLIS TYPE LINE OF SLIS_T_FIELDCAT_ALV.
DATA GT_FIELD_MODEL_LVC TYPE LVC_T_FCAT.
"Definición Configuración (
DATA: BEGIN OF GT_CONFIG OCCURS 1,
        AGRUP(10) TYPE C,  "Agrupador
        LGTXT    TYPE LGTXT, "Decripción
        ANZ_F    TYPE CHAR1, "Flag para ANZHL (Cantidad)
        ANZ_N    TYPE CHAR5, "Nombre Col. ANZHL(Interno)
        ANZ_P    TYPE I,     "N° Columna ANZHL
        ANZ_D    TYPE CHAR1, "Flag de Salida ('' = Si, 'X' = No)
        BET_F    TYPE CHAR1, "Flag para BETRG (Importe)
        BET_N    TYPE CHAR5, "Nombre Col. BETRG(Interno)
        BET_P    TYPE I,     "N° Columna BETRG
        BET_D    TYPE CHAR1, "Flag de Salida ('' = Si, 'X' = No)
*       OPERA(1) TYPE C,     " '+' Suma , '-' resta
*       LGART    TYPE TABLE OF HRPAY00_S_LGART_RANGE, "Concepto de Nomina (SAP)
        LGART    TYPE HRPAY00_T_LGART_RANGE,
*       Marcadores de salida
        ANZHL    TYPE CHAR1, "Marcador de Columnas con valores
        BETRG    TYPE CHAR1, "Marcador de Columnas con valores
      END   OF GT_CONFIG.
FIELD-SYMBOLS <FS_CONFIG> LIKE GT_CONFIG.
DATA WA_LGART TYPE HRPAY00_S_LGART_RANGE.
DATA GT_FIELD TYPE LVC_T_FCAT.
DATA LV_INDEX TYPE SY-TABIX.

*FIELD-SYMBOLS:
FIELD-SYMBOLS <GT_DATA> TYPE STANDARD TABLE."tabla
FIELD-SYMBOLS <WA_DATA>."Work Area
FIELD-SYMBOLS <LGART_A> TYPE ANY. "Field Symbol para asignar valores
FIELD-SYMBOLS <LGART_B> TYPE ANY.
FIELD-SYMBOLS <COLUMN> TYPE ANY.

DEFINE ADD_CCNOM.
*
  READ TABLE GT_CONFIG ASSIGNING <FS_CONFIG> WITH KEY AGRUP = &1.
*
  IF ( sy-subrc eq 0 ).
    CLEAR WA_LGART.
    if ( &9 <> space ).
      WA_LGART-SIGN = 'I'.
      WA_LGART-OPTION = 'EQ'.
      WA_LGART-LOW   = &9.
      WA_LGART-HIGH = ''.
      append WA_LGART to <FS_CONFIG>-lgart.
    endif.
  ELSE.
    APPEND INITIAL LINE TO gt_config ASSIGNING <FS_CONFIG>.
    <FS_CONFIG>-agrup = &1."Agrupador
    <FS_CONFIG>-lgtxt = &2."Descripcion Opcional
    <FS_CONFIG>-anz_f = &3."Flag de Cantidad
    <FS_CONFIG>-anz_n = &1."Nombre Interno Cantidad 'BXXXA'
    REPLACE ALL OCCURRENCES OF '/' IN <FS_CONFIG>-anz_n WITH 'B'.
    CONCATENATE <FS_CONFIG>-anz_n 'A' INTO <FS_CONFIG>-anz_n.
    <FS_CONFIG>-anz_p = &4."Numero de Columna
    <FS_CONFIG>-anz_d = &5."Mostrar (' ' = Si , 'X' = No )
    <FS_CONFIG>-bet_f = &6."Flag de Importe
    <FS_CONFIG>-bet_n = &1."Nombre Interno Importe 'BXXXB'
    REPLACE ALL OCCURRENCES OF '/' IN <FS_CONFIG>-bet_n WITH 'B'.
    CONCATENATE <FS_CONFIG>-bet_n 'B' INTO <FS_CONFIG>-bet_n.
    <FS_CONFIG>-bet_p = &7."Numero de Columna
    <FS_CONFIG>-bet_d = &8."Mostrar (' ' = Si , 'X' = No )
    TRANSLATE <FS_CONFIG>-anz_n TO UPPER CASE.
    TRANSLATE <FS_CONFIG>-bet_n TO UPPER CASE.
*
    CLEAR WA_LGART.
    if ( &9 <> space ).
      WA_LGART-SIGN = 'I'.
      WA_LGART-OPTION = 'EQ'.
      WA_LGART-LOW   = &9.
      WA_LGART-HIGH = ''.
      append WA_LGART to <FS_CONFIG>-lgart.
    endif.
 ENDIF.
END-OF-DEFINITION.
*----------------------------------------------------------------------*
FORM INITIALIZE_FIELDCAT USING P_FIELDTAB TYPE LVC_T_FCAT.
*
  DATA: LT_FIELDTAB TYPE SLIS_T_FIELDCAT_ALV.
  DATA: WA_FIELDTAB TYPE LINE OF SLIS_T_FIELDCAT_ALV.
*
  FREE: LT_FIELDTAB,  WA_FIELDTAB.
*
  CALL FUNCTION 'REUSE_ALV_FIELDCATALOG_MERGE'
    EXPORTING
      I_PROGRAM_NAME         = SY-REPID
      I_INTERNAL_TABNAME     = 'GT_MODEL'
      I_INCLNAME             = SY-REPID
    CHANGING
      CT_FIELDCAT            = LT_FIELDTAB
    EXCEPTIONS
      INCONSISTENT_INTERFACE = 1
      PROGRAM_ERROR          = 2
      OTHERS                 = 3.
  IF ( SY-SUBRC NE 0 ).
    MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
          WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ELSE.
    LOOP AT LT_FIELDTAB INTO WA_FIELDTAB.
      CASE WA_FIELDTAB-FIELDNAME.
        WHEN 'ANZHL'.
          WA_FIELDTAB-NO_ZERO = 'X'.
        WHEN 'BETRG'.
          WA_FIELDTAB-NO_ZERO = 'X'.
          WA_FIELDTAB-CURRENCY  = 'PEN'.
        WHEN 'TOTAL'.
          WA_FIELDTAB-SELTEXT_L = 'ST.Imp.'.
          WA_FIELDTAB-CURRENCY  = 'PEN'.
      ENDCASE.
      WA_FIELDTAB-TABNAME   = 'GT_DATA'.
*      wa_fieldtab-seltext_s = wa_fieldtab-seltext_l.
*      wa_fieldtab-seltext_m = wa_fieldtab-seltext_l.
*      wa_fieldtab-reptext_ddic = wa_fieldtab-seltext_l.
      MODIFY LT_FIELDTAB FROM WA_FIELDTAB.
    ENDLOOP.
  ENDIF.
*
  CALL FUNCTION 'LVC_TRANSFER_FROM_SLIS'
    EXPORTING
      IT_FIELDCAT_ALV = LT_FIELDTAB
*     IT_SORT_ALV     =
*     IT_FILTER_ALV   =
*     IS_LAYOUT_ALV   =
    IMPORTING
      ET_FIELDCAT_LVC = P_FIELDTAB
*     ET_SORT_LVC     =
*     ET_FILTER_LVC   =
*     ES_LAYOUT_LVC   =
    TABLES
      IT_DATA         = GT_MODEL
    EXCEPTIONS
      IT_DATA_MISSING = 1
      OTHERS          = 2.
  IF SY-SUBRC <> 0.
* Implement suitable error handling here
  ENDIF.
*
ENDFORM.
*----------------------------------------------------------------------*
FORM FACT_TRANS USING P_LVC_FACT TYPE LVC_T_FCAT
                      P_SLIS_FCAT TYPE SLIS_T_FIELDCAT_ALV.
*
  CALL FUNCTION 'LVC_TRANSFER_TO_SLIS'
    EXPORTING
      IT_FIELDCAT_LVC         = P_LVC_FACT
*     IT_SORT_LVC             =
*     IT_FILTER_LVC           =
*     IS_LAYOUT_LVC           =
    IMPORTING
      ET_FIELDCAT_ALV         = P_SLIS_FCAT
*     ET_SORT_ALV             =
*     ET_FILTER_ALV           =
*     ES_LAYOUT_ALV           =
* TABLES
*     IT_DATA                 =
    EXCEPTIONS
      IT_DATA_MISSING         = 1
      IT_FIELDCAT_LVC_MISSING = 2
      OTHERS                  = 3.
  IF ( SY-SUBRC <> 0 ).
* Implement suitable error handling here
  ENDIF.
ENDFORM.
*----------------------------------------------------------------------*
FORM ADD_CCNOM USING P_FIELD P_LGART P_NAME P_DESCR P_COL P_OUT
                     P_FIELDTAB TYPE LVC_T_FCAT.
*
  DATA WA_FIELD_MODEL_LVC TYPE LINE OF LVC_T_FCAT.
*
  CLEAR WA_FIELD_MODEL_LVC.
* Para LVC
  READ TABLE GT_FIELD_MODEL_LVC WITH KEY FIELDNAME = P_FIELD
  INTO WA_FIELD_MODEL_LVC.
  IF ( SY-SUBRC EQ 0 ).
    WA_FIELD_MODEL_LVC-COL_POS = P_COL.
    WA_FIELD_MODEL_LVC-FIELDNAME = P_NAME.
    IF ( P_DESCR IS NOT INITIAL ).
      WA_FIELD_MODEL_LVC-SCRTEXT_S = P_DESCR."10
      WA_FIELD_MODEL_LVC-SCRTEXT_M = P_DESCR."20
      WA_FIELD_MODEL_LVC-SCRTEXT_L = P_DESCR."40
    ELSE.
      CONCATENATE P_LGART '-' WA_FIELD_MODEL_LVC-SCRTEXT_S
             INTO WA_FIELD_MODEL_LVC-SCRTEXT_S SEPARATED BY SPACE."10
      CONCATENATE P_LGART '-' WA_FIELD_MODEL_LVC-SCRTEXT_M
             INTO WA_FIELD_MODEL_LVC-SCRTEXT_M SEPARATED BY SPACE."20
      CONCATENATE P_LGART '-' WA_FIELD_MODEL_LVC-SCRTEXT_L
             INTO WA_FIELD_MODEL_LVC-SCRTEXT_L SEPARATED BY SPACE."40
    ENDIF.
    WA_FIELD_MODEL_LVC-TOOLTIP   = P_LGART.
    WA_FIELD_MODEL_LVC-NO_OUT    = P_OUT.
    APPEND WA_FIELD_MODEL_LVC TO P_FIELDTAB.
  ENDIF.
*
ENDFORM.
*----------------------------------------------------------------------*
FORM ADD_COLUMN USING P_FIELD P_TEXT P_COL
                      P_FIELDTAB TYPE LVC_T_FCAT.
*
  DATA WA_FIELD_MODEL_LVC TYPE LINE OF LVC_T_FCAT.
*
  CLEAR WA_FIELD_MODEL_LVC.
* Para LVC
  READ TABLE GT_FIELD_MODEL_LVC WITH KEY FIELDNAME = P_FIELD
  INTO WA_FIELD_MODEL_LVC.
  IF ( SY-SUBRC EQ 0 ).
    WA_FIELD_MODEL_LVC-COL_POS = P_COL.
    IF ( P_TEXT IS NOT INITIAL ).
      WA_FIELD_MODEL_LVC-SCRTEXT_S = P_TEXT.
      WA_FIELD_MODEL_LVC-SCRTEXT_M = P_TEXT.
      WA_FIELD_MODEL_LVC-SCRTEXT_L = P_TEXT.
    ENDIF.
    APPEND WA_FIELD_MODEL_LVC TO P_FIELDTAB.
  ENDIF.
*
ENDFORM.
*----------------------------------------------------------------------*
*----------------------------------------------------------------------*
FORM CREA_DYNAMIC_TABLE.
  "Referencia de un Objeto
  "Para la tabla interna dinámica
  DATA LO_NEWTABLE TYPE REF TO DATA.
  "Invocamos al método 'create_dynamic_table' de la clase
  "cl_alv_table_create
  "Como es una clase estática accedemos directamente desde
  "la clase al método.
  CALL METHOD CL_ALV_TABLE_CREATE=>CREATE_DYNAMIC_TABLE
    EXPORTING
      IT_FIELDCATALOG = GT_FIELD
    IMPORTING
      EP_TABLE        = LO_NEWTABLE.

  ASSIGN LO_NEWTABLE->* TO <GT_DATA>.
*
ENDFORM.
*----------------------------------------------------------------------*
FORM CREA_DYNAMIC_WAREA.
  "Referencia de un Objeto
  "Para el área de trabajo de la tabla interna dinámica.
  DATA LO_NEWLINE TYPE REF TO DATA.

  CREATE DATA LO_NEWLINE LIKE LINE OF <GT_DATA>.
  ASSIGN LO_NEWLINE->* TO <WA_DATA>.
ENDFORM.
*----------------------------------------------------------------------*
FORM SET_PF_STATUS USING RT_EXTAB TYPE SLIS_T_EXTAB.        "#EC CALLED
  SET PF-STATUS 'D1000_STATUS'.
ENDFORM.
*----------------------------------------------------------------------*
FORM DISPLAY_ALV.
  DATA P_FOLDER TYPE STRING.
  DATA: BEGIN OF T_HEAD OCCURS 0,
          CAMPO(20) TYPE C,
        END OF T_HEAD.

  DATA LAYOUT TYPE SLIS_LAYOUT_ALV.
  LAYOUT-COLWIDTH_OPTIMIZE = 'X'.
*  layout-zebra = 'X'.

  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
    EXPORTING
      I_CALLBACK_PROGRAM       = SY-REPID
      I_CALLBACK_PF_STATUS_SET = 'SET_PF_STATUS'
      I_CALLBACK_USER_COMMAND  = 'USER_COMMAND'
      IS_LAYOUT                = LAYOUT
      IT_FIELDCAT              = GT_FIELD_SLIS
    TABLES
      T_OUTTAB                 = <GT_DATA>.
*
ENDFORM.
*----------------------------------------------------------------------*
FORM BUILD_STRUC.
*
  LOOP AT GT_CONFIG.
    IF ( GT_CONFIG-ANZ_F EQ 'X' ).
      PERFORM ADD_CCNOM
      USING 'ANZHL' GT_CONFIG-AGRUP
                    GT_CONFIG-ANZ_N
                    GT_CONFIG-LGTXT
                    GT_CONFIG-ANZ_P
                    GT_CONFIG-ANZ_D
                    GT_FIELD.
    ENDIF.
    IF ( GT_CONFIG-BET_F EQ 'X' ).
      PERFORM ADD_CCNOM
      USING 'BETRG' GT_CONFIG-AGRUP
                    GT_CONFIG-BET_N
                    GT_CONFIG-LGTXT
                    GT_CONFIG-BET_P
                    GT_CONFIG-BET_D
                    GT_FIELD.
    ENDIF.
  ENDLOOP.
*  PERFORM add_ccnom USING 'TOTAL' 'TOTAL' 'TT01B' 99 ' ' gt_field.
*
ENDFORM.
*----------------------------------------------------------------------*
FORM USER_COMMAND USING R_UCOMM LIKE SY-UCOMM               "#EC CALLED
                        RS_SELFIELD TYPE SLIS_SELFIELD.     "#EC CALLED

  CASE R_UCOMM.
    WHEN 'XXX'.

  ENDCASE.
  CLEAR: R_UCOMM.
*
ENDFORM.
*----------------------------------------------------------------------*
FORM AJUST_FIELDCAT.
*
  LOOP AT GT_CONFIG WHERE ANZHL EQ ' ' OR BETRG EQ ' '.
    IF ( GT_CONFIG-ANZ_D EQ ' ' AND GT_CONFIG-ANZHL EQ ' ' ).
      READ TABLE GT_FIELD_SLIS INTO WA_FIELD_SLIS
      WITH KEY FIELDNAME = GT_CONFIG-ANZ_N.
      LV_INDEX = SY-TABIX.
      IF ( SY-SUBRC EQ 0 ).
        WA_FIELD_SLIS-NO_OUT = 'X'.
        MODIFY GT_FIELD_SLIS FROM WA_FIELD_SLIS INDEX LV_INDEX.
      ENDIF.
    ENDIF.
    IF ( GT_CONFIG-BET_D EQ ' ' AND GT_CONFIG-BETRG EQ ' ' ).
      READ TABLE GT_FIELD_SLIS  INTO WA_FIELD_SLIS
      WITH KEY FIELDNAME = GT_CONFIG-BET_N.
      LV_INDEX = SY-TABIX.
      IF ( SY-SUBRC EQ 0 ).
        WA_FIELD_SLIS-NO_OUT = 'X'.
        MODIFY GT_FIELD_SLIS FROM WA_FIELD_SLIS INDEX LV_INDEX.
      ENDIF.
    ENDIF.
*
  ENDLOOP.
*
ENDFORM.
*----------------------------------------------------------------------*
FORM GEN_STRUCT.
  PERFORM CREA_DYNAMIC_TABLE.
  PERFORM CREA_DYNAMIC_WAREA.
  PERFORM FACT_TRANS
    USING GT_FIELD GT_FIELD_SLIS."Ajusto Fieldcat
ENDFORM.
*----------------------------------------------------------------------*
