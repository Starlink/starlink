C# IL>=a, OL>=1
      SUBROUTINE GQSGA ( ISG, IER, RSGTM, IVIS, IHLIT, RSGP, IDTEC )
*
* (C) COPYRIGHT ICL & SERC  1984
*

*---------------------------------------------------------------------
*
*  RUTHERFORD / ICL GKS SYSTEM
*
*  GKS Function name:  Inquire Segment Attributes
*  Author:             CJW
*
      INCLUDE '../include/check.inc'
*
*  PURPOSE OF THE ROUTINE
*  ----------------------
*     Returns the Segment Attributes
*
*  MAINTENANCE LOG
*  ---------------
*     08/03/83  CJW   Original version stabilized
*     27/06/83  CJW   Implement revised error handling precedure
*     22/11/83  AS    Rewrite
*     20/02/84  JRG   Return number of segs even if zero
*     16/01/86  DRJF  Bug fix S116. Min GKS operating state changed from
*                     GGKOP to GWSOP.
*     21/01/87  ARG   IS conversion. Language binding has transposed
*                     matrix.
*
*  ARGUMENTS
*  ---------
*     IN    ISG    Segment Name
*     OUT   IER    Error indicator
*     OUT   RSGTM  Segment Transformation matrix
*     OUT   IVIS   Visibility
*     OUT   IHLIT  Highlighting
*     OUT   RSGP   Segment Priority
*     OUT   IDTEC  Detectability
*
      INTEGER ISG, IER, IVIS, IHLIT, IDTEC
      REAL RSGTM(6), RSGP
*
*  COMMON BLOCK USAGE
*  ------------------
*
      INCLUDE '../include/gkdt.par'
      INCLUDE '../include/gks.par'
      INCLUDE '../include/gkssl.par'
      INCLUDE '../include/gksl.cmn'
      INCLUDE '../include/gkerr.cmn'
*
*  LOCALS
*  ------
*
      INTEGER INTA(KSGISZ)
      REAL REALA(KSGRSZ)
*
*  ERRORS
*  ------
*     120  Invalid segment name
*     122  Segment does not exist
*
*---------------------------------------------------------------------

      CALL GKPRLG (KNIL, GWSOP, GSGOP)

      IER = KERROR
      IF (KERROR .EQ. 0) THEN
*       Check segment name valid
        IF (ISG.GT.0) THEN
*         Get directory entry for segment if segment exists
          IF ( KSGLST.NE.KNIL )
     :           CALL GKDRGE(KSGLST,ISG,KSGISZ,KSGRSZ,INTA,REALA)
          IF ( KSGLST.NE.KNIL .AND. KERROR.EQ.0 ) THEN
            RSGTM (1) = REALA (1)
            RSGTM (2) = REALA (4)
            RSGTM (3) = REALA (2)
            RSGTM (4) = REALA (5)
            RSGTM (5) = REALA (3)
            RSGTM (6) = REALA (6)
            IVIS  = INTA(KSGVIS)
            IHLIT = INTA(KSGHLT)
            RSGP  = REALA(KSGPRI)
            IDTEC = INTA(KSGDTE)
          ELSE
*           Segment name not found
            IER = 122
          ENDIF
        ELSE
*         Segment name invalid
          IER = 120
        ENDIF
      ENDIF

      END
