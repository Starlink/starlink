C# IL>=a, OL>=1
      SUBROUTINE GQASWK (ISG, NTH, IER, NASWK, LASWK )
*
* (C) COPYRIGHT ICL & SERC  1984
*

*---------------------------------------------------------------------
*
*  RUTHERFORD / ICL GKS SYSTEM
*
*  GKS Function name:  INQUIRE SET MEMBER OF ASSOCIATED WORKSTATIONS
*  Author:             CJW
*
      INCLUDE '../include/check.inc'
*
*  PURPOSE OF THE ROUTINE
*  ----------------------
*     Returns the Nth member of the set of associated workstations
*
*  MAINTENANCE LOG
*  ---------------
*     08/03/83  CJW   Original version stabilized
*     27/06/83  CJW   Implement revised error handling precedure
*     23/11/83  AS    Update to new binding
*     20/02/84  JRG   Prevent entry to _KDRGE with empty segment list
*     22/01/87  JCS   IS conversion. Error number changed.
*     18/05/87  DCS   Cycle through open workstations using KOPPT
*                     rather than KWKID directly. Use KOPPT to retrieve
*                     NTH workstation associated with segment (S264).
*     10/07/89  RMK   Corrected value of LASWK returned - was always
*                     using the first workstation identifier (S348).
*     27/09/90  KEVP  Report error 2002 only if there is at least one
*                     associated workst'n and the list element requested
*                     is non-zero (C41). Required by GKS FORTRAN BINDING.
*     23/11/90  KEVP  Add workstation to total, only if no error (C60).
*
*  ARGUMENTS
*  ---------
*     IN    ISG    Segment Name
*     IN    NTH    Set member requested
*     OUT   IER    Error indicator
*     OUT   NASWK  Number of Associated workstations
*     OUT   LASWK  Nth member of set of associated workstations
*
      INTEGER ISG, NTH, IER, NASWK, LASWK
*
*  COMMON BLOCK USAGE
*  ------------------
*
      INCLUDE '../include/gks.par'
      INCLUDE '../include/gkdt.par'
      INCLUDE '../include/gkssl.par'
      INCLUDE '../include/gkwke.par'
      INCLUDE '../include/gksl.cmn'
      INCLUDE '../include/gkerr.cmn'
      INCLUDE '../include/gkwcb.cmn'
      INCLUDE '../include/gkwca.cmn'
*
*  LOCALS
*  ------
*
      INTEGER INTA(KSGISZ), I, N
      REAL REALA(KSGRSZ)
*
*  ERRORS
*  ------
*     120  Invalid segment name
*     122  Segment does not exist
*    2002  Set member not available.
*
*---------------------------------------------------------------------


      CALL GKPRLG(KNIL, GWSOP, GSGOP)
      IER = KERROR

      IF (KERROR .EQ. 0) THEN
*       Check segment name valid
        IF (ISG.GT.0) THEN
*         Get directory entry for segment to see if segment exists
          IF ( KSGLST.NE.KNIL )
     :           CALL GKDRGE(KSGLST,ISG,KSGISZ,KSGRSZ,INTA,REALA)
          IF ( KSGLST.NE.KNIL .AND. KERROR.EQ.0 ) THEN
            N = 0
            LASWK = KNIL
*           For each open workstation see if it has the segment
            DO 10 I=1,KNOPWK
              KWI1 = KREL
              KWI2 = ISG
              CALL GKSONW(KWKID(KOPPT(I)), KQSGWK,1,INTA,1,QDAT,QDAT,
     :                                                             1,CH)
              IF (KERROR .EQ. 0) THEN
                 N = N + 1
                 IF (N.EQ.NTH) LASWK = KWKID(KOPPT(I))
              ELSE
                 KERROR = 0
              ENDIF
   10       CONTINUE
            NASWK = N
            IF (LASWK.EQ.KNIL)THEN
              IF((NTH.NE.0).AND.(NASWK.NE.0)) IER=2002
            ENDIF
          ELSE
*           Segment name does not exist
            IER = 122
          ENDIF
        ELSE
            IER = 120
        ENDIF

      ENDIF

      END
