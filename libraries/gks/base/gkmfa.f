C# IL>=a, OL>=0
      SUBROUTINE GKMFA(NCD,STR)
*
* (C) COPYRIGHT ICL & SERC  1984
*

*---------------------------------------------------------------------
*
*  RUTHERFORD / ICL GKS SYSTEM
*
*  Author:             DSG
*
      INCLUDE '../include/check.inc'
*
*  PURPOSE OF THE ROUTINE
*  ----------------------
*     Metafile interpreter Fill Area output primitive routine
*
*  MAINTENANCE LOG
*  ---------------
*     21/07/83  DSG   Original version stabilized
*     29/11/83  DSG   FORTRAN binding changes
*     21/02/86  DCS   Replace call to GKSCTM by its contents.
*                     Remove unused locals I, J, NK, ISETY.
*     18/03/86  DSG   I246 - Corrected error checking after GKSTAL call.
*     19/01/87  DCS   IS conversion. Remove metafile index and get fill
*                     area attributes from GKS State List.
*     08/03/91  DCS   Corrected updating of the normalisation
*                     transformation and clipping information -
*                     introduced section of code to set the
*                     data as in GKS State List (S475).
*
*  ARGUMENTS
*  ---------
*     INP   NCD    Length of item data record
*     INP   STR    Item data record
*
      INTEGER NCD
      CHARACTER*80 STR(NCD)
*
*  COMMON BLOCK USAGE
*  ------------------
*     Read   /WCA/    RGN: regenerate
*     Read   /ERR/    KERROR
*     Modify /SL/     KSTRWK, KSFAWK
*
      INCLUDE '../include/gkwke.par'
      INCLUDE '../include/GKS_PAR'
      INCLUDE '../include/gksl.par'
      INCLUDE '../include/gkdt.par'
      INCLUDE '../include/gkhp.par'
      INCLUDE '../include/gkpid.par'
      INCLUDE '../include/gkwca.cmn'
      INCLUDE '../include/gksl.cmn'
      INCLUDE '../include/gkerr.cmn'
      INCLUDE '../include/gkstk.cmn'
*
*  LOCALS
*  ------
      INTEGER K,NPTS,ISET,ISETX,IAT,ILA(1)
*
*  ERRORS
*  ------
*     100  Number of points invalid
*
*---------------------------------------------------------------------


      IAT=1
      CALL GKUPSI(IAT,STR,1,ILA)
      IAT=IAT+KPDSSZ
      NPTS=ILA(1)
      IF ( NPTS.LT.3 ) THEN
        KERROR = 100
        GOTO 999
      ENDIF

* Bring Fill Area attributes up to date

      IF (KSFAWK.NE.KGKSFN) THEN
*        Set transformation and clipping information as in GKS state list.
         IF(KSTRWK.NE.KGKSFN)THEN
           CALL GKCCTG
           IF(KERROR.NE.0) GOTO 990
           CALL GKSACW(KNT, 1,KDAT, 1,QDAT,QDAT,1,CH)
           IF(KERROR.NE.0) GOTO 990
*          Set flag
           KSTRWK=KGKSFN
         ENDIF
*        Update attributes.
         CALL GKCFAG
         IF(KERROR.NE.0) GOTO 990
         CALL GKSACW(KSFAA,1,KDAT,1,QDAT,QDAT,1,CH)
         IF (KERROR .NE. 0) GOTO 990
*        Set flag
         KSFAWK=KGKSFN
      ENDIF
*
* Set transformation and clip information for metafile interpretation.
*

      IF (KSTRWK.NE.KMI) THEN
          CALL GKCCTM
          IF(KERROR.NE.0) GOTO 999
          CALL GKSACW(KNT, 1,KDAT, 1,QDAT,QDAT,1,CH)
          IF(KERROR.NE.0) GOTO 999
*        Set flag
          KSTRWK=KMI
      ENDIF

* Quit if truncation occurred on metafile input

      IF(NPTS.GT.(NCD*80-KPDSSZ)/(KPDRSZ*2)) THEN
        KERROR = 165
        GOTO 999
      ENDIF

* Try to get enough stack for all the points
      CALL GKSTAL(KREALS,NPTS*2,ISETX)
      IF (KERROR.NE.0) GOTO 999
      CALL GKSTAL(KREALS,NPTS*2,ISET)
      IF (KERROR.NE.0) GOTO 990

      CALL GKUPR(IAT,STR,NPTS*2,QSTACK(ISET))

* Repack the points into the X array and the Y array
      DO 20 K=1,NPTS
        QSTACK(ISETX+K-1) = QSTACK(ISET+(K-1)*2)
        QSTACK(ISETX+NPTS+K-1) = QSTACK(ISET+(K-1)*2+1)
   20 CONTINUE

* Send the points to all active workstationsp

      CALL GKSACW(KFA,1,KDAT,NPTS,QSTACK(ISETX),
     :               QSTACK(ISETX+NPTS),1,CH)
      IF (KERROR .NE. 0) GOTO 980

* Regenerate if necessary
      IF (KRGN) CALL GKRGN

* Deallocate stack space
  980 CALL GKSTDA (KREALS,ISET)
  990 CALL GKSTDA (KREALS,ISETX)


  999 CONTINUE

      END
