C# IL>=a, OL>=0
      SUBROUTINE GQPX(IWKID,PX,PY,IER,IPIX)
*
* (C) COPYRIGHT ICL & SERC  1984
*

*---------------------------------------------------------------------
*
*  RUTHERFORD / ICL GKS SYSTEM
*
*  GKS Function name:  INQUIRE PIXEL
*  Author:             NGB
*
      INCLUDE '../include/check.inc'
*
*  PURPOSE OF THE ROUTINE
*  ----------------------
*     User level Inquire Pixel routine
*
*  MAINTENANCE LOG
*  ---------------
*     01/02/83  NGB   Original version stabilized
*     17/02/83  PGLS  Call to PRLG inserted
*     13/06/83  AS    Special case of GQPXA
*     10/01/83  AS    Should make sure transformation is up to date
*     11/01/84  MGC   KWI3 first dimension parameter
*     20/02/86  DCS   Send transformation data to this workstation only
*                     and restore values afterwards (cf GQTXX).
*     22/01/87  DCS   IS conversion. Remove metafile index.
*
*  ARGUMENTS
*  ---------
*     INP IWKID  Workstation Identifier
*     INP PX,PY  Origin World Coordinates
*     OUT IER    Error Indicator
*     OUT IPIX   Pixel's Colour Index

      INTEGER IWKID, IER, IPIX
      REAL PX,PY
*
*  COMMON BLOCK USAGE
*  ------------------
*
      INCLUDE '../include/GKS_PAR'
      INCLUDE '../include/gkwke.par'
      INCLUDE '../include/gkdt.par'
      INCLUDE '../include/gksl.par'
      INCLUDE '../include/gksl.cmn'
      INCLUDE '../include/gkwca.cmn'
      INCLUDE '../include/gkerr.cmn'
*
*---------------------------------------------------------------------


      CALL GKPRLG (KNIL,GWSOP,GSGOP)
      IER = KERROR
      IF (IER .NE. 0) RETURN

* Send transformation data

      CALL GKCCTG
      CALL GKSONW(IWKID, KNT, 1,KDAT, 1,QDAT,QDAT,1,CH)
      IER = KERROR
      IF (IER.NE.0) RETURN

      QWR1 = PX
      QWR2 = PY
      KWI1 = 1
      KWI2 = 1
      KWI3 = 1
      CALL GKSONW(IWKID,KQPXA,1,KDAT,1,QDAT,QDAT,1,CH)
      IER = KERROR
      IF (IER .EQ. 0) IPIX = KDAT(1)

* Send transformation data to correspond to 'last source' flag if
* necessary. (Correspond to KGKSFN at present and so only send for value
* of KMI).

      IF (KSTRWK .EQ. KMI) THEN
        CALL GKCCTM
        CALL GKSONW(IWKID,KNT,1,KDAT,1,QDAT,QDAT,1,CH)
      ENDIF

      END
