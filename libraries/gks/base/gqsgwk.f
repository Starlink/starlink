C# IL>=a, OL>=1
      SUBROUTINE GQSGWK (IWKID,NTH,IER,IO,ISGNAM)
*
* (C) COPYRIGHT ICL & SERC  1984
*

*---------------------------------------------------------------------
*
*  RUTHERFORD / ICL GKS SYSTEM
*
*  GKS Function name:  INQUIRE SET MEMBER OF SEGMENT NAMES ON WORKSTATION
*  Author:             AS
*
      INCLUDE '../include/check.inc'
*
*  PURPOSE OF THE ROUTINE
*  ----------------------
*     Returns Nth member of set of segment names on workstation
*
*  MAINTENANCE LOG
*  ---------------
*     04/10/83  AS    Original version stabilized
*     17/01/84  JRG   Different wkstn interface
*     20/02/84  JRG   Return number of segs even if zero
*     22/01/87  JCS   IS conversion. Error number changed.
*     27/09/90  KEVP  Changed error checking to conform to GKS FORTRAN
*                     BINDING standard (C41). See comments in GKQXXI.
*     23/11/90  KEVP  Removed IF condition on set member requested
*                     so that the number of segments is always returned
*                     as required in GKS FORTRAN BINDING standard (C61).
*
*  ARGUMENTS
*  ---------
*     INP IWKID   workstation identifier
*     INP NTH     set member requested
*     OUT IER     error indicator
*     OUT IO      number of segment names
*     OUT ISGNAM  Nth member of set of stored segments for this workstation
*
      INTEGER IWKID, NTH, IER, IO, ISGNAM
*
*  COMMON BLOCK USAGE
*  ------------------
*
*     Modify /WCA/ Set up before entering wkstn
*     Read   /ERR/ Inspect KERROR
*
      INCLUDE '../include/gks.par'
      INCLUDE '../include/gkdt.par'
      INCLUDE '../include/gkwke.par'
      INCLUDE '../include/gkwca.cmn'
      INCLUDE '../include/gkerr.cmn'
*
*  ERRORS
*  ------
*     2002  List or set element invalid.
*
*---------------------------------------------------------------------


      CALL GKPRLG (KNIL,GWSOP,GSGOP)

      IF (KERROR.EQ.0) THEN
         KWI1 = KNUM
         KWI2 = NTH
         KSGRQ = 0
         CALL GKSONW(IWKID,KQSGWK,1,KDAT,1,QDAT,QDAT,1,CH)
*        (if no error, should have segment name in KDAT(1) & total in KWI3)
         IF (KERROR .EQ. 0 .OR. KERROR .EQ. 2002) THEN
            IO = KWI3
            ISGNAM = KNIL
            IF((NTH.EQ.0).OR.(IO.EQ.0))THEN
               KERROR = 0
            ELSEIF((NTH.LT.0).OR.(IO.LT.NTH))THEN
               KERROR = 2002
            ELSEIF(KERROR.EQ.0)THEN
               ISGNAM = KDAT(1)
            ENDIF
         ENDIF
         IER = KERROR
      ELSE
         IER = KERROR
      ENDIF

      END
