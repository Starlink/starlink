C# IL>=b, OL>=0
      SUBROUTINE GRQST (IWKID,ISTDNR,ISTAT,LSTR,STR)
*
* (C) COPYRIGHT ICL & SERC  1984
*

*---------------------------------------------------------------------
*
*  RUTHERFORD / ICL GKS SYSTEM
*
*  GKS Function name:  REQUEST STRING
*  Author:             AS
*
      INCLUDE '../include/check.inc'
*
*  PURPOSE OF THE ROUTINE
*  ----------------------
*     Performs a REQUEST on the specified STRING device.
*
*  MAINTENANCE LOG
*  ---------------
*      4/05/83  AS   Original version stabilized
*      7/12/83  AS   Check for device number < 1
*     10/01/84  NB   Wasn't deallocating Stack if W/S returned ERROR
*     30/05/90  KEVP Set status to KNIL for input device error (S271)
*
*  ARGUMENTS
*  ---------
*     INP IWKID  workstation identifier
*     INP ISTDNR string device number
*     OUT ISTAT  status
*     OUT LSTR   length of string
*     OUT STR    string
*
      INTEGER IWKID, ISTDNR, ISTAT, LSTR
      CHARACTER*(*) STR
*
*  COMMON BLOCK USAGE
*  ------------------
*
      INCLUDE '../include/gks.par'
      INCLUDE '../include/gkse.par'
      INCLUDE '../include/gkdt.par'
      INCLUDE '../include/gkwca.cmn'
      INCLUDE '../include/gkwke.par'
      INCLUDE '../include/gkstk.cmn'
      INCLUDE '../include/gkerr.cmn'
*
*  LOCALS
*  ------
*
      INTEGER LENGTH, IOFF
*
*  STACK USAGE
*  -----------
*     LENGTH    INTEGER  space for character/integer array
*
*  ERRORS
*  ------
*     140  Input device number is invalid
*
*---------------------------------------------------------------------


      CALL GKPRLG (ERQST,GWSOP,GSGOP)

      IF (KERROR.EQ.0) THEN
        ISTAT = KNIL
        IF (ISTDNR.GE.1) THEN

          KWI1 = ISTDNR
          LENGTH = LEN(STR)

* Get space for character/integer array and call workstation

          CALL GKSTAL(1,LENGTH,IOFF)
          IF (KERROR.EQ.0) THEN
            CALL GKSONW(IWKID,KRQST,LENGTH,KSTACK(IOFF),1,QDAT,QDAT,
     :                     1,CH)
            IF (KERROR.EQ.0) THEN
              ISTAT = KWI1
              LSTR = KNIR
              CALL GKATON(LSTR,KSTACK(IOFF),STR)
              IF (KERROR.NE.0) CALL GKERR(KERROR)
            ELSE
              CALL GKERR(KERROR)
            ENDIF
            CALL GKSTDA(0,IOFF)
          ELSE
            CALL GKERR(KERROR)
          ENDIF
        ELSE
          CALL GKERR(140)
        ENDIF
      ELSE
        CALL GKERR(KERROR)
      ENDIF


      END
