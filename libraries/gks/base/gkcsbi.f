C# IL>=a, OL>=0
      SUBROUTINE GKCSBI
*
* (C) COPYRIGHT ICL & SERC  1984
*
*---------------------------------------------------------------------
*
*  RUTHERFORD / ICL GKS SYSTEM
*
*  Type of routine:    Utility
*  Author:             CJW
*
      INCLUDE '../include/check.inc'
*
*  PURPOSE OF THE ROUTINE
*  ----------------------
*     CSS : Begin item (part of read interface)
*
*  MAINTENANCE LOG
*  ---------------
*     17/11/83  CJW   Original version stabilized
*     21/01/87  ARG   IS conversion. Error number change
*
*  COMMON BLOCK USAGE
*  ------------------
*
      INCLUDE '../include/gkdt.par'
      INCLUDE '../include/gkmc.par'
      INCLUDE '../include/gkwke.par'
      INCLUDE '../include/gkcss.par'
      INCLUDE '../include/gkerr.cmn'
      INCLUDE '../include/gkcss.cmn'
*
*  LOCALS
*  ------
*     ICAI   Number of integers in CCA
*     ICAR   Number of reals in CCA
*
      INTEGER ICAI, ICAR
*
*  ERRORS
*  ------
*     -2014  Bug: segment has not been selected on CSS
*
*  COMMENTS
*  --------
*
*---------------------------------------------------------------------

      IF (KCSRDN.EQ.KNIL) THEN
         CALL GKBUG (-2014,'GKCSBI')
         GO TO 9999
      END IF
      IF (KCSRDX.EQ.KNIL) THEN
         CALL GKDRGE(KCSDIR, KCSRDN, KCSIWK, KCSRWK,
     :                                  KCSWRK, QCSWRK)
         IF (KERROR .NE. 0) GO TO 9999
         CALL GKCSGT(KCSWRK(KCSRC),KCSRDX)
         KCSRDO = 1
         KCSENT = KNIL
      END IF
      IF (KCSENT.EQ.KNIL) THEN
         IF ((KCSRDX.EQ.KCSOPX) .AND. (KCSRDO.EQ.KCSOPO)) THEN
*              End of open segment found - generate an "END"
            KCSENT = KENSG
            KCSI = 0
            KCSR = 0
            KCSC = 0
            ICAI = 0
            ICAR = 0
         ELSE
            CALL GKCSGI(6, KCSBUF)
            KCSENT = KCSBUF(1)
            KCSI = KCSBUF(2)
            KCSR = KCSBUF(3)
            KCSC = KCSBUF(4)
            ICAI = KCSBUF(5)
            ICAR = KCSBUF(6)
            CALL GKCSGI(ICAI, KCSWRK)
            CALL GKCSGR(ICAR, QCSWRK)
         END IF
         KCSIA = 0
         KCSRA = 0
         KCSCA = 0
      END IF

 9999 CONTINUE

      END
