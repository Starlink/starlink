C# IL>=a, OL>=0
      SUBROUTINE GKHPQS ( ITYPE, INDEX, ISIZE )
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
*      Inquire amount of heap space allocated.
*
*  MAINTENANCE LOG
*  ---------------
*     21/07/83  CJW   Original version stabilized
*     27/06/83  CJW   Implement revised error handling procedure
*     05/03/84  CJW   More explicit bugs
*     21/01/87  ARG   IS conversion. Error numbers changed.
*     13/02/87  PLP   Character heap now equivalenced
*                     to Integer/Real. Altered code and
*                     comments accordingly.
*     09/06/87  RMK   Merged ICL and RAL versions of this routine.
*
*  ARGUMENTS
*  ---------
*     INP   ITYPE  Heap type
*     INP   INDEX  Heap Index
*     OUT   ISIZE  Size of allocation
*
      INTEGER  ITYPE, INDEX, ISIZE
*
*  COMMON BLOCK USAGE
*  ------------------
*     Read     /HP/    Assorted heap variables
*
      INCLUDE '../include/gkdt.par'
      INCLUDE '../include/gkhp.par'
      INCLUDE '../include/gkmc.par'
      INCLUDE '../include/gkerr.cmn'
      INCLUDE '../include/gkhp.cmn'
*
*  ERRORS
*  ------
*     -2006  Value of internal enumerated type is invalid - ITYPE
*     -2016  Invalid heap pointer
*     -2019  Internal inconsistency in heap management
*
*  COMMENTS
*  --------
*     The following are all considered to be bugs -
*
*     -2016  Heap index bad:
*                         INDEX < 0
*                         INDEX > KHPXSI
*                         KHPXI(INDEX) = KNIL
*
*     -2019  Heap not consistent:
*                         KHPXPI(KHPLXI(INDEX) <> INDEX
*
*---------------------------------------------------------------------


      IF ((ITYPE.EQ.KINTGS) .OR. (ITYPE.EQ.KREALS)) THEN

*        Check index is valid

         IF ((INDEX .LT. 0) .OR. (INDEX .GT. KHPXSI) .OR.
     :          (KHPXI(INDEX) .EQ. KNIL)) THEN
            CALL GKBUG (-2016,'GKHPQS')
         ELSE IF (KHPXPI(KHPLXI(INDEX)) .NE. INDEX) THEN
            CALL GKBUG (-2019,'GKHPQS')
         ELSE

            ISIZE = KHPESI(KHPLXI(INDEX))

         END IF

      ELSE IF (ITYPE .EQ. KCHARS) THEN

*        Treat as Integer/Real heap: check index is valid

         IF ((INDEX .LT. 0) .OR. (INDEX .GT. KHPXSI) .OR.
     :          (KHPXI(INDEX) .EQ. KNIL)) THEN
            CALL GKBUG (-2016,'GKHPQS')
         ELSE IF (KHPXPI(KHPLXI(INDEX)) .NE. INDEX) THEN
            CALL GKBUG (-2019,'GKHPQS')
         ELSE

            ISIZE = KHPESI(KHPLXI(INDEX))*KNIBYT

         END IF

      ELSE

         CALL GKBUG (-2006,'GKHPQS')

      END IF

      END
