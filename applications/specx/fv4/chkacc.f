*  History:
*     06 Dec 1993 (hme):
*        With the expanded include file FILES, we need to INCLUDE
*        DAT_PAR as well.
C-----------------------------------------------------------------------

      LOGICAL FUNCTION CHKACC (IFILE, IACCES)

      INCLUDE   'DAT_PAR'
      INCLUDE   'FILES'
      CHARACTER IACCES*1

      CHKACC=.FALSE.
      IF (ACCESS(IFILE)(1:1).EQ.IACCES .OR.
     &    ACCESS(IFILE)(2:2).EQ.IACCES)     CHKACC = .TRUE.

      RETURN
      END


