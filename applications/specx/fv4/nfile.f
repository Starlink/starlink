*  History:
*     06 Dec 1993 (hme):
*        With the expanded include file FILES, we need to INCLUDE
*        DAT_PAR as well.
C-----------------------------------------------------------------------

      INTEGER FUNCTION NFILE (IFILE, IACCES)

      IMPLICIT  NONE

      INTEGER   IFILE
      CHARACTER IACCES*1

      INTEGER   J
      LOGICAL   OK, CHKACC

      INCLUDE   'DAT_PAR'
      INCLUDE   'FILES'

      NFILE = 0

      DO J = 1,MAX_DFILES
        IF (FILELUNS(J).NE.0)   THEN
          OK = .TRUE.
          IF (IACCES.NE.'X')   OK = CHKACC (J, IACCES)
          IF (OK)   THEN
            NFILE = NFILE+1
            IFILE = J
          END IF
        END IF
      END DO

      RETURN
      END


