*  History:
*     22 Nov 1993 (hme):
*        Replace STR$TRIM with CHR_LEN.
*     15 Jan 1994 (rp):
*        Eliminate CHR_ calls, replace with U... where appropriate
*     20 July 2000 (ajc):
*        Change TYPE * to PRINT *
*        Unused ILS
*-----------------------------------------------------------------------

      SUBROUTINE EXTMAP (IFAIL)

C  Routine to get nominated spectrum from the current map - can read it either
C  by position in file (useful in command procedures) or by position on sky.
C  Positions passed to GET_SPECTRUM are in the map coordinate frame - therefore
C  have to convert R.A. and Dec. if map position angle .ne. 0 degrees.

      IMPLICIT  NONE

*     Formal parameters:

      INTEGER   IFAIL

*     Include files:

      INCLUDE   'MAPHD'

*     Local variables:

      INTEGER   IPOS
      INTEGER   ISTAT
      REAL      OFFSET(2)
      REAL      X1OFF, X2OFF
      REAL      CP,    SP
      CHARACTER PROMPT*80

*     Functions:

      INTEGER GEN_ILEN

*  Ok, go...

      IFAIL = 0

      CALL GEN_GETI4 ('Position in file? (<CR> to select by sky'//
     &               ' position)', 0, ' ', IPOS, ISTAT)

      IF (IPOS.EQ.0) THEN

        PROMPT = 'R.A. and Dec. offset? (arcsec)'
        IF (POS_ANGLE .NE. 0.0) THEN
          PROMPT = 'R.A. and Dec. offset? (arcsec) '//
     &             '(<CR> to select by (X,Y)'
        END IF
        CALL GEN_GETR4A (PROMPT(:GEN_ILEN(PROMPT)),
     &    OFFSET, 2, ' ', OFFSET, ISTAT)

        IF (ISTAT.GT.0) THEN
          CALL GEN_GETR4A ('X and Y offsets? (arcsec) ',
     &                      OFFSET, 2, ' ', OFFSET, ISTAT)
          X1OFF = OFFSET(1)
          X2OFF = OFFSET(2)
        ELSE
          CP    = COS (POS_ANGLE/57.29578)
          SP    = SIN (POS_ANGLE/57.29578)
          X1OFF = OFFSET(1)*CP - OFFSET(2)*SP
          X2OFF = OFFSET(1)*SP + OFFSET(2)*CP
          PRINT *,'Offsets in map coordinates: ',X1OFF, X2OFF
        END IF

      END IF

      CALL GET_SPECTRUM (X1OFF, X2OFF, IPOS, IFAIL)

      RETURN
      END
