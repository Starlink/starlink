*  History:
*      1 Aug 2000 (ajc):
*        Change TYPE * to PRINT *
C-----------------------------------------------------------------------

      SUBROUTINE SPECX_GSD_LOCATE (DXY, INDEX_LIST, NO_SCANS, IERR)

C   Routine to read GSD scan file and extract a single spectrum,
C   putting it onto the SPECX data stack for normal use.

      IMPLICIT  NONE

C   Formal parameters

      REAL*4    DXY(2)
      INTEGER*4 INDEX_LIST(*)
      INTEGER*4 NO_SCANS
      INTEGER*4 IERR

      INTEGER*4 ADAM__OK
      PARAMETER (ADAM__OK=0)

      LOGICAL   POINT_FOUND
      INTEGER*4 I
      INTEGER*4 J
      REAL*4    EPS

      INCLUDE  'SPECX_PARS'
      INCLUDE  'STAKPAR'
      INCLUDE  'STACKCOMM'
      INCLUDE  'GSD_VAR.INC'

      IERR = 0

C  Which point in the map do we want? (This is the tricky bit)
C  Hunt around in the pointing history table until we
C  find a position that satisfies a nearness criterion.
C  (Given that we may actually map in fractional raster units, this
C   is pretty arbitrary - we don't know what the basic granularity of the
C   grid IS: Use absolute nearness of 1.E-3 for safety)

      EPS = 1.e-3

CD    Print *,'Requested (X,Y) offsets: ', DXY(1), DXY(2)

      IF (IXNP*IYNP.EQ.1) THEN
        INDEX_LIST(1) = 1
        NO_SCANS      = 1

      ELSE

        NO_SCANS    = 0
        POINT_FOUND = .FALSE.

        DO J = 1, NGSDSPEC

CD        Print *,'J, PHIST(J) X&Y: ', J, PHIST(1,J), PHIST(2,J)

          IF (       ABS (PHIST(1,J)-DXY(1)) .LE. EPS
     &         .AND. ABS (PHIST(2,J)-DXY(2)) .LE. EPS) THEN
            POINT_FOUND          = .TRUE.
            NO_SCANS             = NO_SCANS + 1
            INDEX_LIST(NO_SCANS) = J
CD          PRINT *,'Point found - index =',J
          END IF
        END DO

        IF (.NOT.POINT_FOUND) THEN
          DO I = 1, LSTK-LHEAD
            DATA(I) = 0.0
          END DO
          IERR = 56
          GO TO 99
        END IF
      END IF

C  Standard return

   99 CONTINUE

      RETURN
      END

C-----------------------------------------------------------------
