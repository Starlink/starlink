      PROGRAM TEST_RAND

* Test subroutines PSX_RAND and PSX_SRAND.

      IMPLICIT NONE
      INCLUDE 'SAE_PAR'
      INTEGER STATUS

* Local Variables:
      INTEGER I, J, INUM, MAXNUM
      REAL FNUM

* Initialize STATUS.
      STATUS = SAI__OK

* Test PSX_RAND
      PRINT *,' '
      PRINT *,'--  Program PSX_RAND, function PSX_RAND  --'
      PRINT *,' '

      DO I = 1,10
         CALL PSX_RAND( INUM, MAXNUM, FNUM, STATUS )
         PRINT *,'INUM = ',INUM,'  MAXNUM = ',MAXNUM,'  FNUM = ',FNUM
      END DO

* Test PSX_SRAND
      PRINT *,' '
      PRINT *,'--  Program PSX_RAND, function PSX_SRAND  --'
      PRINT *,' '

      DO I = 1,5
         DO J = 1,3
            CALL PSX_SRAND( I, STATUS )
            CALL PSX_RAND( INUM, MAXNUM, FNUM, STATUS )
            PRINT *,'Seed = ',I,' Random number = ',INUM
         END DO
      END DO

      END
