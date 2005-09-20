      PROGRAM TEST_CUSERID

* Test the subroutine PSX_CUSERID

      IMPLICIT NONE
      INCLUDE 'SAE_PAR'
      INTEGER STATUS

* Local Variables:
      CHARACTER*32 NAME

* Initialize STATUS.
      STATUS = SAI__OK

* Test PSX_CUSERID
      PRINT *,' '
      PRINT *,'--  Program TEST_CUSERID, function PSX_CUSERID  --'
      PRINT *,' '

      CALL PSX_CUSERID( NAME, STATUS )
      PRINT *,'Username is ',NAME
      CALL PSX_CUSERID( NAME, STATUS )
      PRINT *,'Username is still ',NAME

      END
