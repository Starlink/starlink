      PROGRAM TEST_TTYNAME

* Test the functions PSX_ISATTY and PSX_TTYNAME.

      IMPLICIT NONE
      INCLUDE 'SAE_PAR'
      INTEGER STATUS

* Local variables:
      INTEGER I                  ! Loop counter
      LOGICAL YES		 ! Is this a terminal?
      CHARACTER * ( 32 ) TNAME   ! Terminal name

* Initialize STATUS.
      STATUS = SAI__OK

* Do the test of PSX_ISATTY:
      PRINT *,' '
      PRINT *,'--  Program TEST_TERM. Testing PSX_ISATTY  --'
      PRINT *,' '
      DO I = -5,9
         CALL PSX_ISATTY( I, YES, STATUS )
         PRINT *,'File descriptor = ',I,', PSX_ISATTY returns ',YES
      END DO

* Do the test of PSX_TTYNAME:
      PRINT *,' '
      PRINT *,'--  Program TEST_TERM. Testing PSX_TTYNAME  --'
      PRINT *,' '
      DO I = -5,9
         CALL PSX_TTYNAME( I ,TNAME, STATUS )
         PRINT *,'File descriptor = ',I,', terminal name = ',TNAME
      END DO

      END
