      PROGRAM TEST_GETPID

* Test the subroutines PSX_PID and PSX_PPID.

      IMPLICIT NONE
      INCLUDE 'SAE_PAR'
      INTEGER STATUS

* Local Variables:
      INTEGER PID, PPID

*Initialize STATUS
      STATUS = SAI__OK

* Test PSX_GETPID
      PRINT *,' '
      PRINT *,'--  Program PSX_GETPID, function PSX_GETPID  --'
      PRINT *,' '

      CALL PSX_GETPID( PID, STATUS )
      PRINT *,'Process ID =        ',PID

* Test PSX_GETPID
      PRINT *,' '
      PRINT *,'--  Program PSX_GETPID, function PSX_GETPPID  --'
      PRINT *,' '

      CALL PSX_GETPPID( PPID, STATUS )
      PRINT *,'Parent process ID = ',PPID

      END
