      PROGRAM TEST_GETUID

* Test subroutines PSX_GETUID, PSX_GETGID, PSX_GETEUID and PSX_GETEGID.

      IMPLICIT NONE
      INCLUDE 'SAE_PAR'
      INTEGER STATUS

* Local Variables:
      INTEGER GID, UID

* Initialize STATUS
      STATUS = SAI__OK

* Test PSX_GETUID
      PRINT *,' '
      PRINT *,'--  Program TEST_GETUID, function PSX_GETUID  --'
      PRINT *,' '
      CALL PSX_GETUID( UID, STATUS )
      PRINT *,'UID = ',UID

* Test PSX_GETEUID
      PRINT *,' '
      PRINT *,'--  Program TEST_GETUID, function PSX_GETEUID  --'
      PRINT *,' '
      CALL PSX_GETEUID( UID, STATUS )
      PRINT *,'EUID = ',UID

* Test PSX_GETGID
      PRINT *,' '
      PRINT *,'--  Program TEST_GETUID, function PSX_GETGID  --'
      PRINT *,' '
      CALL PSX_GETGID( GID, STATUS )
      PRINT *,'GID = ',GID

* Test PSX_GETEGID
      PRINT *,' '
      PRINT *,'--  Program TEST_GETUID, function PSX_GETEGID  --'
      PRINT *,' '
      CALL PSX_GETEGID( GID, STATUS )
      PRINT *,'EGID = ',GID

      END
