      PROGRAM TEST_UNAME

* Test the function PSX_UNAME

      IMPLICIT NONE
      INCLUDE 'SAE_PAR'
      INTEGER STATUS

* Local Variables:
      CHARACTER*32 SYSNAME
      CHARACTER*32 NODENAME
      CHARACTER*32 RELEASE
      CHARACTER*32 VERSION
      CHARACTER*32 MACHINE

* Initialize STATUS
      STATUS = SAI__OK

* Test PSX_UNAME
      PRINT *,' '
      PRINT *,'--  Program PSX_UNAME, function PSX_UNAME  --'
      PRINT *,' '

      CALL PSX_UNAME( SYSNAME, NODENAME, RELEASE, VERSION, MACHINE,
     :   STATUS )
      PRINT *,'Sysname = ',SYSNAME
      PRINT *,'Nodename= ',NODENAME
      PRINT *,'Release = ',RELEASE
      PRINT *,'Version = ',VERSION
      PRINT *,'Machine = ',MACHINE
      PRINT *,'Return status = ',STATUS

      END
