      PROGRAM TEST_GETENV

* Test the subroutine PSX_GETENV

      IMPLICIT NONE
      INCLUDE 'SAE_PAR'
      INTEGER STATUS

* Local VAriables:
      CHARACTER*32 NAME1
      CHARACTER*32 NAME2
      CHARACTER*32 NAME3
      CHARACTER*32 NAME4
      CHARACTER*32 RESULT1
      CHARACTER*32 RESULT2
      CHARACTER*32 RESULT3
      CHARACTER*32 RESULT4

* Initialize STATUS
      STATUS = SAI__OK

* Test PSX_GETENV
      PRINT *,' '
      PRINT *,'--  Program TEST_GETENV, function PSX_GETENV  --'
      PRINT *,' '

      NAME1 = 'USER'
      CALL PSX_GETENV( NAME1, RESULT1, STATUS )
      PRINT '(1x,a,a15,a,a32)','Name ',NAME1,' translates to ',RESULT1
      PRINT *,'Return status = ',STATUS

      NAME2 = 'PATH'
      STATUS = SAI__OK
      CALL PSX_GETENV( NAME2, RESULT2, STATUS )
      PRINT '(1x,a,a15,a,a32)','Name ',NAME2,' translates to ',RESULT2
      PRINT *,'Return status = ',STATUS

      NAME3 = 'STARLINK'
      STATUS = SAI__OK
      CALL PSX_GETENV( NAME3, RESULT3, STATUS )
      PRINT '(1x,a,a15,a,a32)','Name ',NAME3,' translates to ',RESULT3
      PRINT *,'Return status = ',STATUS

      NAME4 = 'nothing'
      STATUS = SAI__OK
      CALL PSX_GETENV( NAME4, RESULT4, STATUS )
      PRINT '(1x,a,a15,a,a32)','Name ',NAME4,' translates to ',RESULT4
      PRINT *,'Return status = ',STATUS

      END
