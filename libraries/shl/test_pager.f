      PROGRAM TEST_PAGER
*  Simple test of pager interface
      IMPLICIT NONE

      INCLUDE 'SAE_PAR'
      INTEGER STATUS
      INTEGER I

      STATUS = SAI__OK

      CALL SHL_PAGRST( STATUS )


      DO I = 1, 50
         CALL SHL_PAGTXT( 'This is a test of the pager', STATUS )
      END DO

      END
