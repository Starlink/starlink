      PROGRAM TEST_ACCESS

* Test the function PSX_ACCESS

      IMPLICIT NONE
      INCLUDE 'SAE_PAR'
      INCLUDE 'PSX_ERR'
      INTEGER STATUS

* Local Variables:
      CHARACTER *(32) FILE
      LOGICAL ACCESS
      INTEGER REASON

*.
      FILE = 'test_access.f'

      PRINT *, ' '
      PRINT *, ' --  Program TEST_ACCESS, function PSX_ACCESS  -- '
      PRINT *, ' '

*     Look for this source code
      STATUS = SAI__OK
      CALL PSX_ACCESS( FILE, ' ', ACCESS, REASON, STATUS )
      IF ( ACCESS ) THEN
         PRINT *, 'Correctly found source code ',FILE
      ELSE
         PRINT *, 'Error finding source file: ', FILE
         PRINT *, 'Reason = ', REASON
      END IF


      CALL PSX_ACCESS( FILE, 'R', ACCESS, REASON, STATUS )
      IF ( ACCESS ) THEN
         PRINT *, 'Correctly found readable source code ',FILE
      ELSE
         PRINT *, 'Error finding readable source file: ', FILE
         PRINT *, 'Reason = ', REASON
      END IF


      CALL PSX_ACCESS( FILE, 'X', ACCESS, REASON, STATUS )
      IF ( .NOT. ACCESS ) THEN
         PRINT *, 'Correctly did not find executable source code ',FILE
      ELSE
         PRINT *, 'Error! Found executable source file: ', FILE
      END IF

      END
