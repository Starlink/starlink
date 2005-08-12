      PROGRAM TEST_REMOVE

* Test the function PSX_REMOVE

      IMPLICIT NONE
      INCLUDE 'SAE_PAR'
      INCLUDE 'PSX_ERR'
      INTEGER STATUS

      EXTERNAL PSX_ACCESS
      INTEGER PSX_ACCESS

* Local Variables:
      CHARACTER *(32) FILE

*.
      FILE = 'test_access.f'

      PRINT *, ' '
      PRINT *, ' --  Program PSX_ACCESS, function PSX_ACCESS  -- '
      PRINT *, ' '

*     Look for this source code
      STATUS = ACCESS( FILE, ' ' )
      IF ( STATUS .EQ. 0 ) THEN
         PRINT *, 'Correctly found source code ',FILE
      ELSE
         PRINT *, 'Error finding source file: ', FILE
         PRINT *, '-- Got status ', STATUS
      END IF


      STATUS = ACCESS( FILE, 'r' )
      IF ( STATUS .EQ. 0 ) THEN
         PRINT *, 'Correctly found readable source code ',FILE
      ELSE
         PRINT *, 'Error finding readable source file: ', FILE
         PRINT *, '-- Got status ', STATUS
      END IF

      END
