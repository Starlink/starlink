      PROGRAM TEST_ACCESS

* Test the function PSX_ACCESS

      IMPLICIT NONE
      INCLUDE 'SAE_PAR'
      INCLUDE 'PSX_ERR'
      INTEGER ERRNO
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
      STATUS = SAI__OK
      ERRNO = PSX_ACCESS( FILE, ' ', STATUS )
      IF ( ERRNO .EQ. 0 ) THEN
         PRINT *, 'Correctly found source code ',FILE
      ELSE
         PRINT *, 'Error finding source file: ', FILE
         PRINT *, '-- Got errno ', ERRNO
      END IF


      ERRNO = PSX_ACCESS( FILE, 'r', STATUS )
      IF ( ERRNO .EQ. 0 ) THEN
         PRINT *, 'Correctly found readable source code ',FILE
      ELSE
         PRINT *, 'Error finding readable source file: ', FILE
         PRINT *, '-- Got errno ', ERRNO
      END IF

      END
