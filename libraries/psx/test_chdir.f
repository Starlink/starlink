      PROGRAM TEST_CHDIR

* Test the function PSX_CHDIR

      IMPLICIT NONE
      INCLUDE 'SAE_PAR'
      INCLUDE 'PSX_ERR'
      INTEGER STATUS

* Local Variables:
      CHARACTER *(256) CWD

*.
      PRINT *, ' '
      PRINT *, ' --  Program PSX_CHDIR, function PSX_CHDIR  -- '
      PRINT *, ' '

*     Look for this source code
      STATUS = SAI__OK

      CALL PSX_GETCWD( CWD, STATUS )

      IF (STATUS .EQ. SAI__OK) THEN
         PRINT *, 'Seemed to find CWD okay'
         CALL PSX_CHDIR( CWD, STATUS )
         IF (STATUS .EQ. SAI__OK) THEN
            PRINT *, 'CHDIR to CWD worked okay'
         ELSE
            PRINT *, 'Error changing to CWD'
         END IF
      ELSE
         PRINT *, 'Error obtaining CWD' 
      END IF


      END
