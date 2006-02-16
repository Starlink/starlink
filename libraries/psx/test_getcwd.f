      PROGRAM TEST_GETCWD

* Test the function PSX_GETCWD

      IMPLICIT NONE
      INCLUDE 'SAE_PAR'
      INCLUDE 'PSX_ERR'
      INTEGER STATUS

* Local Variables:
      CHARACTER *(256) CWD

*.
      PRINT *, ' '
      PRINT *, ' --  Program PSX_GETCWD, function PSX_GETCWD  -- '
      PRINT *, ' '

*     Look for this source code
      STATUS = SAI__OK

      CALL PSX_GETCWD( CWD, STATUS )

      IF (STATUS .EQ. SAI__OK) THEN
         PRINT *, 'Seemed to find CWD okay: ',CWD
      ELSE
         PRINT *, 'Error obtaining CWD' 
      END IF


      END
