      PROGRAM TEST_REMOVE

* Test the function PSX_REMOVE

      IMPLICIT NONE
      INCLUDE 'SAE_PAR'
      INCLUDE 'PSX_ERR'
      INTEGER STATUS

* Local Variables:
      CHARACTER *(12) TEMPFILE

*.

      PRINT *, ' '
      PRINT *, ' --  Program PSX_REMOVE, function PSX_REMOVE  -- '
      PRINT *, ' '


* Initialize STATUS
      STATUS = SAI__OK
      CALL EMS_BEGIN( STATUS )

* Test PSX_REMOVE on a file that does not exist
      CALL PSX_REMOVE( 'xxx--xxx--xxxx-ttt', STATUS )

      IF (STATUS .NE. SAI__OK) THEN
         PRINT *,'Attempt to remove non-existant file failed correctly'
         CALL EMS_ANNUL( STATUS )
      ELSE
         PRINT *,'Attempt to remove non-existant file succeeded!!'
      ENDIF

* Create a new file
      TEMPFILE = 'xxx-xxx.dat'
      OPEN ( UNIT=20, FILE=TEMPFILE )
      CALL PSX_REMOVE( TEMPFILE, STATUS )
      IF (STATUS .EQ. SAI__OK) THEN
         PRINT *, 'Successfully removed temp file'
      END IF


      CALL EMS_END( STATUS )

      END
