      SUBROUTINE KAPLIBS_TEST( STATUS )
      IMPLICIT NONE
      INCLUDE 'SAE_PAR'
      INTEGER STATUS
      LOGICAL OK
      CHARACTER LIST*80
      
      IF( STATUS .NE. SAI__OK ) RETURN
      OK = .TRUE.
      
      CALL IRA_IPROJ( LIST, STATUS )
      IF( INDEX( LIST, 'AITOFF' ) .EQ. 0 ) THEN
         OK = .FALSE.
         GO TO 999
      END IF      
      
*  Add other tests here...


 999  CONTINUE

      IF( STATUS .NE. SAI__OK ) THEN
         CALL ERR_FLUSH( STATUS )
         OK = .FALSE.
      END IF

      CALL MSG_BLANK( STATUS )
      IF( .NOT. OK ) THEN
         CALL MSG_OUT( ' ', 'KAPLIBS test failed', STATUS )
      ELSE
         CALL MSG_OUT( ' ', 'KAPLIBS test passed', STATUS )
      END IF 
      CALL MSG_BLANK( STATUS )

      END
