      SUBROUTINE PCS_TEST(STATUS)
      INCLUDE 'SAE_PAR'
      INCLUDE 'PAR_ERR'
      REAL X
      INTEGER STATUS

!      dowhile (Status .eq. SAI__OK )
         CALL PAR_DEF0R( 'X', 5.0, STATUS )
         CALL PAR_GET0R( 'X', X, STATUS )
         IF (STATUS .EQ. SAI__OK) THEN
            call msg_blank( status )
            CALL MSG_SETR( 'X', X )
            CALL MSG_OUT( 'D1', 'Successful test prints ^X', STATUS )
         ENDIF

         CALL PAR_CANCL( 'X', STATUS )

!      enddo
      END
