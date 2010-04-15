      SUBROUTINE CTRLC_AST
*
*  Handles control-c, saving stacks.
*
      IMPLICIT NONE
      INCLUDE 'SAE_PAR'

      INTEGER STATUS
      LOGICAL SUBCHK
      INTEGER IHX, NDIMPS, NDI2PS, NPARPS, NUMFITPS, NCURPS, MAXFCPS

      COMMON /DAT5/ NDIMPS, NDI2PS, NPARPS, NUMFITPS, NCURPS, MAXFCPS

*  Create a new error reporting environment.
      STATUS = SAI__OK
      CALL ERR_BEGIN( STATUS )

*  Tell the user that the current arrays will be thrown away.
      CALL MSGOUT( 'CTRL-C', 'Contents of current arrays lost', .TRUE.,
     :              STATUS )

*   Save ELF fit coefficient stack
      IF( NUMFITPS .EQ. 0 ) THEN
         CALL MSGOUT( 'CTRL-C', 'ELF coefficient stack is empty',
     :                 .TRUE., STATUS )

      ELSE
         SUBCHK = .TRUE.
         OPEN( UNIT=34, FILE='ELF.ESTK', STATUS='NEW',
     :         FORM='UNFORMATTED',IOSTAT=IHX )

         IF( IHX .NE. 0 ) THEN
            CALL MSGOUT( 'CTRL-C', 'Unable to open ELF.ESTK',
     :                   .TRUE., STATUS )
         ELSE
            CALL MSGOUT( 'CTRL-C', 'Saving fit coefficient stack '//
     :                   '(ELF.ESTK)', .TRUE., STATUS )
            CALL FCSAVE( SUBCHK )
         END IF

         IF( .NOT. SUBCHK ) THEN
            CALL MSGOUT( 'CTRL-C', 'Error saving ELF.ESTK', .TRUE.,
     :                   STATUS )
            SUBCHK = .TRUE.
         END IF

         CLOSE (34)

      END IF

*   Save DIPSO stack (to an NDF)
      CALL SAVE( 'CTRL-C', 'CRASH', STATUS )

*  Flush any errors that have occurred.
      IF( STATUS .NE. SAI__OK ) CALL ERR_FLUSH( STATUS )

*  End the error reporting environment.
      CALL ERR_END( STATUS )

*  Quit the dipso program.
      CALL EXIT

      END
