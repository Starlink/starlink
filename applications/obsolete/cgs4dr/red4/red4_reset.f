*+  RED4_RESET - Reset the RED4 common block pointers and virtual memory.
      SUBROUTINE RED4_RESET( STATUS )
*    Description :
*     This routine resets the common block area where the current BIAS
*     DARK, FLAT and STANDARD are stored, and frees any virtual memory
*     associated with them. The routine should be invoked whenever a
*     change is made to the current bad pixel mask.
*    Invocation :
*     CALL RED4_RESET( STATUS )
*    Parameters :
*    Method :
*    Deficiencies :
*    Bugs :
*    Authors :
*     Steven Beard (REVAD::SMB)
*     Phil Daly (JACH::PND)
*    History :
*     24-Feb-1991: Original version, written to solve a difficulty
*                  in getting the system to accept a new bad pixel
*                  mask.                                           (SMB)
*     23-Feb-1993: Conform to error strategy                       (PND)
*     24-Nov-1993: Replace VMS-specific calls with PSX calls       (PND)
*      7-Jan-1993: Simplify logic                                  (PND)
*    endhistory
*    Global constants :
      INCLUDE 'SAE_PAR'
*    Status :
      INTEGER STATUS
*    Global variables :
      INCLUDE 'RED4_COMMON.INC'   ! RED4 common block.
*    External references :
*    Local Constants :
*    Local variables :
*    Internal References :
*    Local data :
*-

*   Check for error on entry
      IF ( STATUS .NE. SAI__OK ) RETURN

*   Free the virtual memory area used for the BIAS arrays.
      IF ( (CURRENT_BIAS .NE. ' ') .AND. (BIAS_NELM .GT. 0) ) THEN

         IF ( VERBOSE ) CALL MSG_OUT( ' ',
     :     'Releasing memory used to map BIAS data, '/
     :     /'variance and quality arrays', STATUS )

         CALL PSX_FREE( BIAS_DATA, STATUS )
         CALL PSX_FREE( BIAS_VAR, STATUS )
         CALL PSX_FREE( BIAS_QUAL, STATUS )

         IF ( STATUS .NE. SAI__OK ) THEN

            STATUS = SAI__ERROR
            CALL ERR_REP( ' ', 'RED4_RESET: '/
     :        /'Error freeing virtual memory '/
     :        /'associated with current BIAS arrays', STATUS )
         ELSE
            IF ( VERBOSE ) THEN

               CALL MSG_SETC( 'CURRENT_BIAS', CURRENT_BIAS )
               CALL MSG_OUT( ' ', 'Current BIAS, ^CURRENT_BIAS, '/
     :           /'removed from memory OK', STATUS )
            ENDIF

            CURRENT_BIAS = ' '
            BIAS_DATA = 0
            BIAS_VAR  = 0
            BIAS_QUAL = 0
            BIAS_NELM = 0
         ENDIF
      ELSE

         IF ( VERBOSE )
     :      CALL MSG_OUT( ' ',
     :         'No current BIAS was mapped', STATUS )
      ENDIF

*   Free the virtual memory area used for the DARK arrays.
      IF ( (CURRENT_DARK .NE. ' ') .AND. (DARK_NELM .GT. 0) ) THEN

         IF ( VERBOSE ) CALL MSG_OUT( ' ',
     :     'Releasing memory used to map DARK data, '/
     :     /'variance and quality arrays', STATUS )

         CALL PSX_FREE( DARK_DATA, STATUS )
         CALL PSX_FREE( DARK_VAR, STATUS )
         CALL PSX_FREE( DARK_QUAL, STATUS )

         IF ( STATUS .NE. SAI__OK ) THEN

            STATUS = SAI__ERROR
            CALL ERR_REP( ' ', 'RED4_RESET: '/
     :        /'Error freeing virtual memory '/
     :        /'associated with current DARK arrays', STATUS )
         ELSE
            IF ( VERBOSE ) THEN

               CALL MSG_SETC( 'CURRENT_DARK', CURRENT_DARK )
               CALL MSG_OUT( ' ', 'Current DARK, ^CURRENT_DARK, '/
     :           /'removed from memory OK', STATUS )
            ENDIF

            CURRENT_DARK = ' '
            DARK_DATA = 0
            DARK_VAR  = 0
            DARK_QUAL = 0
            DARK_NELM = 0
         ENDIF
      ELSE

         IF ( VERBOSE )
     :      CALL MSG_OUT( ' ',
     :         'No current DARK was mapped', STATUS )
      ENDIF

*   Free the virtual memory area used for the FLAT arrays.
      IF ( (CURRENT_FLAT .NE. ' ') .AND. (FLAT_NELM .GT. 0) ) THEN

         IF ( VERBOSE ) CALL MSG_OUT( ' ',
     :     'Releasing memory used to map FLAT data, '/
     :     /'variance and quality arrays', STATUS )

         CALL PSX_FREE( FLAT_DATA, STATUS )
         CALL PSX_FREE( FLAT_VAR, STATUS )
         CALL PSX_FREE( FLAT_QUAL, STATUS )

         IF ( STATUS .NE. SAI__OK ) THEN

            STATUS = SAI__ERROR
            CALL ERR_REP( ' ', 'RED4_RESET: '/
     :        /'Error freeing virtual memory '/
     :        /'associated with current FLAT arrays', STATUS )
         ELSE
            IF ( VERBOSE ) THEN

               CALL MSG_SETC( 'CURRENT_FLAT', CURRENT_FLAT )
               CALL MSG_OUT( ' ', 'Current FLAT, ^CURRENT_FLAT, '/
     :           /'removed from memory OK', STATUS )
            ENDIF

            CURRENT_FLAT = ' '
            FLAT_DATA = 0
            FLAT_VAR  = 0
            FLAT_QUAL = 0
            FLAT_NELM = 0
         ENDIF
      ELSE

         IF ( VERBOSE )
     :      CALL MSG_OUT( ' ',
     :         'No current FLAT was mapped', STATUS )
      ENDIF

*   Free the virtual memory area used for the STANDARD arrays.
      IF ( (CURRENT_STANDARD .NE. ' ') .AND.
     :     (STANDARD_NELM .GT. 0) ) THEN

         IF ( VERBOSE ) CALL MSG_OUT( ' ',
     :     'Releasing memory used to map STANDARD data, '/
     :     /'variance and quality arrays', STATUS )

         CALL PSX_FREE( STANDARD_DATA, STATUS )
         CALL PSX_FREE( STANDARD_VAR, STATUS )
         CALL PSX_FREE( STANDARD_QUAL, STATUS )

         IF ( STATUS .NE. SAI__OK ) THEN

            STATUS = SAI__ERROR
            CALL ERR_REP( ' ', 'RED4_RESET: '/
     :        /'Error freeing virtual memory '/
     :        /'associated with current STANDARD arrays', STATUS )
         ELSE
            IF ( VERBOSE ) THEN

               CALL MSG_SETC( 'CURRENT_STANDARD', CURRENT_STANDARD )
               CALL MSG_OUT( ' ', 'Current STANDARD, '/
     :           /'^CURRENT_STANDARD, '/
     :           /'removed from memory OK', STATUS )
            ENDIF

            CURRENT_STANDARD = ' '
            STANDARD_DATA = 0
            STANDARD_VAR  = 0
            STANDARD_QUAL = 0
            STANDARD_NELM = 0
         ENDIF
      ELSE

         IF ( VERBOSE )
     :      CALL MSG_OUT( ' ',
     :        'No current STANDARD was mapped', STATUS )
      ENDIF
      END
