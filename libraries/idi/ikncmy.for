*-----------------------------------------------------------------------
*+  IKNCMY - Clear Memory

      SUBROUTINE IKNCMY ( DISPID, MEMID, NMEM, BACK, STATUS )

*    Description :
*     This does the Ikon specific work for the IDI routine IIMCMY.
*     The arguments are identical to those in IIMCMY.
*
*    Invocation :
*     CALL IKNCMY( DISPID, MEMID, NMEM, BACK, STATUS )
*
*    Parameters :
*     parameter[(dimensions)]=type(access)
*           <description of parameter>
*
*    Method :
*     Verify the input arguments then send the Ikon commands.
*
*    Deficiencies :
*     Very non-standard Fortran - INTEGER * 2
*
*    Bugs :
*     <description of any "bugs" which have not been fixed>
*
*    Authors :
*     Nick Eaton  ( DUVAD::NE )
*
*    History :
*     December 1988
*     July 1990  Set background colour correctly
*    endhistory
*
*    Type Definitions :
      IMPLICIT NONE

*    Global constants :
      INCLUDE 'IDIINC(IKN_PAR)'
      INCLUDE 'IDIINC(IDI_ERR)'

*    Import :
*     Display identifier
      INTEGER DISPID

*     List of memory identifiers
      INTEGER MEMID( * )

*     Number of memory identifiers
      INTEGER NMEM

*     Background value
      INTEGER BACK

*    Status :
      INTEGER STATUS

*    Global variables :
      INCLUDE 'IDIINC(IKN_COMCH)'
      INCLUDE 'IDIINC(IKN_COMEM)'

*    Local variables :
      INTEGER * 2 WORDS( 7 )

      INTEGER J, NUMWOR
*-

*   Recover the common blocks if the device is not the current one
      IF ( DISPID .NE. CURRID ) THEN
         CALL IKNOUT( STATUS )
         CALL IDSRCO( DISPID, STATUS )
         IF ( STATUS .NE. IDI__OK ) THEN
            STATUS = IDI__NOREC
            GOTO 99
         ENDIF
      ENDIF

*   Check the number of memories is valid
      IF ( ( NMEM .LT. 1 ) .OR. ( NMEM .GT. CNMEM ) ) THEN
         STATUS = IDI__RANGE
         GOTO 99
      ENDIF

*   Check the memory identifiers are valid
      DO J = 1, NMEM
         IF ( ( MEMID( J ) .LT. 0 ) .OR.
     :        ( MEMID( J ) .GE. CNMEM ) ) THEN
            STATUS = IDI__INMID
            GOTO 99
         ENDIF
      ENDDO

*   Verify the background colour
      IF ( BACK .LT. 0 ) THEN
         STATUS = IDI__RANGE
         GOTO 99
      ENDIF

*   Loop through the memories
      DO J = 1, NMEM

*   Select the correct memory
*   Ikon command 124 = '7C'X = Set frame buffer to read
*   Ikon command 125 = '7D'X = Set frame buffer to write
         WORDS( 1 ) = 124
         WORDS( 2 ) = MEMID( J )
         WORDS( 3 ) = 125
         WORDS( 4 ) = MEMID( J )

*   Set the background colour to the one specified
*   Ikon command 64 = '40'X = Set background colour register
         WORDS( 5 ) = 64
         WORDS( 6 ) = BACK

*   Clear the frame buffer to the background colour
*   Ikon command 161 = 'A1'X = Clear all frame buffer
         WORDS( 7 ) = 161

*   Send these commands
         NUMWOR = 7
         CALL IKNOBW( DISPID, NUMWOR, WORDS, STATUS )

      ENDDO

*   Flush the buffer
      CALL IKNOUT( STATUS )

  99  CONTINUE

      END

