*-----------------------------------------------------------------------
*+  IKNSMV - Set Memory Visibility

      SUBROUTINE IKNSMV ( DISPID, MEMID, NMEM, LVIS, STATUS )

*    Description :
*     This does the Ikon specific work for the IDI routine IIMBLM.
*     The arguments are identical to those in IIMBLM.
*
*    Invocation :
*     CALL IKNSMV( DISPID, MEMID, NMEM, LVIS, STATUS )
*
*    Method :
*     Verify the input arguments.
*     Set the visibility of all the memories according to the flag.
*     Store the order of memories passed to this routine as a priority
*     list of the memories. If the memories are being displayed then
*     increase their priority; first in list has highest priority.
*     If the memories are being undisplayed then decrease their
*     priority, below all other memories.
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
*     July 1990  Correct setting of memory visibility.
*     October 1992  Allow for undefined memories.
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

*     Visibility
      LOGICAL LVIS

*    Status :
      INTEGER STATUS

*    Global variables :
      INCLUDE 'IDIINC(IKN_COMCH)'
      INCLUDE 'IDIINC(IKN_COMEM)'
      INCLUDE 'IDIINC(IKN_COMID)'

*    Local variables :
      INTEGER * 2 WORDS( 4 )
      INTEGER J, MAXPR, MINPR, NUMWOR
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

*   Set the memory visibilities
      DO J = 1, NMEM
         IF ( LVIS ) THEN
            CMEMVI( MEMID( J ) ) = 1
         ELSE
            CMEMVI( MEMID( J ) ) = 0
         ENDIF
      ENDDO

*   Display the memories according to their visibility.
*   If both memories are invisible have to use unblank
*   Ikon command 92 = '5C'X = Set frame grab control latch
*   Ikon command 211 = 'D3'X = Blank currently selected screen
      IF ( ( CMEMVI( 0 ) .LE. 0 ) .AND. ( CMEMVI( 1 ) .LE. 0 ) ) THEN
         WORDS( 1 ) = 92
         WORDS( 2 ) = 1
         WORDS( 3 ) = 211

*   Otherwise use the frame grab control latch
*   Ikon command 212 = 'D4'X = Unblank currently selected screen
*   Ikon command 92 = '5C'X = Set frame grab control latch
      ELSE
         WORDS( 1 ) = 212
         WORDS( 2 ) = 92
         IF ( ( CMEMVI( 0 ) .EQ. 1 ) .AND. ( CMEMVI( 1 ) .EQ. 1 ) ) THEN
            WORDS( 3 ) = 1
         ELSEIF( CMEMVI( 0 ) .EQ. 1 ) THEN
            WORDS( 3 ) = 3
         ELSEIF( CMEMVI( 1 ) .EQ. 1 ) THEN
            WORDS( 3 ) = 0
         ENDIF
      ENDIF
      NUMWOR = 3

*   Send these commands
      CALL IKNOBW( DISPID, NUMWOR, WORDS, STATUS )
      CALL IKNOUT( STATUS )

*   Change the priority list for the memories.
*   Find the highest and lowest vaules of priority
      MAXPR = CMEMPR( 0 )
      MINPR = CMEMPR( 0 )
      DO J = 1, CNMEM - 1
         IF ( CMEMPR( J ) .GT. MAXPR ) THEN
            MAXPR = CMEMPR( J )
         ENDIF
         IF ( CMEMPR( J ) .LT. MINPR ) THEN
            MINPR = CMEMPR( J )
         ENDIF
      ENDDO

*   Assign new priorities according to the given order
*   If the memories are visible then increase their priority
      IF ( LVIS ) THEN
         DO J = 1, NMEM
            CMEMPR( MEMID( J ) ) = MAXPR + NMEM + 1 - J
         ENDDO

*   If the memories are invisible then decrease their priority
      ELSE
         DO J = 1, NMEM
            CMEMPR( MEMID( J ) ) = MINPR - J
         ENDDO
      ENDIF

  99  CONTINUE

      END

