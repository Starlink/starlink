*-----------------------------------------------------------------------
*+  IKNWSC - Write Ikon Memory Scroll

      SUBROUTINE IKNWSC ( DISPID, MEMID, NMEM, XOFF, YOFF, STATUS )

*    Description :
*     This does the Ikon specific work for the IDI routine IIZWSC.
*     The arguments are identical to those in IIZWSC.
*
*    Invocation :
*     CALL IKNWSC( DISPID, MEMID, NMEM, XOFF, YOFF, STATUS )
*
*    Parameters :
*     parameter[(dimensions)]=type(access)
*           <description of parameter>
*
*    Method :
*     Verify the input arguments.
*     The scroll factors for the Ikon are stored in a device specific
*     way so have to translate them from the device independent factors.
*     The Ikon can only scroll in steps of 16 in the x-direction so
*     calculate the x scroll to the nearest 16.
*     Loop through all the given memories performing the scroll.
*     In reality the Ikon memories cannot be scrolled independently.
*
*    Deficiencies :
*     Very non-standard-Fortran - INTEGER * 2
*
*    Bugs :
*     <description of any "bugs" which have not been fixed>
*
*    Authors :
*     Nick Eaton  ( DUVAD::NE )
*
*    History :
*     December 1988
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

*     X offset
      INTEGER XOFF

*     Y offset
      INTEGER YOFF

*    Status :
      INTEGER STATUS

*    Global variables :
      INCLUDE 'IDIINC(IKN_COMCH)'
      INCLUDE 'IDIINC(IKN_COMEM)'
      INCLUDE 'IDIINC(IKN_COMPOS)'

*    Local variables :
      INTEGER * 2 WORDS( 5 )

      INTEGER J, NUMWOR, XSCROL, YSCROL
*-

*   Recover the characteristics if the device is not the current one
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

*   The Ikon only scrolls in steps of 16 in the x direction
      XSCROL = ( ( XOFF + SIGN( 8, XOFF ) ) / 16 ) * 16
      YSCROL = YOFF

*   Store position in common block
*   Save this for all memories as they all scroll together
      DO J = 0, CNMEM - 1
         CMEMX( J ) = XSCROL
         CMEMY( J ) = YSCROL
         CSCROX( J ) = XSCROL
         CSCROY( J ) = YSCROL
      ENDDO

*   Loop through the memories
      DO J = 1, NMEM

*   Select the frame buffer to display
*   Ikon command 123 = '7B'X = Set frame buffer to write
         WORDS( 1 ) = 123
         WORDS( 2 ) = MEMID( J )

*   Send scroll command to Ikon
*   Ikon command 219 = 'DB'X = Pan /scroll current screen
         WORDS( 3 ) = 219
         WORDS( 4 ) = -XSCROL
         WORDS( 5 ) = YSCROL + CSCROF( 2 )

*   Send these commands
         NUMWOR = 5
         CALL IKNOBW( DISPID, NUMWOR, WORDS, STATUS )

      ENDDO

*   Flush the buffer
      CALL IKNOUT( DISPID, STATUS )

  99  CONTINUE

      END

