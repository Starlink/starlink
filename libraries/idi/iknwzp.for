*-----------------------------------------------------------------------
*+  IKNWZP - Write Ikon Display Zoom and Pan

      SUBROUTINE IKNWZP ( DISPID, XOFF, YOFF, ZOOMF, STATUS )

*    Description :
*     This does the Ikon specific work for the IDI routine IIZWZP.
*     The arguments are identical to those in IIZWZP.
*
*    Invocation :
*     CALL IKNWZP( DISPID, XOFF, YOFF, ZOOMF, STATUS )
*
*    Parameters :
*     parameter[(dimensions)]=type(access)
*           <description of parameter>
*
*    Method :
*     Verify the input arguments.
*     Send the command to zoom the Ikon screen.
*     Calculate the scrolls to bring what was the middle of the
*     screen back to that position.
*     Send the scroll commands.
*     Since the Ikon zooms and scrolls all memories at the same time
*     then only need to perform the action on one.
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
*     May 1990
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

*     X offset
      INTEGER XOFF

*     Y offset
      INTEGER YOFF

*     Zoom factor
      INTEGER ZOOMF

*    Status :
      INTEGER STATUS

*    Global variables :
      INCLUDE 'IDIINC(IKN_COMCH)'
      INCLUDE 'IDIINC(IKN_COMEM)'
      INCLUDE 'IDIINC(IKN_COMPOS)'

*    Local variables :
      INTEGER * 2 WORDS( 5 )

      INTEGER L, NUMWOR, OLZOOM, XSCROL, YSCROL
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

*   Check the zoom factor is valid
      IF ( ( ZOOMF .LT. CZOOMR( 0 ) ) .OR.
     :     ( ZOOMF .GT. CZOOMR( 1 ) ) ) THEN
         STATUS = IDI__RANGE
         GOTO 99
      ENDIF

*   Get the old zoom factor and store the new one in the common block
      OLZOOM = CMEMZ( 0 )

*   Save this for all memories as they all zoom together
      DO L = 0, CNMEM - 1
         CMEMZ( L ) = ZOOMF
      ENDDO

*   Select the frame buffer to display
*   Ikon command 123 = '7B'X = Set frame buffer to display
      WORDS( 1 ) = 123
      WORDS( 2 ) = 0

*   Send zoom command to Ikon. Zoom equally in both axes.
*   Ikon command 216 = 'D8'X = Zoom base screen
      WORDS( 3 ) = 216
      WORDS( 4 ) = ZOOMF
      WORDS( 5 ) = ZOOMF

*   Send these commands
      NUMWOR = 5
      CALL IKNOBW( DISPID, NUMWOR, WORDS, STATUS )

*   Zoom about the centre of the display and add on the required offsets
*   But only scroll in steps of 16 in the x direction
      XSCROL = XOFF - ( CNPIX( 0 ) * ZOOMF ) / ( 2 * ( ZOOMF + 1 ) )
      XSCROL = ( ( XSCROL + SIGN( 8, XSCROL ) ) / 16 ) * 16
      YSCROL = YOFF + ( CNPIX( 1 ) * ZOOMF ) / ( 2 * ( ZOOMF + 1 ) )

*   Store the scroll position in the common blocks
*   Save this for all memories as they all scroll together
      DO L = 0, CNMEM - 1
         CSCROX( L ) = XSCROL
         CSCROY( L ) = YSCROL
         CMEMX( L ) = ( ( XOFF + SIGN( 8, XOFF ) ) / 16 ) * 16
         CMEMY( L ) = YOFF
      ENDDO

*   Send the scroll command
*   Ikon command 219 = 'DB'X = Pan / scroll current screen
      WORDS( 1 ) = 219
      WORDS( 2 ) = -XSCROL
      WORDS( 3 ) = YSCROL + CSCROF( 2 )
      NUMWOR = 3

*   Send these commands
      CALL IKNOBW( DISPID, NUMWOR, WORDS, STATUS )
      CALL IKNOUT( STATUS )

  99  CONTINUE

      END

