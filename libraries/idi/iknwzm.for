*-----------------------------------------------------------------------
*+  IKNWZM - Write Ikon Memory Zoom

      SUBROUTINE IKNWZM ( DISPID, MEMID, NMEM, ZOOMF, STATUS )

*    Description :
*     This does the Ikon specific work for the IDI routine IIZWZM.
*     The arguments are identical to those in IIZWZM.
*
*    Invocation :
*     CALL IKNWZM( DISPID, MEMID, NMEM, ZOOMF, STATUS )
*
*    Parameters :
*     parameter[(dimensions)]=type(access)
*           <description of parameter>
*
*    Method :
*     Verify the input arguments.
*     Send the command to zoom the Ikon screen.
*     Calculate the scrolls to bring about one of the following
*        if a cursor was displayed bring the cursor back to the middle
*        of the display
*        else bring what was the middle of the screen back to that
*        position.
*     Send the scroll commands.
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
*     May 1990       Allow for zoom about ROI
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

*     Zoom factor
      INTEGER ZOOMF

*    Status :
      INTEGER STATUS

*    Global variables :
      INCLUDE 'IDIINC(IKN_COMCH)'
      INCLUDE 'IDIINC(IKN_COMEM)'
      INCLUDE 'IDIINC(IKN_COMPOS)'

*    Local variables :
      LOGICAL GOTONE

      INTEGER * 2 WORDS( 5 )

      INTEGER J, K, L, NUMWOR, OLZOOM, ROIX, ROIY, XSCROL, YSCROL
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

*   Loop through the memories
      DO J = 1, NMEM

*   Get the old zoom factor and store the new one in the common block
         OLZOOM = CMEMZ( MEMID( J ) )

*   Save this for all memories as they all zoom together
         DO L = 0, CNMEM - 1
            CMEMZ( L ) = ZOOMF
         ENDDO

*   Select the frame buffer to display
*   Ikon command 123 = '7B'X = Set frame buffer to display
         WORDS( 1 ) = 123
         WORDS( 2 ) = MEMID( J )

*   Send zoom command to Ikon. Zoom equally in both axes.
*   Ikon command 216 = 'D8'X = Zoom base screen
         WORDS( 3 ) = 216
         WORDS( 4 ) = ZOOMF
         WORDS( 5 ) = ZOOMF

*   Send these commands
         NUMWOR = 5
         CALL IKNOBW( DISPID, NUMWOR, WORDS, STATUS )

*   Scroll the screen to keep either a cursor or a ROI or a memory
*   in the centre of the screen
*   See if any cursors are visible
         GOTONE = .FALSE.
         K = 0
         DO WHILE ( ( K .LT. CURN ) .AND. ( .NOT. GOTONE ) )
            IF ( CURVIS( K ) ) THEN
               GOTONE = .TRUE.

*   Zoom about this cursor, but scroll only in steps of 16 in x
*   Bring the cursor back to the middle of the screen
*   The scrolls
*   xscrol = ( curx * zoomf ) / ( zoomf + 1 )
*   yscrol = ( ( cnpix - cury ) * zoomf ) / ( zoomf + 1 )
*   bring the cursor back to its original position.
*   The scrolls
*   xscrol = xscrol + ( curx - cnpix / 2 ) / ( zoomf + 1 )
*   yscrol = yscrol + ( cnpix / 2 - cury ) / ( zoomf + 1 )
*   bring the cursor to the middle of the screen.
*   These equations can then be simplified
               XSCROL = CURX( K ) - CNPIX( 0 ) / ( 2 * ( ZOOMF + 1 ) ) -
     :                  CMEMX( MEMID( J ) )
               XSCROL = ( ( XSCROL + SIGN( 8, XSCROL ) ) / 16 ) * 16
               YSCROL = -CURY( K ) + ( CNPIX( 1 ) * ZOOMF +
     :                  CNPIX( 1 ) / 2 ) / ( ZOOMF + 1 ) +
     :                  CMEMY( MEMID( J ) )


*   Store the scroll position in the common blocks
*   Save this for all memories as they all scroll together
               DO L = 0, CNMEM - 1
                  CSCROX( L ) = -XSCROL
                  CSCROY( L ) = YSCROL
               ENDDO

*   Send the scroll command
*   Ikon command 219 = 'DB'X = Pan / scroll current screen
               WORDS( 1 ) = 219
               WORDS( 2 ) = XSCROL
               WORDS( 3 ) = YSCROL + CSCROF( 2 )
               NUMWOR = 3

            ELSE
               K = K + 1
            ENDIF
         ENDDO

*   See of any ROI's are visible
         K = 0
         IF ( CANROI ) THEN
            DO WHILE ( ( K .LT. CNROI ) .AND. ( .NOT. GOTONE ) )
               IF ( CROIVI( K ) ) THEN
                  GOTONE = .TRUE.

*   Calculate the middle of the ROI
                  ROIX = ( CROIXL( K ) + CROIXH( K ) ) / 2
                  ROIY = ( CROIYL( K ) + CROIYH( K ) ) / 2

*   Zoom about this ROI, but scroll only in steps of 16 in x
*   Bring the middle of the ROI back to the middle of the screen
*   The scrolls
*   xscrol = ( roix * zoomf ) / ( zoomf + 1 )
*   yscrol = ( ( cnpix - roiy ) * zoomf ) / ( zoomf + 1 )
*   bring the ROI back to its original position.
*   The scrolls
*   xscrol = xscrol + ( roix - cnpix / 2 ) / ( zoomf + 1 )
*   yscrol = yscrol + ( cnpix / 2 - roiy ) / ( zoomf + 1 )
*   bring the ROI to the middle of the screen.
*   These equations can then be simplified
                  XSCROL = ROIX - CNPIX( 0 ) / ( 2 * ( ZOOMF + 1 ) ) -
     :                     CMEMX( MEMID( J ) )
                  XSCROL = ( ( XSCROL + SIGN( 8, XSCROL ) ) / 16 ) * 16
                  YSCROL = -ROIY + ( CNPIX( 1 ) * ZOOMF +
     :                     CNPIX( 1 ) / 2 ) / ( ZOOMF + 1 ) +
     :                     CMEMY( MEMID( J ) )

*   Store the scroll position in the common blocks
*   Save this for all memories as they all scroll together
               DO L = 0, CNMEM - 1
                  CSCROX( L ) = -XSCROL
                  CSCROY( L ) = YSCROL
               ENDDO

*   Send the scroll command
*   Ikon command 219 = 'DB'X = Pan / scroll current screen
               WORDS( 1 ) = 219
               WORDS( 2 ) = XSCROL
               WORDS( 3 ) = YSCROL + CSCROF( 2 )
               NUMWOR = 3

               ELSE
                  K = K + 1
               ENDIF
            ENDDO
         ENDIF

*   Otherwise zoom about the centre of the display
*   But only scroll in steps of 16 in the x direction
         IF ( .NOT. GOTONE ) THEN

*   Calculate the required scroll factors
            XSCROL = CSCROX( MEMID( J ) ) +
     :               ( CNPIX( 0 ) * OLZOOM ) / ( 2 * ( OLZOOM + 1 ) ) -
     :               ( CNPIX( 0 ) * ZOOMF ) / ( 2 * ( ZOOMF + 1 ) )
            XSCROL = ( ( XSCROL + SIGN( 8, XSCROL ) ) / 16 ) * 16
            YSCROL = CSCROY( MEMID( J ) ) -
     :               ( CNPIX( 1 ) * OLZOOM ) / ( 2 * ( OLZOOM + 1 ) ) +
     :               ( CNPIX( 1 ) * ZOOMF ) / ( 2 * ( ZOOMF + 1 ) )

*   Store the scroll position in the common blocks
*   Save this for all memories as they all scroll together
            DO L = 0, CNMEM - 1
               CSCROX( L ) = XSCROL
               CSCROY( L ) = YSCROL
            ENDDO

*   Send the scroll command
*   Ikon command 219 = 'DB'X = Pan / scroll current screen
            WORDS( 1 ) = 219
            WORDS( 2 ) = -XSCROL
            WORDS( 3 ) = YSCROL + CSCROF( 2 )
            NUMWOR = 3
         ENDIF

*   Send these commands
         CALL IKNOBW( DISPID, NUMWOR, WORDS, STATUS )

      ENDDO

*   Update the display
      CALL IKNOUT( STATUS )

  99  CONTINUE

      END

