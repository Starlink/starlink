*-----------------------------------------------------------------------
*+  IKNOLT - Output Video Look Up Table to Ikon

      SUBROUTINE IKNOLT ( DISPID, LUTNUM, STATUS )

*    Description :
*     Unlike most of the routines beginning IKN-, this routine does not
*     have an argument list matching that of the calling IDI parent
*     routine. The look-up tables are stored in common blocks. The
*     manipulation of the common blocks is left to the calling routines.
*     All this routine does is send one of the LUT's in the common
*     blocks to the Ikon.
*
*    Invocation :
*     CALL IKNOLT( DISPID, LUTNUM, STATUS )
*
*    Parameters :
*     parameter[(dimensions)]=type(access)
*           <description of parameter>
*
*    Method :
*     Send the given look-up table to all the Ikon memories that are
*     bound to this LUT.
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
*     December 1990  Changed name from IKNWLT
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

*     VLUT number
      INTEGER LUTNUM

*    Status :
      INTEGER STATUS

*    Global variables :
      INCLUDE 'IDIINC(IKN_COMCH)'
      INCLUDE 'IDIINC(IKN_COMEM)'
      INCLUDE 'IDIINC(IKN_COMLUT)'

*    Local variables :
      INTEGER * 2 WORDS( MAXCOL * 3 + 5 )
      INTEGER I, J, K, LUTLEN, NUMWOR
*-

*   Calculate the number of LUT entries from the depth
      LUTLEN = 2 ** CLUTDE

*   See if this LUT is bound to any memories
      DO I = 0, CNMEM - 1
         IF ( CLUTBI( I ) .EQ. LUTNUM ) THEN

*   Set frame buffer to write
*   Ikon command 125 = '7D'X = Set frame buffer to write
            WORDS( 1 ) = 125
            WORDS( 2 ) = I

*   Set up Ikon LUT
*   Ikon command 82 = '52'X = Set palette colours
            WORDS( 3 ) = 82
            WORDS( 4 ) = LUTLEN - 1
            WORDS( 5 ) = 0
            NUMWOR = 5

*   Send the LUT stored in the common block
            DO K = 1, LUTLEN

               IF ( LUTNUM .EQ. 0 ) THEN
                  DO J = 1, 3
                     WORDS( NUMWOR + J ) = NINT( CLUT0( J, K ) * 255.0 )
                  ENDDO
               ELSEIF ( LUTNUM .EQ. 1 ) THEN
                  DO J = 1, 3
                     WORDS( NUMWOR + J ) = NINT( CLUT1( J, K ) * 255.0 )
                  ENDDO
               ENDIF

               NUMWOR = NUMWOR + 3
            ENDDO

*   Send this to the buffer
            CALL IKNOBW( DISPID, NUMWOR, WORDS, STATUS )
         ENDIF
      ENDDO

      CALL IKNOUT( STATUS )

  99  CONTINUE

      END

