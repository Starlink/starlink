*-----------------------------------------------------------------------
*+  IKNCON - Switch on the cursor according to shape

      SUBROUTINE IKNCON ( DISPID, NUMCUR, STATUS )

*    Description :
*     A number of routines need to perform the function of this
*     routine so it has been removed from the in-line code.
*     This does not check any of the input arguments as it is
*     assumed that this has been done in the calling routine.
*
*    Invocation :
*     CALL IKNCON( DISPID, NUMCUR, STATUS )
*
*    Parameters :
*     parameter[(dimensions)]=type(access)
*           <description of parameter>
*
*    Method :
*     Switch the Ikon cursor on according to the shape.
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
*     February 1990
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

*     Cursor number
      INTEGER NUMCUR

*    Status :
      INTEGER STATUS

*    Global variables :
      INCLUDE 'IDIINC(IKN_COMCH)'
      INCLUDE 'IDIINC(IKN_COMPOS)'

*    Local variables :
      INTEGER * 2 WORDS( 15 )

      INTEGER NUMWOR
*-

*   Move to where the cursor should be
*   Ikon command 164 = 'A4'X = Move to
      WORDS( 1 ) = 164
      WORDS( 2 ) = CURX( NUMCUR ) - CMEMX( 0 )
      WORDS( 3 ) = CURY( NUMCUR ) - CMEMY( 0 )

*   Select the appropriate cursor shape
*   Cross hair ( full screen )
      IF ( ( CURSHA( NUMCUR ) .LE. 1 ) .OR.
     :     ( CURSHA( NUMCUR ) .GT. CURNSH ) ) THEN

*   Ikon command 99 = '63'X = Set register - 32 bit
*   Ikon address 57 = '39'X = Crosshair cursor size x-y
         WORDS( 4 ) = 99
         WORDS( 5 ) = 57
         WORDS( 6 ) = 0
         WORDS( 7 ) = 0

*   Ikon command 193 = 'C1'X = Crosshair cursor on
         WORDS( 8 ) = 193
         NUMWOR = 8

*   Cross
      ELSEIF ( CURSHA( NUMCUR ) .EQ. 2 ) THEN

*   Ikon command 99 = '63'X = Set register - 32 bit
*   Ikon address 57 = '39'X = Crosshair cursor size x-y
         WORDS( 4 ) = 99
         WORDS( 5 ) = 57
         WORDS( 6 ) = 32
         WORDS( 7 ) = 32

*   Ikon command 193 = 'C1'X = Crosshair cursor on
         WORDS( 8 ) = 193
         NUMWOR = 8

*   Hand ( use 64x64 bit cursor starting at symbol 46 )
      ELSEIF ( CURSHA( NUMCUR ) .EQ. 8 ) THEN

*   Ikon command 194 = 'C2'X = Symbol cursor on
         WORDS( 4 ) = 194
         WORDS( 5 ) = 46
         NUMWOR = 5

*   Otherwise use a symbol cursor defined in IICINC
*   starting with symbol numbers 6, 8, 10, 12, 14
      ELSE

*   Ikon command 194 = 'C2'X = Symbol cursor on
         WORDS( 4 ) = 194
         WORDS( 5 ) = CURSHA( NUMCUR ) * 2
         NUMWOR = 5

      ENDIF

*   Send the commands and flush the buffer
      CALL IKNOBW( DISPID, NUMWOR, WORDS, STATUS )
      CALL IKNOUT( STATUS )

      END

