*-----------------------------------------------------------------------
*+  IKNWCP - Write Cursor Position on Ikon

      SUBROUTINE IKNWCP ( DISPID, MEMID, NUMCUR, XC, YC, STATUS )

*    Description :
*     This does the Ikon specific work for the IDI routine IICWCP.
*     The arguments are identical to those in IICWCP.
*
*    Invocation :
*     CALL IKNWCP( DISPID, MEMID, NUMCUR, XC, YC, STATUS )
*
*    Parameters :
*     parameter[(dimensions)]=type(access)
*           <description of parameter>
*
*    Method :
*     Verify the input arguments.
*     Calculate the cursor position relative to the memory origin
*     or the screen origin according to MEMID.
*     Plot the cursor at the new position.
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
*     Novmeber 1989 Removed check of cursor visibility
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

*     Memory identifier
      INTEGER MEMID

*     Cursor number
      INTEGER NUMCUR

*     X position
      INTEGER XC

*     Y position
      INTEGER YC

*    Status :
      INTEGER STATUS

*    Global variables :
      INCLUDE 'IDIINC(IKN_COMCH)'
      INCLUDE 'IDIINC(IKN_COMEM)'
      INCLUDE 'IDIINC(IKN_COMPOS)'

*    Local variables :
      INTEGER * 2 WORDS( 7 )

      INTEGER NUMWOR, XP, YP
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

*   Check the cursor number
      IF ( ( NUMCUR .LT. 0 ) .OR. ( NUMCUR .GE. CURN ) ) THEN
         STATUS = IDI__INCID
         GOTO 99
      ENDIF

*   If MEMID = -1 then the position is relative to the screen origin
*   Use memory 0 to work out the screen coordinates allowing for zoom
      IF ( MEMID .EQ. -1 ) THEN
         XP = XC - CSCROX( 0 )
         YP = YC - CSCROY( 0 ) + CNPIX( 1 ) *
     :        CMEMZ( 0 ) / ( CMEMZ( 0 ) + 1 )
         CURX( NUMCUR ) = XP + CMEMX( 0 )
         CURY( NUMCUR ) = YP + CMEMY( 0 )

*   Ikon command 123 = '7B'X = Set frame buffer to write
         WORDS( 1 ) = 123
         WORDS( 2 ) = 0

*   Otherwise it is relative to the memory origin
      ELSEIF ( ( MEMID .GE. 0 ) .AND. ( MEMID .LT. CNMEM ) ) THEN
         CURX( NUMCUR ) = XC + CMEMX( MEMID )
         CURY( NUMCUR ) = YC + CMEMY( MEMID )
         XP = XC
         YP = YC

*   Ikon command 123 = '7B'X = Set frame buffer to write
         WORDS( 1 ) = 123
         WORDS( 2 ) = MEMID

*   Otherwise flag an error
      ELSE
         STATUS = IDI__INMID
         GOTO 99
      ENDIF

*   Plot the new position 
*   Ikon command 164 = 'A4'X = Move to
      WORDS( 3 ) = 164
      WORDS( 4 ) = XP
      WORDS( 5 ) = YP
      NUMWOR = 5

*   Send these commands
      CALL IKNOBW( DISPID, NUMWOR, WORDS, STATUS )
      CALL IKNOUT( STATUS )

  99  CONTINUE

      END

