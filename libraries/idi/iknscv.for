*-----------------------------------------------------------------------
*+  IKNSCV - Set Cursor Visibility on Ikon

      SUBROUTINE IKNSCV ( DISPID, NUMCUR, LVIS, STATUS )

*    Description :
*     This does the Ikon specific work for the IDI routine IICSCV.
*     The arguments are identical to those in IICSCV.
*
*    Invocation :
*     CALL IKNSCV( DISPID, NUMCUR, LVIS, STATUS )
*
*    Parameters :
*     parameter[(dimensions)]=type(access)
*           <description of parameter>
*
*    Method :
*     Verify the input arguments.
*     Switch the Ikon cursor on or off according to the flag.
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
*     Novemver 1989 Added cursor shapes
*     February 1990 Put cursor shapes into IKNCON
*     April    1990 Bug fix: moved buffer flush inside IF block
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

*     Visibility
      LOGICAL LVIS

*    Status :
      INTEGER STATUS

*    Global variables :
      INCLUDE 'IDIINC(IKN_COMCH)'

*    Local variables :
      INTEGER * 2 WORDS( 15 )

      INTEGER NUMWOR
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

*   Check the cursor number
      IF ( ( NUMCUR .GE. 0 ) .AND. ( NUMCUR .LT. CURN ) ) THEN

*   Store the visibility
         CURVIS( NUMCUR ) = LVIS

*   Display the cursor
         IF ( LVIS ) THEN
            CALL IKNCON( DISPID, NUMCUR, STATUS )

*   Undisplay cursor
         ELSE

*   Ikon command 192 = 'C0'X = Cursor off
            WORDS( 1 ) = 192
            NUMWOR = 1

*   Send the commands and flush the buffer
            CALL IKNOBW( DISPID, NUMWOR, WORDS, STATUS )
            CALL IKNOUT( STATUS )
         ENDIF

*   Otherwise flag an error
      ELSE
         STATUS = IDI__INCID
      ENDIF

  99  CONTINUE

      END

