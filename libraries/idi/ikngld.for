*-----------------------------------------------------------------------
*+  IKNGLD - Get Locator Displacement

      SUBROUTINE IKNGLD ( DISPID, LOCNUM, DX, DY, STATUS )

*    Description :
*     This does the Ikon specific work for the IDI routine IIIGLD.
*     The arguments are identical to those in IIIGLD.
*
*    Invocation :
*     CALL IKNGLD( DISPID, LOCNUM, DX, DY, STATUS )
*
*    Parameters :
*     parameter[(dimensions)]=type(access)
*           <description of parameter>
*
*    Method :
*     Verify the input arguments. Read the current position of the
*     locator ( GID ) and compare it to the previous value.
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
*     June 1989
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

*     Locator number
      INTEGER LOCNUM

*    Export :
*     X displacement
      INTEGER DX

*     Y displacement
      INTEGER DY

*    Status :
      INTEGER STATUS

*    Global variables :
      INCLUDE 'IDIINC(IKN_COMCH)'
      INCLUDE 'IDIINC(IKN_COMPOS)'

*    Local variables :
      INTEGER * 2 WORDS( 1 ), XYPOS( 2 )

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

*   Verify the locator number
      IF ( ( LOCNUM .LT. 0 ) .AND. ( LOCNUM .GT. CNLOC - 1 ) ) THEN
         STATUS = IDI__NOINT
         GOTO 99
      ENDIF

*   Read the current position
*   Ikon command 165 = 'A5'X = Return current position
      WORDS( 1 ) = 165
      NUMWOR = 1
      CALL IKNOBW( DISPID, NUMWOR, WORDS, STATUS )
      CALL IKNOUT( STATUS )
      NUMWOR = 2
      CALL IKNIBW( DISPID, NUMWOR, XYPOS, STATUS )

*   Calculate the displacement from the last position
      DX = XYPOS( 1 ) - CLOCXY( 1 )
      DY = XYPOS( 2 ) - CLOCXY( 2 )
      CLOCXY( 1 ) = XYPOS( 1 )
      CLOCXY( 2 ) = XYPOS( 2 )

  99  CONTINUE

      END

