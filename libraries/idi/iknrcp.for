*-----------------------------------------------------------------------
*+  IKNRCP - Read Cursor Position

      SUBROUTINE IKNRCP ( DISPID, INMID, NUMCUR, XC, YC, OUTMID,
     :                    STATUS )

*    Description :
*     This does the Ikon specific work for the IDI routine IICRCP.
*     The arguments are identical to those for IICRCP.
*
*    Invocation :
*     CALL IKNRCP( DISPID, INMID, NUMCUR, XC, YC, OUTMID, STATUS )
*
*    Method :
*     Check the input arguments, then calculate the cursor position
*     from entries in the common blocks.
*
*    Deficiencies :
*     <description of any deficiencies>
*    Bugs :
*     <description of any "bugs" which have not been fixed>
*
*    Authors :
*     Nick Eaton  ( DUVAD::NE )
*
*    History :
*     December 1988
*     December 1990  Changed name from IICRCP
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

*     Input memory identifier
      INTEGER INMID

*     Cursor number
      INTEGER NUMCUR

*    Export :
*     X position
      INTEGER XC

*     Y position
      INTEGER YC

*     Output memory identifier
      INTEGER OUTMID

*    Status :
      INTEGER STATUS

*    Global variables :
      INCLUDE 'IDIINC(IKN_COMCH)'
      INCLUDE 'IDIINC(IKN_COMEM)'
      INCLUDE 'IDIINC(IKN_COMPOS)'

*    Local variables :
      INTEGER J, MAXPR
*-

*   Recover the common blocks if the device is not the current one
      IF ( DISPID .NE. CURRID ) THEN
         CALL IKNUPD( DISPID, STATUS )
         CALL IDSRCO( DISPID, STATUS )
         IF ( STATUS .NE. IDI__OK ) THEN
            STATUS = IDI__NOREC
            GOTO 99
         ENDIF
      ENDIF

*   Check the cursor number
      IF ( ( NUMCUR .LT. 0 ) .AND. ( NUMCUR .GE. CURN ) ) THEN
         STATUS = IDI__INCID
         GOTO 99
      ENDIF

*   If INMID = -1 then the position is relative to the screen origin
      IF ( INMID .EQ. -1 ) THEN
         XC = CURX( NUMCUR ) + CSCROX( 0 ) - CMEMX( 0 )
         YC = CURY( NUMCUR ) + CSCROY( 0 ) - CMEMY( 0 ) -
     :        CNPIX( 1 ) * CMEMZ( 0 ) / ( CMEMZ( 0 ) + 1 )

*   Output memory to which cursor currently points
*   The Ikon memories fill the screen and wrap around, so just find
*   the one with the highest priority
         OUTMID = 0
         MAXPR = CMEMPR( 0 )
         DO J = 1, CNMEM - 1
            IF ( CMEMPR( J ) .GT. MAXPR ) THEN
               MAXPR = CMEMPR( J )
               OUTMID = J
            ENDIF
         ENDDO

*   Otherwise it is relative to the memory origin
      ELSEIF ( ( INMID .GE. 0 ) .AND. ( INMID .LT. CNMEM ) ) THEN
         XC = CURX( NUMCUR ) - CMEMX( INMID )
         YC = CURY( NUMCUR ) - CMEMY( INMID )
         OUTMID = INMID

*   Otherwise flag an error
      ELSE
         STATUS = IDI__INMID
      ENDIF

  99  CONTINUE

      END

