*-----------------------------------------------------------------------
*+  IKNSLT - Select Memory Look up Tables

      SUBROUTINE IKNSLT ( DISPID, MEMID, LUTNUM, ITTNUM, STATUS )

*    Description :
*     This does the Ikon specific work for the IDI routine IIMSLT.
*     The arguments are identical to those in IIMSLT.
*
*    Invocation :
*     CALL IKNSLT( DISPID, MEMID, LUTNUM, ITTNUM, STATUS )
*
*    Method :
*     Verify the input arguments and save the information in the
*     common block. Update the display if the new bindings affect it.
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
*     June 1989
*     December 1990  Changed name from IIMSLT
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

*     VLUT identifier
      INTEGER LUTNUM

*     ITT number
      INTEGER ITTNUM

*    Status :
      INTEGER STATUS

*    Global variables :
      INCLUDE 'IDIINC(IKN_COMCH)'
      INCLUDE 'IDIINC(IKN_COMEM)'
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

*   Verify the memory identifier
      IF ( ( MEMID .LT. 0 ) .OR. ( MEMID .GT. CNMEM - 1 ) ) THEN
         STATUS = IDI__INMID
         GOTO 99
      ENDIF

*   Verify the LUT number
      IF ( ( LUTNUM .LT. -1 ) .OR. ( LUTNUM .GT. CNLUT - 1 ) ) THEN
         STATUS = IDI__INLUT
         GOTO 99
      ENDIF

*   Check the ITT number
      IF ( ( ITTNUM .LT. -1 ) .OR. ( ITTNUM .GT. CNITT - 1 ) ) THEN
         STATUS = IDI__INITT
         GOTO 99
      ENDIF

*   A ITT number of -1 uses the current setting, otherwise
*   save the memory bindings in the common block
      IF ( ITTNUM .GT. -1 ) THEN
         CITTBI( MEMID ) = ITTNUM
      ENDIF

*   A LUT number of -1 uses the current setting, otherwise...
      IF ( LUTNUM .GT. -1 ) THEN

*   Save the memory bindings in the common block
         CLUTBI( MEMID ) = LUTNUM

*   Call the device specific routines to see if the new bindings
*   affect the displayed LUTs
         CALL IKNOLT( DISPID, LUTNUM, STATUS )
      ENDIF

  99  CONTINUE

      END
      
