*-----------------------------------------------------------------------
*+  IKNUPD - Update Ikon

      SUBROUTINE IKNUPD ( DISPID, STATUS )

*    Description :
*     This does the Ikon specific work for the IDI routine IIDUPD.
*     The arguments are identical to those in IIDUPD.
*
*    Invocation :
*     CALL IKNUPD( DISPID, STATUS )
*
*    Method :
*     Send the output buffer if it contains data for this device.
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
*     November 1988
*    endhistory
*
*    Type Definitions :
      IMPLICIT NONE

*    Global constants :
      INCLUDE 'IDIINC(IKN_PAR)'

*    Import :
*     Display identifier
      INTEGER DISPID

*    Status :
      INTEGER STATUS

*    Global variables :
      INCLUDE 'IDIINC(IKN_COMID)'
      INCLUDE 'IDIINC(IKN_COMBUF)'
*-

*   Output the buffer if the given display is currently being written to
      IF ( QCHAN .EQ. ACHAN( DISPID ) ) THEN
         CALL IKNOUT( STATUS )
      ENDIF

      END

