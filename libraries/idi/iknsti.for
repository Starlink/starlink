*-----------------------------------------------------------------------
*+  IKNSTI - Stop Interactive Input

      SUBROUTINE IKNSTI ( DISPID, STATUS )

*    Description :
*     This does the Ikon specific work for the IDI routine IIISTI.
*     The arguments are identical to those in IIISTI.
*
*    Invocation :
*     CALL IKNSTI( DISPID, STATUS )
*
*    Method :
*     Verify the display identifier and set the common block entries
*     for the interactions to zero
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
*     May 1989
*     December 1990  Changed name from IIISTI
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

*    Status :
      INTEGER STATUS

*    Global variables :
      INCLUDE 'IDIINC(IKN_COMCH)'
      INCLUDE 'IDIINC(IKN_COMINT)'

*    Local variables :
      INTEGER J
*-

*   Recover the characterisitics if the device is not the current one
      IF ( DISPID .NE. CURRID ) THEN
         CALL IKNUPD( DISPID, STATUS )
         CALL IDSRCO( DISPID, STATUS )
         IF ( STATUS .NE. IDI__OK ) THEN
            GOTO 99
         ENDIF
      ENDIF

*   Stop any interactions that are in progress
*   THIS ONLY APPLIES TO SYNCHRONOUS INTERACTIONS
*   THESE HAVE NOT BEEN USED FOR THE IKON

*   Reset all the interactions to zero
      CINTN = 0
      DO J = 0, MAXINT - 1
         CINTTY( J ) = 0
         CINTID( J ) = 0
         COBJTY( J ) = 0
         COBJID( J ) = 0
         CINTOP( J ) = 0
         CEXTRN( J ) = 0
      ENDDO

  99  CONTINUE

      END
      
