************************************************************************
*+  AGD_1IINIT - Initialise the IDI parameter common blocks.

      SUBROUTINE AGD_1IINIT ( STATUS )

*    Description :
*     Initialise the IDI parameter common blocks which contain the
*     values of the zoom and scroll factors for the memories.
*     The definitions are held in the INCLUDE file 'AGI_IDIPS'.
*
*    Invocation :
*     CALL AGD_1IINIT( STATUS )
*
*    Method :
*     Set all thje values to zero. For the zoom and scroll factors
*     these are valid vaules which indicate the default display;
*     i.e. no zoom and the display origin in the bottom left hand
*     corner.
*
*    Deficiencies :
*
*    Bugs :
*
*    Authors :
*     Nick Eaton  ( DUVAD::NE )
*
*    History :
*     June 1990
*    endhistory
*
*    Type Definitions :
      IMPLICIT NONE

*    Global constants :
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'

*    Status :
      INTEGER STATUS

*    Global variables :
      INCLUDE 'agi_idips'

*    Local variables :
      INTEGER I
*-

      IF ( STATUS .EQ. SAI__OK ) THEN

*   Clear out all the common block entries.
         CDIPID = 0
         CNMEMS = 0
         DO I = 0, MXMEMS - 1
            CZOOMF( I ) = 0
            CXSCRL( I ) = 0
            CYSCRL( I ) = 0
         ENDDO

      ENDIF

      END

