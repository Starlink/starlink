*+  SSHOW - Dump contents of current model to ascii device
      SUBROUTINE SSHOW( STATUS )
*
*    Description :
*
*     Writes a description of the specified model to a user specified
*     ascii device.
*
*    Environment parameters :
*
*     MODEL=UNIV(U)
*		Object containing model
*     DEV=CHAR(R)
*               Output device specification
*
*    Deficiencies :
*    Bugs :
*    Authors :
*
*     David J. Allan (BHVAD::DJA)
*
*    History :
*
*     23 Oct 92 : V1.7-0 Adapted from SEDIT (DJA)
*      5 May 94 : V1.7-1 Use AIO for output (DJA)
*     24 Nov 94 : V1.8-0 Now use USI for user interface (DJA)
*     24 Apr 95 : V1.8-1 Use updated ADI routines (DJA)
*
*    Type Definitions :
*
      IMPLICIT NONE
*
*    Global constants :
*
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
      INCLUDE 'FIT_PAR'
*
*    Status :
*
      INTEGER STATUS
*
*    Local variables :
*
      INTEGER                	COMP(NPAMAX)     	! List of components
      INTEGER			FID			! Model spec dataset
      INTEGER                	OCH              	! Output channel
      INTEGER                	PAR(NPAMAX)      	! List of parameters
      INTEGER                	PARTOT           	! No. of parameters
      INTEGER                	WID              	! Width for output
*
*    Version :
*
      CHARACTER*30         	VERSION
        PARAMETER           	( VERSION = 'SSHOW Version 2.1-0' )
*-

*    Check status
      IF ( STATUS .NE. SAI__OK ) RETURN

*    Version
      CALL MSG_PRNT( VERSION )

*    Initialise ASTERIX
      CALL AST_INIT()

*    Retrieve fit_model object
      CALL USI_ASSOC( 'MODEL', '*', 'READ', FID, STATUS )

*    Declare file to user :
      CALL DISP_FILENAM( FID, 'Model', STATUS )

*    Set up output channel
      CALL AIO_ASSOCO( 'DEV', 'LIST', OCH, WID, STATUS )

*    Display component parameters
      CALL SEDIT_LISTPAR( FID, PARTOT, COMP, PAR, OCH, STATUS )

*    Free channel
      CALL AIO_CANCL( 'DEV', STATUS )

*    Exit
      CALL AST_CLOSE()
      CALL AST_ERR( STATUS )

      END
