      SUBROUTINE DATAPIC( STATUS )
*+
*  Name:
*     DATAPIC

*  Purpose:
*     Reports information about the most recent AGI DATA picture.

*  Language:
*     Starlink Fortran 77

*  Type of Module:
*     ADAM A-task

*  Invocation:
*     CALL DATAPIC( STATUS )

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Description:
*     This routine gets information about the most recent AGI DATA
*     picture and assigns the information to several output parameters.
*     Specifically it gets the size in normalised device coordinates and 
*     in world coordinates. 

*  Usage:
*     DATAPIC DEVICE

*  ADAM Parameters:
*     DEVICE = DEVICE (Write)
*        The name of the device. [Current display device]
*     NCX1 = _REAL (Write)
*        The lower x normalised device co-ordinate of the current
*        picture.
*     NCX2 = _REAL (Write)
*        The upper x normalised device co-ordinate of the current
*        picture.
*     NCY1 = _REAL (Write)
*        The lower y normalised device co-ordinate of the current
*        picture.
*     NCY2 = _REAL (Write)
*        The upper y normalised device co-ordinate of the current
*        picture.
*     WCX1 = _REAL (Write)
*        The lower x world co-ordinate of the current picture.
*     WCX2 = _REAL (Write)
*        The upper x world co-ordinate of the current picture.
*     WCY1 = _REAL (Write)
*        The lower y world co-ordinate of the current picture.
*     WCY2 = _REAL (Write)
*        The upper y world co-ordinate of the current picture.

*  Notes:
*     -  This application mimics KAPPA:GDSTATE which gives problems
*     when used with StarTCL for some reason (it gives "HDS locator
*     invalid" messages).
*     -  The returned NDC values refer to an unit NDC square which 
*     exactly spans the shorter of the two axes, and thus extends beyond
*     1.0 in the direction of the other (longer) axis.

*  Copyright:
*     Copyright (C) 1998 Central Laboratory of the Research Councils
 
*  Authors:
*     DSB: D. Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     14-APR-1997 (DSB)
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'         ! Standard SAE constants

*  Status:
      INTEGER STATUS            ! Global status

*  External references:
      INTEGER CHR_LEN

*  Local Variables:
      CHARACTER TEXT*255        ! Results strng
      INTEGER PICID             ! Database picture id
      REAL XP1, XP2, YP1, YP2   ! World coordinates of DATA picture
      REAL XB1, XB2, YB1, YB2   ! NDC of DATA picture
      INTEGER IZDATA, IZBASE, PICBAS, ISTAT
      REAL YM, XM, FAC, XBHI, YBHI, XBLO, YBLO

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Open the device, selecting the most recent DATA picture as current.
      CALL AGS_ASSOC( 'DEVICE', 'UPDATE', 'DATA', PICID, IZDATA,
     :                STATUS )

*  Get the size of the DATA picture in pixels.
      CALL SGS_IZONE( XP1, XP2, YP1, YP2, XM, YM )

*  Get a zone identifier for the base picture.
      CALL AGI_IBASE( PICBAS, STATUS ) 
      CALL AGI_SELP( PICBAS, STATUS )
      CALL AGS_NZONE( IZBASE, STATUS )

*  Get the bounds of the DATA picture in the coordinates of the BASE
*  picture.
      ISTAT = 0
      CALL SGS_TPZ( IZDATA, XP1, YP1, IZBASE, XB1, YB1, ISTAT )
      CALL SGS_TPZ( IZDATA, XP2, YP2, IZBASE, XB2, YB2, ISTAT )

*  Get the bounds of the BASE picture.
      CALL SGS_IZONE( XBLO, XBHI, YBLO, YBHI, XM, YM )

*  Convert the base picture coordinates, so that they refer to a
*  unit NDC square which exactly spans the *SHORTER* of the two
*  axes (SGS uses a unit NDC square which exactlyt spans the 
*  *LONGER* of the two axes).
      if( STATUS .EQ. SAI__OK ) THEN
         XB1 = ( XB1 - XBLO ) / ( XBHI - XBLO ) 
         XB2 = ( XB2 - XBLO ) / ( XBHI - XBLO ) 
         YB1 = ( YB1 - YBLO ) / ( YBHI - YBLO ) 
         YB2 = ( YB2 - YBLO ) / ( YBHI - YBLO ) 
      END IF


*  Write the results to output parameters.
      WRITE( TEXT, * ) XB1, XB2, YB1, YB2, XP1, XP2, YP1, YP2
      CALL PAR_PUT0C( 'RESULT', TEXT( : CHR_LEN( TEXT ) ), STATUS )

      CALL PAR_PUT0R( 'NCX1', XB1, STATUS )
      CALL PAR_PUT0R( 'NCX2', XB2, STATUS )
      CALL PAR_PUT0R( 'NCY1', YB1, STATUS )
      CALL PAR_PUT0R( 'NCY2', YB2, STATUS )
      CALL PAR_PUT0R( 'WCX1', XP1, STATUS )
      CALL PAR_PUT0R( 'WCX2', XP2, STATUS )
      CALL PAR_PUT0R( 'WCY1', YP1, STATUS )
      CALL PAR_PUT0R( 'WCY2', YP2, STATUS )

*  Close the graphics device.
      CALL AGS_DEASS( 'DEVICE', .TRUE., STATUS )

*  If an error occurred, then report a contextual message.
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERR_REP( 'DATAPIC_ERR',
     :   'DATAPIC: Error reading device information.',
     :   STATUS )
      END IF

      END
