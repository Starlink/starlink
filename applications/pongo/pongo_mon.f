      SUBROUTINE PONGO_MON( STATUS )
*+
*  Name:
*     PONGO_MON

*  Purpose:
*     The PONGO monolith.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL PONGO_MON( STATUS )

*  Description:
*     This is the monolith subroutine used call each of the PONGO tasks.

*  Arguments:
*     NAME = CHARACTER * ( * ) (Given)
*        The name of the task to be called.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     PAH: Paul Harrison (STARLINK, Jodrell Bank)
*     MJC: Malcolm J. Currie (STARLINK)
*     PDRAPER: P.W. Draper (STARLINK - Durham University)
*     {enter_new_authors_here}

*  History:
*     9-FEB-1990 (PAH):
*        Original version.
*     1994 April 22 (MJC):
*        Uses the new mechanism for obtaining the task name.
*     10-JUN-1994 (PDRAPER):
*        Ported to UNIX.
*     23-FEB-1995 (PDRAPER):
*        Added PON_INIT block data.
*     17-OCT-1996 (PDRAPER):
*        Added PON_INIT as a callable subroutine (problems with
*        block data on Linux systems using g77).
*     23-APR-1997 (PDRAPER):
*        Added SETGLOBAL and SETGLOBAL commands.
*     28-APR-1997 (PDRAPER):
*        Added UNSETGLOBAL and LIMITS.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'PAR_PAR'          ! Parameter-library constants

*  Status:
      INTEGER STATUS             ! Global status

*  External References (not used at present):
C      EXTERNAL PON_INIT          ! Initialise common blocks

*  Local Variables:
      CHARACTER * ( PAR__SZNAM ) NAME ! Task name
      LOGICAL STARTD
      LOGICAL INITED
      DATA INITED /.FALSE./
      SAVE INITED
      DATA STARTD /.FALSE./
      SAVE STARTD
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  If not already done initialise the common blocks.
      IF ( .NOT. INITED ) THEN
         INITED = .TRUE.
         CALL PON_INIT
      END IF

*  Get the action name.
      CALL TASK_GET_NAME( NAME, STATUS )

      IF ( NAME.EQ. 'ANNOTATE' ) THEN
         CALL ANNOTATE( STATUS )
      ELSE IF ( NAME.EQ. 'ARC' ) THEN
         CALL ARC( STATUS )
      ELSE IF ( NAME.EQ. 'AVEDAT' ) THEN
         CALL AVEDAT( STATUS )
      ELSE IF ( NAME.EQ. 'BEGPONGO' ) THEN
         CALL BEGPONGO( STATUS )

*  If not started then register ENDPONGO as an atexit routine. Note
*  we leave this until after this call to try and get to the top of the
*  exit stack (i.e. we want to be first called).
         IF ( .NOT. STARTD ) THEN
            CALL PON_ATEXIT( STATUS )
            STARTD = .TRUE.
         END IF
      ELSE IF ( NAME.EQ. 'BOXFRAME' ) THEN
         CALL BOXFRAME( STATUS )
      ELSE IF ( NAME.EQ. 'CCMATH' ) THEN
         CALL CCMATH( STATUS )
      ELSE IF ( NAME.EQ. 'CHANGE' ) THEN
         CALL CHANGE( STATUS )
      ELSE IF ( NAME.EQ. 'CLEAR' ) THEN
         CALL CLEAR( STATUS )
      ELSE IF ( NAME.EQ. 'CLLOG' ) THEN
         CALL CLLOG( STATUS )
      ELSE IF ( NAME.EQ. 'CURSE' ) THEN
         CALL CURSE( STATUS )
      ELSE IF ( NAME.EQ. 'ELLIPSES' ) THEN
         CALL ELLIPSES( STATUS )
      ELSE IF ( NAME.EQ. 'ENDPONGO' ) THEN
         CALL ENDPONGO( STATUS )
      ELSE IF ( NAME.EQ. 'ERRORBAR' ) THEN
         CALL ERRORBAR( STATUS )
      ELSE IF ( NAME.EQ. 'FITCURVE' ) THEN
         CALL FITCURVE( STATUS )
      ELSE IF ( NAME.EQ. 'FITLINE' ) THEN
         CALL FITLINE( STATUS )
      ELSE IF ( NAME.EQ. 'GETPOINT' ) THEN
         CALL GETPOINT( STATUS )
      ELSE IF ( NAME.EQ. 'GPOINTS' ) THEN
         CALL GPOINTS( STATUS )
      ELSE IF ( NAME.EQ. 'GRID' ) THEN
         CALL GRID( STATUS )
      ELSE IF ( NAME.EQ. 'GT_CIRCLE' ) THEN
         CALL GT_CIRCLE( STATUS )
      ELSE IF ( NAME.EQ. 'INQUIRE' ) THEN
         CALL INQUIRE( STATUS )
      ELSE IF ( NAME.EQ. 'LABEL' ) THEN
         CALL LABEL( STATUS )
      ELSE IF ( NAME.EQ. 'PALETTE' ) THEN
         CALL PALETTE( STATUS )
      ELSE IF ( NAME.EQ. 'PAPER' ) THEN
         CALL PAPER( STATUS )
      ELSE IF ( NAME.EQ. 'PLOTFUN' ) THEN
         CALL PLOTFUN( STATUS )
      ELSE IF ( NAME.EQ. 'PLOTHIST' ) THEN
         CALL PLOTHIST( STATUS )
      ELSE IF ( NAME.EQ. 'PRIM' ) THEN
         CALL PRIM( STATUS )
      ELSE IF ( NAME.EQ. 'PVECT' ) THEN
         CALL PVECT( STATUS )
      ELSE IF ( NAME.EQ. 'READF' ) THEN
         CALL READF( STATUS )
      ELSE IF ( NAME.EQ. 'VECT' ) THEN
         CALL VECT( STATUS )
      ELSE IF ( NAME.EQ. 'VIEWPORT' ) THEN
         CALL VIEWPORT( STATUS )
      ELSE IF ( NAME.EQ. 'WORLD' ) THEN
         CALL WORLD( STATUS )
      ELSE IF ( NAME.EQ. 'WRITEI' ) THEN
         CALL WRITEI( STATUS )
      ELSE IF ( NAME.EQ. 'WTEXT' ) THEN
         CALL WTEXT( STATUS )
      ELSE IF ( NAME.EQ. 'SETGLOBAL' ) THEN
         CALL SETGLOBAL( STATUS )
      ELSE IF ( NAME.EQ. 'GETGLOBAL' ) THEN
         CALL GETGLOBAL( STATUS )
      ELSE IF ( NAME.EQ. 'UNSETGLOBAL' ) THEN
         CALL UNSETGLOBAL( STATUS )
      ELSE IF ( NAME.EQ. 'LIMITS' ) THEN
         CALL LIMITS( STATUS )
      ELSE IF ( NAME.EQ. 'DRAWPOLY' ) THEN
         CALL DRAWPOLY( STATUS )
      END IF
      END
* $Id$
