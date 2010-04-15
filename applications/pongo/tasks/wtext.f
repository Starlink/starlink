      SUBROUTINE WTEXT( STATUS )
*+
*  Name:
*     WTEXT

*  Purpose:
*     Draw a text string on the plot.

*  Language:
*     Starlink Fortran 77

*  Type of Module:
*     ADAM A-task

*  Description:
*     Draw a text string on the current plot at a given position,
*     justification and orientation.

*  Usage:
*     wtext action xpos ypos text [side] [justification] [angle]

*  ADAM Parameters:
*     ACTION = _CHAR (Read and Write)
*        The way in which the text string is to be written. It may be
*        one of the following:
*           - "P" -- Use PGPTEXT which allows the position,
*           justification and angle of the text to be specified.
*           - "M" -- Use PGMTEXT which allows the text to be written
*           relative to the viewport.
*           - "S" -- Use PGTEXT which allows only simple (x,y)
*           positioning of the text.
*
*        [The value is prompted for.]
*     XPOS = _REAL (Read and Write)
*        If ACTION is "P" or "S", the X coordinate of the text.  With
*        the "M" action, this parameter specifies the number of
*        character heights from the viewport where the text is to be
*        plotted (negative values are allowed).
*
*        [The value is prompted for.]
*     YPOS = _REAL (Read and Write)
*        If ACTION is "P" or "S", the Y coordinate of the text.  With
*        the "M" action, this parameter specifies the fraction along
*        the edge where the text is to be plotted.
*
*        [The value is prompted for.]
*     TEXT = _CHAR (Read and Write)
*        The text string to be plotted. This may include any of the
*        PGPLOT control sequences for producing special characters.
*
*        [The value is prompted for.]
*     SIDE = _CHAR (Read and Write)
*        If ACTION="M", the side of the viewport where the text is to
*        plotted. This may be one of the following:
*           - "T" -- The top edge.
*           - "B" -- The bottom edge.
*           - "L" -- The left-hand edge.
*           - "R" -- The right-hand edge.
*           - "LV" -- The left-hand edge, but with the string written
*           vertically.
*           - "RV" -- The right-hand edge, but with the string written
*           vertically.
*
*        If the value is not specified on the command line, the current
*        value is used. The current value is initially set to "T".
*     JUSTIFICATION = _REAL (Read and Write)
*        The justification about the specified point (in the range 0.0
*        to 1.0).  Here, 0.0 means left justify the text relative to
*        the data point, 1.0 means right justify the text relative to
*        the data point, 0.5 means centre the string on the data point,
*        other values will give intermediate justifications.
*
*        If the value is not specified on the command line, the current
*        value is used. The current value is initially set to 0.0.
*     ANGLE = _REAL (Read and Write)
*        If ACTION="P", the angle relative to the horizontal at which
*        the text string is to be plotted.
*
*        If the value is not specified on the command line, the current
*        value is used. The current value is initially set to 0.0.

*  Authors:
*     JBVAD::PAH: Paul Harrison (STARLINK)
*     PCTR: P.C.T. Rees (STARLINK)
*     PDRAPER: Peter Draper (STARLINK - Durham University)
*     {enter_new_authors_here}

*  History:
*     23-MAR-1992 (JBVAD::PAH):
*        Original version.
*     19-OCT-1992 (PCTR):
*        Added contextual error report on exit.
*     22-JUN-1994 (PDRAPER):
*        Now only prompts for justification when appropriate.
*     22-JUN-1994 (PDRAPER):
*        Added check for device open.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants

*  Status:
      INTEGER STATUS             ! Global status

*  External References:
      EXTERNAL PON_DEVOP
      LOGICAL PON_DEVOP          ! PGPLOT device is open

*  Local Variables:
      CHARACTER * ( 132 ) PTEXT  ! Text to be displayed
      CHARACTER * ( 7 ) SIDE     ! SIDE parameter value
      CHARACTER * ( 1 ) TYPE     ! PGTYPE

      REAL ANGLE                 ! ANGLE parameter value
      REAL JUST                  ! JUSTIFICATION parameter value
      REAL X                     ! XPOS parameter value
      REAL Y                     ! YPOS parameter value

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN
      IF ( PON_DEVOP( .TRUE., STATUS ) ) THEN
         CALL PAR_GET0C( 'ACTION', TYPE, STATUS )
         CALL CHR_UCASE( TYPE )
         CALL PAR_GET0R( 'XPOS', X, STATUS )
         CALL PAR_GET0R( 'YPOS', Y, STATUS )
         CALL PAR_GET0C( 'TEXT', PTEXT, STATUS )

         IF ( TYPE .EQ. 'S' ) THEN
            IF ( STATUS .EQ. SAI__OK ) CALL PGTEXT( X, Y, PTEXT )
         ELSE
            CALL PAR_GET0R( 'JUSTIFICATION', JUST, STATUS )
            IF ( TYPE .EQ. 'M' ) THEN
               CALL PAR_GET0C( 'SIDE', SIDE, STATUS )
               IF ( STATUS .EQ. SAI__OK ) CALL PGMTEXT( SIDE, X, Y,
     :                                                  JUST, PTEXT )
            ELSE IF ( TYPE .EQ. 'P' ) THEN
               CALL PAR_GET0R( 'ANGLE',ANGLE, STATUS )
               IF ( STATUS .EQ. SAI__OK ) CALL PGPTEXT( X, Y, ANGLE,
     :                                                  JUST, PTEXT )
            END IF
         END IF
      END IF

*  Check the returned status and report a contextual error message if
*  necessary.
      IF ( STATUS .NE. SAI__OK ) CALL ERR_REP( 'WTEXT_END',
     :                              'WTEXT: Unable to draw text ' //
     :                              'string.', STATUS )

      END
* $Id$
