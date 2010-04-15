      SUBROUTINE LABEL( STATUS )
*+
*  Name:
*     LABEL

*  Purpose:
*     Draw the axis labels and title on the plot.

*  Language:
*     Starlink Fortran 77

*  Type of Module:
*     ADAM A-task

*  Description:
*     Draw the axis labels and the title on the plot. If the COLUMNS
*     parameter is specified, the axis labels are obtained from the
*     column labels read from the data file.

*  Usage:
*     label [xlabel] [ylabel] [title]

*  ADAM Parameters:
*     XLABEL = _CHAR (Read and Write)
*        The X-axis label.
*
*        If no value is specified on the command line, then if COLUMNS
*        is TRUE the value is taken from the column heading in the
*        data file, otherwise the value of the global parameter
*        PONGO_XLABEL is used. If PONGO_XLABEL is not defined, the
*        value " " is used.
*     YLABEL = _CHAR (Read and Write)
*        The Y-axis label.
*
*        If no value is specified on the command line, then if COLUMNS
*        is TRUE the value is taken from the column heading in the
*        data file, otherwise the value of the global parameter
*        PONGO_YLABEL is used. If PONGO_YLABEL is not defined, the
*        value " " is used.
*     TITLE = _CHAR (Read and Write)
*        The plot title.
*
*        [The value of the global parameter PONGO_TITLE is used. If
*        PONGO_TITLE is not defined, the value " " is used.]
*     COLUMNS = _LOGICAL (Read)
*        If TRUE, the values of the X and Y labels will be obtained from
*        the column headings in the data file.
*        [FALSE]

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     JBVAD::PAH: Paul Harrison (STARLINK)
*     PCTR: P.C.T. Rees (STARLINK)
*     PDRAPER: Peter Draper (STARLINK - Durham University)
*     {enter_new_authors_here}

*  History:
*     6-APR-1990 (JBVAD::PAH):
*        Original version.
*     19-OCT-1992 (PCTR):
*        Added contextual error report on exit.
*     20-JUN-1994 (PDRAPER):
*        Added check for device open.
*     9-MAY-1997 (PDRAPER):
*        Modified so that ! for XLABEL or YLABEL when
*        COLUMNS is TRUE also uses stored labels (IRAF).
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'PONGO_PAR'        ! PONGO global constants
      INCLUDE 'PAR_ERR'          ! Parameter system error codes

*  Global Variables:
      INCLUDE 'PONGO_CMN'        ! PONGO global variables

*  Status:
      INTEGER STATUS             ! Global status

*  External References:
      EXTERNAL PON_DEVOP
      LOGICAL PON_DEVOP          ! PGPLOT device is open

*  Local Variables:
      LOGICAL COLS               ! COLUMNS parameter value

      CHARACTER * ( 132 ) TITLE  ! TITLE
      CHARACTER * ( 132 ) XLABEL ! X label
      CHARACTER * ( 132 ) YLABEL ! Y label

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  See if the device is open....
      IF ( PON_DEVOP( .TRUE., STATUS ) ) THEN
         CALL PAR_GET0L( 'COLUMNS', COLS, STATUS )

         IF ( COLS ) THEN

*  Get the column labels.
            CALL PAR_DEF0C( 'XLABEL', COLLAB( XCOL ), STATUS )
            CALL PAR_DEF0C( 'YLABEL', COLLAB( YCOL ), STATUS )
         END IF
         IF ( STATUS .EQ. SAI__OK ) THEN

*  Otherwise use the labels specified.
            CALL ERR_MARK
            CALL PAR_GET0C( 'XLABEL', XLABEL, STATUS )
            IF ( STATUS .EQ. PAR__NULL .AND. COLS ) THEN
               CALL ERR_ANNUL( STATUS )
               XLABEL = COLLAB( XCOL )
            END IF
            CALL PAR_GET0C( 'YLABEL', YLABEL, STATUS )
            IF ( STATUS .EQ. PAR__NULL .AND. COLS ) THEN
               CALL ERR_ANNUL( STATUS )
               YLABEL = COLLAB( YCOL )
            END IF
            CALL ERR_RLSE
            CALL PAR_GET0C( 'TITLE', TITLE, STATUS )

*  Check the returned status act.
            IF ( STATUS .EQ. SAI__OK ) THEN
               CALL PGLABEL( XLABEL, YLABEL, TITLE )
            END IF
         END IF
      END IF

*  Issue error report on failure of routine.
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERR_REP( 'LABEL_END',
     :        'LABEL: Unable to plot the labels.', STATUS )
      END IF
      END
* $Id$
