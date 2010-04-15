      SUBROUTINE CHT_BSEARCH( STATUS )
*+
*  Name:
*     CHT_BSEARCH

*  Purpose:
*     Search a 24 hour band at constant right ascension

*  Language:
*     Starlink Fortran 77

*  Type of Module:
*     ADAM A-task

*  Invocation:
*     CALL CHT_BSEARCH( STATUS )

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Description:
*     This program reads the current parameter values and searches the
*     specified catalogues for all the stars within the selected area,
*     subject to any selection criteria which have been laid down.

*  Usage:
*     BSEARCH RA DEC EQUINOX TITLE

*  ADAM Parameters:
*     RA = _CHAR (Read)
*        Right Ascension of the field centre
*        ' '
*     DEC = _CHAR (Read)
*        Declination of the field centre (not used)
*        '0'
*     EQUINOX = _CHAR (Read)
*        Equinox of the coordiates given above
*        ' '
*     TITLE = _CHAR (Read)
*        Title for the final plot
*        ' '

*  Examples:
*     BSEARCH RA='12:45' EQUINOX='1950' TITLE='Test plot'

*  Notes:
*     -  The subroutines called are all minor modifications of those in
*        the earlier version of CHART, written by P B Taylor. They in
*        turn were derived from earlier work by Roger Wood and Bill
*        Nicholson, all at RGO. Several other people at RGO also worked
*        on various aspects of these programs at various times.

*  Copyright:
*     Copyright (C) 1992 Science & Engineering Research Council

*  Authors:
*     KFH: Ken Hartley (RGO)
*     PMA: Peter Allan (Starlink, RAL)
*     {enter_new_authors_here}

*  History:
*    10-DEC-1991: (PMA)
*       Changed from main program to subroutine to be an ADAM task
*     2-MAR-1993 (AJJB):
*        STATUS argument added to CON_FACTOR call
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      CHARACTER * ( 55 ) FILENAME ! File containing field centres

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Set up the common block CONVF
      CALL CON_FACTOR( STATUS )

*  Read the parameter file and store all the values needed in the
*  Chart COMMON BLOCKS defined in MAIN.CBL
      CALL SPARIN( FILENAME, STATUS )

*  If the field centres are to come from a file, then open it.
      CALL SUB_BSRCH( FILENAME, STATUS )

*  If an error occurred, then report a contextual message.
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERR_REP( 'BSEARCH_ERR',
     :   'BSEARCH: Error running BSEARCH.',
     :   STATUS )
      END IF

      END
