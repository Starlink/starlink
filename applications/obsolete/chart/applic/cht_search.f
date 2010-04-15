      SUBROUTINE CHT_SEARCH( STATUS )
*+
*  Name:
*     CHT_SEARCH

*  Purpose:
*     Search a CHART catalogue

*  Language:
*     Starlink Fortran 77

*  Type of Module:
*     ADAM A-task

*  Invocation:
*     CALL CHT_SEARCH( STATUS )

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Description:
*     This program reads the current parameter values and searches
*     the specified catalogues for all the stars within the selected
*     area, subject to any selection criteria which have been laid down.
*
*     The subroutines called are all minor modifications of those in
*     the earlier version of CHART, written by P B Taylor. They in turn
*     were derived from earlier work by Roger Wood and Bill Nicholson,
*     all at RGO. Several other people at RGO also worked on various
*     aspects of these programs at various times.

*  Usage:
*     SEARCH RA DEC EQUINOX EPOCH TITLE

*  [ADAM_parameters]
*  [examples]
*  Algorithm:
*     Call subroutines to do all of the work, including prompting for
*     the parameters.

*  Copyright:
*     Copyright (C) 1992 Science & Engineering Research Council

*  Authors:
*     KFH: Ken Hartley (RGO)
*     PMA: Peter Allan (Starlink, RAL)
*     {enter_new_authors_here}

*  History:
*     11-JAN-1983 (KFH):
*        Original version.
*     10-DEC-1991 (PMA):
*        Changed from main program to subroutine to be an ADAM task.
*     13-SEP-1992 (PMA):
*        Tidy up prologue.
*     1-MAR-1993 (AJJB):
*        STATUS arg added to SPARIN call
*     2-MAR-1993 (AJJB):
*        STATUS argument added to CON_FACTOR call
*     5-MAR-1993 (Andrew Broderick (AJJB)):
*        STATUS argument added to all calls to routines within Chart
*        which did'nt already have one.
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

*  Local Variables:
      CHARACTER*55 FILENAME      ! Name of the parameter file

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Set up the common block CONVF
      CALL CON_FACTOR( STATUS )

*  Read the parameter file and store all the values needed in the CHART
*  COMMON blocks defined in MAIN.
      CALL SPARIN( FILENAME, STATUS )

*  If the field centres are to come from a file, then open it.
      CALL SUB_SRCH( FILENAME , STATUS )

*  If an error occurred, then report a contextual message.
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERR_REP( 'SEARCH_ERR',
     :   'SEARCH: failed .',
     :   STATUS )
      END IF

      END
