      SUBROUTINE CHART_MON( STATUS )
*+
*  Name:
*     CHART_MON

*  Purpose:
*     Top-level ADAM monolith routine for the CHART package

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL CHART_MON( STATUS )

*  Description:
*     This routine interprets the action name passed to it and calls
*     the appropriate routine to perform the specified action. An error
*     will be reported and STATUS will be set if the action name is not
*     recognised.

*  Arguments:
*     ACTION = CHARACTER * ( * ) (Given and Returned)
*        The action name to be interpreted. The value given will be
*        forced to upper case by this routine.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Implementation Deficiencies:
*     -  This first version of the CHART monolith incorporates many
*        features left over from the INTERIM version. This may or may
*        not be considered a deficiency.

*  Copyright:
*     Copyright (C) 1991 Science & Engineering Research Council

*  Authors:
*     PMA: Peter Allan (Starlink, RAL)
*     TIMJ: Tim Jenness (JAC, Hawaii)
*     {enter_new_authors_here}

*  History:
*     19-DEC-1991 (PMA):
*        Original version.
*     25-FEB-1993 (PMA):
*        Rename the A-tasks to have a CHT_ prefix.
*     18-MAR-1993 (PMA):
*        Add a section of code at the end of this module to ensure that
*        all files are closed at the end of each action.
*      3-AUG-2004 (TIMJ):
*        Update to current spec (Task name obtained via subroutine)
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

*  Local variables
      CHARACTER * ( 15 ) ACTION  ! Task name


*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Obtain the command from the environment.  This returns uppercase
*  names.
      CALL TASK_GET_NAME( ACTION, STATUS )

*  Test the action name against each valid value in turn, calling the
*  appropriate routine...

*  Create a file for feeding into ASTROM
      IF ( ACTION .EQ. 'ASTRO' ) THEN
         CALL CHT_ASTRO( STATUS )

*  Read the search parameters, search the catalogue in a declination
*  band and store the positions in an intermediate file.
      ELSE IF ( ACTION .EQ. 'BSEARCH' ) THEN
         CALL CHT_BSEARCH( STATUS )

*  Provide a formatted display of the CHART parameters
      ELSE IF ( ACTION .EQ. 'DISPLAY' ) THEN
         CALL CHT_DISPLAY( STATUS )

*  Output the contents of the parameters file to the terminal
      ELSE IF ( ACTION .EQ. 'DISP' ) THEN
         CALL CHT_DISP( STATUS )

*  Create a file of supplementary objects
      ELSE IF ( ACTION .EQ. 'ECREATE' ) THEN
         CALL CHT_ECREATE( STATUS )

*  Create a file of field centres
      ELSE IF ( ACTION .EQ. 'FCREATE' ) THEN
         CALL CHT_FCREATE( STATUS )

*  Create an output listing file from the intermediate file
      ELSE IF ( ACTION .EQ. 'LISTO' ) THEN
         CALL CHT_LISTO( STATUS )

*  Set the CHART output parameters
      ELSE IF ( ACTION .EQ. 'OSET' ) THEN
         CALL CHT_OSET( STATUS )

*  Plot the star positions stored in the intermediate file
      ELSE IF ( ACTION .EQ. 'PLOTO' ) THEN
         CALL CHT_PLOTO( STATUS )

*  Create a file of coordinates for a error box
      ELSE IF ( ACTION .EQ. 'QCREATE' ) THEN
         CALL CHT_QCREATE( STATUS )

*  Alternative to LISTO for RGO guide stars
      ELSE IF ( ACTION .EQ. 'RGOGS' ) THEN
         CALL CHT_RGOGS( STATUS )

*  Read the search parameters, search the catalogue and store the
*  positions in an intermediate file.
      ELSE IF ( ACTION .EQ. 'SEARCH' ) THEN
         CALL CHT_SEARCH( STATUS )

*  Set a single CHART parameter
      ELSE IF ( ACTION .EQ. 'SETONE' .OR. ACTION .EQ. 'ONESET' ) THEN
         CALL CHT_SETONE( STATUS )

*  Set the CHART search parameters
      ELSE IF ( ACTION .EQ. 'SSET' ) THEN
         CALL CHT_SSET( STATUS )

*  Show the contents of the intermediate file on the screen
      ELSE IF ( ACTION .EQ. 'TYPEO' ) THEN
         CALL CHT_TYPEO( STATUS )

*  If the action name is not recognised, then report an error.
      ELSE
         STATUS = SAI__ERROR
         CALL MSG_SETC( 'ACTION', ACTION )
         CALL ERR_REP( 'CHART_ERR',
     :                 'CHART: The action name ''^ACTION'' is ' //
     :                 'not recognised by the CHART monolith.',
     :                 STATUS )
      END IF

*  Check that all of the files are closed.

      CALL CHKCLS( STATUS )

      END
